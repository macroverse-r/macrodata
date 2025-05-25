
# TODO add comments
in_convert_single_date <- function(date_string, period_type = "middle") {
  # Function to convert a single date string to as.Date
  
  # Remove 'X' prefix if present
  date_string <- sub("^X", "", date_string)
  
  # Try parsing as is
  parsed_date <- try(as.Date(date_string), silent = TRUE)
  
  if (!inherits(parsed_date, "try-error") && !is.na(parsed_date)) {
    return(parsed_date)
  }
  
  # Check for year only
  if (grepl("^\\d{4}$", date_string)) {
    year <- as.numeric(date_string)
    if (period_type == "start") return(as.Date(paste0(year, "-01-01")))
    if (period_type == "middle") return(as.Date(paste0(year, "-07-01")))
    if (period_type == "end") return(as.Date(paste0(year, "-12-31")))
  }
  
  # Check for year-month (YYYY-MM or YYYY-M)
  if (grepl("^\\d{4}-\\d{1,2}$", date_string)) {
    parts <- strsplit(date_string, "-")[[1]]
    year <- parts[1]
    month <- sprintf("%02d", as.numeric(parts[2]))
    return(as.Date(paste0(year, "-", month, "-01")))
  }
  
  # Check for quarter (YYYY-QN or YYYYQN)
  if (grepl("^\\d{4}-?Q[1-4]$", date_string)) {
    year <- substr(date_string, 1, 4)
    quarter <- as.numeric(substr(date_string, nchar(date_string), nchar(date_string)))
    if (period_type == "start") return(as.Date(paste0(year, "-", (quarter-1)*3+1, "-01")))
    if (period_type == "middle") return(as.Date(paste0(year, "-", (quarter-1)*3+2, "-15")))
    if (period_type == "end") return(as.Date(paste0(year, "-", quarter*3, "-", c(31,30,30,31)[quarter])))
  }
  
  # Try various formats
  formats <- c("%Y%b", "%Y%m%d", "%d%m%Y", "%b%Y", "%d-%m-%Y", "%B %Y", "%d/%m/%Y")
  for (fmt in formats) {
    parsed_date <- try(as.Date(date_string, format = fmt), silent = TRUE)
    if (!inherits(parsed_date, "try-error") && !is.na(parsed_date)) {
      return(parsed_date)
    }
  }
  
  # If all else fails, return NA
  return(as.Date(NA))
}

in_convert_dates <- function(data, period_type = "start") {
  # Function to convert the 'Date' column in the dataframe
  
  data$Date <- as.Date(sapply(data$Date, function(x) in_convert_single_date(x, period_type)))
  return(data)
}


in_fast_convert_dates <- function(data, period_type = "middle") {
  dates <- as.character(data$Date)  # Ensure all inputs are character strings
  
  # Remove 'X' prefix if present
  dates <- sub("^X", "", dates)
  
  # Initialize result vector
  result <- as.Date(rep(NA, length(dates)))
  
  # Function to safely convert to date
  safe_as_date <- function(x, format = NULL) {
    if (is.null(format)) {
      as.Date(as.character(x), optional = TRUE)
    } else {
      as.Date(as.character(x), format = format, optional = TRUE)
    }
  }
  
  # Standard date format
  standard_dates <- safe_as_date(dates)
  mask_standard <- !is.na(standard_dates)
  result[mask_standard] <- standard_dates[mask_standard]
  
  # Year only
  mask_year <- grepl("^\\d{4}$", dates)
  if (any(mask_year)) {
    years <- as.numeric(dates[mask_year])
    if (period_type == "start") result[mask_year] <- as.Date(paste0(years, "-01-01"))
    else if (period_type == "middle") result[mask_year] <- as.Date(paste0(years, "-07-01"))
    else if (period_type == "end") result[mask_year] <- as.Date(paste0(years, "-12-31"))
  }
  
  # Year-month
  mask_year_month <- grepl("^\\d{4}-\\d{1,2}$", dates)
  if (any(mask_year_month)) {
    year_month <- dates[mask_year_month]
    year_month_parts <- strsplit(year_month, "-")
    year_month_dates <- safe_as_date(paste0(sapply(year_month_parts, "[", 1), "-",
                                       sprintf("%02d", as.numeric(sapply(year_month_parts, "[", 2))),
                                       "-01"))
    result[mask_year_month] <- year_month_dates
  }
  
  # Quarter
  mask_quarter <- grepl("^\\d{4}-?Q[1-4]$", dates)
  if (any(mask_quarter)) {
    quarter_dates <- dates[mask_quarter]
    years <- as.numeric(substr(quarter_dates, 1, 4))
    quarters <- as.numeric(substr(quarter_dates, nchar(quarter_dates), nchar(quarter_dates)))
    if (period_type == "start") {
      result[mask_quarter] <- safe_as_date(paste0(years, "-", (quarters-1)*3+1, "-01"))
    } else if (period_type == "middle") {
      result[mask_quarter] <- safe_as_date(paste0(years, "-", (quarters-1)*3+2, "-15"))
    } else if (period_type == "end") {
      result[mask_quarter] <- safe_as_date(paste0(years, "-", quarters*3, "-", 
                                             c(31,30,30,31)[quarters]))
    }
  }
  
  # Other formats
  mask_remaining <- is.na(result)
  if (any(mask_remaining)) {
    remaining_dates <- dates[mask_remaining]
    formats <- c("%Y%b", "%Y%m%d", "%d%m%Y", "%b%Y", "%d-%m-%Y", "%B %Y", "%d/%m/%Y")
    for (fmt in formats) {
      parsed_dates <- safe_as_date(remaining_dates, format = fmt)
      mask_parsed <- !is.na(parsed_dates)
      result[mask_remaining][mask_parsed] <- parsed_dates[mask_parsed]
      remaining_dates <- remaining_dates[!mask_parsed]
      mask_remaining[mask_remaining][mask_parsed] <- FALSE
      if (!any(mask_remaining)) break
    }
  }
  
  # Handle any remaining unparsed dates
  if (any(is.na(result))) {
    warning(paste("Some dates could not be parsed. Indices:", 
                  paste(which(is.na(result)), collapse = ", ")))
  }
  
  data$Date <- result
  return(data)
}
