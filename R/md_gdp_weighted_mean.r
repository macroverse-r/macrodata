# Calculate GDP-weighted inflation for ASEAN
# Define key variables
# years <- c(1990, 2023) # time range
# indicator <- "FP_CPI_TOTL_ZG" # CPI inflation
# iso <- "ASEAN"



in_iso_gdp_normalization <- function(
                                 ISO, years, indicator, 
                                 constant_sample = TRUE,
                                 verbose = TRUE){

    # Step 0: check if indicator is yearly or quarterly
    frequency <- WPD:::in_locate_symbol(indicator)
    gdp <- ifelse(frequency == "Y", "GDP_C_Y", 
                  ifelse(frequency == "Q", "GDP_C", NA))

    # Step 1: load row data
    df_ind <- wp_data(ISO = ISO, years = years,
                      formula = paste(indicator, "*", gdp),
                      verbose = FALSE)

    df_gdp <- wp_data(ISO = ISO, years = years,
                      formula = gdp, verbose = FALSE)

    # Step 2: check data availability and adjust sample
    both_avail <- !is.na(df_gdp$Value) & !is.na(df_ind$Value)
    iso2remove <- unique(df_gdp[!both_avail, c("ISO")])
    if (verbose) { 
        in_format_removed_data(df_gdp[!both_avail, c("ISO", "Date")]) 
    }
    if (constant_sample) {
        gdp_pre <- wp_data(ISO = ISO, years = years, formula = gdp,
                                variable = "Total GDP", aggregate_iso = "Sum", verbose = FALSE)
        ISO <- setdiff(unique(df_gdp$ISO), iso2remove)
        gdp_post <- wp_data(ISO = ISO, years = years, formula = gdp,
                                variable = "Total GDP", aggregate_iso = "Sum", verbose = FALSE)
        summary <- cbind(gdp_pre[, c("Date", "Value")], gdp_post$Value, 100*gdp_post$Value/gdp_pre$Value)
        names(summary) <- c("Date", "Pre-filter", "Post-filter", "% remaining")
        in_print_two_columns(summary)

    } else {
        # not yet implemented
        stop("Error in wp_gmw(): option constant_sample = FALSE not yet implemented.")
        df_ind <- df_ind[both_avail, ]
        df_gdp <- df_gdp[both_avail, ]
    }
    
    output <- list(ISO = ISO, gdp_measure = gdp)
    return(output)

}



wp_gwm <- function(ISO, years, indicator, 
                   iso_name = "TOT",
                   var_name = "GWM",
                   add_simple_mean = TRUE, 
                   constant_sample = TRUE,
                   verbose = TRUE){

    gdp_norm <- in_iso_gdp_normalization(ISO, years, indicator, constant_sample, verbose)
    ISO <- gdp_norm$ISO 
    gdp_measure <- gdp_norm$gdp_measure

    # Step 3: Get weighted sum components
    weighted_ind <- wp_data(ISO = ISO, years = years,
                            formula = c(paste(indicator, "*", gdp_measure), gdp_measure),
                            variable = c("Weighted Sum", "Total GDP"),
                            aggregate_iso = "Sum", verbose = FALSE)

    # Step 4: Calculate weighted average
    weighted_avg <- weighted_ind
    weighted_avg$Value[weighted_avg$Variable == "Weighted Sum"] <- 
        weighted_avg$Value[weighted_avg$Variable == "Weighted Sum"] / 
        weighted_avg$Value[weighted_avg$Variable == "Total GDP"]
    # # Update variable name and remove Total GDP row
    weighted_avg$Variable[weighted_avg$Variable == "Weighted Sum"] <- var_name
    gwm <- weighted_avg[weighted_avg$Variable != "Total GDP", ]

    # Step 5: Add simple average
    if (add_simple_mean) {
        simple_avg <- wp_data(ISO = ISO, years = years,
                              formula = indicator,
                              variable = c("Mean"),
                              aggregate_iso = "Mean", verbose = FALSE)

        gwm <- rbind(gwm, simple_avg)
    }
    gwm$ISO <- iso_name

    return(gwm)
}




wp_data_by_gdp <- function(
                          ISO, years, indicator, 
                          iso_name = "TOT",
                          var_name = "Ratio",
                          constant_sample = TRUE,
                          verbose = TRUE){



    # ISO codes for which we have data for GDP and Indicator for every period
    gdp_norm <- in_iso_gdp_normalization(ISO, years, indicator, constant_sample, verbose)
    ISO <- gdp_norm$ISO 
    if (verbose) {
        cat("------------------ \n ")
        cat("Remaing ISO codes: \n ")
        cat(strwrap(paste(ISO, collapse = ", "), width = 80), sep = "\n")
    }
    gdp_measure <- gdp_norm$gdp_measure


    # Step 3: Get weighted sum components
    data <- wp_data(ISO = ISO, years = years,
                            formula = c(indicator, gdp_measure),
                            variable = c("Indicator", "GDP"),
                            aggregate_iso = "Sum")

    # Step 4: Calculate weighted average
    data_ratio <- data
    data_ratio$Value[data_ratio$Variable == "Indicator"] <- 
        100*data_ratio$Value[data_ratio$Variable == "Indicator"] / 
        data_ratio$Value[data_ratio$Variable == "GDP"]
    # Update variable name and remove Total GDP row
    data_ratio$Variable[data_ratio$Variable == "Indicator"] <- var_name
    data_ratio <- data_ratio[data_ratio$Variable != "GDP", ]

    data <- rbind(data, data_ratio)
    data$ISO <- iso_name

    return(data)
}


in_numeric_dates <- function(dates, only_year = FALSE) {
  # Remove 'X' from the beginning if present
  dates <- gsub("^X", "", dates)
  
  # Function to handle quarterly dates
  handle_quarterly <- function(x) {
    if (grepl("Q[1-4]$", x)) {
      year <- as.numeric(sub("Q[1-4]$", "", x))
      quarter <- as.numeric(sub(".*Q", "", x))
      if (only_year) {
          return(year)
      }
      return(year + (quarter - 1) / 4)
    } else {
      return(as.numeric(x))
    }
  }
  
  # Apply the function to each date
  numeric_dates <- sapply(dates, handle_quarterly)
  
  return(numeric_dates)
}



in_format_removed_data <- function(df, max_width = options()$width) {
    if (nrow(df) == 0) { 
        cat("# All data are available! \n")
        return(NULL) 
    }
    # Remove 'X' from dates and create numeric years
    df$Year <- in_numeric_dates(df$Date, only_year = TRUE)

    # Get unique ISO codes
    unique_iso <- unique(df$ISO)

    # Process each ISO code
    result <- character(length(unique_iso))

    for(i in seq_along(unique_iso)) {
        iso <- unique_iso[i]
        years <- sort(df$Year[df$ISO == iso])
        years <- unique(years)

        # Find breaks in consecutive years
        breaks <- c(0, which(diff(years) != 1), length(years))
        ranges <- character()

        # Process each range
        for(j in seq_along(breaks)[-1]) {
            range_years <- years[(breaks[j-1] + 1):breaks[j]]
            if(length(range_years) == 1) {
                ranges[j-1] <- as.character(range_years[1])
            } else {
                start_year <- range_years[1]
                end_year <- range_years[length(range_years)]
                if(floor(start_year/100) == floor(end_year/100)) {
                    ranges[j-1] <- paste0(start_year, "-", substr(end_year, 3, 4))
                } else {
                    ranges[j-1] <- paste0(start_year, "-", end_year)
                }
            }
        }

        # Combine ranges for this ISO
        result[i] <- paste(iso, paste(ranges, collapse=", "), sep=": ")
    }

    # Format output in columns
    format_columns <- function(strings, max_width) {
        # Get max string length
        max_length <- max(nchar(strings)) + 2  # Add 2 for padding

        # Calculate optimal number of columns
        n_cols <- max(1, min(4, floor(max_width / max_length)))
        n_rows <- ceiling(length(strings) / n_cols)

        # Pad strings to equal length
        padded_strings <- format(strings, width = max_length, justify = "left")

        # Create matrix
        if (length(padded_strings) %% n_cols != 0) {
            padding_needed <- n_cols - (length(padded_strings) %% n_cols)
            padded_strings <- c(padded_strings, rep("", padding_needed))
        }

        # Convert to matrix by columns
        m <- matrix(padded_strings, ncol = n_cols, byrow = FALSE)

        # Format each row
        formatted_rows <- apply(m, 1, paste, collapse = "")

        # Remove trailing whitespace
        formatted_rows <- trimws(formatted_rows, "right")

        return(formatted_rows)
    }
  
    # Print header and formatted results
    cat("# Data not available: \n")
    formatted_output <- format_columns(result, max_width)
    cat(paste(formatted_output, collapse="\n"), "\n")
}

in_print_two_columns <- function(summary) {
    # Split the data into two parts
    n <- nrow(summary)
    mid <- ceiling(n/2)
    
    # Create the two parts
    part1 <- summary[1:mid, ]
    part2 <- summary[(mid+1):n, ]
    
    # Function to remove X from date if present
    clean_date <- function(date) {
        gsub("^X", "", as.character(date))
    }
    
    # Function to format a row consistently
    format_row <- function(row) {
        sprintf("%-6s %12s %12s %8.2f", 
                clean_date(row[1]), 
                format(as.numeric(row[2]), scientific = TRUE, digits = 3),
                format(as.numeric(row[3]), scientific = TRUE, digits = 3),
                as.numeric(row[4]))
    }
    
    # Print headers
    cat(sprintf("%-6s %12s %12s %8s    |       %-6s %12s %12s %8s\n",
        "Date", "Pre-filter", "Post-filter", "% remaining",
        "Date", "Pre-filter", "Post-filter", "% remaining"))
    
    # Print separator
    cat(sprintf("%44s    |   %44s\n",
        paste(rep("-", 44), collapse=""),
        paste(rep("-", 44), collapse="")))
    
    # Print rows side by side
    for(i in 1:mid) {
        left_part <- format_row(part1[i,])
        if(i <= nrow(part2)) {
            right_part <- format_row(part2[i,])
            cat(sprintf("%-40s       |       %-40s\n", left_part, right_part))
        } else {
            cat(sprintf("%-40s\n", left_part))
        }
    }
}





# years <- c(1970, 2022)
# ISO <- c("CTR","SMP","PERI")
# gwm_cpi <- wp_gwm(ISO=ISO, years=years, indicator = "CPI_INFL_RATE", add_simple_mean=F, var_name = "CPI")
# gwm_cpi2 <- wp_gwm(ISO=ISO, years=years, indicator = "FP_CPI_TOTL_ZG", add_simple_mean=F, var_name = "CPI2")
# gwm_defl <- wp_gwm(ISO=ISO, years=years, indicator = "GDP_DEFL_Y", add_simple_mean=F, var_name = "GDP deflator")
# gwm_defl <- wp_gwm(ISO=ISO, years=years, indicator = "GDP_DEFL_Y", add_simple_mean=F, var_name = "GDP deflator")
# # gwm_defl_wld <- wp_gwm(ISO=ISO, years=years, indicator = "GDP_DEFL_WLD_Y", add_simple_mean=F, var_name = "World GDP deflator")
# defl_wld <- wp_data(ISO="USA", years=years, formula = "GDP_DEFL_WLD_Y")
# defl_wld$ISO <- "TOT"
#
# defl_wld_nf <- wp_data(ISO="USA", years=years, formula = "GDP_DEFL_WLD_NF_Y")
# defl_wld_nf$ISO <- "TOT"
#
# defl_wld_dl <- wp_data(ISO="USA", years=years, formula = "GDP_DEFL_WLD_DL_Y")
# defl_wld_dl$ISO <- "TOT"
#
# # GDP_DEFL_WLD_Y
#
# data <- rbind(gwm_cpi, gwm_cpi2, gwm_defl, defl_wld, defl_wld_nf, defl_wld_dl)
# # data$Value <- log(data$Value)
#
# wp_plot_series(data)




