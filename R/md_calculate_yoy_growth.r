#' Calculate Year-over-Year Growth Rates
#' 
#' @description
#' Computes year-over-year growth rates for both quarterly and annual data.
#' Automatically detects data frequency and applies appropriate calculations.
#'
#' @param data data.frame with Date and Value columns
#'   - Date: Character or Date column with year (YYYY) or year-quarter (YYYYQN) format
#'   - Value: Numeric column with values to calculate growth for
#'
#' @return Numeric vector of growth rates (as decimals, not percentages)
#'
#' @details
#' The function handles:
#' - Quarterly data: Calculates quarter-to-quarter year-over-year growth
#' - Annual data: Calculates year-to-year growth
#' - Missing values: Skips NA values in calculations
#' - Zero base values: Handles gracefully by skipping those calculations
#'
#' @examples
#' \dontrun{
#' # Annual data
#' annual_data <- data.frame(
#'   Date = c("2020", "2021", "2022"),
#'   Value = c(100, 105, 110)
#' )
#' md_calculate_yoy_growth(annual_data)
#' 
#' # Quarterly data
#' quarterly_data <- data.frame(
#'   Date = c("2020Q1", "2020Q2", "2021Q1", "2021Q2"),
#'   Value = c(100, 102, 105, 108)
#' )
#' md_calculate_yoy_growth(quarterly_data)
#' }
#'
#' @export
md_calculate_yoy_growth <- function(data) {
    # Input validation
    if (!is.data.frame(data)) {
        mvcommon::mv_stop("Invalid input",
                         "x" = "Expected a data.frame",
                         "i" = "Got {.cls {class(data)}}")
    }
    
    if (!all(c("Date", "Value") %in% names(data))) {
        mvcommon::mv_stop("Missing required columns",
                         "x" = "Data must have 'Date' and 'Value' columns",
                         "i" = "Available columns: {.val {names(data)}}")
    }
    
    # Ensure data is sorted by date
    data <- data[order(data$Date), ]
    
    # Extract year and quarter (if applicable) from dates
    dates <- data$Date
    years <- as.numeric(substr(dates, 1, 4))
    is_quarterly <- nchar(dates[1]) == 6
    
    if (is_quarterly) {
        quarters <- substr(dates, 6, 6)
        growth_rates <- numeric()
        
        # Calculate year-over-year growth for each quarter
        for (q in unique(quarters)) {
            q_indices <- which(quarters == q)
            if (length(q_indices) >= 2) {
                for (i in 2:length(q_indices)) {
                    current <- data$Value[q_indices[i]]
                    previous <- data$Value[q_indices[i-1]]
                    if (!is.na(current) && !is.na(previous) && previous != 0) {
                        growth_rates <- c(growth_rates, (current - previous) / previous)
                    }
                }
            }
        }
    } else {
        # Calculate year-over-year growth for annual data
        values <- data$Value
        growth_rates <- numeric()
        for (i in 2:length(values)) {
            if (!is.na(values[i]) && !is.na(values[i-1]) && values[i-1] != 0) {
                growth_rates <- c(growth_rates, (values[i] - values[i-1]) / values[i-1])
            }
        }
    }
    
    return(growth_rates)
}