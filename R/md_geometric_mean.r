#' Calculate Geometric Mean
#' 
#' @description
#' Computes geometric mean with proper handling of negative values and zeroes.
#' Uses log transformation for numerical stability and handles edge cases gracefully.
#'
#' @param x Numeric vector of values to average
#' @param na.rm Logical, whether to remove NA values (default: TRUE)
#'
#' @return Numeric value representing the geometric mean
#'
#' @details
#' The function:
#' - Handles NA values by removing them (if na.rm = TRUE)
#' - Uses log transformation for computation: exp(mean(log(1 + x))) - 1
#' - Returns NA for empty or all-NA inputs
#' - Adjusts values by +1 before calculation to handle negative values
#' - Suitable for growth rates and percentage changes
#'
#' @examples
#' \dontrun{
#' # Growth rates as decimals
#' growth_rates <- c(0.05, 0.03, -0.02, 0.08)
#' md_geometric_mean(growth_rates)
#' 
#' # Handle missing values
#' with_na <- c(0.05, NA, 0.03, 0.08)
#' md_geometric_mean(with_na, na.rm = TRUE)
#' }
#'
#' @export
md_geometric_mean <- function(x, na.rm = TRUE) {
    # Input validation
    if (!is.numeric(x)) {
        mvcommon::mv_stop("Invalid input",
                         "x" = "Expected a numeric vector",
                         "i" = "Got {.cls {class(x)}}")
    }
    
    # Handle NA values
    if (na.rm) {
        x <- x[!is.na(x)]
    }
    
    # Check for empty or all-NA input
    if (length(x) == 0 || all(is.na(x))) {
        return(NA_real_)
    }
    
    # Check for values that would cause issues with log(1 + x)
    if (any(x <= -1, na.rm = TRUE)) {
        mvcommon::mv_warn("Values <= -1 detected",
                         "!" = "Some values are <= -100%, which may cause issues",
                         "i" = "Consider checking your data for validity")
    }
    
    # Calculate geometric mean using log transformation
    result <- exp(mean(log(1 + x), na.rm = !na.rm)) - 1
    
    return(result)
}