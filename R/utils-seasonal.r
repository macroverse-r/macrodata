

# Function to adjust for seasonal variation
in_adjust_seasonal <- function(data, window_seasadj) {

  original_data <- data

  # Ensure all dates are in Date format
  data <- in_fast_convert_dates(data, period_type = "start")
  
  
  # Store original column order
  original_columns <- colnames(data)
  
  # Create a copy of the original data to modify
  adjusted_data <- data
  
  # Loop through each unique country in the dataset
  for (country in unique(data$ISO)) {
    # Filter data for the current country
    country_data <- data[data$ISO == country, ]
    
    # Remove NA values for calculation purposes
    valid_data <- country_data[!is.na(country_data$Value), ]
    
    if (nrow(valid_data) < 16) {  # Not enough data for seasonal adjustment
      warning(paste("Not enough non-NA data for country:", country))
      next
    }
    
    # Convert to time series object
    ts_data <- ts(valid_data$Value, 
                  start = c(as.numeric(format(min(valid_data$Date), "%Y")), 
                            (as.numeric(format(min(valid_data$Date), "%m")) - 1) %/% 3 + 1),
                  frequency = 4)
    
    # Apply STL decomposition
    tryCatch({
      # Use STL decomposition
      stl_result <- stl(ts_data, s.window = 'periodic', t.window = window_seasadj)
      
      # Extract the seasonally adjusted series (trend + remainder)
      adjusted_values <- stl_result$time.series[, "trend"] #+ stl_result$time.series[, "remainder"]
      
      # Update the Value column in the adjusted_data for this country
      adjusted_data$Value[adjusted_data$ISO == country & !is.na(adjusted_data$Value)] <- as.numeric(adjusted_values)
      
    }, error = function(e) {
      warning(paste("Failed to adjust seasonal data for country:", country, "- Error:", e$message))
      # If adjustment fails, original data is kept (no change needed)
    })
  }
  
  # Ensure the output data frame has the same structure as the input
  adjusted_data <- adjusted_data[, original_columns]
  
  output_data <- original_data
  output_data$Value <- adjusted_data$Value

  return(output_data)
}


in_test_seasonal_adjustment <- function(data, iso_codes) {
  # Ensure the 'ISO' column exists in the data
  if (!"ISO" %in% colnames(data)) {
    stop("The data frame must contain an 'ISO' column.")
  }
  
  data <- in_fast_convert_dates(data, period_type = "start")

  # Apply seasonal adjustment to the entire dataset
  adjusted_data <- in_adjust_seasonal(data)
  

  # Loop through each provided ISO code
  for (iso in iso_codes) {
    # Check if the ISO code exists in the data
    if (!(iso %in% unique(data$ISO))) {
      cat(sprintf("ISO code %s not found in the data. Skipping.\n\n", iso))
      next
    }
    
    # Filter raw and adjusted data for the current ISO code
    raw_country_data <- data[data$ISO == iso, ]
    adjusted_country_data <- adjusted_data[adjusted_data$ISO == iso, ]
    
    plot(raw_country_data$Date, raw_country_data$Value, type='l', col='black')
    lines(raw_country_data$Date, adjusted_country_data$Value, col='red')
    grid()

    # Print raw data
    cat(sprintf("Raw data for %s (%s):\n", raw_country_data$Country[1], iso))
    print(head(raw_country_data, 10))
    cat("\n")
    
    # Print adjusted data
    cat(sprintf("Seasonally adjusted data for %s (%s):\n", adjusted_country_data$Country[1], iso))
    print(head(adjusted_country_data, 10))
    cat("\n")
    
    # Print a summary of changes
    cat("Summary of changes:\n")
    cat(sprintf("Number of rows: %d (raw) vs %d (adjusted)\n", 
                nrow(raw_country_data), nrow(adjusted_country_data)))
    cat(sprintf("Column names: %s\n", 
                ifelse(identical(colnames(raw_country_data), colnames(adjusted_country_data)), 
                       "Identical", "Different")))
    
    # Compare the 'Value' column
    value_diff <- raw_country_data$Value - adjusted_country_data$Value
    cat(sprintf("Mean absolute difference in 'Value': %.4f\n", mean(abs(value_diff), na.rm = TRUE)))
    cat(sprintf("Max absolute difference in 'Value': %.4f\n", max(abs(value_diff), na.rm = TRUE)))
    cat("\n---\n\n")
  }
}

