
#' Load and Process World Panel Data
#' 
#' @description
#' Loads and processes data from a comprehensive panel database containing economic,
#' financial, and development indicators. The function handles data filtering,
#' frequency adjustments, aggregation, and seasonal adjustments. Memory usage is
#' optimized by loading data on demand (approximately 15-50MB during execution).
#' 
#' @param ISO character vector; ISO 3-letter country codes or category names:
#'        - Individual countries (e.g., "USA", "CHN")
#'        - Categories (e.g., "CTR_LDR", "BRICS", "AFRICA")
#'        - Category exclusions using hyphen (e.g., "CTR_LDR - USA")
#'        See wp_get_category() for available categories.
#' @param formula character vector; mathematical expressions using Symbol codes:
#'        - Simple variables (e.g., "GDP_C")
#'        - Calculations with basic operators (e.g., "100*CU_C/GDP_C")
#'        - Multiple formulas as vector
#'        - Basic operators: + - * / ( )
#'        Division by zero not handled, use with caution.
#' @param variable character vector or NULL; names for formula outputs (Column Variable):
#'        - Must match length of formula if provided
#'        - Used for output labeling and plotting
#'        Default is NULL, using formula as names.
#' @param years numeric vector of length 2; year range; c(start_year, end_year):
#'        - First element: start year
#'        - Second element: end year
#'        Data availability varies by country/indicator.
#' @param adjust_seasonal logical (TRUE/FALSE); apply seasonal adjustment:
#'        - TRUE: adjust quarterly data using STL decomposition
#'        - Only affects quarterly data (no effect on annual)
#'        Default is FALSE.
#' @param window_seasadj numeric or NULL (= 7); window for seasonal adjustment: 
#'        - Controls smoothing in STL decomposition
#'        - Larger values = more smoothing
#'        - Only used if adjust_seasonal = TRUE
#'        Default is 7.
#' @param matching_yq character; method for handling mixed frequencies:
#'        - "Q2Y": convert quarterly to yearly
#'        - "Y2Q": convert yearly to quarterly
#'        Default is "Q2Y".
#' @param interpolation_method character; method for Y2Q conversion:
#'        - "None": repeat yearly value
#'        - "Linear": linear interpolation
#'        - "Linear-Scale": scaled linear interpolation
#'        Only used if matching_yq = "Y2Q".
#' @param aggregate_iso character or NULL; method for country aggregation:
#'        - "Sum": sum values across countries
#'        - "Mean": average values across countries
#'        - "Median": median values across countries
#'        - NULL: no aggregation
#' @param aggregate_period character or NULL; method for time aggregation:
#'        - "Sum": sum over period
#'        - "Mean"/"Median": central tendency
#'        - "SD": standard deviation
#'        - "Growth": period-over-period growth
#'        - "CAGR": compound annual growth rate
#'        - "GeoMean": geometric mean
#'        - NULL: no aggregation
#' @param quartile logical; include quartile calculations:
#'        - TRUE: add first/third quartiles to aggregations
#'        - Only used if aggregate_iso or aggregate_period is specified
#'        Default is FALSE.
#' @param na.rm logical; handle missing values in aggregations:
#'        - TRUE: exclude NA values
#'        - FALSE: return NA if any value is NA
#'        Default is TRUE.
#' @param reference logical; include data source citations:
#'        - TRUE: add Reference column to output
#'        Default is TRUE.
#' @param country_names logical; include full country names:
#'        - TRUE: add Country column with names from ISO codes
#'        Default is FALSE.
#' @param clean logical; remove rows with NA values:
#'        - TRUE: remove NA rows from final output
#'        Default is FALSE.
#' @param na2zero logical; transform NA values into zeros:
#'        - TRUE: remove NA values and replace them by zeros
#'        Default is FALSE.
#' @param verbose logical; print processing information:
#'        - TRUE: show progress and warnings
#'        Default is TRUE.
#' @param debug logical; print detailed debugging information:
#'        - TRUE: show technical details
#'        Default is FALSE.
#'
#' @return A data.frame containing:
#'   - ISO: 3-letter country codes
#'   - Date: time period (YYYY or YYYYQN format)
#'   - Variable: indicator names from formula/variable
#'   - Value: calculated values
#'   - Reference: data sources (if reference=TRUE)
#'   - Country: country names (if country_names=TRUE)
#'
#' @details
#' The function processes data in several steps:
#' 1. Validates inputs and resolves country categories
#' 2. Loads required data (quarterly/yearly) on demand
#' 3. Extracts symbols from formulas and filters data
#' 4. Handles frequency mismatches (Q2Y or Y2Q conversion)
#' 5. Evaluates formulas for each country
#' 6. Performs any requested aggregations
#' 7. Applies seasonal adjustments if specified
#' 8. Cleans and formats output
#'
#' Memory usage is optimized by:
#' - Loading data only when needed
#' - Filtering to required columns early
#' - Processing one formula at a time
#' - Clearing intermediate objects
#'
#' Data Validation and Error Handling:
#' - Missing data warnings by country/variable
#' - Automatic date range adjustment if requested years unavailable
#' - Minimum observations check for seasonal adjustment (16 required)
#' - Warnings for inappropriate aggregation requests (e.g., growth rates with negative values)
#'
#' Variable Types and Units:
#' - Stock variables: measured at a point in time (e.g., reserves, debt)
#' - Flow variables: measured over a period (e.g., GDP, trade)
#' - Index variables: base year representations (various base years available)
#' - Percentage variables: bounded ratios
#' - For IMF Balance of Payments data, quarterly values are multiplied by 4 to represent
#'   annualized flows, ensuring consistency with yearly data
#'
#' Common Data Patterns:
#' World Bank (WB) indicators:
#' - _ZS suffix: ratios expressed as percentages
#' - _CD suffix: current US dollars
#' - _KD suffix: constant dollars
#' - _XD suffix: indices
#' - _PC suffix: per capita values
#' - _FE/_MA suffixes: female/male specific indicators
#'
#' IMF Balance of Payments (BOP):
#' - _C suffix: current prices
#' - _R suffix: real/constant prices (with base year)
#' - o/i prefixes: outward/inward flows
#' - DI/POR/OI suffixes: direct investment/portfolio/other investment
#' - E/D suffixes: equity/debt components
#'
#' Natural Disasters (EMDAT):
#' - DIS prefix: disaster-related indicators
#' - _AFF/_DEATH/_DMG suffixes: affected people/fatalities/economic damage
#' - BIO/CLIM/GEO/HYDRO prefixes: biological/climatic/geological/hydrological disasters
#'
#' Financial Market Data (BIS):
#' - CRED prefix: credit-related indicators
#' - _ALL/_BANK prefixes: all sectors/banking sector
#' - _CD/_KN/_ZS suffixes: USD/local currency/percentage of GDP
#'
#' Other specialized databases (JST, KOF, etc.) have their own consistent naming patterns
#' that are documented in their respective sources.
#'
#' @examples
#' # Basic usage - GDP ratio for one country
#' data <- wp_data(
#'   ISO = "USA",
#'   formula = "100*CU_C/GDP_C",
#'   variable = "Current Account (% GDP)",
#'   years = c(2000, 2023)
#' )
#'
#' # Multiple countries and indicators with aggregation
#' data <- wp_data(
#'   ISO = c("CHN", "JPN", "KOR"),
#'   formula = c("EXg_C/GDP_C", "IMg_C/GDP_C"),
#'   variable = c("Exports", "Imports"),
#'   years = c(2010, 2023),
#'   adjust_seasonal = TRUE,
#'   aggregate_iso = "Mean"
#' )
#'
#' # Using categories with exclusions
#' data <- wp_data(
#'   ISO = "CTR_LDR - USA",
#'   formula = "FA_C/GDP_C",
#'   years = c(1990, 2023),
#'   adjust_seasonal = TRUE,
#'   aggregate_period = "Growth"
#' )
#'
#' # Mixed frequency handling
#' data <- wp_data(
#'   ISO = "DEU",
#'   formula = c("GDP_R_2015_Y", "CU_C"),
#'   years = c(2015, 2023),
#'   matching_yq = "Y2Q",
#'   interpolation_method = "Linear"
#' )
#'
#' @seealso 
#' \code{\link{wp_plot_series}} for plotting time series
#' \code{\link{wp_plot_scatter}} for scatter plots
#' \code{\link{wp_plot_bar}} for bar plots
#' \code{\link{wp_get_category}} for available country categories
#'
#' @note
#' Variables in formulas refer to Symbol codes in the underlying database.
#' Users should understand the economic/financial meaning of variables
#' and their units before performing calculations.
#'
#' @family data functions
#' @export
md_data <- function(ISO,                           # ISO3 codes or categories
                   formula,                         # mathematical expressions
                   variable = NULL,                 # names for formula outputs
                   years,                          # c(start_year, end_year)
                   adjust_seasonal = FALSE,         # TRUE/FALSE
                   window_seasadj = NULL,          # window size (default 7)
                   matching_yq = "Q2Y",            # "Q2Y" or "Y2Q"
                   interpolation_method = "Linear", # "None", "Linear", "Linear-Scale"
                   aggregate_iso = NULL,           # "Sum", "Mean", "Median"
                   aggregate_period = NULL,        # "Sum", "Mean", "Median", "SD", "Growth", "CAGR", "GeoMean"
                   quartile = FALSE,               # TRUE/FALSE
                   na.rm = TRUE,                   # TRUE/FALSE
                   reference = TRUE,               # TRUE/FALSE
                   country_names = FALSE,          # TRUE/FALSE
                   clean = FALSE,                   # TRUE/FALSE
                   na2zero = FALSE,                # TRUE/FALSE
                   verbose = TRUE,                 # TRUE/FALSE
                   debug = FALSE) {                # TRUE/FALSE


    ###############
    # STEP 0: Get Category

    # Function to check if any category is present as a substring in any ISO element
    has_match <- !ISO %in% isomapper::im_get_category(ISO, verbose = FALSE)

    if (sum(has_match) > 0) {
        ISO_i <- ISO
        ISO <- unique(isomapper::im_get_category(ISO))

        for (i in seq_along(has_match)) {
            if (has_match[i]) {
                mvcommon::mv_debug(paste0("Get ISO codes from category code: ", ISO_i[i]), debug = debug || verbose)
                category <- isomapper::im_get_category(ISO_i[i], verbose = FALSE)
                msg <- paste0("ISO3(",length(category),"): " , paste0(category, collapse = " "))
                mvcommon::mv_debug(msg, verbose, debug, "info_loop")
            }
        }
    }


    ###############
    # STEP 1: Input Validation
    mvcommon::mv_debug("Input Validation.", verbose, debug, "info", "Step 1")
    if (is.null(variable)){ variable <- formula }
    .validate_inputs(ISO, formula, variable, years, adjust_seasonal)

    ###############
    # STEP 2: Data Filtering (ISO codes, Symbols, and Years)
    # reduce the size of data to only incorporate relevant ISO codes, columns and symbols
    mvcommon::mv_debug("Data Filtering (ISO codes, Symbols, and Years).", verbose, debug, "info", "Step 2")
    filtered_data <- .load_filter_data(ISO, years, formula, verbose) 

    # Initialize an empty list to store results for each formula
    results_list <- list()




    ###############
    # STEP 3 to 7
    # Loop through each formula and corresponding variable
    mvcommon::mv_debug("Loop through each formula.", verbose, debug, "info", "Steps 3 to 7")
    for (i in seq_along(formula)) {
        form <- formula[i]
        var <- variable[i]

        # Call .data_one_formula for each formula
        result <- .data_one_formula(
          countries = ISO,  # Pass ISO parameter as countries
          formula = form,
          variable = var,
          data = filtered_data,
          adjust_seasonal = adjust_seasonal,
          reference = reference,
          matching_yq = matching_yq,
          interpolation_method = interpolation_method,
          na2zero = na2zero,
          verbose = verbose,
          debug = debug
        )

        # Add the result to the list
        results_list[[i]] <- result
    }

    # Combine all results into a single dataframe
    combined_results <- do.call(rbind, results_list)

    # Reset row names
    rownames(combined_results) <- NULL

    ###############
    # STEP 8: AGGREGATE

    # Aggregate by country groups
    if (!is.null(aggregate_iso)) {
        iso <- unique(combined_results$ISO)
        msg <- paste0("Aggregate values (group of ISO codes) - Method: ", aggregate_iso, " | Quartile: ", quartile, " | na.rm: ", na.rm, "\n",
                "ISO (", length(iso), "): ", paste0(iso, collapse = ", "))
        mvcommon::mv_debug(msg, verbose, debug, "info", "Step 8")
        combined_results <- .aggregate_countries(combined_results, 
                                                method = aggregate_iso, 
                                                quartile = quartile, 
                                                na.rm = na.rm) 
    }


    # Aggregate by time period
    if (!is.null(aggregate_period)) {
        msg <- paste0("Aggregate values (time periods) - Method: ", aggregate_period, " | Quartile: ", quartile, " | na.rm: ", na.rm)
        mvcommon::mv_debug(msg, verbose, debug, "info", "Step 8")
        combined_results <- .aggregate_time(combined_results, 
                                                method = aggregate_period, 
                                                quartile = quartile, 
                                                na.rm = na.rm) 
    }



    ###############
    # STEP 9: Post-processing (adjust for seasonal variations)
    if (adjust_seasonal && is.null(aggregate_period)) {
        if (is.null(window_seasadj)) {
            window_seasadj <- 7
        }

        msg <- paste0("Adjust for seasonal variations [only for quarterly data] - adjust_seasonal is ", adjust_seasonal)
        mvcommon::mv_debug(msg, verbose, debug, "info", "Step 9")
        V <- NULL
        if (is.null(variable)) {variable <- formula}
        for (v in variable){
            # if quarterly data (dates in 6 letters)
            if( nchar(combined_results[combined_results$Variable == v,]$Date[1]) == 6 ){
                V <- c(V,v)
                mvcommon::mv_debug(paste0("Seasonal adjustment for: ", v), FALSE, debug, "debug")
                head(combined_results)
                combined_results[combined_results$Variable == v,] <- .adjust_seasonal(combined_results[combined_results$Variable == v,],
                                                                                        window_seasadj = window_seasadj)
                head(combined_results)
            }else{
                mvcommon::mv_debug(paste0("No seasonal adjustment for: ", v), FALSE, debug, "debug")
            }
        }
        msg <- paste0("Seasonal adjustments for: ", paste0(V, collapse = "   "))
        mvcommon::mv_debug(msg, verbose, FALSE, "info")
    }

    ###############
    # STEP 10: CLEAN (remove NAs)
    if (clean) {
        mvcommon::mv_debug(paste0("Clean database [remove NAs in output] - clean is ",clean,"."), verbose, debug, "info", "Step 10")
        combined_results <- combined_results[!is.na(combined_results$Value), ]
    } 
    # else if (na2zero && sum(is.na(combined_results$Value)) > 0) {
    #     mvcommon::mv_debug(paste0("Transform all NAs into zeros - na2zero is ", na2zero,"."), verbose, debug, "info", "Step 10")
    #     combined_results[is.na(combined_results$Value), ]$Value <- 0
    # }


    ###############
    # Add Country names
    if (country_names) {
        combined_results$Country <- isomapper::im_from_iso(combined_results$ISO, opt = "name")
    }


    # Ensure consistent variable order by explicitly ordering
    combined_results <- combined_results[order(match(combined_results$Variable, variable)), ]

    # remove X is present in Date
    combined_results$Date <- gsub("^X", "", combined_results$Date)

    return(combined_results)
}



#####################################################################################################################



#' Process Single Formula for World Panel Data
#' 
#' @description
#' Internal function that processes a single formula for multiple countries, handling
#' data loading, frequency adjustments, and formula evaluation.
#'
#' Used by: wp_data
#' Uses: .extract_symbols, .determine_symbol_frequencies, .check_missing_data,
#'       .adjust_columns_to_frequency, .create_symbol_environment
#'
#' Features:
#' - Single formula processing
#' - Multi-country handling
#' - Mixed frequency support
#' - Formula evaluation
#' - Data validation
#' - Reference preservation
#' - Missing data detection
#' - Seasonal adjustment option
#'
#' @param countries character vector; ISO codes
#' @param formula character; mathematical expression
#' @param variable character; name for formula output
#' @param data data.frame; filtered dataset
#' @param adjust_seasonal logical; apply seasonal adjustment
#' @param reference logical; include source citations
#' @param matching_yq character; method for mixed frequencies
#' @param interpolation_method character; Y2Q conversion method
#' @param verbose logical; print processing information
#' @param debug logical; print debug information
#'
#' @return data.frame containing processed data with columns:
#'   ISO, Date, Variable, Value, Reference (if reference=TRUE)
#'
#' @keywords internal
.data_one_formula <- function(countries,                  # ISO3 codes
                              formula,                      # mathematical expression
                              variable,                     # output name
                              data,                         # filtered input data
                              adjust_seasonal = FALSE,      # TRUE/FALSE
                              reference = TRUE,             # TRUE/FALSE
                              matching_yq = "Q2Y",         # "Q2Y" or "Y2Q"
                              interpolation_method = "Linear", # "None", "Linear", "Linear-Scale"
                              na2zero = FALSE,              # TRUE/FALSE
                              verbose = TRUE,               # TRUE/FALSE
                              debug = FALSE) {             # TRUE/FALSE
    
    # Note: This step is assumed to be performed before calling this function

    ###############
    # STEP 3: Extract symbols from the formula and keep only these data
    symbols <- .extract_symbols(formula)
    
    msg <- paste0("Step 3: Symbols: ", paste0(symbols, "  ", collapse = " "))
    mvcommon::mv_debug(msg, FALSE, debug, "debug")
    
    # Filter data to keep only rows with symbols present in the formula
    data <- data[data$Symbol %in% symbols, ]

    # Get year range from data columns
    date_cols <- grep("^(X)?(18|19|20)\\d{2}(Q[1-4])?$", colnames(data), value = TRUE)
    years <- range(as.numeric(gsub("^X(\\d{4}).*$", "\\1", date_cols)))

    na_matrix <- is.na(data[, date_cols])
    if (na2zero && sum(na_matrix) > 0) {
        mvcommon::mv_debug(paste0("Transform all NAs into zeros - na2zero is ", na2zero,"."), verbose, debug, "info", "Step 10")
        data[,date_cols][na_matrix] <- 0
    }

    all_combinations <- expand.grid(ISO = countries, Symbol = symbols)
    existing_combinations <- unique(data[, c("ISO", "Symbol")])
    # Find missing combinations using merge
    missing_combinations <- all_combinations[
        !paste(all_combinations$ISO, all_combinations$Symbol) %in%
        paste(existing_combinations$ISO, existing_combinations$Symbol),
    ]

    

    # Create empty rows if data are not included in DATA_Q and DATA_Y
    # This is particularly useful when na2zero = TRUE, to enable to aggregrate (sum/mean) when missing rows suggest 0s
    if(nrow(missing_combinations) > 0) {
        # print(" ============================================================== ")
        # print(" --- Missing combinations ----")
        # print(missing_combinations)
        replace_value <- if (na2zero) 0 else NA
        new_row <- data.frame(matrix(ncol = ncol(data), nrow = 0))
        colnames(new_row) <- colnames(data)
        for(i in 1:nrow(missing_combinations)) {
            sym <- as.character(missing_combinations$Symbol[i])
            new_row[i, ] <- replace_value  # First fill everything with NA/0s
            new_row[i, "ISO"] <- as.character(missing_combinations$ISO[i])
            new_row[i, "Indicator"] <- in_from_symbol(sym, "Indicator")
            new_row[i, "Symbol"] <- sym
            new_row[i, "Origin"] <- in_from_symbol(sym, "Origin")
            new_row[i, "Reference"] <- in_from_symbol(sym, "Reference")
            new_row[i, "Frequency"] <- in_from_symbol(sym, "Frequency")
        }

        # print(" --- BEFORE ----")
        # print(data)
        # print(" --- AFTER ----")
        data <- rbind(data, new_row)
        # print(data)
        # print(" ============================================================== ")
    }


    ###############
    # STEP 4: Determine frequency
    frequencies <- .determine_symbol_frequencies(data, symbols)

    msg <- paste0("Step 4: Frequencies: ", paste0(frequencies, "  ", collapse = " "))
    mvcommon::mv_debug(msg, FALSE, debug, "debug")


    msg <- paste0("Step 3-4: Formula: ", formula, "  --  Symbols: ", paste0(symbols, " (", frequencies ,")  ", collapse = " "))
    mvcommon::mv_debug(msg, verbose, FALSE, "info_loop")


    # Check for missing data (consolidated check)
    .check_missing_data(data, countries, symbols, formula, years, verbose)


    ###############
    # STEP 5: Adjust to frequency
    mvcommon::mv_debug("Step 5: Adjust frequencies.", verbose, debug, "info_loop")

    data <- .adjust_columns_to_frequency(data = data, 
                                           frequencies = frequencies, 
                                           symbols = symbols, 
                                           matching_yq = matching_yq,
                                           interpolation_method = interpolation_method,
                                           verbose = verbose,
                                           debug = debug
    )

    ###############
    # STEP 5.2: Prepare quarter columns
    date_cols <- grep("^(X)?(18|19|20)\\d{2}(Q[1-4])?$", colnames(data), value = TRUE)

    ###############
    # STEP 6-7: Data Processing
    # Prepare the result dataframe
    result_data <- data.frame(Country = character(),
                              ISO = character(),
                              Variable = character(),
                              Date = as.Date(character()),
                              Value = numeric())

    mvcommon::mv_debug("Step 6-7: Data Processing (Loop for each country).", verbose, debug, "info_loop")
    for (country in countries) {
        mvcommon::mv_debug(paste0("Step 6.0: Country: ", country), FALSE, debug, "debug")
        country_data <- data[data$ISO == country, ]

        # # check that all data are present / and replace by NAs if not
        # missing_symbols <- setdiff(symbols, country_data$Symbol)
        # # print(paste("== ISO:", country, " | missing_symbol = ", missing_symbols))
        # for (missing_symbol in missing_symbols) {
        #     new_row <- as.data.frame(t(rep(NA, ncol(country_data))), stringsAsFactors = FALSE)
        #     colnames(new_row) <- colnames(country_data) # Ensure column names match
        #     new_row$ISO <- country
        #     new_row$Symbol <- missing_symbol
        #     country_data <- rbind(country_data, new_row)
        # }

        mvcommon::mv_debug("Step 6.1: Create an environment for each formula.", FALSE, debug, "debug")
        # STEP 6
        # Create an environment with the data for each symbol
        env <- .create_symbol_environment(country_data, symbols, date_cols)

        mvcommon::mv_debug("Step 6.2: Evaluate the formula.", FALSE, debug, "debug")
        # Evaluate the formula
        result <- eval(parse(text = formula), envir = env)

        # STEP 7
        # Create country result
        mvcommon::mv_debug("Step 7: Create country result and merge.", FALSE, debug, "debug")
        country_result <- data.frame(
                                ISO = country_data$ISO[1],
                                Variable = variable,
                                Date = date_cols,
                                Value = result)

        # Only merge if country_result was successfully created
        if (!is.null(country_result)) {
            result_data <- rbind(result_data, country_result)
        }
    }


    # To end the loop for this indicator
    mvcommon::mv_debug("", verbose, debug, "info_loop")

    if(reference){
        result_data$Reference <- paste(unique(data$Reference), collapse = "; ")
    }

    return(result_data)
}



#####################################################################################################################


# STEP 8: AGGREGATE

#' Aggregate Data Across Countries
#' 
#' @description
#' Internal function that aggregates panel data across countries using various
#' statistical methods, with optional quartile calculations.
#'
#' Used by: wp_data
#' Uses: None
#'
#' Features:
#' - Multiple aggregation methods (Sum/Mean/Median)
#' - Optional quartile calculations
#' - NA handling options
#' - Group-wise processing
#' - ISO code generation for aggregates
#' - Reference preservation
#' - Date consistency maintenance
#' - Automatic warning generation
#'
#' @param df data.frame; input data with standard columns
#' @param method character; aggregation method
#' @param quartile logical; include quartile calculations
#' @param na.rm logical; handle missing values
#'
#' @return data.frame with same structure as input, aggregated by method
#'
#' @keywords internal
.aggregate_countries <- function(df,                     # input dataframe
                                method = "Median",         # "Sum", "Mean", "Median"
                                quartile = FALSE,          # TRUE/FALSE
                                na.rm = TRUE) {           # TRUE/FALSE


    # Define ISO codes based on method
    iso_codes <- list("Sum" = "SUM",
                      "Mean" = "MEA",
                      "Median" = "MED",
                      "First Quartile" = "Q1L",
                      "Third Quartile" = "Q3L")

    if (length(unique(df$ISO)) == 1) {
        df$ISO <- iso_codes[[method]]
        return(df)
    }

    # Input validation
    if (!method %in% c("Sum", "Mean", "Median")) {
        stop("Method must be one of: 'Sum', 'Mean', or 'Median'")
    }
    if (!is.logical(quartile)) {
        stop("quartile must be TRUE or FALSE")
    }
    if (!is.logical(na.rm)) {
        stop("na.rm must be TRUE or FALSE")
    }

    # Get unique variable
    unique_vars <- unique(df$Variable)

    # Initialize list to store results
    results_list <- list()


    # Safe quantile function that returns NA if calculation fails
    safe_quantile <- function(x, prob, na.rm = TRUE) {
        result <- try(quantile(x, prob, na.rm = na.rm), silent = TRUE)
        if (inherits(result, "try-error")) {
            return(NA)
        }
        return(result)
    }

    # Process each variable separately
    for (var in unique_vars) {
        # Subset data for current variable
        subset_df <- df[df$Variable == var, ]

        # Create a list to store date-wise calculations
        date_results <- list()

        # Process each date separately
        for (d in unique(subset_df$Date)) {
            date_data <- subset_df[subset_df$Date == d, ]
            values <- date_data$Value
            # cat("\n\n values: \n")
            # print(values)

            if (method == "Sum") {
                result <- data.frame(
                                     Country = method,
                                     ISO = iso_codes[[method]],
                                     Variable = var,
                                     Date = d,
                                     Value = sum(values, na.rm = na.rm)
                )
                result$Reference <- if (!is.null(df$Reference)) date_data$Reference[1] else NULL 
                date_results[[as.character(d)]] <- result

            } else {
                # Calculate central tendency and quartiles if needed
                central_value <- if(method == "Mean") {
                    mean(values, na.rm = na.rm)
                } else {
                    median(values, na.rm = na.rm)
                }


                result <- data.frame(
                                     Country = method,
                                     ISO = iso_codes[[method]],
                                     Variable = var,
                                     Date = d,
                                     Value = central_value
                )

                if (quartile) {
                    q1_value <- safe_quantile(values, 0.25, na.rm = na.rm)
                    q3_value <- safe_quantile(values, 0.75, na.rm = na.rm)

                    # Create three rows for central tendency and quartiles
                    result <- rbind(
                                    result,
                                    data.frame(
                                               Country = "First Quartile",
                                               ISO = iso_codes[["First Quartile"]],
                                               Variable = var,
                                               Date = d,
                                               Value = q1_value
                                               ),
                                    data.frame(
                                               Country = "Third Quartile",
                                               ISO = iso_codes[["Third Quartile"]],
                                               Variable = var,
                                               Date = d,
                                               Value = q3_value
                                    )
                    )
                }

                result$Reference <- if (!is.null(df$Reference)) date_data$Reference[1] else NULL 
                date_results[[as.character(d)]] <- result
            }
        }

        # Combine all dates for this variable
        var_result <- do.call(rbind, date_results)
        results_list[[var]] <- var_result
    }

    # Combine all results
    final_result <- do.call(rbind, results_list)

    # Reset row names
    rownames(final_result) <- NULL

    # Reorder columns to match input
    final_result <- final_result[, colnames(df)]

    return(final_result)
}


#' Aggregate Data Across Time Periods
#' 
#' @description
#' Internal function that aggregates time series data using various statistical 
#' and growth-based methods.
#'
#' Used by: wp_data
#' Uses: calculate_yoy_growth, geometric_mean
#'
#' Features:
#' - Multiple aggregation methods (Sum/Mean/Median/SD)
#' - Growth calculations (CAGR, period-over-period)
#' - Geometric mean calculations
#' - Optional quartile calculations
#' - NA handling options
#' - Period range labeling
#' - Warning generation for insufficient data
#' - Negative value handling in growth calculations
#'
#' @param df data.frame; input data with standard columns
#' @param method character; aggregation method
#' @param quartile logical; include quartile calculations
#' @param na.rm logical; handle missing values
#'
#' @return data.frame with aggregated values and time range in Variable names
#'
#' @keywords internal
.aggregate_time <- function(df,                         # input dataframe
                           method = "Mean",               # "Sum", "Mean", "Median", "SD", "Growth", "CAGR", "GeoMean" 
                           quartile = FALSE,              # TRUE/FALSE
                           na.rm = TRUE) {               # TRUE/FALSE

    # Input validation
    if (!method %in% c("Sum", "Mean", "Median", "SD", "Growth", "CAGR", "GeoMean")) {
        stop("Method must be one of: 'Sum', 'Mean', 'Median', 'SD', 'Growth', 'CAGR', or 'GeoMean'")
    }
    if (!is.logical(quartile)) {
        stop("quartile must be TRUE or FALSE")
    }
    if (!is.logical(na.rm)) {
        stop("na.rm must be TRUE or FALSE")
    }

    # Get unique variable
    unique_vars <- unique(df$Variable)
    
    # Initialize list to store results
    results_list <- list()

    # Process each variable separately
    for (var in unique_vars) {
        subset_df <- df[df$Variable == var, ]
        
        # Extract years
        dates <- subset_df$Date
        years <- as.numeric(gsub("X", "", dates))
        year_range <- paste(min(years), max(years), sep = "-")
        
        # Group by ISO code
        iso_groups <- split(subset_df, subset_df$ISO)
        
        # Process each ISO group
        iso_results <- lapply(iso_groups, function(iso_data) {
            # Sort data by date
            iso_data <- iso_data[order(iso_data$Date), ]
            values <- iso_data$Value/100  # Convert percentages to decimals
            
            if (method == "Growth") {
                # Check for sufficient data points
                non_na_values <- values[!is.na(values)]
                if (length(non_na_values) < 3) {
                    warning_msg <- paste("Insufficient data points for Growth calculation for ISO:",
                              iso_data$ISO[1], ". Minimum 3 points required. Returning NA.")
                    mvcommon::mv_debug(warning_msg, TRUE, FALSE, "warning")
                    agg_value <- NA
                } else {

                    if (length(non_na_values) < 10) {
                        warning_msg <- paste("Less than 10 data points for Growth calculation for ISO:",
                                           iso_data$ISO[1], ". Results may be less reliable.")
                        mvcommon::mv_debug(warning_msg, TRUE, FALSE, "warning")
                    }
                    
                    # Check for negative values
                    if (any(values < 0, na.rm = TRUE)) {
                        warning_msg <- paste("Negative values detected for", var, "in ISO:", iso_data$ISO[1], 
                                           ". Growth calculation not suitable for negative values. Returning NA.")
                        mvcommon::mv_debug(warning_msg, TRUE, FALSE, "warning")
                        agg_value <- NA
                    } else {
                        # Calculate growth rates and geometric mean
                        growth_rates <- md_calculate_yoy_growth(iso_data)
                        if (length(growth_rates) > 0) {
                            agg_value <- md_geometric_mean(growth_rates) * 100  # Convert back to percentage
                        } else {
                            agg_value <- NA
                        }
                    }
                }
        
                
            } else if (method == "CAGR") {
                # Find first and last non-NA values
                non_na_indices <- which(!is.na(values))
                if (length(non_na_indices) < 2) {
                    stop(paste("Insufficient data points for CAGR calculation for ISO:",
                              iso_data$ISO[1], ". Need at least first and last values."))
                }
                
                first_val <- values[non_na_indices[1]]
                last_val <- values[non_na_indices[length(non_na_indices)]]
                
                # Check for negative values
                if (first_val < 0 || last_val < 0) {
                    warning_msg <- paste("Negative values detected for", var, "in ISO:", iso_data$ISO[1], 
                                       ". CAGR calculation not suitable for negative values. Returning NA.")
                    mvcommon::mv_debug(warning_msg, TRUE, FALSE, "warning")
                    agg_value <- NA
                } else {
                    first_year <- years[non_na_indices[1]]
                    last_year <- years[non_na_indices[length(non_na_indices)]]
                    years_diff <- last_year - first_year
                    
                    # Calculate CAGR
                    agg_value <- ((last_val/first_val)^(1/years_diff) - 1) * 100  # Convert to percentage
                }
                
            } else if (method == "GeoMean") {
                if (any(values < -1, na.rm = TRUE)) {  # Check if any value < -100%
                    warning_msg <- paste("Values below -100% detected for", var, "in ISO:", iso_data$ISO[1], 
                                       ". Geometric mean calculation not suitable for such values. Returning NA.")
                    mvcommon::mv_debug(warning_msg, TRUE, FALSE, "warning")
                    agg_value <- NA
                } else {
                    agg_value <- md_geometric_mean(values)*100   # Convert back to percentage
                }
            } else {
                # Original calculation methods
                agg_value <- switch(method,
                    "Sum" = sum(values * 100, na.rm = na.rm),  # Convert back to percentage
                    "Mean" = mean(values * 100, na.rm = na.rm),  # Convert back to percentage
                    "Median" = median(values * 100, na.rm = na.rm),  # Convert back to percentage
                    "SD" = sd(values * 100, na.rm = na.rm)  # Convert back to percentage
                )
            }
            
            # Create base result row
            base_result <- data.frame(
                ISO = iso_data$ISO[1],
                Variable = paste0(var, " (", tolower(method), " ", year_range, ")"),
                Value = agg_value,
                stringsAsFactors = FALSE
            )
            
            if (!is.null(df$Reference)) {
                base_result$Reference <- paste(unique(sort(unlist(strsplit(iso_data$Reference, "; ")))), collapse = "; ")
            } 

            # Add quartiles if requested and not SD method
            if (quartile && !method %in% c("Sum", "SD")) {
                if (method %in% c("Growth", "CAGR")) {
                    growth_rates <- md_calculate_yoy_growth(iso_data) * 100  # Use actual growth rates for quartiles
                    q1_value <- quantile(growth_rates, 0.25, na.rm = na.rm)
                    q3_value <- quantile(growth_rates, 0.75, na.rm = na.rm)
                } else {
                    q1_value <- quantile(values * 100, 0.25, na.rm = na.rm)
                    q3_value <- quantile(values * 100, 0.75, na.rm = na.rm)
                }
                
                # Create quartile rows
                q1_row <- base_result
                q3_row <- base_result
                q1_row$Variable <- paste0(var, " (First quartile ", year_range, ")")
                q3_row$Variable <- paste0(var, " (Third quartile ", year_range, ")")
                q1_row$Value <- q1_value
                q3_row$Value <- q3_value
                
                result <- rbind(base_result, q1_row, q3_row)
            } else {
                result <- base_result
            }
            
            return(result)
        })
        
        # Combine results for this variable
        var_result <- do.call(rbind, iso_results)
        results_list[[var]] <- var_result
    }
    
    # Combine all results
    final_result <- do.call(rbind, results_list)
    rownames(final_result) <- NULL
    
    # Reorder columns to match input
    final_result <- final_result[, colnames(df)[!colnames(df) %in% "Date"]]
    
    return(final_result)
}

# NOTE: calculate_yoy_growth and geometric_mean functions are now in separate files:
# - md_calculate_yoy_growth.r: md_calculate_yoy_growth()
# - md_geometric_mean.r: md_geometric_mean()



# STEP 6 - Evaluate the formula

#' Create Environment for Formula Evaluation
#' 
#' @description
#' Internal helper function that creates an environment containing symbol values
#' for formula evaluation.
#'
#' Used by: .data_one_formula
#' Uses: None
#'
#' Features:
#' - Creates clean evaluation environment
#' - Maps symbols to their values
#' - Handles missing symbols gracefully
#' - Preserves numeric precision
#'
#' @param country_data data.frame; source data for one country
#' @param symbols character; vector of symbols to extract
#' @param quarter_cols character; vector of quarter column names
#'
#' @return environment containing symbol values
#'
#' @keywords internal
.create_symbol_environment <- function(country_data,    # source country data
                                       symbols,           # symbols to extract
                                       quarter_cols) {    # quarter column names

    env <- new.env()
    for (sym in symbols) {
        sym_data <- country_data[country_data$Symbol == sym, quarter_cols]
        # print(paste0(" -- sym_data=", paste0(sym_data, collapse = ","), "  |  nrow(sym_data)=", nrow(sym_data), "  |  length(sym_data)=", length(sym_data)  ))
        if (length(sym_data) == 1) {
            env[[sym]] <- as.numeric(sym_data)
        } else if (length(sym_data) > 1) {
            env[[sym]] <- as.numeric(sym_data[1, ])
        } else {
            # warning(paste("[.create_symbol_environment] No data found for symbol:", sym, "in country:", country_data$ISO[1]))
            env[[sym]] <- rep(NA, length(quarter_cols))
        }
    }
    return(env)
}



# STEP 5 - Adjust Columns to frequency

#' Adjust Data Columns for Frequency Matching
#' 
#' @description
#' Internal function that handles conversion between quarterly and yearly data
#' frequencies.
#'
#' Used by: .data_one_formula
#' Uses: .process_yearly_data, .match_Q2Y, .match_Y2Q
#'
#' Features:
#' - Handles Q-to-Y and Y-to-Q conversions
#' - Supports multiple interpolation methods
#' - Preserves data relationships
#' - Maintains column naming conventions
#'
#' @param data data.frame; input dataset
#' @param frequencies character; frequencies for each symbol
#' @param symbols character; vector of symbols to process
#' @param matching_yq character; frequency matching method
#' @param interpolation_method character; interpolation method for Y2Q
#' @param verbose logical; print progress information
#' @param debug logical; print debug information
#'
#' @return data.frame with adjusted frequencies
#'
#' @keywords internal
.adjust_columns_to_frequency <- function(data,                    # input dataset
                                         frequencies,               # "Q" or "Y" for each symbol
                                         symbols,                   # symbols to process
                                         matching_yq = "Q2Y",      # "Q2Y" or "Y2Q"
                                         interpolation_method = "Linear", # "None", "Linear", "Linear-Scale"
                                         verbose = TRUE,           # TRUE/FALSE
                                         debug = FALSE) {          # TRUE/FALSE

    # Handle different frequency scenarios
    if (all(frequencies == "Q")) {
        # Adjust column names for quarterly data
        quarter_cols <- grep("^X\\d{4}Q[1-4]$", names(data), value = TRUE)
        # new_names <- sapply(quarter_cols, quarter_to_date)
        new_names <- substr(quarter_cols, 2, 7)
        colnames(data)[colnames(data) %in% quarter_cols] <- new_names
    } else if (all(frequencies == "Y")) {
        data <- .process_yearly_data(data)
    } else {
        # Handle mixed frequencies
        if (length(symbols) != length(frequencies)) {
            stop("The number of symbols does not match the number of frequencies provided.")
        }
        mvcommon::mv_debug("Mixed frequencies detected.", verbose, debug, type = "info_loop")

        if(matching_yq == "Q2Y"){
            data <- .match_Q2Y(data, frequencies, symbols, verbose, debug)
        }else if(matching_yq == "Y2Q"){
            data <- .match_Y2Q(data, frequencies, symbols, interpolation_method = interpolation_method, verbose, debug)
        }

    }
    
    return(data)
}

#' Process Yearly Data Format
#' 
#' @description
#' Internal function that reformats yearly data to match quarterly data structure
#' by duplicating yearly values in Q1.
#'
#' Used by: .adjust_columns_to_frequency
#' Uses: None
#'
#' Features:
#' - Copies yearly values to Q1
#' - Removes Q2-Q4 columns
#' - Maintains data integrity
#' - Preserves metadata columns
#'
#' @param symbol_data data.frame; yearly data to process
#'
#' @return data.frame with quarterly structure
#'
#' @keywords internal
.process_yearly_data <- function(symbol_data) {        # yearly data to process

    year_cols <- grep("^X\\d{4}Q1$", names(symbol_data), value = TRUE)
    new_names <- as.integer(substr(year_cols, 2, 5))

    # Remove Q2, Q3, and Q4 columns
    cols_to_remove <- grep("^X\\d{4}Q[2-4]$", names(symbol_data), value = TRUE)
    symbol_data <- symbol_data[, !(names(symbol_data) %in% cols_to_remove)]

    # Rename Q1 columns
    colnames(symbol_data)[colnames(symbol_data) %in% year_cols] <- new_names
    return(symbol_data)
}


#' Convert Quarterly Data to Yearly Frequency
#' 
#' @description
#' Internal function that converts quarterly data to yearly frequency by averaging
#' quarterly values.
#'
#' Used by: .adjust_columns_to_frequency
#' Uses: .process_yearly_data
#'
#' Features:
#' - Averages quarterly values
#' - Handles missing quarters
#' - Maintains series relationships
#' - Updates frequency indicators
#'
#' @param data data.frame; input dataset
#' @param frequencies character; frequencies for each symbol
#' @param symbols character; vector of symbols to process
#' @param verbose logical; print progress information
#' @param debug logical; print debug information
#'
#' @return data.frame with yearly frequency
#'
#' @keywords internal
.match_Q2Y <- function(data,                           # input dataset
                        frequencies,                      # "Q" or "Y" for each symbol
                        symbols,                          # symbols to process
                        verbose = TRUE,                   # TRUE/FALSE
                        debug = FALSE) {                  # TRUE/FALSE

    mvcommon::mv_debug("Using .match_Q2Y to match frequencies: Adjusting data quarterly data to yearly frequency.", verbose, debug, type = "info_loop")

    list_symbol_data <- list()
    for (i in seq_along(symbols)) {
        symbol <- symbols[i]
        freq <- frequencies[i]
        symbol_data <- data[data$Symbol == symbol, ]

        if (freq == "Y") {
            symbol_data <- .process_yearly_data(symbol_data)
            mvcommon::mv_debug(paste("Symbol", symbol, "already in yearly frequency."), verbose, debug, type = "info_loop")
        } else if (freq == "Q") {
            quarter_cols <- grep("^X\\d{4}Q[1-4]$", names(symbol_data), value = TRUE)
            years <- unique(substr(quarter_cols, 2, 5))
            
            for (year in years) {
                q_cols <- grep(paste0("^X", year, "Q[1-4]$"), names(symbol_data), value = TRUE)
                if (length(q_cols) > 0) {
                    mean_value <- mean(as.numeric(symbol_data[1, q_cols]), na.rm = TRUE)
                    symbol_data[1, paste0("X", year, "Q1")] <- mean_value
                }
            }
            
            symbol_data <- .process_yearly_data(symbol_data)
            mvcommon::mv_debug(paste("Symbol", symbol, "converted from quarterly to yearly frequency."), verbose, debug, type = "info_loop")
        } else {
            stop(paste("Unsupported frequency", freq, "for symbol", symbol))
        }
        
        
        # Update the main data frame
        list_symbol_data[[i]] <- symbol_data



    }
    
    # Combine all processed symbol data into a single data frame
    data <- do.call(rbind, list_symbol_data)
    data$Frequency <- "Y"
    return(data)

}


#' Convert Yearly Data to Quarterly Frequency
#' 
#' @description
#' Internal function that converts yearly data to quarterly frequency using
#' specified interpolation method.
#'
#' Used by: .adjust_columns_to_frequency
#' Uses: .linear_interpolate_quarterly
#'
#' Features:
#' - Multiple interpolation methods
#' - Q1 value preservation option
#' - Scale preservation option
#' - Handles missing years
#'
#' @param data data.frame; input dataset
#' @param frequencies character; frequencies for each symbol
#' @param symbols character; vector of symbols to process
#' @param interpolation_method character; interpolation method
#' @param verbose logical; print progress information
#' @param debug logical; print debug information
#'
#' @return data.frame with quarterly frequency
#'
#' @keywords internal
.match_Y2Q <- function(data,                           # input dataset
                        frequencies,                      # "Q" or "Y" for each symbol
                        symbols,                          # symbols to process
                        interpolation_method = "Linear",  # "None", "Linear", "Linear-Scale"
                        verbose = TRUE,                   # TRUE/FALSE
                        debug = FALSE) {                  # TRUE/FALSE


    mvcommon::mv_debug("Using .match_Y2Q to match frequencies: Adjusting yearly data to quarterly frequency.", verbose, debug, type = "info_loop")
    # cat("\n\n===========================================================================\n\n")
    # cat("Date input of .match_Y2Q:\n")
    # print(dim(data))
    # print(data)
    # cat("\n\n===========================================================================\n\n")
    list_symbol_data <- list()
    
    for (i in seq_along(symbols)) {
        symbol <- symbols[i]
        freq <- frequencies[i]
        symbol_data <- data[data$Symbol == symbol, ]
        

        if (freq == "Q") {
            mvcommon::mv_debug(paste("Symbol", symbol, "already in quartely frequency."), verbose, debug, type = "info_loop")
        } else if (freq == "Y") {
            q1_cols <- grep("^X\\d{4}Q1$", names(symbol_data), value = TRUE)
            
            for (q1_col in q1_cols) {
                year <- substr(q1_col, 2, 5)
                q1_value <- symbol_data[[q1_col]]
                
                # Fill Q2, Q3, and Q4 with the same value as Q1
                for (quarter in 2:4) {
                    q_col <- paste0("X", year, "Q", quarter)
                    symbol_data[[q_col]] <- q1_value
                }
            }
            
            # Perform interpolation if method is not "None"
            if (interpolation_method != "None") {
                date_cols <- grep("^X\\d{4}Q[1-4]$", names(symbol_data), value = TRUE)
                interpolation_data <- symbol_data[, date_cols]
                
                # cat("interpolation_data:\n")
                # print(dim(interpolation_data))
                # print(interpolation_data)
                interpol_data <<- interpolation_data

                if (interpolation_method == "Linear") {
                    interpolation_data <- .linear_interpolate_quarterly(interpolation_data, scale = FALSE)
                } else if (interpolation_method == "Linear-Scale") {
                    interpolation_data <- .linear_interpolate_quarterly(interpolation_data, scale = TRUE)
                } else {
                    stop("Invalid interpolation method. Choose 'None', 'Linear', 'Linear-Scale'.")
                }
                interpolated_data <<- interpolation_data

                
                symbol_data[, date_cols] <- interpolation_data
            }
            
            mvcommon::mv_debug(paste("Symbol", symbol, "converted from yearly to quarterly frequency using", interpolation_method, "interpolation"), verbose, debug, type = "info_loop")
        } else {
            stop(paste("Unsupported frequency", freq, "for symbol", symbol))
        }
        
        list_symbol_data[[i]] <- symbol_data
    }
    
    # print(list_symbol_data)
    # Combine all processed symbol data into a single data frame
    data <- do.call(rbind, list_symbol_data)


    # Change column names for dates by removing the 'X' at the beginning
    date_cols <- grep("^X\\d{4}Q[1-4]$", names(data), value = TRUE)
    new_names <- substr(date_cols, 2, nchar(date_cols))
    names(data)[names(data) %in% date_cols] <- new_names

    data$Frequency <- "Q"
    return(data)
}


#' Interpolate Quarterly Values
#' 
#' @description
#' Internal function that performs linear interpolation to fill quarterly values
#' from yearly data.
#'
#' Used by: .match_Y2Q
#' Uses: in_find_years_q2na_q3not, in_find_years_q3na_q2not
#'
#' Features:
#' - Linear or scaled interpolation
#' - Edge case handling
#' - Preserves yearly totals option
#' - Handles missing quarters
#'
#' @param data data.frame; input data matrix
#' @param scale logical; preserve yearly totals
#'
#' @return data.frame with interpolated values
#'
#' @keywords internal
.linear_interpolate_quarterly <- function(data,          # input data matrix
                                          scale = FALSE) { # TRUE/FALSE for total preservation


  # Function to interpolate a single row
  interpolate_row <- function(row) {
    # Extract years and values
    years <- as.numeric(substr(names(row), 2, 5))
    unique_years <- unique(years)
    yearly_values <- row[match(unique_years, years)]
    
    # Create a sequence of quarters
    all_quarters <- seq(min(years), max(years), by = 0.25)
    all_quarters_true_time <- seq(min(years)+0.125, max(years)+0.875, by = 0.25)
    
    # Perform linear interpolation
    interpolated_values <- approx(unique_years+0.5, yearly_values, xout = all_quarters_true_time)$y
    
    # Return named vector of interpolated values
    result <- setNames(interpolated_values, names(row))
    

    # missing values for Q2
    # For ends on the left, make Q1 and Q2 values equals to yearly values - gap between Q3 values and yearly values (follow the trend)
    missingQ2_cols <- paste0("X",in_find_years_q2na_q3not(result),"Q2")
    missingQ2_cols_Q3 <- paste0("X",in_find_years_q2na_q3not(result),"Q3")
    missingQ2_cols_Q1 <- paste0("X",in_find_years_q2na_q3not(result),"Q1")

    gap_missingQ2 <- row[missingQ2_cols_Q3] - result[missingQ2_cols_Q3]
    result[missingQ2_cols] <- as.numeric(row[missingQ2_cols] + gap_missingQ2)
    result[missingQ2_cols_Q1] <- as.numeric(result[missingQ2_cols])


    # missing values for Q3
    # For ends on the right, make Q3 and Q4 values equals to yearly values - gap between Q2 values and yearly values (follow the trend)
    missingQ3_cols <- paste0("X",in_find_years_q3na_q2not(result),"Q3")
    missingQ3_cols_Q2 <- paste0("X",in_find_years_q3na_q2not(result),"Q2")
    missingQ3_cols_Q4 <- paste0("X",in_find_years_q3na_q2not(result),"Q4")

    gap_missingQ3 <- row[missingQ3_cols_Q2] - result[missingQ3_cols_Q2]
    result[missingQ3_cols] <- as.numeric(row[missingQ3_cols] + gap_missingQ3)
    result[missingQ3_cols_Q4] <- as.numeric(result[missingQ3_cols])

    # Scale the values if required
    if (scale) {
    cat("\n\n ====================== SCALING ================== \n\n")
      result_scaled <- result
      for (year in unique_years[-length(unique_years)]) {  # Exclude the last year
        year_indices <- which(floor(all_quarters) == year)
        year_mean <- mean(result[year_indices])
        year_target <- yearly_values[which(unique_years == year)]
        result_scaled[year_indices] <- result[year_indices] * (year_target / year_mean)
      }
      # result <- result_scaled
      result_scaled <- zoo::rollmean(result_scaled, k = 3, fill = "extend", align = "center")
      names(result_scaled) <- names(result)
      result <- result_scaled
      # result <- t(apply(result_scaled, 1, function(x) stats::filter(x, rep(1/4, 4), sides = 2)))
    }

    # Ensure the result has the same number of columns as the input
    missing_cols <- setdiff(names(row), names(result))
    result[missing_cols] <- NA
    result[names(row)]

  }
  
  # Apply the interpolation to each row
  result <- t(apply(data, 1, interpolate_row))
  
  # Convert the result to a data frame
  as.data.frame(result)
}


#' Find Years with Q2 NA and Q3 Not NA
#' 
#' @description
#' Internal helper function that identifies years where Q2 is NA but Q3 is not,
#' used for interpolation edge cases.
#'
#' Used by: .linear_interpolate_quarterly
#' Uses: None
#'
#' Features:
#' - Pattern recognition for Q2/Q3
#' - Column name parsing
#' - Year extraction
#' - NA comparison logic
#'
#' @param data data.frame; matrix of quarterly values
#'
#' @return numeric vector of years matching pattern
#'
#' @keywords internal
in_find_years_q2na_q3not <- function(data) {            # quarterly data matrix

  # Get all column names
  col_names <- names(data)
  
  # Extract years from column names
  years <- unique(substr(col_names, 2, 5))
  
  # Function to check if Q2 is NA and Q3 is not NA for a given year
  check_year <- function(year) {
    q2_col <- paste0("X", year, "Q2")
    q3_col <- paste0("X", year, "Q3")
    
    if (q2_col %in% col_names && q3_col %in% col_names) {
      return(all(is.na(data[[q2_col]])) && !all(is.na(data[[q3_col]])))
    } else {
      return(FALSE)
    }
  }
  
  # Apply the check to all years
  matching_years <- years[sapply(years, check_year)]
  
  return(as.numeric(matching_years))
}

#' Find Years with Q3 NA and Q2 Not NA
#' 
#' @description
#' Internal helper function that identifies years where Q3 is NA but Q2 is not,
#' used for interpolation edge cases.
#'
#' Used by: .linear_interpolate_quarterly
#' Uses: None
#'
#' Features:
#' - Pattern recognition for Q2/Q3
#' - Column name parsing
#' - Year extraction
#' - NA comparison logic
#'
#' @param data data.frame; matrix of quarterly values
#'
#' @return numeric vector of years matching pattern
#'
#' @keywords internal
in_find_years_q3na_q2not <- function(data) {            # quarterly data matrix

  # Get all column names
  col_names <- names(data)
  
  # Extract years from column names
  years <- unique(substr(col_names, 2, 5))
  
  # Function to check if Q3 is NA and Q2 is not NA for a given year
  check_year <- function(year) {
    q2_col <- paste0("X", year, "Q2")
    q3_col <- paste0("X", year, "Q3")
    
    if (q2_col %in% col_names && q3_col %in% col_names) {
      return(all(is.na(data[[q3_col]])) && !all(is.na(data[[q2_col]])))
    } else {
      return(FALSE)
    }
  }
  
  # Apply the check to all years
  matching_years <- years[sapply(years, check_year)]
  
  return(as.numeric(matching_years))
}


# STEP 4 - Determine Frequency

#' Determine Data Frequencies for Symbols
#' 
#' @description
#' Internal function that determines the frequency (yearly/quarterly) for each
#' symbol in the formula.
#'
#' Used by: .data_one_formula
#' Uses: None
#'
#' Features:
#' - Frequency detection
#' - Warning for missing data
#' - Multiple symbol handling
#' - NA for unknown frequencies
#'
#' @param data data.frame; input dataset
#' @param symbols character; vector of symbols to check
#'
#' @return character vector of frequencies
#'
#' @keywords internal
.determine_symbol_frequencies <- function(data,        # input dataset
                                          symbols) {     # symbols to check


    frequencies <- character(length(symbols))
    for (i in seq_along(symbols)) {
        symbol_data <- data[data$Symbol == symbols[i], ]
        if (nrow(symbol_data) > 0) {
            frequencies[i] <- symbol_data$Frequency[1]
        } else {
            frequencies[i] <- NA
            warning(paste("No frequency data found for symbol:", symbols[i]))
        }
    }
    return(frequencies)
}

# STEP 3 - extract symbols

#' Extract Symbols from Formula
#' 
#' @description
#' Internal function that extracts variable symbols from mathematical formula
#' using regex.
#'
#' Used by: .data_one_formula, .load_filter_data
#' Uses: None
#'
#' Features:
#' - Regex pattern matching
#' - Space handling
#' - Unique symbol extraction
#' - Mathematical operator exclusion
#'
#' @param formula character; mathematical expression
#'
#' @return character vector of unique symbols
#'
#' @keywords internal
.extract_symbols <- function(formula) {                # mathematical expression

    # Remove spaces from the formula
    formula <- gsub("\\s+", "", formula)
    
    # Regular expression to match symbols
    # Matches strings that start with a letter, followed by letters, numbers, or underscores
    symbol_pattern <- "(?<![A-Za-z0-9_])([A-Za-z][A-Za-z0-9_]*)"
    
    # Extract all matches
    symbols <- unlist(regmatches(formula, gregexpr(symbol_pattern, formula, perl = TRUE)))
    
    # Return unique symbols
    unique(symbols)
}




# STEP 2 - reduce size of data to only keep relevant countries, columns and symbols + merge yearly and quarterly data

#' Load and Filter Required Data
#' 
#' @description
#' Internal function that loads and filters data based on required symbols
#' and date range.
#'
#' Used by: wp_data
#' Uses: .extract_symbols, .load_frequency_data, .filter_data_by_frequency, .merge_yearly_quarterly
#'
#' Features:
#' - Selective data loading
#' - Memory optimization
#' - Database merging
#' - Column filtering
#'
#' @param countries character; vector of ISO codes
#' @param years numeric; vector of start/end years
#' @param formula character; mathematical expression
#' @param verbose logical; print progress information
#'
#' @return data.frame of filtered data
#'
#' @keywords internal
.load_filter_data <- function(countries,              # ISO codes
                               years,                    # c(start_year, end_year)
                               formula,                  # mathematical expression
                               verbose) {                # TRUE/FALSE


    # Extract symbols from all formulas
    all_symbols <- unique(.extract_symbols(formula))

    # Determine which databases contain our symbols
    required_dbs <- .locate_symbol(all_symbols)

    if (is.null(required_dbs)) {
        stop("None of the symbols were found in either quarterly or yearly databases")
    }

   # Initialize data variables
    data_q <- NULL
    data_y <- NULL
    
    # Load only required databases
    if ("Q" %in% required_dbs) {
        .load_frequency_data(frequency = "Q", 
                               verbose = verbose)
        DATA_Q <- .pkgenv[["DATA_Q"]]
        data_q <- .filter_data_by_frequency(countries, years, DATA_Q, formula, frequency = 'Q')
    }
    
    if ("Y" %in% required_dbs) {
        .load_frequency_data(frequency = "Y",
                               verbose = verbose)
        DATA_Y <- .pkgenv[["DATA_Y"]]
        data_y <- .filter_data_by_frequency(countries, years, DATA_Y, formula, frequency = 'Y')
    }
    
    # Determine final dataset based on what was loaded
    if (is.null(data_y) || dim(data_y)[1] == 0) {
        data <- data_q
    } else if (is.null(data_q) || dim(data_q)[1] == 0) {
        data <- data_y
    } else {
        data <- .merge_yearly_quarterly(data_y, data_q)
    }

    # cat("\n\n\n\n =========================================== \n\n\n")
    #
    # print(paste0("Dim before:", dim(data)[1] ,' x ', dim(data)[2] ))
    # data <- data[, colSums(is.na(data)) < nrow(data)]
    # print(paste0("Dim after:", dim(data)[1] ,' x ', dim(data)[2] ))
    #
    # print(data)
    #
    # cat("\n\n\n\n =========================================== \n\n\n")

    return(data)

}


#' Filter Data by Frequency and Date Range
#' 
#' @description
#' Internal function that filters data for specific frequency (Q/Y) and
#' date range.
#'
#' Used by: .load_filter_data
#' Uses: None
#'
#' Features:
#' - Date range validation
#' - Column subsetting
#' - Automatic range adjustment
#' - Warning generation
#'
#' @param countries character; vector of ISO codes
#' @param years numeric; vector of start/end years
#' @param data data.frame; input dataset
#' @param formula character; mathematical expression
#' @param frequency character; data frequency
#'
#' @return data.frame of filtered data
#'
#' @keywords internal
.filter_data_by_frequency <- function(countries,      # ISO codes
                                      years,            # c(start_year, end_year)
                                      data,             # input dataset
                                      formula,          # mathematical expression
                                      frequency) {      # "Q" or "Y"


  all_columns <- names(data)

  if(frequency == 'Q'){
      start_column <- paste0("X", years[1], "Q1")
      end_column <- paste0("X", years[2], "Q4")
      period_columns <- all_columns[grep("^X(18|19|20)\\d{2}Q[1-4]$", all_columns)]
  }else if(frequency == 'Y'){
      start_column <- paste0("X", years[1])
      end_column <- paste0("X", years[2])
      period_columns <- all_columns[grep("^X(18|19|20)\\d{2}$", all_columns)]
  }else{
      stop(paste("Invalid frequency:", frequency))
  }
  
  # Check if start and end columns exist in data
  
  if (!start_column %in% period_columns) {
    potential_start <- min(period_columns)
    if (substr(potential_start, 2, 5) > years[1]) {
      warning(paste("Start year", years[1], "not available. Using earliest available year:", substr(potential_start, 2, 5)))
      start_column <- potential_start
    } else {
      stop(paste("Invalid start year:", years[1]))
    }
  }
  
  if (!end_column %in% period_columns) {
    potential_end <- max(period_columns)
    if (substr(potential_end, 2, 5) < years[2]) {
      warning(paste("End year", years[2], "not available. Using latest available year:", substr(potential_end, 2, 5)))
      end_column <- potential_end
    } else {
      stop(paste("Invalid end year:", years[2]))
    }
  }
  
  # Get the indices of start and end columns
  start_index <- which(all_columns == start_column)
  end_index <- which(all_columns == end_column)
  
  # Filter data based on countries and date range
  filtered_data <- data[data$ISO %in% countries, c("ISO", "Indicator", "Symbol", "Origin", "Reference", "Frequency", all_columns[start_index:end_index])]
  

  # Filter data based on symbols
  symbols <- .extract_symbols(formula)
  filtered_data <- filtered_data[filtered_data$Symbol %in% symbols,]


  return(filtered_data)
}


###########################



#' Merge Yearly and Quarterly Datasets
#' 
#' @description
#' Internal function that combines yearly and quarterly data into a single
#' consistent dataset.
#'
#' Used by: .load_filter_data
#' Uses: None
#'
#' Features:
#' - Frequency tracking
#' - NA handling
#' - Column alignment
#' - Row ordering
#'
#' @param DATA_Y data.frame; yearly dataset
#' @param DATA_Q data.frame; quarterly dataset
#'
#' @return data.frame of merged data
#'
#' @keywords internal
.merge_yearly_quarterly <- function(DATA_Y,           # yearly dataset
                                    DATA_Q) {           # quarterly dataset


  # Helper function to create new column names
  create_new_cols <- function(year) {
    paste0("X", year, c("Q1", "Q2", "Q3", "Q4"))
  }

  # Get the range of years
  year_cols_Y <- grep("^X\\d{4}$", names(DATA_Y), value = TRUE)
  min_year <- min(as.numeric(gsub("X", "", year_cols_Y)))
  max_year <- max(as.numeric(gsub("X", "", year_cols_Y)))

  # Create new column names
  new_cols <- unlist(lapply(min_year:max_year, create_new_cols))

  # Create the structure for the merged dataset
  merged_data <- DATA_Y[, c("ISO", "Symbol", "Indicator", "Origin", "Reference")]
  merged_data$Frequency <- "Y"
  merged_data[, new_cols] <- NA

  # Fill in the yearly data
  for (year in year_cols_Y) {
    q1_col <- paste0(year, "Q1")
    merged_data[[q1_col]] <- DATA_Y[[year]]
  }

  # Process quarterly data
  quarter_cols_Q <- grep("^X\\d{4}Q\\d$", names(DATA_Q), value = TRUE)
  

  # print("DATA:")
  # print(paste0("dim(DATA_Y) = ", dim(DATA_Y)[1], ' x ', dim(DATA_Y)[2] ))
  # print(paste0("dim(DATA_Q) = ", dim(DATA_Q)[1], ' x ', dim(DATA_Q)[2] ))
  #
  #
  # print("DATA_Y:")
  # print(DATA_Y)
  #
  # print("DATA_Q:")
  # print(DATA_Q)

  for (i in 1:nrow(DATA_Q)) {
    row_Q <- DATA_Q[i, ]
    match_index <- which(merged_data$ISO == row_Q$ISO & 
                         merged_data$Symbol == row_Q$Symbol)
    
    if (length(match_index) == 0) {
      new_row <- as.list(row_Q[c("ISO", "Symbol", "Indicator", "Origin", "Reference")])
      new_row$Frequency <- "Q"
      new_row[new_cols] <- NA

      # print("merged_data:")
      # print(paste0("dim(merged_data) = ", dim(merged_data)[1], ' x ', dim(merged_data)[2] ))
      # print(merged_data)
      # print("new_row:")
      # print(paste0("dim(new_row) = ", length(new_row)))
      # print(new_row)
      merged_data <- rbind(merged_data, new_row)
      # print("~~~~~~~~~~~~~~~~~~~~~")
      match_index <- nrow(merged_data)
    } else if (merged_data$Frequency[match_index] == "Y") {
      merged_data$Frequency[match_index] <- "Both"
    }
    
    for (col in quarter_cols_Q) {
      if (!is.na(row_Q[[col]])) {
        merged_data[match_index, col] <- row_Q[[col]]
      }
    }
  }

  # Sort the merged data
  merged_data <- merged_data[order(merged_data$ISO, merged_data$Symbol), ]
  
  # Reset row names
  rownames(merged_data) <- NULL

  return(merged_data)
}




# STEP 1

#' Validate Input Parameters
#' 
#' @description
#' Internal function that checks validity of all input parameters before
#' processing.
#'
#' Used by: wp_data
#' Uses: None
#'
#' Features:
#' - Type checking
#' - Length validation
#' - Format verification
#' - Relationship validation
#'
#' @param countries character; vector of ISO codes
#' @param formula character; vector of formulas
#' @param variable character; vector of variable names
#' @param years numeric; vector of years
#' @param adjust_seasonal logical; seasonal adjustment flag
#'
#' @return None (stops with error if validation fails)
#'
#' @keywords internal
.validate_inputs <- function(countries,               # ISO codes
                             formula,                   # mathematical expressions
                             variable,                  # variable names
                             years,                     # c(start_year, end_year)
                             adjust_seasonal) {         # TRUE/FALSE


  # Check if required arguments are provided
  if (missing(countries) || missing(formula) || missing(variable) || missing(years)) {
    stop("Missing required argument(s). Please provide countries, formula, variable, and years.")
  }


  # Validate countries
  if (!is.character(countries) || !is.vector(countries) ||  !all(nchar(countries) == 3) || !all(countries == toupper(countries))) {
    stop("'countries' must be a vector of 3-letter uppercase ISO3 codes.")
  }
  
  # Validate formula
  if (!is.character(formula) || !is.vector(formula)) {
    stop("'formula' must be a vector of character string.")
  }
  
  # Validate variable
  if (!is.character(variable) || !is.vector(variable)) {
    stop("'variable' must be a vector of character string.")
  }

  # Check if formula and variable have the same length
  if (length(formula) != length(variable)) {
    stop("formula and variable must have the same length")
  }


  # Validate years
  if (!is.numeric(years) || !is.vector(years) || length(years) != 2) {
    stop("'years' must be a numeric vector of length 2, e.g., c(2011, 2014).")
  }
  
  # Validate data
  # if (!is.data.frame(data)) {
  #   stop("'data' must be a data frame.")
  # }
  
  # required_columns <- c("Country", "Indicator", "Symbol", "ISO", "Origin", "Reference", "Frequency")
  # missing_columns <- setdiff(required_columns, names(data))
  # if (length(missing_columns) > 0) {
  #   stop(paste("Missing required column(s) in data:", paste(missing_columns, collapse = ", ")))
  # }
  
  # Check for quarter columns
  # quarter_columns <- grep("^X\\d{4}Q[1-4]$", names(data), value = TRUE)
  # if (length(quarter_columns) == 0) {
  #   stop("Data frame should contain columns for specific quarters (e.g., 'X1977Q4', 'X1978Q1').")
  # }
  
  # Validate adjust_seasonal
  if (!is.logical(adjust_seasonal) || length(adjust_seasonal) != 1) {
    stop("'adjust_seasonal' must be a single logical value.")
  }
}


##########################################################
# message missing data

#' Check for Missing Data
#' 
#' @description
#' Internal function that identifies and reports missing data patterns in
#' the dataset.
#'
#' Used by: .data_one_formula
#' Uses: .create_year_ranges
#'
#' Features:
#' - Missing data detection
#' - Pattern identification
#' - Range summarization
#' - Warning generation
#'
#' @param data data.frame; input dataset
#' @param countries character; vector of ISO codes
#' @param symbols character; vector of symbols
#' @param formula character; mathematical expression
#' @param years numeric; vector of years
#' @param verbose logical; print warnings
#'
#' @return None (generates warnings for missing data)
#'
#' @keywords internal
.check_missing_data <- function(data,                 # input dataset
                                countries,               # ISO codes
                                symbols,                 # symbols to check
                                formula,                 # mathematical expression
                                years,                   # year range
                                verbose) {               # TRUE/FALSE


    # Pre-compute year range
    all_years <- years[1]:years[2]
    
    # Process each symbol
    for (symbol in symbols) {
        # Filter data for this symbol
        symbol_data <- data[data$Symbol == symbol, ]
        
        # Track countries with missing data
        missing_data_msgs <- character(0)
        
        for (country in countries) {
            # Get data for this country and symbol
            country_data <- symbol_data[symbol_data$ISO == country, ]
            
            if (nrow(country_data) == 0) {
                # No data at all for this country
                missing_data_msgs <- c(missing_data_msgs, country)
                next
            }
            
            # Get years with data based on frequency
            if (country_data$Frequency[1] == "Q") {
                date_cols <- grep("^X\\d{4}Q[1-4]$", names(country_data), value = TRUE)
                # Check each year's quarters for non-NA values
                years_with_data <- sapply(all_years, function(year) {
                    year_cols <- grep(paste0("^X", year, "Q"), date_cols, value = TRUE)
                    any(!is.na(as.numeric(country_data[1, year_cols])))
                })
                years_with_values <- all_years[years_with_data]
            } else {
                date_cols <- grep("^X\\d{4}Q1$", names(country_data), value = TRUE)
                # Directly check which years have non-NA values
                years_with_values <- all_years[all_years %in% 
                    as.numeric(substr(date_cols[!is.na(as.numeric(country_data[1, date_cols]))], 2, 5))]
            }
            
            if (length(years_with_values) == 0) {
                # No non-NA data at all
                missing_data_msgs <- c(missing_data_msgs, country)
            } else if (length(years_with_values) < length(all_years)) {
                # Some years are missing, get the ranges
                year_ranges <- .create_year_ranges(years_with_values, all_years)
                if (!is.null(year_ranges)) {
                    missing_data_msgs <- c(missing_data_msgs, 
                                         paste0(country, "[", year_ranges, "]"))
                }
            }
        }
        
        # Generate warning if there are missing data messages
        if (length(missing_data_msgs) > 0) {
            msg <- paste0(" ---  No data for ", symbol, 
                         ": ", paste(missing_data_msgs, collapse=", "))
            # warning(msg)
            mvcommon::mv_debug(msg, verbose = verbose, debug = FALSE, type = "missing")

        }
    }
}


#' Create Year Range Strings
#' 
#' @description
#' Internal helper function that creates compact string representations of
#' year ranges.
#'
#' Used by: .check_missing_data
#' Uses: None
#'
#' Features:
#' - Range compression
#' - Efficient string creation
#' - Century handling
#' - Single year handling
#'
#' @param years_with_data numeric; vector of years with data
#' @param all_years numeric; vector of all possible years
#'
#' @return character string of year ranges
#'
#' @keywords internal
.create_year_ranges <- function(years_with_data,      # years with data
                                all_years) {            # all possible years

    # Pre-allocate logical vector instead of using setdiff
    has_data <- logical(length(all_years))
    has_data[match(years_with_data, all_years)] <- TRUE
    
    # If all years missing or all years present, return early
    if (all(!has_data)) return(NULL)
    if (all(has_data)) return(NULL)
    
    # Find runs of missing years using rle (more efficient than diff)
    missing_runs <- rle(!has_data)
    
    if (length(missing_runs$lengths) == 0) return(NULL)
    
    # Pre-calculate start positions
    positions <- cumsum(c(1, missing_runs$lengths[-length(missing_runs$lengths)]))
    
    # Only process runs of FALSE (missing years)
    missing_idx <- which(missing_runs$values)
    
    if (length(missing_idx) == 0) return(NULL)
    
    # Pre-allocate result vector
    ranges <- character(length(missing_idx))
    
    # Create ranges in a single pass
    for (i in seq_along(missing_idx)) {
        idx <- missing_idx[i]
        start_year <- all_years[positions[idx]]
        end_year <- all_years[positions[idx] + missing_runs$lengths[idx] - 1]
        ranges[i] <- if (start_year == end_year) {
            as.character(start_year)
        } else {
            if (substr(start_year, 1, 2) == substr(end_year, 1, 2)) {
                end_year <- substr(end_year, 3, 4) 
            }
            paste(start_year, end_year, sep="-")
        }
    }
    
    # Single paste operation
    paste(ranges, collapse=", ")
}



