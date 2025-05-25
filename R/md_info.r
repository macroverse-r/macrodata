#' Display Information About Available Data Variables
#'
#' @description
#' Provides a comprehensive view of available data variables in the WPD package, organized by data source and frequency.
#' This function is typically used before wp_data() to discover available variables and their codes.
#' Variables are grouped by their origin (data source) and can be filtered or searched.
#'
#' @param frequency Character, either "Q" (quarterly), "Y" (yearly), or NULL (both). If NULL (default),
#'        displays both quarterly and yearly data.
#' @param origin Character vector, filter by data source(s). Multiple origins can be specified.
#'        Invalid origins trigger a warning. Default is NULL (all origins).
#' @param search Character vector, search terms to filter data. Searches both Symbol and Indicator columns.
#'        Case-insensitive, supports partial matches. Maximum length is limited by available colors.
#'        Default is NULL (no search).
#' @param color Logical, whether to use color highlighting for search matches. Default is TRUE.
#' @param special Character, specifies special display modes. Currently only "BOP" (Balance of Payments) 
#'        is supported. Default is NULL.
#' @param print Logical, whether to print the output to console (default = TRUE)
#'
#' @details
#' The function helps users discover available variables through several features:
#'
#' \strong{Variable Naming Patterns:}
#' Common suffixes:
#' \itemize{
#'   \item _C: Current prices/values
#'   \item _R\_YYYY: Real/constant prices (YYYY is base year)
#'   \item _ZS: Percentages/shares
#'   \item _CD: Current US dollars
#'   \item _KN: Local currency units
#'   \item _PC: Per capita values
#'   \item _XD: Indices
#'   \item _MA/\_FE: Male/Female specific
#'   \item _GD: GDP-related
#'   \item _IX: Index values
#'   \item _IN: Count/number
#'   \item _PT: Points/percentages
#'   \item _TOT: Total values
#'   \item _EST: Estimates
#'   \item _ZG: Growth rates
#'   \item _KD: Constant dollars
#'   \item _PPP: Purchasing Power Parity
#'   \item _SA: Seasonally adjusted
#'   \item _AD: Adjusted/linked series
#'   \item _GI: Global/composite indices
#' }
#'
#' Common prefixes:
#' \itemize{
#'   \item NE\_ : National accounts expenditure
#'   \item NY\_ : National accounts income
#'   \item BX\_ : Balance of payments credits/exports
#'   \item BM\_ : Balance of payments debits/imports
#'   \item SL\_ : Labor market indicators
#'   \item SP\_ : Population indicators
#'   \item SE\_ : Education indicators
#'   \item SH\_ : Health indicators
#'   \item FI\_ : Financial reserves
#'   \item EN\_ : Environment indicators
#'   \item IC\_ : Investment climate
#'   \item TX\_ : Trade/export indicators
#'   \item TM\_ : Trade/import indicators
#' }
#'
#' \strong{Balance of Payments Mode (special="BOP"):}
#' When using special="BOP", displays detailed Balance of Payments relationships with specific prefixes:
#' \itemize{
#'   \item i/o: Inward/outward flows
#'   \item DI/POR/OI: Direct Investment/Portfolio/Other Investment
#'   \item E/D: Equity/Debt components
#' }
#'
#' \strong{Data Sources:}
#' The output displays data sources in headers, including major sources like:
#' \itemize{
#'   \item IMF IFS (International Financial Statistics)
#'   \item IMF BOP (Balance of Payments)
#'   \item BIS Statistics
#'   \item World Bank Indicators
#' }
#'
#' \strong{Data Availability:}
#' Some indicators are available in both quarterly (METADATA_Q) and yearly (METADATA_Y) frequencies,
#' while others are specific to one frequency. Both METADATA_Q and METADATA_Y are accessible in the
#' package namespace for direct inspection.
#'
#'
#' \strong{Search Functionality:}
#' The search parameter provides flexible filtering:
#' \itemize{
#'   \item Searches across both Symbol and Indicator columns simultaneously
#'   \item Multiple search terms work as OR condition (matches any term)
#'   \item Case-insensitive and supports partial matches
#'   \item Search highlighting makes matches easily visible (can be disabled with color=FALSE)
#' }
#'
#' \strong{Special Modes:}
#' Currently supports "BOP" (Balance of Payments) mode for detailed payment relationships.
#' Future versions will include additional special modes for other data categories
#' and analytical views.
#'
#' @return 
#' When print=TRUE (default), displays the data in console.
#' Invisibly returns:
#' \itemize{
#'   \item A data frame combining Q and Y data (with Frequency column) when frequency=NULL
#'   \item A single data frame when frequency is specified ("Q" or "Y")
#'   \item For special="BOP", returns the filtered Balance of Payments data frame
#' }
#'
#' @examples
#' # Show quarterly data
#' wp_info("Q")
#'
#' # Filter by origin
#' wp_info(origin = c("BIS", "IMF BOP"))
#'
#' # Search for GDP-related variables
#' wp_info(search = "GDP")
#'
#' # Multiple search terms with color highlighting
#' wp_info(search = c("GDP", "growth"))
#'
#' # Display Balance of Payments relationships
#' wp_info(special = "BOP")
#'
#' # Common workflow with wp_data()
#' # 1. Discover available GDP variables
#' wp_info(search = "GDP")
#' # 2. Use discovered variable in wp_data
#' wp_data(ISO = "USA", 
#'         formula = "GDP_R_2015", 
#'         years = c(2000, 2023))
#'
#'
#' # Combined origin and search filters
#' wp_info(origin = "IMF BOP", search = "investment")
#' 
#' # Search for percentage indicators
#' wp_info(search = "_ZS")
#' 
#' # Search for real/constant price variables
#' wp_info(search = "_R_")
#' 
#' # Multiple search terms finding either pattern
#' wp_info(search = c("GDP", "investment"))
#'
#' # Get data without printing
#' gdp_vars <- wp_info(search = "GDP", print = FALSE)
#'
#' @seealso \code{\link{wp_data}} for using the discovered variables in data retrieval
#' @export

wp_info <- function(frequency = NULL, origin = NULL, search = NULL, color = TRUE, special = NULL, print = TRUE) {

    # Define ANSI color codes
    colors <- list(
        red = "\033[31m",
        orange = "\033[33m",  # Actually yellow but appears orange in most terminals
        purple = "\033[35m",
        green = "\033[32m",
        magenta = "\033[95m",
        brown = "\033[38;5;130m",
        pink = "\033[38;5;205m",
        teal = "\033[38;5;37m",
        gold = "\033[38;5;178m",
        maroon = "\033[38;5;88m",
        reset = "\033[0m"
    )


    # Special cases are managed in in_info_special
    if (!is.null(special)) {
        return(in_info_special(special, frequency, origin, search, color, colors_def = colors, print = print))
    }


    # If single unnamed argument is provided and it's not a valid frequency, treat it as search
    if (!missing(frequency) && is.null(origin) && is.null(search) && 
        !frequency %in% c("Q", "Y", "BOP")) {
        search <- frequency
        frequency <- NULL
    }

    
    # Validate search length
    if (!is.null(search) && length(search) > length(colors) - 1) { # -1 for reset
        stop("Maximum number of search terms is ", length(colors) - 1)
    }
    
    # Function to highlight text with colors
    highlight_text <- function(text, search_terms) {
        if (!color || is.null(search_terms)) return(text)
        
        # Create a list to store positions of matches
        matches <- list()
        
        # Find all matches for each search term
        for (i in seq_along(search_terms)) {
            term <- search_terms[i]
            # Find all matches (case-insensitive)
            starts <- gregexpr(term, text, ignore.case = TRUE)[[1]]
            if (starts[1] != -1) {
                term_length <- nchar(term)
                for (start in starts) {
                    matches[[length(matches) + 1]] <- list(
                        start = start,
                        end = start + term_length - 1,
                        color = names(colors)[i]
                    )
                }
            }
        }
        
        # If no matches, return original text
        if (length(matches) == 0) return(text)
        
        # Sort matches by start position
        matches <- matches[order(sapply(matches, function(x) x$start))]
        
        # Build highlighted text
        result <- ""
        last_pos <- 1
        
        for (match in matches) {
            # Add text before match
            if (match$start > last_pos) {
                result <- paste0(result, substr(text, last_pos, match$start - 1))
            }
            
            # Add colored match
            matched_text <- substr(text, match$start, match$end)
            result <- paste0(
                result,
                colors[[match$color]],
                matched_text,
                colors$reset
            )
            
            last_pos <- match$end + 1
        }
        
        # Add remaining text
        if (last_pos <= nchar(text)) {
            result <- paste0(result, substr(text, last_pos, nchar(text)))
        }
        
        return(result)
    }
    
    # Validate frequency argument
    if (!is.null(frequency)) {
        frequency <- match.arg(frequency, c("Q", "Y"))
        metadata_list <- list(get(paste0("METADATA_", frequency)))
        names(metadata_list) <- frequency
    } else {
        metadata_list <- list(
            Q = METADATA_Q,
            Y = METADATA_Y
        )
    }
    
    # Validate origin argument and warn about non-existent origins
    if (!is.null(origin)) {
        all_origins <- unique(unlist(lapply(metadata_list, function(x) unique(x$Origin))))
        invalid_origins <- setdiff(origin, all_origins)
        if (length(invalid_origins) > 0) {
            warning("The following origins were not found in the data: ", 
                    paste(invalid_origins, collapse = ", "))
        }
    }
    
    # Function to apply search filter
    apply_search_filter <- function(metadata, search_terms) {
        if (is.null(search_terms)) return(metadata)
        
        # For each search term
        for (term in search_terms) {
            # Match in either Symbol or Indicator column (case-insensitive)
            matches <- grepl(term, metadata$Symbol, ignore.case = TRUE) |
                      grepl(term, metadata$Indicator, ignore.case = TRUE)
            metadata <- metadata[matches, ]
        }
        return(metadata)
    }
    
    # Function to print metadata for one frequency
    print_metadata <- function(metadata, freq) {
        # Get total count before any filtering
        total_count <- nrow(metadata)
        
        # Filter by origin if specified
        if (!is.null(origin)) {
            metadata <- metadata[metadata$Origin %in% origin, ]
        }
        
        # Apply search filter after origin filter
        if (!is.null(search)) {
            metadata <- apply_search_filter(metadata, search)
        }
        
        # Store filtered data in parent environment
        filtered_metadata_list[[freq]] <<- metadata

        # Get filtered count
        filtered_count <- nrow(metadata)
        
        if (print) {

            # Print header with counts
            count_text <- if (!is.null(origin) || !is.null(search)) {
                sprintf("%d/%d variables", filtered_count, total_count)
            } else {
                sprintf("%d variables", total_count)
            }
            
            in_print_debug(sprintf("############# %s Data (%s) #############\n", 
                            ifelse(freq == "Q", "Quarterly", "Yearly"),
                            count_text),
                        verbose = TRUE, debug = TRUE, 
                        type = "empty")
            
            # Skip if no data after filtering
            if (filtered_count == 0) return()
            
            # Get unique Origins
            unique_origins <- unique(metadata$Origin)
            
            # Print the results grouped by Origin
            for (origin in rev(unique_origins)) {
                ref <- paste0(unique(metadata[metadata$Origin == origin,]$Reference), 
                             collapse=", ")

                in_print_debug(sprintf("Data from %s", ref),
                        verbose = TRUE, 
                        debug = FALSE,
                        type = "info",
                        text_type = origin)
                
                # Filter data for current Origin
                origin_data <- metadata[metadata$Origin == origin, ]
                
                # Print Symbol and Indicator for current Origin
                for (i in seq_len(nrow(origin_data))) {
                    symbol <- origin_data$Symbol[i]
                    indicator <- origin_data$Indicator[i]
                    # Apply highlighting to both symbol and indicator
                    symbol <- highlight_text(symbol, search)
                    indicator <- highlight_text(indicator, search)
                    cat(sprintf("%-26s | %s\n", symbol, indicator))
                }
                
                cat("\n")
            }
        }
    }
    

    filtered_metadata_list <- list()

    # Print metadata for each frequency
    for (freq in names(metadata_list)) {
        print_metadata(metadata_list[[freq]], freq)
    }
    
    if (length(filtered_metadata_list) > 1) {
        # Combine Q and Y data with frequency column, only including non-empty dataframes
        non_empty_dfs <- lapply(names(filtered_metadata_list), function(freq) {
            df <- filtered_metadata_list[[freq]]
            if (nrow(df) > 0) {
                df$Frequency <- freq
                return(df)
            }
            return(NULL)
        })
        non_empty_dfs <- non_empty_dfs[!sapply(non_empty_dfs, is.null)]
        
        if (length(non_empty_dfs) > 0) {
            combined_data <- do.call(rbind, non_empty_dfs)
            invisible(combined_data)
        } else {
            invisible(data.frame()) # Return empty dataframe if all are empty
        }
    } else {
        invisible(filtered_metadata_list[[1]])
    }

}



#' Display Special Data Information
#' 
#' Internal function to handle special data display cases in wp_info. Currently supports
#' Balance of Payments (BOP) data display with search and highlighting capabilities.
#' 
#' @param special Character string specifying the special data type (currently only "BOP" is supported)
#' @param frequency Character, either "Q", "Y", or NULL (ignored when special is used, triggers warning)
#' @param origin Character vector of data origins (ignored when special is used, triggers warning)
#' @param search Character vector of search terms to filter data (searches across all columns)
#' @param color Logical, whether to use color highlighting (default = TRUE)
#' @param colors_def List of ANSI color codes used for highlighting search matches
#' @param print Logical, whether to print the output to console (default = TRUE)
#' 
#' @return Invisibly returns the displayed data frame
#' 
#' @details
#' This function provides specialized display formatting for different types of data.
#' For BOP data:
#' - Displays data in a formatted table with aligned columns
#' - Supports text wrapping for long entries
#' - Implements search across all columns
#' - Provides color highlighting for search matches
#' - Automatically adjusts to terminal width
#' 
#' The function will issue warnings if:
#' - Terminal width is too narrow for proper display
#' - frequency or origin parameters are provided (as they are ignored)
#' 
#' The function's output can be controlled via the print parameter:
#' - When print=TRUE (default), displays formatted output to console
#' - When print=FALSE, silently returns the filtered data
#'
#' @examples
#' \dontrun{
#' # These are called internally by wp_info:
#' in_info_special("BOP", search = "Export")
#' in_info_special("BOP", search = c("Export", "Import"), color = TRUE)
#' }
#' 
#' @seealso \code{\link{wp_info}}
#' @keywords internal
in_info_special <- function(special, frequency = NULL, origin = NULL, search = NULL, 
                          color = TRUE, colors_def = NULL, print = TRUE) {
    # Validate special parameter
    if (!special %in% c("BOP")) {
        stop("Invalid special parameter. Currently only 'BOP' is supported.")
    }
    
    # Warning if frequency or origin are provided
    if (!is.null(frequency) || !is.null(origin)) {
        warning("When special parameter is used, frequency and origin parameters are ignored.")
    }
    
    # Get terminal width and check if sufficient
    width <- getOption("width")
    min_width <- 120
    if (width < min_width) {
        warning(sprintf(
            "Terminal width (%d) may be too narrow for proper display. Consider increasing with options(width = %d)", 
            width, min_width))
    }
    
    # Get BOP data
    data <- in_info_bop()
    
    # Calculate column widths (accounting for separators " | ")
    available_width <- width - 6  # Remove space for 3 separators
    col_widths <- list(
        Short = floor(available_width * 0.25),    # 25%
        Symbol = floor(available_width * 0.10),   # 10%
        Relation = floor(available_width * 0.30), # 30%
        Original = floor(available_width * 0.35)  # 35%
    )
    
    # Function to highlight text with colors (adapted from wp_info)
    highlight_text <- function(text, search_terms) {
        if (!color || is.null(search_terms) || is.null(colors_def)) return(text)
        
        # Create a list to store positions of matches
        matches <- list()
        
        # Find all matches for each search term
        for (i in seq_along(search_terms)) {
            term <- search_terms[i]
            # Find all matches (case-insensitive)
            starts <- gregexpr(term, text, ignore.case = TRUE)[[1]]
            if (starts[1] != -1) {
                term_length <- nchar(term)
                for (start in starts) {
                    matches[[length(matches) + 1]] <- list(
                        start = start,
                        end = start + term_length - 1,
                        color = names(colors_def)[i]
                    )
                }
            }
        }
        
        # If no matches, return original text
        if (length(matches) == 0) return(text)
        
        # Sort matches by start position
        matches <- matches[order(sapply(matches, function(x) x$start))]
        
        # Build highlighted text
        result <- ""
        last_pos <- 1
        
        for (match in matches) {
            # Add text before match
            if (match$start > last_pos) {
                result <- paste0(result, substr(text, last_pos, match$start - 1))
            }
            
            # Add colored match
            matched_text <- substr(text, match$start, match$end)
            result <- paste0(
                result,
                colors_def[[match$color]],
                matched_text,
                colors_def$reset
            )
            
            last_pos <- match$end + 1
        }
        
        # Add remaining text
        if (last_pos <= nchar(text)) {
            result <- paste0(result, substr(text, last_pos, nchar(text)))
        }
        
        return(result)
    }
    
    # Function to apply search filter across all columns
    apply_search_filter_special <- function(data, search_terms) {
        if (is.null(search_terms)) return(data)
        
        # For each search term
        for (term in search_terms) {
            # Match in any column (case-insensitive)
            matches <- apply(data, 1, function(row) {
                any(sapply(row, function(cell) grepl(term, cell, ignore.case = TRUE)))
            })
            data <- data[matches, ]
        }
        return(data)
    }
    
    # Function to format and wrap text maintaining alignment
    format_text <- function(text, width) {
        if (nchar(text) <= width) return(text)
        wrapped <- strwrap(text, width = width)
        return(wrapped)
    }
    
    # Apply search filter if needed
    if (!is.null(search)) {
        data <- apply_search_filter_special(data, search)
    }
    filtered_data <- data  # Store filtered data
    
    # If no data after filtering, return early
    if (nrow(data) == 0) {
        cat("No matches found.\n")
        return(invisible(NULL))
    }
    
    if (print) {
        # Print header
        header <- sprintf("%-*s | %-*s | %-*s | %-*s",
                         col_widths$Short, "Short",
                         col_widths$Symbol, "Symbol",
                         col_widths$Relation, "Relation",
                         col_widths$Original, "Original")
        cat(header, "\n")
        cat(paste(rep("-", nchar(header)), collapse = ""), "\n")
        
        # Calculate real lengths (excluding ANSI codes)
        strip_ansi <- function(text) {
            gsub("\033\\[[0-9;]*m", "", text)
        }


        # Print each row with wrapping
        for (i in 1:nrow(data)) {
            row <- data[i, ]
            
            # Highlight text if search is active
            highlighted_row <- lapply(row, highlight_text, search)
            
            # Format each column
            short_lines <- format_text(highlighted_row$Short, col_widths$Short)
            symbol_lines <- format_text(highlighted_row$Symbol, col_widths$Symbol)
            relation_lines <- format_text(highlighted_row$Relation, col_widths$Relation)
            original_lines <- format_text(highlighted_row$Original, col_widths$Original)
            
            # Calculate maximum number of lines needed
            max_lines <- max(length(short_lines), length(symbol_lines),
                            length(relation_lines), length(original_lines))
            
            # Pad shorter columns with empty strings
            short_lines <- c(short_lines, rep("", max_lines - length(short_lines)))
            symbol_lines <- c(symbol_lines, rep("", max_lines - length(symbol_lines)))
            relation_lines <- c(relation_lines, rep("", max_lines - length(relation_lines)))
            original_lines <- c(original_lines, rep("", max_lines - length(original_lines)))
            
            # Print each line
            for (j in 1:max_lines) {
                if (j == 1) {
                    # First line with all columns
                    cat(sprintf("%-*s | %-*s | %-*s | %s\n",
                              col_widths$Short + (nchar(short_lines[j]) - nchar(strip_ansi(short_lines[j]))),
                              short_lines[j],
                              col_widths$Symbol + (nchar(symbol_lines[j]) - nchar(strip_ansi(symbol_lines[j]))),
                              symbol_lines[j],
                              col_widths$Relation + (nchar(relation_lines[j]) - nchar(strip_ansi(relation_lines[j]))),
                              relation_lines[j],
                              original_lines[j]))
                } else {
                    # Continuation lines
                    cat(sprintf("%-*s | %-*s | %-*s | %s\n",
                              col_widths$Short + (nchar(short_lines[j]) - nchar(strip_ansi(short_lines[j]))),
                              short_lines[j],
                              col_widths$Symbol + (nchar(symbol_lines[j]) - nchar(strip_ansi(symbol_lines[j]))),
                              symbol_lines[j],
                              col_widths$Relation + (nchar(relation_lines[j]) - nchar(strip_ansi(relation_lines[j]))),
                              relation_lines[j],
                              original_lines[j]))
                }
            }
            
            # Add a blank line between entries
            cat("\n")
        }
        
    }

    # Return data invisibly
    invisible(filtered_data)
}





#' Get Balance of Payments (BOP) Data Details
#' 
#' Internal function that provides detailed information about Balance of Payments (BOP) 
#' variables, including their descriptions, symbols, and relationships.
#' 
#' @return A data frame with 4 columns:
#'   \item{Short}{Brief description of the BOP variable}
#'   \item{Symbol}{BOP variable symbol/code}
#'   \item{Relation}{Mathematical and conceptual relationships with other BOP variables}
#'   \item{Original}{Original full description of the variable from source data}
#' 
#' @details
#' The function returns a predefined set of BOP variables and their descriptions.
#' Each row represents one BOP variable with its associated metadata.
#' This function is used internally by \code{wp_info} when \code{special = "BOP"}.
#' 
#' @keywords internal
in_info_bop <- function(){

    df_bop_details <- data.frame(
      Short = c(
        "Exports of Goods",
        "Imports of Goods",
        "Goods Trade Balance",
        "Exports of Services",
        "Imports of Services",
        "Trade Balance",
        "Primary Income Payments",
        "Primary Income Received",
        "Secondary Income Payments",
        "Secondary Income Received",
        "Goods, Services, and Primary Income Balance",
        "Current Account Balance",
        "Capital Account Outflow",
        "Capital Account Inflow (Excl. Reserves)",
        "Capital Account Balance (Excl. Reserves)",
        "Outward Direct Investment (Debt)",
        "Outward Direct Investment (Equity & Funds)",
        "Outward FDI",
        "Inward Direct Investment (Debt)",
        "Inward Direct Investment (Equity & Funds)",
        "Inward FDI",
        "Debt Securities Acquired",
        "Equity and Investment Funds Acquired",
        "Outward Portfolio Investments (Acquired)",
        "Debt Securities Incurred",
        "Equity and Investment Funds Incurred",
        "Inward Portfolio Investments (Incurred)",
        "Financial Derivatives and Stock Options Liabilities",
        "Financial Derivatives and Stock Options Assets",
        "Net Financial Derivatives and Stock Options",
        "Other Debt Instruments Acquired",
        "Other Equity Acquired",
        "Other Investments Acquired",
        "Other Debt Instruments Incurred",
        "Other Equity Incurred",
        "Other Investments Incurred",
        "Financial Account Balance (Excl. Excep. Financing)",
        "Exceptional Financing",
        "Change in Foreign Exchange Reserves",
        "Change in IMF Reserve Assets",
        "Net IMF Credit and Loans",
        "Current and Capital Account Balance",
        "Errors and Omissions",
        "Overall Balance of Payments (Should be 0)"
      ),
      Symbol = c(
        "EXg", "IMg", "GBT", "EXs", "IMs", "TB", "PIo", "PIi", "SIo", "SIi", "GSI", "CU", "CAo", "CAi", "CA", "oDID", "oDIE", "oFDI", "iDID", "iDIE", "iFDI", "oPORD", "oPORE", "oPOR", "iPORD", "iPORE", "iPOR", "iDER", "oDER", "DER", "oOID", "oOIE", "oOI", "iOID", "iOIE", "iOI", "FA", "ExF", "FER", "imfRE", "imfCL", "TCC", "EO", "BP"
      ),
      Relation = c(
        "EXg (included in GBT and then in TB)",
        "IMg (included in GBT and then in TB)",
        "GBT = EXg - IMg (included in TB and then in GSI)",
        "EXs (included in TB)",
        "IMs (included in TB)",
        "TB = (EXg - IMg) + (EXs - IMs) = GBT + (EXs - IMs)",
        "PIo (PIi+PIo = Primary Income | included in CU)",
        "PIi (PIi+PIo = Primary Income | included in CU)",
        "SIo (SIi+SIo = Secondary Income | included in CU)",
        "SIi (SIi+SIo = Secondary Income | included in CU)",
        "GSI = TB + (PIi - PIo)",
        "CU = GSI + (SIi - SIo)",
        "CAo (included in CA)",
        "CAi (included in CA)",
        "CA = CAi - CAo | included in TCC",
        "oDID (subset of oFDI)",
        "oDIE (subset of oFDI)",
        "oFDI = oDID + oDIE",
        "iDID (subset of iFDI)",
        "iDIE (subset of iFDI)",
        "iFDI = iDID + iDIE",
        "oPORD (subset of oPOR)",
        "oPORE (subset of oPOR)",
        "oPOR = oPORD + oPORE",
        "iPORD (subset of iPOR)",
        "iPORE (subset of iPOR)",
        "iPOR = iPORD + iPORE",
        "iDER (included in DER)",
        "oDER (included in DER)",
        "DER = oDER - iDER (included in FA)",
        "oOID (included in oOI | include bank loans)",
        "oOIE (included in oOI)",
        "oOI ~= oOID + oOIE",
        "iOID (included in iOI | include bank loans",
        "iOIE (included in iOI)",
        "iOI ~= iOID + iOIE",
        "FA(excl.ExF) = FDI + POR + OI + DER",
        "ExF (included in FA)",
        "FER (includes imfRE)",
        "imfRE (subset of FER)",
        "imfCL",
        "TCC = CU + CA",
        "EO | theory: BP + EO = 0",
        "BP = CU + CA + (FA+ExF) | theory: BP + EO = 0"
      ),
      Original = c(
        "Current Account, Goods and Services, Goods, Credit, US Dollars",
        "Current Account, Goods and Services, Goods, Debit, US Dollars",
        "Current Account, Goods and Services, Goods, Net, US Dollars",
        "Current Account, Goods and Services, Services, Credit, US Dollars",
        "Current Account, Goods and Services, Services, Debit, US Dollars",
        "Current Account, Goods and Services, Net, US Dollars",
        "Current Account, Primary Income, Debit, US Dollars",
        "Current Account, Primary Income, Credit, US Dollars",
        "Current Account, Secondary Income, Debit, US Dollars",
        "Secondary Income, Credit (Excluding Exceptional Financing), US Dollars",
        "Balance on Goods, Services, and Income, US Dollars",
        "Current Account, Net (Excluding Exceptional Financing), US Dollars",
        "Capital Account, Total, Debit, US Dollars",
        "Capital Account, Credit (Excludes Reserves and Related Items), US Dollars",
        "Capital Account (Excludes Reserves and Related Items), US Dollars",
        "Financial Account, Net Lending (+) / Net Borrowing (-) (Balance from Financial Account), Direct Investment, Net Acquisition of Financial Assets, Debt Instruments, US Dollars",
        "Financial Account, Net Lending (+) / Net Borrowing (-) (Balance from Financial Account), Direct Investment, Net Acquisition of Financial Assets, Equity and Investment Fund Shares, US Dollars",
        "Financial Account, Net Lending (+) / Net Borrowing (-) (Balance from Financial Account), Direct Investment, Net Acquisition of Financial Assets, US Dollars",
        "Direct Investment: Net Incurrence of Liabilities: Debt Instruments (Excluding Exceptional Financing), US Dollars",
        "Direct Investment, Net Incurrence of Liabilities, Equity and Investment Fund Shares (Excluding Exceptional Financing), US Dollars",
        "Direct Investment, Net Incurrence of Liabilities (Excluding Exceptional Financing), US Dollars",
        "Financial Account, Portfolio Investment, Net Acquisition of Financial Assets, Debt Securities, US Dollars",
        "Financial Account, Portfolio Investment, Net Acquisition of Financial Assets, Equity and Investment Fund Shares, US Dollars",
        "Financial Account, Portfolio Investment, Net Acquisition of Financial Assets, US Dollars",
        "Portfolio Investment, Net Incurrence of Liabilities, Debt Securities (Excluding Exceptional Financing), US Dollars",
        "Portfolio Investment, Net Incurrence of Liabilities, Equity Securities (Excluding Exceptional Financing), US Dollars",
        "Portfolio Investment, Net Incurrence of Liabilities (Excluding Exceptional Financing), US Dollars",
        "Financial Account, Financial Derivatives (Other Than Reserves) and Employee Stock Options, Net Incurrence of Liabilities, US Dollars",
        "Financial Account, Financial Derivatives (Other Than Reserves) and Employee Stock Options, Net Acquisition of Financial Assets, US Dollars",
        "Financial Account, Financial Derivatives (Other Than Reserves) and Employee Stock Options, US Dollars",
        "Financial Account, Other Investment, Net Acquisition of Financial Assets, Debt Instruments, US Dollars",
        "Financial Account, Other Investment, Other Equity, Net Acquisition of Financial Assets, US Dollars",
        "Financial Account, Other Investment, Net Acquisition of Financial Assets, US Dollars",
        "Other Investment: Net Incurrence of Liabilities, Debt Instruments (Excluding Exceptional Financing), US Dollars",
        "Other Investment: Other Equity: Net Incurrence of Liabilities (Excluding Exceptional Financing), US Dollars",
        "Financial Account, Net (excluding exceptional financing), Other investment, Net incurrence of liabilities (excluding exceptional financing), US Dollars",
        "Financial Account, Net (Excluding Exceptional Financing), US Dollars",
        "Exceptional Financing, US Dollars",
        "Reserves and Related items, US Dollars",
        "Reserve Assets (with Fund Record), US Dollars",
        "Net Credit and Loans from the IMF (Excluding Reserve Position), US Dollars",
        "Total Current + Capital Account, US Dollars",
        "Errors and Omissions (with Fund Record), US Dollars",
        "Current Acct + Capital Acct + Financial Acct, US Dollars"
      ),
      Explanation = c(
        "Value of physical goods a country exports, recorded as an inflow of money.",
        "Value of physical goods a country imports, recorded as an outflow of money.",
        "The net value of goods traded, considering both exports and imports of physical goods.",
        "Value of services a country exports, such as tourism or consulting, recorded as an inflow of money.",
        "Value of services a country imports, such as insurance or travel, recorded as an outflow of money.",
        "The net value of both goods and services traded, considering both exports and imports.",
        "The money a country pays out to foreign investors or other primary income sources, such as interest payments.",
        "The money a country receives from foreign investments and other primary income sources, such as interest and dividends.",
        "The money a country pays out in transfers like remittances or foreign aid, recorded as an outflow of money.",
        "The money a country received in transfers like remittances or foreign aid, recorded as an inflow of money.",
        "The overall balance of transactions related to goods, services, and primary income.",
        "The net balance of the current account, excluding exceptional financing.",
        "This represents the total capital transfers out of the country, such as debt forgiveness or transfers of assets.",
        "The credits in the capital account, excluding reserves and related items.",
        "The net value of capital transactions, excluding reserves and related items.",
        "The net acquisition or sale of direct investments in debt instruments.",
        "The net acquisition or sale of direct investments in equity and investment fund shares.",
        "The net amount of direct investments in assets, showing whether the country is lending or borrowing.",
        "The net increase in liabilities from direct investment in debt instruments, excluding exceptional items.",
        "The net increase in liabilities from direct investment in equity and investment funds, excluding exceptional items.",
        "The net increase in liabilities from direct investment, excluding exceptional items.",
        "The net acquisition of debt securities through portfolio investment.",
        "The net amount spent on acquiring equity shares and investment fund shares.",
        "The net amount spent on acquiring financial assets like stocks and bonds from abroad.",
        "The net increase in liabilities from debt securities investments, excluding exceptional items.",
        "The net increase in liabilities from equity investments, excluding exceptional items.",
        "The net increase in liabilities from portfolio investments, excluding exceptional items.",
        "The net increase in liabilities from financial derivatives and employee stock options, such as options or futures contracts.",
        "The net increase in assets from financial derivatives and employee stock options, such as options or futures contracts.",
        "Transactions related to financial derivatives and stock options, without separating liabilities or assets.",
        "The net amount spent on acquiring debt instruments, like government or corporate bonds.",
        "The net amount spent on acquiring other types of equity investments, not including stocks or bonds.",
        "The net amount spent on acquiring other types of financial assets not classified elsewhere.",
        "The net increase in liabilities from debt instruments, excluding exceptional financing.",
        "The net increase in liabilities from other equity investments, excluding exceptional items.",
        "The net increase in liabilities from other types of investments, excluding exceptional items.",
        "Financial account (excl. exceptional financing) = FDI + Portfolio Inv. + Derivatives (no reserves) and Employee Stock Options + Other Invest + Reserve",
        "Unusual or non-recurring financial transactions, such as emergency loans from IMF, debt relief, bi or multilateral aid, and arrears.",
        "The country's reserves of foreign currencies and related assets.",
        "The reserves held by the country with the IMF or similar institutions.",
        "Credit and loans from the IMF, excl. reserve (might be part of: (i) OI if considered regular transactions, (ii) ExF if considerd BOP support, (iii) none)",
        "The total value of the current account and capital account combined.",
        "Adjustments for discrepancies in the data recording, including errors and omissions as noted by the IMF.",
        "The sum of the current account, capital account, and financial account balances."
      ),
      stringsAsFactors = FALSE
    )

    return(df_bop_details[, 1:5])
}



