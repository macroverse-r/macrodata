% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/md_info.r
\name{wp_info}
\alias{wp_info}
\title{Display Information About Available Data Variables}
\usage{
wp_info(
  frequency = NULL,
  origin = NULL,
  search = NULL,
  color = TRUE,
  special = NULL,
  print = TRUE
)
}
\arguments{
\item{frequency}{Character, either "Q" (quarterly), "Y" (yearly), or NULL (both). If NULL (default),
displays both quarterly and yearly data.}

\item{origin}{Character vector, filter by data source(s). Multiple origins can be specified.
Invalid origins trigger a warning. Default is NULL (all origins).}

\item{search}{Character vector, search terms to filter data. Searches both Symbol and Indicator columns.
Case-insensitive, supports partial matches. Maximum length is limited by available colors.
Default is NULL (no search).}

\item{color}{Logical, whether to use color highlighting for search matches. Default is TRUE.}

\item{special}{Character, specifies special display modes. Currently only "BOP" (Balance of Payments)
is supported. Default is NULL.}

\item{print}{Logical, whether to print the output to console (default = TRUE)}
}
\value{
When print=TRUE (default), displays the data in console.
Invisibly returns:
\itemize{
\item A data frame combining Q and Y data (with Frequency column) when frequency=NULL
\item A single data frame when frequency is specified ("Q" or "Y")
\item For special="BOP", returns the filtered Balance of Payments data frame
}
}
\description{
Provides a comprehensive view of available data variables in the WPD package, organized by data source and frequency.
This function is typically used before wp_data() to discover available variables and their codes.
Variables are grouped by their origin (data source) and can be filtered or searched.
}
\details{
The function helps users discover available variables through several features:

\strong{Variable Naming Patterns:}
Common suffixes:
\itemize{
\item _C: Current prices/values
\item _R\_YYYY: Real/constant prices (YYYY is base year)
\item _ZS: Percentages/shares
\item _CD: Current US dollars
\item _KN: Local currency units
\item _PC: Per capita values
\item _XD: Indices
\item _MA/\_FE: Male/Female specific
\item _GD: GDP-related
\item _IX: Index values
\item _IN: Count/number
\item _PT: Points/percentages
\item _TOT: Total values
\item _EST: Estimates
\item _ZG: Growth rates
\item _KD: Constant dollars
\item _PPP: Purchasing Power Parity
\item _SA: Seasonally adjusted
\item _AD: Adjusted/linked series
\item _GI: Global/composite indices
}

Common prefixes:
\itemize{
\item NE\_ : National accounts expenditure
\item NY\_ : National accounts income
\item BX\_ : Balance of payments credits/exports
\item BM\_ : Balance of payments debits/imports
\item SL\_ : Labor market indicators
\item SP\_ : Population indicators
\item SE\_ : Education indicators
\item SH\_ : Health indicators
\item FI\_ : Financial reserves
\item EN\_ : Environment indicators
\item IC\_ : Investment climate
\item TX\_ : Trade/export indicators
\item TM\_ : Trade/import indicators
}

\strong{Balance of Payments Mode (special="BOP"):}
When using special="BOP", displays detailed Balance of Payments relationships with specific prefixes:
\itemize{
\item i/o: Inward/outward flows
\item DI/POR/OI: Direct Investment/Portfolio/Other Investment
\item E/D: Equity/Debt components
}

\strong{Data Sources:}
The output displays data sources in headers, including major sources like:
\itemize{
\item IMF IFS (International Financial Statistics)
\item IMF BOP (Balance of Payments)
\item BIS Statistics
\item World Bank Indicators
}

\strong{Data Availability:}
Some indicators are available in both quarterly (METADATA_Q) and yearly (METADATA_Y) frequencies,
while others are specific to one frequency. Both METADATA_Q and METADATA_Y are accessible in the
package namespace for direct inspection.

\strong{Search Functionality:}
The search parameter provides flexible filtering:
\itemize{
\item Searches across both Symbol and Indicator columns simultaneously
\item Multiple search terms work as OR condition (matches any term)
\item Case-insensitive and supports partial matches
\item Search highlighting makes matches easily visible (can be disabled with color=FALSE)
}

\strong{Special Modes:}
Currently supports "BOP" (Balance of Payments) mode for detailed payment relationships.
Future versions will include additional special modes for other data categories
and analytical views.
}
\examples{
# Show quarterly data
wp_info("Q")

# Filter by origin
wp_info(origin = c("BIS", "IMF BOP"))

# Search for GDP-related variables
wp_info(search = "GDP")

# Multiple search terms with color highlighting
wp_info(search = c("GDP", "growth"))

# Display Balance of Payments relationships
wp_info(special = "BOP")

# Common workflow with wp_data()
# 1. Discover available GDP variables
wp_info(search = "GDP")
# 2. Use discovered variable in wp_data
wp_data(ISO = "USA", 
        formula = "GDP_R_2015", 
        years = c(2000, 2023))


# Combined origin and search filters
wp_info(origin = "IMF BOP", search = "investment")

# Search for percentage indicators
wp_info(search = "_ZS")

# Search for real/constant price variables
wp_info(search = "_R_")

# Multiple search terms finding either pattern
wp_info(search = c("GDP", "investment"))

# Get data without printing
gdp_vars <- wp_info(search = "GDP", print = FALSE)

}
\seealso{
\code{\link{wp_data}} for using the discovered variables in data retrieval
}
