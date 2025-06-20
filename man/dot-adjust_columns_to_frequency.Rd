% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/md_data.r
\name{.adjust_columns_to_frequency}
\alias{.adjust_columns_to_frequency}
\title{Adjust Data Columns for Frequency Matching}
\usage{
.adjust_columns_to_frequency(
  data,
  frequencies,
  symbols,
  matching_yq = "Q2Y",
  interpolation_method = "Linear",
  verbose = TRUE,
  debug = FALSE
)
}
\arguments{
\item{data}{data.frame; input dataset}

\item{frequencies}{character; frequencies for each symbol}

\item{symbols}{character; vector of symbols to process}

\item{matching_yq}{character; frequency matching method}

\item{interpolation_method}{character; interpolation method for Y2Q}

\item{verbose}{logical; print progress information}

\item{debug}{logical; print debug information}
}
\value{
data.frame with adjusted frequencies
}
\description{
Internal function that handles conversion between quarterly and yearly data
frequencies.

Used by: .data_one_formula
Uses: .process_yearly_data, .match_Q2Y, .match_Y2Q

Features:
\itemize{
\item Handles Q-to-Y and Y-to-Q conversions
\item Supports multiple interpolation methods
\item Preserves data relationships
\item Maintains column naming conventions
}
}
\keyword{internal}
