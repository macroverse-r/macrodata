% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/md_data.r
\name{.process_yearly_data}
\alias{.process_yearly_data}
\title{Process Yearly Data Format}
\usage{
.process_yearly_data(symbol_data)
}
\arguments{
\item{symbol_data}{data.frame; yearly data to process}
}
\value{
data.frame with quarterly structure
}
\description{
Internal function that reformats yearly data to match quarterly data structure
by duplicating yearly values in Q1.

Used by: .adjust_columns_to_frequency
Uses: None

Features:
\itemize{
\item Copies yearly values to Q1
\item Removes Q2-Q4 columns
\item Maintains data integrity
\item Preserves metadata columns
}
}
\keyword{internal}
