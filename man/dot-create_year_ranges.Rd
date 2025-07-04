% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/md_data.r
\name{.create_year_ranges}
\alias{.create_year_ranges}
\title{Create Year Range Strings}
\usage{
.create_year_ranges(years_with_data, all_years)
}
\arguments{
\item{years_with_data}{numeric; vector of years with data}

\item{all_years}{numeric; vector of all possible years}
}
\value{
character string of year ranges
}
\description{
Internal helper function that creates compact string representations of
year ranges.

Used by: .check_missing_data
Uses: None

Features:
\itemize{
\item Range compression
\item Efficient string creation
\item Century handling
\item Single year handling
}
}
\keyword{internal}
