% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_rvs.R
\name{update_zoom_string}
\alias{update_zoom_string}
\title{Update the saved zoom string for a given region based on a current zoom level}
\usage{
update_zoom_string(rv_zoom_string, zoom, zoom_levels)
}
\arguments{
\item{rv_zoom_string}{<\code{character}> A character string representing a previously
saved zoom level for a given region}

\item{zoom}{<\code{numeric}> A numeric value representing the current zoom level}

\item{zoom_levels}{<\verb{named numeric vector}> A named numeric vector of zoom
levels. Usually one of the \verb{mzl_*}, or the output of
\code{\link{geography_server}}.}
}
\value{
A character string representing the updated zoom level for the given region
}
\description{
This function takes in a previously saved zoom string and updates it based on
the current zoom level for a given region. If the new zoom string is
different from the previously saved one, it returns the new zoom string.
Otherwise, it returns the old one.
}
