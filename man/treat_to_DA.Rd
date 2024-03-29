% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{treat_to_DA}
\alias{treat_to_DA}
\title{Treat data frames as a DA (DA) scale}
\usage{
treat_to_DA(scales_as_DA, scale)
}
\arguments{
\item{scales_as_DA}{<\verb{character vector}> dfs to check if they should be
treated like a DA scale}

\item{scale}{<\code{character}> The \code{scale} to check if it is a DA scale}
}
\value{
If the current \code{df} is part of the scales that should be treated
as a DA, thye function appends "_DA" to the \code{df} string instead of the
current scale, and returns the new name. If not, the original \code{df} is returned.
}
\description{
This function takes a list of scales that should be treated like DAs.
If they should, the function appends "_DA" to the \code{df} string instead of the
current scale, and returns the new name. If not, the original \code{df} is returned.
}
