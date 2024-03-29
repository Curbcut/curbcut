% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_unit.R
\name{convert_unit}
\alias{convert_unit}
\title{Convert a numeric vector into a character vector with a specified unit}
\usage{
convert_unit(var = NULL, x, ...)
}
\arguments{
\item{var}{<\code{character}> String specifying the variable type. Currently
supported types are "pct" (percentage), "dollar" (dollar), and "default"
(comma-separated with varying decimal points).}

\item{x}{<\code{numeric}> Vector to be converted into a character vector with a
specified unit.}

\item{...}{Additional arguments to be passed to the methods.}
}
\value{
A character vector with a specified unit based on the variable type
specified.
}
\description{
This function converts a numeric vector into a character vector with a
specified unit based on the variable type specified. It dispatches to methods
based on the class of \code{x}. Supported unit types are percentage, dollar,
and default (comma-separated with varying decimal points). If \code{compact}
is \code{TRUE}, the function will attempt to compact large numbers.
}
