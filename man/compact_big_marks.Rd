% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_unit.R
\name{compact_big_marks}
\alias{compact_big_marks}
\title{Compact big marks}
\usage{
compact_big_marks(x, min_dig, scale_fun = scales::comma)
}
\arguments{
\item{x}{<\verb{numeric vector}> Numeric values.}

\item{min_dig}{<\code{integer}> An integer indicating the minimum number of
significant digits. It is used to determine the scale factor for the input
vector.}

\item{scale_fun}{<\code{function}> The scale function to be used to format the
output. The default is \code{scales::comma}.}
}
\value{
A string representation of the input value with a suffix of M, K,
or B as appropriate.
}
\description{
This function takes a numeric value and returns a compact representation of
the value in millions (M), thousands (K), or billions (B) based on its
magnitude, depending on the magnitude of the smallest significant digit.
The scale function used to format the output can be customized.
It is a helper function for the \code{\link{convert_unit}} function
and so needs pre-calculated \code{min_dig}. If the output is not unique, it
adds decimals until a maximum of three is attained.
}
