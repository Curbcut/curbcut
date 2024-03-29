% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{filter_inrange}
\alias{filter_inrange}
\title{Filter Rows Based on a Column Value Range}
\usage{
filter_inrange(data, col, range, select_id = NA)
}
\arguments{
\item{data}{<\code{dataframe}> The table to be filtered.}

\item{col}{<\code{character}> The name of the column to filter on.}

\item{range}{<\verb{numeric vector}> A vector indicating the lower and upper
range to filter the column by.}

\item{select_id}{<\code{character}> Selection. Defaults to NA. If there is an ID
specified, the range will be tweaked to make sure it keeps select_id in the
range.}
}
\value{
<\code{data.table}> A data.table containing only rows where the specified
column's values are within the given range. If a select_id is supplied, the
range is tweak to make sure to include the selection. The new range is then
output as an attribute (\verb{range_\{col\}})
}
\description{
This function filters the rows of a table where a specified column's values
fall within a given range.
}
