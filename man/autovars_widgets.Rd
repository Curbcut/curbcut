% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autovars_helpers.R
\name{autovars_widgets}
\alias{autovars_widgets}
\title{Get the Widgets for a Specific Module ID and Group Name}
\usage{
autovars_widgets(id, group_name, common_vals)
}
\arguments{
\item{id}{<\code{character}> Identifying the module in modules$id}

\item{group_name}{<\code{character}> String identifying the group name to only filter
the widgets of the same group.}

\item{common_vals}{<\verb{named list}> A list of common widget values. Usually
\code{common_vals()} from the autovars module.}
}
\value{
A list of widgets. If \code{tb} is not a data frame, an empty list is returned.
}
\description{
This function retrieves the widgets associated with a specific module ID and
group name. The widgets are retrieved from a global environment object named
'modules'. Only widgets that share the same values as the common widgets are
returned.
}
