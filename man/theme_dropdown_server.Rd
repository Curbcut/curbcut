% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme_dropdown_module.R
\name{theme_dropdown_server}
\alias{theme_dropdown_server}
\title{Theme Dropdown Server}
\usage{
theme_dropdown_server(id, r)
}
\arguments{
\item{id}{<\code{character}> The ID of the page in which this module will appear,
e.g. \code{alp}.}

\item{r}{<\code{reactiveValues}> The reactive values shared between modules and
pages. Created in the \code{server.R} file. The output of \code{\link{r_init}}.}
}
\value{
This function does not return a value. It is used for its side effects
}
\description{
This module creates an observer for detecting page clicks on the theme dropdown
in Curbcut. When a page is clicked, it updates the selected tab in
the 'cc_page' tabset panel according to the selected page.
}
