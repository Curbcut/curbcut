% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/page_title.R
\name{title_page_update}
\alias{title_page_update}
\title{Update the web title of Curbcut}
\usage{
title_page_update(r, active_page, site_name)
}
\arguments{
\item{r}{<\code{reactiveValues}> The reactive values shared between modules and
pages. Created in the \code{server.R} file. The output of \code{\link{r_init}}.}

\item{active_page}{<\code{reactive}> The ID of the active page. Usually \code{input$cc_page}
in a \code{\link[shiny]{reactive}}, e.g. \code{shiny::reactive(input$cc_page)}.}

\item{site_name}{<\code{character}> The current site name. Usually initiated in
\code{global.R}.}
}
\description{
This function is used to update the title of a Curbcut page based on the
ID of the active page. The function takes in a reactive object \code{r},
the parent session object \code{parent_session}, the ID of the active page
\code{active_page}, and the current site name \code{site_name}. The function
then calls the \code{\link{title_page_get}} function to update the site name
with the title of the active page, and sends a custom message to the parent
session to update the page title.
}
