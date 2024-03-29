% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autovars_module.R
\name{autovars_server}
\alias{autovars_server}
\alias{autovars_UI}
\title{Auto-vars module}
\usage{
autovars_server(
  id,
  r,
  main_dropdown_title,
  default_year,
  time_div_label = shiny::reactive("Time"),
  time_div_icon = shiny::reactive("date_range"),
  compare_label = shiny::reactive("Compare dates")
)

autovars_UI(id, ...)
}
\arguments{
\item{id}{<\code{character}> The ID of the page in which the legend will appear,
e.g. \code{alp}.}

\item{r}{<\code{reactiveValues}> The reactive values shared between modules and
pages. Created in the \code{server.R} file. The output of \code{\link{r_init}}.}

\item{main_dropdown_title}{<\code{character}> A character string to be used as the
title of the main dropdown selector.}

\item{default_year}{<\code{numeric}> An optional numeric value specifying the default
year for time widgets. If not provided, these widgets will not be initialized.}

\item{time_div_label}{<\verb{reactive character}> The label of the time div. Defaults
to \code{Time}.}

\item{time_div_icon}{<\verb{reactive character}> Material ion name for the time
selection UI, defaults to "date_range".}

\item{compare_label}{<\verb{reactive character}> The label of the compare checkbox.
Defaults to \verb{Compare dates}.}

\item{...}{UIs to be inserted in the advanced controls div.}
}
\value{
A reactive list with the final variable ('var') and the selected
time ('time').
}
\description{
This Shiny module allows for the automatic generation and control of pickers and
sliders within Curbcut. It uses the entries in the \code{modules} table to draw
the necessary pickers and sliders. It prepares global settings, creates
common widgets, handles their reactivity, and sets up the main dropdown.
It also updates the UI dynamically based on selected values, and finally,
generates the final variable code using all of these values.
}
\section{Functions}{
\itemize{
\item \code{autovars_UI()}: Create the UI for the autovars module

}}
