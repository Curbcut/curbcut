% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/popup_module.R
\name{popup_UI}
\alias{popup_UI}
\title{UI-side function for a popup in Curbcut}
\usage{
popup_UI(id)
}
\arguments{
\item{id}{<\code{character}> The ID of the page in which the legend will appear,
e.g. \code{alp}.}
}
\value{
An HTML output object for Shiny to render.
}
\description{
This function defines the UI-side markup for a popup element in a Curbcut
app. It returns an HTML output placeholder where the popup content
will be injected.
}
