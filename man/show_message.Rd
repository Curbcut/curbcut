% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geography_helpers.R
\name{show_message}
\alias{show_message}
\title{Show a temporary message with an undo button}
\usage{
show_message(
  id,
  r,
  input,
  message,
  undo_fun,
  container_div,
  undo_btn_id = "undo_btn",
  duration_ms = 8000,
  fading_s = 0.5
)
}
\arguments{
\item{id}{<\code{character}> The ID of the page in which the bookmark_server function
will appear, e.g. \code{alp}.}

\item{r}{<\code{reactiveValues}> The reactive values shared between modules and
pages. Created in the \code{server.R} file. The output of \code{\link{r_init}}.}

\item{input}{<\verb{shiny input}> Shiny input object in the module. Usually \code{input}.}

\item{message}{<\code{character}> The message to be displayed.}

\item{undo_fun}{<\code{function}> Function to execute if undo button is clicked.}

\item{container_div}{<\code{character}> ID of the div where the message
will be inserted. This div needs to have been previously created.}

\item{undo_btn_id}{<\code{character}> Optional ID for the undo button.
Defaults to "undo_btn".}

\item{duration_ms}{<\code{numeric}> Duration the message is displayed for,
in milliseconds. Defaults to 6000.}

\item{fading_s}{<\code{numeric}> Duration the fade of the message takes, in seconds.
Directly fed to \code{time} of shinyjs::hide(). Defaults to 0.5.}
}
\value{
Places a message in the \code{container_div} with an undo button which,
when clicked on, triggers the \code{undo_fun}. The message stays for \code{duration_ms}.
}
\description{
This function shows a message to the user in the \code{container_div},
and provides an undo button. The message and the button will fade away
after a pre-defined time.
}
