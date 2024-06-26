% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/action_button.R
\name{action_button_task}
\alias{action_button_task}
\title{Create a task button for launching longer-running operations with UI feedback}
\usage{
action_button_task(
  classes = c(),
  id,
  icon = NULL,
  icon_style = NULL,
  text_class = NULL,
  text = NULL,
  text_busy = cc_t("Processing..."),
  auto_reset = TRUE
)
}
\arguments{
\item{classes}{<\verb{character vector}> Additional CSS classes to apply to the
button, enhancing its default appearance. Default is an empty character vector.}

\item{id}{<\code{character}> The input slot that will be used to access the value.}

\item{icon}{<\code{character}> An optional Material Design icon name for the button's
ready state. Defaults to NULL for no icon. Utilizes \code{\link{icon_material}} for
icon creation.}

\item{icon_style}{<\code{character}> Additional CSS classes for customizing the icon's
appearance. Default is NULL.}

\item{text_class}{<\code{character}> Additional CSS classes for customizing the button
text appearance. Default is NULL.}

\item{text}{<\code{character}> Label of the button while in its ready (clickable) state.
Default is NULL.}

\item{text_busy}{<\code{character}> Label of the button while it is processing, providing
feedback that an operation is in progress. Defaults to "Processing...".}

\item{auto_reset}{<\code{logical}> Whether the button should automatically revert to its
initial state after the operation completes. Defaults to TRUE, enhancing user
experience by clearly indicating the operation's end.}
}
\value{
A Shiny button element with enhanced UI feedback for indicating operation
progress. It behaves similarly to \code{bslib::input_task_button}, but with
customization options for icon and text styling, and integrates seamlessly within
the Curbcut platform's UI.
}
\description{
This function is a reimplementation of \code{bslib::input_task_button},
tailored for the Curbcut platform. It creates a button designed for triggering
actions or recomputations that might take some time, such as data processing
or loading operations. Like \code{bslib::input_task_button}, it integrates with
\code{shiny::bindEvent()} (or \code{shiny::eventReactive()} and
\code{shiny::observeEvent()}) to manage reactive operations. The button prevents
additional clicks during its operation, displaying a customizable progress message
and icon, and automatically resets after completion.
}
