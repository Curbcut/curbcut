#' Heartbeat Function
#'
#' This function is used to create a "heartbeat" that keeps the Curbcut app
#' running in the background even if the user is not interacting with it.
#'
#' @param input <`input`> A reactive input object that contains all of the input
#' values from the user interface. As this function will be placed in `server.R`
#' in the \code{\link[shiny]{shinyServer}} function, this will usually be
#' `input = input`.
#'
#' @return The function returns a Shiny observer that triggers every 10 seconds
#' when `input` haven't changed to keep the app alive. After 2 hours, the
#' heartbeat will stop and the app can disconnect with usual Shiny timeout.
#'
#' @details The function first creates an eventReactive object that listens for
#' changes to the input values. This is used to update the reactive input object
#' every time an input changes. he observer function is then created using the
#' \code{\link[shiny]{observe}} function. This function triggers every 10 seconds
#' using \code{\link[shiny]{invalidateLater}}. The observer is invalidated and
#' re-triggered after 10 seconds have elapsed. This creates a "heartbeat" that
#' keeps the app running even if the user is not interacting with it. After 2
#' hours (7200 seconds), the heartbeat will stop and the app can disconnect with
#' the usual Shiny timeout. This prevents the app from running indefinitely if
#' the user forgets to close it.
#' @export
heartbeat <- function(r, input) {
  # Update the reactive every time an input changes
  timeout_start <- shiny::eventReactive(
    shiny::reactiveValuesToList(input),
    Sys.time()
  )

  # Every 10 seconds, this reactive context will be invalidated and so
  # re-triggered. Alike a while loop. After 2 hours, then the heartbeat will
  # stop and the app can disconnect with usual Shiny timeout.
  shiny::observe({
    rerun <- timeout_start() + 28800 > Sys.time()
    if (rerun) shiny::invalidateLater(10000, session = r$server_session())
  })
}
