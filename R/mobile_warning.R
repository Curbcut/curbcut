#' Display a warning message if the user is accessing the app from a mobile device
#'
#' This function displays a warning message if the user is accessing the Shiny app
#' from a mobile device. The message is displayed using the
#' \code{\link[shinyjs]{info}} function.
#'
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param session <`session`> The session/scope of the `server.R`.
#' Usually `session = session`.
#'
#' @export
mobile_warning <- function(r, session) {

    shiny::observe({
      device <- session$input$.shinybrowser$device
      # Return nothing if device isn't detected
      if (is.null(device)) return(NULL)
      # If not desktop, return the error.
      if (device != "Desktop") {
        shinyjs::info(
          cc_t(lang = r$lang(),
               "Curbcut does not currently support mobile phones. ",
               "Please visit from a computer."))
      }
    })

}
