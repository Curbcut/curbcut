#' Display a warning message if the user is accessing the app from a mobile device
#'
#' This function displays a warning message if the user is accessing the Shiny app
#' from a mobile device. The message is displayed using the
#' \code{\link[shinyjs]{info}} function.
#'
#' @param id <`character`> A unique id for the module. Default is "mobile_warning".
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file.
#' @param parent_session <`session`> The session of the parent module that the
#' warning is embedded in (`server.R`). Usually `parent_session = session`.
#'
#' @export
mobile_warning <- function(id = "mobile_warning", r, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observe({
      device <- parent_session$input$.shinybrowser$device
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

  })
}
