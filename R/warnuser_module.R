#' Warn User Module
#'
#' This module provides a server function and a UI function to warn users
#' based on specific conditions. The warnings are displayed as text.
#'
#' @param id <`character`> The ID of the page in which the legend will appear,
#' e.g. `canale`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link{vars_build}} function.
#' @param time <`reactive numeric vector`> Vector of time values to use for
#' appending a time to the variables picked.
#' @param data <`reactive data.frame`> Data frame containing all the scale and
#' the `var_left` and `var_right`. The output of \code{\link{data_get}}.
#' @param more_text <`reactive character`> A reactive expression containing optional
#' additional text to be added to the warning message. Default is shiny::reactive(NULL).
#'
#' @return The warnuser_UI function returns a tag list for inclusion in a Shiny UI,
#' while the warnuser_server function returns the server function.
#' @export
warnuser_server <- function(id, r, vars, time, data,
                            more_text = shiny::reactive(NULL)) {
  stopifnot(shiny::is.reactive(data))
  stopifnot(shiny::is.reactive(vars))
  stopifnot(shiny::is.reactive(time))
  stopifnot(shiny::is.reactive(data))
  stopifnot(shiny::is.reactive(more_text))

  shiny::moduleServer(id, function(input, output, session) {
    warn <- shiny::reactive(warnuser_get(
      vars = vars(), data = data(),
      time = time(), more_text = more_text(),
      lang = r$lang()
    ))

    output$warnuser <- shiny::renderText(shiny::HTML(warn()))
  })
}

#' @describeIn warnuser_server Create the UI for the warnuser module
#' @export
warnuser_UI <- function(id) {
  shiny::tagList(
    shiny::htmlOutput(shiny::NS(id, "warnuser"))
  )
}
