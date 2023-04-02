#' Server logic for comparing variables in Curbcut
#'
#' This function provides the server logic for comparing the main variable with
#' different variables in Curbcut.
#'
#' @param id <`character`> The ID of the page in which the module will appear,
#' e.g. `canale`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param var_list <`named list`> Choices to display in the picker input. Normally
#' made using \code{\link{dropdown_make}}.
#' @param time <`reactive numeric vector`> Vector of time values to use for
#' appending a time to the variables picked. The returned vector will be the
#' same length as this argument.
#' @param show_panel <`reactive logical`> Controls whether the comparison panel
#' should be shown.
#'
#' @details This function uses the \code{\link{picker_server}} function to
#' generate a reactive expression (`var_right`) that contains the selected
#' variable(s) for comparison. It also uses the \code{\link[shinyjs]{toggle}}
#' function to show/hide the comparison panel based on the value of the
#' `show_panel` reactive expression.
#'
#' @return A Shiny module server function that provides the logic for
#' comparing variables in Curbcut
#' @export
compare_server <- function(id, r, var_list, time = shiny::reactive(NULL),
                           show_panel = shiny::reactive(TRUE)) {
  stopifnot(!shiny::is.reactive(var_list))
  stopifnot(shiny::is.reactive(time))
  stopifnot(shiny::is.reactive(show_panel))

  shiny::moduleServer(id, function(input, output, session) {
    # Get the var_right reactive
    var_right <- picker_server(
      id = "compare",
      r = r,
      var_list = var_list,
      time = time
    )

    # If we shouldn't show the panel
    shiny::observeEvent(show_panel(), {
      shinyjs::toggle("compare_panel", condition = show_panel())
    })

    var_right
  })
}

#' @describeIn compare_server Create the UI for the compare module
#' @export
compare_UI <- function(id, var_list) {
  shiny::div(
    id = shiny::NS(id, "compare_panel"),
    shiny::fluidRow(
      shiny::column(width = 7, shiny::h4(cc_t("Compare")))
    ),
    shiny::div(
      id = shiny::NS(id, "widgets"),
      picker_UI(
        id = shiny::NS(id, "compare"),
        var_list = var_list
      )
    ),
    shiny::hr()
  )
}
