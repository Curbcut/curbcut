#' Server logic for comparing variables in Curbcut
#'
#' This function provides the server logic for comparing the main variable with
#' different variables in Curbcut.
#'
#' @param id <`character`> The ID of the page in which the module will appear,
#' e.g. `alp`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param var_left <`reactive character`> Character string of the selected
#' left-hand variable, e.g. `alp`
#' @param var_list <`reactive list`> Choices to display in the picker input. Normally
#' made using \code{\link{dropdown_make}}.
#' @param time <`reactive numeric vector`> Vector of time values to use for
#' appending a time to the variables picked. The returned vector will be the
#' same length as this argument.
#' @param zoom_levels <`named numeric vector`> A named numeric vector of zoom
#' levels. One of `mzl_*`. Usually `r[[id]]$zoom_levels`.
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
compare_server <- function(id, r, var_left, var_list, time = shiny::reactive(NULL),
                           zoom_levels, show_panel = shiny::reactive(TRUE)) {
  stopifnot(shiny::is.reactive(var_left))
  stopifnot(shiny::is.reactive(var_list))
  stopifnot(shiny::is.reactive(time))
  stopifnot(shiny::is.reactive(zoom_levels))
  stopifnot(shiny::is.reactive(show_panel))

  shiny::moduleServer(id, function(input, output, session) {

    # Get the high correlation combinations
    modules <- get_from_globalenv("modules")
    high_corrs <- modules$high_corr_combination[modules$id == id][[1]]
    # For every variable in the list, does it have a high correlation with the
    # current left variable? Returns a character vector
    choices_class <- shiny::reactive({
      this_corr <- high_corrs[[var_left()]]
      ifelse(unlist(var_list()) %in% this_corr, "dropdown_high_corr", "")
    })

    # Look the zoom levels. Are all data available at the higher scale? If not,
    # disable the options
    disable_options <- shiny::reactive({
      vars <- unname(unlist(var_list()))
      out <- !sapply(vars, is_data_present_in_scale,
              scale = names(zoom_levels())[1], USE.NAMES = FALSE)

      # If one of the list is " " which means no comparison, do not disable it
      out[which(vars == " ")] <- FALSE

      out
    })

    # Dynamically generate the checkbox
    cbox <- checkbox_server(id = "compare_cbx", r = r,
                           event_reset = shiny::reactive(var_right() != " "))

    # Get the var_right reactive
    var_right <- picker_server(
      id = "compare",
      page_id = id,
      r = r,
      var_list = var_list,
      time = time,
      classNames = choices_class,
      disable_options = disable_options,
      selected = shiny::reactive(if (!cbox()) " " else NULL)
    )

    shiny::observe({
      shinyjs::toggle("compare_cbx_div", condition = var_right() != " ")
    })

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
    shiny::hr(id = shiny::NS(id, "hr_compare_panel")),
    shiny::div(
      class = "shiny-split-layout sidebar-section-title",
      shiny::div(
        style = "width: 9%",
        icon_material_title("balance")
      ),
      shiny::div(
        style = "width: 89%",
        cc_t_span("Comparison")
      )
    ),
    shiny::div(
      id = shiny::NS(id, "compare_widgets"),
      class = "compare-dropdown",
      shiny::div(
        class = "shiny-split-layout sidebar-section-title",
        shiny::div(id = shiny::NS(id, "compare_drop_div"),
                   style = "width: 100%",
                   picker_UI(
                     id = shiny::NS(id, "compare"),
                     var_list = var_list,
                     live_search = TRUE)
        ),
        shinyjs::hidden(
          shiny::div(id = shiny::NS(id, "compare_cbx_div"),
                     style = "width: 15%; margin-bottom: 12px; padding-left: 15px;",
                     checkbox_UI(shiny::NS(id, "compare_cbx"), value = TRUE)
          )
        )
      )
    )
  )
}
