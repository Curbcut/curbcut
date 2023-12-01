#' Server logic for comparing variables in Curbcut
#'
#' This function provides the server logic for comparing the main variable with
#' different variables in Curbcut.
#'
#' @param id <`character`> The ID of the page in which the module will appear,
#' e.g. `alp`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param var_list <`reactive list`> Choices to display in the picker input. Normally
#' made using \code{\link{dropdown_make}}.
#' @param time <`reactive numeric vector`> Vector of time values to use for
#' appending a time to the variables picked. The returned vector will be the
#' same length as this argument.
#' @param show_panel <`reactive logical`> Controls whether the comparison panel
#' should be shown.
#' @param scales_as_DA <`character vector`> A character vector of `scales` that
#' should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
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
                           show_panel = shiny::reactive(TRUE),
                           scales_as_DA = shiny::reactive(c("building", "street"))) {
  stopifnot(shiny::is.reactive(var_list))
  stopifnot(shiny::is.reactive(time))
  stopifnot(shiny::is.reactive(show_panel))

  shiny::moduleServer(id, function(input, output, session) {
    # Get the var_right reactive
    var_right <- picker_server(
      id = "compare",
      r = r,
      var_list = var_list,
      time = time # ,
      # identifier = "housing_comparedrop"
    )

    # Is var_right available in the scale? Useful for when, e.g. variables are
    # available at the DB level, but the comapare variable isn't.
    avail_comparison <- shiny::reactive({
      cvars <- unname(unlist(var_list()))
      cvars <- cvars[cvars != " "]

      scale <- treat_to_DA(scales_as_DA(), r[[id]]$scale())
      scale_files <- get_from_globalenv(sprintf("%s_files", scale))
      return(all(cvars %in% scale_files))
    })

    # If we shouldn't show the panel
    selector <- sprintf("#%s-%s-compare_widgets", id, id)
    new_div <- shiny::NS(id, shiny::NS(id, "compare_warn"))
    shiny::observeEvent(avail_comparison(), {
      shinyjs::toggle("compare_widgets", condition = avail_comparison())

      if (avail_comparison()) {
        shiny::removeUI(selector = sprintf("#%s", new_div))
      } else {
        shiny::removeUI(selector = sprintf("#%s", new_div))

        scale_name <- cc_t(zoom_get_name(r[[id]]$scale()), lang = r$lang())
        scale_name <- tolower(scale_name)
        txt <- sprintf(cc_t("Comparison is unavailable at the <b>%s</b> scale.",
                            lang = r$lang()), scale_name)
        txt <- shiny::p(class = "sus-sidebar-widget-warning-text", shiny::HTML(txt))
        shiny::insertUI(selector = selector, where = "beforeBegin",
                        ui = shiny::div(id = new_div, txt))
      }
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
      picker_UI(
        id = shiny::NS(id, "compare"),
        var_list = var_list,
        live_search = TRUE
      )
    )
  )
}
