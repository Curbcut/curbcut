#' Time Slider Server and UI Functions for R Shiny
#'
#' A Curbcut module that provides a user interface for selecting a single year or
#' a range of years. The output is a reactive value containing either the selected
#' year or a vector of two years.
#'
#' @param id <`character`> The ID of the page in which the legend will appear,
#' e.g. `alp`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#'
#' @return A reactive value containing either the selected year or a vector of two years.
#' @export
time_slider_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    # When to grab which time value
    # The slider_server function is expecting the
    # correct namespaced id, but it is receiving only the base id as an argument.
    # This causes the mismatch between the input ID in the UI and the server.
    # To fix this, I need to use the NS() function when calling the slider_server
    # function on the server side.
    slider_uni <- slider_server(id = shiny::NS(id, id), slider_id = "slu")
    slider_bi <- slider_server(id = shiny::NS(id, id), slider_id = "slb")
    slider_switch <- checkbox_server(
      id = shiny::NS(id, id), r = r,
      label = shiny::reactive("Compare dates")
    )

    # Enable or disable first and second slider
    shiny::observeEvent(slider_switch(), {
      shinyjs::toggle(shiny::NS(id, shiny::NS(id, "ccslider_slu")), condition = !slider_switch())
      shinyjs::toggle(shiny::NS(id, shiny::NS(id, "ccslider_slb")), condition = slider_switch())
    })

    # Grab the right time
    time <- shiny::reactive(if (slider_switch()) slider_bi() else slider_uni())

    # Return the time
    return(time)
  })
}

#' @describeIn time_slider_server Create the UI for the time slider module
#' @param min <`numeric`> The minimum value for the slider.
#' @param max <`numeric`> The maximum value for the slider.
#' @param step <`numeric`> Specifies the interval between each selectable value
#' on the slider. Either NULL, the default, which uses a heuristic to determine
#' the step size or a single number. If the values are dates, step is in days;
#' if the values are date-times, step is in seconds. Defaults to 5 has it is the
#' gap between census years and accessibility calculations goes by steps of 5 too.
#' @param double_value <`numeric vector`> A vector of two initial values for
#' the bi-directional slider.
#' @export
time_slider_UI <- function(id, min, max, step, double_value) {
  if (length(double_value) != 2) {
    stop("length of `double_value` must be 2.")
  }

  length <- (max - min) / step
  ys_classes <- if (length < 10) "year-slider-slim" else "year-slider"

  shiny::div(
    id = shiny::NS(id, "year_sliders"),
    class = ys_classes,
    shiny::hr(id = shiny::NS(id, "above_year_hr")),
    shiny::div(
      class = "shiny-split-layout sidebar-section-title",
      shiny::div(
        style = "width: 9%",
        icon_material_title("date_range")
      ),
      shiny::div(
        style = "width: 24%",
        shiny::tags$span(
          id = shiny::NS(id, "year_label"),
          cc_t("Time", force_span = TRUE)
        )
      ),
      shiny::div(
        id = shiny::NS(id, "compare_dates"),
        style = "width: 64%; margin:0px !important; text-align: right;",
        checkbox_UI(
          id = shiny::NS(id, id),
          label = cc_t("Compare dates", force_span = TRUE),
          value = FALSE
        )
      )
    ),
    slider_UI(
      id = shiny::NS(id, id), slider_id = "slu", min = min, max = max,
      step = step, label = NULL
    ),
    shinyjs::hidden(slider_UI(
      id = shiny::NS(id, id), slider_id = "slb", min = min, max = max,
      step = step, label = NULL, value = double_value
    ))
  )
}
