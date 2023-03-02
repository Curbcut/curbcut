#' Create a Slider Input Server Module
#'
#' This function creates a server module for a slider input element in Curbcut.
#'
#' @param id <`character`> The ID of the page in which the widget will appear,
#' e.g. `canale`.
#' @param slider_id <`character`> A character string giving the identifier for
#' the picker input object. This will be used as the input's `inputId` and will
#' inform the bookmarking. It needs to be unique in the page. The default value
#' is `"var"`.#' @param value A reactive expression representing the current value of the slider input element.
#' @param value <`reactive`> Represents the new value of the slider (to update).
#' @param min <`reactive`> Represents the minimum value of the slider (to update).
#' @param max <`reactive`> Represents the maximum value of the slider (to update).
#'
#' @return A Shiny server module for a slider input element.
#' @export
slider_server <- function(id, slider_id = "sld",
                          value = shiny::reactive(NULL),
                          min = shiny::reactive(NULL),
                          max = shiny::reactive(NULL)) {
  stopifnot(shiny::is.reactive(value))
  stopifnot(shiny::is.reactive(min))
  stopifnot(shiny::is.reactive(max))

  shiny::moduleServer(id, function(input, output, session) {
    # Reformat the slider_id to make it obvious it's a slider (for bookmark)
    slider_id <- paste0("ccslider_", slider_id)

    # If updates must be done to the slider
    shiny::observe({
      shiny::updateSliderInput(
        session = session,
        inputId = slider_id,
        value = value(),
        min = min(),
        max = max()
      )
    })

    # Output the slider input
    shiny::reactive(input[[slider_id]])
  })
}

#' Create a Slider Input UI Element
#'
#' This function creates a slider input element for use in Curbcut using
#' \code{\link[shiny]{sliderInput}}
#'
#' @param id <`character`> The ID of the page in which the widget will appear,
#' e.g. `canale`.
#' @param slider_id <`character`> string giving the identifier for the slider input
#' object. This will be used as the input's `inputId` and will inform the bookmarking.
#' It needs to be unique in the page. The default value is `"sld"`.
#' @param label <`character`> The label text for the slider input element.
#' Defaults to "Select a year".
#' @param step <`numeric`> Specifies the interval between each selectable value
#' on the slider. Either NULL, the default, which uses a heuristic to determine
#' the step size or a single number. If the values are dates, step is in days;
#' if the values are date-times, step is in seconds. Defaults to 5 has it is the
#' gab between census years and accessibility calculations goes by steps of 5 too.
#' @param min <`numeric|date`> The minimum value of the slider.
#' @param max <`numeric|date`> The maximum value of the slider.
#' @param sep <`character`> The separator character used for displaying the
#' thousand places in numbers. Defaults to an empty string.
#' @param value <`numeric|date`> The initial value of the slider, either a
#' number, a date (class Date), or a date-time (class POSIXt). A length one
#' vector will create a regular slider; a length two vector will create a
#' double-ended range slider. Must lie between min and max. Defaults to `max`.
#' @param width <`character`> The width of the slider input element. Defaults to
#' "95%".
#' @param ... Additional arguments to be passed to \code{\link[shiny]{sliderInput}}
#'
#' @return A Shiny slider input element.
#' @export
slider_UI <- function(id, slider_id = "sld", label = cc_t("Select a year"),
                      step = 5, min, max, sep = "", value = max,
                      width = "95%", ...) {
  # Verify if the widget ID will interfere with bookmark
  slider_id <- widget_id_verif(widget_id = slider_id)

  # Reformat the slider_id to make it obvious it's a slider (for bookmark)
  slider_id <- paste0("ccslider_", slider_id)

  # Declare the input in an optionally styled div
  shiny::sliderInput(
    inputId = shiny::NS(id, slider_id),
    label = cc_t(label),
    min = min,
    max = max,
    step = step,
    sep = sep,
    value = value,
    width = width,
    ...
  )
}
