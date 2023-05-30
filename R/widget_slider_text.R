#' Shiny module for a slider text input control
#'
#' @param id <`character`> The ID of the page in which the widget will appear,
#' e.g. `canale`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param slider_text_id <`character`> string giving the identifier for the slider input
#' object. This will be used as the input's `inputId` and will inform the bookmarking.
#' It needs to be unique in the page. The default value is `"slt"`.
#' @param choices <`reactive character vector`> Choices that the user can
#' select from.
#' @param selected <`reactive character`> Represents the selected value to update
#' to.
#'
#' @return A reactive value representing the user's selection.
#' @seealso \code{\link{slider_text_UI}}
#' @export
slider_text_server <- function(id, r, slider_text_id = "slt",
                               choices = shiny::reactive(NULL),
                               selected = shiny::reactive(NULL)) {
  stopifnot(shiny::is.reactive(choices))
  stopifnot(shiny::is.reactive(selected))

  shiny::moduleServer(id, function(input, output, session) {
    # Reformat the slider_text_id to make it obvious it's a slider_text (for bookmark)
    slider_text_id <- paste0("ccslidertext_", slider_text_id)

    # Update choices if they change. It creates interferences with the pickers
    # on the climate risk module of Curbcut Montreal (for an unknown reason).
    # By updating it after the pickers are initiated, we fix the issue.
    shiny::observeEvent(choices(), {
      shinyjs::delay(
        500,
        shinyWidgets::updateSliderTextInput(
          session = session,
          inputId = slider_text_id,
          choices = choices()
        )
      )
    })

    # Update selected if it changes
    shiny::observe({
      shinyWidgets::updateSliderTextInput(
        session = session,
        inputId = slider_text_id,
        selected = selected()
      )
    })

    # Return the selection as a reactive
    return(shiny::reactive(input[[slider_text_id]]))
  })
}

#' Create a slider text UI element for selecting a value from a list of choices.
#'
#' @param id <`character`> The ID of the page in which the widget will appear,
#' e.g. `canale`.
#' @param slider_text_id <`character`> string giving the identifier for the slider input
#' object. This will be used as the input's `inputId` and will inform the bookmarking.
#' It needs to be unique in the page. The default value is `"slt"`.
#' @param label <`character`> A text label to display next to the checkbox input.
#' If NULL (default), no label will be displayed.
#' @param choices <`character vector`> Choices that the user can select from.
#' @param selected <`character`> The initially selected value. If length > 1,
#' create a range slider.
#' @param force_edges <`logical`> Slider will be always inside it's container.
#' @param ... Additional arguments to pass to the \code{\link{sliderTextInput}}
#' function.
#'
#' @return A \code{\link{sliderTextInput}} object that can be used as an input
#' in the app.
#' @seealso \code{\link{slider_text_server}}
#' @export
slider_text_UI <- function(id, slider_text_id = "slt", label = cc_t("Select a year"),
                           choices, selected = NULL, force_edges = TRUE, ...) {
  # Verify if the widget ID will interfere with bookmark
  slider_text_id <- widget_id_verif(widget_id = slider_text_id)

  # Reformat the slider_text_id to make it obvious it's a slider_text (for bookmark)
  slider_text_id <- paste0("ccslidertext_", slider_text_id)

  shinyWidgets::sliderTextInput(
    inputId = shiny::NS(id, slider_text_id),
    label = label,
    choices = choices,
    selected = selected,
    force_edges = force_edges,
    ...
  )
}
