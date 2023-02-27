#' Shiny module for a checkbox input control
#'
#' This function updates a checkbox input control for use in Shiny server. The
#' checkbox allows users to select a boolean value. The function uses the
#' \code{\link{checkboxInput}} function and returns its value as a reactive object.
#'
#' @param id <`character`> The ID of the page in which the widget will appear,
#' e.g. `canale`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file.
#' @param checkbox_id <`character`> string giving the identifier for the checkbox input
#' object. This will be used as the input's `inputId` and will inform the bookmarking.
#' It needs to be unique in the page. The default value is `"cbx"`.
#' @param label <`character`> A text label to display next to the checkbox
#' input. If NULL (default), no label will be displayed.
#' @param event_reset <`reactive expression`> A change in this reactive will trigger
#' the reset of the checkbox value to TRUE. If NULL (default), the checkbox value
#' will never be reset.
#'
#' @return A reactive object representing the value of the checkbox input control.
#' @seealso \code{\link{checkbox_UI}}
#' @export
checkbox_server <- function(id, r, checkbox_id = "cbx",
                            label = shiny::reactive(NULL),
                            event_reset = shiny::reactive(NULL)) {
  stopifnot(shiny::is.reactive(label))
  stopifnot(shiny::is.reactive(event_reset))

  shiny::moduleServer(id, function(input, output, session) {
    # Reformat the checkbox_id to make it obvious it's a checkbox (for bookmark)
    checkbox_id <- paste0("cccheckbox_", checkbox_id)

    # Translate label
    label_t <- shiny::reactive(cc_t(label(), lang = r$lang()))

    # Updates if the label has changed
    shiny::observe({
      shiny::updateCheckboxInput(
        session = session,
        inputId = checkbox_id,
        label = label_t()
      )
    })

    # Whenever the event gets triggered or changed, the value of the checkbox
    # turns back to TRUE
    shiny::observeEvent(event_reset(),
      {
        shiny::updateCheckboxInput(
          session = session,
          inputId = checkbox_id,
          value = TRUE
        )
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    # Return the value of the checkbox
    return(shiny::reactive(input[[checkbox_id]]))
  })
}

#' Create a checkbox input control
#'
#' This function creates a checkbox input control for use in Curbcut. The checkbox
#' allows users to select a logical value. The function uses the \code{\link{checkboxInput}}
#' function and returns its output.
#'
#' @param id <`character`> The ID of the page in which the widget will appear,
#' e.g. `canale`.
#' @param checkbox_id <`character`> string giving the identifier for the checkbox input
#' object. This will be used as the input's `inputId` and will inform the bookmarking.
#' It needs to be unique in the page. The default value is `"cbx"`.
#' @param label <`character`> A text label to display next to the checkbox input.
#' If NULL (default), no label will be displayed.
#' @param value <`logical`> The initial value of the checkbox input. Defaults to
#' `TRUE`.
#' @param ... Additional arguments to pass to the \code{\link{checkboxInput}} function.
#'
#' @return A checkbox input control for use in Shiny UI.
#' @seealso \code{\link{checkbox_server}}
#' @export
checkbox_UI <- function(id, checkbox_id = "cbx", label = NULL, value = TRUE,
                        ...) {

  # Verify if the widget ID will interfere with bookmark
  checkbox_id <- widget_id_verif(widget_id = checkbox_id)

  # Reformat the checkbox_id to make it obvious it's a checkbox (for bookmark)
  checkbox_id <- paste0("cccheckbox_", checkbox_id)

  # Get the checkbox UI
  shiny::checkboxInput(
    inputId = shiny::NS(id, checkbox_id),
    label = label,
    value = value,
    ...
  )
}
