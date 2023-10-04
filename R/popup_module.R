#' Server-side function for a popup in Curbcut
#'
#' This function defines the server-side logic for a popup element in the Curbcut
#' app It creates a reactive UI element for displaying a popup when
#' specific conditions are met.
#'
#' @param id <`character`> The ID of the page in which the legend will appear,
#' e.g. `alp`.
#' @param content <`character reactive`> A reactive expression that supplies the content
#' to be displayed in the popup.
#' @param show_popup <`logical reactive`> A reactive expression that determines whether
#' the popup should be shown.
#'
#' @export
popup_server <- function(id, content, show_popup) {
  stopifnot(shiny::is.reactive(content))
  stopifnot(shiny::is.reactive(show_popup))

  shiny::moduleServer(id, function(input, output, session) {

    # Initiate the content
    content_val <- shiny::reactiveVal(NULL)
    show_popup_val <- shiny::reactiveVal(FALSE)

    # Update content when it changes. Also, if show_pop changes, make sure
    # the content is up to date.
    shiny::observeEvent({content()
      show_popup()}, {
        content_val(content())
        show_popup_val(show_popup())
      })

    # If show_popup is true, show the popup with an X
    output$popup <- shiny::renderUI({
      if (!show_popup_val()) return(NULL)
      if (is.null(content_val())) return(NULL)

      shiny::div(
        class = "main_panel_popup",
        shiny::div(class = "back-to-map",
                   shiny::actionLink(
                     shiny::NS(id, shiny::NS(id, "back")), "X"
                   )
        ),
        content_val()
      )
    })

    # When the X is clicked, update the content reactive to nothing
    shiny::observeEvent(input$back, {
      content_val(NULL)
      show_popup_val(FALSE)
    })

  })
}

#' UI-side function for a popup in Curbcut
#'
#' This function defines the UI-side markup for a popup element in a Curbcut
#' app. It returns an HTML output placeholder where the popup content
#' will be injected.
#'
#' @param id <`character`> The ID of the page in which the legend will appear,
#' e.g. `alp`.
#'
#' @return An HTML output object for Shiny to render.
#' @export
popup_UI <- function(id) {
  shiny::uiOutput(outputId = shiny::NS(id, "popup"))
}
