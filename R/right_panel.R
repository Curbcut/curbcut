#' Map page right panel
#'
#' This function creates a right panel for the Map page of Curbcut.
#' The panel is an Shiny absolute panel with a default panel class and two nested
#' div elements. The first div element has a custom class for styling purposes,
#' and can contain multiple other Shiny widgets or HTML elements.
#'
#' @param id <`character`> The ID of the page in which this module will appear,
#' e.g. `alp`.
#' @param ... Additional arguments to be passed to the \code{\link[shiny]{absolutePanel}}
#' function. Would normally be the `compare_UI`, `explore_UI`, `dyk_UI`. TKTK LINK
#'
#' @return An absolutePanel object with a custom class for the content div.
#' @export
right_panel <- function(id, ...) {
  shiny::absolutePanel(
    id = shiny::NS(id, "right_panel"),
    class = "panel panel-default sus-map-panel sus-scroll",
    shiny::div(
      class = "sus-map-panel-content scrollable-div",
      id = "cc-right-panel", ...
    )
  )
}
