#' Add Curbcut CSS
#'
#' This function adds the Curbcut shared CSS files by creating a resource path to
#' the curbcut package and linking to the CSS stylesheet in the HTML head.
#'
#' @export
use_curbcut_css <- function() {
  shiny::addResourcePath("curbcut_css", system.file("css", package = "curbcut"))

  shiny::tagList(
    tags$head(tags$link(rel = "stylesheet", type = "text/css",
                        href = paste0("https://fonts.googleapis.com/icon?",
                                      "family=Material+Icons"))),
    shiny::tags$head(shiny::tags$link(rel = "stylesheet", type = "text/css",
                                      href = "curbcut_css/panel_view.css"))
  )
}
