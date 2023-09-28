#' Add Curbcut CSS
#'
#' This function adds the Curbcut shared CSS files by creating a resource path to
#' the curbcut package and linking to the CSS stylesheet in the HTML head.
#'
#' @param lang_init <`character`> The language that should be used as the default
#' language to display.
#'
#' @export
use_curbcut_css <- function(lang_init = "en") {
  # Add the CSS resource path
  shiny::addResourcePath("curbcut_css", system.file("styles", package = "curbcut"))
  shiny::addResourcePath("curbcut_fonts", system.file("fonts", package = "curbcut"))

  # List all style files
  style_files <- paste0(
    "curbcut_css/",
    list.files(system.file("styles", package = "curbcut"))
  )
  style_files <- paste0(style_files, "?id=1")
  # Build the tags for the style files
  style_tags <- shiny::tagList(
    lapply(style_files, function(x) {
      shiny::tags$head(shiny::tags$link(rel = "stylesheet", type = "text/css", href = x))
    })
  )

  shiny::tagList(
    shiny::tags$head(shiny::tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = paste0(
        "https://fonts.googleapis.com/icon?",
        "family=Material+Icons"
      )
    )),
    style_tags,
    shiny::tags$body(class = sprintf("user-lang-%s", lang_init))
  )
}
