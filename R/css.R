#' Add Curbcut CSS
#'
#' This function adds the Curbcut shared CSS files by creating a resource path to
#' the curbcut package and linking to the CSS stylesheet in the HTML head.
#'
#' @param lang_init <`logical`> Is the app bilingual? In that case, there must
#' be an added class to the body to inform the app it starts as `fr`.
#'
#' @export
use_curbcut_css <- function(lang_init = FALSE) {
  shiny::addResourcePath("curbcut_css", system.file("css", package = "curbcut"))

  shiny::tagList(
    tags$head(tags$link(rel = "stylesheet", type = "text/css",
                        href = paste0("https://fonts.googleapis.com/icon?",
                                      "family=Material+Icons"))),
    shiny::tags$head(shiny::tags$link(rel = "stylesheet", type = "text/css",
                                      href = "curbcut_css/panel_view.css")),
    shiny::tags$head(shiny::tags$link(rel = "stylesheet", type = "text/css",
                                      href = "curbcut_css/language_span.css")),
    if (lang_init) tags$body(class = "user-lang-fr")
  )
}
