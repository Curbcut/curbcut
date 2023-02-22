#' Use Curbcut javascript functions
#'
#' Curbcut package includes a few javascript functions to supplement the
#' available shiny functions. This function will import and declare them.
#' They can afterwards be used using the `shinyjs` package.
#'
#' @return A `script` tag to be placed in the `ui` function, `ui.R` file.
#' @export
use_curbcut_js <- function() {
  copy_current_url <- readLines(system.file("js_scripts/copy_current_url.js",
                                            package = "curbcut"))
  shiny::tagList(
    shiny::tags$head(shinyjs::extendShinyjs(
      text = copy_current_url,
      functions = c("copy_current_url"))))
}

#' Helper to use JS functions
#'
#' Helper to use javascript functions previously declared by \code{\link{use_curbcut_js}}.
#' It looks the the use of the script is in the UI or in the server. If in the
#' UI, then the js function is outputed as a character (e.g. for use in an `a`
#' tag as the `onclick` argument). Inside a reactive context, it uses the
#' `shinyjs` framework.
#'
#' @param js_fun <`character`> The name of the function to be used in the helper.
#' Must be one of the `functions` argument of the declared functions in
#' \code{\link{use_curbcut_js}}.
#'
#' @return If in the I, returns the function as a character (e.g. for use in an `a`
#' tag as the `onclick` argument). Inside a reactive context, it returns the
#' `shinyjs` framework.
js_function_helper <- function(js_fun) {

  # If outside of a reactive context, use the raw JS fonction.
  session <- shiny::getDefaultReactiveDomain()
  if (is.null(session)) return(paste0(js_fun, "()"))

  # If inside a reactive context, use shinyjs
  return(shinyjs::js[[js_fun]]())
}

#' Copy the current URL
#'
#' This function lets the user copy the current URL. It uses the JS function
#' `copy_current_url` previous declared by \code{\link{use_curbcut_js}}.
#'
#' @return Copies the URL.
#' @export
copy_current_url <- function() {
  js_function_helper("copy_current_url")
}
