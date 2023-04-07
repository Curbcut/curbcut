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
    package = "curbcut"
  ))
  set_language <- readLines(system.file("js_scripts/language.js",
    package = "curbcut"
  ))

  # Add the JS resource path
  shiny::addResourcePath("curbcut_js", system.file("js_scripts",
    package = "curbcut"
  ))

  shiny::tagList(
    # Copy URL
    shiny::tags$head(shinyjs::extendShinyjs(
      text = copy_current_url,
      functions = c("copy_current_url")
    )),
    # Set language
    shiny::tags$head(shinyjs::extendShinyjs(
      text = set_language,
      functions = c("set_language")
    )),
    # Right panel hides some div depending on window size
    shiny::tags$head(shiny::tags$script(
      src = "curbcut_js/right_panel.js"
    )),
    # Change window title
    shiny::tags$head(shiny::tags$script(
      shiny::HTML(
        paste0('Shiny.addCustomMessageHandler("changetitle", function(x)
                   {document.title=x});')
      ),
    )),
    # Allow hover with texts on elements of the picker menus
    shiny::tags$head(shiny::tags$script(
      "var myDefaultWhiteList = $.fn.selectpicker.Constructor.DEFAULTS.whiteList;
    myDefaultWhiteList.div = ['title'];"
    )),
    # (linked with the previous) Make the hover on the block rather than the
    # text itself
    shiny::tags$head(shiny::tags$style("span.text {display: block !important;}")),
  )
}

#' Copy the current URL
#'
#' This function lets the user copy the current URL. It uses the JS function
#' `copy_current_url` previous declared by \code{\link{use_curbcut_js}}.
#'
#' @return Copies the URL.
#' @export
copy_current_url <- function() {
  if (!is.null(shiny::getDefaultReactiveDomain())) {
    stop("This function can only be used in a non-reactive context.")
  }

  return("copy_current_url()")
}

#' Update language
#'
#' This function changes the active language dynamically based on the user
#' preferences. It both uses the JS `set_language` function to update which
#' `span` gets shown ("user- lang-en" or "user-lang-fr") and updates the `r$lang()`
#' value.
#'
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param lang <`character`> Value of the new language (`"fr"` or `"en"`)
#'
#' @return Sets the language to the value of `lang`
#' @export
update_lang <- function(r, lang) {
  if (is.null(shiny::getDefaultReactiveDomain())) {
    stop("This function can only be used in a reactive context.")
  }

  # Update the page so that we display, in the UI, the language of the user.
  shinyjs::js$set_language(lang)

  # Update the reactive language function for the server Shiny side.
  r$lang(lang)
}
