#' Use Curbcut javascript functions
#'
#' Curbcut package includes a few javascript functions to supplement the
#' available shiny functions. This function will import and declare them.
#' They can afterwards be used using the `shinyjs` package.
#'
#' @return A `script` tag to be placed in the `ui` function, `ui.R` file.
#' @export
use_curbcut_js <- function() {
  copy_current_url <- readLines(system.file("js_scripts/shinyjs/copy_current_url.js",
    package = "curbcut"
  ))
  set_language <- readLines(system.file("js_scripts/shinyjs/language.js",
    package = "curbcut"
  ))
  highlightOptions <- readLines(system.file("js_scripts/shinyjs/highlight_dropdown_option.js",
    package = "curbcut"
  ))

  # Add the JS resource path
  shiny::addResourcePath("curbcut_js", system.file("js_scripts",
    package = "curbcut"
  ))

  # List all JS files
  js_files <- paste0(
    "curbcut_js/",
    list.files(system.file("js_scripts", package = "curbcut"))
  )
  js_files <- js_files[grepl("\\.js$", js_files)]
  js_files <- paste0(js_files, "?id=2")
  # Build the tags for the style files
  js_tags <- shiny::tagList(
    lapply(js_files, function(x) {
      shiny::tags$head(shiny::tags$script(src = x))
    })
  )

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
    # Highlight options
    shiny::tags$head(shinyjs::extendShinyjs(
      text = highlightOptions,
      functions = c("highlightOptions")
    )),
    # Change window title
    shiny::tags$head(shiny::tags$script(
      shiny::HTML(
        paste0('Shiny.addCustomMessageHandler("changetitle", function(x)
                   {document.title=x});')
      ),
    )),
    # Open URL with blank target
    shiny::tags$head(shiny::tags$script(
      shiny::HTML("
      Shiny.addCustomMessageHandler('openURL', function(url) {
        window.open(url, '_blank');
      });
    ")
    )),
    # Allow hover with texts on elements of the picker menus
    shiny::tags$head(shiny::tags$script(
      "var myDefaultWhiteList = $.fn.selectpicker.Constructor.DEFAULTS.whiteList;
    myDefaultWhiteList.div = ['title'];"
    )),
    # (linked with the previous) Make the hover on the block rather than the
    # text itself
    shiny::tags$head(shiny::tags$style("span.text {display: block !important;}")),
    # Other JS scripts
    js_tags,
    # Script needed for cookies
    shiny::tags$head(shiny::tags$script(src = paste0(
      "https://cdn.jsdelivr.net/npm/js-cookie@rc/",
      "dist/js.cookie.min.js"
    )))
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

#' Highlight options in a dropdown menu
#'
#' This function highlights specified options in a dropdown menu of a shinyWidgets
#' picker input. It requires the dropdown options to highlight and the desired color.
#'
#' @param dropdown_identifier <`character`> The dropdown identifier. Must have been
#' previously added using the options argument of picker widget, e.g.:
#' `options = list(identifier = "dropdown_id")`. In the latter case, the value of
#' this argument should be 'dropdown_id'.
#' @param options <`character vector`> A character vector of option labels to be
#' highlighted in the dropdown. It's the user-facing option, e.g. 'housing_tenant'
#' is 'Tenant-occupied (%)'. The latter must be used.
#' @param color <`character`> The color (as a hex code) for highlighting. Defaults
#' to NULL which mean the light blue of colours_dfs$bivar will be used.
#'
#' @return Invisible NULL. The function is called for its side effects.
#' @export
highlight_dropdown <- function(dropdown_identifier, options,
                               color = NULL) {
  if (is.null(shiny::getDefaultReactiveDomain())) {
    stop("This function can only be used in a reactive context.")
  }
  # If not color is supplied, use light blue from colours_dfs$bivar.
  if (is.null(color)) {
    colours_dfs <- get_from_globalenv("colours_dfs")
    color <- colours_dfs$bivar$fill[colours_dfs$bivar$group == "1 - 2"]
  }

  # Collapse the options so that it's a vector of length 1 separated with
  # double ; (which is the code used to split variables in the JS)
  options <- paste0(options, collapse = ";;")

  # The hex needs to be added with the options (last)
  options <- sprintf("%s;;%s;;%s", dropdown_identifier, options, color)

  # Update the dropdown options background colors
  shinyjs::js$highlightOptions(options)
}
