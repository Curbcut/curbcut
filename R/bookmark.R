#' Use Bookmark
#'
#' This function is a module server that updates reactive values and UI elements
#' based on URL query string. The function is designed enable bookmarking and sharing
#' of a specific state of the application. It is designed to be placed only once,
#' in the `server.R` file.
#'
#' @param id <`character`> A unique id for the module. Default is "parse_url".
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file.
#' @param parent_session <`session`> The session of the parent module that the
#' bookmarking module is embedded in (`server.R`). Usually `parent_session = session`.
#'
#' @return handles updating the state of the app based on the URL query string.
#' @export
use_bookmark <- function(id = "parse_url", r, parent_session) {

  shiny::moduleServer(id, function(input, output, session) {

    shiny::observeEvent(shiny::parseQueryString(session$clientData$url_search), {

      # Get the URL search query
      query <- shiny::parseQueryString(session$clientData$url_search)

      # If no query, do no action
      if (length(query) == 0) return(NULL)

      # Start by updating app-wide reactives
      if ("region" %in% names(query)) r$region(query$region)
      if ("lng" %in% names(query)) r$lang(query$lng)

      # The rest are tab dependent.
      # Grab the tab
      if (!"tb" %in% names(query)) return(NULL)
      tab <- query$tb
      # Update the current tab
      shiny::updateTabsetPanel(session = parent_session,
                               inputId = "cc_page",
                               selected = tab)

      # Start by the reactive values
      if ("df" %in% names(query)) r[[tab]]$df(query$df)
      if ("zm" %in% names(query)) r[[tab]]$zoom(as.numeric(query$zm))
      if ("s_id" %in% names(query)) {
        new_id <- if (query$s_id %in% c("", "NA")) NA else query$s_id
        r[[tab]]$select_id(new_id)
      }

      # Other updates that must have impacts on widgets
      # Once we have widgets in, we can grab all the inputs of a page,
      # detect the ones that are from our widgets, and throw them to the URL
      # with character shortcuts. We can grab all inputs of a page
      # using names(input) and then just apply over the ones from widgets
      # lapply(all_inputs, \(x) input[[x]]) to get values.

    })
  })
}

