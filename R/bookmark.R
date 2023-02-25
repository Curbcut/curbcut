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
      if (length(query) == 0) {
        return(NULL)
      }

      # Create a function to test whether a value is numeric
      is_numeric <- function(x) {
        # Convert the input value to a numeric type using as.numeric()
        # Check whether the conversion was successful using is.na()
        # If the conversion was successful, the value is numeric
        !is.na(suppressWarnings(as.numeric(x)))
      }

      # Start by updating app-wide reactives
      if ("reg" %in% names(query)) {
        reg <- r$region(query$region)
        # As we convert an ID in the region table to a numeric for smaller URL
        # bookmarking, retrieve the region string using its index.
        regions_dictionary <- get_from_globalenv("regions_dictionary")
        reg <- regions_dictionary$region[as.numeric(reg)]
        r$region(reg)
      }
      if ("lng" %in% names(query)) r$lang(query$lng)

      # The rest are tab dependent.
      # Grab the tab
      if (!"tb" %in% names(query)) {
        return(NULL)
      }

      # As we convert an ID in the modules table to a numeric for smaller URL
      # bookmarking, retrieve the page ID using its index.
      tab <- query$tb
      if (is_numeric(tab)) {
        modules <- get_from_globalenv("modules")
        tab <- modules$id[as.numeric(tab)]
      }
      # Update the current tab
      shiny::updateTabsetPanel(
        session = parent_session,
        inputId = "cc_page",
        selected = tab
      )

      # Start by the map viewstate (if a map module)
      if ("zm" %in% names(query)) r[[tab]]$zoom(as.numeric(query$zm))
      if ("crds" %in% names(query)) {
        coords <- query$crds
        coords <- strsplit(coords, ";")[[1]]
        r[[tab]]$coords(coords)
      }

      # Other updates that must have impacts on widgets
      # Once we have widgets in, we can grab all the inputs of a page,
      # detect the ones that are from our widgets, and throw them to the URL
      # with character shortcuts. We can grab all inputs of a page
      # using names(input) and then just apply over the ones from widgets
      # lapply(all_inputs, \(x) input[[x]]) to get values.

      # Finish by the ID selection
      if ("sid" %in% names(query)) {
        new_id <- if (query$sid %in% c("", "NA")) NA else query$sid
        r[[tab]]$select_id(new_id)
      }

    }, priority = -5)
  })
}
