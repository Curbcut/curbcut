#' Server function for creating a bookmarkable URL with Curbcut widgets and an
#' rdeck map
#'
#' This function creates a module server for bookmarkable app that use Curbcut
#' widgets and an rdeck map. It bookmarks the tab id, the region, language,
#' widgets, the map view state (zoom and lat/lon coordinates), the selected id
#' and the `df`.
#'
#' @param id <`character`> The ID of the page in which the bookmark_server function
#' will appear, e.g. `canale`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file.
#' @param select_id <`character`> the current selected ID, usually `r[[id]]$select_id()`
#'
#' @return The module server function for creating bookmarkable URLs.
#' @export
bookmark_server <- function(id, r, select_id = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    # Grab all inputs built using Curbcut widgets
    widgets <- shiny::reactive({
      # Grab all the available input in the page and subset the Curbcut widgets
      all_input <- names(input)
      if (length(all_input) == 0) {
        return(NULL)
      }
      all_widgets <- grep("ccpicker_|ccslidertext_|cccheckbox_",
        all_input,
        value = TRUE
      )
      # If there are no widgets, return null
      if (length(all_widgets) == 0) {
        return(NULL)
      }

      # Get the widget's value
      sapply(all_widgets, \(x) {
        input[[x]]
      }, simplify = FALSE, USE.NAMES = TRUE)
    })


    # Grab the map's view state (if there is)
    map_viewstate <- shiny::reactive({
      tryCatch(rdeck::get_view_state("map"),
        error = function(e) NULL
      )
    })


    # Produce the URL
    url <- shiny::reactive({
      bookmark_build_url(
        id = id,
        region = r$region(),
        lang = r$lang(),
        widgets = widgets(),
        map_viewstate = map_viewstate(),
        select_id = select_id()
      )
    })

    # Update the URL
    shiny::observe(shiny::updateQueryString(url()))
  })
}

#' Build a bookmarkable URL
#'
#' This function builds a URL that can be used to bookmark the state of any
#' module of Curbcut. The URL includes information about the selected region, ID,
#' language, widgets, map viewstate, elected ID, and `df`. If the page is in the
#' module's table, it makes the value a numeric. If the region is in the regions
#' dictionary, it makes the region a numeric as well. It also makes the value
#' of pickers numeric if they are present in the `variables` table, all to shorten
#' the length of the URL.
#'
#' @param id <`character`> A string or numeric value representing the ID of the
#' selected page.
#' @param region <`character`> A string or numeric value representing the
#' selected region.
#' @param lang <`¸character`> An optional string value representing the selected
#' language. Default is NULL.
#' @param widgets A named list representing the values of the Curbcut widgets present
#' on the page.
#' @param map_viewstate <`named list`> An optional named list representing the
#' state of the map view. Usually `rdeck::get_view_state("map")`.
#' @param select_id <`character`> An optional string or numeric value representing
#' the ID of the selected item.
#'
#' @return A character string representing the bookmarkable URL.
bookmark_build_url <- function(id, region, lang = NULL, widgets, map_viewstate,
                               select_id) {
  # If the page is in the module's table, make it a numeric
  modules <- get_from_globalenv("modules")
  if (id %in% modules$id) id <- which(modules$id == id)

  # Make the region a numeric too
  regions_dictionary <- get_from_globalenv("regions_dictionary")
  if (region %in% regions_dictionary$region) {
    region <- which(regions_dictionary$region == region)
  }

  # First string
  url <- sprintf("/?reg=%s&tb=%s", region, id)

  # If language
  if (!is.null(lang)) url <- sprintf("%s&lng=%s", url, lang)

  # Process the widgets
  if (!is.null(widgets)) {
    widgets_processed <-
      mapply(\(name, value) {
        # If the value is a checkbox-like, make it shorter
        if (isTRUE(value)) value <- "T"
        if (isFALSE(value)) value <- "F"
        # If from a picker, grab the row ID if available
        if (grepl("ccpicker_", name)) {
          # Throw warning if numeric, which won't work with bookmarking
          if (is.numeric(value)) {
            stop("A picker should not be numeric. It will interfere with bookmarking.")
          }
          value <- var_row_index(value)
        }
        # Grab the code rather than the label for the zoom module
        if (name == "zoom_slider-ccslidertext_sldt") {
          value <- zoom_get_code(value, lang = lang)
        }

        bookmark_codes <- curbcut::bookmark_codes
        # If the name is part of a known code, switch it
        if (name %in% names(bookmark_codes)) {
          name <- bookmark_codes[which(names(bookmark_codes) == name)]
        }

        # Return the widgets name with their value
        return(paste0(name, ":", value))
      }, names(widgets), widgets)
    widgets_processed <- paste0(widgets_processed, collapse = ";")

    url <- sprintf("%s&wgt=%s", url, widgets_processed)
  }

  # Map arguments
  if (!is.null(map_viewstate)) {
    zm <- curbcut::zoom_get(map_viewstate$zoom)
    lon <- round(as.numeric(map_viewstate$longitude), digits = 2)
    lat <- round(as.numeric(map_viewstate$latitude), digits = 2)
    coords <- paste(lon, lat, sep = ";")
    url <- sprintf("%s&zm=%1.1f&crds=%s", url, zm, coords)
  }

  # Selected ID
  if (!is.null(select_id)) url <- sprintf("%s&sid=%s", url, select_id)

  # Return the final URL
  return(url)
}
