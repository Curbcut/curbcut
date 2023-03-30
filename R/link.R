#' Update tabset panel and select an ID
#'
#' This function updates the tabset panel with the given \code{page} and selects
#' the corresponding \code{select_id}. The \code{page} and \code{select_id}
#' arguments must be reactive objects.
#'
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param page A reactive object containing the ID of the tab to be selected.
#' @param select_id A reactive object containing the ID of the item to be selected
#' within the selected tab.
#'
#' @export
link <- function(r, page, select_id = NA, df = NULL) {

  # Detect if we're in a reactive context
  if (is.null(shiny::getDefaultReactiveDomain())) {
    stop("The function needs to be used in a reactive context")
  }

  # Update the current tab
  shiny::updateTabsetPanel(
    session = r$server_session(),
    inputId = "cc_page",
    selected = page
  )

  # Make a namespace function that goes twice deep in each page module
  ns <- function(widget) {
    paste(page, page, widget, sep = "-")
  }

  # After half a second and the tab is opened, update the widgets
  shinyjs::delay(500, {
    # Recreate the `df`
    if (!is.null(df)) {
      shinyWidgets::updateSliderTextInput(
        session = r$server_session(),
        inputId = ns("zoom_slider-ccslidertext_slt"),
        selected = zoom_get_name(df, lang = r$lang())
      )
    }
  })

  # If there is a selection, update the map view through zoom and coords, and
  # the selection
  if (!is.na(select_id)) {

    # Update the zoom and map location
    if (!is.null(df)) {
      # Zoom
      mzl <- paste("map_zoom_levels", r$region(), sep = "_")
      map_zoom_level <- get_from_globalenv(mzl)

      zoom <-
        map_zoom_level[is_scale_df(names(map_zoom_level), df, vectorized = TRUE)]
      zoom <- unname(zoom)

      if (zoom == 0) zoom <- 9

      # Location
      df_data <- get_from_globalenv(df)
      coords <- df_data$centroid[df_data$ID == select_id][[1]]
      coords <- sapply(coords, round, digits = 2)

      # Update the reactives
      r[[page]]$zoom(zoom)
      r[[page]]$coords(coords)
      # Update the map using the zoom and location
      rdeck::rdeck_proxy(session = r$server_session(),
                         id = ns("map"),
                         initial_view_state =
                           rdeck::view_state(center = coords, zoom = zoom))
    }

    # Update the selection
    r[[page]]$select_id(select_id)
  }

}


