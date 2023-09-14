#' Retrieve the Appropriate Zoom Level
#'
#' This function obtains the zoom level corresponding to the scale of a data
#' frame. If the zoom level is 0, it defaults to 9.
#'
#' @param zoom_levels <`named vector`> A named vector of potential zoom levels.
#' @param df <`character`> df used for scale matching.
#'
#' @return Returns the appropriate zoom level.
link_get_zoom <- function(zoom_levels, df) {
  if (is.null(df)) {
    return(NULL)
  }
  zoom <- zoom_levels[is_scale_df(names(zoom_levels), df, vectorized = TRUE)]
  zoom <- unname(zoom)

  if (zoom == 0) {
    zoom <- if (length(zoom_levels) > 1) unname(zoom_levels[2] - 1) else 9
  }

  return(zoom)
}

#' Update tabset panel and select an ID
#'
#' This function updates the tabset panel with the given \code{page} and selects
#' the corresponding \code{select_id}. The \code{page} and \code{select_id}
#' arguments must be reactive objects.
#'
#' @param session <`session`> A shiny session object from the server function.
#' The actual session of the module, should be  `session = session`
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param page <`character`> The id of the tab to be opened.
#' @param select_id <`character`> ID of the item to be selected within the newly
#' opened page.
#' @param df <`character`> Combination of region and scale to update the zoom
#' of the newly opened page's map.
#'
#' @export
link <- function(session, r, page, region = r$region(),
                 select_id = NA, df = NULL,
                 zoom_levels = get_from_globalenv(paste("map_zoom_levels", region, sep = "_")),
                 zoom = link_get_zoom(zoom_levels, df)) {
  # Detect if we're in a reactive context
  if (is.null(shiny::getDefaultReactiveDomain())) {
    stop("The function needs to be used in a reactive context")
  }

  # Update the current tab
  if (!is.null(page)) {
    shiny::updateTabsetPanel(
      session = r$server_session(),
      inputId = "cc_page",
      selected = page
    )
  }

  # After half a second and the tab is opened, update the widgets
  shinyjs::delay(500, {
    # Recreate the `df`
    if (!is.null(df)) {
      shinyWidgets::updateSliderTextInput(
        session = r$server_session(),
        inputId = ns_doubled(
          page_id = page,
          element = "zoom_slider-ccslidertext_slt"
        ),
        selected = zoom_get_name(df, lang = r$lang())
      )
    }

    if (!is.na(select_id)) {
      if (!is.null(df)) {
        if (!is.null(zoom)) r[[page]]$zoom(zoom)

        # df_data <- get_from_globalenv(df)
        # # Skip the zoom update if the 'centroid' is not in the df
        # if (!"centroid" %in% names(df_data)) {
        #   return(NULL)
        # }
        # coords <- df_data$centroid[df_data$ID == select_id][[1]]
        # coords <- sapply(coords, round, digits = 2)
        # cc.map::map_viewstate(
        #   session = session,
        #   map_ID = "map",
        #   longitude = as.numeric(coords[1]),
        #   latitude = as.numeric(coords[2]),
        #   zoom = r[[page]]$zoom()
        # )

        r[[page]]$select_id(select_id)
      }
    }
  })
}
