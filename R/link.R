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
    zoom <- if (length(zoom_levels) > 1) unname(zoom_levels[2] - 0.2) else 9
  }

  return(zoom)
}

#' Update tabset panel and select an ID
#'
#' This function updates the tabset panel with the given \code{page} and selects
#' the corresponding \code{select_id}. The \code{page} and \code{select_id}
#' arguments must be reactive objects.
#'
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param page <`character`> The id of the tab to be opened.
#' @param select_id <`character`> ID of the item to be selected within the newly
#' opened page.
#' @param region <`character`> Character string specifying the name of the region.
#' Defaults to `r$region()`.
#' @param df <`character`> Combination of region and scale to update the zoom
#' of the newly opened page's map.
#' @param date <`numeric vector`> Change the date on the page. It will be changing
#' the dates widgets from the autovars module, with html id as `alp-alp-alp-ccslider_slb`.
#' @param var_left <`character`> Character string of the compare variable
#' to update to. It will be changing the html id of the `alp-alp-compare-ccpicker_var`
#' widget.
#' @param var_right <`character`> Character string of the var_left variable
#' to update to. It will be `r[[id]]$var_left_force()`, which will re-draw the
#' autovars module with the new selected variable.
#' @param zoom_levels <`named numeric vector`> A named numeric vector of zoom
#' levels. Usually one of the `map_zoom_levels_x`, or the output of
#' \code{\link{zoom_get_levels}}.
#' @param zoom <`numeric`> Zoom used to update the map viewstate. Defaults to
#' using \code{\link{link_get_zoom}}, which will grab the values in the `zoom_levels`
#' argument.
#'
#' @export
link <- function(r, page, region = r$region(), select_id = NA, df = NULL,
                 date = NULL, var_left = NULL, var_right = NULL,
                 zoom_levels = get_from_globalenv(paste("map_zoom_levels", region, sep = "_")),
                 zoom = link_get_zoom(zoom_levels, df)) {
  # Detect if we're in a reactive context
  if (is.null(shiny::getDefaultReactiveDomain())) {
    stop("The function needs to be used in a reactive context")
  }

  # Update the current tab
  if (!is.null(page)) {
    update_tab(session = r$server_session(), selected = page)
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

    if (!is.null(date)) {
      shiny::updateCheckboxInput(
        session = r$server_session(),
        inputId = sprintf("%s-%s", page, ns_doubled(
          page_id = page,
          element = "cccheckbox_cbx"
        )),
        value = if (length(date) == 2) TRUE else FALSE)
      shiny::updateSliderInput(
        session = r$server_session(),
        inputId = sprintf("%s-%s", page, ns_doubled(
          page_id = page,
          element = if (length(date) == 2) "ccslider_slb" else "ccslider_slu"
        )),
        value = date
      )
    }

    if (!is.null(var_left)) {
      r[[page]]$var_left_force(var_left)
    }

    if (!is.null(var_right)) {
      shinyWidgets::updatePickerInput(
        session = r$server_session(),
        inputId = ns_doubled(
          page_id = page,
          element = "compare-ccpicker_var"
        ),
        selected = var_right
      )
    }
  })

  if (!is.na(select_id)) {
    if (!is.null(df)) {
      if (!is.null(zoom)) r[[page]]$zoom(zoom)

      df_data <- get_from_globalenv(df)
      # Skip the zoom update if the 'centroid' is not in the df
      if (!"centroid" %in% names(df_data)) {
        return(NULL)
      }
      coords <- df_data$centroid[df_data$ID == select_id][[1]]
      coords <- sapply(coords, round, digits = 2)
      cc.map::map_viewstate(
        session = r$server_session(),
        map_ID = ns_doubled(
          page_id = page,
          element = "map"
        ),
        longitude = as.numeric(coords[1]),
        latitude = as.numeric(coords[2]),
        zoom = r[[page]]$zoom()
      )

    }
  }

  # Selection MUST be in the viewstate for the selection to happen. Add a longer
  # delay to make sure the viewstate changed. Then select.
  if (!is.na(select_id)) {
    shinyjs::delay(750, {
      r[[page]]$select_id(select_id)
      cc.map::map_choropleth_update_selection(
        session = r$server_session(),
        map_ID = ns_doubled(
          page_id = page,
          element = "map"
        ),
        select_id = select_id
      )
    })
  }

}
