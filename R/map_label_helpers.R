#' Determine whether to show texture for a given zoom level and tile name
#'
#' This function determines whether to show texture for a given zoom level and
#' tile based on a set of conditions.
#'
#' @param zoom <`numeric`> Value representing the current zoom level. Usually
#' is `r[[id]]$zoom()`.
#' @param tile <`character`> String representing the tile name.
#' @param zoom_levels <`named numeric vector`> A named numeric vector of zoom
#' levels. Usually one of the `map_zoom_levels_x`, or the output of
#' \code{\link{zoom_get_levels}}. It needs to be `numeric` as the function
#' will sort them to make sure the lower zoom level is first, and the highest
#' is last (so it makes sense on an auto-zoom).
#' @param map_module <`logical`> Is the function placed inside the map module
#' to informe extrusion? If so, zoom should have no effect on extrusion.
#'
#' @return A logical value indicating whether or not to show texture for the given
#' zoom level and tile name
map_label_show_texture <- function(zoom, tile, zoom_levels, map_module = FALSE) {
  # In no case we show empty buildings on a building scale
  if (is_scale_df("building", tile)) {
    return(FALSE)
  }

  # In no case we show empty buildings on an auto_zoom after the building threshold
  building_zml <- zoom_levels[names(zoom_levels) == "building"]
  if (is_scale_df("auto_zoom", tile) &
    length(building_zml) > 0 &
    zoom > (building_zml - 0.5)) {
    return(FALSE)
  }

  # In no case we show texture under 11
  if (zoom < 11.5 && !map_module) {
    return(FALSE)
  }

  # We show empty building all the time on a choropleth or autozoom map afterwards
  all_choropleths <- get_from_globalenv("all_choropleths")
  if (is_scale_df(c(all_choropleths, "auto_zoom"), tile)) {
    return(TRUE)
  }

  # If not on the known choropleths, do not show buildings
  return(FALSE)
}
