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
  if (is_scale_df("auto_zoom", tile) &&
    length(building_zml) > 0 &&
    zoom > (building_zml - 0.5)) {
    return(FALSE)
  }

  # In no case we show texture under 11
  if (zoom < 14 && !map_module) {
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

#' Extrude polygons on map
#'
#' This function determines if polygons should be extruded on a map. It
#' considers parameters like zoom level, tile, and a boolean 'extrude'.
#'
#' @param map_view_state <`list`> A list containing the current state of the map view.
#' Usually rdeck::get_view_state("map")
#' @param zoom <`numeric`> Value representing the current zoom level. Usually
#' is `r[[id]]$zoom()`.
#' @param zoom_levels <`named numeric vector`> A named numeric vector of zoom
#' levels. Usually one of the `map_zoom_levels_x`, or the output of
#' \code{\link{zoom_get_levels}}. It needs to be `numeric` as the function
#' will sort them to make sure the lower zoom level is first, and the highest
#' is last (so it makes sense on an auto-zoom).
#' @param tile <`character`> String representing the tile name.
#' @param extrude <`logical`> Indicates whether to extrude or not.
#' If FALSE, the function will immediately return FALSE.
#'
#' @return Returns TRUE if the polygons should be extruded on the map. Otherwise,
#' returns FALSE.
map_label_extrude <- function(map_view_state, zoom, zoom_levels, tile, extrude) {
  if (is.null(map_view_state)) return(FALSE)
  if (map_view_state$pitch < 25) return(FALSE)
  if (!extrude) return(FALSE)

  # Return TRUE or FALSE depending if the textures are present or not
  !map_label_show_texture(
    zoom = zoom,
    zoom_levels = zoom_levels,
    tile = tile,
    map_module = TRUE
  )
}
