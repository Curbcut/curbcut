#' Map scale fill
#'
#' This function creates a color scale for the fill colours of the maps. It
#' takes the output of \code{\link{data_get_colours}} and feeds it
#' to the \code{\link[rdeck]{scale_color_category}} function.
#'
#' @param data_colours <`data.frame`> The output of \code{\link{data_get_colours}}.
#' Two columns: `ID` and `fill`. Every `ID` will be coloured using its according
#' `fill` colour.
#' @param tileset_ID_color <`character`> The name of the column on the tileset
#' used to colour every polygon. Defaults to `ID_color`, where the buildings
#' scales have in their `ID_color` column the `ID` of their DA.
#'
#' @return A color scale for polygons used in an rdeck map.
#' @export
map_scale_fill <- function(data_colours, tileset_ID_color = "ID_color") {
  rdeck::scale_color_category(
    col = !!rlang::sym(tileset_ID_color),
    palette = data_colours$fill,
    unmapped_color = "#B3B3BB",
    levels = data_colours$ID,
    legend = FALSE
  )
}

#' Map scale colour
#'
#' @param ... Unused arguments.
#'
#' @return Returns the hex of the white colour.
#' @export
map_scale_colour <- function(...) {
  "#FFFFFF"
}

#' Map scale line width
#'
#' This function creates a line width scale for polygons in an rdeck map based
#' on the zoom level of the map. If `tile`, `zoom` and `zoom_levels` are given,
#' the borders will disappear at one zoom digit under the zoom at which they
#' would have appeared on an auto-zoom.
#'
#' @param select_id <`character`> A string specifying the ID of the selected polygon.
#' The selected polygon will have an increased in the width of its border.
#' @param tile <`character`> The name of the tileset currently present on the map.
#' @param zoom <`numeric`> The current zoom level of the map.
#' @param zoom_levels <`named vector`> Zoom levels under study. One of the
#' map_zoom_levels_x in the global environment. It contains the zoom at which
#' a scale should switch on an autozoom, e.g. `c(CMA = 0, CT = 10.5, DA = 12.5, ...)`.
#'
#' @return A line width scale for polygons in an rdeck map.
#' @export
map_scale_lwd <- function(select_id, tile = NULL, zoom = NULL,
                          zoom_levels = NULL) {
  lwd <- 1

  if (all(!sapply(list(tile, zoom, zoom_levels), is.null))) {
    # If we are one zoom below the threshold at which the scale would have
    # appeared on an auto-zoom, do not display any borders around the polygons
    normal_zoom <- zoom_levels[sapply(names(zoom_levels), is_scale_df, tile)]
    # If tile wasn't found in the zoom_levels, return 1. On auto-zoom, there
    # will always be borders around the polygons.
    lwd <-
      if (length(normal_zoom) == 0) 1 else as.numeric(zoom > (normal_zoom - 1))
  }

  # Return the categorical scale
  rdeck::scale_category(
    col = !!rlang::sym("ID"),
    range = c(5, lwd),
    unmapped_value = lwd,
    levels = c(select_id, "NA"),
    legend = FALSE
  )
}
