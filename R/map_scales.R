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
    col = !!as.name(tileset_ID_color),
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
  "#63666A"
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
#' @param zoom_levels <`named numeric vector`> A named numeric vector of zoom
#' levels. Usually one of the `map_zoom_levels_x`, or the output of
#' \code{\link{zoom_get_levels}}. It needs to be `numeric` as the function
#' will sort them to make sure the lower zoom level is first, and the highest
#' is last (so it makes sense on an auto-zoom).
#' @param lwd <`numeric`> Value specifying the line width of the polygon borders,
#' with a default value of 1. This value controls the thickness of the borders
#' around the polygons on the map. A larger value results in thicker borders,
#' while a smaller value results in thinner borders.
#'
#' @return A line width scale for polygons in an rdeck map.
#' @export
map_scale_lwd <- function(select_id, tile = NULL, zoom = NULL,
                          zoom_levels = NULL, lwd = 1) {

  if (all(!sapply(list(tile, zoom, zoom_levels), is.null))) {
    # If we are one zoom below the threshold at which the scale would have
    # appeared on an auto-zoom, do not display any borders around the polygons
    normal_zoom <- zoom_levels[sapply(names(zoom_levels), is_scale_df, tile)]
    # If tile wasn't found in the zoom_levels, return 1. On auto-zoom, there
    # will always be borders around the polygons.
    lwd <-
      if (length(normal_zoom) == 0) {
        lwd
      } else {
        if (zoom > (normal_zoom - 2)) lwd else 0
      }
  }

  # Return the categorical scale
  rdeck::scale_category(
    col = !!as.name("ID"),
    range = c(10, lwd),
    unmapped_value = lwd,
    levels = c(select_id, "NA"),
    legend = FALSE
  )
}

#' Update a reactive value object
#'
#' This function updates a reactive value object and does not trigger a change
#' in the reactive value if the new value is the same as the previous value. It
#' observes a new value, and if this new value is different from the current one,
#' the function updates the rv. It uses the reactive programming features of Shiny.
#'
#' @param id <`character`> The ID of the page in which this module will appear,
#' e.g. `canale`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param rv_name <`character`> A character string specifying the name of the
#' reactive value to create and update.
#' @param default_val <`vector`> A vector of length one. This parameter is used
#' to initialize the rv.
#' @param new_val <`reactive`> A reactive expression returning the new value to
#' assign to the rv. If the new value is the same as the current one, the function
#' does nothing.
#'
#' @return
#' This function does not return a value. It updates the reactive value in place.
#' The updated value can be accessed using the rv object and the rv_name parameter.
#' If the new value is the same as the current one, the function does nothing and
#' returns NULL.
update_map_rv <- function(id, r, rv_name, default_val, new_val) {
  r[[id]][[rv_name]] <- shiny::reactiveVal(default_val)

  shiny::observe({
    if (identical(new_val(), shiny::isolate(r[[id]][[rv_name]]()))) {
      return(NULL)
    }
    r[[id]][[rv_name]](new_val())
  })
}
