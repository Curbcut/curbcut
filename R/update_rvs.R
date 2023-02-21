#' Update the saved zoom string for a given region based on a current zoom level
#'
#' This function takes in a previously saved zoom string and updates it based on
#' the current zoom level for a given region. If the new zoom string is
#' different from the previously saved one, it returns the new zoom string.
#' Otherwise, it returns the old one.
#'
#' @param rv_zoom_string <`character`> A character string representing a previously
#' saved zoom level for a given region
#' @param zoom <`numeric`> A numeric value representing the current zoom level
#' @param zoom_levels <`named numeric vector`> A named numeric vector of zoom
#' levels. Usually one of the `map_zoom_levels_x`, or the output of
#' \code{\link[curbcut]{zoom_get_levels}}. It needs to be `numeric` as the function
#' will sort them to make sure the lower zoom level is first, and the highest
#' is last (so it makes sense on an auto-zoom).
#' @param region <`character`> The region to retrieve the zoom levels for,
#' usually `r$region()`.
#'
#' @return A character string representing the updated zoom level for the given region
update_zoom_string <- function(rv_zoom_string, zoom, zoom_levels, region) {

  # Get the zoom string that would fit in the zoom level
  new <- zoom_get_string(zoom = zoom, region = region,
                         zoom_levels = zoom_levels)

  # If the new zoom string is different, return it.
  if (new != rv_zoom_string) return(new)

  # If the same, return the old
  return(rv_zoom_string)
}

#' Get nearby points of interests (POIs) based on map view state
#'
#' This function retrieves the nearby POIs based on the current view state of a
#' map specified by the \code{map_id} parameter. The function filters the POIs
#' based on their proximity to the center of the map, keeping only those POIs
#' that are within 2000 meters. Currently only returns the points of interests
#' of the `stories` data.frame.
#'
#' @param id <`character`> The ID of the page in which the legend will appear,
#' e.g. `canale`.
#' @param map_id <`character`> The ID of the map. By default, it is
#' set to the value of \code{paste0(id, "-map")} as the \code{\link[curbcut]{map_server}}
#' module assigns the `map` id to the maps.
#' @param poi <`character vector`> The current POIs showing on the map.
#'
#' @return A character vector of nearby POIs if new POIs are found; otherwise,
#' returns the input POI vector.
#' @export
update_poi <- function(id, map_id = paste0(id, "-map"), poi) {

  # Get the map view state
  map_input <- rdeck::get_view_state(map_id)

  # Initialize objects
  out <- NULL
  zoom <- map_input$zoom
  lat <- map_input$latitude
  lon <- map_input$longitude

  # Exit early if the map isn't sufficiently zoomed in
  if (zoom < 13) return(NULL)

  # Get POIs; currently just Stories. Return nothing if the `stories` df is
  # missing.
  stories <- get0("stories", envir = .GlobalEnv)
  if (is.null(stories)) return(NULL)

  points <- stories[c("name_id", "lon", "lat")]

  # Find distance from map centre to the POIs
  dist <- get_dist(points[c("lon", "lat")], c(lon, lat))

  # If any POI is within 2000 m of centre, filter it. If not, return NULL
  new_pois <- points$name_id[dist < 2000]
  if (length(new_pois) == 0) return(NULL)

  # If the new pois are the same as the ones currently showing, do not update
  # them.
  if (length(poi) == length(new_pois)) {
    if (all(new_pois == poi)) {
      return(poi)
    }
  }

  # If they are different, return the new points of interests
  return(new_pois)
}
