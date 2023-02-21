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
poi_get <- function(id, map_id = paste0(id, "-map"), poi) {

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
