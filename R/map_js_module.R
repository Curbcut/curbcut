#' Shiny Server function to update the interactive map of `cc.map`
#'
#' This function creates an interactive map using the cc.map library. It supports
#' map tile updates and changes in the fill colour of map elements, as defined by
#' the 'tile' and 'data_colours' reactive values respectively.
#'
#' @param id <`character`> The ID of the page in which this module will appear,
#' e.g. `alp`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param tile <`reactive character`> A reactive string with the map tile to
#' be used. Either a combination of a region with auto-scale (e.g. `city_auto_zoom`)
#' or a combination of a region and a scale (e.g. `city_DA`). The output of
#' \code{\link{zoom_server}}.
#' @param select_id <`reactive character`> The ID of the selected polygon.
#' @param zoom <`reactive numeric`> The current zoom level of the map, also
#' the zoom at which the map will initiate. Usually `r[[id]]$zoom`
#' @param coords <`reactive numeric vector`> The current central map location
#' of the map. Bookmark can have an input on it. Usually `r[[id]]$coords`.
#' @param data_colours <`reactive data.frame`> The output of
#' \code{\link{data_get_colours}}. Used for the fill colour of the
#' polygons.
#'
#' @return A list. The function returns the viewstate of the map, which includes
#' the current position and zoom level.
#'
#' @export
map_js_server <- function(id, r, tile, coords, zoom, select_id, data_colours) {
  stopifnot(shiny::is.reactive(tile))
  stopifnot(shiny::is.reactive(data_colours))

  shiny::moduleServer(id, function(input, output, session) {

    tileset_prefix <- get_from_globalenv("tileset_prefix")

    # Form the tileset
    tileset <- shiny::reactive(sprintf("%s_%s", tileset_prefix, tile()))

    # Update map coordinates if needed
    shiny::observeEvent(coords(), {
      cc.map::map_viewstate(
        session = session,
        map_ID = "map",
        longitude = as.numeric(unname(coords()[1])),
        latitude = as.numeric(unname(coords()[2])),
        zoom = zoom())
    })

    # Whenever the tileset changes, load it with the according data_colours.
    shiny::observeEvent(tileset(), {
      shinyjs::delay(250,
                     cc.map::map_choropleth(
                       session = session,
                       map_ID = "map",
                       tileset = tileset(),
                       fill_colour = data_colours(),
                       select_id = select_id()
                     ))
    })

    # Only update the fill_colour when data_colours change
    shiny::observeEvent(data_colours(), {
      cc.map::map_choropleth_update_fill_colour(
        session = session,
        map_ID = "map",
        fill_colour = data_colours())
    },
    ignoreInit = TRUE)

    # Grab the viewstate (lat, lon, zoom)
    viewstate <- curbcut::get_viewstate("map")

    # Return
    return(viewstate)
  })
}


#' @describeIn map_js_server Create the UI for the map module
#' @param stories <`data.frame`> Stories data.frame if they are to be shown
#' on the map. Defaults to NULL for not showing them.
#' @export
map_js_UI <- function(id, stories = NULL) {
  map_zoom <- get_from_globalenv("map_zoom")
  map_loc <- get_from_globalenv("map_loc")
  tileset_prefix <- get_from_globalenv("tileset_prefix")
  map_token <- get_from_globalenv("map_token")
  map_base_style <- get_from_globalenv("map_base_style")
  mapbox_username <- get_from_globalenv("mapbox_username")

  shiny::div(
    class = "map_div", id = shiny::NS(id, "map_div"),
    cc.map::map_input(
      map_ID = shiny::NS(id, "map"),
      username = mapbox_username,
      token = map_token,
      longitude = map_loc[1],
      latitude = map_loc[2],
      zoom = map_zoom,
      map_style_id = map_base_style,
      tileset_prefix = tileset_prefix,
      stories = stories
    )
  )
}
