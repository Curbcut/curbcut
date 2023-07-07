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
#' @param data_colours <`reactive data.frame`> The output of
#' \code{\link{data_get_colours}}. Used for the fill colour of the
#' polygons.
#'
#' @return A list. The function returns the viewstate of the map, which includes
#' the current position and zoom level.
#'
#' @export
map_js_server <- function(id, r, tile, data_colours) {
  stopifnot(shiny::is.reactive(tile))
  stopifnot(shiny::is.reactive(data_colours))

  shiny::moduleServer(id, function(input, output, session) {

    # Form the tileset
    tileset <- shiny::reactive(sprintf("mtl_%s", tile()))

    # Whenever the tileset changes, load it with the according data_colours.
    shiny::observeEvent(tileset(), {
      cc.map::update_map(session = session,
                         map_ID = "map",
                         configuration = list(tileset = tileset(),
                                              fill_colour = data_colours()))
    })

    # Only update the fill_colour when data_colours change
    shiny::observeEvent(data_colours(), {
      cc.map::update_map(session = session,
                         map_ID = "map",
                         configuration = list(fill_colour = data_colours()))
    }, ignoreInit = TRUE)

    # Grab the viewstate (lat, lon, zoom)
    viewstate <- curbcut::get_viewstate("map")

    # Return
    return(viewstate)
  })
}

#' @describeIn map_js_server Create the UI for the map module
#' @export
map_js_UI <- function(id) {
  map_zoom <- get_from_globalenv("map_zoom")
  map_loc <- get_from_globalenv("map_loc")
  tileset_prefix <- get_from_globalenv("tileset_prefix")

  shiny::div(
    class = "map_div", id = shiny::NS(id, "map_div"),
    cc.map::map_input(
      map_ID = shiny::NS(id, "map"),
      username = "curbcut",
      token = 'pk.eyJ1IjoiY3VyYmN1dCIsImEiOiJjbGprYnVwOTQwaDAzM2xwaWdjbTB6bzdlIn0.Ks1cOI6v2i8jiIjk38s_kg',
      longitude = map_loc[1],
      latitude = map_loc[2],
      zoom = map_zoom,
      tileset_prefix = tileset_prefix)
  )
}
