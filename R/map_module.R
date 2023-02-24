#' Creates a Map Server Shiny module using Rdeck library
#'
#' This function is a Shiny module that generates an interactive map using the
#' `rdeck` package. The map displays data in the form of MVT (Mapbox Vector Tile)
#' layers with customizable colors and line widths. Arguments control the shown
#' MVT and the aesthetic of the map. The function also requires several global
#' environment variables to be set in order to work properly.
#'
#' @param id <`character`> The ID of the page in which the legend will appear,
#' e.g. `canale`.
#' @param tile <`reactive character`> A reactive string with the map tile to
#' be used. Either a combination of a region with auto-zoom (e.g. `city_auto_zoom`)
#' or a combination of a region and a scale (e.g. `city_DA`). The output of
#' \code{\link{zoom_server}}.
#' @param data_colours <`reactive data.frame`> The output of
#' \code{\link{data_get_colours}}. Used for the fill colour of the
#' polygons.
#' @param select_id <`reactive character`> The ID of the selected polygon.
#' @param zoom_levels <`named numeric vector`> A named numeric vector of zoom
#' levels. Usually one of the `map_zoom_levels_x`, or the output of
#' \code{\link{zoom_get_levels}}. It needs to be `numeric` as the function
#' will sort them to make sure the lower zoom level is first, and the highest
#' is last (so it makes sense on an auto-zoom).
#' @param zoom <`reactive numeric`> The current zoom level of the map.
#' @param fill_fun <`function`> A function used to calculate the fill color of
#' the polygons. It needs to be created using \code{\link[rdeck]{scale_color_category}}.
#' Defaults to \code{\link{map_scale_fill}}.
#' @param fill_args <`reactive list`> List of arguments to be passed to the
#' `fill_fun` argument.
#' @param colour_fun <`function`> A function used to calculate the line color of
#' the polygons. \code{\link{map_scale_colour}}.
#' @param colour_args  <`reactive list`> List of arguments to be passed to the
#' `colour_fun` argument.
#' @param lwd_fun <`function`> A function used to calculate the border width of
#' the polygons. It needs to be created using \code{\link[rdeck]{scale_category}}.
#' \code{\link{map_scale_lwd}}.
#' @param lwd_args  <`reactive list`> List of arguments to be passed to the
#' `lwd_fun` argument.
#' @param auto_highlight <`logical`> When TRUE, the current object hovered by
#' the cursor is highlighted.
#' @param pickable <`logical`> Determines if the layer responds to pointer /
#' touch events.
#' @param mapbox_username <`character`> Mapbox account username. Defaults to
#' grabbing the `mapbox_username` object from the global environment.
#' @param tileset_prefix <`character`> Prefix attached to every tileset. Should
#' correspond to the Curbcut city, e.g. `mtl`. Defaults to grabbing the
#' `tileset_prefix` object from the global environment.
#' @param map_base_style <`character`> The mapbox basemap style url.
#' See https://docs.mapbox.com/api/maps/#mapbox-styles
#' @param map_loc <`numeric`> A numeric vector of length two with lat/lon.
#' It will be the center of the viewport on map. Defaults to grabbing the
#' `map_loc` object from the global environment.
#'
#' @return The map Shiny UI and server module functions
#' @export
map_server <- function(id, tile, data_colours, select_id, zoom_levels, zoom,
                       fill_fun = map_scale_fill,
                       fill_args = shiny::reactive(list(data_colours())),
                       colour_fun = map_scale_colour,
                       colour_args = shiny::reactive(list(NULL)),
                       lwd_fun = map_scale_lwd,
                       lwd_args = shiny::reactive(list(
                         select_id(), tile(), zoom(),
                         zoom_levels()
                       )),
                       auto_highlight = TRUE,
                       pickable = TRUE,
                       mapbox_username = get0("mapbox_username", envir = .GlobalEnv),
                       tileset_prefix = get0("tileset_prefix", envir = .GlobalEnv),
                       map_base_style = get0("map_base_style", envir = .GlobalEnv),
                       map_loc = get0("map_loc", envir = .GlobalEnv)) {
  stopifnot(shiny::is.reactive(tile))
  stopifnot(shiny::is.reactive(data_colours))
  stopifnot(shiny::is.reactive(select_id))
  stopifnot(shiny::is.reactive(zoom_levels))
  stopifnot(shiny::is.reactive(fill_args))
  stopifnot(shiny::is.reactive(colour_args))
  stopifnot(shiny::is.reactive(lwd_args))

  shiny::moduleServer(id, function(input, output, session) {
    # Check for missing arguments
    if (is.null(mapbox_username)) {
      stop(paste0(
        "`mapbox_username` must be present in the global ",
        "environment or supplied to the `map_server` function."
      ))
    }
    if (is.null(tileset_prefix)) {
      stop(paste0(
        "`tileset_prefix` must be present in the global ",
        "environment or supplied to the `map_server` function."
      ))
    }
    if (is.null(map_base_style)) {
      stop(paste0(
        "`map_base_style` must be present in the global ",
        "environment or supplied to the `map_server` function."
      ))
    }
    if (is.null(map_loc)) {
      stop(paste0(
        "`map_loc` must be present in the global ",
        "environment or supplied to the `map_server` function."
      ))
    }

    # Map
    output$map <- rdeck::renderRdeck({
      rdeck::rdeck(
        map_style = map_base_style, layer_selector = FALSE,
        initial_view_state = rdeck::view_state(
          center = map_loc, zoom = shiny::isolate(zoom())
        )
      )
    })

    # Helper variables
    extrude <- shiny::reactive((grepl("auto_zoom$", tile()) && zoom() >= 15.5) |
      grepl("building", tile()))

    # Grab the tile json and if fail, return NULL so that the app doesn't crash.
    map_tilejson <- shiny::reactive(tilejson(
      mapbox_username = mapbox_username,
      tileset_prefix = tileset_prefix,
      tile = tile()
    ))

    # Update data layer source on tile() change only. Make sure once the tile
    # gets updated that all the argument follow to.
    shiny::observeEvent(map_tilejson(), {
      rdeck::rdeck_proxy("map") |>
        rdeck::add_mvt_layer(
          id = id,
          data = map_tilejson()
        )
    })

    # Update layer aesthetics on change of any aesthetic
    ## TKTK find ways so that ALL reactives doesn't necessarily trigger this.
    ## Should every change in zoom trigger this? Or only the threshold where
    ## the zoom actually has an impact on the aesthetics.
    shiny::observe(
      rdeck::rdeck_proxy("map") |>
        rdeck::update_mvt_layer(
          id = id,
          pickable = pickable,
          auto_highlight = auto_highlight,
          highlight_color = "#FFFFFF50",
          get_fill_color = do.call(fill_fun, fill_args()),
          get_line_color = do.call(colour_fun, colour_args()),
          get_line_width = do.call(lwd_fun, lwd_args()),
          line_width_units = "pixels",
          extruded = extrude(),
          material = FALSE,
          get_elevation = 5
        )
    )
  })
}

#' @describeIn map_server Create the UI for the map module
#' @export
map_UI <- function(id) {
  shiny::div(
    class = "map_div",
    rdeck::rdeckOutput(shiny::NS(id, "map"), height = "100%")
  )
}
