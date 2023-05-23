#' Creates a Map Server Shiny module using Rdeck library
#'
#' This function is a Shiny module that generates an interactive map using the
#' `rdeck` package. The map displays data in the form of MVT (Mapbox Vector Tile)
#' layers with customizable colors and line widths. Arguments control the shown
#' MVT and the aesthetic of the map. The function also requires several global
#' environment variables to be set in order to work properly.
#'
#' @param id <`character`> The ID of the page in which this module will appear,
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
#' @param zoom <`reactive numeric`> The current zoom level of the map, also
#' the zoom at which the map will initiate. Usually `r[[id]]$zoom`
#' @param coords <`reactive numeric vector`> The current central map location
#' of the map. Bookmark can have an input on it. Usually `r[[id]]$coords`.
#' @param fill_fun <`reactive function`> A function used to calculate the fill color of
#' the polygons. It needs to be created using \code{\link[rdeck]{scale_color_category}}.
#' Defaults to \code{\link{map_scale_fill}}.
#' @param tileset_ID_color <`reactive character`> Which is the column of the mapbox tileset
#' that should be caught to display colours. Defaults to `ID_color`. All scales
#' have their ID duplicated at the ID_color, except buildings, streets, and other
#' large scales. Their ID_color is then the ID of the DA in which they fall. This
#' can be switched to `ID` if buildings or streets have their own data (and they
#' don't just display the value of their DA).
#' @param fill_args <`reactive list`> List of arguments to be passed to the
#' `fill_fun` argument.
#' @param colour_fun <`reactive function`> A function used to calculate the line color of
#' the polygons. \code{\link{map_scale_colour}}.
#' @param colour_args  <`reactive list`> List of arguments to be passed to the
#' `colour_fun` argument.
#' @param lwd_fun <`reactive function`> A function used to calculate the border width of
#' the polygons. It needs to be created using \code{\link[rdeck]{scale_category}}.
#' \code{\link{map_scale_lwd}}.
#' @param lwd_args  <`reactive list`> List of arguments to be passed to the
#' `lwd_fun` argument.
#' @param auto_highlight <`reactive logical`> When TRUE, the current object hovered by
#' the cursor is highlighted.
#' @param pickable <`reactive logical`> Determines if the layer responds to pointer /
#' touch events.
#' @param extrude <`reactive logical`> Whether to use \code{\link{map_label_show_texture}}
#' to determine if polygons should be extrude.
#' @param mapbox_username <`character`> Mapbox account username. Defaults to
#' grabbing the `mapbox_username` object from the global environment.
#' @param tileset_prefix <`character`> Prefix attached to every tileset. Should
#' correspond to the Curbcut city, e.g. `mtl`. Defaults to grabbing the
#' `tileset_prefix` object from the global environment.
#' @param map_base_style <`character`> The mapbox basemap style url.
#' See https://docs.mapbox.com/api/maps/#mapbox-styles
#'
#' @return The server function returns the map viewstate coming from
#' \code{\link[rdeck]{get_view_state}}.
#' @export
map_server <- function(id, tile, data_colours, select_id, zoom_levels, zoom,
                       coords,
                       fill_fun = shiny::reactive(map_scale_fill),
                       tileset_ID_color = shiny::reactive("ID_color"),
                       fill_args = shiny::reactive(list(data_colours(),
                                                        tileset_ID_color = tileset_ID_color())),
                       colour_fun = shiny::reactive(map_scale_colour),
                       colour_args = shiny::reactive(list(NULL)),
                       lwd_fun = shiny::reactive(map_scale_lwd),
                       lwd_args = shiny::reactive(list(
                         select_id = select_id(), tile = tile(), zoom = zoom(),
                         zoom_levels = zoom_levels(), lwd = 1
                       )),
                       auto_highlight = shiny::reactive(TRUE),
                       pickable = shiny::reactive(TRUE),
                       extrude = shiny::reactive(TRUE),
                       mapbox_username = get_from_globalenv("mapbox_username"),
                       tileset_prefix = get_from_globalenv("tileset_prefix"),
                       map_base_style = get_from_globalenv("map_base_style")) {

  stopifnot(shiny::is.reactive(tile))
  stopifnot(shiny::is.reactive(data_colours))
  stopifnot(shiny::is.reactive(select_id))
  stopifnot(shiny::is.reactive(zoom_levels))
  stopifnot(shiny::is.reactive(fill_args))
  stopifnot(shiny::is.reactive(colour_args))
  stopifnot(shiny::is.reactive(lwd_args))
  stopifnot(shiny::is.reactive(coords))
  stopifnot(shiny::is.reactive(auto_highlight))
  stopifnot(shiny::is.reactive(pickable))
  stopifnot(shiny::is.reactive(extrude))

  shiny::moduleServer(id, function(input, output, session) {
    # Map
    output$map <- rdeck::renderRdeck({
      rdeck::rdeck(
        map_style = map_base_style, layer_selector = FALSE,
        initial_view_state = rdeck::view_state(
          center = shiny::isolate(as.numeric(coords())),
          zoom = shiny::isolate(zoom())
        )
      ) |> rdeck::add_mvt_layer(id = id)
    })

    # Grab the tile json and if fail, return NULL so that the app doesn't crash.
    map_tilejson <- shiny::reactive(tilejson(
      mapbox_username = mapbox_username,
      tileset_prefix = tileset_prefix,
      tile = tile()
    ))

    # Update data layer source on tile() change only.
    shiny::observeEvent(map_tilejson(), {
      rdeck::rdeck_proxy("map") |>
        rdeck::update_mvt_layer(
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
          pickable = pickable(),
          auto_highlight = auto_highlight(),
          highlight_color = "#FFFFFF50",
          get_fill_color = do.call(fill_fun(), fill_args()),
          get_line_color = do.call(colour_fun(), colour_args()),
          get_line_width = do.call(lwd_fun(), lwd_args()),
          line_width_units = "pixels",
          material = FALSE,
          get_elevation = 5
        )
    )

    # Show the buildings extrude at the same moment texture is off.
    # A change in the extrude reactive only triggers the `extrude` change.
    # Attempt to improve user experience between auto-zoom DA and building level.
    extrude_final <- shiny::reactive({
      {!map_label_show_texture(
        zoom = zoom(),
        zoom_levels = zoom_levels(),
        tile = tile(),
        map_module = TRUE
      )} & extrude()
    })

    shiny::observeEvent(
      extrude_final(),
      rdeck::rdeck_proxy("map") |>
        rdeck::update_mvt_layer(
          id = id,
          extruded = extrude_final()
        )
    )

    # Return the viewstate
    return(shiny::reactive(rdeck::get_view_state("map")))
  })
}

#' @describeIn map_server Create the UI for the map module
#' @export
map_UI <- function(id) {
  shiny::div(
    class = "map_div", id = shiny::NS(id, "map_div"),
    rdeck::rdeckOutput(shiny::NS(id, "map"), height = "100%")
  )
}
