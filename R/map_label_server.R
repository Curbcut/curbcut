#' Label and texture server for a Curbcut map
#'
#' This module server shows labels, for a given region on the map and also loads
#' and shows textures for streets, buildings, and parks depending on the zoom
#' level and tile name.
#'
#' @param id <`character`> The ID of the page in which this module will appear,
#' e.g. `canale`.
#' @param tile <`reactive character`> A reactive string with the map tile to
#' be used. Either a combination of a region with auto-scale (e.g. `city_auto_zoom`)
#' or a combination of a region and a scale (e.g. `city_DA`). The output of
#' \code{\link{zoom_server}}.
#' @param zoom <`reactive numeric`> Value representing the current zoom level. Usually
#' is `r[[id]]$zoom()`.
#' @param zoom_levels <`reactive named numeric vector`> A named numeric vector of zoom
#' levels. Usually one of the `map_zoom_levels_x`, or the output of
#' \code{\link{zoom_get_levels}}. It needs to be `numeric` as the function
#' will sort them to make sure the lower zoom level is first, and the highest
#' is last (so it makes sense on an auto-scale).
#' @param region <`reactive character`> representing the current region displayed
#' on the map. Usually one of the output of \code{\link{zoom_get_levels}}.
#' @param show <`reactive logical`> Should there be texture (streets, buildings, ...)?
#' @param mapbox_username <`character`> Mapbox account username. Defaults to
#' grabbing the `mapbox_username` object from the global environment.
#' @param tileset_prefix <`character`> Prefix attached to every tileset. Should
#' correspond to the Curbcut city, e.g. `mtl`. Defaults to grabbing the
#' `tileset_prefix` object from the global environment.
#'
#' @return A module server for the label and texture server.
#' @export
label_server <- function(id, tile, zoom, zoom_levels, region, show = shiny::reactive(TRUE),
                         mapbox_username = get0("mapbox_username", envir = .GlobalEnv),
                         tileset_prefix = get0("tileset_prefix", envir = .GlobalEnv)) {
  stopifnot(shiny::is.reactive(tile))
  stopifnot(shiny::is.reactive(zoom))
  stopifnot(shiny::is.reactive(region))
  stopifnot(shiny::is.reactive(zoom_levels))

  shiny::moduleServer(id, function(input, output, session) {
    # Check for missing arguments
    if (is.null(mapbox_username)) {
      stop(paste0(
        "`mapbox_username` must be present in the global ",
        "environment or supplied to the `label_server` function."
      ))
    }
    if (is.null(tileset_prefix)) {
      stop(paste0(
        "`tileset_prefix` must be present in the global ",
        "environment or supplied to the `label_server` function."
      ))
    }

    # Load texture (streets, buildings, parks (included in street_3), etc.) but
    # invisibly
    street_1 <- tilejson(
      mapbox_username = mapbox_username,
      tileset_prefix = tileset_prefix,
      tile = "street_1"
    )
    street_2 <- tilejson(
      mapbox_username = mapbox_username,
      tileset_prefix = tileset_prefix,
      tile = "street_2"
    )
    street_3 <- tilejson(
      mapbox_username = mapbox_username,
      tileset_prefix = tileset_prefix,
      tile = "street_3"
    )
    building <- shiny::reactive(tilejson(
      mapbox_username = mapbox_username,
      tileset_prefix = tileset_prefix,
      tile = paste0(region(), "_", "building")
    ))

    rdeck::rdeck_proxy("map") |>
      rdeck::add_mvt_layer(
        id = paste0(id, "_street_1"),
        data = street_1,
        visible = FALSE,
        line_width_units = "meters",
        line_width_min_pixels = 2,
        line_joint_rounded = TRUE,
        line_cap_rounded = TRUE,
        get_line_width = 15,
        get_line_color = "#FFFFFFBB",
        get_fill_color = "#A9A9A94D"
      ) |>
      rdeck::add_mvt_layer(
        id = paste0(id, "_street_2"),
        data = street_2,
        visible = FALSE,
        line_width_units = "meters",
        line_width_min_pixels = 1,
        line_joint_rounded = TRUE,
        line_cap_rounded = TRUE,
        get_line_width = 8,
        get_line_color = "#FFFFFFBB",
        get_fill_color = "#A9A9A94D"
      ) |>
      rdeck::add_mvt_layer(
        id = paste0(id, "_street_3"),
        data = street_3,
        visible = FALSE,
        line_width_units = "meters",
        line_width_min_pixels = 0.5,
        line_joint_rounded = TRUE,
        line_cap_rounded = TRUE,
        get_line_width = 4,
        get_line_color = "#FFFFFFBB",
        get_fill_color = "#A9A9A94D"
      )

    # Show labels on top of everything (crated last)
    rdeck::rdeck_proxy("map") |>
      rdeck::add_mvt_layer(
        id = paste0(id, "_label"),
        data = tilejson(
          mapbox_username = mapbox_username,
          tileset_prefix = tileset_prefix,
          tile = paste0(region(), "_label")
        ),
        visible = TRUE,
        point_type = "text",
        get_text = !!as.name("name"),
        text_background = TRUE,
        text_background_padding = rep(2, 4),
        text_font_family = "source-sans-pro-regular",
        text_font_weight = "bold",
        get_text_color = "#000000FF",
        get_text_size = 10,
        get_text_background_color = "#FFFFFF90",
        get_text_border_color = "#00000000",
        get_text_border_width = 0
      )

    # Reload the building layer depending on the region
    shiny::observeEvent(
      building(),
      rdeck::rdeck_proxy("map") |>
        rdeck::add_mvt_layer(
          id = paste0(id, "_building"),
          visible = FALSE,
          data = building(),
          pickable = FALSE,
          auto_highlight = FALSE,
          get_fill_color = "#FFFFFF55",
          extruded = TRUE,
          material = FALSE,
          get_elevation = 5
        )
    )

    # Show streets, parks, buildings, ...
    show_texture <- shiny::reactive(
      map_label_show_texture(
        zoom = zoom(),
        tile = tile(),
        zoom_levels = zoom_levels()
      )
    )

    shiny::observe(
      rdeck::rdeck_proxy("map") |>
        rdeck::update_mvt_layer(
          id = paste0(id, "_building"),
          visible = show_texture() & show()
        )
    )

    shiny::observeEvent(
      show(),
      rdeck::rdeck_proxy("map") |>
        rdeck::update_mvt_layer(
          id = paste0(id, "_street_1"),
          visible = show()
        ) |>
        rdeck::update_mvt_layer(
          id = paste0(id, "_street_2"),
          visible = show()
        ) |>
        rdeck::update_mvt_layer(
          id = paste0(id, "_street_3"),
          visible = show()
        )
    )

    # Add stories bubble with zooms higher than 14
    stories_layer_added <- shiny::reactiveVal(FALSE)
    stories_mapping <- get0("stories_mapping", envir = .GlobalEnv)
    if (is.null(stories_mapping)) {
      return(NULL)
    }

    shiny::observeEvent(
      zoom(),
      {
        if (is.null(stories_mapping)) {
          return(NULL)
        }
        # Check if the layer has been added and if the zoom is higher than 14
        if (!stories_layer_added() && zoom() > 14) {
          # Set the reactive value to TRUE to indicate the layer has been added
          stories_layer_added(TRUE)

          rdeck::rdeck_proxy("map") |>
            rdeck::add_mvt_layer(
              id = "stories",
              data = tilejson(
                mapbox_username = mapbox_username,
                tileset_prefix = tileset_prefix,
                tile = "stories"
              ),
              point_type = "icon",
              get_icon = as.symbol("name"),
              icon_atlas = "stories/image_atlas.png",
              icon_mapping = stories_mapping,
              icon_size_scale = 60,
              pickable = TRUE,
              auto_highlight = TRUE,
              highlight_color = "#FFFFFF50"
            )
        }
      }
    )

    shiny::observeEvent(
      zoom(),
      {
        if (is.null(stories_mapping)) {
          return(NULL)
        }
        rdeck::rdeck_proxy("map") |>
          rdeck::update_mvt_layer(
            id = "stories",
            visible = zoom() > 14 & show()
          )
      }
    )
  })
}
