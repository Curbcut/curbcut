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
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link{vars_build}} function. The class of the vars object is
#' used to determine how to grab de data and output it.
#' @param coords <`reactive numeric vector`> The current central map location
#' of the map. Bookmark can have an input on it. Usually `r[[id]]$coords`.
#' @param data_colours <`reactive data.frame`> The output of
#' \code{\link{data_get_colours}}. Used for the fill color of the
#' polygons.
#' @param outline_width <`reactive numeric`> The width of the polygon outline.
#' Defaults to \code{shiny::reactive(1)}.
#' @param outline_color <`reactive object`> The color of the polygon outline.
#' Defaults to \code{shiny::reactive("transparent")}. It can be a color, or a
#' JSON object representing the line-color mapping.
#' @param pickable <`reactive logical`> Determines if the polygons are
#' selectable. Defaults to \code{shiny::reactive(TRUE)}.
#' @param fill_fun <`reactive function`> The function used for fill colour of
#' polygons. Defaults to \code{shiny::reactive(cc.map::map_choropleth_fill_fun)}.
#' @param fill_fun_args <`reactive list`> Arguments passed to the `fill_fun`
#' function. Defaults to \code{shiny::reactive(list(df = data_colours(),
#' get_col = names(data_colours())[1], fallback = "#B3B3BB"))}.
#'
#' @return A list. The function returns the viewstate of the map, which includes
#' the current position and zoom level.
#'
#' @export
map_js_server <- function(id, r, tile, coords, zoom, vars = r[[id]]$vars,
                          stories = NULL, stories_min_zoom = 13,
                          select_id = shiny::reactive(NA),
                          data_colours = shiny::reactive(data.frame()),
                          outline_width = shiny::reactive(1),
                          outline_color = shiny::reactive("transparent"),
                          pickable = shiny::reactive(TRUE),
                          fill_fun = shiny::reactive(cc.map::map_choropleth_fill_fun),
                          fill_fun_args = shiny::reactive(list(
                            df = data_colours(),
                            get_col = names(data_colours())[1],
                            fallback = "transparent"
                          ))) {
  stopifnot(shiny::is.reactive(tile))
  stopifnot(shiny::is.reactive(data_colours))

  shiny::moduleServer(id, function(input, output, session) {
    inst_prefix <- get_from_globalenv("inst_prefix")

    # Make complete sure the map is loaded: Check every x ms if the map is
    # loaded, and in the case it's not loaded, load it!
    # Initialize reactive variables
    counter <- shiny::reactiveVal(0)
    shouldContinue <- shiny::reactiveVal(TRUE)

    # Follow if map exists
    shiny::observe({
      # Check if observer should continue running
      if (!isTRUE(shiny::isolate(shouldContinue()))) {
        return()
      }

      # Increment counter
      shiny::isolate({
        new_count <- counter() + isTRUE(input$mapboxDivExists)
        counter(new_count)
      })

      # Check if input$mapboxDivExists is TRUE, and invalidate this observer otherwise
      shinyjs::js$checkForMapDiv(id)

      # Stop observer after 10 runs
      if (shiny::isolate(counter()) >= 7) {
        shiny::isolate(shouldContinue(FALSE))
      }

      # Invalidate this observer after 500ms
      shiny::invalidateLater(500, session)
    })
    shiny::observeEvent(input$mapboxDivExists, {
      if (input$mapboxDivExists) r[[id]]$map_exists(input$mapboxDivExists)
    })

    # Form the tileset with stability. Do not get it to trigger the cc.map::map_choropleth
    # if it hasn't changed.
    tileset <- shiny::reactive(sprintf("%s_%s", inst_prefix, tile()))
    tileset_trigger <- shiny::reactiveVal(NULL)
    shiny::observeEvent(tileset(), {
      if (is.null(tileset_trigger())) {
        return(tileset_trigger(tileset()))
      }
      if (tileset() == tileset_trigger()) {
        return()
      }
      tileset_trigger(tileset())
    })

    # Update map coordinates if needed
    shiny::observeEvent(coords(), {
      map_loc <- get_from_globalenv("map_loc")
      if (!identical(map_loc, coords())) {
        cc.map::map_viewstate(
          session = session,
          map_ID = "map",
          longitude = as.numeric(unname(coords()[1])),
          latitude = as.numeric(unname(coords()[2])),
          zoom = zoom()
        )
      }
    })

    # Whenever the tileset changes, load it with the according data_colours.
    shiny::observeEvent(tileset_trigger(),
                        {
                          cc.map::map_choropleth(
                            session = session,
                            map_ID = "map",
                            tileset = tileset_trigger(),
                            fill_colour = data_colours(),
                            select_id = select_id(),
                            fill_fun = fill_fun(),
                            fill_fun_args = fill_fun_args(),
                            pickable = pickable(),
                            outline_width = outline_width(),
                            outline_color = outline_color()
                          )
                        },
                        ignoreNULL = TRUE
    )

    # Only update the fill_colour when data_colours change
    shiny::observe({
      cc.map::map_choropleth_update_fill_colour(
        session = session,
        map_ID = "map",
        fill_colour = data_colours(),
        fill_fun = fill_fun(),
        fill_fun_args = fill_fun_args()
      )
    })

    # Change language for stories hover text
    shiny::observeEvent(r$lang(), {
      cc.map::map_update_lang(session, map_ID = "map", lang = r$lang())
    })

    # Add points for access module
    is_access <- shiny::eventReactive(vars(), {
      var <- vars()$var_left
      if (grepl("^access_", var)) {
        # Remove the mode, send it
        return(gsub(paste0("_foot|_bicycle|_car|_transit_opwe|_transit_pwe|",
                           "_transit_nwd|_transit_nwe|_transit_opwd|_transit_pwd"),
                    "", var))
      } else NULL
    }, ignoreNULL = TRUE)
    shiny::observeEvent(is_access(), {

      colours_dfs <- colours_get()
      colours <- colours_dfs$left_5$fill[1:5]
      colours[1] <- sprintf("%s00", colours[1])
      colours <- sapply(colours, hex_to_rgb_or_rgba, USE.NAMES = FALSE)

      cc.map::map_heatmap(
        session = session,
        map_ID = "map",
        tileset = is_access(),
        pickable = FALSE,
        colours = colours
      )

    }, ignoreNULL = TRUE)

    # Grab the viewstate (lat, lon, zoom)
    viewstate <- curbcut::get_viewstate("map")

    shiny::observeEvent(input$mapboxDivExists, {
      if (isFALSE(input$mapboxDivExists)) {
        # The following follows if a map redraw should be asked for (in cases
        # where the map faults are initiation, or if the tileset_trigger() event
        # happens, at first, before the map is loaded.)
        shinyjs::delay(1500, cc.map::map_choropleth_redraw(session = session, map_ID = "map"))
        shinyjs::delay(3000, cc.map::map_choropleth_redraw(session = session, map_ID = "map"))
        shinyjs::delay(5000, cc.map::map_choropleth_redraw(session = session, map_ID = "map"))
      }
    },
    ignoreNULL = TRUE
    )

    # Return
    return(viewstate)
  })
}


#' @describeIn map_js_server Create the UI for the map module
#' @param stories <`data.frame`> Stories data.frame if they are to be shown
#' on the map. Defaults to NULL for not showing them.
#' @param stories_min_zoom <`numeric`> Zoom level at which stories start to be
#' shown. Defaults to 13.
#' @export
map_js_UI <- function(id) {
  shiny::div(
    class = "map_div", id = shiny::NS(id, "map_div"),
    shiny::uiOutput(outputId = shiny::NS(id, "map_ph"))
  )
}
