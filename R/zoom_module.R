#' zoom_server Function
#'
#' This function creates a module for Curbcut that allows users to control
#' the zoom level of a map using a slider input. The function disables the slider
#' if the user selects auto-zoom. The slider is updated if the available
#' zoom levels change. If on auto-zoom, the slider gets updated when the zoom
#' levels threshold are attained. When there is a change in the `r$region()`,
#' the auto-zoom gets activated to provide better transition between regions.
#' The function then creates a reactive tile that returns the appropriate tile
#' based on the user's selection. If auto-zoom is selected, the function returns
#' the auto-zoom tile. If a specific scale is selected, the function checks to
#' see if the scale is available for the current region. If it is not
#' available, the function defaults back to auto-zoom.
#'
#' @param id <`character`> The ID of the page in which the legend will appear,
#' e.g. `canale`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file.
#' @param zoom_string <`reactive character`> A reactive object representing the
#' current zoom level, e.g. `CSD`.
#' @param zoom_levels <`named numeric vector`> A named numeric vector of zoom
#' levels. Usually one of the `map_zoom_levels_x`, or the output of
#' \code{\link{zoom_get_levels}}. It needs to be `numeric` as the function
#' will sort them to make sure the lower zoom level is first, and the highest
#' is last (so it makes sense on an auto-zoom).
#'
#' @return A reactive object representing the current tile that should be
#' displayed on the map.
#' @export
zoom_server <- function(id, r = r, zoom_string, zoom_levels) {
  stopifnot(shiny::is.reactive(zoom_string))
  stopifnot(shiny::is.reactive(zoom_levels))

  shiny::moduleServer(id, function(input, output, session) {
    # Disable the slider if in auto mode
    shiny::observe({
      shinyjs::toggleState(id = "zoom_slider", condition = !input$zoom_auto)
    })

    # Update the slider if zoom_levels() changes
    shiny::observe({
      shinyWidgets::updateSliderTextInput(
        session = session,
        inputId = "zoom_slider",
        selected = zoom_get_name(shiny::isolate(zoom_string()), lang = r$lang()),
        choices = zoom_get_label(zoom_levels()$zoom_levels, lang = r$lang())
      )
    })

    # Update the slider when zoom changes, only on auto_zoom
    shiny::observe({
      if (input$zoom_auto) {
        shinyWidgets::updateSliderTextInput(
          session = session,
          inputId = "zoom_slider",
          selected = zoom_get_name(zoom_string(), lang = r$lang())
        )
      }
    })

    # A change in region should switch it back to auto-zoom to get a better
    # transition
    shiny::observeEvent(zoom_levels()$region, {
      shiny::updateCheckboxInput(
        session = session,
        inputId = "zoom_auto",
        value = TRUE
      )
    })

    # Return the tile() reactive, indicating if the map should show an
    # auto-zoom or a scale (e.g. `CMA_auto_zoom` vs `CMA_DA`)
    tile <- shiny::reactive({
      # On auto-zoom, return the auto_zoom
      if (input$zoom_auto) {
        return(paste(zoom_levels()$region, "auto_zoom", sep = "_"))
      }

      scale <- zoom_get_code(input$zoom_slider, lang = r$lang())

      # A change in region should switch the tile back to auto-zoom to deal with
      # the fact that some scales are not available in some regions
      get_mzl <- paste0("map_zoom_levels_", zoom_levels()$region)
      mzl <- tryCatch(get_from_globalenv(get_mzl),
        error = function(e) c(missing = "missing")
      )
      if (!scale %in% names(mzl)) scale <- "auto_zoom"

      return(paste(zoom_levels()$region, scale, sep = "_"))
    })

    # Return the tile
    return(tile)
  })
}


#' @describeIn zoom_server Create the UI for the zoom module
#' @export
zoom_UI <- function(id, zoom_levels) {
  shiny::tagList(
    shiny::div(class = "sus-sidebar-control", shiny::checkboxInput(
      inputId = shiny::NS(id, "zoom_auto"),
      label = "Auto-zoom",
      value = TRUE
    )),
    shiny::div(class = "sus-sidebar-control", shinyWidgets::sliderTextInput(
      inputId = shiny::NS(id, "zoom_slider"),
      label = NULL,
      choices = zoom_get_label(zoom_levels),
      hide_min_max = TRUE,
      force_edges = TRUE
    ))
  )
}
