#' zoom_server Function
#'
#' This function creates a module for Curbcut that allows users to control
#' the zoom level of a map using a slider input. The function disables the slider
#' if the user selects auto-scale. The slider is updated if the available
#' zoom levels change. If on auto-scale, the slider gets updated when the zoom
#' levels threshold are attained. When there is a change in the `r$region()`,
#' the auto-scale gets activated to provide better transition between regions.
#' The function then creates a reactive tile that returns the appropriate tile
#' based on the user's selection. If auto-scale is selected, the function returns
#' the auto-scale tile. If a specific scale is selected, the function checks to
#' see if the scale is available for the current region. If it is not
#' available, the function defaults back to auto-scale.
#'
#' @param id <`character`> The ID of the page in which this module will appear,
#' e.g. `alp`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param zoom_string <`reactive character`> A reactive object representing the
#' current zoom level, e.g. `CSD`.
#' @param suffix_zoom_levels <`character`> Suffix that should be appended at the
#' end of an auto-scale. If the maximum zoom possible is CT and we previously created
#' such a tileset, we could have the suffix_zoom_levels `max_CT`, which would
#' be appended at the end of the auto-scale tile name. Defaults to NA for none.
#' @param zoom_levels <`named numeric vector`> A named numeric vector of zoom
#' levels. Usually one of the `map_zoom_levels_x`, or the output of
#' \code{\link{zoom_get_levels}}. It needs to be `numeric` as the function
#' will sort them to make sure the lower zoom level is first, and the highest
#' is last (so it makes sense on an auto-scale).
#' @param no_autozoom <`reactive logical`> Whether the output tile() reactive
#' should include autozooms or not.
#'
#' @return A reactive object representing the current tile that should be
#' displayed on the map.
#' @export
zoom_server <- function(id, r = r, zoom_string, zoom_levels,
                        suffix_zoom_levels = NA,
                        no_autozoom = shiny::reactive(FALSE)) {
  stopifnot(shiny::is.reactive(zoom_string))
  stopifnot(shiny::is.reactive(zoom_levels))

  shiny::moduleServer(id, function(input, output, session) {
    # Get the auto zoom checkbox server and add that a change in region means
    # switching back the auto-scale to get a better transition
    zoom_auto <- checkbox_server(
      id = "zoom_auto",
      r = r,
      label = shiny::reactive("Auto-scale"),
      event_reset = shiny::reactive(zoom_levels()$region)
    )

    # Disable the slider if in auto mode
    shiny::observe({
      shinyjs::toggleState(
        id = "zoom_slider-ccslidertext_slt",
        condition = !zoom_auto()
      )
    })

    # Update the slider if zoom_levels() changes
    choices <- shiny::reactive({
      zoom_get_label(zoom_levels()$zoom_levels, lang = r$lang())
    })

    # Update the slider when zoom changes, only on auto_zoom
    selected <- shiny::reactive({
      if (!zoom_auto()) {
        return(NULL)
      }
      zoom_get_name(zoom_string(), lang = r$lang())
    })

    zoom_slider <- slider_text_server(
      id = "zoom_slider",
      r = r,
      choices = choices,
      selected = selected
    )

    # Return the tile() reactive, indicating if the map should show an
    # auto-scale or a scale (e.g. `CMA_auto_zoom` vs `CMA_DA`)

    # NDS: This won't use map_zoom_levels. `modules` will hold paths of
    # possible scales combinations (`borough_CT_DA_building`). On auto-zoom,
    # if the top-scale of the user is borough, this is what will be the output
    # of `tile` as it must inform the map tileset to show. If the user is
    # not on autozoom, then tile will be the scale the user selected on the slider.
    tile <- shiny::reactive({
      # On auto-scale, return the auto_zoom
      if (zoom_auto() && !no_autozoom()) {
        out <- paste(zoom_levels()$region, "auto_zoom", sep = "_")
        if (!is.na(suffix_zoom_levels)) out <- sprintf("%s_%s", out, suffix_zoom_levels)
        return(out)
      }

      scale <- zoom_get_code(zoom_slider(), lang = r$lang())

      # A change in region should switch the tile back to auto-scale to deal with
      # the fact that some scales are not available in some regions
      get_mzl <- paste0("map_zoom_levels_", zoom_levels()$region)
      mzl <- tryCatch(get_from_globalenv(get_mzl),
        error = function(e) c(missing = "missing")
      )
      if (!scale %in% names(mzl)) {
        scale <- if (!no_autozoom()) "auto_zoom" else names(mzl)[[1]]
      }

      return(paste(zoom_levels()$region, scale, sep = "_"))
    })

    # If there's only one zoom level, hide the slider and the auto-zoom
    shiny::observe({
      # Do nothing if there are more than one zoom level
      if (length(zoom_levels()$zoom_levels) > 1) {
        return()
      }

      # Add one namespace as these are inside other module servers
      shinyjs::hide(shiny::NS(id, "zoom_auto"))
      # Follow the rest of the slidertext addition ID for the zoom slider
      shinyjs::hide("zoon_slider_div")

      # Replace the content of zoom_cbx_loc (zoom checkbox location)
      zoom_name <- zoom_get_name(names(zoom_levels()$zoom_levels), r$lang())
      shinyjs::html("zoom_cbx_loc", zoom_name)
    })

    # Return the tile
    return(tile)
  })
}


#' @describeIn zoom_server Create the UI for the zoom module
#' @export
zoom_UI <- function(id, zoom_levels) {
  shiny::tagList(
    shiny::div(
      class = "scale-panel",
      id = shiny::NS(id, "zoom_div"),
      shiny::div(
        class = "shiny-split-layout sidebar-section-title",
        shiny::div(
          style = "width: 9%",
          icon_material_title("filter_alt")
        ),
        shiny::div(
          style = "width: 24%",
          cc_t_span("Scale")
        ),
        shiny::div(
          id = shiny::NS(id, "zoom_cbx_loc"),
          style = "width: 64%; margin:0px !important; text-align: right;",
          checkbox_UI(
            id = shiny::NS(id, "zoom_auto"),
            label = cc_t("Auto-scale"),
            value = TRUE
          )
        )
      ),
      shiny::div(
        id = shiny::NS(id, "zoon_slider_div"),
        class = "sus-sidebar-control",
        slider_text_UI(
          id = shiny::NS(id, "zoom_slider"),
          label = NULL,
          choices = zoom_get_label(zoom_levels)
        )
      )
    )
  )
}
