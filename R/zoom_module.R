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
#' @param region <`reactive character`> A string or character value representing the
#' selected region. Usually `r[[id]]$region`.
#' @param zoom_levels <`named numeric vector`> A named numeric vector of zoom
#' levels. Usually one of the `mzl_*`, or the output of
#' \code{\link{geography_server}}, becoming `r[[id]]$zoom_levels` It needs to be
#' `numeric` as the function will sort them to make sure the lower zoom level is
#' first, and the highest is last (so it makes sense on an auto-scale).
#' @param no_autozoom <`reactive logical`> Whether the output tile() reactive
#' should include autozooms or not.
#' @param hide_if_one_zoom_level <`reactive logical`> If there is only one
#' zoom level available, should the zoom slider be hidden? Careful, as setting
#' it will have a \code{\link[shinyjs]{toggle}} around the zoom div and create
#' some inconsistencies with other potential shinyjs hide/show functions.
#'
#' @return A reactive object representing the current tile that should be
#' displayed on the map.
#' @export
zoom_server <- function(id, r = r, zoom_string, region, zoom_levels,
                        no_autozoom = shiny::reactive(FALSE),
                        hide_if_one_zoom_level = shiny::reactive(FALSE)) {
  stopifnot(shiny::is.reactive(zoom_string))
  stopifnot(shiny::is.reactive(zoom_levels))

  shiny::moduleServer(id, function(input, output, session) {
    # Get the auto zoom checkbox server and add that a change in region means
    # switching back the auto-scale to get a better transition
    zoom_auto <- checkbox_server(
      id = "zoom_auto",
      r = r,
      label = shiny::reactive("Auto-scale"),
      # If any of `region` or `zoom_levels` change, reset the auto zoom
      event_reset = shiny::reactive(paste0(region(), zoom_levels()))
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
      zoom_get_label(zoom_levels(), lang = r$lang())
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
    # auto-scale or a scale. Auto-scale are tiles like CMA_CT_DA_building (
    # a collapsed version of the individual scales).
    tile <- shiny::reactive({
      # On auto-scale, return the auto_zoom
      if (zoom_auto() && !no_autozoom()) {
        out <- paste0(names(zoom_levels()), collapse = "_")
        return(out)
      }

      scale <- zoom_get_code(zoom_slider(), lang = r$lang())

      return(scale)
    })

    # If there's only one zoom level, hide the slider and the auto-zoom
    shiny::observe({
      if (!hide_if_one_zoom_level()) return(NULL)

      multiple_levels <- length(zoom_levels()) > 1

      # Add one namespace as these are inside other module servers
      shinyjs::toggle("zoom_cbx_loc", condition = multiple_levels)
      # Follow the rest of the slidertext addition ID for the zoom slider
      shinyjs::toggle("zoom_slider_div", condition = multiple_levels)

      # Replace the content of zoom_cbx_loc (zoom checkbox location)
      zoom_name <- zoom_get_name(names(zoom_levels()), r$lang())
      zoom_name <- s_sentence(zoom_name)

      if (!multiple_levels) shinyjs::html("zoom_scale_chr", zoom_name)
      if (multiple_levels) shinyjs::html("zoom_scale_chr", character())
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
          id = shiny::NS(id, "zoom_right_lab"),
          style = "width: 64%; margin:0px !important; text-align: right;",
          shiny::div(
            id = shiny::NS(id, "zoom_cbx_loc"),
            checkbox_UI(
              id = shiny::NS(id, "zoom_auto"),
              label = cc_t("Auto-scale"),
              value = TRUE
            )
          ),
          shiny::div(id = shiny::NS(id, "zoom_scale_chr"))
        )
      ),
      shiny::div(
        id = shiny::NS(id, "zoom_slider_div"),
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
