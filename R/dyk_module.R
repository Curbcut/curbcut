#' Did You Know Server and UI Module
#'
#' This function provides a server module for the "Did You Know" (DYK) section
#' of the a Curbcut map page. It retrieves and displays interesting facts, based
#' on `vars` and `poi` (point of interests). The server module retrieves DYKs and
#' handles the click events on the links.
#'
#' @param id <`character`> The ID of the page in which the module will appear,
#' e.g. `alp`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link{vars_build}} function.
#' @param df <`reactive character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_scale}}.
#' @param select_id <`character`> A string indicating the ID of the currently
#' selected region (if any). Usually `r[[id]]$select_id()`
#' @param region <`character`> Character string specifying the name of the region.
#' Usually equivalent of `r$region()`.
#' @param zoom_levels <`named numeric vector`> A named numeric vector of zoom
#' levels. Usually one of the `map_zoom_levels_x`, or the output of
#' \code{\link{geography_server}}.
#' @param poi <`reactive`> (Optional) Point of interests. Default is NULL.
#' @param scales_as_DA <`character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#'
#' @return A Shiny module server function for the DYK module.
#' @export
dyk_server <- function(id, r, vars, df, select_id, region, zoom_levels,
                       poi = shiny::reactive(NULL),
                       scales_as_DA = shiny::reactive(c("building", "street"))) {

  # Error checking
  stopifnot(shiny::is.reactive(vars))
  stopifnot(shiny::is.reactive(select_id))
  stopifnot(shiny::is.reactive(poi))
  stopifnot(shiny::is.reactive(region))
  stopifnot(shiny::is.reactive(zoom_levels))

  shiny::moduleServer(id, function(input, output, session) {

    # Get the DYKs
    dyk <- shiny::reactive(dyk_get(
      id = id, vars = vars(), df = df(), select_id = select_id(), poi = poi(),
      region = region(), zoom_levels = zoom_levels(), scales_as_DA = scales_as_DA(),
      lang = r$lang()
    ))

    # Hide the panel if there are no DYK
    shiny::observe({
      shinyjs::toggle(id = "dyk_panel", condition = !is.null(dyk()))
    })

    # Observe for clicks
    shiny::observeEvent(input$dyk_1, do.call(
      link, c(r = list(r), attr(dyk()[[1]], "links"))
    ))
    shiny::observeEvent(input$dyk_2, do.call(
      link, c(r = list(r), attr(dyk()[[2]], "links"))
    ))

    # Only show contents if dyk_output isn't empty
    output$dyk_contents <- shiny::renderUI({
      if (!is.null(dyk())) {
        # Convert back the character to HTML tag
        out <- lapply(dyk(), shiny::HTML)
        if (length(out) > 1) Reduce(shiny::tags$ul, out) else shiny::tags$ul(out)
      }
    })
  })
}

#' @describeIn dyk_server Create the UI for the legend module
#' @export
dyk_UI <- function(id) {
  shiny::tagList(
    shinyjs::hidden(shiny::div(
      id = shiny::NS(id, "dyk_panel"),
      shiny::fluidRow(
        class = "dyk-panel-title",
        shiny::column(
          width = 7,
          shiny::h4(
            icon_material_title("info"),
            cc_t_span("Did you know?")
          )
        )
      ),
      shiny::div(
        shiny::uiOutput(shiny::NS(id, "dyk_contents"))
      )
    ))
  )
}
