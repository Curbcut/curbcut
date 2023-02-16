#' Create the UI for the legend module
#'
#' @param id <`character`> The ID of the page in which the legend will appear,
#' e.g. `canale`.
#'
#' @return The legend Shiny UI object
#' @export
legend_UI <- function(id) {
  shiny::div(id = shiny::NS(id, "legend_div"),
             shiny::h5(cc_t("Legend"),
                       style = "font-size: 12px;"),
             shiny::uiOutput(shiny::NS(id, "legend_render")))
}

#' Create the server logic for the legend module
#'
#' @param id <`character`> The ID of the page in which the legend will appear,
#' e.g. `canale`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file.
#' @param data <`reactive data.frame`> Data frame containing all the scale and
#' the `var_left` and `var_right`. The output of \code{\link[curbcut]{get_data}}.
#' @param var_left <`reactive character`> Character string of the selected
#' variable, e.g. `canale_2016`.
#' @param var_right <`reactive character`> Character string of the selected
#' compared variable, e.g. `housing_tenant_2016`.
#' @param region <`reactive character`> The region under study, e.g. `CMA`.
#' Normally `r$region`, reactive created in the `server.R` file.
#' @param scale <`reactive character`> The scale under study, e.g. `DA`.
#' Normally `input$zoom_slider`, created in the every page server.
#' @param df <`reactive character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`.
#' @param hide <`reactive logical`> Should the legend be hidden? Defaults to
#' `shiny::reactive(FALSE)`
#' @param build_str_as_DA <`reactive logical`> Should the buildings and streets
#' be treated as DAs? Defaults to `shiny::reactive(TRUE)`.
#' @param breaks <`reactive numeric vector`> Breaks if they need to be manually
#' supplied to the legend module.
#'
#' @return The legend Shiny server function
#' @export
legend_server <- function(id, r, data, vars, region, scale,
                          df, hide = shiny::reactive(FALSE),
                          build_str_as_DA = shiny::reactive(TRUE),
                          breaks = shiny::reactive(NULL)) {

  stopifnot(shiny::is.reactive(data))
  stopifnot(shiny::is.reactive(vars))
  stopifnot(shiny::is.reactive(region))
  stopifnot(shiny::is.reactive(df))
  stopifnot(shiny::is.reactive(build_str_as_DA))
  stopifnot(shiny::is.reactive(hide))
  stopifnot(shiny::is.reactive(breaks))

  shiny::moduleServer(id, function(input, output, session) {

    # Define plot height
    plot_height <- function() {
      # if (length(var_left()) == 1 && var_right()[1] == " ") 1 else 2.5
      if ((length(var_left()) == 1 && var_right()[1] == " ") ||
          (length(var_left()) == 2 && var_right()[1] == " ")) 60 else 150
    }

    # Make legend
    legend <- shiny::reactive(tryCatch(
      legend_render(
        r = r,
        data = data(),
        vars = vars(),
        df = df(),
        region = region(),
        scale = scale(),
        build_str_as_DA = build_str_as_DA(),
        breaks = breaks()),
      error = function(e) NULL)
    )

    # Output legend
    output$legend_render <- shiny::renderUI({
      output$legend <- shiny::renderPlot(legend())
      # Weird hack to get legend plot to inherit full namespace
      shiny::plotOutput(session$ns("legend"), height = plot_height(),
                        width = "100%")
    })

    # Toggle legend display
    shiny::observe(shinyjs::toggle("legend_div", condition = !is.null(legend())))

  })
}



