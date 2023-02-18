#' Create the UI and server logic for the legend module
#'
#' The height of the legend plot is determined based on the the class of the
#' `vars` object. If the `bivar` string is detected in in the class, then the
#' height is set to 150 pixels. In all other cases, the height is 60 pixels.
#' This ensures that the plot legend is sized appropriately for the content
#' of the plot (`bivar` plot is displayed as a 3x3 blocks square). Around the
#' \code{\link[curbcut]legend_render} function, there is a tryCatch used to
#' catch any errors that occur when rendering the legend. If an error occurs,
#' the error message is printed to the console and the function returns
#' \code{NULL}. This ensures that the Shiny application continues to function
#' even if there is an error with the legend module.
#'
#' @param id <`character`> The ID of the page in which the legend will appear,
#' e.g. `canale`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file.
#' @param vars <`named list`> Named list with a class. Object build using the
#' \code{\link[curbcut]{build_vars}} function.
#' @param data <`reactive data.frame`> Data frame containing all the scale and
#' the `var_left` and `var_right`. The output of \code{\link[curbcut]{get_data}}.
#' @param df <`reactive character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`.
#' @param hide <`reactive logical`> Should the legend be hidden? Defaults to
#' `shiny::reactive(FALSE)`
#' @param build_str_as_DA <`reactive logical`> Should the buildings and streets
#' be treated as DAs? Defaults to `shiny::reactive(TRUE)`.
#' @param breaks <`reactive numeric vector`> Breaks if they need to be manually
#' supplied to the legend module.
#'
#' @return The legend Shiny UI and server functions
#' @export
legend_server <- function(id, r, vars, data, df, hide = shiny::reactive(FALSE),
                          build_str_as_DA = shiny::reactive(TRUE),
                          breaks = shiny::reactive(NULL)) {
  stopifnot(shiny::is.reactive(data))
  stopifnot(shiny::is.reactive(vars))
  stopifnot(shiny::is.reactive(df))
  stopifnot(shiny::is.reactive(build_str_as_DA))
  stopifnot(shiny::is.reactive(hide))
  stopifnot(shiny::is.reactive(breaks))

  shiny::moduleServer(id, function(input, output, session) {
    # Define plot height
    plot_height <- function() {
      if (grepl("bivar", attr(vars(), "class"))) return(150)
      return(60)
    }

    # Make legend
    legend <- shiny::reactive(
      tryCatch(legend_render(
        vars = vars(),
        lang = r$lang(),
        data = data(),
        df = df(),
        build_str_as_DA = build_str_as_DA(),
        breaks = breaks()
      ), error = function(e) {
        # If does not work as intended, warn the error and return nothing
        print(warning(e))
        return(NULL)
      })
      )

    # Output legend
    output$legend_render <- shiny::renderUI({
      output$legend <- shiny::renderPlot(legend())
      # Weird hack to get legend plot to inherit full namespace
      shiny::plotOutput(session$ns("legend"),
        height = plot_height(),
        width = "100%"
      )
    })

    # Toggle legend display
    shiny::observe(shinyjs::toggle("legend_div", condition = !is.null(legend())))
  })
}

#' @describeIn legend_server Create the UI for the legend module
#' @export
legend_UI <- function(id) {
  shiny::div(
    id = shiny::NS(id, "legend_div"),
    shiny::h5(cc_t("Legend"),
              style = "font-size: 12px;"
    ),
    shiny::uiOutput(shiny::NS(id, "legend_render"))
  )
}
