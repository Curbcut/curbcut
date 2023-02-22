#' Create the UI and server logic for the legend module
#'
#' The height of the legend plot is determined based on the class of the `vars`
#' object. If the string `bivar` is detected in the class, the height is set to
#' 150 pixels to accommodate a larger legend required for a `bivar` plot
#' (displayed as a 3x3 blocks square). In all other cases, the height is set to
#' 60 pixels, ensuring that the legend is sized appropriately for the content
#' of the plot. The tryCatch function is used to catch any errors that occur
#' when rendering the legend with the \code{\link{legend_render}}
#' function. If an error occurs, the error message is printed to the console
#' and the function returns \code{NULL}. This ensures that the Shiny application
#' continues to function even if there is an error with the legend module.
#'
#' The class of the `vars` object is important for deciding which type of legend
#' to draw. If the `vars` object has an unrecognized class, the default is that
#' no legend will be drawn.
#'
#' @param id <`character`> The ID of the page in which the legend will appear,
#' e.g. `canale`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file.
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link{vars_build}} function. The class of the vars object is
#' used to determine which type of legend to draw.
#' @param df <`reactive character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{df_get}}.
#' @param data <`reactive data.frame`> Data frame containing all the scale and
#' the `var_left` and `var_right`. The output of \code{\link{data_get}}.
#' @param hide <`reactive logical`> Should the legend be hidden? Defaults to
#' `shiny::reactive(FALSE)`
#' @param breaks <`reactive numeric vector`> Breaks if they need to be manually
#' supplied to the legend module.
#' @param scales_as_DA <`character vector`> A character vector of `scales` that
#' should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#'
#' @return The legend Shiny UI and server module functions
#' @export
legend_server <- function(id, r, vars, df, data, hide = shiny::reactive(FALSE),
                          breaks = shiny::reactive(NULL),
                          scales_as_DA = shiny::reactive(c("building", "street"))) {
  stopifnot(shiny::is.reactive(df))
  stopifnot(shiny::is.reactive(data))
  stopifnot(shiny::is.reactive(vars))
  stopifnot(shiny::is.reactive(hide))
  stopifnot(shiny::is.reactive(breaks))

  shiny::moduleServer(id, function(input, output, session) {
    # Define plot height
    plot_height <- function() {
      if (grepl("bivar", attr(vars(), "class"))) {
        return(150)
      }
      return(60)
    }

    # Switch scales to DA if necessary
    treated_df <-
      shiny::reactive(treat_to_DA(scales_as_DA = scales_as_DA(), df = df()))

    # Make legend
    legend <- shiny::reactive(
      tryCatch(legend_render(
        vars = vars(),
        lang = r$lang(),
        df = treated_df(),
        data = data(),
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
