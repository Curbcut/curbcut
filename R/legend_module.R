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
#' e.g. `alp`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link{vars_build}} function. The class of the vars object is
#' used to determine which type of legend to draw.
#' @param data <`reactive data.frame`> Data frame containing all the scale and
#' the `var_left` and `var_right`. The output of \code{\link{data_get}}.
#' @param scale <`character`> Current scale.
#' @param time <`reactive named list`> Object built using the \code{\link{vars_build}}
#' function. It contains the time for both var_left and var_right variables.
#' @param hide <`reactive logical`> Should the legend be hidden? Defaults to
#' `shiny::reactive(FALSE)`
#' @param breaks <`reactive numeric vector`> Breaks if they need to be manually
#' supplied to the legend module.
#' @param scales_as_DA <`reactive character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#' @param legend_fun <`reactive function`> A function of which the output is a `ggplot`
#' that will be placed as the legend. Defaults to \code{\link{legend_render}}.
#' @param legend_args <`reactive list`> List of arguments to be passed to the
#' `legend_fun` argument.
#' @param force_height <`reactive numeric`> Optional argument. Use to overwrite
#' the height of the legend. Default height is 60 pixels, and 150 on a `bivar`
#' class.
#'
#' @return The legend Shiny UI and server module functions
#' @export
legend_server <- function(id, r, vars, scale, data, time, hide = shiny::reactive(FALSE),
                          breaks = shiny::reactive(NULL),
                          scales_as_DA = shiny::reactive(c("building", "street")),
                          legend_fun = shiny::reactive(legend_render),
                          legend_args = shiny::reactive(list(
                            vars = vars(), lang = r$lang(), scale = scale(),
                            data = data(), breaks = breaks(),
                            scales_as_DA = scales_as_DA()
                          )),
                          force_height = shiny::reactive(NULL)) {
  stopifnot(shiny::is.reactive(data))
  stopifnot(shiny::is.reactive(vars))
  stopifnot(shiny::is.reactive(hide))
  stopifnot(shiny::is.reactive(breaks))
  stopifnot(shiny::is.reactive(scales_as_DA))
  stopifnot(shiny::is.reactive(legend_fun))
  stopifnot(shiny::is.reactive(legend_args))

  shiny::moduleServer(id, function(input, output, session) {
    # If `time` should have an impact on the legend, add it to the list of arguments.
    # It's not directly an argument as we don't want the legend to be triggered by a
    # change in `time` when it's not needed (q5, bivar, ...)
    legend_arguments <- shiny::reactiveVal(legend_args())
    # shiny::observeEvent(legend_args(), legend_arguments(legend_args()))
    shiny::observeEvent(
      {
        time()
        legend_args()
      },
      {
        # Isolate the change to legend_arguments
        shiny::isolate(legend_arguments(legend_args()))

        # If in delta, add time
        if (sum(grepl("delta", attr(vars(), "class"))) > 0) {
          # Remove `time` before adding it
          # args <- legend_arguments()[names(legend_arguments()) != "time"]
          legend_arguments(c(legend_arguments(), list(time = time())))
        }
      }
    )

    # Make legend
    legend <- shiny::reactive(
      tryCatch(do.call(legend_fun(), legend_arguments()), error = function(e) {
        # If does not work as intended, warn the error and return nothing
        print(e)
        return(NULL)
      })
    )

    # Define plot height
    plot_height <- function() {
      # If the height is to be override
      if (!is.null(force_height())) {
        return(force_height())
      }
      # If there's the `bivar` string detected in one of the classes
      if (sum(grepl("bivar", attr(vars(), "class"))) > 0) {
        return(150)
      }
      return(60)
    }

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
    shiny::h5(cc_t_span("Legend"),
      style = "font-size: 12px;"
    ),
    shiny::uiOutput(shiny::NS(id, "legend_render"))
  )
}
