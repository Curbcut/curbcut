#' Explore Server
#'
#' This function creates the server for the explore module. The module displays
#' an info table and a graph of the selected variables and geographic region.
#' The explore_server function has an argument named table, which is a reactive
#' expression for the function used to generate the info table in the explore
#' module. By default, this argument is set to `shiny::reactive(explore_text)`,
#' which is a reactive expression that evaluates to the explore_text function.
#' If you want to substitute the table argument with another function, you can
#' simply pass a different reactive expression for this argument. The function
#' you provide should have the same arguments as table_args, which is another
#' argument of the explore_server function that is a reactive expression for
#' the arguments to be passed to the info table function. Same goes for the `graph`
#' and `graph_args` arguments.
#'
#' @param id <`character`> The ID of the page in which the legend will appear,
#' e.g. `alp`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param vars <`reactive named list`> Named list with a class. Object built
#' using the \code{\link{vars_build}} function. The class of the vars object is
#' used to determine which type of legend to draw.
#' @param scale <`reactive character`> Current scale. The output of
#' \code{\link{update_scale}}.
#' @param data <`reactive data.frame`> Data frame containing all the scale and
#' the `var_left` and `var_right`. The output of \code{\link{data_get}}.
#' @param region <`reactive character`> A string or character value representing the
#' selected region.
#' @param select_id <`reactive character`> The current selected ID, usually
#' `r[[id]]$select_id()`.
#' @param time <`reactive numeric vector`> The `time` at which data is displayed.
#' A list for var_left and var_right. The output of \code{\link{vars_build}}(...)$time.
#' Usually r[[id]]$time.
#' @param scales_as_DA <`reactive character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#' @param table_fun <`reactive function`> A reactive expression for the function used
#' to generate the info table in the explore module. The default value is
#' `shiny::reactive(explore_text)`, which is a reactive expression that
#' evaluates to the `explore_text` function. If you want to use a different
#' function to generate the info table, you can provide a custom reactive
#' expression for this argument. The function you provide should have the same
#' arguments as table_args.
#' @param table_args <`reactive list`> A reactive expression for the arguments
#' to be passed to the info table function. If you want to customize the arguments
#' passed to the info table function, you can provide a different reactive
#' expression for this argument. The reactive expression should evaluate to a
#' list of arguments to be passed to the info table function, including any
#' additional arguments required by your custom table function.
#' @param graph_fun <`reactive function`> A reactive expression for the function used
#' to generate the explore graph in the explore module. The default value is
#' `shiny::reactive(explore_graph)`, which is a reactive expression that
#' evaluates to the `explore_graph` function. If you want to use a different
#' function to generate the explore graph, you can provide a custom reactive
#' expression for this argument. The function you provide should have the same
#' arguments as graph_args.
#' @param graph_args <`reactive list`> A reactive expression for the arguments
#' to be passed to the explore graph function. If you want to customize the arguments
#' passed to the explore graph function, you can provide a different reactive
#' expression for this argument. The reactive expression should evaluate to a
#' list of arguments to be passed to the explore graph function, including any
#' additional arguments required by your custom graph function.
#'
#' @return The explore Shiny UI and server module functions
#' @export
explore_server <- function(id, r, data, vars, region, scale, select_id, time,
                           scales_as_DA = shiny::reactive(c("building", "street")),
                           graph_fun = shiny::reactive(explore_graph),
                           graph_args = shiny::reactive(list(
                             r = r, data = data(), vars = vars(), scale = scale(),
                             time = time(), select_id = select_id(), region = region(),
                             scales_as_DA = scales_as_DA(), lang = r$lang()
                           )),
                           table_fun = shiny::reactive(explore_text),
                           table_args = shiny::reactive(list(
                             r = r, data = data(), vars = vars(), scale = scale(),
                             time = time(), select_id = select_id(), region = region(),
                             scales_as_DA = scales_as_DA(), lang = r$lang()
                           ))) {
  stopifnot(shiny::is.reactive(data))
  stopifnot(shiny::is.reactive(vars))
  stopifnot(shiny::is.reactive(region))
  stopifnot(shiny::is.reactive(scale))
  stopifnot(shiny::is.reactive(time))
  stopifnot(shiny::is.reactive(select_id))
  stopifnot(shiny::is.reactive(scales_as_DA))
  stopifnot(shiny::is.reactive(graph_fun))
  stopifnot(shiny::is.reactive(graph_args))
  stopifnot(shiny::is.reactive(table_fun))
  stopifnot(shiny::is.reactive(table_args))

  shiny::moduleServer(id, function(input, output, session) {
    # Make info table. If fails, returns NULL
    table_out <- shiny::reactive(
      tryCatch(
        do.call(table_fun(), table_args()),
        error = function(e) {
          print(e)
          return(NULL)
        }
      )
    )

    # Display info table
    output$info_table <- shiny::renderUI(shiny::HTML(table_out()))

    # Make graph
    graph_out <- shiny::reactive(
      tryCatch(
        do.call(graph_fun(), graph_args()),
        error = function(e) {
          print(e)
          return(NULL)
        }
      )
    )

    # Display graph
    output$explore_graph <- shiny::renderPlot(graph_out())

    # Show/hide components
    shiny::observe({
      shinyjs::toggle("info_table", condition = !is.null(table_out()))
      # shinyjs::toggle("explore_graph", condition = !is.null(graph_out()))
      shinyjs::toggle("clear_selection", condition = !is.na(select_id()))
    })

    # Clear selection on button click
    shiny::observeEvent(input$clear_selection, {
      r[[id]]$select_id(NA)
      cc.map::map_choropleth_update_selection(
        session = session,
        map_ID = "map",
        select_id = NA
      )
    },
    ignoreInit = TRUE
    )
  })
}

#' @describeIn explore_server Create the UI for the explore module
#' @export
explore_UI <- function(id) {
  shiny::tagList(
    shiny::div(
      id = shiny::NS(id, "explore_full"),
      class = "explore-panel",
      shiny::div(
        id = shiny::NS(id, "explore_title"),
        shiny::fluidRow(
          shiny::column(
            width = 7,
            shiny::h4(icon_material_title("location_on"), cc_t("Explore"))
          )
        )
      ),
      shiny::div(
        id = shiny::NS(id, "explore_content"),
        shiny::htmlOutput(outputId = shiny::NS(id, "info_table")),
        shiny::plotOutput(
          outputId = shiny::NS(id, "explore_graph"),
          height = 150
        ),
        shinyjs::hidden(
          shiny::actionLink(
            inputId = shiny::NS(id, "clear_selection"),
            label = cc_t("Clear selection")
          )
        )
      )
    )
  )
}
