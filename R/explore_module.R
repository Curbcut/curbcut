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
#' e.g. `canale`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param vars <`reactive named list`> Named list with a class. Object built
#' using the \code{\link{vars_build}} function. The class of the vars object is
#' used to determine which type of legend to draw.
#' @param df <`reactive character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param data <`reactive data.frame`> Data frame containing all the scale and
#' the `var_left` and `var_right`. The output of \code{\link{data_get}}.
#' @param region <`reactive character`> A string or numeric value representing the
#' selected region.
#' @param select_id <`reactive character`> the current selected ID, usually
#' `r[[id]]$select_id()`.
#' @param scales_as_DA <`reactive character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#' @param table <`reactive function`> A reactive expression for the function used
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
#'
#' @return The explore Shiny UI and server module functions
#' @export
explore_server <- function(id, r, data, vars, region, df, select_id,
                           scales_as_DA = shiny::reactive(c("building", "street")),
                           # graph = reactive(explore_graph),
                           # graph_args = reactive(list(
                           #   r = r,
                           #   data = data(), vars = vars(), df = df(),
                           #   select_id = select_id(), region = region())),
                           table = shiny::reactive(explore_text),
                           table_args = shiny::reactive(list(
                             r = r, data = data(), vars = vars(),
                             select_id = select_id(), region = region(),
                             scales_as_DA = scales_as_DA(), df = df()))) {

  stopifnot(shiny::is.reactive(data))
  stopifnot(shiny::is.reactive(vars))
  stopifnot(shiny::is.reactive(region))
  stopifnot(shiny::is.reactive(df))
  stopifnot(shiny::is.reactive(select_id))
  stopifnot(shiny::is.reactive(scales_as_DA))
  # stopifnot(shiny::is.reactive(graph))
  # stopifnot(shiny::is.reactive(graph_args))
  stopifnot(shiny::is.reactive(table))
  stopifnot(shiny::is.reactive(table_args))

  shiny::moduleServer(id, function(input, output, session) {

    # Make info table. If fails, returns NULL
    table_out <- shiny::reactive(
      tryCatch(
        do.call(table(), table_args())
        , error = function(e) {
          print(e)
          return(NULL)
        })
    )

    # Display info table
    output$info_table <- shiny::renderUI(shiny::HTML(table_out()))

    # # Make graph
    # graph_out <- reactive(tryCatch(do.call(graph(), graph_args()),
    #                                error = function(e) {
    #                                  print(e)
    #                                  return(NULL)
    #                                }))

    # Display graph
    # output$explore_graph <- renderPlot(graph_out())

    # Show/hide components
    shiny::observe({
      shinyjs::toggle("info_table", condition = !is.null(table_out()))
      # shinyjs::toggle("explore_graph", condition = !is.null(graph_out()))
      shinyjs::toggle("clear_selection", condition = !is.na(select_id()))
    })

    # Clear selection on button click
    shiny::observeEvent(input$clear_selection, r[[id]]$select_id(NA),
                        ignoreInit = TRUE)

    # Hide compare picker and update the the action link
    shiny::observeEvent(input$hide_explore, {
      shinyjs::toggle("explore_content",
                      condition = input$hide_explore %% 2 == 0)

      # Change label
      lab <- if (input$hide_explore %% 2 == 0) {
        cc_t(lang = r$lang(), "Hide")
      } else {
        cc_t(lang = r$lang(), "Show")
      }
      shiny::updateActionButton(
        session = session,
        inputId = "hide_explore",
        label = lab
      )
    })

  })
}

#' @describeIn explore_server Create the UI for the explore module
#' @export
explore_UI <- function(id) {

  shiny::tagList(
    shiny::div(id = shiny::NS(id, "explore_title"),
               shiny::fluidRow(shiny::column(
                 width = 7,
                 shiny::h4(cc_t("Explore"))),
                 shiny::column(
                   width = 5, align = "right",
                   shiny::actionLink(
                     inputId = shiny::NS(id, "hide_explore"),
                     class = "sus-small-link",
                     label = cc_t("Hide"))))),

    shiny::div(id = shiny::NS(id, "explore_content"),
               shiny::htmlOutput(outputId = shiny::NS(id, "info_table")),
               # shiny::plotOutput(outputId = shiny::NS(id, "explore_graph"),
               #                   height = 150),
               shinyjs::hidden(
                 shiny::actionLink(inputId = shiny::NS(id, "clear_selection"),
                                   label = cc_t("Clear selection"))))
  )
}