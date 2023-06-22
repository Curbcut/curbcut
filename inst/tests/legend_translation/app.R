# Get a minimalist variables table in the global env
assign("variables",
       value = qs::qread(here::here("tests/testthat/resources/variables.qs")),
       envir = .GlobalEnv
)

# Assign the colours list in the global environment
assign("colours_dfs",
       value = qs::qread(here::here("tests/testthat/resources/colours_dfs.qs")),
       envir = .GlobalEnv
)

# Variables present in the .GlobalEnv
assign("all_choropleths",
       value = c("CSD", "CT", "DA", "building", "grid"),
       envir = .GlobalEnv
)

# Translation dataframe present in the .GlobalEnv
assign("translation_df",
       value = qs::qread(here::here("tests/testthat/resources/translation_df.qs")),
       envir = .GlobalEnv
)

ui <- shiny::fluidPage(
  curbcut::use_curbcut_js(),
  curbcut::language_UI(),
  curbcut::legend_UI(id = "legend")
)

server <- function(input, output, session) {

  r <- shiny::reactiveValues(lang = shiny::reactiveVal("en"))
  curbcut::language_server(r = r, parent_session = session)

  df <- shiny::reactive("CMA_CSD")
  vars <- shiny::reactive(curbcut::vars_build(
    var_left = "alp_2016",
    var_right = "housing_tenant_2016",
    df = df()
  ))
  data <- shiny::reactive(data.frame())

  curbcut::legend_server(
    id = "legend",
    r = r,
    vars = vars,
    data = data,
    df = df
  )
}

shiny::shinyApp(ui, server)


# shiny::shinyApp(
#   ui = shiny::fluidPage(
#     # LANGUAGE
#     shiny::actionLink(
#       inputId = "language_button",
#       style = "min-width: 112px;",
#       label = shiny::span(shiny::span("Lang"))
#     ),
#     legend_UI(id = "legend")
#   ),
#   server = function(input, output, session) {
#     # LANGUAGE
#     r <- shiny::reactiveValues(lang = shiny::reactiveVal("en"))
#     # A click on the button switches the active language
#     shiny::observeEvent(input$language_button,
#                         {
#                           r$lang(if (r$lang() == "en") "fr" else "en")
#                         },
#                         ignoreNULL = FALSE,
#                         ignoreInit = TRUE
#     )
#
#     df <- shiny::reactive("raster")
#     vars <- shiny::reactive(vars_build(
#       var_left = c("c_flood"),
#       var_right = c(" "),
#       df = df()
#     ))
#     data <- shiny::reactive(data.frame())
#
#     legend_server(
#       id = "legend",
#       r = r,
#       vars = vars,
#       data = data,
#       df = df
#     )
#   }
# )
