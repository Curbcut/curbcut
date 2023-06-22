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

ui <- shiny::fluidPage(
  curbcut::legend_UI(id = "legend")
)

server <- function(input, output, session) {
  df <- shiny::reactive("CMA_CSD")
  vars <- shiny::reactive(curbcut::vars_build(
    var_left = "alp_2016",
    var_right = " ",
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
