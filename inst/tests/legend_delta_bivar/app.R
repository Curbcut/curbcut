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

# Connection to the sqlite db present in the .GlobalEnv
assign("city_CSD_conn",
       value = DBI::dbConnect(RSQLite::SQLite(),
                              here::here("tests/testthat/resources/city_CSD.sqlite")),
       envir = .GlobalEnv
)

ui <- shiny::fluidPage(
  curbcut::legend_UI(id = "legend")
)

server <- function(input, output, session) {
  df <- shiny::reactive("city_CSD")
  vars <- shiny::reactive(curbcut::vars_build(
    var_left = c("housing_rent_2006", "housing_rent_2016"),
    var_right = c(
      "housing_tenant_2006",
      "housing_tenant_2016"
    ),
    df = df()
  ))
  data <- shiny::reactive(curbcut::data_get(vars = vars(), df = df()))

  curbcut::legend_server(
    id = "legend",
    r = r,
    vars = vars,
    data = data,
    df = df
  )
}

shiny::shinyApp(ui, server)
