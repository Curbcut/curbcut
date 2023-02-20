shiny::shinyApp(
  ui = shiny::fluidPage(
    # LANGUAGE
    shiny::actionLink(
      inputId = "language_button",
      style = "min-width: 112px;",
      label = shiny::span(shiny::span("Lang"))
    ),
    legend_UI(id = "legend")
  ),
  server = function(input, output, session) {
    # LANGUAGE
    r <- shiny::reactiveValues(lang = shiny::reactiveVal("en"))
    # A click on the button switches the active language
    shiny::observeEvent(input$language_button,
      {
        r$lang(if (r$lang() == "en") "fr" else "en")
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    df <- shiny::reactive("CMA_CSD")
    vars <- shiny::reactive(vars_build(
      var_left = "canale_2016",
      var_right = "housing_tenant_2016",
      df = df()
    ))
    data <- shiny::reactive(tibble::tibble())

    legend_server(
      id = "legend",
      r = r,
      vars = vars,
      data = data,
      df = df
    )
  }
)


shiny::shinyApp(
  ui = shiny::fluidPage(
    # LANGUAGE
    shiny::actionLink(
      inputId = "language_button",
      style = "min-width: 112px;",
      label = shiny::span(shiny::span("Lang"))
    ),
    legend_UI(id = "legend")
  ),
  server = function(input, output, session) {
    # LANGUAGE
    r <- shiny::reactiveValues(lang = shiny::reactiveVal("en"))
    # A click on the button switches the active language
    shiny::observeEvent(input$language_button,
      {
        r$lang(if (r$lang() == "en") "fr" else "en")
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    df <- shiny::reactive("CMA_CSD")
    vars <- shiny::reactive(vars_build(
      var_left = "canale_2016",
      var_right = " ",
      df = df()
    ))
    data <- shiny::reactive(tibble::tibble())

    legend_server(
      id = "legend",
      r = r,
      vars = vars,
      data = data,
      df = df
    )
  }
)


shiny::shinyApp(
  ui = shiny::fluidPage(
    # LANGUAGE
    shiny::actionLink(
      inputId = "language_button",
      style = "min-width: 112px;",
      label = shiny::span(shiny::span("Lang"))
    ),
    legend_UI(id = "legend")
  ),
  server = function(input, output, session) {
    # LANGUAGE
    r <- shiny::reactiveValues(lang = shiny::reactiveVal("en"))
    # A click on the button switches the active language
    shiny::observeEvent(input$language_button,
      {
        r$lang(if (r$lang() == "en") "fr" else "en")
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    df <- shiny::reactive("CMA_CSD")
    vars <- shiny::reactive(vars_build(
      var_left = c("inc_limat_2006", "inc_limat_2016"),
      var_right = c(
        "housing_tenant_2006",
        "housing_tenant_2016"
      ),
      df = df()
    ))
    data <- shiny::reactive(tibble::tibble())

    legend_server(
      id = "legend",
      r = r,
      vars = vars,
      data = data,
      df = df
    )
  }
)


shiny::shinyApp(
  ui = shiny::fluidPage(
    # LANGUAGE
    shiny::actionLink(
      inputId = "language_button",
      style = "min-width: 112px;",
      label = shiny::span(shiny::span("Lang"))
    ),
    legend_UI(id = "legend")
  ),
  server = function(input, output, session) {
    # LANGUAGE
    r <- shiny::reactiveValues(lang = shiny::reactiveVal("en"))
    # A click on the button switches the active language
    shiny::observeEvent(input$language_button,
      {
        r$lang(if (r$lang() == "en") "fr" else "en")
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    df <- shiny::reactive("CMA_CSD")
    vars <- shiny::reactive(vars_build(
      var_left = c("inc_limat_2006", "inc_limat_2016"),
      var_right = c(" "),
      df = df()
    ))
    data <- shiny::reactive(tibble::tibble())

    legend_server(
      id = "legend",
      r = r,
      vars = vars,
      data = data,
      df = df
    )
  }
)

shiny::shinyApp(
  ui = shiny::fluidPage(
    # LANGUAGE
    shiny::actionLink(
      inputId = "language_button",
      style = "min-width: 112px;",
      label = shiny::span(shiny::span("Lang"))
    ),
    legend_UI(id = "legend")
  ),
  server = function(input, output, session) {
    # LANGUAGE
    r <- shiny::reactiveValues(lang = shiny::reactiveVal("en"))
    # A click on the button switches the active language
    shiny::observeEvent(input$language_button,
      {
        r$lang(if (r$lang() == "en") "fr" else "en")
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    df <- shiny::reactive("raster")
    vars <- shiny::reactive(vars_build(
      var_left = c("c_flood"),
      var_right = c(" "),
      df = df()
    ))
    data <- shiny::reactive(tibble::tibble())

    legend_server(
      id = "legend",
      r = r,
      vars = vars,
      data = data,
      df = df
    )
  }
)
