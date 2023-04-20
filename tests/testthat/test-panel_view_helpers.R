test_that("table_view_prep_table works as expected", {
  df <- "city_CSD"
  vars <- vars_build("housing_tenant_2021", df = df)
  data <- data_get(vars, df)

  actual <- table_view_prep_table(vars = vars, data = data, df = df, lang = NULL)
  expect_equal(names(actual), c("pretty_data", "data", "title_vars"))
})

test_that("table_view_prep_table translates all", {
  df <- "city_CSD"
  vars <- vars_build(c("housing_tenant_2016", "housing_tenant_2021"),
    c("canale_2016", "canale_2021"),
    df = df
  )
  data <- data_get(vars, df)

  actual <- table_view_prep_table(vars = vars, data = data, df = df, lang = "fr") |>
    suppressWarnings()
  expect_equal(
    names(actual$pretty_data)[6:11],
    c(
      "Locataire (2016)", "Locataire (2021)", "Locataire (Δ 2016 - 2021)",
      "AVA-Can (2016)", "AVA-Can (2021)", "AVA-Can (Δ 2016 - 2021)"
    )
  )
})

test_that("table_view_prep_table title vars have the right classes", {
  df <- "city_CSD"
  vars <- vars_build(c("housing_tenant_2016", "housing_tenant_2021"),
    c("canale_2016"),
    df = df
  )
  data <- data_get(vars, df)

  actual <- table_view_prep_table(vars = vars, data = data, df = df, lang = "fr") |>
    suppressWarnings()

  expect_equal(
    actual$title_vars,
    list(
      structure(c("Locataire (2016)", "Locataire (2021)"), class = c(
        "pct",
        "scalar", "character"
      )), structure("Locataire (Δ 2016 - 2021)", class = "pct"),
      structure("AVA-Can (2016)", class = c("ind", "scalar", "character"))
    )
  )
})