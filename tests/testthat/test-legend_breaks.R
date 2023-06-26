test_that("legend_breaks.q5 works", {
  vars1 <- vars_build(var_left = "housing_tenant_2016", df = "CMA_CSD")
  vars2 <- vars_build(var_left = "housing_rent_2011", df = "CMA_DA")
  vars3 <- vars_build(var_left = "climate_drought_2015", df = "grid_grid50")

  expect_equal(
    all(grepl("^\\d", legend_breaks(vars1, df = "CMA_CSD"))),
    TRUE
  )
  expect_equal(
    all(grepl("^\\$\\d", legend_breaks(vars2, df = "CMA_CSD"))),
    TRUE
  )
})

test_that("legend_breaks.q5_ind works", {
  vars1 <- vars_build(var_left = "climate_drought_2015", df = "grid_grid50")
  vars2 <- vars_build(var_left = "alp_2016", df = "grid_grid50")

  expect_equal(
    unname(legend_breaks(vars1, df = "grid_grid50")),
    structure(c("Insig.", "Minor", "Mod.", "Elev.", "Major"))
  )

  expect_equal(
    legend_breaks(vars2, df = "CMA_CSD"),
    structure(list("Low", NULL, NULL, NULL, "High"))
  )
})

test_that("legend_breaks.q100 works", {
  vars <- vars_build(var_left = "climate_drought_2015", df = "raster")
  expect_equal(
    legend_breaks(vars, df = "raster"),
    list(
      "Low", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
      "High"
    )
  )
})

# test_that("legend_breaks.qual works", {
#
# })

test_that("legend_breaks.bivar_ldelta_rq3 works", {
  vars <- vars_build(
    var_left = c(
      "housing_tenant_2006",
      "housing_tenant_2016"
    ),
    var_right = "alp_2016",
    df = "city_CSD"
  )
  data <- data_get(
    vars = vars,
    df = "city_CSD"
  )
  actual <- legend_breaks(vars,
    data = data,
    df = "city_CSD"
  )
  expect_equal(
    names(actual),
    c("x", "y")
  )
  expect_equal(
    sum(grepl("%$", actual[[2]])),
    sum(c(T, T, T, T))
  )
  expect_equal(
    length(actual$x), 4
  )
})

test_that("legend_breaks.delta works", {
  vars <- vars_build(var_left = c(
    "housing_tenant_2006",
    "housing_tenant_2016"
  ), df = "CMA_CT")
  expect_equal(
    legend_breaks(vars, df = "CMA_CT"),
    c("-10%", "-2%", "+2%", "+10%")
  )
})

test_that("legend_breaks.bivar works", {
  vars <- vars_build(
    var_left = "climate_drought_2015",
    var_right = "housing_tenant_2016",
    df = "city_CSD"
  )
  expect_equal(
    legend_breaks(vars, df = "city_CSD"),
    list(
      x = c("25.91%", "60.37%", "69.61%", "73.35%"),
      y = c(
        "1.42",
        "3.13", "3.40", "3.81"
      )
    )
  )
})

test_that("legend_breaks.delta_bivar works", {
  vars <- vars_build(
    var_left = c("housing_rent_2006", "housing_rent_2016"),
    var_right = c(
      "housing_tenant_2006",
      "housing_tenant_2016"
    ),
    df = "city_CSD"
  )
  data <- data_get(
    vars = vars,
    df = "city_CSD"
  )
  expect_equal(
    legend_breaks(vars, data = data),
    list(
      x = c("-8.27%", "-4.75%", "-1.42%", "4.11%"),
      y = c(
        "20.93%",
        "26.08%", "31.48%", "38.09%"
      )
    )
  )
})
