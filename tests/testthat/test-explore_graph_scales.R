
lefts <- c("housing_tenant_2021", "housing_rent_2021",
           "access_foot_20_food_grocery_2023", "canale_2021", "alley_sqkm_2023",
           "alley_per1k_2023", "climate_drought_2022")
vars_b <- sapply(lefts, vars_build, df = "city_DA", simplify = FALSE,
                 USE.NAMES = TRUE)
vars_b <- lapply(vars_b, `[[`, "var_left")

test_that("explore_graph_scale works", {
  cl <- explore_graph_scale(var = vars_b$housing_tenant_2021, x_y = "x")[[1]]
  cl <- class(cl)
  expect_true("ScaleContinuousPosition" %in% cl)

  data <- data_get(vars_build("housing_rent_2021", df = "city_DA"), df = "city_DA")
  cl <- explore_graph_scale(var = vars_b$housing_rent_2021, x_y = "x",
                            data_vals = data$var_left, df = "city_DA")[[1]]
  cl <- class(cl)
  expect_true("ScaleContinuousPosition" %in% cl)

  cl <- explore_graph_scale(var = vars_b$access_foot_20_food_grocery_2023, x_y = "x")[[1]]
  cl <- class(cl)
  expect_true("ScaleContinuousPosition" %in% cl)

  data <- data_get(vars_build("canale_2021", df = "city_DA"), df = "city_DA")
  cl <- explore_graph_scale(var = vars_b$canale_2021, x_y = "x",
                            data_vals = data$var_left, df = "city_DA")[[1]]
  cl <- class(cl)
  expect_true("ScaleContinuousPosition" %in% cl)

  cl <- explore_graph_scale(var = vars_b$alley_sqkm_2023, x_y = "x")[[1]]
  cl <- class(cl)
  expect_true("ScaleContinuousPosition" %in% cl)

  cldr <- vars_build("climate_drought_2022", df = "grid_grid250")
  cl <- explore_graph_scale(var = cldr$var_left, x_y = "x", df = "grid_grid250")[[1]]
  cl <- class(cl)
  expect_true("ScaleDiscretePosition" %in% cl)
})
