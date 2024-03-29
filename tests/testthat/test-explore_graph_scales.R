lefts <- c(
  "housing_tenant",
  "housing_rent",
  "alp",
  "access_foot_food_grocery",
  "alley_sqkm",
  "alley_per1k",
  "climate_drought"
)
vars_b <- sapply(lefts, \(vl) {
  time <- variables$dates[variables$var_code == vl][[1]]
  time <- time[length(time)]
  vars_build(vl, time = time, scale = "DA")$vars
}, simplify = FALSE, USE.NAMES = TRUE)

vars_b <- lapply(vars_b, `[[`, "var_left")

test_that("explore_graph_scale works", {
  cl <- explore_graph_scale(var = vars_b$housing_tenant, x_y = "x")[[1]]
  cl <- class(cl)
  expect_true("ScaleContinuousPosition" %in% cl)

  data <- data_get(vars_build("housing_rent", scale = "DA", time = 2021, )$vars,
    scale = "DA", region = "CMA"
  )
  cl <- explore_graph_scale(
    var = vars_b$housing_rent, x_y = "x",
    data_vals = data$var_left_2021, scale = "DA", region = "CMA"
  )[[1]]
  cl <- class(cl)
  expect_true("ScaleContinuousPosition" %in% cl)

  data <- data_get(vars_build("alp", scale = "DA", time = 2021)$vars,
    scale = "DA", region = "CMA"
  )
  cl <- explore_graph_scale(
    var = vars_b$alp, x_y = "x",
    data_vals = data$var_left_2021, scale = "DA", region = "CMA"
  )[[1]]
  cl <- class(cl)
  expect_true("ScaleContinuousPosition" %in% cl)


  cl <- explore_graph_scale(var = vars_b$access_foot_food_grocery, x_y = "x")[[1]]
  cl <- class(cl)
  expect_true("ScaleContinuousPosition" %in% cl)

  cl <- explore_graph_scale(var = vars_b$alley_sqkm_2023, x_y = "x")[[1]]
  cl <- class(cl)
  expect_true("ScaleContinuousPosition" %in% cl)

  cldr <- vars_build("climate_drought", scale = "grd250", time = 2022)
  time <- cldr$time
  v <- cldr$vars
  data <- data_get(vars = v, scale = "grd250")
  cl <- explore_graph_scale(
    var = v$var_left, x_y = "x", scale = "grd250",
    data_vals = data$var_left_2022, breaks = 0:5
  )[[1]]
  cl <- class(cl)
  expect_true("ScaleDiscretePosition" %in% cl)
})
