test_explore_graph_helper <- function(var_left, var_right, scale, region, time, select_id) {
  vars <- vars_build(var_left, var_right = var_right, scale = scale, time = time)
  time <- vars$time
  vars <- vars$vars
  data <- data_get(vars, scale = scale, region = region, time = time)
  actual <- explore_graph(vars,
    region = region, select_id = select_id, scale = scale,
    data = data, time = time
  )

  # If there is a selection, the selection is within range
  if (!is.na(select_id)) {
    # q5
    if ("q5" %in% class(vars)) {
      x_range <-
        ggplot2::ggplot_build(actual)$layout$panel_scales_x[[1]]$range$range

      # The value is in x_range
      dat_col <- match_schema_to_col(data = data, time = time)
      val <- data[[dat_col]][data$ID == select_id]

      # The value is within range of the plot
      if (length(val) > 0) {
        expect_true(val >= x_range[1] & val <= x_range[2])
      }

    } else if ("bivar" %in% class(vars)) {

    }

  }

  expect_true(is.data.frame(actual$data))
  expect_true(is.list(actual$labels))
}

test_explores <- function(var_right, select_id, scale, region) {
  # Pct
  test_explore_graph_helper(
    var_left = "housing_tenant",
    var_right = var_right, scale = scale, region = region,
    time = 2021, select_id = select_id
  )
  test_explore_graph_helper(
    var_left = "housing_tenant",
    var_right = var_right, scale = scale, region = region,
    time = 1996, select_id = select_id
  )

  # Dollar
  test_explore_graph_helper(
    var_left = "housing_rent",
    var_right = var_right, scale = scale, region = region,
    time = 2021, select_id = select_id
  )

  # # Ind scalar
  # test_explore_graph_helper("access_foot_food_grocery_20",
  #   var_right = var_right, scale = "DA", region = "city",
  #   select_id = select_id
  # )

  # Ind scalar
  test_explore_graph_helper(
    var_left = "alp",
    var_right = var_right, scale = scale, region = region,
    time = 2021, select_id = select_id
  )

  # # sqkm
  # test_explore_graph_helper(
  #   "alley_sqkm",
  #   var_right = var_right, scale = scale, region = "city",
  #   time = 2023, select_id = select_id
  # )
  #
  # # per1k
  # test_explore_graph_helper(
  #   "alley_per1k",
  #   var_right = var_right, scale = scale, region = "city",
  #   time = 2023, select_id = select_id
  # )
}


# q5 ----------------------------------------------------------------------


test_that("q5 explore works without a selection", {
  test_explores(var_right = " ", select_id = NA, scale = "CSD", region = "CMA")
  test_explores(var_right = " ", select_id = NA, scale = "building", region = "city")
})

test_that("q5 explore works with selections", {
  test_explores(var_right = " ", select_id = "2466023", scale = "CSD", region = "CMA")
  test_explores(var_right = " ", select_id = "b10000763", scale = "building", region = "city")
})

# test_that("q5_ind ordinal", {
#   var_left <- "climate_drought_2022"
#   df <- "grid_grid250"
#   var_right <- " "
#   vars <- vars_build(var_left, var_right = var_right, df = df)
#   data <- data_get(vars, df = df)
#   actual <- explore_graph(vars,
#     region = "grid", select_id = NA, df = df,
#     data = data
#   )
#   expect_true(is.data.frame(actual$data))
#   expect_true(is.list(actual$labels))
# })


# bivar -------------------------------------------------------------------

test_that("q5 explore works without a selection", {
  test_explores(var_right = "alp", select_id = NA, scale = "CSD", region = "CMA")
  test_explores(var_right = "inc_median_income", select_id = NA, scale = "CSD", region = "CMA")

  # test_explores(var_right = "climate_drought", select_id = NA, scale = "building", region = "city")
})

test_that("q5 explore works with selections", {
  test_explores(var_right = "housing_tenant", select_id = "2466023", scale = "CSD", region = "CMA")
  test_explores(var_right = "housing_rent", select_id = "b10000763", scale = "building", region = "city")
})

# test_that("bivar_ind ordinal", {
#   var_left <- "climate_drought_2022"
#   df <- "grid_grid250"
#   var_right <- "housing_tenant_2021"
#   vars <- vars_build(var_left, var_right = var_right, df = df)
#   data <- data_get(vars, df = df)
#   actual <- explore_graph(vars,
#     region = "grid", select_id = NA, df = df,
#     data = data
#   )
#   expect_true(is.data.frame(actual$data))
#   expect_true(is.list(actual$labels))
# })


# delta -------------------------------------------------------------------

test_explores_delta <- function(var_right, select_id, scale, region) {
  # Pct
  test_explore_graph_helper(
    var_left = "housing_tenant", time = c(1996, 2021),
    var_right = var_right,
    scale = scale, region = region, select_id = select_id
  )

  # Dollar
  test_explore_graph_helper("housing_rent", time = c(2016, 2021),
    var_right = var_right,
    scale = scale, region = region, select_id = select_id
  )

  # # Ind scalar
  # test_explore_graph_helper("access_foot_20_food_grocery_2023", var_right = var_right, df = "city_DA",
  # select_id = select_id)

  # Ind scalar
  test_explore_graph_helper(var_left = "alp", time = c(2016, 2021),
    var_right = var_right, scale = scale, region = region,
    select_id = select_id
  )
}

test_that("delta explore works without a selection", {
  test_explores_delta(var_right = " ", select_id = NA, scale = "CSD", region = "CMA")
  test_explores_delta(var_right = " ", select_id = NA, scale = "building", region = "city")
})

test_that("delta explore works with selections", {
  test_explores_delta(var_right = " ", select_id = "2466023", scale = "CSD", region = "CMA")
  test_explores_delta(var_right = " ", select_id = "b10000763", scale = "building", region = "city")
})

# test_that("delta_ind ordinal", {
#   var_left <- c("climate_drought_2015", "climate_drought_2022")
#   df <- "grid_grid250"
#   var_right <- " "
#   vars <- vars_build(var_left, var_right = var_right, df = df)
#   data <- data_get(vars, df = df)
#   actual <- explore_graph(vars,
#     region = "grid", select_id = NA, df = df,
#     data = data
#   )
#   expect_true(is.data.frame(actual$data))
#   expect_true(is.list(actual$labels))
# })


# delta bivar -------------------------------------------------------------

test_that("delta explore works without a selection", {
  test_explores_delta(var_right = "inc_50", select_id = NA, scale = "CSD", region = "CMA")
  test_explores_delta(var_right = "alp", select_id = NA, scale = "building", region = "city")
})

test_that("delta explore works with selections", {
  test_explores_delta(var_right = "lst", select_id = "2466023", scale = "CSD", region = "CMA")
  test_explores_delta(var_right = "inc_limat", select_id = "b10000763", scale = "building", region = "city")
})













