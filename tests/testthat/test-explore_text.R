test_explores_helper <- function(var_left, var_right, region, scale, time, select_id, schemas = NULL) {
  vars <- vars_build(var_left, var_right = var_right, scale = scale, time = time)
  time_ <- vars$time
  vars <- vars$vars
  data <- data_get(vars, region = region, scale = scale, time = time_)
  actual <- explore_text(vars,
    region = region, select_id = select_id, scale = scale,
    data = data, time = time_, zoom_levels = mzl_borough_CT_DA_building,
    schemas = schemas
  )

  expect_false(grepl("__", actual)) # missing schemas? __ detected in the output
  expect_equal(class(actual), "character")
  expect_equal(length(actual), 1)
  # print(actual)
}

test_explores <- function(var_right, select_id, region, scale) {
  # Pct
  test_explores_helper(
    var_left = "housing_tenant",
    var_right = var_right, region = region, scale = scale,
    select_id = select_id, time = 2021
  )

  # Dollar
  test_explores_helper(
    var_left = "housing_rent",
    var_right = var_right, region = region, scale = scale,
    select_id = select_id, time = 2021
  )

  # Ind scalar
  test_explores_helper(
    var_left = "alp",
    var_right = var_right, region = region, scale = scale,
    select_id = select_id, time = 2021
  )

  # Count
  test_explores_helper(
    var_left = "crash_count_ped",
    var_right = var_right, region = region, scale = scale,
    select_id = select_id, time = 2021
  )

  # Ind ordinal
  test_explores_helper(
    var_left = "climate_drought",
    var_right = var_right, region = region, scale = scale,
    select_id = select_id, time = 2021
  )

  # Ind ordinal, multiple schemas
  test_explores_helper(
    var_left = "access_foot_food_grocery",
    var_right = var_right,
    region = region, scale = "DA", select_id = select_id,
    time = 2023, schemas = list(var_left = stats::setNames(20, "transportationtime"))
  )

  # sqkm
  test_explores_helper(
    var_left = "alley_sqkm",
    var_right = var_right, region = "city", scale = "CT",
    select_id = if (is.na(select_id)) NA else "4620225.00", time = 2023
  )

  # per1k
  test_explores_helper(
    var_left = "alley_per1k",
    var_right = var_right, region = "city", scale = "DA",
    select_id = if (is.na(select_id)) NA else "24662027", time = 2023
  )
}


# q5 ----------------------------------------------------------------------


test_that("q5 explore works without a selection", {
  test_explores(var_right = " ", select_id = NA, region = "CMA", scale = "CSD")
  test_explores(var_right = " ", select_id = NA, region = "city", scale = "building")

  # Ind ordinal (using SQLite)
  test_explores_helper(
    var_left = "climate_drought",
    var_right = " ", region = "island", scale = "grd100",
    select_id = "grd100_10200", time = 2022
  )
})

test_that("q5 explore works with selections", {
  test_explores(var_right = " ", select_id = "borough_2", region = "city", scale = "borough")
  test_explores(var_right = " ", select_id = "2466092", region = "CMA", scale = "CSD")
  test_explores(var_right = " ", select_id = "b10000763", region = "city", scale = "building")
})


# bivar -------------------------------------------------------------------

test_that("q5 explore works without a selection", {
  test_explores(var_right = "alp", select_id = NA, region = "city", scale = "boroughCSD")
  # test_explores(var_right = "climate_drought", select_id = NA, region = "city", scale = "building")
})

test_that("q5 explore works with selections", {
  test_explores(var_right = "housing_tenant", select_id = "2466023_19", region = "city", scale = "boroughCSD")
  test_explores(var_right = "housing_rent", select_id = "b10000763", region = "city", scale = "building")
  test_explores(var_right = "housing_rent", select_id = "24520109", region = "city", scale = "DA")
})


# delta -------------------------------------------------------------------

test_explores_delta <- function(var_right, select_id, scale, region) {
  # Pct
  test_explores_helper(
    var_left = "housing_tenant", time = c(2016, 2021),
    var_right = var_right, scale = scale, region = region,
    select_id = select_id
  )

  # Dollar
  test_explores_helper(
    var_left = "housing_rent", time = c(2016, 2021),
    var_right = var_right, scale = scale, region = region,
    select_id = select_id
  )

  # # Ind scalar
  # test_explores_helper("access_foot_20_food_grocery_2023", var_right = var_right, df = "city_DA",
  # select_id = select_id)

  # Ind scalar
  test_explores_helper(
    var_left = "alp", time = c(2016, 2021),
    var_right = var_right, scale = scale, region = region,
    select_id = select_id
  )

  # Count
  test_explores_helper(
    var_left = "crash_count_ped",
    var_right = var_right, region = region, scale = scale,
    select_id = select_id, time = c(2015, 2020)
  )

  # Ind ordinal
  test_explores_helper(
    var_left = "climate_drought",
    var_right = var_right, region = region, scale = scale,
    select_id = select_id, time = c(2015, 2022)
  )
}

test_that("q5 explore works without a selection", {
  test_explores_delta(var_right = " ", select_id = NA, region = "CMA", scale = "CSD")
  test_explores_delta(var_right = " ", select_id = NA, region = "city", scale = "building")
})

test_that("q5 explore works with selections", {
  test_explores_delta(var_right = " ", select_id = "2471095", scale = "CSD", region = "CMA")
  test_explores_delta(var_right = " ", select_id = "borough_5", region = "city", scale = "borough")
  test_explores_delta(var_right = " ", select_id = "b10000763", region = "city", scale = "building")
})


# delta bivar -------------------------------------------------------------

test_that("delta explore works without a selection", {
  test_explores_delta(var_right = "inc_50", select_id = NA, scale = "CSD", region = "CMA")
  test_explores_delta(var_right = "alp", select_id = NA, scale = "building", region = "city")
})

test_that("delta explore works with selections", {
  test_explores_delta(var_right = "lst", select_id = "2466023", scale = "CSD", region = "CMA")
  test_explores_delta(var_right = "iden_imm", select_id = "2471095", scale = "CSD", region = "CMA")
  test_explores_delta(var_right = "inc_limat", select_id = "b10000763", scale = "building", region = "city")
})
