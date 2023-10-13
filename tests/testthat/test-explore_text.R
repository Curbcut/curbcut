test_explores_helper <- function(var_left, var_right, region, scale, time, select_id) {
  vars <- vars_build(var_left, var_right = var_right, scale = scale, time = time)
  time <- vars$time
  vars <- vars$vars
  data <- data_get(vars, region = region, scale = scale, data_path = .curbcut_montreal_data)
  actual <- explore_text(vars,
    region = region, select_id = select_id, scale = scale,
    data = data, time = time
  )
  expect_equal(class(actual), "character")
  expect_equal(length(actual), 1)
}

test_explores <- function(var_right, select_id, region, scale) {
  # Pct
  test_explores_helper(var_left = "housing_tenant",
    var_right = var_right, region = region, scale = scale,
    select_id = select_id, time = 2021
  )

  # Dollar
  test_explores_helper(var_left = "housing_rent",
    var_right = var_right, region = region, scale = scale,
    select_id = select_id, time = 2021
  )

  # Ind ordinal
  test_explores_helper(var_left = "access_foot_20_food_grocery",
    var_right = var_right,
    region = region, scale = "DA", select_id = select_id, time = 2023
  )

  # Ind scalar
  test_explores_helper(var_left = "alp",
    var_right = var_right, region = region, scale = scale,
    select_id = select_id, time = 2021
  )

  # sqkm
  test_explores_helper(var_left = "alley_sqkm",
    var_right = var_right, region = "city", scale = scale,
    select_id = select_id, time = 2023
  )

  # per1k
  test_explores_helper(var_left = "alley_per1k",
    var_right = var_right, region = "city", scale = scale,
    select_id = select_id, time = 2023
  )
}


# q5 ----------------------------------------------------------------------


test_that("q5 explore works without a selection", {
  test_explores(var_right = " ", select_id = NA, region = "CMA", scale = "CSD")
  test_explores(var_right = " ", select_id = NA, region = "city", scale = "building")
})

test_that("q5 explore works with selections", {
  test_explores(var_right = " ", select_id = "2466023_19", region = "city", scale = "CSD")
  test_explores(var_right = " ", select_id = "b10000763", region = "city", scale = "building")
})


# bivar -------------------------------------------------------------------

test_that("q5 explore works without a selection", {
  test_explores(var_right = "alp", select_id = NA, region = "city", scale = "CSD")
  test_explores(var_right = "climate_drought", select_id = NA, region = "city", scale = "building")
})

test_that("q5 explore works with selections", {
  test_explores(var_right = "housing_tenant", select_id = "2466023_19", region = "city", scale = "CSD")
  test_explores(var_right = "housing_rent", select_id = "b10000763", region = "city", scale = "building")
})


# delta -------------------------------------------------------------------

test_explores_delta <- function(var_right, select_id, df) {
  # Pct
  test_explores_helper(paste0("housing_tenant_", c(2016, 2021)),
    var_right = var_right,
    df = df, select_id = select_id
  )

  # Dollar
  test_explores_helper(paste0("housing_rent_", c(2016, 2021)),
    var_right = var_right,
    df = df, select_id = select_id
  )

  # # Ind scalar
  # test_explores_helper("access_foot_20_food_grocery_2023", var_right = var_right, df = "city_DA",
  # select_id = select_id)

  # Ind scalar
  test_explores_helper(paste0("alp_", c(2016, 2021)),
    var_right = var_right, df = df,
    select_id = select_id
  )
}

test_that("q5 explore works without a selection", {
  test_explores_delta(var_right = " ", select_id = NA, df = "city_CSD")
  test_explores_delta(var_right = " ", select_id = NA, df = "city_building")
})

test_that("q5 explore works with selections", {
  test_explores_delta(var_right = " ", select_id = "2466023_19", df = "city_CSD")
  test_explores_delta(var_right = " ", select_id = "b10000763", df = "city_building")
})


# delta bivar -------------------------------------------------------------

test_explores_delta <- function(var_right, select_id, df) {
  # Pct
  test_explores_helper(paste0("housing_tenant_", c(2016, 2021)),
    var_right = var_right,
    df = df, select_id = select_id
  )

  # Dollar
  test_explores_helper(paste0("housing_rent_", c(2016, 2021)),
    var_right = var_right,
    df = df, select_id = select_id
  )

  # # Ind scalar TKTK NO SCALAR YET WITH 2 YEARS
  # test_explores_helper("access_foot_20_food_grocery_2023", var_right = var_right,
  # df = "city_DA", select_id = select_id)

  # Ind scalar
  test_explores_helper(paste0("alp_", c(2016, 2021)),
    var_right = var_right,
    df = df, select_id = select_id
  )
}

test_that("q5 explore works without a selection", {
  test_explores_delta(var_right = paste0("alp_", c(2016, 2021)), select_id = NA, df = "city_CSD")
  test_explores_delta(var_right = paste0("climate_drought_", c(2015, 2022)), select_id = NA, df = "city_building")
})

test_that("q5 explore works with selections", {
  test_explores_delta(var_right = paste0("housing_tenant_", c(2016, 2021)), select_id = "2466023_19", df = "city_CSD")
  test_explores_delta(var_right = paste0("housing_rent_", c(2016, 2021)), select_id = "b10000763", df = "city_building")
})
