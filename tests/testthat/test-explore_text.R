test_explores_helper <- function(var_left, var_right, df, select_id) {
  vars <- vars_build(var_left, var_right = var_right, df = df)
  data <- data_get(vars, df = df)
  actual <- explore_text(vars,
    region = "city", select_id = select_id, df = df,
    data = data
  )
  expect_equal(class(actual), "character")
  expect_equal(length(actual), 1)
}

test_explores <- function(var_right, select_id, df) {
  # Pct
  test_explores_helper("housing_tenant_2021",
    var_right = var_right, df = df,
    select_id = select_id
  )

  # Dollar
  test_explores_helper("housing_rent_2021",
    var_right = var_right, df = df,
    select_id = select_id
  )

  # Ind scalar
  test_explores_helper("access_foot_20_food_grocery_2023",
    var_right = var_right,
    df = "city_DA", select_id = select_id
  )

  # Ind scalar
  test_explores_helper("canale_2021",
    var_right = var_right, df = df,
    select_id = select_id
  )

  # sqkm
  test_explores_helper("alley_sqkm_2023",
    var_right = var_right, df = df,
    select_id = select_id
  )

  # per1k
  test_explores_helper("alley_per1k_2023",
    var_right = var_right, df = df,
    select_id = select_id
  )
}


# q5 ----------------------------------------------------------------------


test_that("q5 explore works without a selection", {
  test_explores(var_right = " ", select_id = NA, df = "city_CSD")
  test_explores(var_right = " ", select_id = NA, df = "city_building")
})

test_that("q5 explore works with selections", {
  test_explores(var_right = " ", select_id = "2466023_19", df = "city_CSD")
  test_explores(var_right = " ", select_id = "b10000763", df = "city_building")
})


# bivar -------------------------------------------------------------------

test_that("q5 explore works without a selection", {
  test_explores(var_right = "canale_2021", select_id = NA, df = "city_CSD")
  test_explores(var_right = "climate_drought_2015", select_id = NA, df = "city_building")
})

test_that("q5 explore works with selections", {
  test_explores(var_right = "housing_tenant_2021", select_id = "2466023_19", df = "city_CSD")
  test_explores(var_right = "housing_rent_2021", select_id = "b10000763", df = "city_building")
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
  test_explores_helper(paste0("canale_", c(2016, 2021)),
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
  test_explores_helper(paste0("canale_", c(2016, 2021)),
    var_right = var_right,
    df = df, select_id = select_id
  )
}

test_that("q5 explore works without a selection", {
  test_explores_delta(var_right = paste0("canale_", c(2016, 2021)), select_id = NA, df = "city_CSD")
  test_explores_delta(var_right = paste0("climate_drought_", c(2015, 2022)), select_id = NA, df = "city_building")
})

test_that("q5 explore works with selections", {
  test_explores_delta(var_right = paste0("housing_tenant_", c(2016, 2021)), select_id = "2466023_19", df = "city_CSD")
  test_explores_delta(var_right = paste0("housing_rent_", c(2016, 2021)), select_id = "b10000763", df = "city_building")
})
