test_that("explore_text works with q5 pct", {
  vars <- vars_build("housing_tenant_2021", df = "city_CSD")
  data <- data_get(vars, df = "city_CSD")
  actual <- explore_text(vars, region = "CMA", df = "CMA_CSD", select_id = NA, data = data)
  expected <- "<p>In the Montreal region, 835,700 households (45.5%) are occupied by tenants. <i>(Data from 2021.)</i>"
  expect_equal(actual, expected)

  df <- "city_building"
  vars <- vars_build("housing_tenant_2021", df = df)
  select_id <- "b10106060"
  data <- data_get(vars, df)
  actual <- explore_text(vars,
    region = "CMA", df = df, data = data,
    select_id = select_id
  )
  expect_equal(class(actual), "character")
  expect_equal(length(actual), 1)
})

test_that("explore_text works with q5 dollar", {
  vars <- vars_build("housing_rent_2021", df = "city_CSD")
  data <- data_get(vars, df = "city_CSD")
  actual <- explore_text(vars,
    region = "CMA", select_id = NA, df = "city_CSD",
    data = data
  )
  expected <- "<p>In the Montreal region, the average rent is $979. <i>(Data from 2021.)</i>"
  expect_equal(actual, expected)

  df <- "city_building"
  vars <- vars_build("housing_rent_2021", df = df)
  select_id <- "b10106060"
  data <- data_get(vars, df)
  actual <- explore_text(vars,
    region = "CMA", df = df, data = data,
    select_id = select_id
  )
  expect_equal(class(actual), "character")
  expect_equal(length(actual), 1)
})

test_that("explore_text works with q5 ind ordinal", {
  vars <- vars_build("climate_drought_2015", df = "grid_grid50")
  data <- data_get(vars, df = "grid_grid50")
  actual <- explore_text(vars,
    region = "grid", select_id = NA, df = "grid_grid50",
    data = data
  )
  expect_equal(class(actual), "character")
  expect_equal(length(actual), 1)
})

test_that("explore_text works with q5 ind scalar", {
  vars <- vars_build("canale_2021", df = "city_CSD")
  data <- data_get(vars, df = "city_CSD")
  actual <- explore_text(vars,
    region = "city", select_id = NA, df = "CMA_CSD",
    data = data
  )
  expect_equal(class(actual), "character")
  expect_equal(length(actual), 1)
})
