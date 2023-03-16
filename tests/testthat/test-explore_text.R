test_that("explore_text works with q5 pct", {
  vars <- vars_build("housing_tenant_2021", df = "CMA_CSD")
  actual <- explore_text(vars, region = "CMA", df = "CMA_CSD", select_id = NA)
  expected <- "<p>In the Montreal region, 835,700 households (45.5%) are occupied by tenants. <i>(Data from 2021.)</i>"
  expect_equal(actual, expected)

  df <- "city_building"
  vars <- vars_build("housing_tenant_2021", df = df)
  select_id <- "b10106060"
  data <- data_get(vars, df)
  actual <- explore_text(vars, region = "CMA", df = df, data = data,
                         select_id = select_id)
  expected <- "<p><b>16017 Rue Caroline-Racicot, Montréal</b><p>In the dissemination area around 16017 Rue Caroline-Racicot, Montréal, 40 households (20.5%) are occupied by tenants.<p>This is exceptionally low for the Montreal region. The percentage of private households occupied by tenants in 16017 Rue Caroline-Racicot, Montréal is higher than in 10% of other dissemination areas in the region. <i>(Data from 2021.)</i>"
  expect_equal(actual, expected)
})

test_that("explore_text works with q5 dollar", {
  vars <- vars_build("housing_rent_2021", df = "CMA_CSD")
  actual <- explore_text(vars, region = "CMA", select_id = NA)
  expected <- "<p>In the Montreal region, the average rent is $979. <i>(Data from 2021.)</i>"
  expect_equal(actual, expected)

  df <- "city_building"
  vars <- vars_build("housing_rent_2021", df = df)
  select_id <- "b10106060"
  data <- data_get(vars, df)
  actual <- explore_text(vars, region = "CMA", df = df, data = data,
                         select_id = select_id)
  expected <- "<p><b>16017 Rue Caroline-Racicot, Montréal</b><p>In the dissemination area around 16017 Rue Caroline-Racicot, Montréal, the average rent is $840.<p>This is unusually inexpensive for the Montreal region. The average rent paid by tenants per month in 16017 Rue Caroline-Racicot, Montréal is higher than in 27% of other dissemination areas in the region. <i>(Data from 2021.)</i>"
  expect_equal(actual, expected)
})

test_that("explore_text works with q5 ind", {
  vars <- vars_build("climate_drought_2017", df = "grid_grid")
  actual <- explore_text(vars, region = "island", select_id = NA)
  expected <- "<p>On the island of Montreal, 280,100 households (30.8%) are living in areas with `elevated` to `major` vulnerability to climate-change related drought⁠. <i>(Data from 2017.)</i>"
  expect_equal(actual, expected)
})


