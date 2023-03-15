test_that("explore_text works with q5 pct", {
  vars <- vars_build("housing_tenant_2021", df = "CMA_CSD")
  actual <- explore_text(vars, region = "CMA", select_id = NA)
  expected <- "<p>In the Montreal region, 835,700 households (45.5%) are occupied by tenants. <i>(Data from 2021.)</i>"
  expect_equal(actual, expected)
})

test_that("explore_text works with q5 dollar", {
  vars <- vars_build("housing_rent_2021", df = "CMA_CSD")
  actual <- explore_text(vars, region = "CMA", select_id = NA)
  expected <- "<p>In the Montreal region, the average rent is $979. <i>(Data from 2021.)</i>"
  expect_equal(actual, expected)
})

test_that("explore_text works with q5 ind", {
  vars <- vars_build("climate_drought_2017", df = "grid_grid")
  actual <- explore_text(vars, region = "island", select_id = NA)
  expected <- "<p>On the island of Montreal, 280,100 households (30.8%) are living in areas with `elevated` to `major` vulnerability to climate-change related drought⁠. <i>(Data from 2017.)</i>"
  expect_equal(actual, expected)
})


