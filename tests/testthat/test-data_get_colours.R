test_that("data_get_colours.q5  works", {
  vars <- vars_build(var_left = "housing_tenant_2016", df = "city_CSD")
  data_colours <- data_get_colours(
    vars = vars, region = "city",
    zoom_levels = map_zoom_levels_city
  )
  expect_equal(nrow(data_colours), 3335)
  expect_equal(unique(nchar(data_colours$fill)), 7)
  expect_equal(names(data_colours), c("ID", "fill"))
})

test_that("data_get_colours.bivar works", {
  vars <- vars_build(
    var_left = "housing_tenant_2016",
    var_right = "alp_2016",
    df = "city_CSD"
  )
  data_colours <- data_get_colours(
    vars = vars, region = "city",
    zoom_levels = map_zoom_levels_city
  )
  expect_equal(nrow(data_colours), 3335)
  expect_equal(unique(nchar(data_colours$fill)), 7)
  expect_equal(names(data_colours), c("ID", "fill"))
})

test_that("data_get_colours.delta works", {
  vars <- vars_build(
    var_left = c("housing_tenant_1996", "housing_tenant_2016"),
    var_right = c(" "),
    df = "city_CSD"
  )
  data_colours <- data_get_colours(
    vars = vars, region = "city",
    zoom_levels = map_zoom_levels_city
  )
  expect_equal(nrow(data_colours), 3335)
  expect_equal(unique(nchar(data_colours$fill)), 7)
  expect_equal(names(data_colours), c("ID", "fill"))
})

test_that("data_get_colours.delta_bivar works", {
  vars <- vars_build(
    var_left = c("housing_tenant_1996", "housing_tenant_2016"),
    var_right = c("housing_rent_1996", "housing_rent_2016"),
    df = "city_CSD"
  )
  data_colours <- data_get_colours(
    vars = vars, region = "city",
    zoom_levels = map_zoom_levels_city
  )
  expect_equal(nrow(data_colours), 3335)
  expect_equal(unique(nchar(data_colours$fill)), 7)
  expect_equal(names(data_colours), c("ID", "fill"))
})

test_that("data_get_colours.bivar_ldelta_rq3 works", {
  vars <- vars_build(
    var_left = c("housing_tenant_1996", "housing_tenant_2016"),
    var_right = c("alp_2016"),
    df = "city_CSD"
  )
  data_colours <- data_get_colours(
    vars = vars, region = "city",
    zoom_levels = map_zoom_levels_city
  )
  expect_equal(nrow(data_colours), 3335)
  expect_equal(unique(nchar(data_colours$fill)), 7)
  expect_equal(names(data_colours), c("ID", "fill"))
})
