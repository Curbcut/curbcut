test_that("map_scale_fill works", {
  data_colours <- data_get_colours(
    vars = vars_build(
      var_left = "housing_tenant_2016",
      df = "city_CSD"
    ),
    region = "city",
    zoom_levels = map_zoom_levels_city
  )
  expect_equal(
    names(map_scale_fill(data_colours)),
    c(
      "scale_type", "trans", "legend", "col", "get_palette", "unmapped_color",
      "levels", "tick_format", "col_label"
    )
  )
})

test_that("map_scale_colour works", {
  data_colours <- data_get_colours(
    vars = vars_build(
      var_left = "housing_tenant_2016",
      df = "city_CSD"
    ),
    region = "city",
    zoom_levels = map_zoom_levels_city
  )
  actual <- map_scale_colour("1234", data_colours)

  expect_equal(
    names(actual),
    c("scale_type", "trans", "legend", "col", "get_palette", "unmapped_color",
      "levels", "tick_format", "col_label")
  )
})

test_that("map_scale_lwd works", {
  data_colours <- data_get_colours(
    vars = vars_build(
      var_left = "housing_tenant_2016",
      df = "city_CSD"
    ),
    region = "city",
    zoom_levels = map_zoom_levels_city
  )
  z <- map_scale_lwd(select_id = "24660001")
  expect_equal(
    names(map_scale_lwd(select_id = "24660001")),
    c(
      "scale_type", "trans", "legend", "col", "get_range", "unmapped_value",
      "levels", "col_label"
    )
  )
})
