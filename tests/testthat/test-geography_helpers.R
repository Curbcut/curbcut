test_that("zoom_level_selection returns the right scale combinations", {

  # On unavailable bivars
  vars <- vars_build("climate_drought", "housing_tenant",
                     scale = "grd250", time = 2022)$vars
  zoom_level_selection(vars,
                       top_scale = "grd250",
                       avail_scale_combinations = c("grd250", "grd250_grd100_grd50"),
                       scales_as_DA = c("building", "street"))

  # On q5 (default method)
  vars <- vars_build("climate_drought", " ", scale = "grd250", time = 2022)$vars
  out <- zoom_level_selection(vars,
                       top_scale = "grd250",
                       avail_scale_combinations = c("grd250", "grd250_grd100_grd50"),
                       scales_as_DA = c("building", "street"))

  expect_equal(out, "grd250_grd100_grd50")

})

test_that("longest_scale_combination returns the right scale combinations", {

  # On unavailable bivars
  out <- longest_scale_combination(
    top_scale = "borough",
    avail_scale_combinations = c("borough_CT", "borough_CT_DA_building"))

  expect_equal(out, "borough_CT_DA_building")

})
