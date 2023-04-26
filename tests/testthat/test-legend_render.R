test_that("legend_breaks.q5 (without `chr_breaks` attribute) works", {
  vars <- vars_build(
    var_left = "housing_tenant_2016",
    var_right = " ", df = "CMA_DA"
  )
  p <- legend_render(vars, df = "CMA_DA", font_family = NULL)
  expect_error(print(p), NA)
})

test_that("legend_breaks.q5_ind works", {
  vars <- vars_build(
    var_left = "climate_drought_2015",
    var_right = " ", df = "grid_grid50"
  )
  p1 <- legend_render(vars, df = "grid_grid50", font_family = NULL)
  expect_error(print(p1), NA)

  vars <- vars_build(
    var_left = "canale",
    var_right = " ", df = "CMA_CSD"
  )
  p2 <- legend_render(vars, df = "CMA_CSD", font_family = NULL)
  expect_error(print(p2), NA)
})


# test_that("legend_render.qual works", {
#   vars <- vars_build(var_left = "",
#                      var_right = " ", df = "")
#   p <- legend_render(vars)
#   expect_error(print(p), NA)
# })

test_that("legend_render.bivar  works", {
  vars <- vars_build(
    var_left = "housing_tenant_2016",
    var_right = "inc_limat_2016", df = "CMA_CSD"
  )
  p <- legend_render(vars, df = "CMA_CSD", font_family = NULL)
  expect_error(print(p), NA)
})

test_that("legend_render.delta  works", {
  vars <- vars_build(
    var_left = c("housing_tenant_2006", "housing_tenant_2016"),
    var_right = " ", df = "CMA_CSD"
  )
  p <- legend_render(vars, df = "CMA_CSD", font_family = NULL)
  expect_error(p, NA)
})

# test_that("legend_render.q100  works", {
#   vars <- vars_build(
#     var_left = "c_flood",
#     var_right = " ", df = "raster"
#   )
#   p <- legend_render(vars, df = "raster", font_family = NULL)
#   expect_error(print(p), NA)
# })

test_that("legend_render.delta_bivar  works", {
  vars <- vars_build(
    var_left = c("housing_tenant_2006", "housing_tenant_2016"),
    var_right = c("inc_50_2006", "inc_50_2016"), df = "city_CSD"
  )
  data <- data_get(vars, df = "city_CSD")
  p <- legend_render(vars, df = "city_CSD", data = data, font_family = NULL)
  expect_error(p, NA)
})

test_that("legend_render.bivar_ldelta_rq3  works", {
  vars <- vars_build(
    var_left = c("housing_tenant_2006", "housing_tenant_2016"),
    var_right = "inc_limat_2016", df = "city_CSD"
  )
  data <- data_get(vars, df = "city_CSD")
  p <- legend_render(vars, df = "city_CSD", data = data, font_family = NULL)
  expect_error(p, NA)
})
