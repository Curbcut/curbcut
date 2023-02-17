test_that("legend_breaks.q5 (without `chr_breaks` attribute) works", {
  vars <- build_vars(
    var_left = "housing_tenant_2016",
    var_right = " ", df = "CMA_DA"
  )
  p <- legend_render(vars, df = "CMA_DA", font_family = NULL)
  expect_error(print(p), NA)
})

test_that("legend_breaks.q5 (with `chr_breaks` attribute) works", {
  vars <- build_vars(
    var_left = "climate_drought_2016",
    var_right = " ", df = "grid_grid"
  )
  p <- legend_render(vars, df = "grid_grid", font_family = NULL)
  expect_error(print(p), NA)
})

# test_that("legend_render.qual works", {
#   vars <- build_vars(var_left = "",
#                      var_right = " ", df = "")
#   p <- legend_render(vars)
#   expect_error(print(p), NA)
# })

test_that("legend_render.bivar  works", {
  vars <- build_vars(
    var_left = "housing_tenant_2016",
    var_right = "inc_limat_2016", df = "CMA_CSD"
  )
  p <- legend_render(vars, df = "CMA_CSD", font_family = NULL)
  expect_error(print(p), NA)
})

test_that("legend_render.delta  works", {
  vars <- build_vars(
    var_left = c("housing_tenant_2006", "housing_tenant_2016"),
    var_right = " ", df = "CMA_CSD"
  )
  p <- legend_render(vars, font_family = NULL)
  expect_error(print(p), NA)
})

test_that("legend_render.q100  works", {
  vars <- build_vars(
    var_left = "c_flood",
    var_right = " ", df = "raster"
  )
  p <- legend_render(vars, font_family = NULL)
  expect_error(print(p), NA)
})

# test_that("legend_render.delta_bivar  works", {
#   vars <- build_vars(var_left = c("housing_tenant_2006", "housing_tenant_2016"),
#                      var_right = c("inc_limat_2006", "inc_limat_2016"), df = "CMA_DA")
#   p <- legend_render(vars)
#   expect_error(print(p), NA)
# })
#
# test_that("legend_render.bivar_ldelta_rq3  works", {
#   vars <- build_vars(var_left = c("housing_tenant_2006", "housing_tenant_2016"),
#                      var_right = "inc_limat_2006", df = "CMA_DA")
#   p <- legend_render(vars)
#   expect_error(print(p), NA)
# })

structure(
  list(
    var_left = "canale_2016",
    var_right = "housing_tenant_2016"
  ),
  class = "bivar"
) |>
  legend_render(df = "CMA_CSD")
