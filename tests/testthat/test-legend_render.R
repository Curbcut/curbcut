test_that("legend_breaks.q5 (without `chr_breaks` attribute) works", {
  region <- "CMA"
  scale <- "DA"
  vars <- vars_build(
    var_left = "housing_tenant", scale = scale, time = 2016
  )$vars
  data <- data_get(vars = vars, scale = scale, region = region)
  p <- legend_render(vars, scale = scale, data = data, font_family = NULL)
  expect_equal(p$layers[[1]]$setup_layer |> class(), "ggproto_method")
  expect_equal(p$labels$fill, "fill")
})

test_that("legend_breaks.q5_ind works", {
  region <- "CMA"
  scale <- "DA"
  vars <- vars_build(
    var_left = "alp", scale = scale, time = 2016
  )$vars
  data <- data_get(vars = vars, scale = scale, region = region)
  p1 <- legend_render(vars, scale = scale, data = data, font_family = NULL)
  expect_equal(p1$layers[[1]]$setup_layer |> class(), "ggproto_method")
  expect_equal(p1$labels$fill, "fill")

  # vars <- vars_build(
  #   var_left = "CLIMATE",
  #   var_right = " ", df = "CMA_CSD"
  # )
  # p2 <- legend_render(vars, df = "CMA_CSD", font_family = NULL)
  # expect_error(print(p2), NA)
})


# test_that("legend_render.qual works", {
#   vars <- vars_build(var_left = "",
#                      var_right = " ", df = "")
#   p <- legend_render(vars)
#   expect_error(print(p), NA)
# })

# test_that("legend_render.bivar  works", {
#   vars <- vars_build(
#     var_left = "housing_tenant_2016",
#     var_right = "alp_2016", df = "CMA_CSD"
#   )
#   p <- legend_render(vars, df = "CMA_CSD", font_family = NULL)
#   expect_error(print(p), NA)
# })
#
# test_that("legend_render.delta  works", {
#   vars <- vars_build(
#     var_left = c("housing_tenant_2006", "housing_tenant_2016"),
#     var_right = " ", df = "CMA_CSD"
#   )
#   p <- legend_render(vars, df = "city_CSD", font_family = NULL)
#   expect_error(p, NA)
# })

# test_that("legend_render.q100  works", {
#   vars <- vars_build(
#     var_left = "c_flood",
#     var_right = " ", df = "raster"
#   )
#   p <- legend_render(vars, df = "raster", font_family = NULL)
#   expect_error(print(p), NA)
# })

# test_that("legend_render.delta_bivar  works", {
#   vars <- vars_build(
#     var_left = c("housing_tenant_2006", "housing_tenant_2016"),
#     var_right = c("alp_2006", "alp_2016"), df = "city_CSD"
#   )
#   data <- data_get(vars, df = "city_CSD")
#   p <- legend_render(vars, df = "city_CSD", data = data, font_family = NULL)
#   expect_error(p, NA)
# })
#
# test_that("legend_render.bivar_ldelta_rq3  works", {
#   vars <- vars_build(
#     var_left = c("housing_tenant_2006", "housing_tenant_2016"),
#     var_right = "alp_2016", df = "city_CSD"
#   )
#   data <- data_get(vars, df = "city_CSD")
#   p <- legend_render(vars, df = "city_CSD", data = data, font_family = NULL)
#   expect_error(p, NA)
# })
