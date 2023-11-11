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

  vars <- vars_build(
    var_left = "climate_drought",
    var_right = " ", scale = "CSD", time = 2016
  )$vars
  p2 <- legend_render(vars, scale = "CSD", font_family = NULL)
  expect_equal(p2$layers[[1]]$setup_layer |> class(), "ggproto_method")
  expect_equal(p2$labels$fill, "fill")
})


# test_that("legend_render.qual works", {
#   vars <- vars_build(var_left = "",
#                      var_right = " ", df = "")
#   p <- legend_render(vars)
#   expect_error(print(p), NA)
# })

test_that("legend_render.bivar  works", {
  region <- "CMA"
  scale <- "DA"
  vars <- vars_build(
    var_left = "housing_tenant", "housing_rent", scale = scale, time = 2016
  )$vars
  data <- data_get(vars = vars, scale = scale, region = region)
  p <- legend_render(vars = vars, scale = scale, data = data, font_family = NULL)
  expect_equal(p$layers[[1]]$setup_layer |> class(), "ggproto_method")
  expect_equal(p$labels$fill, "fill")
})

test_that("legend_render.delta  works", {
  vars <- vars_build("housing_tenant", scale = "DA", time = c(2001, 2021))
  time <- vars$time
  vars <- vars$vars
  data <- data_get(vars, time = time, scale = "DA", region = "city")

  p <- legend_render(vars, scale = "DA", time = time, data = data, font_family = NULL)
  expect_error(p, NA)
})


test_that("legend_render.delta_ind  works", {
  vars <- vars_build("alp", scale = "DA", time = c(2001, 2021))
  time <- vars$time
  vars <- vars$vars
  data <- data_get(vars, time = time, scale = "DA", region = "city")

  p <- legend_render(vars, scale = "DA", time = time, data = data, font_family = NULL)
  expect_error(p, NA)
  expect_equal(p$layers[[1]]$setup_layer |> class(), "ggproto_method")
  expect_equal(p$labels$fill, "fill")

  vars <- vars_build("climate_drought", scale = "grid250", time = c(2015, 2022))
  time <- vars$time
  vars <- vars$vars
  data <- data_get(vars, time = time, scale = "DA", region = "city")

  p <- legend_render(vars, scale = "grid250", time = time, data = data,
                     font_family = NULL, lang = "fr")
  expect_error(p, NA)
  expect_equal(p$layers[[1]]$setup_layer |> class(), "ggproto_method")
  expect_equal(p$labels$fill, "fill")
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
  vars <- vars_build("housing_tenant", var_right = "alp",
                     scale = "DA", time = c(2001, 2021))
  time <- vars$time
  vars <- vars$vars
  data <- data_get(vars, time = time, scale = "DA", region = "city")

  p <- legend_render(vars, scale = "DA", time = time,
                     data = data, font_family = NULL)
  expect_error(p, NA)
  expect_equal(p$layers[[1]]$setup_layer |> class(), "ggproto_method")
  expect_equal(p$labels$fill, "fill")
})

test_that("legend_render.bivar_ldelta_rq3  works", {
  vars <- vars_build("housing_tenant", var_right = "alp",
                     scale = "DA", time = c(1996, 2001))
  time <- vars$time
  vars <- vars$vars
  data <- data_get(vars, time = time, scale = "DA", region = "city")

  p <- legend_render(vars, scale = "DA", time = time,
                     data = data, font_family = NULL)
  expect_error(p, NA)
  expect_equal(p$layers[[1]]$setup_layer |> class(), "ggproto_method")
  expect_equal(p$labels$fill, "fill")


  vars <- vars_build("climate_drought", var_right = "alp",
                     scale = "DA", time = c(2006, 2016))
  time <- vars$time
  vars <- vars$vars
  data <- data_get(vars, time = time, scale = "DA", region = "city")

  p <- legend_render(vars, scale = "DA", time = time,
                     data = data, font_family = NULL)
  expect_error(p, NA)
  expect_equal(p$layers[[1]]$setup_layer |> class(), "ggproto_method")
  expect_equal(p$labels$fill, "fill")
})
