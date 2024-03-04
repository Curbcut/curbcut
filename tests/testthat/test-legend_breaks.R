test_that("legend_breaks.q5 works", {
  vars1 <- vars_build(var_left = "housing_tenant", scale = "CSD", time = 2021)$vars
  data1 <- data_get(vars = vars1, scale = "CSD", region = "CMA")
  vars2 <- vars_build(var_left = "housing_rent", scale = "DA", time = 2011)$vars
  data2 <- data_get(vars = vars2, scale = "DA", region = "CMA")
  # vars3 <- vars_build(var_left = "climate_drought_2015", df = "grid_grid50")

  expect_equal(
    all(grepl("^\\d", legend_breaks(vars1, scale = "CSD", data = data1))),
    TRUE
  )
  expect_equal(
    all(grepl("^\\$\\d", legend_breaks(vars2, scale = "DA", data = data2))),
    TRUE
  )
})

test_that("legend_breaks.q5_ind works", {
  # vars1 <- vars_build(var_left = "climate_drought", scale = "grid50", time = 2015)
  vars2 <- vars_build(var_left = "climate_drought", scale = "grd250", time = 2016)$vars
  data2 <- data_get(vars = vars2, scale = "DA", region = "island")

  # expect_equal(
  #   unname(legend_breaks(vars1, df = "grid_grid50")),
  #   structure(c("Insig.", "Minor", "Mod.", "Elev.", "Major"))
  # )

  legend <- legend_breaks(vars2, scale = "grd250", data = data2)
  expect_true(length(legend) == 5)
  expect_true(all(is.character(legend)))
})

test_that("legend_breaks.q100 works", {
  vars <- vars_build(var_left = "c_priority", scale = "raster", time = 2021)$vars
  expect_equal(
    legend_breaks(vars, scale = "raster"),
    list(
      "Low", NULL, NULL, NULL, NULL,
      "High"
    )
  )
})

# test_that("legend_breaks.qual works", {
#
# })

test_that("legend_breaks.bivar_ldelta_rq3 works", {
  vars <- vars_build(
    var_left = "housing_tenant",
    var_right = "alp",
    scale = "CSD",
    time = c(1996, 2001)
  )
  time <- vars$time
  vars <- vars$vars
  data <- data_get(
    vars = vars,
    scale = "CSD",
    region = "island",
    time = time
  )
  actual <- legend_breaks(vars,
    data = data,
    time = time,
    schemas =
    )
  expect_equal(
    names(actual),
    c("x", "y")
  )
  expect_equal(
    sum(grepl("%$", actual[[2]])),
    sum(c(T, T, T, T))
  )
  expect_equal(
    length(actual$x), 4
  )
})

test_that("legend_breaks.delta works", {
  vars <- vars_build(var_left = c(
    "housing_tenant"
  ), scale = "CT", time = c(2006, 2016))
  time <- vars$time
  vars <- vars$vars
  data <- data_get(vars, scale = "CT", region = "CMA", time = time)

  brks <- legend_breaks(vars, scale = "CT", data = data)
  expect_true(all(endsWith(brks, "%")))
  expect_true(grepl("^-", brks[1]))


  vars <- vars_build(var_left = c(
    "alp"
  ), scale = "CT", time = c(2006, 2016))
  time <- vars$time
  vars <- vars$vars
  data <- data_get(vars, scale = "CT", region = "CMA", time = time)

  brks <- legend_breaks(vars, scale = "CT", data = data)
  expect_true(all(endsWith(brks, "%")))
  expect_true(grepl("^-", brks[1]))
})

test_that("legend_breaks.bivar works", {
  vars <- vars_build(
    var_left = "inc_50",
    var_right = "lst",
    scale = "CSD",
    time = 2021
  )
  time <- vars$time
  vars <- vars$vars

  data <- data_get(vars, scale = "CSD", region = "CMA")

  brks <- legend_breaks(vars, data = data, scale = "city")

  expect_true(brks$x[1] < brks$x[2])
  expect_true(brks$y[1] < brks$y[2])
  expect_equal(
    length(brks$x),
    length(brks$y)
  )
  expect_true(all(endsWith(brks$x, "C")))
  expect_true(all(endsWith(brks$y, "%")))
  expect_true(length(brks$x) == 4)
  expect_true(length(brks$y) == 4)
})

test_that("legend_breaks.delta_bivar works", {
  vars <- vars_build(
    var_left = "housing_tenant",
    var_right = "edu_no_degree",
    scale = "DA",
    time = c(1996, 2021)
  )
  time <- vars$time
  vars <- vars$vars

  data <- data_get(vars, scale = "CSD", region = "CMA", time = time)

  brks <- legend_breaks(vars, data = data, scale = "city")

  expect_true(brks$x[1] < brks$x[2])
  expect_true(brks$y[1] < brks$y[2])
  expect_equal(
    length(brks$x),
    length(brks$y)
  )
  expect_true(all(endsWith(brks$x, "%")))
  expect_true(all(endsWith(brks$y, "%")))
  expect_true(length(brks$x) == 4)
  expect_true(length(brks$y) == 4)
})
