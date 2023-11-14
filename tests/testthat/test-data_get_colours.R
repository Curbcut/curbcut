test_that("data_get_colours.q5  works", {
  vars <- vars_build(var_left = "housing_tenant", scale = "CSD",
                     time = 2021)
  time <- vars$time
  vars <- vars$vars


  data_colours <- data_get_colours(
    vars = vars, region = "CMA",
    zoom_levels = mzl_CSD_CT_DA_building,
    time = time
  )
  expect_true(nrow(data_colours) > 1000)
  expect_equal(unique(nchar(data_colours$fill)), 7)
  expect_equal(names(data_colours), c("ID_color", "fill"))
})

test_that("data_get_colours.q5  works", {
  vars <- vars_build(var_left = "access_foot_food_grocery", scale = "CSD",
                     time = 2023)
  time <- vars$time
  vars <- vars$vars
  schemas <- list(transportationtime = 20)


  data_colours <- data_get_colours(
    vars = vars, region = "CMA",
    zoom_levels = mzl_CSD_CT_DA_building,
    time = time, schemas = schemas
  )
  expect_true(nrow(data_colours) > 1000)
  expect_equal(unique(nchar(data_colours$fill)), 7)
  expect_equal(names(data_colours), c("ID_color", "fill"))
})

test_that("data_get_colours.bivar works", {
  vars <- vars_build(
    var_left = "housing_tenant",
    var_right = "alp",
    scale = "CSD",
    time = 2016
  )
  time <- vars$time
  vars <- vars$vars
  data_colours <- data_get_colours(
    vars = vars, region = "CMA", time = time,
    zoom_levels = mzl_CSD_CT
  )
  expect_true(nrow(data_colours) > 1000)
  expect_equal(unique(nchar(data_colours$fill)), 7)
  expect_equal(names(data_colours), c("ID_color", "fill"))
})

test_that("data_get_colours.delta works", {
  vars <- vars_build(
    var_left = "housing_tenant",
    var_right = " ",
    scale = "CSD",
    time = c(1996, 2016)
  )
  time <- vars$time
  vars <- vars$vars
  data_colours <- data_get_colours(
    vars = vars, region = "CMA", time = time,
    zoom_levels = mzl_CSD_CT
  )
  expect_true(nrow(data_colours) > 1000)
  expect_equal(unique(nchar(data_colours$fill)), 7)
  expect_equal(names(data_colours), c("ID_color", "fill"))
})

test_that("data_get_colours.delta_bivar works", {
  vars <- vars_build(
    var_left = "housing_tenant",
    var_right = "alp",
    scale = "CSD",
    time = c(1996, 2016)
  )
  time <- vars$time
  vars <- vars$vars
  data_colours <- data_get_colours(
    vars = vars, region = "CMA", time = time,
    zoom_levels = mzl_CSD_CT
  )
  expect_true(nrow(data_colours) > 1000)
  expect_equal(unique(nchar(data_colours$fill)), 7)
  expect_equal(names(data_colours), c("ID_color", "fill"))
})

test_that("data_get_colours.bivar_ldelta_rq3 works", {
  vars <- vars_build(
    var_left = "crash_count_ped",
    var_right = "alp",
    scale = "CT",
    time = c(2015, 2017)
  )
  time <- vars$time
  vars <- vars$vars
  data_colours <- data_get_colours(
    vars = vars, region = "CMA", time = time,
    zoom_levels = mzl_CSD_CT
  )
  expect_true(nrow(data_colours) > 1000)
  expect_equal(unique(nchar(data_colours$fill)), 7)
  expect_equal(names(data_colours), c("ID_color", "fill"))
})
