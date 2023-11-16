test_table_view_prep_table_helper <- function(var_left, var_right, scale, region,
                                              select_id, time, map_zoom_levels,
                                              schemas = NULL) {
  vars <- vars_build(var_left, var_right = var_right, scale = scale, time = time)
  time <- vars$time
  vars <- vars$vars
  data <- data_get(vars, scale = scale, region = region, time = time)

  actual <- table_view_prep_table(
    vars = vars,
    data = data,
    scale = scale,
    time = time,
    zoom_levels = map_zoom_levels,
    schemas = schemas
  )
  expect_equal(is.list(actual), TRUE)
  expect_equal(is.character(actual$text), TRUE)
  expect_equal(is.data.frame(actual$data), TRUE)
  expect_equal(is.data.frame(actual$pretty_data), TRUE)
}

test_that("table_view_prep_table works as expected (q5)", {
  test_table_view_prep_table_helper(
    var_left = "housing_tenant",
    var_right = " ",
    scale = "DA",
    region = "city",
    time = 2021,
    map_zoom_levels = mzl_borough_CT_DA_building
  )

  var_left <- "access_foot_food_grocery"
  scale <- "DA"
  region <- "city"
  time <- 2023
  var_right <- " "
  schemas <- list(var_left = setNames(20, "transportationtime"))
  map_zoom_levels <- mzl_borough_CT_DA_building
  test_table_view_prep_table_helper(
    var_left = var_left,
    var_right = var_right,
    scale = scale,
    region = region,
    time = time,
    map_zoom_levels = map_zoom_levels,
    schemas = schemas
  )

  test_table_view_prep_table_helper(
    var_left = "alp",
    var_right = " ",
    scale = "DA",
    region = "city",
    time = 2021,
    map_zoom_levels = mzl_borough_CT_DA_building
  )

  test_table_view_prep_table_helper(
    var_left = "alley_sqkm",
    var_right = " ",
    scale = "DA",
    region = "city",
    time = 2023,
    map_zoom_levels = mzl_borough_CT_DA_building
  )

  test_table_view_prep_table_helper(
    var_left = "alley_per1k",
    var_right = " ",
    scale = "DA",
    region = "city",
    time = 2023,
    map_zoom_levels = mzl_borough_CT_DA_building
  )

  test_table_view_prep_table_helper(
    var_left = "vac_rate_bachelor_bed",
    var_right = " ",
    scale = "cmhczone",
    region = "CMA",
    time = 2021,
    map_zoom_levels = mzl_cmhczone
  )

})

test_that("table_view_prep_table works as expected (bivar)", {
  test_table_view_prep_table_helper(
    var_left = "housing_tenant",
    var_right = "lst",
    scale = "DA",
    region = "city",
    time = 2021,
    map_zoom_levels = mzl_CSD_CT_DA_building
  )

  test_table_view_prep_table_helper(
    var_left = "access_foot_food_grocery",
    var_right = "lst",
    scale = "DA",
    region = "city",
    time = 2023,
    map_zoom_levels = mzl_CSD_CT_DA_building,
    schemas = list(var_left = setNames(20, "transportationtime"))
  )

  test_table_view_prep_table_helper(
    var_left = "alp",
    var_right = "climate_drought",
    scale = "DA",
    region = "city",
    time = 2021,
    map_zoom_levels = mzl_CSD_CT_DA_building
  )

  test_table_view_prep_table_helper(
    var_left = "alley_sqkm",
    var_right = "housing_rent",
    scale = "DA",
    region = "city",
    time = 2023,
    map_zoom_levels = mzl_CSD_CT_DA_building
  )

  test_table_view_prep_table_helper(
    var_left = "alley_per1k",
    var_right = "climate_drought",
    scale = "DA",
    region = "city",
    time = 2023,
    map_zoom_levels = mzl_CSD_CT_DA_building
  )

  test_table_view_prep_table_helper(
    var_left = "vac_rate_bachelor_bed",
    var_right = "inc_50",
    scale = "cmhczone",
    region = "CMA",
    time = 2021,
    map_zoom_levels = mzl_cmhczone
  )
})

test_that("table_view_prep_table works as expected (delta)", {
  test_table_view_prep_table_helper(
    var_left = "housing_tenant",
    var_right = " ",
    scale = "DA",
    region = "city",
    time = c(2001, 2021),
    map_zoom_levels = mzl_borough_CT_DA_building
  )

  # var_left <- "access_foot_food_grocery"
  # scale <- "DA"
  # region <- "city"
  # time <- 2023
  # var_right <- " "
  # map_zoom_levels <- mzl_borough_CT_DA_building
  # test_table_view_prep_table_helper(
  #   var_left = var_left,
  #   var_right = var_right,
  #   scale = scale,
  #   region = region,
  #   time = time,
  #   map_zoom_levels = map_zoom_levels,
  #   schemas = list(var_left = setNames(20, "transportationtime"))
  # )

  test_table_view_prep_table_helper(
    var_left = "alp",
    var_right = " ",
    scale = "DA",
    region = "city",
    time = c(2001, 2021),
    map_zoom_levels = mzl_borough_CT_DA_building
  )

  test_table_view_prep_table_helper(
    var_left = "climate_drought",
    var_right = " ",
    scale = "grd250",
    region = "island",
    time = c(2015, 2022),
    map_zoom_levels = mzl_grd250_grd100_grd50_grd25
  )

  test_table_view_prep_table_helper(
    var_left = "alley_per1k",
    var_right = " ",
    scale = "DA",
    region = "city",
    time = 2023,
    map_zoom_levels = mzl_borough_CT_DA_building
  )

  test_table_view_prep_table_helper(
    var_left = "vac_rate_bachelor_bed",
    var_right = " ",
    scale = "cmhczone",
    region = "CMA",
    time = c(2016, 2021),
    map_zoom_levels = mzl_cmhczone
  )
})


test_that("table_view_prep_table works as expected (delta_bivar)", {
  test_table_view_prep_table_helper(
    var_left = "housing_tenant",
    var_right = "housing_rent",
    scale = "DA",
    region = "city",
    time = c(2001, 2021),
    map_zoom_levels = mzl_borough_CT_DA_building
  )

  test_table_view_prep_table_helper(
    var_left = "alp",
    var_right = "inc_limat",
    scale = "DA",
    region = "city",
    time = c(2001, 2021),
    map_zoom_levels = mzl_borough_CT_DA_building
  )

  test_table_view_prep_table_helper(
    var_left = "vac_rate_bachelor_bed",
    var_right = "lst",
    scale = "cmhczone",
    region = "CMA",
    time = c(2016, 2021),
    map_zoom_levels = mzl_cmhczone
  )
})
