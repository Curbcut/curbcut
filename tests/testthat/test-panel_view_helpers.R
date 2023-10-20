test_table_view_prep_table_helper <- function(var_left, var_right, scale, region,
                                              select_id, time, map_zoom_levels) {
  vars <- vars_build(var_left, var_right = var_right, scale = scale, time = time)
  time <- vars$time
  vars <- vars$vars
  data <- data_get(vars, scale = scale, region = region)

  actual <- table_view_prep_table(
    vars = vars,
    data = data,
    scale = scale,
    time = time,
    zoom_levels = map_zoom_levels
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
    map_zoom_levels = mzl_tablequartier_CT_DA_building
  )

  # var_left <- "access_foot_food_grocery_20"
  # scale <- "DA"
  # region <- "city"
  # time <- 2023
  # var_right <- " "
  # map_zoom_levels <- mzl_tablequartier_CT_DA_building
  # test_table_view_prep_table_helper(
  #   var_left = var_left,
  #   var_right = var_right,
  #   scale = scale,
  #   region = region,
  #   time = time,
  #   map_zoom_levels = map_zoom_levels
  # )

  test_table_view_prep_table_helper(
    var_left = "alp",
    var_right = " ",
    scale = "DA",
    region = "city",
    time = 2021,
    map_zoom_levels = mzl_tablequartier_CT_DA_building
  )

  # test_table_view_prep_table_helper(
  #   var_left = "alley_sqkm",
  #   var_right = " ",
  #   scale = "DA",
  #   region = "city",
  #   time = 2023,
  #   map_zoom_levels = mzl_tablequartier_CT_DA_building
  # )
  #
  # test_table_view_prep_table_helper(
  #   var_left = "alley_per1k",
  #   var_right = " ",
  #   scale = "DA",
  #   region = "city",
  #   time = 2023,
  #   map_zoom_levels = mzl_tablequartier_CT_DA_building
  # )

  test_table_view_prep_table_helper(
    var_left = "vac_rate_bachelor_bed",
    var_right = " ",
    scale = "cmhczone",
    region = "CMA",
    time = 2021,
    map_zoom_levels = mzl_cmhczone
  )

})

# test_that("table_view_prep_table works as expected (bivar)", {
#   var_left <- "housing_tenant_2021"
#   df <- "city_DA"
#   var_right <- "climate_drought_2022"
#   map_zoom_levels <- map_zoom_levels_city
#   test_table_view_prep_table_helper(
#     var_left = var_left,
#     var_right = var_right,
#     df = df,
#     map_zoom_levels = map_zoom_levels
#   )
#
#   var_left <- "access_foot_20_food_grocery_2023"
#   df <- "city_DA"
#   var_right <- "climate_drought_2022"
#   map_zoom_levels <- map_zoom_levels_city
#   test_table_view_prep_table_helper(
#     var_left = var_left,
#     var_right = var_right,
#     df = df,
#     map_zoom_levels = map_zoom_levels
#   )
#
#   var_left <- "alp_2021"
#   df <- "city_DA"
#   var_right <- "climate_drought_2022"
#   map_zoom_levels <- map_zoom_levels_city
#   test_table_view_prep_table_helper(
#     var_left = var_left,
#     var_right = var_right,
#     df = df,
#     map_zoom_levels = map_zoom_levels
#   )
#
#   var_left <- "alley_sqkm_2023"
#   df <- "city_DA"
#   var_right <- "climate_drought_2022"
#   map_zoom_levels <- map_zoom_levels_city
#   test_table_view_prep_table_helper(
#     var_left = var_left,
#     var_right = var_right,
#     df = df,
#     map_zoom_levels = map_zoom_levels
#   )
#
#   var_left <- "alley_per1k_2023"
#   df <- "city_DA"
#   var_right <- "climate_drought_2022"
#   map_zoom_levels <- map_zoom_levels_city
#   test_table_view_prep_table_helper(
#     var_left = var_left,
#     var_right = var_right,
#     df = df,
#     map_zoom_levels = map_zoom_levels
#   )
#   #
#   #   var_left <-  "vac_rate_bachelor_bed_2021"
#   #   df <- "cmhc_cmhczone"
#   #   var_right <- "housing_tenant_2021"
#   #   map_zoom_levels <- map_zoom_levels_cmhc
#   #   test_table_view_prep_table_helper(var_left = var_left,
#   #                                     var_right = var_right,
#   #                                     df = df,
#   #                                     map_zoom_levels = map_zoom_levels)
# })
#
# test_that("table_view_prep_table works as expected (delta)", {
#   var_left <- c("housing_tenant_2016", "housing_tenant_2021")
#   df <- "city_DA"
#   var_right <- " "
#   map_zoom_levels <- map_zoom_levels_city
#   test_table_view_prep_table_helper(
#     var_left = var_left,
#     var_right = var_right,
#     df = df,
#     map_zoom_levels = map_zoom_levels
#   )
#
#   var_left <- c("alp_2016", "alp_2021")
#   df <- "city_DA"
#   var_right <- " "
#   map_zoom_levels <- map_zoom_levels_city
#   test_table_view_prep_table_helper(
#     var_left = var_left,
#     var_right = var_right,
#     df = df,
#     map_zoom_levels = map_zoom_levels
#   )
#   #
#   #   var_left <-  c("vac_rate_bachelor_bed_2015", "vac_rate_bachelor_bed_2021")
#   #   df <- "cmhc_cmhczone"
#   #   var_right <- " "
#   #   map_zoom_levels <- map_zoom_levels_cmhc
#   #   test_table_view_prep_table_helper(var_left = var_left,
#   #                                     var_right = var_right,
#   #                                     df = df,
#   #                                     map_zoom_levels = map_zoom_levels)
# })
