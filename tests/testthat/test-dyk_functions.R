test_dyk_helper <- function(var_left, var_right, region, scale, time, select_id) {
  vars <- vars_build(var_left,
    var_right = var_right,
    scale = scale, time = time
  )
  time <- vars$time
  vars <- vars$vars
  data <- data_get(vars, time = time, scale = scale, region = region)

  actual <- dyk_text(
    vars = vars, scale = scale, select_id = select_id,
    region = region, zoom_levels = mzl_CSD_CT_DA_building, time = time,
    scales_as_DA = c("building", "street"), lang = "en"
  )

  expect_equal(class(actual), "list")
  expect_equal(class(actual[[1]]), "character")
}

test_dyks <- function(var_right, select_id, region, scale) {
  # Pct
  test_dyk_helper(
    var_left = "housing_tenant",
    var_right = var_right, region = region, scale = scale,
    select_id = select_id, time = 2021
  )

  # Dollar
  test_dyk_helper(
    var_left = "housing_rent",
    var_right = var_right, region = region, scale = scale,
    select_id = select_id, time = 2021
  )

  # Ind scalar
  test_dyk_helper(
    var_left = "alp",
    var_right = var_right, region = region, scale = scale,
    select_id = select_id, time = 2021
  )

  # # Count
  # test_dyk_helper(var_left = "crash_count_ped",
  #                 var_right = var_right, region = region, scale = scale,
  #                 select_id = select_id, time = 2021
  # )

  # # Ind ordinal
  # test_dyk_helper(var_left = "climate_drought",
  #                 var_right = var_right, region = region, scale = scale,
  #                 select_id = select_id, time = 2021
  # )

  # # Ind ordinal, multiple schemas
  # test_dyk_helper(var_left = "access_foot_food_grocery",
  #                 var_right = var_right,
  #                 region = region, scale = "DA", select_id = select_id,
  #                 time = 2023
  # )

  # sqkm
  test_dyk_helper(
    var_left = "alley_sqkm",
    var_right = var_right, region = "city", scale = "CT",
    select_id = if (is.na(select_id)) NA else "4620225.00", time = 2023
  )

  # per1k
  test_dyk_helper(
    var_left = "alley_per1k",
    var_right = var_right, region = "city", scale = "DA",
    select_id = if (is.na(select_id)) NA else "24662027", time = 2023
  )
}


# q5 ----------------------------------------------------------------------


test_that("dyk_text.q5 works", {
  test_dyks(var_right = " ", select_id = NA, region = "CMA", scale = "CSD")
  test_dyks(var_right = " ", select_id = NA, region = "city", scale = "building")
})
