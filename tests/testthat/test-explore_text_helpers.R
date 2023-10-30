test_that("explore_context works", {
  actual <- explore_context(
    region = "CMA", select_id = NA,
    scale = "CSD", switch_DA = FALSE
  )
  expect_equal(names(actual), c("p_start", "treated_scale"))
  expect_equal(class(unlist(actual)), "character")

  # All IDs gives the corresponding list
  expected_names <-
    c(
      "heading", "p_start", "name", "to_compare_determ", "to_compare_short",
      "scale_plur", "select_id", "treated_scale"
    )
  actual_names <-
    explore_context(
      select_id = "4620003.00", region = "city", scale = "CT",
      zoom_levels = mzl_CSD_CT_DA_building,
      switch_DA = FALSE
    )
  expect_equal(all(names(actual_names) == expected_names), TRUE)
})

test_that("explore_text_parent_title works", {
  vars <- vars_build("housing_tenant", scale = "CT", time = 2016)
  vars <- vars$vars
  actual <- explore_text_parent_title(var = vars$var_left)
  expected <- "households"
  expect_equal(actual, expected)

  vars <- vars_build("housing_rent", scale = "CT", time = 2021)
  vars <- vars$vars
  actual <- explore_text_parent_title(var = vars$var_left)
  expected <- "tenant households"
  expect_equal(actual, expected)

  vars <- vars_build("alp", scale = "CT", time = 2021)
  vars <- vars$vars
  actual <- explore_text_parent_title(var = vars$var_left)
  expected <- "households"
  expect_equal(actual, expected)
})

test_that("explore_text_region_val_df works", {
  scale <- "CT"
  region <- "city"
  vars <- vars_build("housing_tenant", scale = scale, time = 2016)
  time <- vars$time
  vars <- vars$vars
  data <- data_get(vars, scale = scale, region = region)
  actual <- explore_text_region_val_df(
    var = vars$var_left,
    region = region,
    scale = scale,
    select_id = NA,
    data = data,
    time = time
  )
  expect_equal(all(c("val", "count") %in% names(actual)), TRUE)

  vars <- vars_build("alp", scale = scale, time = 2016)
  time <- vars$time
  vars <- vars$vars
  data <- data_get(vars = vars, scale = scale)
  actual <- explore_text_region_val_df(
    var = vars$var_left,
    region = region,
    select_id = "4620003.00",
    data = data,
    scale = scale,
    time = time
  )
  expect_equal(all(c("val", "num") %in% names(actual)), TRUE)

  vars <- vars_build("housing_rent", scale = scale, time = 2021)
  time <- vars$time
  vars <- vars$vars
  data <- data_get(vars = vars, scale = scale)
  actual <- explore_text_region_val_df(
    var = vars$var_left,
    region = region,
    select_id = "4620003.00",
    data = data,
    scale = scale,
    time = time
  )
  expect_equal(all(c("val") %in% names(actual)), TRUE)

})


test_that("explore_text_selection_comparison works", {
  scale <- "CT"
  region <- "city"
  vars <- vars_build("housing_rent", scale = scale, time = 2021)
  time <- vars$time
  vars <- vars$vars
  data <- data_get(vars = vars, scale = scale, region = region)
  actual <- explore_text_selection_comparison(
    var = vars$var_left,
    select_id = "4620003.00",
    data = data,
    time = time
  )
  expect_true(all(c("higher_than", "rank_chr", "higher_than_num") %in% names(actual)))


  vars <- vars_build("housing_tenant", scale = scale, time = 2021)
  time <- vars$time
  vars <- vars$vars
  data <- data_get(vars = vars, scale = scale, region = region)
  actual <- explore_text_selection_comparison(
    var = vars$var_left,
    select_id = "4620003.00",
    data = data,
    time = time
  )
  expect_true(all(c("higher_than", "rank_chr", "higher_than_num") %in% names(actual)))

  vars <- vars_build("alp", scale = scale, time = 2021)
  time <- vars$time
  vars <- vars$vars
  data <- data_get(vars = vars, scale = scale, region = region)
  actual <- explore_text_selection_comparison(
    var = vars$var_left,
    select_id = "4620003.00",
    data = data,
    time = time
  )
  expect_true(all(c("higher_than", "rank_chr", "higher_than_num") %in% names(actual)))
})

