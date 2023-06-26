test_that("explore_context works", {
  actual <- explore_context(
    region = "CMA", select_id = NA,
    df = "CSD", switch_DA = FALSE
  )
  expect_equal(names(actual), "p_start")
  expect_equal(class(unlist(actual)), "character")

  # All IDs gives the corresponding list
  expected_names <-
    c(
      "heading", "p_start", "name", "to_compare_determ", "to_compare_short",
      "scale_plur", "select_id", "treated_df"
    )
  actual_names <-
    explore_context(
      region = "city", select_id = "4620003.00", df = "city_CT",
      switch_DA = FALSE
    )
  expect_equal(all(names(actual_names) == expected_names), TRUE)
})

test_that("explore_text_parent_title works", {
  vars <- vars_build("housing_tenant_2016", df = "city_CT")
  actual <- explore_text_parent_title(var = vars$var_left)
  expected <- "households"
  expect_equal(actual, expected)

  vars <- vars_build("housing_rent_2021", df = "city_CT")
  actual <- explore_text_parent_title(var = vars$var_left)
  expected <- "tenant households"
  expect_equal(actual, expected)

  vars <- vars_build("alp_2021", df = "city_CT")
  actual <- explore_text_parent_title(var = vars$var_left)
  expected <- "households"
  expect_equal(actual, expected)
})

test_that("explore_text_region_val_df works", {
  vars <- vars_build("housing_tenant_2016", df = "city_CT")
  region <- "city"
  actual <- explore_text_region_val_df(
    var = vars$var_left,
    region = region,
    select_id = NA
  )
  expect_equal(all(c("val", "count") %in% names(actual)), TRUE)

  df <- "city_CT"
  vars <- vars_build("alp_2021", df = df)
  region <- "city"
  data <- data_get(vars = vars, df = df)
  actual <- explore_text_region_val_df(
    var = vars$var_left,
    region = region,
    select_id = "4620003.00",
    data = data,
    df = df
  )
  expect_equal(all(c("val", "num") %in% names(actual)), TRUE)

  df <- "city_CT"
  vars <- vars_build("housing_rent_2021", df = df)
  region <- "city"
  data <- data_get(vars = vars, df = df)
  actual <- explore_text_region_val_df(
    var = vars$var_left,
    region = region,
    select_id = "4620003.00",
    data = data,
    df = df
  )
  expect_equal(all(c("val") %in% names(actual)), TRUE)

  # TKTK TEST `IND` TOO
})


test_that("explore_text_selection_comparison works", {
  df <- "city_CT"
  vars <- vars_build("housing_rent_2021", df = df)
  region <- "city"
  data <- data_get(vars = vars, df = df)
  actual <- explore_text_selection_comparison(
    var = vars$var_left,
    select_id = "4620003.00",
    data = data
  )
  expect_equal(
    actual,
    list(
      higher_than = "18%", rank_chr = "<b>exceptionally inexpensive</b>",
      higher_than_num = 0.179324894514768
    )
  )


  df <- "city_CT"
  vars <- vars_build("housing_tenant_2021", df = df)
  region <- "city"
  data <- data_get(vars = vars, df = df)
  actual <- explore_text_selection_comparison(
    var = vars$var_left,
    select_id = "4620003.00",
    data = data
  )
  expect_equal(
    actual,
    list(
      higher_than = "31%", rank_chr = "<b>unusually low</b>",
      higher_than_num = 0.307368421052632
    )
  )
})
