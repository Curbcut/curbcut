test_that("explore_context works", {
  actual <- explore_context(
    region = "CMA", select_id = NA,
    df = "CSD", switch_DA = FALSE
  )
  expected <- list(p_start = c(CMA = "in the Montreal region"))
  expect_equal(actual, expected)

  # All IDs gives the corresponding list
  expected_names <-
    c(
      "heading", "p_start", "name", "to_compare_determ", "to_compare_short",
      "scale_plur", "select_id"
    )
  actual <- sapply(city_CT$ID, \(x) {
    actual_names <-
      explore_context(
        region = "city", select_id = x, df = "city_CT",
        switch_DA = FALSE
      )
    all(names(actual_names) == expected_names)
  })
  expect_equal(all(actual), TRUE)
})

test_that("explore_text_parent_title works", {
  vars <- vars_build("housing_tenant_2016", df = "CMA_CT")
  actual <- explore_text_parent_title(var = vars$var_left)
  expected <- "households"
  expect_equal(actual, expected)

  vars <- vars_build("trans_walk_or_bike_2021", df = "CMA_CT")
  actual <- explore_text_parent_title(var = vars$var_left)
  expected <- "employed individuals"
  expect_equal(actual, expected)

  vars <- vars_build("trans_walk_or_bike_2021", df = "CMA_CT")
  actual <- explore_text_parent_title(var = vars$var_left)
  expected <- "employed individuals"
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
  vars <- vars_build("age_0_14_2021", df = df)
  region <- "city"
  data <- data_get(vars = vars, df = df)
  actual <- explore_text_region_val_df(
    var = vars$var_left,
    region = region,
    select_id = "4620003.00",
    data = data,
    df = df
  )
  expect_equal(all(c("val", "count") %in% names(actual)), TRUE)

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

