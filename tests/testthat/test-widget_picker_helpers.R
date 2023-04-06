test_that("picker_hover_divs creates divs with explanations on hover for a list of variables", {
  var_list <- dropdown_make(vars = variables$var_code[1:3])
  lang <- NULL
  expected_output <- list(content = c(
    "<div title=\"the percentage of private households occupied by tenants\" value=\"housing_tenant\" style=\"width: 100%;\">Tenant-occupied (%)</div>",
    "<div title=\"the average rent paid by tenants per month\" value=\"housing_rent\" style=\"width: 100%;\">Average rent ($)</div>",
    "<div title=\"the percentage of dwellings requiring major repairs\" value=\"housing_repairs\" style=\"width: 100%;\">Housing requiring major repairs (%)</div>"
  ))
  actual_output <- picker_hover_divs(var_list, lang)
  expect_identical(actual_output, expected_output)
})

test_that("picker_hover_divs returns NULL when input variable list is empty", {
  var_list <- list()
  lang <- NULL
  expected_output <- NULL
  actual_output <- picker_hover_divs(var_list, lang)
  expect_identical(actual_output, expected_output)
})

test_that("picker_hover_divs creates divs with translated explanations on hover for a list of variables", {
  var_list <- dropdown_make(vars = variables$var_code[1:3])
  lang <- "fr"
  var_list_t <- cc_t(var_list, lang = lang)
  actual_output <- suppressWarnings(picker_hover_divs(var_list_t, lang))
  expect_identical(names(actual_output), "content")
  expect_identical(all(grepl("<div", actual_output$content)), TRUE)
})

test_that("picker_hover_divs creates divs with translated explanations on hover for a comparison", {
  var_list <- dropdown_make(vars = variables$var_code[1:3], compare = TRUE)
  lang <- "fr"
  var_list_t <- cc_t(var_list, lang = "fr")
  actual_output <- suppressWarnings(picker_hover_divs(var_list_t, lang))
  expect_identical(names(actual_output), "content")
  expect_identical(all(grepl("<div", actual_output$content)), TRUE)
})

test_that("picker_hover_divs does not fail when variables are not part of the variables table", {
  var_list <- list("First" = "first", "Second" = "second", "Third" = "third")
  lang <- "fr"
  expected_output <- NULL
  actual_output <- picker_hover_divs(var_list, lang)
  expect_identical(actual_output, expected_output)
})

test_that("picker_hover_divs does not fail when variables are unknown vectors", {
  var_list <- c("First", "Second", "third")
  lang <- "fr"
  actual_output <- picker_hover_divs(var_list, lang)
  expect_identical(actual_output, NULL)
})


vars <- variables$var_code[variables$source == "Canadian census" & !is.na(variables$parent_vec)]

test_that("picker_multi_year_disable disables variables when necessary", {
  var_list <- dropdown_make(vars = vars)
  disable <- TRUE
  actual_output <- picker_multi_year_disable(var_list, disable)
  expected_output <-
    c(
      FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
    )
  expect_identical(actual_output, expected_output)
})

test_that("picker_multi_year_disable disables nothing when variables aren't in `variables` table", {
  var_list <- c("unkown", "in", "variables")
  disable <- TRUE
  actual_output <- picker_multi_year_disable(var_list, disable)
  expected_output <-
    c(
      FALSE, FALSE, FALSE
    )
  expect_identical(actual_output, expected_output)
})

test_that("picker_multi_year_disable does not disable any variables when none are needed", {
  var_list <- dropdown_make(vars = vars)
  disable <- FALSE
  actual_output <- picker_multi_year_disable(var_list, disable)
  expect_identical(actual_output, rep(F, length(vars)))
})

test_that("picker_multi_year_disable returns NULL when the list is empty", {
  var_list <- list()
  disable <- TRUE
  expected_output <- NULL
  actual_output <- picker_multi_year_disable(var_list, disable)
  expect_identical(actual_output, expected_output)
})

test_that("picker_multi_year_disable works when on compare", {
  var_list <- dropdown_make(vars = vars, compare = TRUE)
  disable <- TRUE
  expected_output <- TRUE
  actual_output <- picker_multi_year_disable(var_list, disable)
  expect_identical(sum(actual_output) > 0, expected_output)
})

test_that("picker_return_var returns input if time is NULL", {
  input <- "housing_tenant"
  time <- NULL
  expect_equal(picker_return_var(input, time), input)
})

test_that("picker_return_var returns empty string if input is empty", {
  input <- " "
  time <- 2010
  expect_equal(picker_return_var(input, time), " ")
})

test_that("picker_return_var returns variable code with closest year", {
  input <- "housing_tenant"
  time <- 2017
  expect_equal(picker_return_var(input, time), "housing_tenant_2016")
})

test_that("picker_return_var returns correct year when `time` isn't in the dates", {
  input <- "housing_tenant"
  time <- 2015
  var <- picker_return_var(input, time)
  expect_equal(var, "housing_tenant_2016")
})

test_that("picker_return_var returns the input code when there is no date", {
  input <- "c_flood"
  time <- NULL
  expect_equal(picker_return_var(input, time), input)
})

test_that("picker_return_var works fine when there is multiple times", {
  input <- "housing_tenant"
  time <- c(2011, 2016)
  expect_equal(
    picker_return_var(input, time),
    c("housing_tenant_2011", "housing_tenant_2016")
  )
})

test_that("picker_return_var works fine when there is multiple times (wrong)", {
  input <- "housing_tenant"
  time <- c(2012, 2017)
  expect_equal(
    picker_return_var(input, time),
    c("housing_tenant_2011", "housing_tenant_2016")
  )
})
