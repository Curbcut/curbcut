# Test generating a dropdown list for variables in one theme
test_that("dropdown_make generates a dropdown list for variables in one theme", {
  vars <- variables$var_code
  compare <- FALSE
  actual_output <- dropdown_make(vars, compare)
  expect_identical(as.numeric(vec_dep(actual_output)), 3)
  expect_identical(unname(unlist(actual_output))[
    order(unname(unlist(actual_output)))
  ], vars[order(vars)])
})

# Test generating a dropdown list with 'no comparison' option
test_that("dropdown_make generates a dropdown list with 'no comparison' option", {
  vars <- variables$var_code
  compare <- TRUE
  actual_output <- dropdown_make(vars, compare)
  expect_identical(as.numeric(vec_dep(actual_output)), 3)
  expect_identical(unname(unlist(actual_output))[
    order(unname(unlist(actual_output)))
  ], c(" ", vars[order(vars)]))
})

# Test generating a dropdown list with no variables selected
test_that("dropdown_make generates an empty dropdown list when no variables are selected", {
  vars <- character(0)
  compare <- FALSE
  expected_output <- structure(list(), names = character(0))
  actual_output <- dropdown_make(vars, compare)
  expect_identical(actual_output, expected_output)
})

# Test generating a dropdown list with duplicate variable codes
test_that("dropdown_make generates a dropdown list with duplicate variable codes", {
  vars <- rep(variables$var_code[1], 2)
  compare <- FALSE
  expected_output <- list(Housing = list(`Tenant-occupied (%)` = "housing_tenant"))
  actual_output <- dropdown_make(vars, compare)
  expect_identical(actual_output, expected_output)
})
