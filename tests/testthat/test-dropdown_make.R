# Test generating a dropdown list for variables in one theme
test_that("dropdown_make generates a dropdown list for variables in one theme", {
  vars <- variables$var_code[1:10]
  compare <- FALSE
  expected_output <- list(Housing = list(`Tenant-occupied (%)` = "housing_tenant",
                                         `Average rent ($)` = "housing_rent", `Housing requiring major repairs (%)` = "housing_repairs",
                                         `Average property value ($)` = "housing_value", `Unaffordable housing (%)` = "housing_unafford",
                                         `Unsuitable housing (%)` = "housing_unsuit", `Renter housing stress (%)` = "housing_stress_renter",
                                         `Owner housing stress (%)` = "housing_stress_owner", `One-year housing mobility (%)` = "housing_mobility_one",
                                         `Five-year housing mobility (%)` = "housing_mobility_five"))
  actual_output <- dropdown_make(vars, compare)
  expect_identical(actual_output, expected_output)
})

# Test generating a dropdown list with 'no comparison' option
test_that("dropdown_make generates a dropdown list with 'no comparison' option", {
  vars <- variables$var_code[1:10]
  compare <- TRUE
  expected_output <- list(`----` = " ", Housing = list(`Tenant-occupied (%)` = "housing_tenant",
                                                       `Average rent ($)` = "housing_rent", `Housing requiring major repairs (%)` = "housing_repairs",
                                                       `Average property value ($)` = "housing_value", `Unaffordable housing (%)` = "housing_unafford",
                                                       `Unsuitable housing (%)` = "housing_unsuit", `Renter housing stress (%)` = "housing_stress_renter",
                                                       `Owner housing stress (%)` = "housing_stress_owner", `One-year housing mobility (%)` = "housing_mobility_one",
                                                       `Five-year housing mobility (%)` = "housing_mobility_five"))
  actual_output <- dropdown_make(vars, compare)
  expect_identical(actual_output, expected_output)
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

# Test generating a dropdown list for variables in multiple themes
test_that("dropdown_make generates a dropdown list for variables in multiple themes", {
  vars <- variables$var_code[c(1,15,30)]
  compare <- FALSE
  expected_output <- list(Housing = list(`Tenant-occupied (%)` = "housing_tenant"),
                          Income = list(`Income above $100k (%)` = "inc_high"), Household = list(
                            `Living alone (%)` = "family_one_person"))
  actual_output <- dropdown_make(vars, compare)
  expect_identical(actual_output, expected_output)
})
