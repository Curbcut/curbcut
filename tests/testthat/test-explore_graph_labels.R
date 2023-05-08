test_that("explore_graph_labels works", {

  # Delta
  vars <- vars_build(c("housing_tenant_2016", "housing_tenant_2021"),
                     df = "city_CSD")
  actual <- explore_graph_labels(vars)
  expect_true("labels" %in% class(actual))
  expect_equal(names(actual), c("x", "y"))
  expect_true(!is.null(actual$x))
  expect_true(!is.null(actual$y))

  # Normal
  vars <- vars_build(c("housing_tenant_2021"),
                     df = "city_CSD")
  actual <- explore_graph_labels(vars)
  expect_true("labels" %in% class(actual))
  expect_equal(names(actual), c("x", "y"))
  expect_true(!is.null(actual$x))
  expect_true(is.null(actual$y))

})
