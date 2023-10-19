test_that("explore_graph_labels works", {
  # # Delta
  # vars <- vars_build(c("housing_tenant"),
  #   scale = "CSD", time = c(2016, 2021)
  # )
  # vars <- vars$vars
  # actual <- explore_graph_labels(vars)
  # expect_true("labels" %in% class(actual))
  # expect_equal(names(actual), c("x", "y"))
  # expect_true(!is.null(actual$x))
  # expect_true(!is.null(actual$y))

  # Normal
  vars <- vars_build(c("housing_tenant"),
                     scale = "CSD", time = 2021)
  vars <- vars$vars
  actual <- explore_graph_labels(vars)
  expect_true("labels" %in% class(actual))
  expect_equal(names(actual), c("x", "y"))
  expect_true(!is.null(actual$x))
  expect_true(is.null(actual$y))
})
