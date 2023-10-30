test_that("explore_graph_labels q5 works", {
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

test_that("explore_graph_labels bivar works", {
  # Normal
  vars <- vars_build(c("housing_tenant"), "alp",
                     scale = "CSD", time = 2021)
  vars <- vars$vars
  actual <- explore_graph_labels(vars)
  expect_true("labels" %in% class(actual))
  expect_equal(names(actual), c("x", "y"))
  expect_true(!is.null(actual$x))
  expect_true(!is.null(actual$y))
})
