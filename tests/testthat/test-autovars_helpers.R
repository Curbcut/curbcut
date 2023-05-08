test_that("autovars_groupnames works", {
  major_grouping <- autovars_groupnames(id = "access")
  expect_true(is.character(major_grouping))
  expect_true(length(major_grouping) > 1)
})

test_that("autovars_groupnames works", {
  common_widgets <- autovars_common_widgets(id = "housing")

  expect_equal(names(common_widgets), c("time", "widgets"))
  expect_equal(length(common_widgets$time) > 1, TRUE)
})
