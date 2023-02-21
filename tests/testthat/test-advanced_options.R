test_that("adv_opt_region is a shiny tagList", {
  expect_s3_class(adv_opt_lock_selection("en"), "shiny.tag.list")
})

test_that("adv_opt_lock_selection is a shiny tagList", {
  expect_s3_class(adv_opt_lock_selection("en"), "shiny.tag.list")
})

