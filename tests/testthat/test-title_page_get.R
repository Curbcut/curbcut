test_that("title_page_get works", {
  result <- title_page_get(
    active_page = "alp",
    site_name = "Curbcut",
    lang = NULL
  )
  expect_equal(result, "Curbcut - Active living potential")

  result <- title_page_get(
    active_page = "anything_else",
    site_name = "Curbcut",
    lang = NULL
  )
  expect_equal(result, "Curbcut")
})
