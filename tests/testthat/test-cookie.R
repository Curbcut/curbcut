test_that("use_curbut_cookie function loads necessary JavaScript files", {
  output <- use_curbcut_cookie()
  expect_s3_class(output, "shiny.tag.list")
  expect_true(length(output) == 2)
  expect_s3_class(output[[1]], "shiny.tag")
  expect_equal(output[[1]]$name, "head")
  expect_s3_class(output[[1]]$children[[1]], "shiny.tag")
  expect_equal(output[[1]]$children[[1]]$name, "script")
  expect_equal(output[[1]]$children[[1]]$attribs$src, "curbcut_js/cookie.js")
  expect_s3_class(output[[2]], "shiny.tag")
  expect_equal(output[[2]]$name, "head")
  expect_equal(output[[2]]$children[[1]]$attribs$src, "https://cdn.jsdelivr.net/npm/js-cookie@rc/dist/js.cookie.min.js")
})

# Impossible to test with shinytest2 cookies as it's then reviewd on a headless
# browser, which do not take cookies.
# test_that("cookies works", {
#   # Don't run these tests on the CRAN build servers
#   skip_on_cran()
#
#   appdir <- system.file(package = "curbcut", "tests/cookies")
#   test_app(appdir)
# })
