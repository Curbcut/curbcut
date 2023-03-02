test_that("legend render works", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  appdir <- system.file(package = "curbcut", "tests/legend_translation")
  shinytest2::test_app(appdir)

  appdir <- system.file(package = "curbcut", "tests/legend_q5")
  shinytest2::test_app(appdir)

  appdir <- system.file(package = "curbcut", "tests/legend_delta_bivar")
  shinytest2::test_app(appdir)

  appdir <- system.file(package = "curbcut", "tests/legend_delta")
  shinytest2::test_app(appdir)

  appdir <- system.file(package = "curbcut", "tests/legend_raster")
  shinytest2::test_app(appdir)

  expect_s3_class(legend_UI("canale"), "shiny.tag")
})
