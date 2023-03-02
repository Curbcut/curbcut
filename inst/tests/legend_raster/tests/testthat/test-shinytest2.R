library(shinytest2)

test_that("{shinytest2} recording: legend_raster", {
  app <- AppDriver$new(variant = platform_variant(), name = "legend_raster", height = 961, 
      width = 1619)
  app$expect_values()
  app$expect_screenshot()
})
