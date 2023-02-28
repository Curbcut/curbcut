library(shinytest2)

test_that("{shinytest2} recording: legend_delta_bivar", {
  app <- AppDriver$new(variant = platform_variant(), name = "legend_delta_bivar", 
      height = 961, width = 1619)
  app$expect_values()
  app$expect_screenshot()
})
