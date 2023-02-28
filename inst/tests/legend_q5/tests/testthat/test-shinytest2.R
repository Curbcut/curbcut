library(shinytest2)

test_that("{shinytest2} recording: legend_q5", {
  app <- AppDriver$new(variant = platform_variant(), name = "legend_q5", height = 961, 
      width = 1619)
  app$expect_screenshot()
  app$expect_values()
})
