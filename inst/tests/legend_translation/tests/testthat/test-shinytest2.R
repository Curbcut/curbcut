library(shinytest2)

test_that("{shinytest2} recording: legend_translation", {
  app <- AppDriver$new(variant = platform_variant(), name = "legend_translation", 
      height = 961, width = 1619)
  app$expect_values()
  app$click("language-language_button")
  app$expect_values()
  app$click("language-language_button")
  app$expect_values()
  app$click("language-language_button")
  app$expect_values()
  app$expect_screenshot()
})
