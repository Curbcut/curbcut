variables <- qs::qread("resources/variables.qs")

test_that("legend_labels.q5 works", {
  vars <- structure(list(var_left = "housing_tenant_2016", var_right = " "),
                    class = "q5")
  expect_equal(legend_labels(vars),
               list(ggplot2::labs(x = "Tenant-occupied (%)", y = NULL)))
})

# test_that("legend_labels.q100 works", {
#
# })

test_that("legend_labels.qual works", {
  vars <- structure(list(var_left = "climate_flood", var_right = " "),
                    class = "qual")
  expect_equal(legend_labels(vars),
               list(ggplot2::labs(x = "Flood vulnerability", y = NULL)))
})

# test_that("bivariate_xdelta_yq3 works", {
#
# })

test_that("legend_labels.delta works", {
  vars <- structure(list(var_left = c("housing_tenant_2006",
                                      "housing_tenant_2016"), var_right = " "),
                    class = "delta")
  expect_equal(legend_labels(vars),
               list(ggplot2::labs(x = "Tenant (\u0394 2006 - 2016)", y = NULL)))
})

test_that("legend_labels.bivar works", {
  vars <- structure(list(var_left = "climate_flood",
                         var_right = "housing_tenant_2016"),
                    class = "bivar")
  expect_equal(legend_labels(vars),
               list(ggplot2::labs(x = "Tenant-occupied (%) (2016)",
                                  y = "Flood vulnerability"),
                    x_short = "Tenant",
                    y_short = "Flood"))
})

test_that("legend_labels.delta_bivar works", {
  vars <- structure(list(var_left = c("inc_50_2006", "inc_50_2016"),
                         var_right = c("housing_tenant_2006",
                                       "housing_tenant_2016")),
                    class = "delta_bivar")
  expect_equal(legend_labels(vars),
               list(ggplot2::labs(x = "Tenant (\u0394 2006 - 2016)",
                                  y = "Inc. <$50k (\u0394 2006 - 2016)"),
                    x_short = "Tenant",
                    y_short = "Inc. <$50k"))
})
