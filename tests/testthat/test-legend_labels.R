test_that("legend_labels.q5 works", {
  vars1 <- structure(list(var_left = "housing_tenant_2016", var_right = " "),
                    class = "q5")
  vars2 <- structure(list(var_left = "inc_limat_2021", var_right = " "),
                     class = "q5")
  expect_equal(legend_labels(vars1),
               list(ggplot2::labs(x = "Tenant-occupied (%)", y = NULL)))
  expect_equal(legend_labels(vars2),
               list(ggplot2::labs(x = "Low income", y = NULL)))
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

test_that("legend_labels.bivar_ldelta_rq3 works", {
  vars <- structure(list(var_left = c("housing_tenant_2006",
                                      "housing_tenant_2016"),
                         var_right = "climate_drought_2016"),
                    class = "bivar_ldelta_rq3")
  expect_equal(legend_labels(vars),
               list(ggplot2::labs(x = "Drought (2016)",
                                  y = "Tenant (\u0394 2006 - 2016)"),
                    x_short = "Drought", y_short = "Tenant"))
})

test_that("legend_labels.delta works", {
  vars <- structure(list(var_left = c("housing_tenant_2006",
                                      "housing_tenant_2016"), var_right = " "),
                    class = "delta")
  expect_equal(legend_labels(vars),
               list(ggplot2::labs(x = "Tenant-occupied (%) (\u0394 2006 - 2016)",
                                  y = NULL)))
})

test_that("legend_labels.bivar works", {
  vars <- structure(list(var_left = "climate_flood",
                         var_right = "housing_tenant_2016"),
                    class = "bivar")
  expect_equal(legend_labels(vars),
               list(ggplot2::labs(x = "Tenant-occupied (%) (2016)",
                                  y = "Flood"),
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
