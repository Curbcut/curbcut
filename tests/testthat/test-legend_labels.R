test_that("legend_labels.q5 works", {
  vars1 <- vars_build(var_left = "housing_tenant", time = 2016, scale = "CSD")$vars
  vars2 <- vars_build(var_left = "housing_rent", time = 2021, scale = "CSD")$vars

  expect_equal(
    legend_labels(vars1),
    list(ggplot2::labs(x = "Tenant-occupied (%)", y = NULL))
  )
  expect_equal(
    legend_labels(vars2),
    list(ggplot2::labs(x = "Average rent ($)", y = NULL))
  )
})

# test_that("legend_labels.q100 works", {
#   vars <- vars_build(var_left = "c_flood", var_right = " ", df = "raster")
#
#   expect_equal(
#     legend_labels(vars),
#     list(structure(list(x = structure("Flood prev.", class = c(
#       "glue", "character"
#     )), y = NULL), class = "labels"))
#   )
# })

# test_that("legend_labels.qual works", {
#   vars <- structure(list(var_left = "climate_drought_2015", var_right = " "),
#     class = "qual"
#   )
#   expect_equal(
#     legend_labels(vars),
#     list(ggplot2::labs(x = "Drought vulnerability", y = NULL))
#   )
# })

test_that("legend_labels.bivar_ldelta_rq3 works", {
  vars <- vars_build(
    var_left = "housing_tenant",
    var_right = "climate_drought",
    scale = "CSD",
    time = c(2006, 2016)
  )
  time <- vars$time
  vars <- vars$vars

  expect_equal(
    legend_labels(vars, time = time),
    list(
      ggplot2::labs(
        x = "Drought (2015)",
        y = "Tenant (\u0394 2006 - 2016)"
      ),
      x_short = "Drought", y_short = "Tenant"
    )
  )
  expect_equal(
    legend_labels(vars, time = time, lang = "fr"),
    list(structure(list(x = "Sécheresse (2015)", y = "Locataire (Δ 2006 - 2016)"), class = "labels"),
      x_short = structure("Sécheresse", class = c("glue", "character")), y_short = structure("Locataire", class = c("glue", "character"))
    )
  )
})

test_that("legend_labels.delta works", {
  vars <- vars_build("housing_tenant", scale = "DA", time = c(2001, 2021))
  time <- vars$time
  vars <- vars$vars

  expect_equal(
    legend_labels(vars, time = time),
    list(ggplot2::labs(
      x = "Tenant-occupied (%) (\u0394 2001 - 2021)",
      y = NULL
    ))
  )

  expect_equal(
    legend_labels(vars, lang = "fr", time = time),
    list(structure(list(x = "Locataire (\u0394 2001 - 2021)", y = NULL), class = "labels"))
  )
})

test_that("legend_labels.bivar works", {
  vars <- structure(
    list(
      var_left = "alp",
      var_right = "housing_tenant"
    ),
    class = "bivar"
  )
  expect_equal(
    legend_labels(vars),
    list(
      ggplot2::labs(
        x = "Tenant",
        y = "Active living"
      ),
      x_short = "Tenant",
      y_short = "Active living"
    )
  )
})

test_that("legend_labels.delta_bivar works", {
  vars <- vars_build("housing_tenant",
    var_right = "housing_rent",
    scale = "DA", time = c(2001, 2021)
  )
  time <- vars$time
  vars <- vars$vars

  expect_equal(
    legend_labels(vars, time = time)[[1]],
    ggplot2::labs(
      x = "Avg. rent (Δ 2001 - 2021)",
      y = "Tenant (\u0394 2001 - 2021)"
    )
  )

  expect_equal(
    names(legend_labels(vars, time = time)),
    c("", "x_short", "y_short")
  )
})

test_that("legend_labels translation works", {
  vars <- vars_build("housing_tenant",
    var_right = "alp",
    scale = "DA", time = c(2001, 2021)
  )
  time <- vars$time
  vars <- vars$vars

  actual <- legend_labels(vars, lang = "fr", time = time)

  expect_equal(actual[[1]]$x, "Vie active (Δ 2001 - 2021)")
  expect_equal(actual[[1]]$y, "Locataire (Δ 2001 - 2021)")
  expect_equal(actual$x_short, "Vie active")
  expect_equal(actual$y_short, "Locataire")
})
