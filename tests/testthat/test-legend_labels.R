test_that("legend_labels.q5 works", {
  vars1 <- structure(list(var_left = "housing_tenant_2016", var_right = " "),
    class = "q5"
  )
  vars2 <- structure(list(var_left = "inc_limat_2021", var_right = " "),
    class = "q5"
  )
  expect_equal(
    legend_labels(vars1),
    list(ggplot2::labs(x = "Tenant-occupied (%)", y = NULL))
  )
  expect_equal(
    legend_labels(vars2),
    list(ggplot2::labs(x = "Low income", y = NULL))
  )
})

test_that("legend_labels.q100 works", {
  vars <- vars_build(var_left = "c_flood", var_right = " ", df = "raster")

  expect_equal(
    legend_labels(vars),
    list(structure(list(x = structure("Flood prev.", class = c(
      "glue", "character"
    )), y = NULL), class = "labels"))
  )
})

test_that("legend_labels.qual works", {
  vars <- structure(list(var_left = "climate_flood_2017", var_right = " "),
    class = "qual"
  )
  expect_equal(
    legend_labels(vars),
    list(ggplot2::labs(x = "Flood vulnerability", y = NULL))
  )
})

test_that("legend_labels.bivar_ldelta_rq3 works", {
  vars <- structure(
    list(
      var_left = c(
        "housing_tenant_2006",
        "housing_tenant_2016"
      ),
      var_right = "climate_drought_2017"
    ),
    class = "bivar_ldelta_rq3"
  )
  expect_equal(
    legend_labels(vars),
    list(
      ggplot2::labs(
        x = "Drought (2017)",
        y = "Tenant (\u0394 2006 - 2016)"
      ),
      x_short = "Drought", y_short = "Tenant"
    )
  )
  expect_equal(
    legend_labels(vars, lang = "fr"),
    list(structure(list(x = "Sécheresses (2016)", y = "Locataire (Δ 2006 - 2016)"), class = "labels"),
      x_short = structure("Sécheresses", class = c("glue", "character")), y_short = structure("Locataire", class = c("glue", "character"))
    )
  )
})

test_that("legend_labels.delta works", {
  vars <- structure(
    list(var_left = c(
      "housing_tenant_2006",
      "housing_tenant_2016"
    ), var_right = " "),
    class = "delta"
  )
  expect_equal(
    legend_labels(vars),
    list(ggplot2::labs(
      x = "Tenant-occupied (%) (\u0394 2006 - 2016)",
      y = NULL
    ))
  )

  expect_equal(
    legend_labels(vars, lang = "fr"),
    list(structure(list(x = "Locataire (Δ 2006 - 2016)", y = NULL), class = "labels"))
  )
})

test_that("legend_labels.bivar works", {
  vars <- structure(
    list(
      var_left = "climate_flood_2017",
      var_right = "housing_tenant_2016"
    ),
    class = "bivar"
  )
  expect_equal(
    legend_labels(vars),
    list(
      ggplot2::labs(
        x = "Tenant-occupied (%) (2016)",
        y = "Flood"
      ),
      x_short = "Tenant",
      y_short = "Flood"
    )
  )
})

test_that("legend_labels.delta_bivar works", {
  vars <- structure(
    list(
      var_left = c("inc_50_2006", "inc_50_2016"),
      var_right = c(
        "housing_tenant_2006",
        "housing_tenant_2016"
      )
    ),
    class = "delta_bivar"
  )
  expect_equal(
    legend_labels(vars),
    list(
      ggplot2::labs(
        x = "Tenant (\u0394 2006 - 2016)",
        y = "Inc. <$50k (\u0394 2006 - 2016)"
      ),
      x_short = "Tenant",
      y_short = "Inc. <$50k"
    )
  )
})

test_that("legend_labels translation works", {
  vars <- structure(list(var_left = "housing_tenant_2016", var_right = "canale_2016"),
    class = "bivar"
  )
  expect_equal(
    legend_labels(vars, lang = "fr"),
    list(structure(list(x = "Indice AVA-Can (2016)", y = "Locataire (2016)"), class = "labels"),
      x_short = structure("AVA-Can", class = c("glue", "character")), y_short = structure("Locataire", class = c("glue", "character"))
    )
  )
})
