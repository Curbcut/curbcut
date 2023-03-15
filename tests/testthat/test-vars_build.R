test_that("vars_build works", {
  expect_equal(
    vars_build(
      var_left = "housing_tenant_2016", var_right = " ",
      df = "CMA_CSD"
    ),
    structure(list(var_left = structure("housing_tenant_2016", class = c(
      "pct",
      "character"
    )), var_right = " "), class = "q5")
  )

  expect_equal(
    vars_build(
      var_left = "housing_tenant_2016", var_right = " ",
      df = "CMA_building"
    ),
    structure(list(var_left = structure("housing_tenant_2016", class = c(
      "pct",
      "character"
    )), var_right = " "), class = "q5")
  )

  expect_equal(
    vars_build(
      var_left = "housing_tenant_2016", var_right = "inc_limat_2016",
      df = "CMA_CSD"
    ),
    structure(list(var_left = structure("housing_tenant_2016", class = c(
      "pct",
      "character"
    )), var_right = structure("inc_limat_2016", class = c(
      "pct",
      "character"
    ))), class = "bivar")
  )

  expect_equal(
    vars_build(
      var_left = c(
        "housing_tenant_2006",
        "housing_tenant_2016"
      ),
      var_right = c(
        "inc_limat_2006",
        "inc_limat_2016"
      ),
      df = "CMA_DA"
    ),
    structure(list(var_left = structure(c(
      "housing_tenant_2006",
      "housing_tenant_2016"
    ), class = c("pct", "character")), var_right = structure(c(
      "inc_limat_2006",
      "inc_limat_2016"
    ), class = c("pct", "character"))), class = "delta_bivar")
  )

  expect_equal(
    vars_build(
      var_left = c(
        "housing_tenant_2006",
        "housing_tenant_2016"
      ),
      var_right = c(" "),
      df = "CMA_DA"
    ),
    structure(list(var_left = structure(c(
      "housing_tenant_2006",
      "housing_tenant_2016"
    ), class = c("pct", "character")), var_right = " "), class = "delta")
  )

  expect_equal(
    vars_build(
      var_left = c(
        "housing_tenant_2006",
        "housing_tenant_2016"
      ),
      var_right = c("inc_limat_2006"),
      df = "CMA_DA"
    ),
    structure(list(var_left = structure(c(
      "housing_tenant_2006",
      "housing_tenant_2016"
    ), class = c("pct", "character")), var_right = structure("inc_limat_2006", class = c(
      "pct",
      "character"
    ))), class = "bivar_ldelta_rq3")
  )

  expect_equal(
    vars_build(
      var_left = "housing_tenant_2016", var_right = " ",
      df = "unknown_df"
    ),
    structure(list(var_left = structure("housing_tenant_2016", class = c(
      "pct",
      "character"
    )), var_right = " "), class = "unknown_df")
  )

  expect_equal(
    vars_build(
      var_left = c(
        "housing_tenant_2016",
        "housing_tenant_2016"
      ),
      var_right = c(" "),
      df = "CMA_DA"
    ),
    structure(list(
      var_left = structure(
        c(
          "housing_tenant_2016",
          "housing_tenant_2016"
        ),
        class = c("pct", "character")
      ),
      var_right = " "
    ), class = "NA")
  )

  expect_equal(
    vars_build(
      var_left = "climate_drought_2017", var_right = " ",
      df = "grid_grid"
    ),
    structure(list(var_left = structure("climate_drought_20176", class = c(
      "ind",
      "character"
    )), var_right = " "), class = c("q5_ind", "q5"))
  )

  expect_equal(
    vars_build(var_left = "c_flood", var_right = " ", df = "raster"),
    structure(list(var_left = structure("c_flood", class = c(
      "qual",
      "character"
    )), var_right = " "), class = "q100")
  )
})
