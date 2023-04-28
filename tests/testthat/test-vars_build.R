test_that("vars_build works", {
  expect_equal(
    vars_build(
      var_left = "housing_tenant_2016", var_right = " ",
      df = "CMA_CSD"
    ),
    structure(list(var_left = structure("housing_tenant_2016", class = c(
      "pct",
      "scalar", "character"
    )), var_right = " "), class = c("q5", "scalar"))
  )

  expect_equal(
    vars_build(
      var_left = "housing_tenant_2016", var_right = " ",
      df = "CMA_building"
    ),
    structure(list(var_left = structure("housing_tenant_2016", class = c(
      "pct",
      "scalar", "character"
    )), var_right = " "), class = c("q5", "scalar"))
  )

  expect_equal(
    vars_build(
      var_left = "housing_tenant_2016", var_right = "canale_2016",
      df = "CMA_CSD"
    ) |> class(),
    c("bivar", "scalar")
  )

  expect_equal(
    vars_build(
      var_left = c(
        "housing_tenant_2016",
        "housing_tenant_2021"
      ),
      var_right = c(
        "climate_drought_2015",
        "climate_drought_2022"
      ),
      df = "city_DA"
    ) |> class(),
    c("delta_bivar", "scalar")
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
    structure(
      list(var_left = structure(c(
        "housing_tenant_2006",
        "housing_tenant_2016"
      ), class = c("pct", "scalar", "character")), var_right = " "),
      class = c("delta", "scalar")
    )
  )

  expect_equal(
    vars_build(
      var_left = c(
        "housing_tenant_2006",
        "housing_tenant_2016"
      ),
      var_right = c("canale_2006"),
      df = "city_DA"
    ),
    structure(list(var_left = structure(c(
      "housing_tenant_2006",
      "housing_tenant_2016"
    ), class = c("pct", "scalar", "character")), var_right = structure("canale_2006", class = c(
      "ind", "scalar",
      "character"
    ))), class = c("bivar_ldelta_rq3", "scalar"))
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
          "housing_tenant_2016"
        ),
        class = c("pct", "scalar", "character")
      ),
      var_right = " "
    ), class = c("q5", "scalar"))
  )

  expect_equal(
    vars_build(
      var_left = "climate_drought_2015", var_right = " ",
      df = "grid_grid50"
    ),
    structure(list(var_left = structure("climate_drought_2015", class = c(
      "ind",
      "ordinal",
      "character"
    )), var_right = " "), class = c("q5_ind", "q5", "ordinal"))
  )

})
