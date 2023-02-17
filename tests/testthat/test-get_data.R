test_that("build_vars works", {
  expect_equal(
    build_vars(
      var_left = "housing_tenant_2016", var_right = " ",
      df = "CMA_CSD"
    ),
    structure(list(var_left = "housing_tenant_2016", var_right = " "),
      class = "q5"
    )
  )

  expect_equal(
    build_vars(
      var_left = "housing_tenant_2016", var_right = " ",
      df = "CMA_building"
    ),
    structure(list(var_left = "housing_tenant_2016", var_right = " "),
      class = "q5"
    )
  )

  expect_equal(
    build_vars(
      var_left = "housing_tenant_2016", var_right = "inc_limat_2016",
      df = "CMA_CSD"
    ),
    structure(list(var_left = "housing_tenant_2016", var_right = "inc_limat_2016"),
      class = "bivar"
    )
  )

  expect_equal(
    build_vars(
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
    structure(
      list(
        var_left = c(
          "housing_tenant_2006",
          "housing_tenant_2016"
        ),
        var_right = c(
          "inc_limat_2006",
          "inc_limat_2016"
        )
      ),
      class = "delta_bivar"
    )
  )

  expect_equal(
    build_vars(
      var_left = c(
        "housing_tenant_2006",
        "housing_tenant_2016"
      ),
      var_right = c(" "),
      df = "CMA_DA"
    ),
    structure(
      list(
        var_left = c(
          "housing_tenant_2006",
          "housing_tenant_2016"
        ),
        var_right = c(" ")
      ),
      class = "delta"
    )
  )

  expect_equal(
    build_vars(
      var_left = c(
        "housing_tenant_2006",
        "housing_tenant_2016"
      ),
      var_right = c("inc_limat_2006"),
      df = "CMA_DA"
    ),
    structure(
      list(
        var_left = c(
          "housing_tenant_2006",
          "housing_tenant_2016"
        ),
        var_right = c("inc_limat_2006")
      ),
      class = "bivar_ldelta_rq3"
    )
  )

  expect_equal(
    build_vars(
      var_left = "housing_tenant_2016", var_right = " ",
      df = "unknown_df"
    ),
    structure(list(var_left = "housing_tenant_2016", var_right = " "),
      class = "unknown_df"
    )
  )

  expect_equal(
    build_vars(
      var_left = c(
        "housing_tenant_2016",
        "housing_tenant_2016"
      ),
      var_right = c(" "),
      df = "CMA_DA"
    ),
    structure(
      list(
        var_left = c(
          "housing_tenant_2016",
          "housing_tenant_2016"
        ),
        var_right = c(" ")
      ),
      class = "NA"
    )
  )

  expect_equal(
    build_vars(
      var_left = "climate_drought_2016", var_right = " ",
      df = "grid_grid"
    ),
    structure(list(var_left = "climate_drought_2016", var_right = " "),
      class = "q5"
    )
  )
})
