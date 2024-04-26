test_that("vars_build works", {
  q5 <- vars_build(
    var_left = "housing_tenant", var_right = " ",
    scale = "CSD", time = 2016
  )
  expect_type(q5, "list")
  expect_equal(q5$vars$var_left, structure("housing_tenant", class = c("pct", "scalar", "character", "ondisk")))
  expect_equal(q5$vars$var_right, " ")
  expect_equal(attributes(q5$vars)$class, c("q5", "scalar"))
  expect_equal(q5$time$var_left, 2016)


  q5_building <- vars_build(
    var_left = "housing_tenant", var_right = " ",
    scale = "building", time = 2016
  )
  expect_type(q5_building, "list")
  expect_equal(q5_building$vars$var_left, structure("housing_tenant", class = c("pct", "scalar", "character", "ondisk")))
  expect_equal(q5_building$vars$var_right, " ")
  expect_equal(attributes(q5_building$vars)$class, c("q5", "scalar"))
  expect_equal(q5_building$time$var_left, 2016)



  expect_true(
    c("bivar", "scalar") %in% {vars_build(
      var_left = "housing_tenant", var_right = "alp",
      scale = "CSD", time = 2016
    )$vars |> class()} |> all()
  )

  expect_true(
    c("delta_bivar", "scalar") %in% {vars_build(
      var_left = c(
        "housing_tenant"
      ),
      var_right = c(
        "climate_drought"
      ),
      scale = "DA",
      time = c(2016, 2021)
    )$vars |> class()} |> all()

  )

  expect_equal(
    vars_build(
      var_left = c(
        "housing_tenant"
      ),
      var_right = c(" "),
      scale = "DA",
      time = c(2006, 2016)
    ),
    list(vars = structure(list(var_left = structure("housing_tenant", class = c(
      "pct",
      "scalar", "character", "ondisk"
    )), var_right = " "), class = c(
      "delta",
      "scalar"
    )), time = list(var_left = c(2006, 2016)))
  )

  expect_true(
    c("delta_bivar", "scalar") %in% {vars_build(
      var_left = c(
        "housing_tenant"
      ),
      var_right = c("alp"),
      scale = "DA",
      time = c(2006, 2016)
    )$vars |> class()} |> all()
  )

  expect_equal(
    vars_build(
      var_left = "c_priority",
      var_right = " ",
      scale = "unknown_scale",
      time = 2016
    )$vars |> class(),
    "unknown_scale"
  )

  expect_true(
    c("q5", "scalar") %in% {vars_build(
      var_left = c(
        "housing_tenant"
      ),
      var_right = c(" "),
      scale = "DA",
      time = c("2016", "2016")
    )$vars |> class()} |> all()
  )

  expect_true(
    c("pct", "scalar", "character") %in% {
      vars_build(
        var_left = c(
          "housing_tenant"
        ),
        var_right = c(" "),
        scale = "DA",
        time = c("2016", "2016")
      )$vars$var_left |> class()
    } |> all()
  )

  expect_equal(
    vars_build(
      var_left = "climate_drought", var_right = " ",
      scale = "grd50", time = "2015"
    )$vars,
    structure(list(var_left = structure("climate_drought", class = c(
      "ind",
      "ordinal",
      "character",
      "postgresql"
    )), var_right = " "), class = c("q5_ind", "q5", "ordinal"))
  )
})
