test_that("legend_breaks.q5 works", {
  vars1 <- vars_build(var_left = "housing_tenant_2016", df = "CMA_CSD")
  vars2 <- vars_build(var_left = "housing_value_2011", df = "CMA_DA")
  vars3 <- vars_build(var_left = "climate_drought", df = "grid_grid")
  expect_equal(
    legend_breaks(vars1, df = "CMA_CSD"),
    structure(c("0%", "20%", "40%", "60%", "80%", "100%"))
  )
  expect_equal(
    legend_breaks(vars2, df = "CMA_DA"),
    structure(c("$0K", "$200K", "$400K", "$600K", "$800K", "$1,000K"))
  )
})

test_that("legend_breaks.q5_ind works", {
  vars1 <- vars_build(var_left = "climate_drought", df = "grid_grid")
  vars2 <- vars_build(var_left = "canale", df = "grid_grid")

  expect_equal(
    legend_breaks(vars1, df = "grid_grid"),
    structure(c(NA, "Insig.", "Minor", "Mod.", "Elev.", "Major"))
  )

  expect_equal(
    legend_breaks(vars2, df = "CMA_CSD"),
    structure(list("Low", NULL, NULL, NULL, "High"))
  )
})

test_that("legend_breaks.q100 works", {
  vars <- vars_build(var_left = "climate_flood", df = "raster")
  expect_equal(
    legend_breaks(vars, df = "raster"),
    list(
      "Low", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
      "High"
    )
  )
})

# test_that("legend_breaks.qual works", {
#
# })

test_that("legend_breaks.bivar_ldelta_rq3 works", {
  vars <- vars_build(
    var_left = c(
      "housing_tenant_2006",
      "housing_tenant_2016"
    ),
    var_right = "canale_2016",
    df = "city_CSD"
  )
  data <- data_get(
    vars = vars,
    df = "city_CSD"
  )
  expect_equal(
    legend_breaks(vars,
      data = data,
      df = "city_CSD"
    ),
    list(x = c("-0.6", "1.8", "4.1", "10.6"), y = c(
      "-8.27%", "-4.77%",
      "-1.51%", "4.24%"
    ))
  )
})

test_that("legend_breaks.delta works", {
  vars <- vars_build(var_left = c(
    "housing_tenant_2006",
    "housing_tenant_2016"
  ), df = "CMA_CT")
  expect_equal(
    legend_breaks(vars, df = "CMA_CT"),
    c("-10%", "-2%", "+2%", "+10%")
  )
})

test_that("legend_breaks.bivar works", {
  vars <- vars_build(
    var_left = "climate_flood",
    var_right = "housing_tenant_2016",
    df = "grid_grid"
  )
  expect_equal(
    legend_breaks(vars, df = "grid_grid"),
    list(x = c("0.24%", "20.2%", "57.12%", "100%"), y = c(
      "0", "1",
      "3", "5"
    ))
  )
})

test_that("legend_breaks.delta_bivar works", {
  vars <- vars_build(
    var_left = c("inc_50_2006", "inc_50_2016"),
    var_right = c(
      "housing_tenant_2006",
      "housing_tenant_2016"
    ),
    df = "city_CSD"
  )
  data <- data_get(
    vars = vars,
    df = "city_CSD"
  )
  expect_equal(
    legend_breaks(vars, data = data),
    list(x = c("-8.27%", "-4.77%", "-1.51%", "4.24%"), y = c(
      "-27.72%",
      "-21.88%", "-18.54%", "-16.09%"
    ))
  )
})

