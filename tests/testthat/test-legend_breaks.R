test_that("legend_breaks.q5 works", {
  vars1 <- vars_build(var_left = "housing_tenant", scale = "CSD", time = 2021)$vars
  data1 <- data_get(vars = vars1, scale = "CSD", region = "CMA")
  vars2 <- vars_build(var_left = "housing_rent", scale = "DA", time = 2011)$vars
  data2 <- data_get(vars = vars2, scale = "DA", region = "CMA")
  # vars3 <- vars_build(var_left = "climate_drought_2015", df = "grid_grid50")

  expect_equal(
    all(grepl("^\\d", legend_breaks(vars1, scale = "CSD", data = data1))),
    TRUE
  )
  expect_equal(
    all(grepl("^\\$\\d", legend_breaks(vars2, scale = "DA", data = data2))),
    TRUE
  )
})

test_that("legend_breaks.q5_ind works", {
  # vars1 <- vars_build(var_left = "climate_drought", scale = "grid50", time = 2015)
  vars2 <- vars_build(var_left = "alp", scale = "grid50", time = 2016)$vars
  data2 <- data_get(vars = vars2, scale = "DA", region = "island")

  # expect_equal(
  #   unname(legend_breaks(vars1, df = "grid_grid50")),
  #   structure(c("Insig.", "Minor", "Mod.", "Elev.", "Major"))
  # )

  expect_equal(
    legend_breaks(vars2, scale = "CSD", data = data2),
    structure(list("Low", NULL, NULL, NULL, "High"))
  )
})

test_that("legend_breaks.q100 works", {
  vars <- vars_build(var_left = "housing_tenant", scale = "raster", time = 2021)$vars
  expect_equal(
    legend_breaks(vars, scale = "raster"),
    list(
      "Low", NULL, NULL, NULL, NULL,
      "High"
    )
  )
})

# test_that("legend_breaks.qual works", {
#
# })

# test_that("legend_breaks.bivar_ldelta_rq3 works", {
#   vars <- vars_build(
#     var_left = c(
#       "housing_tenant_2006",
#       "housing_tenant_2016"
#     ),
#     var_right = "alp_2016",
#     df = "city_CSD"
#   )
#   data <- data_get(
#     vars = vars,
#     df = "city_CSD"
#   )
#   actual <- legend_breaks(vars,
#     data = data,
#     df = "city_CSD"
#   )
#   expect_equal(
#     names(actual),
#     c("x", "y")
#   )
#   expect_equal(
#     sum(grepl("%$", actual[[2]])),
#     sum(c(T, T, T, T))
#   )
#   expect_equal(
#     length(actual$x), 4
#   )
# })

# test_that("legend_breaks.delta works", {
#   vars <- vars_build(var_left = c(
#     "housing_tenant_2006",
#     "housing_tenant_2016"
#   ), df = "CMA_CT")
#   expect_equal(
#     legend_breaks(vars, df = "CMA_CT"),
#     c("-10%", "-2%", "+2%", "+10%")
#   )
# })

# test_that("legend_breaks.bivar works", {
#   vars <- vars_build(
#     var_left = "climate_drought_2015",
#     var_right = "housing_tenant_2016",
#     df = "city_CSD"
#   )
#   expect_equal(
#     legend_breaks(vars, df = "city_CSD"),
#     list(
#       x = c("25.91%", "60.37%", "69.61%", "73.35%"),
#       y = c(
#         "1.42",
#         "3.13", "3.40", "3.81"
#       )
#     )
#   )
# })
#
# test_that("legend_breaks.delta_bivar works", {
#   vars <- vars_build(
#     var_left = c("housing_rent_2006", "housing_rent_2016"),
#     var_right = c(
#       "housing_tenant_2006",
#       "housing_tenant_2016"
#     ),
#     df = "city_CSD"
#   )
#   data <- data_get(
#     vars = vars,
#     df = "city_CSD"
#   )
#   expect_equal(
#     legend_breaks(vars, data = data),
#     list(
#       x = c("-8.27%", "-4.75%", "-1.42%", "4.11%"),
#       y = c(
#         "20.93%",
#         "26.08%", "31.48%", "38.09%"
#       )
#     )
#   )
# })
