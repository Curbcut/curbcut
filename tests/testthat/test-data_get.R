test_that("data_get.q5  works", {
  vars <- vars_build(var_left = "housing_tenant_2016", df = "city_CSD")
  output <- data_get(vars = vars, df = "city_CSD")
  expect_equal(nrow(output) > 15, TRUE)
  expect_equal(
    names(output),
    c("ID", "var_left", "var_left_q3", "var_left_q5", "group")
  )
  expect_equal(class(output[[1]]), "character")
  expect_equal(class(output[[2]]), "numeric")
})

# test_that("data_get.bivar works", {
#   vars <- vars_build(
#     var_left = c("canale_2016"),
#     var_right = c("housing_tenant_2016"),
#     df = "city_CSD"
#   )
#   output <- data_get(
#     vars = vars,
#     df = "city_CSD"
#   )
#   expect_equal(nrow(output) > 15, TRUE)
#   expect_equal(names(output),
#                c("ID", "var_left", "var_left_q3", "var_left_q5", "group"))
#   expect_equal(class(output[[1]]), "character")
#   expect_equal(class(output[[2]]), "numeric")
# })

test_that("data_get.delta works", {
  vars <- vars_build(
    var_left = c("housing_tenant_1996", "housing_tenant_2016"),
    var_right = c(" "),
    df = "city_CSD"
  )
  output <- data_get(vars = vars, df = "city_CSD")
  expect_equal(nrow(output) > 15, TRUE)
  # ORDER IS IMPORTANT FOR TABLE VIEW (PANEL_VIEW)
  expect_equal(names(output), c(
    "ID", "var_left_1", "var_left_2", "var_left",
    "var_left_q5", "group"
  ))
  expect_equal(class(output[[1]]), "character")
  expect_equal(class(output[[2]]), "numeric")
  expect_equal(class(output[[4]]), "numeric")
})

test_that("data_get.delta_bivar works", {
  vars <- vars_build(
    var_left = c("housing_tenant_1996", "housing_tenant_2016"),
    var_right = c("housing_value_1996", "housing_value_2016"),
    df = "city_CSD"
  )

  output <- data_get(vars = vars, df = "city_CSD")
  expect_equal(nrow(output) > 15, TRUE)
  # ORDER IS IMPORTANT FOR TABLE VIEW (PANEL_VIEW)
  expect_equal(names(output), c(
    "ID", "var_left_1", "var_left_2", "var_left",
    "var_right_1", "var_right_2", "var_right",
    "var_left_q3", "var_right_q3", "group"
  ))
  expect_equal(class(output[[1]]), "character")
  expect_equal(class(output[[2]]), "numeric")
  expect_equal(class(output[[4]]), "numeric")
  expect_equal(class(output[[7]]), "numeric")
})

# test_that("data_get.bivar_ldelta_rq3 works", {
#   vars <- vars_build(
#     var_left = c("housing_tenant_1996", "housing_tenant_2016"),
#     var_right = c("canale_2016"),
#     df = "city_CSD"
#   )
#   output <- data_get(vars = vars, df = "city_CSD")
#
#   output <- data_get(vars = vars, df = "city_CSD")
#   expect_equal(nrow(output) > 15, TRUE)
#   expect_equal(names(output), c("ID", "var_left_1", "var_left_2", "var_left",
#                                 "var_right_1", "var_right_2", "var_right",
#                                 "var_left_q3", "var_right_q3", "group"))
#   expect_equal(class(output[[1]]), "character")
#   expect_equal(class(output[[2]]), "numeric")
#   expect_equal(class(output[[4]]), "numeric")
#   expect_equal(class(output[[7]]), "numeric")
# })
