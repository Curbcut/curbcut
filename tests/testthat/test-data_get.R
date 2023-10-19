test_that("data_get.q5  works", {
  # Extracting scale and time from var_left and building vars
  vars <- vars_build(var_left = "housing_tenant", scale = "CSD", time = 2016)
  vars <- vars$vars

  # Getting data with the updated structure
  output <- data_get(vars = vars, region = "CMA", scale = "CSD", data_path = .curbcut_montreal_data)

  # Testing if output has more than 15 rows
  expect_equal(nrow(output) > 15, TRUE)

  # # Testing column names of output
  # expect_equal(
  #   names(output),
  #   c("ID", "var_left", "var_left_q3", "var_left_q5", "group")
  # )

  # Testing data type of first column
  expect_equal(class(output[[1]]), "character")

  # Testing data type of second column
  expect_equal(class(output[[2]]), "numeric")
})


# test_that("data_get.bivar works", {
#   vars <- vars_build(
#     var_left = c("alp_2016"),
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

# test_that("data_get.delta works", {
#   vars <- vars_build(
#     var_left = c("housing_tenant_1996", "housing_tenant_2016"),
#     var_right = c(" "),
#     df = "city_CSD"
#   )
#   output <- data_get(vars = vars, df = "city_CSD")
#   expect_equal(nrow(output) > 15, TRUE)
#   # ORDER IS IMPORTANT FOR TABLE VIEW (PANEL_VIEW)
#   expect_equal(names(output), c(
#     "ID", "var_left_1", "var_left_2", "var_left",
#     "var_left_q5", "group"
#   ))
#   expect_equal(class(output[[1]]), "character")
#   expect_equal(class(output[[2]]), "numeric")
#   expect_equal(class(output[[4]]), "numeric")
# })
#
# test_that("data_get.delta_bivar works", {
#   vars <- vars_build(
#     var_left = c("housing_tenant_1996", "housing_tenant_2016"),
#     var_right = c("housing_rent_1996", "housing_rent_2016"),
#     df = "city_CSD"
#   )
#
#   output <- data_get(vars = vars, df = "city_CSD")
#   expect_equal(nrow(output) > 15, TRUE)
#   # ORDER IS IMPORTANT FOR TABLE VIEW (PANEL_VIEW)
#   expect_equal(names(output), c(
#     "ID", "var_left_1", "var_left_2", "var_left",
#     "var_right_1", "var_right_2", "var_right",
#     "var_left_q3", "var_right_q3", "group"
#   ))
#   expect_equal(class(output[[1]]), "character")
#   expect_equal(class(output[[2]]), "numeric")
#   expect_equal(class(output[[4]]), "numeric")
#   expect_equal(class(output[[7]]), "numeric")
# })

# test_that("data_get.bivar_ldelta_rq3 works", {
#   vars <- vars_build(
#     var_left = c("housing_tenant_1996", "housing_tenant_2016"),
#     var_right = c("alp_2016"),
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
