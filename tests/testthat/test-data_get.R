test_that("data_get.q5 works", {
  # Extracting scale and time from var_left and building vars
  vars <- vars_build(var_left = "housing_tenant", scale = "CSD", time = 2016)
  vars <- vars$vars

  # Getting data with the updated structure
  output <- data_get(vars = vars, region = "CMA", scale = "CSD")

  # Testing if output has more than 15 rows
  expect_equal(nrow(output) > 15, TRUE)

  # Testing column names of output
  expect_equal(
    names(output),
    c("ID", "var_left_1996", "var_left_2001", "var_left_2006", "var_left_2011",
      "var_left_2016", "var_left_2021", "var_left_1996_q5", "var_left_2001_q5",
      "var_left_2006_q5", "var_left_2011_q5", "var_left_2016_q5", "var_left_2021_q5"
    )
  )

  # Testing data type of first column
  expect_equal(class(output[[1]]), "character")

  # Testing data type of second column
  expect_equal(class(output[[2]]), "numeric")
})


test_that("data_get.q5 works", {
  # Extracting scale and time from var_left and building vars
  vars <- vars_build(var_left = "access_foot_food_grocery", scale = "CSD", time = 2023)
  vars <- vars$vars

  # Getting data with the updated structure
  output <- data_get(vars = vars, region = "CMA", scale = "CSD")

  # Testing if output has more than 15 rows
  expect_equal(nrow(output) > 15, TRUE)

  # Testing column names of output
  expect_equal(
    names(output),
    c("ID", "var_left_10_2023", "var_left_15_2023", "var_left_20_2023",
      "var_left_25_2023", "var_left_30_2023", "var_left_35_2023", "var_left_40_2023",
      "var_left_45_2023", "var_left_5_2023", "var_left_50_2023", "var_left_55_2023",
      "var_left_60_2023", "var_left_10_2023_q5", "var_left_15_2023_q5",
      "var_left_20_2023_q5", "var_left_25_2023_q5", "var_left_30_2023_q5",
      "var_left_35_2023_q5", "var_left_40_2023_q5", "var_left_45_2023_q5",
      "var_left_5_2023_q5", "var_left_50_2023_q5", "var_left_55_2023_q5",
      "var_left_60_2023_q5")
  )

  # Testing data type of first column
  expect_equal(class(output[[1]]), "character")

  # Testing data type of second column
  expect_equal(class(output[[2]]), "numeric")
})


test_that("data_get.bivar works", {
  # Extracting scale and time from var_left and building vars
  vars <- vars_build(var_left = "housing_tenant", var_right = "alp", scale = "CSD", time = 2016)
  vars <- vars$vars

  # Getting data with the updated structure
  output <- data_get(vars = vars, region = "CMA", scale = "CSD")

  # Testing column names of output
  expect_equal(
    names(output),
    c("ID", "var_left_1996", "var_left_2001", "var_left_2006", "var_left_2011",
      "var_left_2016", "var_left_2021", "var_left_1996_q3", "var_left_2001_q3",
      "var_left_2006_q3", "var_left_2011_q3", "var_left_2016_q3", "var_left_2021_q3",
      "var_right_2001", "var_right_2006", "var_right_2011", "var_right_2016",
      "var_right_2021", "var_right_2001_q3", "var_right_2006_q3", "var_right_2011_q3",
      "var_right_2016_q3", "var_right_2021_q3", "group_1996", "group_2001",
      "group_2006", "group_2011", "group_2016", "group_2021")
  )

  expect_equal(nrow(output) > 15, TRUE)
  expect_equal(class(output[[1]]), "character")
  expect_equal(class(output[[2]]), "numeric")
})

test_that("data_get.delta works", {
  # Extracting scale and time from var_left and building vars
  vars <- vars_build(var_left = "housing_tenant", scale = "CSD", time = c(1996, 2016))
  time <- vars$time
  vars <- vars$vars

  # Getting data with the updated structure
  output <- data_get(vars = vars, region = "CMA", scale = "CSD", time = time)

  # Testing if output has more than 15 rows
  expect_equal(nrow(output) > 15, TRUE)

  # Testing column names of output
  expect_equal(
    names(output),
    c("ID", "var_left_1996", "var_left_2016", "var_left_1996_q5", "var_left_2016_q5",
      "var_left", "var_left_q5", "group")
  )

  # Testing data type of first column
  expect_equal(class(output[[1]]), "character")

  # Testing data type of second column
  expect_equal(class(output[[2]]), "numeric")
})

test_that("data_get.delta_bivar works", {
  # Extracting scale and time from var_left and building vars
  vars <- vars_build(var_left = "housing_tenant", var_right = "alp",
                     scale = "CSD", time = c(1996, 2016))
  time <- vars$time
  vars <- vars$vars

  # Getting data with the updated structure
  output <- data_get(vars = vars, region = "CMA", scale = "CSD", time = time)

  # Testing if output has more than 15 rows
  expect_equal(nrow(output) > 15, TRUE)

  # Testing column names of output
  expect_equal(
    names(output),
    c("ID", "var_left_1996", "var_left_2016", "var_left_1996_q5",
      "var_left_2016_q5", "var_left", "var_right_2001", "var_right_2016",
      "var_right_2001_q5", "var_right_2016_q5", "var_right", "var_left_q3",
      "var_right_q3", "group")
  )

  # Testing data type of first column
  expect_equal(class(output[[1]]), "character")

  # Testing data type of second column
  expect_equal(class(output[[2]]), "numeric")

  # Testing data type of second column
  expect_equal(class(output$var_left), "numeric")

  # Testing data type of second column
  expect_equal(class(output$var_right), "numeric")

  # Testing data type of second column
  expect_equal(class(output$group), "character")
})

test_that("data_get.bivar_ldelta_rq3 works", {
  # Extracting scale and time from var_left and building vars
  vars <- vars_build(var_left = "crash_count_ped", var_right = "alp",
                     scale = "CSD", time = c(2015, 2017))
  time <- vars$time
  vars <- vars$vars

  # Getting data with the updated structure
  output <- data_get(vars = vars, region = "CMA", scale = "CSD", time = time)

  # Testing if output has more than 15 rows
  expect_equal(nrow(output) > 15, TRUE)

  # Testing column names of output
  expect_true(
    all(c("ID", "var_left_2015", "var_left_2017", "var_left_2015_q5",
          "var_left_2017_q5", "var_left", "group", "var_left_q3",
          "var_right_2016", "var_right_q3") %in% names(output))
  )

  # Testing data type of first column
  expect_equal(class(output[[1]]), "character")

  # Testing data type of second column
  expect_equal(class(output[[2]]), "integer")

  # Testing data type of second column
  expect_equal(class(output$var_left), "numeric")

  # Testing data type of second column
  expect_equal(class(output$group), "character")
})
