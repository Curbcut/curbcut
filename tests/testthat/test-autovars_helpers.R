test_that("autovars_groupnames works", {
  major_grouping <- autovars_groupnames(id = "access")
  expect_true(is.character(major_grouping))
  expect_true(length(major_grouping) > 1)
})

test_that("autovars_groupnames works", {
  common_widgets <- autovars_common_widgets(id = "housing")

  expect_equal(names(common_widgets), c("time", "widgets"))
  expect_equal(length(common_widgets$time) > 1, TRUE)
})

test_that("autovars_widgets works", {
  common_vals <- c(`Mode of transport` = "Public transit", `Transportation time` = 45)
  actual <- autovars_widgets(
    id = "access",
    group_name = "Access to cultural facilities",
    common_vals = common_vals
  )

  expect_true("list" %in% class(actual))
  expect_true(all(c("Timing", "Cultural facility") %in% names(actual)))
})

test_that("autovars_final_value works", {
  picker_vals <- c("Public transit", "30", "Weekend traffic off-peak", "Art or cultural centre")
  actual <- autovars_final_value(
    id = "access",
    group_name = "Access to cultural facilities",
    picker_vals = picker_vals,
    previous_var = "access_car_20_cultural_artcenter"
  )
  expect_equal(actual, "access_transit_opwe_30_cultural_artcentre")

  picker_vals <- c("Walking", "50", "Art or cultural centre")
  actual <- autovars_final_value(
    id = "access",
    group_name = "Access to cultural facilities",
    picker_vals = picker_vals,
    previous_var = "access_car_20_cultural_artcenter"
  )

  expect_equal(actual, "access_foot_50_cultural_artcentre")
})
