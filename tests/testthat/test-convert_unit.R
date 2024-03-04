test_that("convert_unit works", {
  # Test for values in thousands with $ symbol
  expect_equal(
    convert_unit(
      var = vars_build(c("housing_rent"), scale = "CSD", time = 2021)$vars$var_left,
      x = c(250000, 500000, 1000000),
      compact = TRUE
    ),
    c("$250K", "$500K", "$1,000K")
  )

  # Test for values in millions with $ symbol
  expect_equal(
    convert_unit(
      var = vars_build(c("housing_rent"), scale = "CSD", time = 2021)$vars$var_left,
      x = c(25000000, 50000000, 100000000),
      compact = TRUE
    ),
    c("$25M", "$50M", "$100M")
  )

  # Test for values in billions with $ symbol
  expect_equal(
    convert_unit(
      var = vars_build(c("housing_rent"), scale = "CSD", time = 2021)$vars$var_left,
      x = c(2500000000, 5000000000, 10000000000),
      compact = TRUE
    ),
    c("$3B", "$5B", "$10B")
  )

  # Test for percentages
  expect_equal(
    convert_unit(
      var = vars_build(c("housing_tenant"), scale = "CSD", time = 2021)$vars$var_left,
      x = c(0.1, 0.5, 0.99),
      compact = TRUE
    ),
    c("10%", "50%", "99%")
  )

  # Test for non-compact standard numbers
  expect_equal(
    convert_unit(x = c(1000, 2000, 5000)),
    c("1,000", "2,000", "5,000")
  )

  # Test for compact standard numbers
  expect_equal(
    convert_unit(x = c(10000, 20000, 50000), compact = TRUE),
    c("10K", "20K", "50K")
  )

  # Test for values below a thousand with $ symbol
  expect_equal(
    convert_unit(
      var = vars_build(c("housing_rent"), scale = "CSD", time = 2021)$vars$var_left,
      x = c(750, 1500, 2500),
      compact = TRUE
    ),
    c("$750", "$1,500", "$2,500")
  )

  # Test for values in tens of thousands with $ symbol
  expect_equal(
    convert_unit(
      var = vars_build(c("housing_rent"), scale = "CSD", time = 2021)$vars$var_left,
      x = c(7500, 15000, 25000),
      compact = TRUE
    ),
    c("$8K", "$15K", "$25K")
  )
})
