test_that("geocode returns coordinates for a valid address", {
  result <- geocode("160 Elgin St, Ottawa")
  expect_equal(result, c(lat = -75.6923378, lon = 45.4200807))
})

test_that("geocode returns NULL for an invalid address", {
  result <- geocode("Some fake address")
  expect_equal(result, NULL)
})

test_that("Function handles non-string input", {
  result <- geocode(123)
  expect_equal(result, NULL)
})
