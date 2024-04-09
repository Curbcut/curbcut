test_that("geocode returns coordinates for a valid address", {
  result <- geocode("160 Elgin St, Ottawa")
  expect_equal(result, c(lat = -75.6923378, lon = 45.4200807))
})

test_that("geocode returns NULL for an invalid address", {
  result <- geocode("Some fake address")
  expect_equal(result, NULL)
})

test_that("geocode handles non-string input", {
  result <- geocode(123)
  expect_equal(result, NULL)
})

test_that("rev_geocode handles non-string input", {
  result <- rev_geocode(lon = -73.5750825, lat = 45.5055633, timeout = 2)
  expect_equal(result, "3458 Rue University, Montréal")
})

test_that("rev_geocode returns NA when the query is invalid", {
  result <- rev_geocode(lon = "h", lat = 45.5055633)
  expect_equal(result, NA_character_)
})
