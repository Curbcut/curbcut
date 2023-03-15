test_that("zoom_get_string returns the correct zoom string for a given zoom level", {
  expect_equal(zoom_get_string(10, map_zoom_levels_CMA, "CMA"), "CMA_CSD")
})

test_that("zoom_get_string returns the correct zoom string for a given zoom level", {
  expect_equal(zoom_get_string(14, map_zoom_levels_city, "city"), "city_DA")
})

test_that("zoom_get_string returns the correct zoom string for a zoom level below the minimum zoom level", {
  expect_equal(zoom_get_string(0, map_zoom_levels_CMA, "CMA"), "CMA_CSD")
})

test_that("zoom_get_name returns correct output for specific input", {
  expect_equal(
    zoom_get_name(c("city_CT", "city_DA")),
    c("Census tract", "Dissemination area")
  )
})

test_that("zoom_get_name throws error for invalid input", {
  expect_error(zoom_get_name("invalid_scale"))
})

test_that("zoom_get_name returns correct output for `fr` in a non-reactive context (en)", {
  expect_equal(
    zoom_get_name("centraide_centraide", lang = "fr"),
    c("Centraide zone")
  )
})

test_that("zoom_get_name returns the expected output for a single input", {
  input <- "CT"
  expected_output <- "Census tract"
  output <- zoom_get_name(input)
  expect_equal(output, expected_output)
})

test_that("zoom_get_name returns the expected output for multiple inputs", {
  input <- c("CT", "DA", "grid")
  expected_output <- c("Census tract", "Dissemination area", "250-m")
  output <- zoom_get_name(input)
  expect_equal(output, expected_output)
})

test_that("zoom_get_name handles missing inputs correctly", {
  input <- c("CT", "unknown", "DA")
  expect_error(zoom_get_name(input))
})

test_that("zoom_get_name throws an error for multiple invalid scale names", {
  expect_error(zoom_get_name(c("invalid_scale1", "invalid_scale2")))
})


test_that("zoom_get_label returns the expected slider titles", {
  # Test case 1: all zoom levels are present in the scales dictionary
  zoom_levels <- c("CSD" = 1, "CT" = 2, "DA" = 3)
  expected <- c("Borough/City", "Census tract", "Dissemination area")
  result <- zoom_get_label(zoom_levels, lang = NULL)
  expect_equal(result, expected)

  # Test case 2: some zoom levels are missing from the scales dictionary
  zoom_levels <- c("CSD" = 1, "XYZ" = 2, "DA" = 3)
  expect_error(zoom_get_label(zoom_levels, lang = NULL))

  # Test case 3: zoom levels are in a different order than in the scales dictionary
  zoom_levels <- c("DA" = 3, "CT" = 2, "CSD" = 1)
  expected <- c("Borough/City", "Census tract", "Dissemination area")
  result <- zoom_get_label(zoom_levels, lang = NULL)
  expect_equal(result, expected)

  # Test case 4: translation to a different language in a non-reactive context (same as input)
  zoom_levels <- c("CSD" = 1, "CT" = 2, "DA" = 3)
  expected <- c("Borough/City", "Census tract", "Dissemination area")
  result <- zoom_get_label(zoom_levels, lang = "fr")
  expect_equal(result, expected)
})

# Test case 1: Test with one scale name in English
test_that("zoom_get_code returns correct scale code with one scale name in English", {
  scales_name <- c("Borough/City")
  expected_output <- "CSD"
  result <- zoom_get_code(scales_name)
  expect_equal(result, expected_output)
})

# Test case 2: Test with one scale name in French
test_that("zoom_get_code returns correct scale code with one scale name in French", {
  scales_name <- c("Arrondissement/ville")
  expected_output <- "CSD"
  result <- zoom_get_code(scales_name, lang = "fr")
  expect_equal(result, expected_output)
})

# Test case 3: Test with multiple scale names in English
test_that("zoom_get_code returns correct scale codes with multiple scale names in English", {
  scales_name <- c("Borough/City", "Census tract", "Building")
  expected_output <- c("CSD", "CT", "building")
  result <- zoom_get_code(scales_name)
  expect_equal(result, expected_output)
})

# Test case 4: Test with multiple scale names in French
test_that("zoom_get_code returns correct scale codes with multiple scale names in French", {
  scales_name <- c("Arrondissement/ville", "Secteur de recensement", "Aire de diffusion")
  expected_output <- c("CSD", "CT", "DA")
  result <- zoom_get_code(scales_name, lang = "fr")
  expect_equal(result, expected_output)
})

test_that("zoom_get_levels returns error if missing id", {
  expect_error(zoom_get_levels(id = "cc", region = "CMA"))
})

test_that("zoom_get_levels returns the correct map_zoom_levels and region", {
  result <- zoom_get_levels(id = "canale", region = "city")
  expect_equal(
    result,
    list(zoom_levels = c(CSD = 0, CT = 10.5, DA = 12.5, building = 15.5), region = "city")
  )
})

test_that("zoom_get_levels returns the correct map_zoom_levels and region with a suffix", {
  result <- zoom_get_levels(id = "canale", region = "city", "_max_CT")
  expect_equal(
    result,
    list(zoom_levels = c(CSD = 0, CT = 10.5), region = "city")
  )
})

test_that("zoom_get_levels returns the correct map_zoom_levels and region when region is different from the one supplied", {
  result <- zoom_get_levels(id = "vac_rate", region = "city")
  expect_equal(
    result,
    list(zoom_levels = c(cmhczone = 0), region = "cmhc")
  )
})

