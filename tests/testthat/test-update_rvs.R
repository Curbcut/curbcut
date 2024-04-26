test_that("update_zoom_string returns new zoom string when it is different from the old one", {
  new_zoom_string <-
    update_zoom_string(
      rv_zoom_string = "CSD",
      zoom = 12,
      zoom_levels = mzl_borough_CT_DA
    )
  expect_equal(new_zoom_string, "DA")
})

test_that("update_zoom_string returns old zoom string when the new one is the same as the old one", {
  new_zoom_string <-
    update_zoom_string(
      rv_zoom_string = "CSD",
      zoom = 9.9,
      zoom_levels = mzl_CSD_CT_DA
    )
  expect_equal("CSD", new_zoom_string)
})

# Test that the zoom string is generated correctly for the highest zoom level
test_that("update_zoom_string generates correct zoom string for the highest zoom level", {
  new_zoom_string <- update_zoom_string(
    rv_zoom_string = "CSD",
    zoom = 16,
    zoom_levels = mzl_CSD_CT_DA
  )
  expect_equal(new_zoom_string, "DA")
})

# Test that the zoom string is generated correctly for the lowest zoom level
test_that("update_zoom_string generates correct zoom string for the lowest zoom level", {
  new_zoom_string <- update_zoom_string(
    rv_zoom_string = "",
    zoom = 0,
    zoom_levels = mzl_CSD_CT_DA
  )
  expect_equal(new_zoom_string, "CSD")
})

test_that("update_select_id_from_default returns select_id when default_select_ids is NULL", {
  data <- data.frame(ID = c("1", "2", "3"))
  default_select_ids <- NULL
  select_id <- "1"
  expected_output <- select_id
  actual_output <- update_select_id_from_default(data, default_select_ids, select_id)
  expect_equal(actual_output, expected_output)
})

test_that("update_select_id_from_default returns select_id when default_select_ids is not in data$ID", {
  data <- data.frame(ID = c("1", "2", "3"))
  default_select_ids <- c("4", "5", "6")
  select_id <- "1"
  expected_output <- select_id
  actual_output <- update_select_id_from_default(data, default_select_ids, select_id)
  expect_equal(actual_output, expected_output)
})

test_that("update_select_id_from_default returns matching ID from data$ID when default_select_ids matches one ID in data$ID", {
  data <- data.frame(ID = c("1", "2", "3"))
  default_select_ids <- c("2")
  select_id <- "1"
  expected_output <- "2"
  actual_output <- update_select_id_from_default(data, default_select_ids, select_id)
  expect_equal(actual_output, expected_output)
})

test_that("update_select_id_from_default returns first matching ID from data$ID when default_select_ids match multiple IDs in data$ID", {
  data <- data.frame(ID = c("1", "2", "3", "4", "5"))
  default_select_ids <- c("2", "5")
  select_id <- "1"
  expected_output <- "2"
  actual_output <- update_select_id_from_default(data, default_select_ids, select_id)
  expect_equal(actual_output, expected_output)
})

test_that("update_select_id_from_default returns select_id when data$ID is empty", {
  data <- data.frame(ID = integer(0))
  default_select_ids <- c("1", "2", "3")
  select_id <- "1"
  expected_output <- select_id
  actual_output <- update_select_id_from_default(data, default_select_ids, select_id)
  expect_equal(actual_output, expected_output)
})

test_that("update_scale returns zoom_string when tile contains underscores (autozoom)", {
  tile <- "CSD_CT_DA_building"
  zoom_string <- "zoom_string"
  expected_output <- zoom_string
  actual_output <- update_scale(tile, zoom_string)
  expect_equal(actual_output, expected_output)
})

test_that("update_scale returns tile when tile does not contain 'auto_zoom'", {
  tile <- "tile"
  zoom_string <- "zoom_string"
  expected_output <- tile
  actual_output <- update_scale(tile, zoom_string)
  expect_equal(actual_output, expected_output)
})

test_that("update_scale returns tile when not on auto-zoom", {
  tile <- "CSD"
  zoom_string <- "zoom_string"
  expected_output <- tile
  actual_output <- update_scale(tile, zoom_string)
  expect_equal(actual_output, expected_output)
})

test_that("update_scale returns zoom_string when tile is 'auto_zoom'", {
  tile <- "CSD_CT_DA_building"
  zoom_string <- "DA"
  expected_output <- zoom_string
  actual_output <- update_scale(tile, zoom_string)
  expect_equal(actual_output, expected_output)
})



test_that("update_poi returns NULL when map zoom is less than 13", {
  poi <- c("little_burgundy", "mirron_quarry")
  map_viewstate <- list(zoom = 12, latitude = 45.5, longitude = -73.6)
  expect_equal(update_poi("alp", poi, map_viewstate), NULL)
})

test_that("update_poi returns nothing too far from the stories", {
  poi <- c("little_burgundy", "mirron_quarry")
  map_viewstate <- list(
    zoom = 13,
    latitude = stories[1, ]$lat - 5,
    longitude = stories[1, ]$lon - 5
  )
  result <- update_poi("id", poi = poi, map_viewstate)
  expect_equal(result, NULL)
})

test_that("update_poi returns the correct nearby POIs", {
  poi <- c("little_burgundy", "mirron_quarry")
  map_viewstate <- list(zoom = 15, latitude = 45.479, longitude = -73.574)
  expected_pois <- c("little_burgundy", "alley_strategy")
  expect_equal(update_poi("alp", poi, map_viewstate), expected_pois)
})


test_that("update_select_id_helper returns new ID when different ID is selected", {
  expect_equal(update_select_id_helper("B", "A"), "B")
})

test_that("update_select_id_helper returns NA when same ID is selected twice", {
  expect_equal(update_select_id_helper("A", "A"), NA)
})

test_that("update_select_id_helper returns new id when current selected ID is NA", {
  expect_equal(update_select_id_helper("A", NA), "A")
})

test_that("update_select_id_helper returns NA when new ID is NA", {
  expect_equal(update_select_id_helper(NA, "A"), NA)
})

test_that("update_select_id_helper returns NA when both new and current IDs are NA", {
  expect_equal(update_select_id_helper(NA, NA), NA)
})
