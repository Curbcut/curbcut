test_that("adv_opt_region is a shiny tagList", {
  expect_s3_class(adv_opt_lock_selection_UI("en"), "shiny.tag.list")
})

test_that("adv_opt_lock_selection is a shiny tagList", {
  expect_s3_class(adv_opt_lock_selection_UI("en"), "shiny.tag.list")
})

test_that("adv_opt_lock_selection returns NULL if postal code isn't within any region", {
  result <- adv_opt_lock_selection(address = "G2L2G1", NULL)
  expect_equal(result, NULL)

  result <- adv_opt_lock_selection(address = "", "en")
  expect_equal(result, NULL)
})

test_that("adv_opt_lock_selection returns NULL if address isn't within any region", {
  result <- adv_opt_lock_selection(address = "Some fake address", NULL)
  expect_equal(result, NULL)
})

test_that("adv_opt_lock_selection returns correct IDs for a postal code in one region", {
  result <- adv_opt_lock_selection(address = "H3A 0G4", NULL)
  expect_equal(
    all(c("borough_3", "4620129.02", "24663358") %in% result),
    TRUE
  )
})

test_that("adv_opt_lock_selection returns correct IDs for an address in one region", {
  result <- adv_opt_lock_selection(address = "845 Sherbrooke St W, Montreal", lang = NULL)
  expect_true(all(c("borough_3", "4620062.00", "24663459") %in% result))
})

test_that("adv_opt_lock_selection handles input containing non-alphanumeric characters", {
  result <- adv_opt_lock_selection(address = "Sherbrooke St. Montréal", "en")
  expect_true(all(c("borough_7", "4620397.00", "24662227") %in% result))
})

test_that("adv_opt_lock_selection returns NULL for an address that is not found within a 1km radius", {
  result <- adv_opt_lock_selection(address = "Yellowknife, NT", "en")
  expect_equal(result, NULL)
})

test_that("adv_opt_lock_selection returns NULL for non-string adress", {
  result <- adv_opt_lock_selection(1234, "en")
  expect_equal(result, NULL)
})

test_that("adv_opt_lock_selection returns NULL for an empty string", {
  result <- adv_opt_lock_selection("", "en")
  expect_equal(result, NULL)
})
