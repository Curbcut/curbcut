test_that("adv_opt_region is a shiny tagList", {
  expect_s3_class(adv_opt_lock_selection_UI("en"), "shiny.tag.list")
})

test_that("adv_opt_lock_selection is a shiny tagList", {
  expect_s3_class(adv_opt_lock_selection_UI("en"), "shiny.tag.list")
})

test_that("adv_opt_lock_selection returns NULL if postal code isn't within any region", {
  result <- adv_opt_lock_selection("G2L2G1", NULL)
  expect_equal(result, NULL)

  result <- adv_opt_lock_selection("", "en")
  expect_equal(result, NULL)
})

test_that("adv_opt_lock_selection returns NULL if address isn't within any region", {
  result <- adv_opt_lock_selection("Some fake address", NULL)
  expect_equal(result, NULL)
})

test_that("adv_opt_lock_selection returns correct IDs for a postal code in one region", {
  result <- adv_opt_lock_selection("H3A 0G4", NULL)
  expect_equal(
    all(c("2466023_3", "4620129.02", "24663358") %in% result),
    TRUE
  )
})

test_that("adv_opt_lock_selection returns correct IDs for an address in one region", {
  result <- adv_opt_lock_selection("845 Sherbrooke St W, Montreal", NULL)
  expect_true(all(c("2466023_3", "4620062.00", "24663459") %in% result))
})

test_that("adv_opt_lock_selection handles input containing non-alphanumeric characters", {
  result <- adv_opt_lock_selection("Sherbrooke St. Montréal", "en")
  expect_true(all(c("2466023_7", "4620397.00", "24662227") %in% result))
})

test_that("adv_opt_lock_selection returns NULL for an address that is not found within a 1km radius", {
  result <- adv_opt_lock_selection("Yellowknife, NT", "en")
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
