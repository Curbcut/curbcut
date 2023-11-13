test_that("is_scale_in works", {
  expect_equal(
    is_scale_in(c("CSD", "CT", "DA"), "DA"),
    TRUE
  )
  expect_equal(
    is_scale_in(c("CSD", "CT", "DA"), "grid50"),
    FALSE
  )
})

test_that("s_extract works", {
  expect_equal(
    s_extract("\\d{4}$", "housing_tenant_2016"),
    "2016"
  )
  expect_equal(
    s_extract("\\d{4}$", c("housing_tenant_2016", "housing_tenant_2021")),
    c("2016", "2021")
  )
  expect_equal(
    s_extract("(?<=housing_tenant_).*", "housing_tenant_2016"),
    "2016"
  )
  expect_equal(
    s_extract("^.*?(?=_)", "CMA_DA"),
    "CMA"
  )
})

test_that("var_get_time works", {
  expect_equal(
    var_get_time("housing_tenant_2016"),
    "2016"
  )
  expect_equal(
    var_get_time(c("housing_tenant_2016", "housing_tenant_2021")),
    c("2016", "2021")
  )
})

test_that("var_remove_time works", {
  expect_equal(
    var_remove_time("housing_tenant_2016"),
    "housing_tenant"
  )
  expect_equal(
    var_remove_time(c("housing_tenant_2016", "housing_tenant_2021")),
    "housing_tenant"
  )
})

test_that("var_get_info works", {
  expect_equal(
    var_get_info(
      var = "housing_tenant_2016",
      what = "var_title"
    ),
    "Tenant-occupied (%)"
  )
  expect_equal(
    var_get_info(
      var = c("housing_tenant_2016", "housing_tenant_2021"),
      what = "var_title"
    ),
    "Tenant-occupied (%)"
  )
  expect_equal(
    var_get_info(
      var = "housing_tenant",
      what = "var_title"
    ),
    "Tenant-occupied (%)"
  )
})

test_that("var_get_title works", {
  expect_equal(
    var_get_title(var = "housing_tenant_2016"),
    "Tenant-occupied (%)"
  )
  expect_equal(
    var_get_title(var = c("housing_tenant_2016", "housing_tenant_2021")),
    "Tenant-occupied (%)"
  )
  expect_equal(
    var_get_title(var = "housing_tenant"),
    "Tenant-occupied (%)"
  )
  expect_equal(
    var_get_title(var = "housing_tenant_2016", short_treshold = 12),
    "Tenant"
  )
  expect_equal(
    var_get_title(
      var = c("housing_tenant_2016", "housing_tenant_2021"),
      short_treshold = 12
    ),
    "Tenant"
  )
  expect_equal(
    var_get_title(var = "housing_tenant", short_treshold = 12),
    "Tenant"
  )
})

# test_that("var_get_breaks works", {
#   actual <- var_get_breaks(
#     var = vars_build("housing_tenant", scale = "CSD", time = 2016)$vars$var_left,
#     scale = "CSD", q3_q5 = "q5",
#     pretty = TRUE, compact = TRUE
#   )
#   expect_equal(all(grepl("^\\d.*%$", actual)), TRUE)
#
#   actual <- var_get_breaks(
#     var = vars_build("housing_rent_2016", df = "city_CSD")$var_left,
#     df = "CMA_CSD", q3_q5 = "q5",
#     pretty = TRUE, compact = TRUE
#   )
#   expect_equal(all(grepl("^\\$\\d", actual)), TRUE)
#
#   # SHOULD CHANGE IN FUTURE VARIABLES.QS -> THE FIRST IS NA, NOT INSIG.
#   expect_equal(
#     var_get_breaks(
#       var = "climate_drought_2015",
#       df = "grid_grid50", q3_q5 = "q5",
#       break_col = "rank_name_short",
#       pretty = TRUE, compact = TRUE
#     ),
#     c(NA, "Insig.", "Minor", "Mod.", "Elev.", "Major")
#   )
# })

test_that("treat_to_DA works", {
  expect_equal(
    treat_to_DA(
      scales_as_DA = c("building", "street"),
      scale = "building"
    ),
    "DA"
  )

  expect_equal(
    treat_to_DA(
      scales_as_DA = c("building", "street"),
      scale = "DA"
    ),
    "DA"
  )

  expect_equal(
    treat_to_DA(
      scales_as_DA = c("building", "street"),
      scale = "CSD"
    ),
    "CSD"
  )
})

test_that("get_from_globalenv works", {
  scale_dict <- get_from_globalenv("scales_dictionary")
  expect_equal(is.data.frame(scale_dict), TRUE)
  expect_error(get_from_globalenv("eso"))
})

test_that("var_row_index works", {
  expect_equal(var_row_index("housing_tenant_2016"), 1)
  expect_equal(var_row_index("housing_tenant"), 1)
  expect_equal(var_row_index("not_there"), "not_there")
  expect_equal(var_row_index(c("housing_rent_2001", "housing_rent_2016")), 2)
  expect_equal(var_row_index(" "), " ")
})

test_that("widget_id_verif returns the original widget ID for a valid widget ID", {
  expect_equal(widget_id_verif("abc"), "abc")
})

test_that("widget_id_verif throws an error when the widget ID contains more than one underscore", {
  expect_error(
    widget_id_verif("a_b_c"),
    "No underscore can be used in a widget ID."
  )
})

test_that("widget_id_verif hrows an error when the widget ID has more than 3 characters", {
  expect_error(
    widget_id_verif("abcd"),
    "Widget ID can contain no more than 3 characters to reduce bookmark URL size."
  )
})

test_that("widget_id_verif throws an error when the widget ID interferes with known codes and short codes", {
  expect_error(widget_id_verif("pi"), "Widget ID can not be the same as a value of `curbcut::bookmark_codes` or `curbcut::bookmark_shorts` to limit interference with bookmark codes.")
})

test_that("fill_name_2 returns correct name_2 ordered", {

  ID_scale <- DA$ID[c(6000,456,3334,5522,22,1,34,77)]
  output <- fill_name_2(ID_scale = ID_scale, scale = "DA", top_scale = "CSD")

  real <- sapply(ID_scale, \(ID) {
    csd <- DA$CSD_ID[DA$ID == ID]
    out <- CSD$name[CSD$ID == csd]
    if (length(out) == 0) return(NA)
    out
  }, USE.NAMES = FALSE)

  expect_equal(output, real)

})
