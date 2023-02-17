test_that("convert_unit works", {
  expect_equal(
    convert_unit(c(250000, 500000, 1000000),
      var = "housing_value_2016",
      compact = TRUE
    ),
    c("$250K", "$500K", "$1,000K")
  )
  expect_equal(
    convert_unit(c(25000000, 50000000, 100000000),
      var = "housing_value_2016",
      compact = TRUE
    ),
    c("$25M", "$50M", "$100M")
  )
  expect_equal(
    convert_unit(c(2500000000, 5000000000, 10000000000),
      var = "housing_value_2016",
      compact = TRUE
    ),
    c("$3B", "$5B", "$10B")
  )
  expect_equal(
    convert_unit(c(0.1, 0.5, 0.99),
      var = "housing_tenant_2016",
      compact = TRUE
    ),
    c("10%", "50%", "99%")
  )
  expect_equal(
    convert_unit(c(1000, 2000, 5000)),
    c("1,000", "2,000", "5,000")
  )
  expect_equal(
    convert_unit(c(10000, 20000, 50000), compact = TRUE),
    c("10K", "20K", "50K")
  )
  expect_equal(
    convert_unit(c(750, 1500, 2500),
      var = "housing_rent_2016",
      compact = TRUE
    ),
    c("$750", "$1,500", "$2,500")
  )
  expect_equal(
    convert_unit(c(7500, 15000, 25000),
      var = "housing_rent_2016",
      compact = TRUE
    ),
    c("$8K", "$15K", "$25K")
  )
  convert_unit(c(0.1, 0.5, 0.99),
    var = "housing_tenant_2016",
    compact = TRUE
  )
})

test_that("is_scale_df works", {
  expect_equal(
    is_scale_df(c("CSD", "CT", "DA"), "CMA_DA"),
    TRUE
  )
  expect_equal(
    is_scale_df(c("CSD", "CT", "DA"), "grid_grid"),
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

test_that("var_get_breaks works", {
  expect_equal(
    var_get_breaks(
      var = "housing_tenant_2016",
      df = "CMA_CSD", q3_q5 = "q5",
      pretty = TRUE, compact = TRUE
    ),
    c("0%", "20%", "40%", "60%", "80%", "100%")
  )

  expect_equal(
    var_get_breaks(
      var = "housing_value_2016",
      df = "CMA_CSD", q3_q5 = "q5",
      pretty = TRUE, compact = TRUE
    ),
    c("$0K", "$200K", "$400K", "$600K", "$800K", "$1,000K")
  )

  # SHOULD CHANGE IN FUTURE VARIABLES.QS -> THE FIRST IS NA, NOT INSIG.
  expect_equal(
    var_get_breaks(
      var = "climate_drought",
      df = "grid_grid", q3_q5 = "q5",
      break_col = "var_name_short",
      pretty = TRUE, compact = TRUE
    ),
    c(NA, "Insig.", "Minor", "Mod.", "Elev.", "Major")
  )
})
