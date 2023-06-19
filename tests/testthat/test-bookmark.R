test_that("bookmark_widget_helper returns the right info using bookmark codes", {
  wgt <- "cp:1"
  result <- bookmark_widget_helper(id = "housing", wgt = wgt)
  expect_equal(
    result,
    list(
      cbox = list(), s_text = list(), picker = list(c(
        "compare-ccpicker_var",
        "housing_tenant"
      )),
      slider = list()
    )
  )

  wgt <- "pv:1"
  result <- bookmark_widget_helper(id = "housing", wgt)
  expect_equal(
    result,
    list(
      cbox = list(), s_text = list(), picker = list(c(
        "ccpicker_var",
        "housing_tenant"
      )),
      slider = list()
    )
  )

  wgt <- "zs:CT"
  result <- bookmark_widget_helper(id = "housing", wgt)
  expect_equal(
    result,
    list(
      cbox = list(), s_text = list(c(
        "zoom_slider-ccslidertext_slt",
        "Census tract"
      )), picker = list(),
      slider = list()
    )
  )

  wgt <- "zs:hola"
  result <- bookmark_widget_helper(id = "housing", wgt)
  expect_equal(
    result,
    list(
      cbox = list(), s_text = list(c(
        "zoom_slider-ccslidertext_slt",
        "hola"
      )), picker = list(),
      slider = list()
    )
  )

  wgt <- "zc:T"
  result <- bookmark_widget_helper(id = "housing", wgt)
  expect_equal(
    result,
    list(
      cbox = list(c("zoom_auto-cccheckbox_cbx", "T")), s_text = list(),
      picker = list(),
      slider = list()
    )
  )

  wgt <- "cb:F"
  result <- bookmark_widget_helper(id = "housing", wgt)
  expect_equal(
    result,
    list(
      cbox = list(c("cccheckbox_cbx", "F")), s_text = list(),
      picker = list(),
      slider = list()
    )
  )
})

test_that("bookmark_widget_helper returns the right info using bookmark shorts", {
  wgt <- "pifir:2"
  result <- bookmark_widget_helper(id = "housing", wgt)
  expect_equal(
    result,
    list(
      cbox = list(), s_text = list(), picker = list(c(
        "ccpicker_fir",
        "housing_rent"
      )),
      slider = list()
    )
  )

  wgt <- "pifir:something_else"
  result <- bookmark_widget_helper(id = "housing", wgt)
  expect_equal(
    result,
    list(
      cbox = list(), s_text = list(), picker = list(c(
        "ccpicker_fir",
        "something_else"
      )),
      slider = list()
    )
  )

  wgt <- "sxabc:test"
  result <- bookmark_widget_helper(id = "housing", wgt)
  expect_equal(
    result,
    list(
      cbox = list(), s_text = list(c("ccslidertext_abc", "test")), picker = list(),
      slider = list()
    )
  )

  wgt <- "cheso:F"
  result <- bookmark_widget_helper(id = "housing", wgt)
  expect_equal(
    result,
    list(
      cbox = list(c("cccheckbox_eso", "F")), s_text = list(),
      picker = list(),
      slider = list()
    )
  )
})

test_that("bookmark_widget_helper can process everything together", {
  wgt <- "pifir:2;pifir:something_else;sxabc:test;cheso:F;cb:F;zc:T;zs:CT;pv:1;lcp:1"
  result <- bookmark_widget_helper(id = "housing", wgt)
  expect_equal(
    result,
    list(
      cbox = list(c("cccheckbox_cbx", "F"), c(
        "zoom_auto-cccheckbox_cbx",
        "T"
      ), c("cccheckbox_eso", "F")), s_text = list(c(
        "zoom_slider-ccslidertext_slt",
        "Census tract"
      ), c("ccslidertext_abc", "test")), picker = list(
        c("ccpicker_var", "housing_tenant"), c("ccpicker_fir", "housing_rent"), c("ccpicker_fir", "something_else")
      ),
      slider = list()
    )
  )
})
