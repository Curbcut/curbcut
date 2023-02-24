test_that("bookmark_build_url works", {
  widgets <- list(
    `compare-ccpicker_var` = " ",
    `zoom_slider-ccslidertext_sldt` = "Secteur de recensement",
    `zoom_auto-cccheckbox_cbox` = TRUE
  )

  url <- bookmark_build_url(
    id = "canale",
    region = "CMA",
    widgets = widgets,
    lang = "fr",
    map_viewstate = list(
      zoom = 10.5,
      latitude = 70.01,
      longitude = 40.02
    ),
    select_id = "123456"
  )
  expect_equal(url, "/?reg=1&tb=3&lng=fr&wgt=cp: ;zs:CT;zc:T&zm=10.5&crds=40.02;70.01&sid=123456")

  widgets <- list(
    `compare-ccpicker_var` = " ",
    `zoom_slider-ccslidertext_sldt` = "Census tract",
    `zoom_auto-cccheckbox_cbox` = TRUE
  )
  url <- bookmark_build_url(
    id = "canale",
    region = "CMA",
    widgets = widgets,
    lang = NULL,
    map_viewstate = list(
      zoom = 10.5,
      latitude = 70.01,
      longitude = 40.02
    ),
    select_id = "123456"
  )
  expect_equal(url, "/?reg=1&tb=3&wgt=cp: ;zs:CT;zc:T&zm=10.5&crds=40.02;70.01&sid=123456")

  url <- bookmark_build_url(
    id = "canale",
    region = "CMA",
    widgets = NULL,
    lang = NULL,
    map_viewstate = list(
      zoom = 10.5,
      latitude = 70.01,
      longitude = 40.02
    ),
    select_id = "123456"
  )
  expect_equal(url, "/?reg=1&tb=3&zm=10.5&crds=40.02;70.01&sid=123456")

  url <- bookmark_build_url(
    id = "something",
    region = "CMA",
    widgets = NULL,
    lang = NULL,
    map_viewstate = NULL,
    select_id = NULL
  )
  expect_equal(url, "/?reg=1&tb=something")



  widgets <- list(
    `compare-ccpicker_var` = 3,
    `zoom_slider-ccslidertext_sldt` = "Secteur de recensement",
    `zoom_auto-cccheckbox_cbox` = TRUE
  )
  expect_error(bookmark_build_url(
    id = "something",
    region = "CMA",
    widgets = widgets,
    lang = "fr",
    map_viewstate = NULL,
    select_id = NULL
  ))
})
