test_that("bookmark_build_url works", {
  translation_df <-
    rbind(
      translation_df,
      data.frame(
        en = c("Borough/City", "Census tract", "Dissemination area"),
        fr = c("Arrondissement/ville", "Secteur de recensement", "Aire de diffusion")
      )
    )
  translation_df <- unique(translation_df)
  assign("translation_df", translation_df, envir = .GlobalEnv)
  widgets <- list(
    `compare-ccpicker_var` = " ",
    `zoom_slider-ccslidertext_slt` = "Secteur de recensement",
    `zoom_auto-cccheckbox_cbx` = TRUE
  )

  url <- bookmark_build_url(
    id = "alp",
    widgets = widgets,
    lang = "fr",
    map_viewstate = list(
      zoom = 10.5,
      latitude = 70.01,
      longitude = 40.02
    ),
    select_id = "123456"
  )
  expect_equal(url, "/?tb=alp&lng=fr&wgt=cp: ;zs:CT;zc:T&zm=10.5&crds=40.02;70.01&sid=123456")

  widgets <- list(
    `compare-ccpicker_var` = " ",
    `zoom_slider-ccslidertext_slt` = "Census tract",
    `zoom_auto-cccheckbox_cbx` = TRUE
  )
  url <- bookmark_build_url(
    id = "alp",
    widgets = widgets,
    lang = NULL,
    map_viewstate = list(
      zoom = 10.5,
      latitude = 70.01,
      longitude = 40.02
    ),
    select_id = "123456"
  )
  expect_equal(url, "/?tb=alp&wgt=cp: ;zs:CT;zc:T&zm=10.5&crds=40.02;70.01&sid=123456")

  url <- bookmark_build_url(
    id = "alp",
    widgets = NULL,
    lang = NULL,
    map_viewstate = list(
      zoom = 10.5,
      latitude = 70.01,
      longitude = 40.02
    ),
    select_id = "123456"
  )
  expect_equal(url, "/?tb=alp&zm=10.5&crds=40.02;70.01&sid=123456")

  url <- bookmark_build_url(
    id = "something",
    widgets = NULL,
    lang = NULL,
    map_viewstate = NULL,
    select_id = NULL
  )
  expect_equal(url, "/?tb=something")



  widgets <- list(
    `compare-ccpicker_var` = 3,
    `zoom_slider-ccslidertext_slt` = "Secteur de recensement",
    `zoom_auto-cccheckbox_cbx` = TRUE
  )
  expect_error(bookmark_build_url(
    id = "something",
    widgets = widgets,
    lang = "fr",
    map_viewstate = NULL,
    select_id = NULL
  ))
})
