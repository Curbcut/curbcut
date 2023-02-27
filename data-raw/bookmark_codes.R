#### BOOKMARK SHORTER ##########################################################
bookmark_codes <- c(
  "compare-ccpicker_var" = "cp",
  "ccpicker_var" = "pv",
  "zoom_slider-ccslidertext_slt" = "zs",
  "ccslidertext_slt" = "st",
  "zoom_auto-cccheckbox_cbx" = "zc",
  "cccheckbox_cbx" = "cb"
)

bookmark_shorts <- c(
  "ccpicker_" = "pi",
  "ccslidertext_" = "sx",
  "cccheckbox_" = "ch"
)

usethis::use_data(bookmark_codes, overwrite = TRUE)
usethis::use_data(bookmark_shorts, overwrite = TRUE)
