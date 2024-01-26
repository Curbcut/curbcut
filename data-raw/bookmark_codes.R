#### BOOKMARK SHORTER ##########################################################
bookmark_codes <- c(
  "compare-ccpicker_var" = "cp",
  "ccpicker_var" = "pv",
  "zoom_slider-ccslidertext_slt" = "zs",
  "ccslidertext_slt" = "st",
  "zoom_auto-cccheckbox_cbx" = "zc",
  "cccheckbox_cbx" = "cb",
  "ccslider_sld" = "sl",
  "ger-ccpicker_var" = "gr",
  "get-ccpicker_var" = "gt",
  "indicators_label-cb_advanced_controls-chcbx" = "ic",
  "ccslider_slu" = "nu",
  "ccslider_slb" = "nb",
  "ccslidertext_slu" = "tu",
  "ccslidertext_slb" = "tb"
)

bookmark_shorts <- c(
  "ccpicker_" = "pi",
  "ccslidertext_" = "sx",
  "cccheckbox_" = "ch",
  "ccslider_" = "sd"
)


# All unique?
if (!all(table(c(bookmark_codes, bookmark_shorts) |> unname()) == 1)) {
  stop("Duplicate bookmark codes")
}

usethis::use_data(bookmark_codes, overwrite = TRUE)
usethis::use_data(bookmark_shorts, overwrite = TRUE)
