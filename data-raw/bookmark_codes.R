#### BOOKMARK SHORTER ##########################################################
bookmark_codes <- c(
  "compare-ccpicker_var" = "cp",
  "ccpicker_var" = "pv",
  "zoom_slider-ccslidertext_sldt" = "zs",
  "ccslidertext_sldt" = "st",
  "zoom_auto-cccheckbox_cbox" = "zc",
  "cccheckbox_cbox" = "cb"
)

bookmark_shorts <- c(
  "ccpicker_" = "pi",
  "ccslidertext_" = "sx",
  "cccheckbox_" = "ch"
)

if (length(c(bookmark_codes, bookmark_shorts)) !=
    length(unique(c(bookmark_codes, bookmark_shorts))))
  stop("Duplicates detected.")

usethis::use_data(bookmark_codes, overwrite = TRUE)
usethis::use_data(bookmark_shorts, overwrite = TRUE)
