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

bookmark_cbox <- c("zc", "cb", "ch")
bookmark_picker <- c("cp", "pv", "pi")
bookmark_slidertxt <- c("zs", "st", "sx")

if (!all(c(bookmark_cbox, bookmark_picker, bookmark_slidertxt) %in%
         c(bookmark_codes, bookmark_shorts))) {
  stop("Bookmarks are not present in both categories")
}

if (!all(c(bookmark_codes, bookmark_shorts) %in%
         c(bookmark_cbox, bookmark_picker, bookmark_slidertxt))) {
  stop("Bookmarks are not present in both categories")
}

if (length(c(bookmark_codes, bookmark_shorts)) !=
    length(unique(c(bookmark_codes, bookmark_shorts))))
  stop("Duplicates detected.")

usethis::use_data(bookmark_codes, overwrite = TRUE)
usethis::use_data(bookmark_shorts, overwrite = TRUE)
usethis::use_data(bookmark_cbox, overwrite = TRUE)
usethis::use_data(bookmark_picker, overwrite = TRUE)
usethis::use_data(bookmark_slidertxt, overwrite = TRUE)
