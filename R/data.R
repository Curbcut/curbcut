#' Make shorter often used widgets in bookmarks using codes
#'
#' @format ## `bookmark_codes`
#' A named vector:
#' \describe{
#'   \item{name}{The real widget namespace}
#'   \item{value}{New widget code}
#' }
"bookmark_codes"

#' Make shorter all widgets id using codes
#'
#' @format ## `bookmark_shorts`
#' A named vector:
#' \describe{
#'   \item{name}{The start of the widget `id`}
#'   \item{value}{`id` for which to switch to make bookmark URL smaller}
#' }
"bookmark_shorts"

#' Which widgets codes are checkboxes
#'
#' @format ## `bookmark_shorts`
#' A vector:
#' \describe{
#'   \item{codes}{Widget codes that are checkboxes}
#' }
"bookmark_cbox"

#' Which widgets codes are slider texts
#'
#' @format ## `bookmark_slidertxt`
#' A vector:
#' \describe{
#'   \item{codes}{Widget codes that are slider texts}
#' }
"bookmark_slidertxt"

#' Which widgets codes are pickers
#'
#' @format ## `bookmark_picker`
#' A vector:
#' \describe{
#'   \item{codes}{Widget codes that are pickers}
#' }
"bookmark_picker"
