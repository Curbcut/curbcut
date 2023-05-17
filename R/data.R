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

#' Translation dataframe (of front-facing Curbcut strings)
#'
#' @format ## `cc_translation_df`
#' A data.frame:
#' \describe{
#'   \item{en}{The english version of all the front-facing strings present in the Curbcut package}
#'   \item{fr}{The french translation of these strings}
#' }
"cc_translation_df"
