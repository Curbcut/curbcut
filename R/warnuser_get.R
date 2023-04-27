#' Generate Warning Text for the Warn User Module
#'
#' This function generates warning text based on the provided conditions.
#' It is used internally by the \code{\link{warnuser_server}} function.
#'
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link{vars_build}} function.
#' @param time <`numeric vector`> Vector of time values to use for
#' appending a time to the variables picked.
#' @param data <`data.frame`> Data frame containing all the scale and
#' the `var_left` and `var_right`. The output of \code{\link{data_get}}.
#' @param more_text <`character`> Optional additional text to be added to the
#' warning message. Default is shiny::reactive(NULL).
#' @param lang <`character`> The language to be used for the warning messages.
#' Defaults to NULL for no translation
#'
#' @return A string containing the warning message in HTML format.
warnuser_get <- function(vars, data, time, more_text, lang = NULL) {

  out <- list()

  # Unique years
  left_year <- var_get_time(vars$var_left) |> unique()
  right_year <- var_get_time(vars$var_right) |> unique()

  # Vars title
  var_left_title <- var_get_title(var = vars$var_left,
                                  short_treshold = 50,
                                  translate = TRUE, lang = lang)
  var_right_title <- var_get_title(var = vars$var_right,
                                   short_treshold = 50,
                                   translate = TRUE, lang = lang)


  # Same year selected ------------------------------------------------------

  if (length(left_year) == 2 && left_year[1] == left_year[2]) {
    out <- c(out,
             list(cc_t(lang = lang,
                       "Comparison requires two different dates.")))
  }


  # Year displayed != year chosen -------------------------------------------

  length_mismatch <- length(time) == 2 & length(left_year) == 1

  # Year displayed LEFT
  if (length(left_year) == 1 & !length_mismatch) {
    if (left_year != unique(time)) {
      out <- c(out,
               list(cc_t(lang = lang,
                         "Displayed data for <b>{var_left_title}</b> is for the ",
                         "closest available year <b>({left_year})</b>.")
               ))
    }
  }

  # Year displayed RIGHT
  if (length(right_year) == 1 & !length_mismatch) {
    if (vars$var_right != " ") {
      if (all(right_year != unique(time))) {
        out <- c(out,
                 list(cc_t(lang = lang,
                           "Displayed data for <b>{var_right_title}</b> is for the ",
                           "closest available year <b>({right_year})</b>.")
                 ))
      }
    }
  }


  # More condition for more disclaimers -------------------------------------

  if (!all(is.null(more_text)))
    out <- c(out, lapply(more_text, cc_t, lang = lang))


  # Make it an HTML paragraph -----------------------------------------------

  out <- lapply(out, \(x) {
    sprintf("<p style 'font-size:12px; font-style:italic;'>%s", x)
  })
  out <- paste0(out, collapse = "")


  # Return ------------------------------------------------------------------

  return(out)

}
