#' Generate Warning Text for the Warn User Module
#'
#' This function generates warning text based on the provided conditions.
#' It is used internally by the \code{\link{warnuser_server}} function.
#'
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link{vars_build}} function.
#' @param data <`data.frame`> Data frame containing all the scale and
#' the `var_left` and `var_right`. The output of \code{\link{data_get}}.
#' @param time <`numeric named list`> Vector of time values to use for
#' appending a time to the variables picked.
#' @param widget_time <`numeric vector`> Raw time values coming out from the
#' time widget. It will be used to match with `time`, and if it doesn't fit,
#' will informe the warning.
#' @param more_text <`character`> Optional additional text to be added to the
#' warning message. Default is NULL.
#' @param lang <`character`> The language to be used for the warning messages.
#' Defaults to NULL for no translation
#'
#' @return A string containing the warning message in HTML format.
warnuser_get <- function(vars, data, time, widget_time, more_text = NULL, lang = NULL) {
  # Initiate the list
  out <- list()

  # Unique years
  left_year <- time$var_left
  right_year <- time$var_right

  # Vars title
  var_left_title <- var_get_title(
    var = vars$var_left,
    short_treshold = 50,
    translate = TRUE, lang = lang
  )
  var_right_title <- var_get_title(
    var = vars$var_right,
    short_treshold = 50,
    translate = TRUE, lang = lang
  )


  # Same year selected ------------------------------------------------------

  if (length(left_year) == 2 && left_year[1] == left_year[2]) {
    out <- c(
      out,
      list(cc_t(
        lang = lang,
        "Comparison requires two different dates."
      ))
    )
  }


  # Year displayed != year chosen -------------------------------------------

  year_mismatch_left <- (\(x) {
    if (!all(widget_time != "")) {
      return(NULL)
    }
    if (length(widget_time) == 2 & length(left_year) == 1) {
      return(NULL)
    }
    if (length(left_year) != 1) {
      return(NULL)
    }
    if (left_year == unique(widget_time)) {
      return(NULL)
    }

    # If all the previous passed
    list(cc_t(
      lang = lang,
      "Displayed data for <b>{var_left_title}</b> is for the ",
      "closest available year <b>({left_year})</b>."
    ))
  })()
  if (!is.null(year_mismatch_left)) out <- c(out, year_mismatch_left)

  year_mismatch_right <- (\(x) {
    if (!all(widget_time != "")) {
      return(NULL)
    }
    if (length(widget_time) == 2 & length(right_year) == 1) {
      return(NULL)
    }
    if (length(right_year) != 1) {
      return(NULL)
    }
    if (right_year == unique(widget_time)) {
      return(NULL)
    }

    # If all the previous passed
    list(cc_t(
      lang = lang,
      "Displayed data for <b>{var_right_title}</b> is for the ",
      "closest available year <b>({right_year})</b>."
    ))
  })()
  if (!is.null(year_mismatch_right)) out <- c(out, year_mismatch_right)


  # More condition for more disclaimers -------------------------------------

  if (!all(is.null(more_text))) {
    out <- c(out, lapply(more_text, cc_t, lang = lang))
  }


  # Make it an HTML paragraph -----------------------------------------------

  out <- lapply(out, \(x) {
    sprintf("<p class='sus-sidebar-widget-warning-text'>%s", x)
  })
  out <- paste0(out, collapse = "")


  # Return ------------------------------------------------------------------

  return(out)
}
