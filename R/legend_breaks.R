#' Generic legend breaks function for Curbcut legends
#'
#' `legend_labels` is a generic function used to produce a vector of break
#' labels for the Curbcut legends. The function invokes
#' particular methods which depend on the class of the `vars` argument.
#'
#' @param vars <`named list`> A list object with a pre-determined class. The
#' output of \code{\link{vars_build}}.
#' @param data <`data.frame`> The current data. The output of \code{\link{data_get}}.
#' @param lang <`character`> String indicating the language to translate the
#' breaks to. Defaults to `NULL`, which is no translation.
#' @param ... Other arguments to be passed to the methods
#'
#' @return It returns an output of \code{\link[ggplot2]{labs}}.
#' @export
legend_breaks <- function(vars, data, lang = NULL, ...) {
  UseMethod("legend_breaks", vars)
}

#' @describeIn legend_breaks q5 method
#' @export
legend_breaks.q5 <- function(vars, data, lang = NULL, ...) {
  # Get pretty breaks
  pretty_breaks <- convert_unit(
    var = vars$var_left,
    x = attr(data, "breaks_var_left"),
    compact = TRUE
  )

  # Return
  return(pretty_breaks)
}

#' @describeIn legend_breaks q5_ind method
#' @export
legend_breaks.q5_ind <- function(vars, data, lang = NULL, ...) {
  # Grab the breaks from the variables table

  # NDS: take out the tryCatch once it's in vars
  breaks <-
    tryCatch(
      var_get_info(var = if (is.list(vars)) vars$var_left else vars, what = "rank_name_short",
                   translate = FALSE, lang = NULL)[[1]],
      error = function(e) {
        return(c("V. low", "Low", "Mod.", "High", "V. high"))
      })
  breaks <- breaks[!is.na(breaks)]

  # If the default, filter out some breaks to lighten the legend
  if (identical(breaks, c("V. low", "Low", "Mod.", "High", "V. high"))) {
    breaks <- c(
      cc_t(lang = lang, "Low"),
      sapply(1:3, \(x) NULL),
      cc_t(lang = lang, "High")
    )
  } else {
    breaks <- sapply(breaks, cc_t, lang = lang)
  }

  # Return
  return(breaks)
}

#' @describeIn legend_breaks q100 method
#' @export
legend_breaks.q100 <- function(vars, scale = NULL, lang = NULL, ...) {
  c(
    cc_t(lang = lang, "Low"),
    # sapply(1:9, \(x) NULL),
    # From viridis with 10 bins to well known left_5
    sapply(1:4, \(x) NULL),
    cc_t(lang = lang, "High")
  )
}

#' @describeIn legend_breaks qual method
#' @export
legend_breaks.qual <- function(vars, scale, lang = NULL, ...) {
  var_get_breaks(
    var = vars$var_left, scale = scale,
    break_col = "rank_name_short", q3_q5 = "q5",
    pretty = TRUE, compact = TRUE, lang = lang
  )
}

#' @describeIn legend_breaks delta method
#' @export
legend_breaks.delta <- function(vars, data, scale, ...) {
  breaks_delta(vars = vars$var_left, scale = scale, character = TRUE, data = data)
}

#' @describeIn legend_breaks bivar_ldelta_rq3 method
#' @export
legend_breaks.bivar_ldelta_rq3 <- function(vars, df, data, ...) {
  break_labs_y <- c(
    min(data$var_left, na.rm = TRUE),
    max(data$var_left[data$var_left_q3 == 1], na.rm = TRUE),
    max(data$var_left[data$var_left_q3 == 2], na.rm = TRUE),
    max(data$var_left, na.rm = TRUE)
  )
  break_labs_y <- convert_unit.pct(
    x = break_labs_y,
    compact = TRUE,
  )

  break_labs_x <- var_get_breaks(
    var = vars$var_right, df = df,
    q3_q5 = "q3", pretty = TRUE,
    compact = TRUE
  )

  return(list(x = break_labs_x, y = break_labs_y))
}

#' @describeIn legend_breaks bivar method
#' @export
legend_breaks.bivar <- function(vars, data, lang = NULL, ...) {
  break_labs_y <- convert_unit(
    var = vars$var_left,
    x = attr(data, "breaks_var_left"),
    compact = TRUE
  )
  break_labs_x <- convert_unit(
    var = vars$var_right,
    x = attr(data, "breaks_var_right"),
    compact = TRUE
  )
  return(list(x = break_labs_x, y = break_labs_y))
}

#' @describeIn legend_breaks delta_bivar method
#' @export
legend_breaks.delta_bivar <- function(vars, data, ...) {
  break_labs_y <- c(
    min(data$var_left, na.rm = TRUE),
    max(data$var_left[data$var_left_q3 == 1], na.rm = TRUE),
    max(data$var_left[data$var_left_q3 == 2], na.rm = TRUE),
    max(data$var_left, na.rm = TRUE)
  )

  break_labs_x <- c(
    min(data$var_right, na.rm = TRUE),
    max(data$var_right[data$var_right_q3 == 1], na.rm = TRUE),
    max(data$var_right[data$var_right_q3 == 2], na.rm = TRUE),
    max(data$var_right, na.rm = TRUE)
  )

  break_labs_y <- convert_unit.pct(
    x = break_labs_y,
    compact = TRUE
  )
  break_labs_x <- convert_unit.pct(
    x = break_labs_x,
    compact = TRUE,
  )

  return(list(x = break_labs_x, y = break_labs_y))
}
