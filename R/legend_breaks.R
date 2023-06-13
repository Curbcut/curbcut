#' Generic legend breaks function for Curbcut legends
#'
#' `legend_labels` is a generic function used to produce a vector of break
#' labels for the Curbcut legends. The function invokes
#' particular methods which depend on the class of the `vars` argument.
#'
#' @param vars <`named list`> A list object with a pre-determined class. The
#' output of \code{\link{vars_build}}.
#' @param ... Arguments to be passed to the methods, e.g. `df`, `data`, or
#' optionally `lang`.
#'
#' @return It returns an output of \code{\link[ggplot2]{labs}}.
#' @export
legend_breaks <- function(vars, ...) {
  UseMethod("legend_breaks", vars)
}

#' Compute legend breaks for a single `var_left`
#'
#' @param vars <`named list`> A list object of class `q5`. The output of
#' \code{\link{vars_build}}.
#' @param df <`character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param lang <`character`> String indicating the language to translate the
#' breaks to. Defaults to `NULL`, which is no translation.
#' @param ... Additional arguments passed to methods.
#'
#' @return A vector of legend breaks with pretty labels.
#' @export
legend_breaks.q5 <- function(vars, df, lang = NULL, ...) {
  # Get the breaks
  breaks <- var_get_breaks(
    var = vars$var_left, df = df,
    q3_q5 = "q5", pretty = TRUE, compact = TRUE
  )

  # Return
  return(breaks)
}

#' Compute legend breaks for a single `ind` `var_left`
#'
#' @param vars <`named list`> A list object of class `q5_ind`. The output of
#' \code{\link{vars_build}}.
#' @param df <`character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param lang <`character`> String indicating the language to translate the
#' breaks to. Defaults to `NULL`, which is no translation.
#' @param ... Additional arguments passed to methods.
#'
#' @return A vector of legend breaks with pretty labels.
#' @export
legend_breaks.q5_ind <- function(vars, df, lang = NULL, ...) {
  # Grab the breaks from the variables table
  breaks <-
    var_get_breaks(
      # In the cases we want to supply `var` instead of vars, ex. for graph
      # creation in `explore_graph_scale.ordinal`
      var = if (is.list(vars)) vars$var_left else vars, df = df,
      break_col = "rank_name_short", q3_q5 = "q5",
      pretty = TRUE, compact = TRUE, lang = NULL
    )
  breaks <- breaks[!is.na(breaks)]

  # If the default, filter out some breaks to lighten the legend
  if (identical(breaks, c(
    "Very low", "Low", "Moderate",
    "High", "Very high"
  ))) {
    breaks <- c(
      cc_t(lang = lang, "Low"),
      sapply(1:3, \(x) NULL),
      cc_t(lang = lang, "High")
    )
  } else {
    breaks <- sapply(breaks, cc_t)
  }

  # Return
  return(breaks)
}

#' Compute legend breaks for a variable displaying 100 breaks
#'
#' @param vars <`named list`> A list object of class `q100`. The output of
#' \code{\link{vars_build}}.
#' @param df <`character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param lang <`character`> String indicating the language to translate the
#' `Low` and `High` breaks to. Defaults to `NULL`, which is no translation.
#' @param ... Additional arguments passed to methods.
#'
#' @return A vector of legend breaks with pretty labels.
#' @export
legend_breaks.q100 <- function(vars, df = NULL, lang = NULL, ...) {
  c(
    cc_t(lang = lang, "Low"),
    sapply(1:9, \(x) NULL),
    cc_t(lang = lang, "High")
  )
}

#' Compute legend breaks for a qualitative variable
#'
#' @param vars <`named list`> A list object of class `qual`. The output of
#' \code{\link{vars_build}}.
#' @param df <`character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param lang <`character`> String indicating the language to translate the
#' breaks to. Defaults to `NULL`, which is no translation.
#' @param ... Additional arguments passed to methods.
#'
#' @return A vector of legend breaks with pretty labels.
#' @export
legend_breaks.qual <- function(vars, df, lang = NULL, ...) {
  var_get_breaks(
    var = vars$var_left, df = df,
    break_col = "rank_name_short", q3_q5 = "q5",
    pretty = TRUE, compact = TRUE, lang = lang
  )
}

#' Compute legend breaks for a single variable of two date times
#'
#' @param vars <`named list`> A list object of class `delta`.
#' @param ... Additional arguments passed to methods.
#'
#' @return A vector of legend breaks with hardcoded labels.
#' @export
legend_breaks.delta <- function(vars, ...) {
  c("-10%", "-2%", "+2%", "+10%")
}

#' Compute legend breaks for a comparison of a single variable of two date times
#' with another single year variable.
#'
#' The `bivariate_xdelta_yq3` class is used when there are multiple year selected
#' for the left-hand variables, but it can only be matched to one year of the
#' right-hand variable. We show a variation on the left-hand, and a static
#' year score on the right-hand.
#'
#' @param vars <`named list`> A list object of class `bivariate_xdelta_yq3`. The
#' output of \code{\link{vars_build}}.
#' @param df <`character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param data <`data.frame`> Must contains the `var_left` and `var_left_q3`
#' columns, which are used to extract the `q3` breaks (variation) for the single
#' variable of two date times.
#' @param ... Additional arguments passed to methods.
#'
#' @return A vector of legend breaks with pretty labels.
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

#' Compute legend breaks for a comparison of two variables
#'
#' The `delta` class is used when we compare two scores of the same left-hand
#' variables but in different years, e.g. "housing_tenant_2016" and
#' "housing_tenant_2021".
#'
#' @param vars <`named list`> A list object of class `delta`. The output of
#' \code{\link{vars_build}}.
#' @param df <`character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param ... Additional arguments passed to methods.
#'
#' @return A vector of legend breaks with pretty labels.
#' @export
legend_breaks.bivar <- function(vars, df, ...) {
  break_labs_y <- var_get_breaks(
    var = vars$var_left, df = df, q3_q5 = "q3", pretty = TRUE,
    compact = TRUE
  )
  break_labs_x <- var_get_breaks(
    var = vars$var_right, df = df, q3_q5 = "q3", pretty = TRUE,
    compact = TRUE
  )
  return(list(x = break_labs_x, y = break_labs_y))
}

#' Compute legend breaks for a comparison of the variable of two variables in
#' two years
#'
#' The `delta_bivar` class is used when we compare two variations together.
#'
#' @param vars <`named list`> A list object of class `delta_bivar`.
#' @param data <`data.frame`> Must contains the `var_left`, `var_left_q3`,
#' `var_right` and `var_right_q3` columns, which are used to extract the `q3`
#' breaks (variation) for the variables in time.
#' @param ... Additional arguments passed to methods.
#'
#' @return A vector of legend breaks with pretty labels.
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
