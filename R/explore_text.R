#' Generate text for the given variables and region
#'
#' This function dispatches to the appropriate text-generating function based on
#' the variable type and returns the resulting text.
#'
#' @param vars <`character`> A list containing the variable names for which the
#' text needs to be generated. Usually the output of \code{\link{vars_build}}.
#' @param ... Additional arguments passed to the dispatched function.
#'
#' @return The resulting text.
#' @export
explore_text <- function(vars, ...) {
  UseMethod("explore_text", vars)
}

#' Generate text for the given variables and region - q5 version
#'
#' This function generates text for the given variables and region using the
#' q5 version. It dispatches to the appropriate text-generating function based on
#' the variable type and returns the resulting text.
#'
#' @param vars <`character`> A list containing the variable names for which the
#' text needs to be generated. Usually the output of \code{\link{vars_build}}.
#' @param region <`character`> String specifying the name of the region.
#' Usually equivalent of `r$region()`.
#' @param select_id A string indicating the ID of the currently selected region
#' (if any). Usually `r[[id]]$select_id()`
#' @param df <`character`> The combination of the region under study and the
#' scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param data <`data.frame`> A data frame containing the variables and
#' observations to be compared. The data frame must have columns named var_left
#' and ID. The output of \code{\link{data_get}}.
#' @param ... Additional arguments passed to the dispatched function.
#'
#' @return The resulting text.
#' @export
explore_text.q5 <- function(vars, region, select_id, df, data, ...) {

  # Grab the shared info
  context <- explore_context(region = region, select_id = select_id, df = df)

  # Grab the value string
  value_string <- explore_text_values_q5(var = vars$var_left, region = region,
                                         select_id = select_id, data = data,
                                         df = df)

  # Put it all together
  out <- sprintf("<p>%s, %s.", context$p_start, value_string$text)

  # Add the second paragraph if there is a selection
  if (!is.na(select_id) && !value_string$na) {
    # Add header
    out <- sprintf("<p><b>%s</b>%s", context$heading, out)

    # Get the information on how the selection compares
    relat <- explore_text_selection_comparison(var = vars$var_left, data = data,
                                               select_id = select_id)

    # Make the first sentence of the paragraph
    first_step <- sprintf("This is %s for %s", relat$rank_chr,
                          context$to_compare_determ)

    # Grab the explanation and capitalize the first letter
    exp <- var_get_info(vars$var_left, what = "explanation") |>
      s_sentence()

    # Plug the right elements for the final sentence
    second_step <- sprintf("%s %s is higher than in %s of other %s %s", exp,
                           context$name, relat$higher_than, context$scale_plur,
                           context$to_compare_short)

    # Bind it all
    out <- sprintf("%s<p>%s. %s.", out, first_step, second_step)
  }

  # Append date
  date <- var_get_time(vars$var_left)
  if (!is.na(date)) {
    out <- sprintf("%s <i>(Data from %s.)</i>", out, date)
  }

  # Return the text
  return(out)

}

#' Generate text for the given variables and region - q5 version
#'
#' This function generates text for the given variables and region using the
#' Q5 version. It dispatches to the appropriate text-generating function based on
#' the variable type and returns the resulting text.
#'
#' @param var <`character`> The variable name for which the text needs to be
#' generated. Usually `vars$var_left`
#' @param region <`character`> Character string specifying the name of the region.
#' Usually equivalent of `r$region()`.
#' @param ... Additional arguments passed to the dispatched function.
#'
#' @return The resulting text.
#' @export
explore_text_values_q5 <- function(var, region, ...) {
  UseMethod("explore_text_values_q5", var)
}

#' Generate text for the given variables and region - q5 version using percentage
#'
#' This function generates text for the given variables and region using the
#' q5 version and percentage. It returns the resulting text.
#'
#' @param var <`character`> The variable name for which the text needs to be
#' generated. Usually `vars$var_left`
#' @param region <`character`> Character string specifying the name of the region.
#' Usually equivalent of `r$region()`.
#' @param data <`data.frame`> The output of \code{\link{data_get}}.
#' @param df <`character`>The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param select_id <`character`> the current selected ID, usually
#' `r[[id]]$select_id()`.
#' @param ... Additional arguments passed to the function.
#'
#' @return The resulting text.
#' @export
explore_text_values_q5.pct <- function(var, region, data, df, select_id, ...) {

  # Grab the parent variable
  parent_string <- explore_text_parent_title(var)

  # Grab the q5 explanation
  exp <- var_get_info(var = var, what = "exp_q5")

  # Grab the region values
  region_values <- explore_text_region_val_df(var = var,
                                              region = region,
                                              data = data,
                                              df = df,
                                              select_id = select_id)

  # NA message
  if (is.na(region_values$val)) {
    exp <- var_get_info(var = var, what = "explanation")
    out <- sprintf("we currently don't have information regarding %s", exp)
    return(list(text = out,
                na = TRUE))
  }

  # Make the region values as characters
  pct_string <- convert_unit.pct(x = region_values$val, decimal = 1)
  count_string <- convert_unit(x = region_values$count, decimal = 1)

  # Build the return
  out <- sprintf("%s %s (%s) %s", count_string, parent_string, pct_string, exp)

  # Return
  return(list(text = out,
              na = FALSE))

}

#' Generate text for the given variables and region - q5 version using dollar
#'
#' This function generates text for the given variables and region using the
#' q5 version and dollar. It returns the resulting text.
#'
#' @param var <`character`> The variable name for which the text needs to be
#' generated. Usually `vars$var_left`
#' @param region <`character`> Character string specifying the name of the region.
#' Usually equivalent of `r$region()`.
#' @param data <`data.frame`> The output of \code{\link{data_get}}.
#' @param select_id <`character`> the current selected ID, usually
#' `r[[id]]$select_id()`.
#' @param ... Additional arguments passed to the function.
#'
#' @return The resulting text.
#' @export
explore_text_values_q5.dollar <- function(var, region, data, select_id, ...) {

  # Grab the region values
  region_values <- explore_text_region_val_df(var = var,
                                              region = region,
                                              data = data,
                                              select_id = select_id)

  # NA message
  if (is.na(region_values$val)) {
    exp <- var_get_info(var = var, what = "explanation")
    out <- sprintf("we currently don't have information regarding %s", exp)
    return(list(text = out,
                na = TRUE))
  }

  dollar_string <- convert_unit.dollar(x = region_values$val, compact = FALSE)

  # Grab the explanation
  exp <- var_get_info(var = var, what = "exp_q5")

  # Build the return
  out <- sprintf("%s %s", exp, dollar_string)

  # Return
  return(list(text = out,
              na = FALSE))

}

#' Generate text for the given variables and region - Q5 version using indices
#'
#' This function generates text for the given variables and region using the
#' q5 version and indices. It returns the resulting text.
#'
#' @param var <`character`> The variable name for which the text needs to be
#' generated. Usually `vars$var_left`
#' @param region <`character`> Character string specifying the name of the region.
#' Usually equivalent of `r$region()`.
#' @param data <`data.frame`> The output of \code{\link{data_get}}.
#' @param df <`character`>The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param select_id <`character`> the current selected ID, usually
#' `r[[id]]$select_id()`.
#' @param ... Additional arguments passed to the function.
#'
#' @return The resulting text.
#' @export
explore_text_values_q5.ind <- function(var, region, select_id, data, df, ...) {

  # Grab the parent variable
  parent_string <- explore_text_parent_title(var)

  # Grab the region values
  region_values <- explore_text_region_val_df(var = var,
                                              region = region,
                                              select_id = select_id,
                                              data = data,
                                              df = df)

  # NA message
  if (is.na(region_values$val)) {
    exp <- var_get_info(var = var, what = "explanation")
    out <- sprintf("we currently don't have information regarding %s", exp)
    return(list(text = out,
                na = TRUE))
  }

  # If there is no selection
  if (is.na(select_id)) {

    # Construct the region values
    pct_string <- convert_unit.pct(x = region_values$val, decimal = 1)
    count_string <- convert_unit(x = region_values$count, decimal = 1)

    # Sub the placeholder for the two last brackets
    breaks <- var_get_info(var = var, what = "breaks_q5")[[1]]
    breaks <- breaks[grepl(paste0("^", region, "_"), breaks$df), ]
    two_last_ranks <- tolower(breaks$rank_name[breaks$rank > 3])[1:2]
    # If the two last brackets is recognized as the default, write a particular string
    exp <- if (identical(two_last_ranks, c("above average", "high"))) {
      gsub("_X_", "a higher-than-average", exp)
    } else {
      gsub("_X_", sprintf("`%s` to `%s`", two_last_ranks[[1]],
                          two_last_ranks[[2]]), exp)
    }

    # Grab the explanation
    exp <- var_get_info(var = var, what = "exp_q5")

    # Build the return
    out <- sprintf("%s %s (%s) %s", count_string, parent_string, pct_string, exp)

    # Return
    return(list(text = out,
                na = TRUE))

  }

  # If there is a selection
  exp <- var_get_info(var = var, what = "explanation")

  # Build the return
  out <- sprintf("%s is %s", exp, region_values$val)

  # Return
  return(list(text = out,
              na = FALSE))

}

