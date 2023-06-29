#' Generate text for the given variables and region
#'
#' This function dispatches to the appropriate text-generating function based on
#' the type of `vars` and returns the resulting text.
#'
#' @param vars <`character`> A list containing the variable names for which the
#' text needs to be generated. Usually the output of \code{\link{vars_build}}.
#' @param region <`character`> String specifying the name of the region.
#' Usually equivalent of `r$region()`.
#' @param select_id <`character`> A string indicating the ID of the currently
#' selected region (if any). Usually `r[[id]]$select_id()`
#' @param df <`character`> The combination of the region under study and the
#' scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param data <`data.frame`> A data frame containing the variables and
#' observations to be compared. The output of \code{\link{data_get}}.
#' @param scales_as_DA <`character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their info will be the one of their DA.
#' @param lang <`character`> A string indicating the language in which to
#' translates the variable. Defaults to NULL. Usually is `r$lang()`.
#' @param ... Additional arguments passed to the dispatched function.
#'
#' @return The resulting text.
#' @export
explore_text <- function(vars, region, select_id, df, data, scales_as_DA,
                         lang, ...) {
  UseMethod("explore_text", vars)
}


# Q5 ----------------------------------------------------------------------

#' @rdname explore_text
#' @export
explore_text.q5 <- function(vars, region, select_id, df, data,
                            scales_as_DA = c("building", "street"),
                            lang = NULL, ...) {
  # Detect if we should switch the scale for DAs in the case the `df` is part
  # of the `scales_as_DA` argument.
  switch_DA <- is_scale_df(scales_as_DA, df)

  # Adjust the selected ID in the case where the selection is not in `data`
  if (!switch_DA && !select_id %in% data$ID) select_id <- NA

  # Grab the shared info
  context <- explore_context(
    region = region, select_id = select_id, df = df,
    switch_DA = switch_DA, lang = lang
  )

  # The context might have used a scale in the `scales_as_DA` argument, and
  # the select_id needs to be switched to that of the dissemination area.
  if ("select_id" %in% names(context)) select_id <- context$select_id

  # Check for NAs in the selected value. Return NA message if it is the case
  na_check <- explore_text_check_na(
    context = context, data = data,
    select_id = select_id, vars = vars,
    lang = lang
  )
  if (!is.null(na_check)) {
    return(na_check)
  }

  # Grab the value string
  value_string <- explore_text_values_q5(
    var = vars$var_left, region = region,
    select_id = select_id, data = data,
    df = context$treated_df, lang = lang
  )

  # Put it all together
  out <- sprintf("<p>%s, %s.", s_sentence(context$p_start), value_string$text)

  # Switch the final comma when there are bullet points
  out <- gsub("</ul>.$", ".</ul>", out)

  # Add the second paragraph if there is a selection
  if (!is.na(select_id) && !value_string$na) {
    # Add header
    out <- sprintf("<p><b>%s</b>%s", context$heading, out)

    # Get the information on how the selection compares
    relat <- explore_text_selection_comparison(
      var = vars$var_left, data = data,
      select_id = select_id, lang = lang
    )

    # Make the first sentence of the paragraph
    first_step <- sprintf(
      cc_t("This is %s for %s", lang = lang), relat$rank_chr,
      context$to_compare_determ
    )

    # Grab the explanation and capitalize the first letter
    exp <- var_get_info(vars$var_left,
      what = "explanation", translate = TRUE,
      lang = lang
    ) |>
      s_sentence()

    if (grepl("</ul>", exp)) {
      exp <- cc_t("This value", lang = lang)
    }

    # Plug the right elements for the final sentence
    second_step <- sprintf(
      cc_t("%s %s is higher than in %s of other %s %s", lang = lang), exp,
      context$p_start, relat$higher_than, context$scale_plur,
      context$to_compare_short
    )

    # Bind it all
    out <- sprintf("%s<p>%s. %s.", out, first_step, second_step)
  }

  # Append date
  date <- var_get_time(vars$var_left)
  if (!is.na(date)) {
    out <- sprintf(cc_t("%s <i>(Data from %s.)</i>", lang = lang), out, date)
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
#' @param col <`character`> Which column of `data` should be selected to grab the
#' value information. Defaults to `var_left`, but could also be `var_right` or
#' `var_left_1` in delta.
#' @param lang <`character`> A string indicating the language in which to
#' translates the variable. Defaults to NULL. Usually is `r$lang()`.
#' @param ... Additional arguments passed to the function.
#'
#' @return The resulting text.
#' @export
explore_text_values_q5.pct <- function(var, region, data, df, select_id,
                                       col = "var_left", lang, ...) {
  # Grab the parent variable
  parent_string <- explore_text_parent_title(var, lang = lang)

  # Grab the q5 explanation
  exp <- var_get_info(
    var = var, what = "exp_q5", translate = TRUE,
    lang = lang
  )

  # Grab the region values
  region_values <- explore_text_region_val_df(
    var = var,
    region = region,
    data = data,
    df = df,
    select_id = select_id,
    col = col,
    lang = lang
  )

  # NA message
  if (is.na(region_values$val)) {
    exp <- var_get_info(var = var, what = "explanation", translate = TRUE, lang = lang)
    out <- sprintf(cc_t("we currently don't have information regarding %s", lang = lang), exp)
    return(list(
      text = out,
      na = TRUE
    ))
  }

  # Make the region values as characters
  pct_string <- convert_unit.pct(x = region_values$val, decimal = 1)
  count_string <- convert_unit(x = region_values$count, decimal = 1)

  # Build the return
  out <- sprintf("%s %s (%s) %s", count_string, parent_string, pct_string, exp)

  # Return
  return(list(
    text = out,
    na = FALSE
  ))
}

#' Generate text for the given variables and region - q5 version using count
#'
#' This function generates text for the given variables and region using the
#' q5 version and count. It returns the resulting text.
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
#' @param col <`character`> Which column of `data` should be selected to grab the
#' value information. Defaults to `var_left`, but could also be `var_right` or
#' `var_left_1` in delta.
#' @param lang <`character`> A string indicating the language in which to
#' translates the variable. Defaults to NULL. Usually is `r$lang()`.
#' @param ... Additional arguments passed to the function.
#'
#' @return The resulting text.
#' @export
explore_text_values_q5.count <- function(var, region, data, df, select_id,
                                         col = "var_left", lang, ...) {
  # Grab the parent variable
  parent_string <- explore_text_parent_title(var, lang = lang)

  # Grab the q5 explanation
  exp <- var_get_info(var = var, what = "exp_q5", translate = TRUE, lang = lang)

  # Grab the region values
  region_values <- explore_text_region_val_df(
    var = var,
    region = region,
    data = data,
    df = df,
    select_id = select_id,
    col = col,
    lang = lang
  )

  # NA message
  if (is.na(region_values$val)) {
    exp <- var_get_info(
      var = var, what = "explanation", translate = TRUE,
      lang = lang
    )
    out <- sprintf(cc_t("we currently don't have information regarding %s",
      lang = lang
    ), exp)
    return(list(
      text = out,
      na = TRUE
    ))
  }

  # Make the region values as characters
  count_string <- convert_unit(x = region_values$val, decimal = 1)

  # Build the return
  out <- sprintf("%s %s %s", count_string, parent_string, exp)

  # Return
  return(list(
    text = out,
    na = FALSE
  ))
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
#' @param col <`character`> Which column of `data` should be selected to grab the
#' value information. Defaults to `var_left`, but could also be `var_right` or
#' `var_left_1` in delta.
#' @param lang <`character`> A string indicating the language in which to
#' translates the variable. Defaults to NULL. Usually is `r$lang()`.
#' @param ... Additional arguments passed to the function.
#'
#' @return The resulting text.
#' @export
explore_text_values_q5.dollar <- function(var, region, data, select_id,
                                          col = "var_left", lang, ...) {
  # Grab the region values
  region_values <- explore_text_region_val_df(
    var = var,
    region = region,
    data = data,
    select_id = select_id,
    col = col,
    lang = lang
  )

  # NA message
  if (is.na(region_values$val)) {
    exp <- var_get_info(
      var = var, what = "explanation", translate = TRUE,
      lang = lang
    )
    out <- sprintf(cc_t("we currently don't have information regarding %s",
      lang = lang
    ), exp)
    return(list(
      text = out,
      na = TRUE
    ))
  }

  dollar_string <- convert_unit.dollar(x = region_values$val, compact = FALSE)

  # Grab the explanation
  exp <- var_get_info(var = var, what = "exp_q5", translate = TRUE, lang = lang)

  # Build the return
  out <- sprintf("%s %s", exp, dollar_string)

  # Return
  return(list(
    text = out,
    na = FALSE
  ))
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
#' @param col <`character`> Which column of `data` should be selected to grab the
#' value information. Defaults to `var_left`, but could also be `var_right` or
#' `var_left_1` in delta.
#' @param lang <`character`> A string indicating the language in which to
#' translates the variable. Defaults to NULL. Usually is `r$lang()`.
#' @param ... Additional arguments passed to the function.
#'
#' @return The resulting text.
#' @export
explore_text_values_q5.ind <- function(var, region, select_id, data, df,
                                       col = "var_left", lang, ...) {
  # Grab the parent variable
  parent_string <- explore_text_parent_title(var, lang = lang)

  # Grab the region values
  region_values <- explore_text_region_val_df(
    var = var,
    region = region,
    select_id = select_id,
    data = data,
    df = df,
    col = col,
    lang = lang
  )

  # NA message
  if (is.na(region_values$val)) {
    exp <- var_get_info(
      var = var, what = "explanation", translate = TRUE,
      lang = lang
    )
    out <- sprintf(cc_t("we currently don't have information regarding %s",
      lang = lang
    ), exp)
    return(list(
      text = out,
      na = TRUE
    ))
  }

  # If there is no selection
  if (is.na(select_id)) {
    # Construct the region values
    pct_string <- convert_unit.pct(x = region_values$val, decimal = 1)
    count_string <- convert_unit(x = region_values$count, decimal = 1)

    # Grab the explanation
    exp_q5 <- var_get_info(
      var = var, what = "exp_q5", translate = TRUE,
      lang = lang
    )

    # Sub the placeholder for the two last brackets
    breaks <- var_get_info(var = var, what = "breaks_q5")[[1]]
    breaks <- breaks[grepl(paste0("^", region, "_"), breaks$df), ]
    two_last_ranks <- breaks$rank_name[breaks$rank > 3][1:2]
    two_last_ranks <- sapply(two_last_ranks, cc_t, lang = lang)
    two_last_ranks <- tolower(two_last_ranks)
    # If the two last brackets is recognized as the default, write a particular string
    exp <- {
      gsub("_X_", sprintf(
        cc_t("'%s' to '%s'", lang = lang), two_last_ranks[[1]],
        two_last_ranks[[2]]
      ), exp_q5)
    }

    # Build the return
    out <- sprintf("%s %s (%s) %s", count_string, parent_string, pct_string, exp)

    # Return
    return(list(
      text = out,
      na = TRUE
    ))
  }

  # If there is a selection
  exp <- var_get_info(
    var = var, what = "explanation", translate = TRUE,
    lang = lang
  )

  # Build the return
  out <- sprintf(cc_t("%s is %s", lang = lang), exp, region_values$val)

  # Return
  return(list(
    text = out,
    na = FALSE
  ))
}

#' Generate text for the given variables and region - Q5 version using avg
#'
#' This function generates text for the given variables and region using the
#' q5 version and avg. It returns the resulting text.
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
#' @param col <`character`> Which column of `data` should be selected to grab the
#' value information. Defaults to `var_left`, but could also be `var_right` or
#' `var_left_1` in delta.
#' @param lang <`character`> A string indicating the language in which to
#' translates the variable. Defaults to NULL. Usually is `r$lang()`.
#' @param ... Additional arguments passed to the function.
#'
#' @return The resulting text.
#' @export
explore_text_values_q5.avg <- function(var, region, select_id, data, df,
                                       col = "var_left", lang, ...) {
  # Grab the parent variable
  parent_string <- explore_text_parent_title(var)

  # Grab the region values
  region_values <- explore_text_region_val_df(
    var = var,
    region = region,
    select_id = select_id,
    data = data,
    df = df,
    col = col,
    lang = lang
  )

  # NA message
  if (is.na(region_values$val)) {
    exp <- var_get_info(
      var = var, what = "explanation", translate = TRUE,
      lang = lang
    )
    out <- sprintf(cc_t("we currently don't have information regarding %s",
      lang = lang
    ), exp)
    return(list(
      text = out,
      na = TRUE
    ))
  }

  # If there is no selection
  if (is.na(select_id)) {
    # Construct the region values
    count_string <- convert_unit(x = region_values$val, decimal = 1)

    # Grab the explanation
    exp_q5 <- var_get_info(
      var = var, what = "exp_q5", translate = TRUE,
      lang = lang
    )

    # If the two last brackets is recognized as the default, write a particular string
    out <- gsub("_X_", count_string, exp_q5)

    # Return
    return(list(
      text = out,
      na = TRUE
    ))
  }

  # If there is a selection
  exp <- var_get_info(
    var = var, what = "explanation", translate = TRUE,
    lang = lang
  )

  # Build the return
  count_string <- convert_unit(x = region_values$val, decimal = 1)
  out <- sprintf(cc_t("%s is %s", lang = lang), exp, count_string)

  # Return
  return(list(
    text = out,
    na = FALSE
  ))
}

#' Generate text for the given variables and region - Q5 version using sqkm
#'
#' This function generates text for the given variables and region using the
#' q5 version and avg. It returns the resulting text.
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
#' @param col <`character`> Which column of `data` should be selected to grab the
#' value information. Defaults to `var_left`, but could also be `var_right` or
#' `var_left_1` in delta.
#' @param lang <`character`> A string indicating the language in which to
#' translates the variable. Defaults to NULL. Usually is `r$lang()`.
#' @param ... Additional arguments passed to the function.
#'
#' @return The resulting text.
#' @export
explore_text_values_q5.sqkm <- function(var, region, select_id, data, df,
                                        col = "var_left", lang, ...) {
  # Grab the region values
  region_values <- explore_text_region_val_df(
    var = var,
    region = region,
    data = data,
    select_id = select_id,
    col = col,
    lang = lang
  )

  # NA message
  if (is.na(region_values$val)) {
    exp <- var_get_info(
      var = var, what = "explanation", translate = TRUE,
      lang = lang
    )
    out <- sprintf(cc_t("we currently don't have information regarding %s",
      lang = lang
    ), exp)
    return(list(
      text = out,
      na = TRUE
    ))
  }

  # Construct the region values
  count_string <- convert_unit(x = region_values$val, decimal = 1)

  # Grab the explanation
  exp_q5 <- var_get_info(
    var = var, what = "exp_q5", translate = TRUE,
    lang = lang
  )

  # If the two last brackets is recognized as the default, write a particular string
  out <- gsub("_X_", count_string, exp_q5)

  # Return
  return(list(
    text = out,
    na = FALSE
  ))
}

#' Generate text for the given variables and region - Q5 version using per1k
#'
#' This function generates text for the given variables and region using the
#' q5 version and avg. It returns the resulting text. It is mirroring the `sqkm`
#' method.
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
#' @param col <`character`> Which column of `data` should be selected to grab the
#' value information. Defaults to `var_left`, but could also be `var_right` or
#' `var_left_1` in delta.
#' @param lang <`character`> A string indicating the language in which to
#' translates the variable. Defaults to NULL. Usually is `r$lang()`.
#' @param ... Additional arguments passed to the function.
#'
#' @return The resulting text.
#' @export
explore_text_values_q5.per1k <- function(var, region, select_id, data, df,
                                         col = "var_left", lang = lang, ...) {
  explore_text_values_q5.sqkm(
    var = var, region = region, select_id = select_id,
    data = data, df = df, col = col, lang = lang, ...
  )
}

#' Generate text for the given variables and region - Q5 version using ppo
#'
#' This function generates text for the given variables and region using the
#' q5 version and avg. It returns the resulting text.
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
#' @param col <`character`> Which column of `data` should be selected to grab the
#' value information. Defaults to `var_left`, but could also be `var_right` or
#' `var_left_1` in delta.
#' @param lang <`character`> A string indicating the language in which to
#' translates the variable. Defaults to NULL. Usually is `r$lang()`.
#' @param ... Additional arguments passed to the function.
#'
#' @return The resulting text.
#' @export
explore_text_values_q5.ppo <- function(var, region, select_id, data, df,
                                       col = "var_left", lang, ...) {
  # Grab the region values
  region_values <- explore_text_region_val_df(
    var = var,
    region = region,
    data = data,
    select_id = select_id,
    col = col,
    lang = lang
  )

  # NA message
  if (is.na(region_values$val)) {
    exp <- var_get_info(
      var = var, what = "explanation", translate = TRUE,
      lang = lang
    )
    out <- sprintf(cc_t("we currently don't have information regarding %s",
      lang = lang
    ), exp)
    return(list(
      text = out,
      na = TRUE
    ))
  }

  # Construct the region values
  count_string <- convert_unit(x = region_values$val, decimal = 1)

  # Grab the explanation
  exp_q5 <- var_get_info(
    var = var, what = "exp_q5", translate = TRUE,
    lang = lang
  )

  # If the two last brackets is recognized as the default, write a particular string
  out <- cc_t(sprintf("there are %s people for every %s", count_string, exp_q5),
    lang = lang
  )

  # Return
  return(list(
    text = out,
    na = FALSE
  ))
}


# BIVAR -------------------------------------------------------------------

#' @rdname explore_text
#' @export
explore_text.bivar <- function(vars, region, select_id, df, data,
                               scales_as_DA = c("building", "street"),
                               lang = NULL, ...) {
  # Append date function helper
  append_date <- \(out) {
    date_1 <- var_get_time(vars$var_left)
    date_2 <- var_get_time(vars$var_right)
    date <- if (is.na(date_1) & is.na(date_2)) {
      NA
    } else if (is.na(date_1) & !is.na(date_2)) {
      explore_text_color(date_2, "right")
    } else if (!is.na(date_1) & is.na(date_2)) {
      explore_text_color(date_1, "left")
    } else if (date_1 == date_2) {
      # No color if both dates are the same.
      date_1
    } else {
      sprintf(
        cc_t("%s and %s", lang = lang), explore_text_color(date_1, "left"),
        explore_text_color(date_2, "right")
      )
    }

    # Add the date
    out <- if (!is.na(date)) {
      sprintf(cc_t("%s <i>(Data from %s.)</i>", lang = lang), out, date)
    } else {
      out
    }

    # Final check if there are bullet point lists, add a ellipsis
    gsub("</ul></span> ", "</ul></span><p>...", out)
  }

  # Detect if we should switch the scale for DAs in the case the `df` is part
  # of the `scales_as_DA` argument.
  switch_DA <- is_scale_df(scales_as_DA, df)

  # Adjust the selected ID in the case where the selection is not in `data`
  if (!switch_DA && !select_id %in% data$ID) select_id <- NA

  # Grab the shared info
  context <- explore_context(
    region = region, select_id = select_id, df = df,
    switch_DA = switch_DA, lang = lang
  )

  # The context might have used a scale in the `scales_as_DA` argument, and
  # the select_id needs to be switched to that of the dissemination area.
  if ("select_id" %in% names(context)) select_id <- context$select_id

  # Check for NAs in the selected value. Return NA message if it is the case
  na_check <- explore_text_check_na(
    context = context, data = data,
    select_id = select_id, vars = vars,
    lang = lang
  )
  if (!is.null(na_check)) {
    return(na_check)
  }

  # If there is a selection, return a completely diferent text
  if (!is.na(select_id)) {
    # Grab the value string
    value_string_left <- explore_text_values_q5(
      var = vars$var_left, region = region,
      select_id = select_id, data = data,
      df = context$treated_df, lang = lang
    )

    # Grab the value string
    value_string_right <- explore_text_values_q5(
      var = vars$var_right, region = region,
      select_id = select_id, data = data,
      df = context$treated_df,
      col = "var_right", lang = lang
    )

    # Add the coloring
    value_string_left$text <-
      explore_text_color(value_string_left$text, meaning = "left")
    value_string_right$text <-
      explore_text_color(value_string_right$text, meaning = "right")

    # If one of the value is NA, return that there is a missing value
    if (value_string_left$na) {
      return(sprintf(
        "<p>%s, %s.", s_sentence(context$p_start),
        value_string_left$text
      ))
    }
    if (value_string_right$na) {
      return(sprintf(
        "<p>%s, %s.", s_sentence(context$p_start),
        value_string_right$text
      ))
    }

    # Start with the header
    out <- sprintf("<p><b>%s</b>", context$heading)

    out <- sprintf(
      cc_t("%s<p>%s, %s and %s.", lang = lang), out, s_sentence(context$p_start),
      value_string_left$text, value_string_right$text
    )

    # Grab the two texts for var_left and var_right
    compare_texts <- lapply(vars, \(var) {
      col <- if (var == vars$var_left) "var_left" else "var_right"

      # Get the information on how the selection compares
      relat <- explore_text_selection_comparison(
        var = var, data = data,
        select_id = select_id,
        col = col,
        lang = lang
      )

      # Grab the explanation
      exp <- var_get_info(var, what = "explanation", translate = TRUE, lang = lang)
      # If there are bullet points, change the explanation to something more generic
      if (grepl("</ul>", exp)) {
        out <- if (col == "var_left") {
          "The first value"
        } else {
          "The second value"
        }
        exp <- cc_t(out, lang = lang)
      }
      # If left, starts with a capital letter.
      if (col == "var_left") exp <- s_sentence(exp)
      exp <- explore_text_color(
        exp,
        meaning = if (col == "var_left") "left" else "right"
      )

      # Plug the right elements for the final sentence
      first_step_1 <- if (var == vars$var_left) {
        sprintf("%s %s", exp, context$p_start)
      } else {
        exp
      }
      first_step <- sprintf(
        cc_t("%s is higher than in %s of other %s", lang = lang), first_step_1,
        relat$higher_than, context$scale_plur
      )

      # Make the second step of the sentence
      second_step <- sprintf(
        cc_t("which is %s for %s", lang = lang), relat$rank_chr,
        context$to_compare_determ
      )

      return(list(
        higher_than = relat$higher_than_num,
        text = sprintf("%s, %s", first_step, second_step)
      ))
    })

    # Is the rank similar or different
    percs <- sapply(compare_texts, `[[`, "higher_than")
    percs_distance <- abs(percs[[1]] - percs[[2]])
    connector <- if (percs_distance > 0.2) "By contrast" else "Similarly"
    connector <- cc_t(connector, lang = lang)

    # Bind it all
    out <- sprintf(
      "%s<p>%s. %s, %s.", out, compare_texts$var_left$text,
      connector, compare_texts$var_right$text
    )

    return(append_date(out))
  }

  # Scales
  scales_dictionary <- get_from_globalenv("scales_dictionary")
  scale_vec <- is_scale_df(scales_dictionary$scale, df, vectorized = TRUE)
  scale_plur <- cc_t(scales_dictionary$plur[scale_vec], lang = lang)

  # Correlation
  relation <- explore_text_bivar_correlation(vars, data, lang = lang)

  # If there is no correlation, the text is slightly different
  if (relation$no_correlation) {
    # Explanations
    left_exp <- var_get_info(vars$var_left,
      what = "explanation",
      translate = TRUE, lang = lang
    ) |>
      explore_text_color(meaning = "left")

    right_exp <- var_get_info(vars$var_right,
      what = "explanation",
      translate = TRUE, lang = lang
    ) |>
      explore_text_color(meaning = "right")

    out <- sprintf(
      cc_t("%s, there is %s (%s) between %s and %s in %s.", lang = lang),
      s_sentence(context$p_start), relation$relation_text, relation$corr,
      left_exp, right_exp, scale_plur
    )

    return(append_date(out))
  }

  # Adjectives
  left_adj <- explore_text_bivar_adjective(
    var = vars$var_left,
    left = TRUE,
    lang = lang
  )
  right_adj <- explore_text_bivar_adjective(
    var = vars$var_right,
    left = FALSE,
    positive = relation$positive,
    lang = lang
  )

  # Explanations
  left_exp <- var_get_info(vars$var_left,
    what = "explanation_nodet",
    translate = TRUE, lang = lang
  ) |>
    explore_text_color(meaning = "left")
  right_exp <- var_get_info(vars$var_right,
    what = "explanation_nodet",
    translate = TRUE, lang = lang
  ) |>
    explore_text_color(meaning = "right")

  # Paragraphs
  first_p <-
    if (grepl("_X_", relation$relation_text)) {
      # When the relationship is 'weak', it's a two-part relation_text
      relation_right_ajd_exp <-
        gsub(
          "_X_", sprintf("%s %s", right_adj, right_exp),
          relation$relation_text
        )

      sprintf(
        cc_t("%s, %s with %s %s %s.", lang = lang),
        s_sentence(context$p_start), scale_plur, left_adj, left_exp,
        relation_right_ajd_exp
      )
    } else {
      sprintf(
        cc_t("%s, %s with %s %s %s %s %s.", lang = lang),
        s_sentence(context$p_start), scale_plur, left_adj, left_exp,
        relation$relation_text, right_adj, right_exp
      )
    }
  second_p <- sprintf(
    cc_t("There is a %s (%s) between these two variables.", lang = lang),
    relation$corr_strength, relation$corr
  )

  # Bind the two paragraphs
  out <- sprintf("<p>%s<p>%s", first_p, second_p)

  # Write STRONG CORRELATION if it is the case
  if (relation$strong) {
    out <- sprintf(cc_t("<p><b>STRONG CORRELATION</b>%s", lang = lang), out)
  }

  return(append_date(out))
}

#' Function for exploring bivariate correlation between two variables
#'
#' This function calculates bivariate correlation between two variables and
#' returns a list containing the correlation coefficient, a boolean indicating
#' whether the correlation is positive or negative, a text string describing the
#' strength and direction of the correlation, and a text string describing the
#' relationship between the variables.
#'
#' @param vars <`character`> A list containing the variable names for which the
#' text needs to be generated. Usually the output of \code{\link{vars_build}}.
#' @param data <`data.frame`> A data frame containing the variables and
#' observations to be compared. The output of \code{\link{data_get}}.
#' @param lang <`character`> A string indicating the language in which to
#' translates the variable. Defaults to NULL.
#'
#' @return A list containing the correlation coefficient, a boolean indicating
#' whether the correlation is positive or negative, a text string describing
#' the strength and direction of the correlation, and a text string describing
#' the relationship between the variables.
explore_text_bivar_correlation <- function(vars, data, lang = NULL) {
  # Get correlation and method string
  corr <- explore_text_bivar_correlation_helper(
    vars = vars,
    data = data,
    lang = lang
  )

  # Is the correlation positive
  positive <- corr$corr > 0
  positive_string <- if (positive) "positive" else "negative"
  positive_string <- cc_t(positive_string, lang = lang)

  # Correlation strings
  absolute <- abs(corr$corr)
  # Flag the correlation as NOT strong to start with.
  strong <- FALSE
  # Flag the correlation as inexistant to start with
  no_correlation <- FALSE

  if (absolute > 0.7) {
    relation_text <- cc_t("almost always have", lang = lang)
    strength <- cc_t("strong", lang = lang)
    strong <- TRUE

    corr_strength <- sprintf(
      cc_t("%s %s correlation", lang = lang), strength,
      positive_string
    )
  } else if (absolute > 0.3) {
    relation_text <- cc_t("tend to have", lang = lang)
    strength <- cc_t("moderate", lang = lang)

    corr_strength <- sprintf(
      cc_t("%s %s correlation", lang = lang), strength,
      positive_string
    )
  } else if (absolute > 0.1) {
    relation_text <- cc_t("often have _X_, although with many exceptions",
      lang = lang
    )
    strength <- cc_t("weak", lang = lang)

    corr_strength <- sprintf(
      cc_t("%s %s correlation", lang = lang), strength,
      positive_string
    )
  } else {
    relation_text <- cc_t("effectively no relationship", lang = lang)
    strength <- cc_t("effectively no correlation", lang = lang)
    no_correlation <- TRUE

    corr_strength <- strength
  }

  return(list(
    corr = corr$corr_string,
    strong = strong,
    positive = positive,
    no_correlation = no_correlation,
    relation_text = relation_text,
    corr_strength = corr_strength
  ))
}

#' Helper function for generating adjective to describe bivariate relationship
#' between text variables
#'
#' This function generates a text string containing an adjective to describe the
#' bivariate relationship between two variables based on whether the relationship
#' is positive or negative.
#'
#' @param var <`character`> The variable code for which the text needs to be
#' generated. `vars$var_left` or `vars$var_right`
#' @param left <`logical>` Whether the `var` supplied is the var_left
#' or the `var_right`. If `var_left`, TRUE.
#' @param positive <`logical`> Wheter the bivariate relationship is positive
#' or negative. One of the output of \code{\link{explore_text_bivar_correlation}}
#' @param lang <`character`> A string indicating the language in which to
#' translates the variable. Defaults to NULL.
#' @param ... Additional arguments to be passed.
#'
#' @return A text string containing an adjective to describe the bivariate
#' relationship.
#' @export
explore_text_bivar_adjective <- function(var, left, positive, lang = NULL, ...) {
  UseMethod("explore_text_bivar_adjective", var)
}

#' @rdname explore_text_bivar_adjective
#' @export
explore_text_bivar_adjective.dollar <- function(var, left, positive, lang,
                                                ...) {
  string <- (\(x) {
    if (left) {
      return(cc_t("higher", lang = lang))
    }
    if (positive) {
      return(cc_t("higher", lang = lang))
    }
    return(cc_t("lower", lang = lang))
  })()

  return(sprintf("<b>%s</b>", string))
}

#' @rdname explore_text_bivar_adjective
#' @export
explore_text_bivar_adjective.default <- function(var, left, positive, lang,
                                                 ...) {
  string <- (\(x) {
    if (left) {
      return(cc_t("a higher", lang = lang))
    }
    if (positive) {
      return(cc_t("a higher", lang = lang))
    }
    return(cc_t("a lower", lang = lang))
  })()

  return(sprintf("<b>%s</b>", string))
}


# DELTA -------------------------------------------------------------------

#' @rdname explore_text
#' @export
explore_text.delta <- function(vars, region, select_id, df, data,
                               scales_as_DA = c("building", "street"),
                               lang = NULL, ...) {
  # Detect if we should switch the scale for DAs in the case the `df` is part
  # of the `scales_as_DA` argument.
  switch_DA <- is_scale_df(scales_as_DA, df)

  # Adjust the selected ID in the case where the selection is not in `data`
  if (!switch_DA && !select_id %in% data$ID) select_id <- NA

  # Grab the shared info
  context <- explore_context(
    region = region, select_id = select_id, df = df,
    switch_DA = switch_DA, lang = lang
  )

  # The context might have used a scale in the `scales_as_DA` argument, and
  # the select_id needs to be switched to that of the dissemination area.
  if ("select_id" %in% names(context)) select_id <- context$select_id

  # Check for NAs in the selected value. Return NA message if it is the case
  na_check <- explore_text_check_na(
    context = context, data = data,
    select_id = select_id, vars = vars,
    lang = lang
  )
  if (!is.null(na_check)) {
    return(na_check)
  }

  # Grab the explanation and region values
  exp_vals <- explore_text_delta_exp(
    var = vars$var_left, region = region,
    select_id = select_id, data = data,
    df = context$treated_df, left_right = "left", lang = lang
  )

  # Get the necessary information for the second paragraph
  change_string <- explore_text_delta_change(
    var = vars$var_left,
    exp_vals = exp_vals,
    lang = lang
  )

  # Construct the first paragraph. Tweaks when `ind`, so dispatched
  out <- explore_text_delta_first_p(
    var = vars$var_left, context = context,
    exp_vals = exp_vals, lang = lang,
    select_id = select_id, change_string = change_string
  )

  # Return the first paragraph if there are no selections
  if (is.na(select_id)) {
    # Add ellipsis if there are bullet points
    out <- gsub("</ul> ", "</ul><p>...", out)
    return(out)
  }

  # Add the header for the selection
  out <- sprintf("<p><b>%s</b><p>%s", context$heading, out)

  inc_dec <- if (change_string$pct_change > 0) {
    cc_t("increase", lang = lang) |>
      explore_text_color(meaning = "increase")
  } else {
    cc_t("decrease", lang = lang) |>
      explore_text_color(meaning = "decrease")
  }
  exp_nodet <- var_get_info(vars$var_left,
    what = "explanation_nodet", translate = TRUE,
    lang =
    )
  # If the explanation is a bullet point, switch to something more generic
  if (grepl("</ul>", exp_nodet)) {
    exp_nodet <- cc_t("the value", lang = lang)
  }
  relat <- explore_text_selection_comparison(
    var = vars$var_left,
    data = data,
    select_id = select_id,
    col = "var_left",
    ranks_override = c(
      "exceptionally small", "unusually small",
      "just about average", "unusually large",
      "exceptionally large"
    ),
    lang = lang
  )

  # If `ind` and data remained the same, we add 'slight' decrease/increase
  subj <- if ("ind" %in% class(vars$var_left)) {
    if (exp_vals$remained) {
      cc_t("The slight", lang = lang)
    } else {
      cc_t("This", lang = lang)
    }
  } else {
    cc_t("This", lang = lang)
  }

  # Craft the second paragraph
  first_part <- sprintf(
    cc_t("%s %s is %s for %s.", lang = lang), subj, inc_dec,
    relat$rank_chr, context$to_compare_deter
  )
  second_part <-
    sprintf(
      cc_t(paste0(
        "The change in %s %s between %s and %s is larger than in %s of ",
        "other %s between the same years."
      ), lang = lang),
      exp_nodet, context$p_start, exp_vals$times[1],
      exp_vals$times[2], relat$higher_than, context$scale_plur
    )

  out <- sprintf("%s<p>%s %s", out, first_part, second_part)

  # Return
  # Add ellipsis if there are bullet points
  out <- gsub("</ul> ", "</ul><p>...", out)
  return(out)
}

#' Explore text `delta` explanation and values for
#'
#' This function delivers the delta text and values of a given variable when
#' compared between two yeas. It dispatches to specific methods depending on the
#' structure of the variable.
#'
#' @param var <`character`> The variable code for which the text and values need
#' to be generated. Usually `vars$var_left`.
#' @param region <`character`> Character string specifying the name of the region.
#' Usually equivalent of `r$region()`.
#' @param select_id A string indicating the ID of the currently selected region
#' (if any). Usually `r[[id]]$select_id()`
#' @param left_right <`character`> Is it a left or right variable? Possible
#' options are "left" or "right".
#' @param df <`character`> The combination of the region under study and the
#' scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param data <`data.frame`> A data frame containing the variables and
#' observations to be compared. The data frame must have columns named var_left
#' and ID. The output of \code{\link{data_get}}.
#' @param ... Additional arguments passed to the dispatched method.
#'
#' @return A list with the following elements:
#' \item{exp}{A character string describing the explanation of the variable}
#' \item{region_vals}{A numeric vector containing the variable values in the
#' current region and time period.}
#' \item{region_vals_strings}{A character vector with the same length as
#' \code{region_vals}, containing the variable values formatted according
#' to the class of `var`}
explore_text_delta_exp <- function(var, region, select_id, left_right = "left",
                                   df, data, ...) {
  UseMethod("explore_text_delta_exp", var)
}

#' @rdname explore_text_delta_exp
#' @param lang <`character`> Language for translation.
#' @export
explore_text_delta_exp.ind <- function(var, region, select_id, left_right = "left",
                                       df, data, lang, ...) {
  # If there is no selection
  if (is.na(select_id)) {
    # Grab the parent variable
    parent <- var_get_info(var = var[[1]], what = "parent_vec")
    parent <- cc_t(parent, lang = lang)

    # Grab the explanation
    exp_q5 <- var_get_info(
      var = var[[1]], what = "exp_q5", translate = TRUE,
      lang = lang
    )

    # Sub the placeholder for the two last brackets
    breaks <- var_get_info(var = var[[1]], what = "breaks_q5")[[1]]
    breaks <- breaks[grepl(paste0("^", region, "_"), breaks$df), ]
    two_last_ranks <- breaks$rank_name[breaks$rank > 3][1:2]
    two_last_ranks <- sapply(two_last_ranks, cc_t, lang = lang)
    two_last_ranks <- tolower(two_last_ranks)
    # If the two last brackets is recognized as the default, write a particular string
    exp <- {
      gsub("_X_", sprintf(
        cc_t("'%s' to '%s'", lang = lang), two_last_ranks[[1]],
        two_last_ranks[[2]]
      ), exp_q5)
    }

    # Grab the region values
    times <- var_get_time(var)
    region_vals <- var_get_info(var[[1]], what = "region_values")[[1]]
    region_vals <- region_vals[region_vals$region == region, ]
    region_vals <- region_vals$val[region_vals$year %in% times]
    region_vals_strings <- convert_unit.pct(var,
      x = region_vals,
      decimal = 1
    )

    # Return
    return(list(
      exp = sprintf(cc_t("the percentage of %s that %s", lang = lang), parent, exp),
      region_vals = region_vals,
      region_vals_strings = region_vals_strings,
      times = times
    ))
  }

  # If there is a selection
  exp <- var_get_info(
    var = var[[1]], what = "explanation", translate = TRUE,
    lang = lang
  )
  if (grepl("</ul>", exp)) {
    out <- if (left_right == "left") {
      "the first value"
    } else {
      "the second value"
    }
    exp <- cc_t(out, lang = lang)
  }
  times <- var_get_time(var)

  # Grab both value strings
  rank_chr_before <- explore_text_selection_comparison(
    var = var[1],
    data = data,
    select_id = select_id,
    col = "var_left_1",
    lang = lang
  )$rank_chr

  rank_chr_after <- explore_text_selection_comparison(
    var = var[2],
    data = data,
    select_id = select_id,
    col = "var_left_2",
    lang = lang
  )$rank_chr

  # Did it remain in the same category, or it moved?
  remained <- rank_chr_before == rank_chr_after

  # Grab the region values
  region_vals <-
    lapply(var, \(x) explore_text_region_val_df(
      var = structure(x, class = class(var)),
      region = region,
      select_id = select_id,
      data = data,
      df = df,
      col = sprintf("%s_%s", sprintf("var_%s", left_right), which(x == var)),
      lang = lang
    )$num) |> unlist()
  # Newest value must be first, like for the no-selection values
  region_vals <- rev(region_vals)

  # Return
  return(list(
    exp = exp,
    region_vals = region_vals,
    region_vals_strings = c(rank_chr_before, rank_chr_after),
    remained = remained,
    times = times
  ))
}

#' @rdname explore_text_delta_exp
#' @export
explore_text_delta_exp.default <- function(var, region, select_id,
                                           left_right = "left", df, data,
                                           lang, ...) {
  # Grab the explanation
  exp <- var_get_info(var[[1]],
    what = "explanation", translate = TRUE,
    lang = lang
  )
  if (grepl("</ul>", exp)) {
    out <- if (left_right == "left") {
      "the first value"
    } else {
      "the second value"
    }
    exp <- cc_t(out, lang = lang)
  }

  # Grab the region values
  times <- var_get_time(var)
  if (is.na(select_id)) {
    region_vals <- var_get_info(var[[1]], what = "region_values")[[1]]
    region_vals <- region_vals[region_vals$region == region, ]
    region_vals <- region_vals$val[region_vals$year %in% times]
  } else {
    # Grab the region values
    region_values <-
      lapply(var, \(x) explore_text_region_val_df(
        var = structure(x, class = class(var)),
        region = region,
        select_id = select_id,
        data = data,
        df = df,
        col = sprintf("%s_%s", sprintf("var_%s", left_right), which(x == var)),
        lang = lang
      ))
    region_vals <- sapply(region_values, `[[`, "val")
    # Newest value must be first, like for the no-selection values
    region_vals <- rev(region_vals)
  }

  region_vals_strings <- convert_unit(var, x = region_vals, decimal = 1)

  # Return
  return(list(
    exp = exp,
    region_vals = region_vals,
    region_vals_strings = region_vals_strings,
    times = times
  ))
}

#' Generate Text for the first paragraph of Delta
#'
#' This function generates text for the first paragraph of delta of a given variable,
#' comparing values at two different points in time.
#'
#' @param var <`character`> The variable code for which the text and values need
#' to be generated. Usually `vars$var_left`.
#' @param context <`list`> The output of \code{\link{explore_context}}
#' @param exp_vals <`list`> The output of \code{\link{explore_text_delta_exp}}
#' @param lang <`character`> Language specifying the language of the generated text.
#' Either `fr` or `en`. Defaults to NULL for no translation.
#' @param change_string <`list`> The output of \code{\link{explore_text_delta_change}}
#' @param ... Additional arguments passed to the methods.
#'
#' @return A character string containing the generated text.
#' @export
explore_text_delta_first_p <- function(var, context, exp_vals, lang = NULL, change_string, ...) {
  UseMethod("explore_text_delta_first_p", var)
}

#' @rdname explore_text_delta_first_p
#' @param select_id A string indicating the ID of the currently selected region
#' (if any). Usually `r[[id]]$select_id()`
#' @export
explore_text_delta_first_p.ind <- function(var, context, exp_vals, lang,
                                           change_string, select_id, ...) {
  if (is.na(select_id)) {
    return(explore_text_delta_first_p.default(var, context, exp_vals, lang, change_string))
  }

  # If there is a selectio
  # Craft the paragraphs
  out <- if (exp_vals$remained) {
    sprintf(
      cc_t("%s, %s has remained %s between %s and %s.", lang = lang),
      s_sentence(context$p_start), exp_vals$exp,
      unique(exp_vals$region_vals_strings),
      exp_vals$times[1], exp_vals$times[2]
    )
  } else {
    sprintf(
      cc_t("%s, %s changed from %s in %s to %s in %s.", lang = lang),
      s_sentence(context$p_start), exp_vals$exp,
      exp_vals$region_vals_strings[2], exp_vals$times[1],
      exp_vals$region_vals_strings[1], exp_vals$times[2]
    )
  }

  # Return
  return(out)
}

#' @rdname explore_text_delta_first_p
#' @export
explore_text_delta_first_p.default <- function(var, context, exp_vals, lang, change_string, ...) {
  # Did it increase or decrease? put in color
  inc_dec <- if (change_string$pct_change > 0) {
    cc_t("increased", lang = lang) |>
      explore_text_color(meaning = "increase")
  } else {
    cc_t("decreased", lang = lang) |>
      explore_text_color(meaning = "decrease")
  }

  # Separate the explanation of the second part to switch it if there are
  # bullet points
  s_explanation <- s_sentence(exp_vals$exp)
  if (grepl("</ul>$", s_explanation)) {
    s_explanation <- cc_t("This number", lang = lang)
  }

  # Craft the paragraphs
  first_part <- sprintf(
    cc_t("%s, %s changed from %s in %s to %s in %s.", lang = lang),
    s_sentence(context$p_start), exp_vals$exp,
    exp_vals$region_vals_strings[2], exp_vals$times[1],
    exp_vals$region_vals_strings[1], exp_vals$times[2]
  )
  second_part <- sprintf(
    cc_t("%s has %s by %s between these years.", lang = lang),
    s_explanation, inc_dec, change_string$text
  )

  # Bind
  out <- sprintf("<p>%s<p>%s", first_part, second_part)

  # Return
  return(out)
}

#' Explore Text Delta Change
#'
#' This function calculates and formats the text for delta change in variables,
#' either in percentage or dollar units, and returns a formatted string.
#'
#' @param var <`character`> The variable code for which the text and values need
#' to be generated. Usually `vars$var_left`.
#' @param exp_vals <`list`> A list of values, the output of
#' \code{\link{explore_text_delta_exp}}
#' @param ... Additional arguments passed to the method functions.
#'
#' @return A character string with the formatted delta change text.
#' @export
explore_text_delta_change <- function(var, exp_vals, ...) {
  UseMethod("explore_text_delta_change", var)
}

#' @rdname explore_text_delta_change
#' @param lang <`character`> Active language. Defaults to NULL for no translation
#' @export
explore_text_delta_change.pct <- function(var, exp_vals, lang = NULL, ...) {
  # Calculate the absolute and variation changes
  abs_change <- abs(exp_vals$region_vals[1] - exp_vals$region_vals[2]) * 100
  pct_change <- (exp_vals$region_vals[1] - exp_vals$region_vals[2]) / exp_vals$region_vals[2]

  # Get the percentage change as percentage points
  abs_change_string <- convert_unit(x = abs_change)

  # Increased/decreased by z x.
  this_x <- convert_unit(x = pct_change + 1)

  out <- sprintf(cc_t("%s percentage points (%sx)", lang = lang), abs_change_string, this_x)

  return(list(
    pct_change = pct_change,
    text = out
  ))
}

#' @rdname explore_text_delta_change
#' @export
explore_text_delta_change.dollar <- function(var, exp_vals, ...) {
  # Calculate the absolute and variation changes
  abs_change <- abs(exp_vals$region_vals[1] - exp_vals$region_vals[2])
  pct_change <- (exp_vals$region_vals[1] - exp_vals$region_vals[2]) / exp_vals$region_vals[2]

  # Get the absolute change as dollar
  abs_change_string <- convert_unit.dollar(x = abs_change)

  # Get the percentage change as percentage points
  pct_change_string <- convert_unit.pct(x = pct_change, decimal = 1)

  out <- sprintf("%s (%s)", abs_change_string, pct_change_string)

  return(list(
    pct_change = pct_change,
    text = out
  ))
}

#' @rdname explore_text_delta_change
#' @export
explore_text_delta_change.count <- function(var, exp_vals, ...) {
  # Calculate the absolute and variation changes
  abs_change <- abs(exp_vals$region_vals[1] - exp_vals$region_vals[2])
  pct_change <- (exp_vals$region_vals[1] - exp_vals$region_vals[2]) / exp_vals$region_vals[2]

  # Get the percentage change as percentage points
  abs_change_string <- convert_unit(x = abs_change)

  # Pretty pct change
  pretty_pct_change <- convert_unit.pct(x = pct_change, decimal = 1)

  out <- sprintf("%s (%s)", abs_change_string, pretty_pct_change)

  return(list(
    pct_change = pct_change,
    text = out
  ))
}

#' @rdname explore_text_delta_change
#' @param lang <`character`> Active language. Defaults to NULL for no translation
#' @export
explore_text_delta_change.ind <- function(var, exp_vals, lang, ...) {
  explore_text_delta_change.pct(var, exp_vals, lang = lang)
}


# DELTA BIVAR -------------------------------------------------------------

#' @rdname explore_text
#' @export
explore_text.delta_bivar <- function(vars, region, select_id, df, data,
                                     scales_as_DA = c("building", "street"),
                                     lang = NULL, ...) {
  # Detect if we should switch the scale for DAs in the case the `df` is part
  # of the `scales_as_DA` argument.
  switch_DA <- is_scale_df(scales_as_DA, df)

  # Adjust the selected ID in the case where the selection is not in `data`
  if (!switch_DA && !select_id %in% data$ID) select_id <- NA

  # Grab the shared info
  context <- explore_context(
    region = region, select_id = select_id, df = df,
    switch_DA = switch_DA, lang = lang
  )

  # The context might have used a scale in the `scales_as_DA` argument, and
  # the select_id needs to be switched to that of the dissemination area.
  if ("select_id" %in% names(context)) select_id <- context$select_id

  # Check for NAs in the selected value. Return NA message if it is the case
  na_check <- explore_text_check_na(
    context = context, data = data,
    select_id = select_id, vars = vars,
    lang = lang
  )
  if (!is.null(na_check)) {
    return(na_check)
  }

  # Grab the explanation and region values for both set of variables
  exp_vals_left <- explore_text_delta_exp(
    var = vars$var_left, region = region,
    select_id = select_id, data = data,
    df = context$treated_df, left_right = "left", lang = lang
  )
  exp_vals_right <- explore_text_delta_exp(
    var = vars$var_right, region = region,
    select_id = select_id, data = data,
    df = context$treated_df, left_right = "right", lang = lang
  )

  # If there is a selection, return a completely different text
  if (!is.na(select_id)) {
    # Start with the header
    out <- sprintf("<p><b>%s</b>", context$heading)

    # Left values
    exp_vals_left$exp <- explore_text_color(exp_vals_left$exp, meaning = "left")
    out <- sprintf(
      cc_t("%s<p>%s, %s changed from %s in %s to %s in %s.", lang = lang),
      out, s_sentence(context$p_start), exp_vals_left$exp,
      exp_vals_left$region_vals_strings[2], exp_vals_left$times[1],
      exp_vals_left$region_vals_strings[1], exp_vals_left$times[2]
    )

    # Right value
    right_sentenced <-
      explore_text_color(s_sentence(exp_vals_right$exp), meaning = "right")
    out <- sprintf(
      cc_t("%s %s changed from %s in %s to %s in %s.", lang = lang),
      out, right_sentenced,
      exp_vals_right$region_vals_strings[2], exp_vals_right$times[1],
      exp_vals_right$region_vals_strings[1], exp_vals_right$times[2]
    )
    exp_vals_right$exp <- explore_text_color(exp_vals_right$exp, meaning = "right")

    # Get the information on how the selection compares
    relat_left <- explore_text_selection_comparison(
      data = data,
      select_id = select_id,
      col = "var_left",
      ranks_override = c(
        "an exceptionally small change", "an unusually small change",
        "a just about average change", "an unusually large change",
        "an exceptionally large change"
      ),
      lang = lang
    )
    # Get the information on how the selection compares
    relat_right <- explore_text_selection_comparison(
      data = data,
      select_id = select_id,
      col = "var_right",
      ranks_override = c(
        "an exceptionally small change", "an unusually small change",
        "a just about average change", "an unusually large change",
        "an exceptionally large change"
      ),
      lang = lang
    )

    # Is the rank similar or different
    percs_distance <- abs(relat_left$higher_than_num - relat_right$higher_than_num)
    connector <- if (percs_distance > 0.2) "By contrast" else "Similarly"
    connector <- cc_t(connector, lang = lang)

    # Craft the left side of the second paragraph
    first_s <-
      sprintf(
        cc_t("The change in %s %s from %s to %s is larger than %s ",
          "other %s, which is %s for %s.",
          lang = lang
        ),
        exp_vals_left$exp, context$name, exp_vals_left$times[1],
        exp_vals_left$times[2], relat_left$higher_than, context$scale_plur,
        relat_left$rank_chr, context$to_compare_determ
      )
    second_s <-
      sprintf(
        cc_t("%s, the change in %s between the same years is larger ",
          "than %s of other %s, which is %s for %s.",
          lang = lang
        ),
        connector, exp_vals_right$exp, relat_right$higher_than,
        context$scale_plur, relat_right$rank_chr, context$to_compare_determ
      )

    # Bind it all
    out <- sprintf("%s<p>%s %s", out, first_s, second_s)

    # Add ellipsis if there are bullet points
    out <- gsub("</ul></span> ", "</ul></span><p>...", out)

    # Return
    return(out)
  }

  # Add colors to the explanations
  exp_vals_left$exp <- explore_text_color(exp_vals_left$exp, meaning = "left")
  exp_vals_right$exp <- explore_text_color(exp_vals_right$exp, meaning = "right")

  # Grab the scale definition
  scales_dictionary <- get_from_globalenv("scales_dictionary")
  scale_vec <- is_scale_df(scales_dictionary$scale, df, vectorized = TRUE)
  scale_plur <- cc_t(scales_dictionary$plur[scale_vec], lang = lang)

  # Correlation
  relation <- explore_text_bivar_correlation(vars, data, lang = lang)

  # If there is no correlation, the text is slightly different
  if (relation$no_correlation) {
    out <- sprintf(
      cc_t("%s from %s to %s, there is %s (%s) between the change in %s and ",
        "the change in %s in %s.",
        lang = lang
      ),
      s_sentence(context$p_start), exp_vals_left$times[1],
      exp_vals_left$times[2], relation$relation_text, relation$corr,
      exp_vals_left$exp, exp_vals_right$exp, scale_plur
    )

    # Add ellipsis if there are bullet points
    out <- gsub("</ul></span> ", "</ul></span><p>...", out)

    # Return
    return(out)
  }

  # Adjectives
  left_adj <- explore_text_delta_bivar_adjective(
    var = vars$var_left,
    left = TRUE,
    lang = lang
  )
  right_adj <- explore_text_delta_bivar_adjective(
    var = vars$var_right,
    left = FALSE,
    positive = relation$positive,
    lang = lang
  )

  # Paragraphs
  first_p <-
    if (grepl("_X_", relation$relation_text)) {
      # When the relationship is 'weak', it's a two-part relation_text
      relation_right_ajd_exp <-
        gsub(
          "_X_", sprintf(cc_t("%s in %s", lang = lang), right_adj, exp_vals_right$exp),
          relation$relation_text
        )

      sprintf(
        cc_t("%s from %s to %s, %s with %s in %s %s.", lang = lang),
        s_sentence(context$p_start), exp_vals_left$times[1],
        exp_vals_left$times[2], scale_plur, left_adj, exp_vals_left$exp,
        relation_right_ajd_exp
      )
    } else {
      sprintf(
        cc_t("%s from %s to %s, %s with %s in %s ",
          "%s have had %s in %s.",
          lang = lang
        ),
        s_sentence(context$p_start), exp_vals_left$times[1],
        exp_vals_left$times[2], scale_plur, left_adj, exp_vals_left$exp,
        relation$relation_text, right_adj, exp_vals_right$exp
      )
    }
  second_p <- sprintf(
    cc_t("There is a %s (%s) between the change in these two variables ",
      "between these years.",
      lang = lang
    ),
    relation$corr_strength, relation$corr
  )

  # Bind the two paragraphs
  out <- sprintf("<p>%s<p>%s", first_p, second_p)

  # Write STRONG CORRELATION if it is the case
  if (relation$strong) {
    out <- sprintf(cc_t("<p><b>STRONG CORRELATION</b>%s", lang = lang), out)
  }

  # Add ellipsis if there are bullet points
  out <- gsub("</ul></span> ", "</ul></span><p>...", out)

  # Return
  return(out)
}

#' Helper function for generating adjective to describe delta/bivariate
#' relationship between text variables
#'
#' This function generates a text string containing an adjective to describe the
#' delta/bivariate relationship between two variables based on whether the relationship
#' is positive or negative.
#'
#' @param var <`character`> The variable code for which the text needs to be
#' generated. `vars$var_left` or `vars$var_right`
#' @param left <`logical>` Whether the `var` supplied is the var_left
#' or the `var_right`. If `var_left`, TRUE.
#' @param positive <`logical`> Wheter the bivariate relationship is positive
#' or negative. One of the output of \code{\link{explore_text_bivar_correlation}}
#' @param lang <`character`> A string indicating the language in which to
#' translates the variable. Defaults to NULL.
#' @param ... Additional arguments to be passed.
#'
#' @return A text string containing an adjective to describe the bivariate
#' relationship.
#' @export
explore_text_delta_bivar_adjective <- function(var, left, positive, lang = NULL, ...) {
  UseMethod("explore_text_delta_bivar_adjective", var)
}


# explore_text_delta_bivar_adjective.dollar <- function(var, left, positive, lang,
#                                                       ...) {
#   string <- (\(x) {
#     if (left) {
#       return(cc_t("larger change", lang = lang))
#     }
#     if (positive) {
#       return(cc_t("larger change", lang = lang))
#     }
#     return(cc_t("smaller change", lang = lang))
#   })()
#
#   return(sprintf("<b>%s</b>", string))
# }

#' @rdname explore_text_delta_bivar_adjective
#' @export
explore_text_delta_bivar_adjective.default <- function(var, left, positive, lang,
                                                       ...) {
  string <- (\(x) {
    if (left) {
      return(cc_t("a larger change", lang = lang))
    }
    if (positive) {
      return(cc_t("a larger change", lang = lang))
    }
    return(cc_t("a smaller change", lang = lang))
  })()

  return(sprintf("<b>%s</b>", string))
}
