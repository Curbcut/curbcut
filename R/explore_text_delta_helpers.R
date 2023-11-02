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
#' @param time <`numeric named list`> The `time` at which data is displayed.
#' A list for var_left and var_right. The output of \code{\link{vars_build}}(...)$time.
#' @param scale <`character`> The scale under study. The output of
#' \code{\link{update_scale}}.
#' @param data <`data.frame`> A data frame containing the variables and
#' observations. The data frame must have columns named var_left
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
                                   time, scale, data, ...) {
  UseMethod("explore_text_delta_exp", var)
}

#' @rdname explore_text_delta_exp
#' @param lang <`character`> Language for translation.
#' @export
explore_text_delta_exp.ind <- function(var, region, select_id, left_right = "left",
                                       time, scale, data, lang, ...) {

  # Grab values for single years. Allow for `apply`
  var_lr <- sprintf("var_%s", left_right)
  times <- sapply(time[[var_lr]], \(x) setNames(list(x), var_lr),
                  simplify = FALSE, USE.NAMES = TRUE)
  names(times) <- time[[var_lr]]

  # If there is no selection
  if (is.na(select_id)) {
    # Grab the parent variable
    parent <- var_get_info(var = var[[1]], what = "parent_vec")
    parent <- cc_t(parent, lang = lang)

    # Grab the explanation
    exp_q5 <- var_get_info(
      var = var, what = "exp_q5", translate = TRUE,
      lang = lang
    )

    # Sub the placeholder for the two last brackets
    breaks <- var_get_info(var = var, what = "rank_name")[[1]]
    two_last_ranks <- breaks[4:5]
    two_last_ranks <- sapply(two_last_ranks, cc_t, lang = lang)
    two_last_ranks <- tolower(two_last_ranks)
    exp <- {
      gsub("_X_", sprintf(
        cc_t("'%s' to '%s'", lang = lang), two_last_ranks[[1]],
        two_last_ranks[[2]]
      ), exp_q5)
    }

    # Grab the region values
    region_vals <-
      lapply(times, \(t) {
        explore_text_region_val_df(
          var = var,
          region = region,
          select_id = NA,
          data = data,
          scale = scale,
          col = var_lr,
          lang = lang,
          time = t
        )
      })
    region_vals <- sapply(region_vals, `[[`, "val")
    region_vals <- rev(region_vals)
    region_vals_strings <- convert_unit.pct(var,
                                            x = region_vals,
                                            decimal = 1
    )

    # Return
    return(list(
      exp = sprintf(cc_t("the percentage of %s that %s", lang = lang), parent, exp),
      region_vals = region_vals,
      region_vals_strings = region_vals_strings,
      times = unname(unlist(times))
    ))
  }

  # If there is a selection
  exp <- var_get_info(
    var = var, what = "explanation", translate = TRUE,
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

  # Grab both value strings
  rank_chr_before <- explore_text_selection_comparison(
    var = var,
    data = data,
    select_id = select_id,
    col = var_lr,
    lang = lang,
    time_col = times[[1]]
  )$rank_chr

  rank_chr_after <- explore_text_selection_comparison(
    var = var,
    data = data,
    select_id = select_id,
    col = var_lr,
    lang = lang,
    time_col = times[[2]]
  )$rank_chr

  # Did it remain in the same category, or it moved?
  remained <- rank_chr_before == rank_chr_after

  # Grab the region values
  region_vals <-
    lapply(times, \(t) {
      explore_text_region_val_df(
        var = var,
        region = region,
        select_id = select_id,
        data = data,
        scale = scale,
        col = var_lr,
        lang = lang,
        time = t
      )
    })
  region_vals <- sapply(region_vals, `[[`, "num")
  # Newest value must be first, like for the no-selection values
  region_vals <- rev(region_vals)

  # Return
  return(list(
    exp = exp,
    region_vals = region_vals,
    region_vals_strings = c(rank_chr_before, rank_chr_after),
    remained = remained,
    times = unname(unlist(times))
  ))
}

#' @rdname explore_text_delta_exp
#' @export
explore_text_delta_exp.default <- function(var, region, select_id,
                                           left_right = "left", scale, data,
                                           time, lang, ...) {
  # Grab the explanation
  exp <- var_get_info(var, what = "explanation", translate = TRUE, lang = lang)
  if (grepl("</ul>", exp)) {
    out <- if (left_right == "left") {
      "the first value"
    } else {
      "the second value"
    }
    exp <- cc_t(out, lang = lang)
  }

  # Grab values for single years. Allow for `apply`
  var_lr <- sprintf("var_%s", left_right)
  times <- sapply(time[[var_lr]], \(x) setNames(list(x), var_lr),
                  simplify = FALSE, USE.NAMES = TRUE)
  names(times) <- time[[var_lr]]

  # Grab the region values
  region_values <-
    lapply(times, \(t) {
      explore_text_region_val_df(
        var = var,
        region = region,
        select_id = select_id,
        data = data,
        scale = scale,
        col = var_lr,
        lang = lang,
        time = t
      )
    })
  region_vals <- sapply(region_values, `[[`, "val")
  # Newest value must be first, like for the no-selection values
  region_vals <- rev(region_vals)

  region_vals_strings <- convert_unit(var, x = region_vals, decimal = 1, precise_round = FALSE)

  # Return
  return(list(
    exp = exp,
    region_vals = region_vals,
    region_vals_strings = region_vals_strings,
    times = unname(unlist(times))
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
      exp_vals$region_vals_strings[1], exp_vals$times[1],
      exp_vals$region_vals_strings[2], exp_vals$times[2]
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
  pct_change <- if (all(exp_vals$region_vals == 0)) {
    0
  } else {
    (exp_vals$region_vals[1] - exp_vals$region_vals[2]) / exp_vals$region_vals[2]
  }

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
  pct_change <- if (all(exp_vals$region_vals == 0)) {
    0
  } else {
    (exp_vals$region_vals[1] - exp_vals$region_vals[2]) / exp_vals$region_vals[2]
  }

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
#' @param lang <`character`> Active language. Defaults to NULL for no translation
#' @export
explore_text_delta_change.ind <- function(var, exp_vals, lang, ...) {
  explore_text_delta_change.pct(var, exp_vals, lang = lang)
}

#' @rdname explore_text_delta_change
#' @export
explore_text_delta_change.default <- function(var, exp_vals, ...) {
  # Calculate the absolute and variation changes
  abs_change <- abs(exp_vals$region_vals[1] - exp_vals$region_vals[2])
  pct_change <- if (all(exp_vals$region_vals == 0)) {
    0
  } else {
    (exp_vals$region_vals[1] - exp_vals$region_vals[2]) / exp_vals$region_vals[2]
  }

  # Get the percentage change as percentage points
  abs_change_string <- convert_unit(var, x = abs_change)

  # Pretty pct change
  pretty_pct_change <- convert_unit.pct(x = pct_change, decimal = 1)

  out <- sprintf("%s (%s)", abs_change_string, pretty_pct_change)

  return(list(
    pct_change = pct_change,
    text = out
  ))
}
