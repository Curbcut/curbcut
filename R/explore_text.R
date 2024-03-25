#' Generate Explore text for the given variables and region
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
#' @param scale <`reactive character`> Current scale. The output of
#' \code{\link{update_scale}}.
#' @param data <`data.frame`> A data frame containing the variables and
#' observations. The output of \code{\link{data_get}}.
#' @param schemas <`named list`> Current schema information. The additional widget
#' values that have an impact on which data column to pick. Usually `r[[id]]$schema()`.
#' @param scales_as_DA <`character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By
#' default, their info will be the one of their DA.
#' @param lang <`character`> A string indicating the language in which to
#' translates the variable. Defaults to NULL. Usually is `r$lang()`.
#' @param zoom_levels <`named numeric vector`> A named numeric vector of zoom
#' levels. Usually one of the `mzl_*`, or the output of
#' \code{\link{geography_server}}.
#' @param time <`numeric named list`> The `time` at which data is displayed.
#' A list for var_left and var_right. The output of \code{\link{vars_build}}(...)$time.
#' @param ... Additional arguments passed to the dispatched function.
#'
#' @return The resulting text.
#' @export
explore_text <- function(vars, region, select_id, scale, time, data,
                         schemas, zoom_levels,
                         scales_as_DA = c("building", "street"),
                         lang = NULL, ...) {
  UseMethod("explore_text", vars)
}


# Q5 ----------------------------------------------------------------------

#' @rdname explore_text
#' @param shown_scale <`character`> While the `scale` argument is the scale
#' for which to calculate regional values, `shown_scale` is the scale which
#' would fit the `select_id`. In use for raster data, where we show region values
#' for the highest resolution possible, but we still want to allow user to select
#' grid cells of lower resolutions. `shown_scale` will only be used to grab the
#' address of the grid cell. Defaults to NULL for normal operations.
#' @param val <`numeric`> If the value is not part of `data`. It happens on raster
#' data where we show region values for the highest resolution possible, but we still
#' want to allow user to select grid cells of lower resolutions. Defaults to NULL
#' for normal operations.
#' @export
explore_text.q5 <- function(vars, region, select_id, scale, time, data,
                            schemas, zoom_levels,
                            scales_as_DA = c("building", "street"),
                            lang = NULL, shown_scale = NULL, val = NULL, ...) {
  # Detect if we should switch the scale for DAs in the case the `scale` is part
  # of the `scales_as_DA` argument.
  switch_DA <- is_scale_in(scales_as_DA, scale)

  # Adjust the selected ID in the case where the selection is not in `data`,
  # except if there is a value supplied, meaning
  if (is.null(shown_scale) & is.null(val)) {
    if (!switch_DA && !select_id %in% data$ID) select_id <- NA
  }

  # Grab the shared info
  context <- explore_context(
    region = region, select_id = select_id, scale = scale, shown_scale = shown_scale,
    zoom_levels = zoom_levels, switch_DA = switch_DA, lang = lang
  )

  # The context might have used a scale in the `scales_as_DA` argument, and
  # the select_id needs to be switched to that of the dissemination area.
  if ("select_id" %in% names(context)) select_id <- context$select_id

  # Check for NAs in the selected value. Return NA message if it is the case
  na_check <- explore_text_check_na(
    context = context, data = data,
    select_id = select_id, vars = vars,
    time = time, lang = lang, schemas = schemas,
    val = val
  )
  if (!is.null(na_check)) {
    return(na_check)
  }

  # Grab the value string
  value_string <- explore_text_values_q5(
    var = vars$var_left, region = region,
    select_id = select_id, data = data,
    scale = context$treated_scale, lang = lang,
    time = time, schemas = schemas, val = val
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
      select_id = select_id, lang = lang,
      time_col = time$var_left, schemas = schemas,
      val = val
    )

    # Make the first sentence of the paragraph
    first_step <- sprintf(
      cc_t("This is %s for %s", lang = lang), relat$rank_chr,
      context$to_compare_determ
    )

    # Grab the explanation and capitalize the first letter
    exp <- var_get_info(vars$var_left,
      what = "explanation", translate = TRUE,
      lang = lang, schemas_col = schemas$var_left
    ) |>
      s_sentence()

    if (grepl("</ul>", exp)) {
      exp <- cc_t("This value", lang = lang)
    }

    # Plug the right elements for the final sentence
    second_step <- sprintf(
      curbcut::cc_t("%s %s is %s than in %s of other %s %s", lang = lang), exp,
      context$p_start, relat$higher_lower, relat$higher_lower_than, context$scale_plur,
      context$to_compare_short
    )

    # Bind it all
    out <- sprintf("%s<p>%s. %s.", out, first_step, second_step)
  }

  # Append date
  date <- time$var_left
  if (!is.na(date)) {
    out <- sprintf(cc_t("%s <i>(Data from %s.)</i>", lang = lang), out, date)
  }

  # Return the text
  return(out)
}


# BIVAR -------------------------------------------------------------------

#' @rdname explore_text
#' @export
explore_text.bivar <- function(vars, region, select_id, scale, time, data,
                               schemas, zoom_levels,
                               scales_as_DA = c("building", "street"),
                               lang = NULL, ...) {
  # Append date function helper
  append_date <- \(out) {
    date_1 <- time$var_left
    date_2 <- time$var_right
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

  # Detect if we should switch the scale for DAs in the case the `scale` is part
  # of the `scales_as_DA` argument.
  switch_DA <- is_scale_in(scales_as_DA, scale)

  # Adjust the selected ID in the case where the selection is not in `data`
  if (!switch_DA && !select_id %in% data$ID) select_id <- NA

  # Grab the shared info
  context <- explore_context(
    region = region, select_id = select_id, scale = scale,
    zoom_levels = zoom_levels, switch_DA = switch_DA, lang = lang
  )

  # The context might have used a scale in the `scales_as_DA` argument, and
  # the select_id needs to be switched to that of the dissemination area.
  if ("select_id" %in% names(context)) select_id <- context$select_id

  # Check for NAs in the selected value. Return NA message if it is the case
  na_check <- explore_text_check_na(
    context = context, data = data,
    select_id = select_id, vars = vars,
    time = time, lang = lang, schemas = schemas
  )
  if (!is.null(na_check)) {
    return(na_check)
  }

  # If there is a selection, return a completely diferent text
  if (!is.na(select_id)) {
    # Grab the value string
    value_string_left <- explore_text_values_q5(
      var = vars$var_left, region = region,
      select_id = select_id, data = data, time = time,
      scale = context$treated_scale, lang = lang,
      col = "var_left", schemas = schemas
    )

    # Grab the value string
    value_string_right <- explore_text_values_q5(
      var = vars$var_right, region = region,
      select_id = select_id, data = data,
      scale = context$treated_scale, time = time,
      col = "var_right", lang = lang, schemas = schemas
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
        lang = lang,
        time_col = time[[col]],
        schemas = schemas
      )

      # Grab the explanation
      exp <- var_get_info(var,
        what = "explanation", translate = TRUE, lang = lang,
        schemas_col = schemas[[col]]
      )
      if (col == "var_right") {
        exp <- explore_text_bivar_right_var_default_schema(exp, data)
      }

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
        cc_t("%s is %s than in %s of other %s", lang = lang), first_step_1,
        relat$higher_lower, relat$higher_lower_than, context$scale_plur
      )

      # Make the second step of the sentence
      second_step <- sprintf(
        cc_t("which is %s for %s", lang = lang), relat$rank_chr,
        context$to_compare_determ
      )

      return(list(
        higher_lower = relat$higher_lower,
        higher_lower_than = relat$higher_lower_than_num,
        text = sprintf("%s, %s", first_step, second_step)
      ))
    })

    # Is the rank similar or different
    percs <- sapply(compare_texts, `[[`, "higher_lower_than")
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
  scale_vec <- is_scale_in(scales_dictionary$scale, scale, vectorized = TRUE)
  scale_plur <- cc_t(scales_dictionary$plur[scale_vec], lang = lang)

  # Correlation
  relation <- explore_text_bivar_correlation(
    vars = vars, data = data, time = time, lang = lang, schemas = schemas
  )

  # If there is no correlation, the text is slightly different
  if (relation$no_correlation) {
    # Explanations
    left_exp <- var_get_info(vars$var_left,
      what = "explanation",
      translate = TRUE, lang = lang, schemas_col = schemas$var_left
    ) |>
      explore_text_color(meaning = "left")

    right_exp <- var_get_info(vars$var_right,
      what = "explanation",
      translate = TRUE, lang = lang, schemas_col = schemas$var_right
    ) |>
      explore_text_color(meaning = "right") |>
      explore_text_bivar_right_var_default_schema(data)

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
    translate = TRUE, lang = lang, schemas_col = schemas$var_left
  ) |>
    explore_text_color(meaning = "left")
  right_exp <- var_get_info(vars$var_right,
    what = "explanation_nodet",
    translate = TRUE, lang = lang, schemas_col = schemas$var_right
  ) |>
    explore_text_color(meaning = "right") |>
    explore_text_bivar_right_var_default_schema(data)

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


# DELTA -------------------------------------------------------------------

#' @rdname explore_text
#' @export
explore_text.delta <- function(vars, region, select_id, scale, time, data,
                               schemas, zoom_levels,
                               scales_as_DA = c("building", "street"),
                               lang = NULL, shown_scale = NULL, val = NULL, ...) {
  # Detect if we should switch the scale for DAs in the case the `scale` is part
  # of the `scales_as_DA` argument.
  switch_DA <- is_scale_in(scales_as_DA, scale)

  # Adjust the selected ID in the case where the selection is not in `data`,
  # except if there is a value supplied, meaning
  if (is.null(shown_scale) & is.null(val)) {
    if (!switch_DA && !select_id %in% data$ID) select_id <- NA
  }

  # Grab the shared info
  context <- explore_context(
    region = region, select_id = select_id, scale = scale, shown_scale = shown_scale,
    zoom_levels = zoom_levels, switch_DA = switch_DA, lang = lang
  )

  # The context might have used a scale in the `scales_as_DA` argument, and
  # the select_id needs to be switched to that of the dissemination area.
  if ("select_id" %in% names(context)) select_id <- context$select_id

  # Check for NAs in the selected value. Return NA message if it is the case
  na_check <- explore_text_check_na(
    context = context, data = data,
    select_id = select_id, vars = vars,
    lang = lang, time = time, schemas = schemas,
    val = val
  )
  if (!is.null(na_check)) {
    return(na_check)
  }

  # Grab the explanation and region values
  exp_vals <- explore_text_delta_exp(
    var = vars$var_left, region = region,
    select_id = select_id, data = data,
    scale = context$treated_scale,
    left_right = "left", lang = lang,
    time = time, schemas = schemas, val = val,
    single_val = TRUE
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
    lang = lang, schemas_col = schemas$var_left
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
    lang = lang,
    time_col = time$var_left,
    schemas = schemas,
    larger = TRUE,
    val = val
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
        "The change in %s %s between %s and %s is %s than in %s of ",
        "other %s between the same years."
      ), lang = lang),
      exp_nodet, context$p_start, exp_vals$times[1], exp_vals$times[2],
      relat$higher_lower, relat$higher_lower_than, context$scale_plur
    )

  out <- sprintf("%s<p>%s %s", out, first_part, second_part)

  # Return
  # Add ellipsis if there are bullet points
  out <- gsub("</ul> ", "</ul><p>...", out)
  return(out)
}


# DELTA BIVAR -------------------------------------------------------------

#' @rdname explore_text
#' @export
explore_text.delta_bivar <- function(vars, region, select_id, scale, time, data,
                                     schemas, zoom_levels,
                                     scales_as_DA = c("building", "street"),
                                     lang = NULL, ...) {
  # Detect if we should switch the scale for DAs in the case the `scale` is part
  # of the `scales_as_DA` argument.
  switch_DA <- is_scale_in(scales_as_DA, scale)

  # Adjust the selected ID in the case where the selection is not in `data`
  if (!switch_DA && !select_id %in% data$ID) select_id <- NA

  # Grab the shared info
  context <- explore_context(
    region = region, select_id = select_id, scale = scale,
    zoom_levels = zoom_levels, switch_DA = switch_DA, lang = lang
  )

  # The context might have used a scale in the `scales_as_DA` argument, and
  # the select_id needs to be switched to that of the dissemination area.
  if ("select_id" %in% names(context)) select_id <- context$select_id

  # Check for NAs in the selected value. Return NA message if it is the case
  na_check <- explore_text_check_na(
    context = context, data = data,
    select_id = select_id, vars = vars,
    lang = lang, time = time, schemas = schemas
  )
  if (!is.null(na_check)) {
    return(na_check)
  }

  # Grab the explanation and region values for both set of variables
  exp_vals_left <- explore_text_delta_exp(
    var = vars$var_left, region = region,
    select_id = select_id, data = data,
    scale = context$treated_scale, left_right = "left",
    lang = lang, time = time, schemas = schemas
  )
  exp_vals_right <- explore_text_delta_exp(
    var = vars$var_right, region = region,
    select_id = select_id, data = data,
    scale = context$treated_scale, left_right = "right",
    lang = lang, time = time, schemas = schemas
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
      time_col = time$var_left,
      lang = lang,
      schemas = schemas,
      larger = TRUE
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
      time_col = time$var_right,
      lang = lang,
      schemas = schemas,
      larger = TRUE
    )

    # Is the rank similar or different
    percs_distance <- abs(relat_left$higher_lower_than_num - relat_right$higher_lower_than_num)
    connector <- if (percs_distance > 0.2) "By contrast" else "Similarly"
    connector <- cc_t(connector, lang = lang)

    # Craft the left side of the second paragraph
    first_s <-
      sprintf(
        cc_t("The change in %s %s from %s to %s is %s than %s ",
          "other %s, which is %s for %s.",
          lang = lang
        ),
        exp_vals_left$exp, context$name, exp_vals_left$times[1],
        exp_vals_left$times[2], relat_left$higher_lower, relat_left$higher_lower_than,
        context$scale_plur,
        relat_left$rank_chr, context$to_compare_determ
      )
    second_s <-
      sprintf(
        cc_t("%s, the change in %s between the same years is %s ",
          "than %s of other %s, which is %s for %s.",
          lang = lang
        ),
        connector, exp_vals_right$exp, relat_right$higher_lower, relat_right$higher_lower_than,
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
  scale_vec <- is_scale_in(scales_dictionary$scale, scale, vectorized = TRUE)
  scale_plur <- cc_t(scales_dictionary$plur[scale_vec], lang = lang)

  # Correlation
  relation <- explore_text_bivar_correlation(
    vars = vars, data = data, time = time, lang = lang
  )

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
explore_text_delta_bivar_adjective.default <- function(var, left, positive, lang = NULL,
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
