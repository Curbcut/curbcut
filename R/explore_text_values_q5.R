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
#' @param data <`data.frame`> The output of \code{\link{data_get}}.
#' @param scale <`character`> Current scale. The output of
#' \code{\link{update_scale}}.
#' @param select_id <`character`> the current selected ID, usually
#' `r[[id]]$select_id()`.
#' @param col <`character`> Which column of `data` should be selected to grab the
#' value information. Defaults to `var_left`, but could also be `var_right` or
#' `var_left_1` in delta.
#' @param lang <`character`> A string indicating the language in which to
#' translates the variable. Defaults to NULL. Usually is `r$lang()`.
#' @param time <`numeric named list`> The `time` at which data is displayed.
#' A list for var_left and var_right. The output of \code{\link{vars_build}}(...)$time.
#' @param schemas <`named list`> Current schema information. The additional widget
#' values that have an impact on which data column to pick. Usually `r[[id]]$schema()`.
#' @param ... Additional arguments passed to the function..
#'
#' @return The resulting text.
#' @export
explore_text_values_q5 <- function(var, region, ...) {
  UseMethod("explore_text_values_q5", var)
}

#' @describeIn explore_text_values_q5 The method for percentage values.
#' @export
explore_text_values_q5.pct <- function(var, region, data, scale, select_id,
                                       col = "var_left", lang, time, schemas = NULL, ...) {
  # Grab the parent variable
  parent_string <- explore_text_parent_title(var, lang = lang)

  # Grab the q5 explanation
  exp <- var_get_info(
    var = var, what = "exp_q5", translate = TRUE,
    lang = lang, schemas_col = schemas[[col]]
  )

  # Grab the region values
  region_values <- explore_text_region_val_df(
    var = var,
    region = region,
    data = data,
    scale = scale,
    select_id = select_id,
    col = col,
    lang = lang,
    time = time,
    schemas = schemas
  )

  # NA message
  if (is.na(region_values$val)) {
    exp <- var_get_info(var = var, what = "explanation", translate = TRUE,
                        lang = lang, schemas_col = schemas[[col]])
    out <- sprintf(cc_t("we currently don't have information regarding %s", lang = lang), exp)
    return(list(
      text = out,
      na = TRUE
    ))
  }

  # Make the region values as characters
  pct_string <- convert_unit.pct(x = region_values$val, decimal = 1)
  count_string <- convert_unit(x = region_values$count, precise_round = FALSE)

  # Build the return
  out <- sprintf("%s %s (%s) %s", count_string, parent_string, pct_string, exp)

  # Return
  return(list(
    text = out,
    na = FALSE
  ))
}

#' @describeIn explore_text_values_q5 The method for count values.
#' @export
explore_text_values_q5.count <- function(var, region, data, scale, select_id,
                                         col = "var_left", lang, time, schemas = NULL, ...) {
  # Grab the parent variable
  parent_string <- explore_text_parent_title(var, lang = lang)

  # Grab the q5 explanation
  exp <- var_get_info(var = var, what = "exp_q5", translate = TRUE, lang = lang,
                      schemas_col = schemas[[col]])

  # Grab the region values
  region_values <- explore_text_region_val_df(
    var = var,
    region = region,
    scale = scale,
    data = data,
    select_id = select_id,
    col = col,
    lang = lang,
    time = time,
    schemas = schemas
  )

  # NA message
  if (is.na(region_values$val)) {
    exp <- var_get_info(
      var = var, what = "explanation", translate = TRUE,
      lang = lang, schemas_col = schemas[[col]]
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
  count_string <- convert_unit(x = region_values$val, decimal = 1, precise_round = FALSE)

  # Build the return
  out <- sprintf("%s %s %s", count_string, parent_string, exp)

  # Return
  return(list(
    text = out,
    na = FALSE
  ))
}

#' @describeIn explore_text_values_q5 The method for dollar values.
#' @export
explore_text_values_q5.dollar <- function(var, region, data, scale, select_id,
                                          col = "var_left", lang, time, schemas = NULL, ...) {
  # Grab the region values
  region_values <- explore_text_region_val_df(
    var = var,
    region = region,
    scale = scale,
    data = data,
    select_id = select_id,
    col = col,
    lang = lang,
    time = time,
    schemas = schemas
  )

  # NA message
  if (is.na(region_values$val)) {
    exp <- var_get_info(
      var = var, what = "explanation", translate = TRUE,
      lang = lang, schemas_col = schemas[[col]]
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
  exp <- var_get_info(var = var, what = "exp_q5", translate = TRUE, lang = lang,
                      schemas_col = schemas[[col]])

  # Build the return
  out <- sprintf("%s %s", exp, dollar_string)

  # Return
  return(list(
    text = out,
    na = FALSE
  ))
}

#' @describeIn explore_text_values_q5 The method for `ind` values.
#' @export
explore_text_values_q5.ind <- function(var, region, select_id, data, scale,
                                       col = "var_left", lang, time, schemas = NULL, ...) {
  # Grab the parent variable
  parent_string <- explore_text_parent_title(var, lang = lang)

  # Grab the region values
  region_values <- explore_text_region_val_df(
    var = var,
    region = region,
    scale = scale,
    select_id = select_id,
    data = data,
    col = col,
    lang = lang,
    time = time,
    schemas = schemas
  )

  # NA message
  if (is.na(region_values$val)) {
    exp <- var_get_info(
      var = var, what = "explanation", translate = TRUE,
      lang = lang, schemas_col = schemas[[col]]
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
    count_string <- convert_unit(x = region_values$count, precise_round = FALSE)

    # Grab the explanation
    exp_q5 <- var_get_info(
      var = var, what = "exp_q5", translate = TRUE,
      lang = lang, schemas_col = schemas[[col]]
    )

    # Sub the placeholder for the two last brackets
    breaks <- attr(data, "breaks")
    two_last_ranks <- var_get_info(var = var, what = "rank_name")[[1]][4:5]
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
    lang = lang, schemas_col = schemas[[col]]
  )

  # Build the return
  out <- sprintf(cc_t("%s is %s", lang = lang), exp, region_values$val)

  # Return
  return(list(
    text = out,
    na = FALSE
  ))
}

#' @describeIn explore_text_values_q5 The method for average values.
#' @export
explore_text_values_q5.avg <- function(var, region, select_id, data, scale,
                                       col = "var_left", lang, time, schemas = NULL, ...) {
  # Grab the parent variable
  parent_string <- explore_text_parent_title(var)

  # Grab the region values
  region_values <- explore_text_region_val_df(
    var = var,
    region = region,
    select_id = select_id,
    scale = scale,
    data = data,
    col = col,
    lang = lang,
    time = time,
    schemas = schemas
  )

  # NA message
  if (is.na(region_values$val)) {
    exp <- var_get_info(
      var = var, what = "explanation", translate = TRUE,
      lang = lang, schemas_col = schemas[[col]]
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
    count_string <- convert_unit(x = region_values$val, decimal = 1, precise_round = FALSE)

    # Grab the explanation
    exp_q5 <- var_get_info(
      var = var, what = "exp_q5", translate = TRUE,
      lang = lang, schemas_col = schemas[[col]]
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
  exp_q5 <- var_get_info(
    var = var, what = "exp_q5", translate = TRUE,
    lang = lang, schemas_col = schemas[[col]]
  )

  # Build the return
  count_string <- convert_unit(x = region_values$val, decimal = 1, precise_round = FALSE)
  out <- gsub("_X_", count_string, exp_q5)

  # Return
  return(list(
    text = out,
    na = FALSE
  ))
}

#' @describeIn explore_text_values_q5 The method for square kilometers values.
#' @export
explore_text_values_q5.sqkm <- function(var, region, select_id, data, scale,
                                        col = "var_left", lang, time, schemas = NULL, ...) {
  # Grab the region values
  region_values <- explore_text_region_val_df(
    var = var,
    region = region,
    scale = scale,
    data = data,
    select_id = select_id,
    col = col,
    lang = lang,
    time = time,
    schemas = schemas
  )

  # NA message
  if (is.na(region_values$val)) {
    exp <- var_get_info(
      var = var, what = "explanation", translate = TRUE,
      lang = lang, schemas_col = schemas[[col]]
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
  count_string <- convert_unit(x = region_values$val, decimal = 1, precise_round = FALSE)

  # Grab the explanation
  exp_q5 <- var_get_info(
    var = var, what = "exp_q5", translate = TRUE,
    lang = lang, schemas_col = schemas[[col]]
  )

  # If the two last brackets is recognized as the default, write a particular string
  out <- gsub("_X_", count_string, exp_q5)

  # Return
  return(list(
    text = out,
    na = FALSE
  ))
}

#' @describeIn explore_text_values_q5 The method for per1k values.
#' @export
explore_text_values_q5.per1k <- function(var, region, select_id, data, scale,
                                         col = "var_left", lang = lang, time, schemas = NULL, ...) {
  explore_text_values_q5.sqkm(
    var = var, region = region, scale = scale, select_id = select_id,
    data = data, col = col, lang = lang, time = time, ...
  )
}

#' @describeIn explore_text_values_q5 The method for people per object values.
#' @export
explore_text_values_q5.ppo <- function(var, region, select_id, data, scale,
                                       col = "var_left", lang, time, schemas = NULL, ...) {
  # Grab the region values
  region_values <- explore_text_region_val_df(
    var = var,
    region = region,
    data = data,
    select_id = select_id,
    col = col,
    lang = lang,
    time = time,
    schemas = schemas
  )

  # NA message
  if (is.na(region_values$val)) {
    exp <- var_get_info(
      var = var, what = "explanation", translate = TRUE,
      lang = lang, schemas_col = schemas[[col]]
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
  count_string <- convert_unit(x = region_values$val, decimal = 1, precise_round = FALSE)

  # Grab the explanation
  exp_q5 <- var_get_info(
    var = var, what = "exp_q5", translate = TRUE,
    lang = lang, schemas_col = schemas[[col]]
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
