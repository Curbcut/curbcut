#' Get region text info
#'
#' This function retrieves the region df from the global environment given the
#' region name and returns it in a list with the first letter of the region name
#' capitalized.
#'
#' @param region <`character`> String specifying the code of the region to
#' retrieve, e.g. `CMA`. Usually equivalent of `r$region()`.
#' @param select_id <`character`> the current selected ID, usually
#' `r[[id]]$select_id()`. If there is a selection (select_id is not NA), the
#' name of the selected polygon will appear.
#' @param scale <`reactive character`> Current scale. The output of
#' \code{\link{update_scale}}.
#' @param switch_DA <`logical`> Is the `df` part of the scales that should be
#' switched as DAs instead.
#' @param zoom_levels <`named numeric vector`> A named numeric vector of zoom
#' levels. Usually one of the `mzl_*`, or the output of
#' \code{\link{geography_server}}.
#' @param shown_scale <`character`> While the `scale` argument is the scale
#' for which to calculate regional values, `shown_scale` is the scale which
#' would fit the `select_id`. In use for raster data, where we show region values
#' for the highest resolution possible, but we still want to allow user to select
#' grid cells of lower resolutions. `shown_scale` will only be used to grab the
#' address of the grid cell. Defaults to NULL for normal operations.
#' @param lang <`character`> Active language. "en" or "fr". Defaults to NULL
#' for no translation.
#'
#' @return A list containing multiple texts used for the explore text panel.
#' @export
explore_context <- function(region, select_id, scale, switch_DA, zoom_levels,
                            shown_scale = NULL, lang = NULL) {
  # Grab the region dictionary
  regions_dictionary <- get_from_globalenv("regions_dictionary")
  region_df <- regions_dictionary[regions_dictionary$region == region, ]

  # Prepare a return when there is no selection
  region_only_return <- \(region_df) {
    # Grab the region text
    to_compare <- cc_t(region_df$to_compare, lang = lang)

    # Return as a sentence
    return(list(p_start = to_compare, treated_scale = scale, select_id = NA))
  }

  # If there is no selection, return the region text only
  if (is.na(select_id)) {
    return(region_only_return(region_df))
  }

  # Grab the right scale
  scale_grab_chr_for <- if (!is.null(shown_scale)) shown_scale else scale

  scales_dictionary <- get_from_globalenv("scales_dictionary")
  scale_df <- scales_dictionary[
    is_scale_in(scales_dictionary$scale, scale = scale_grab_chr_for, vectorized = TRUE),
  ]

  # Normal retrieval when the `df` is not part of the scales to treat as
  # DA.
  if (!switch_DA) {
    # Get the place heading and glue it
    dat <- grab_row_from_bslike(
      scale = scale_grab_chr_for, select_id = select_id,
      cols = c("name", "name_2")
    )
    # the selection is not found in the database
    if (nrow(dat) == 0) return(region_only_return(region_df))

    name <- dat$name
    name_2 <- dat$name_2
    if (is.na(name_2)) {
      name_2 <- fill_name_2(
        ID_scale = select_id, scale = scale_grab_chr_for,
        top_scale = names(zoom_levels)[[1]]
      )
    }
    name_2 <- cc_t(name_2, lang = lang)
    heading <- cc_t(scale_df$place_heading, lang = lang)
    treated_scale <- scale
  }

  # Tweaked retrieval when the `df` is part of the scales to treat as DAs.
  if (switch_DA) {
    # Grab the DA ID and the address from the SQL database
    bs <- grab_row_from_bslike(
      scale = scale, select_id = select_id,
      cols = c("name", "DA_ID")
    )

    # If the selection ID is not in the SQL database, return the region only text
    # with an empty NA. The text that will be showed is the basic one for the region.
    if (nrow(bs) == 0) {
      out <- region_only_return(region_df)
      out$select_id <- NA
      return(out)
    }

    # Get the heading
    name <- sprintf(cc_t("around %s", lang = lang), bs$name)
    heading <- sprintf(
      cc_t("Dissemination area %s", lang = lang),
      cc_t(scale_df$place_heading, lang = lang)
    )

    # Switch the select_id, to be able to use the data values of the `DA`
    select_id <- bs$DA_ID

    # Switch df to DA
    treated_scale <- "DA"

    # Switch the scale
    scale_df <- scales_dictionary[is_scale_in(scales_dictionary$scale,
      treated_scale,
      vectorized = TRUE
    ), ]
  }

  # Get the sentence start (In Borough or In dissemination area XYZ, )
  p_start <- cc_t(scale_df$place_name, lang = lang)

  # Return
  return(list(
    heading = heading,
    p_start = cc_t("in {p_start}", lang = lang),
    name = cc_t("in {name}", lang = lang),
    to_compare_determ = cc_t(region_df$to_compare_determ, lang = lang),
    to_compare_short = cc_t(region_df$to_compare_short, lang = lang),
    scale_plur = cc_t(scale_df$plur, lang = lang),
    select_id = select_id,
    treated_scale = treated_scale
  ))
}

#' Get parent title of a variable
#'
#' This function retrieves the title of the parent variable given the variable
#' name and returns it in lowercase.
#'
#' @param var <`character`> The variable name for which the parent title needs
#' to be retrieved.
#' @param lang <`character`> Active language. "en" or "fr". Defaults to NULL
#' for no translation.
#'
#' @return The title of the parent variable in lowercase.
explore_text_parent_title <- function(var, lang = NULL) {
  # Get the parent_vec of the current variable
  parent_string <- var_get_info(var = var, what = "parent_vec")

  # Grab the title of that parent_vec
  # If the parent vector is not in the variables table, return it
  variables <- get_from_globalenv("variables")
  if (!parent_string %in% variables$var_code) {
    if (parent_string == "population") {
      return(cc_t("individuals", lang = lang))
    }
    return(cc_t(parent_string, lang = lang))
  }

  parent_string <- var_get_info(
    var = parent_string, what = "var_title",
    check_year = FALSE, translate = TRUE, lang = lang
  )

  # To lowercase
  parent_string <- tolower(parent_string)

  # Return
  return(parent_string)
}

#' Get region values data frame
#'
#' This function retrieves the region values data frame given the variable name
#' and region and subsets the data frame based on the region name. If the variable
#' is a year, then it filters the row based on the year. It then returns the
#' resulting data frame.
#'
#' If there is a selection, then
#'
#' @param var <`character`> The variable name for which the region values data
#' frame needs to be retrieved.
#' @param region <`character`> Character string specifying the name of the region.
#' Usually equivalent of `r$region()`.
#' @param select_id <`character`> the current selected ID, usually
#' `r[[id]]$select_id()`.
#' @param col <`character`> Which column of `data` should be selected to grab the
#' value information. Defaults to `var_left`, but could also be `var_right` or
#' `var_left_1` in delta.
#' @param lang <`character`> Active language. "en" or "fr". Defaults to NULL
#' for no translation.
#' @param time <`numeric named list`> The `time` at which data is displayed.
#' A list for var_left and var_right. The output of \code{\link{vars_build}}(...)$time.
#' @param ... Additional arguments for the \code{\link{explore_text_select_val}}
#' @param scale <`reactive character`> Current scale. The output of
#' \code{\link{update_scale}}.
#' @param data <`data.frame`> The output of \code{\link{data_get}}.
#' @param schemas <`named list`> Current schema information. The additional widget
#' values that have an impact on which data column to pick. Usually `r[[id]]$schema()`.
#' @param data_path <`character`> A string representing the path to the
#' directory containing the QS files. Default is "data/".
#'
#' @return The resulting data frame after subsetting or list when there is a
#' selection.
explore_text_region_val_df <- function(var, region, select_id, col = "var_left",
                                       scale, data, lang = NULL, time, schemas = NULL,
                                       data_path = get_data_path(), ...) {
  if (is.na(select_id)) {
    # Grab the region values dataframe
    region_values <- region_value(
      var = var, data = data, time = time, col = col,
      scale = scale, region = region, schemas = schemas,
      data_path = data_path
    )

    # Return the values
    return(region_values)
  }

  return(explore_text_select_val(
    var = var,
    region = region,
    scale = scale,
    select_id = select_id,
    col = col,
    lang = lang,
    time = time,
    data = data,
    schemas = schemas,
    data_path = data_path,
    ...
  ))
}

#' Get parent data for a given variable and ID
#'
#' This function retrieves the parent data for a given variable and ID.
#' If a time variable is present in the dataset, the time value is
#' added to the parent string to retrieve the corresponding data.
#'
#' @param var <`character`> The code of the variable for which to retrieve the
#' parent data.
#' @param select_id <`character`> The ID of the selected zone for which to
#' retrieve the parent data.
#' @param scale <`character`> The crrent scale, e.g. `"CT"`
#' @param col <`character`> Which column of `data` should be selected to grab the
#' value information. Defaults to `var_left`, but could also be `var_right` or
#' `var_left_1` in delta.
#' @param time_col <`numeric`> Time at which to show the data.
#' @param data_path <`character`> A string representing the path to the
#' directory containing the QS files. Default is "data/".
#'
#' @return A vector containing the parent value for the zone.
explore_get_parent_data <- function(var, select_id, scale, col = "var_left",
                                    time_col, data_path) {
  # Get the parent string
  parent_string <- var_get_info(var = var, what = "parent_vec")

  # Grab the parent data, usually through data_get. If it fails, try to grab
  # the data from the global scale in the global environment (this is useful for
  # place explorer generation.)
  parent_data <- tryCatch(
    data_get(parent_string,
      scale = scale, vr_vl = col,
      data_path = data_path
    ),
    error = function(e) {
      data <- get_from_globalenv(scale)
      if (!parent_string %in% names(data)) {
        return(print(paste0(parent_string, " not found in the data files.")))
      }
      data <- data[c("ID", parent_string)]
      names(data)[2] <- "var_left"
      data
    }
  )

  rcol <- sprintf("%s_%s", col, time_col)

  # Get the parent value for the zone
  all_count <- parent_data[[rcol]][parent_data$ID == select_id]

  # Return
  return(all_count)
}

#' Generate values for the given variable and selection
#'
#' This function dispatches to the appropriate value-generating function based on
#' the variable type and returns the resulting values. It is only used when
#' there is a selection, and replaces the value of \code{\link{explore_text_region_val_df}}
#'
#' @param var <`character`> The variable code of the variable for which the
#' values need to be generated. Usually one element of the output of
#' \code{\link{vars_build}}.
#' @param select_id <`character`> The ID of the selected zone.
#' @param data <`data.frame`> A data frame containing the variables and
#' observations. The output of \code{\link{data_get}}.
#' @param scale <`character`> Current scale. The output of
#' \code{\link{update_scale}}.
#' @param time <`numeric named list`> The `time` at which data is displayed.
#' A list for var_left and var_right. The output of \code{\link{vars_build}}(...)$time.
#' @param col <`character`> Which column of `data` should be selected to grab the
#' value information. Defaults to `var_left`, but could also be `var_right` or
#' `var_left_1` in delta.
#' @param schemas <`named list`> Current schema information. The additional widget
#' values that have an impact on which data column to pick. Usually `r[[id]]$schema()`.
#' @param ... Additional arguments passed to the dispatched function.
#'
#' @return The resulting values
#' @export
explore_text_select_val <- function(var, ...) {
  UseMethod("explore_text_select_val", var)
}

#' @describeIn explore_text_select_val Method for pct
#' @param data_path <`character`> A string representing the path to the
#' directory containing the QS files. Default is "data/".
#' @export
explore_text_select_val.pct <- function(var, select_id, data, scale, col = "var_left",
                                        time, schemas = NULL, data_path, ...) {
  # Create empty vector
  out <- c()

  # Throw error if the selected ID is not in the data.
  if (!select_id %in% data$ID) {
    stop(sprintf("`%s` is not in the data.", select_id))
  }

  rcol <- match_schema_to_col(data = data, time = time, col = col, schemas = schemas)

  # Add the percentage value for the selection. Second column is always
  out$val <- data[[rcol]][data$ID == select_id]

  # Get the parent data
  all_count <- explore_get_parent_data(
    var = var, select_id = select_id,
    scale = scale, time_col = time[[col]],
    data_path = data_path
  )

  # Multiply the percentage by the count of parent in the zone
  out$count <- out$val * all_count

  # Round to the closest 5
  out$count <- round(out$count / 5) * 5

  # Return
  return(out)
}

#' @describeIn explore_text_select_val Method for `ind`
#' @param lang <`character`> Active language. `"en"` or `"fr"`
#' @export
explore_text_select_val.ind <- function(var, data, select_id, col = "var_left",
                                        time, lang, schemas = NULL, ...) {
  explore_text_select_val_ind(var = var, data = data, select_id = select_id,
                              col = col, time = time, lang = lang, schemas = schemas,
                              ...)
}

#' Generate values for the given `ind` variable and selection
#'
#' @param var <`character`> The variable code of the variable for which the
#' values need to be generated. Usually one element of the output of
#' \code{\link{vars_build}}.
#' @param select_id <`character`> The ID of the selected zone.
#' @param data <`data.frame`> A data frame containing the variables and
#' observations. The output of \code{\link{data_get}}.
#' @param col <`character`> Which column of `data` should be selected to grab the
#' value information. Defaults to `var_left`, but could also be `var_right` or
#' `var_left_1` in delta.
#' @param time <`numeric named list`> The `time` at which data is displayed.
#' A list for var_left and var_right. The output of \code{\link{vars_build}}(...)$time.
#' @param lang <`character`> Language the ranking character should be translated
#' to. Defaults to NULL for no translation.
#' @param schemas <`named list`> Current schema information. The additional widget
#' values that have an impact on which data column to pick. Usually `r[[id]]$schema()`.
#' @param val <`numeric`> If the value is not part of `data`. It happens on raster
#' data where we show region values for the highest resolution possible, but we still
#' want to allow user to select grid cells of lower resolutions. Defaults to NULL
#' for normal operations.
#' @param ... Additional arguments passed to the dispatched function.
#'
#' @return The resulting values
#' @export
explore_text_select_val_ind <- function(var, data, select_id, col = "var_left",
                                        time, lang, schemas = NULL, val = NULL, ...) {
  UseMethod("explore_text_select_val_ind", var)
}

#' @describeIn explore_text_select_val_ind Method for `scalar`
#' @export
explore_text_select_val_ind.scalar <- function(var, data, select_id, col = "var_left",
                                               time, lang, schemas = NULL, val = NULL, ...) {

  # Create empty vector
  out <- c()

  # Throw error if the selected ID is not in the data.
  if (is.null(val) & !select_id %in% data$ID) {
    stop(sprintf("`%s` is not in the data.", select_id))
  }

  rank <- if (is.null(val)) {
    rcol <- match_schema_to_col(data, time = time, col = col, schemas = schemas)
    brk_col <- sprintf("%s_q5", rcol)

    # Get the group in which falls the selection
    rank <- data[[brk_col]][data$ID == select_id]
  } else {
    findInterval(val, attr(data, "breaks_var_left"))
  }

  # Grab the rank name for the rank
  rank_names <- var_get_info(var = var, what = "rank_name")[[1]]
  out$val <- rank_names[rank]

  # Lower letters
  out$val <- tolower(cc_t(out$val, lang = lang))

  out$num <- if (!is.null(val)) val else data[[rcol]][data$ID == select_id]

  # Return
  return(out)

}

#' @describeIn explore_text_select_val_ind Method for `ordinal`
#' @export
explore_text_select_val_ind.ordinal <- function(var, data, select_id, col = "var_left",
                                                time, lang, schemas = NULL, val = NULL, ...) {

  # Create empty vector
  out <- c()

  # Throw error if the selected ID is not in the data.
  if (is.null(val) & !select_id %in% data$ID) {
    stop(sprintf("`%s` is not in the data.", select_id))
  }

  rank <- if (!is.null(val)) val else {
    rcol <- match_schema_to_col(data, time = time, col = col, schemas = schemas)

    # Get the group in which falls the selection
    data[[rcol]][data$ID == select_id]
  }

  # Grab the rank name for the rank
  rank_names <- var_get_info(var = var, what = "rank_name")[[1]]
  out$val <- rank_names[rank]

  # Lower letters
  out$val <- tolower(cc_t(out$val, lang = lang))

  out$num <- rank

  # Return
  return(out)

}

#' @describeIn explore_text_select_val Default method
#' @export
explore_text_select_val.default <- function(var, data, select_id, col = "var_left",
                                            time, schemas = NULL, ...) {
  # Create empty vector
  out <- c()

  # Throw error if the selected ID is not in the data.
  if (!select_id %in% data$ID) {
    stop(sprintf("`%s` is not in the data.", select_id))
  }

  rcol <- match_schema_to_col(data = data, time = time, col = col, schemas = schemas)

  # Add the value for the selection
  out$val <- data[[rcol]][data$ID == select_id]

  # Return
  return(out)
}

#' Explore Text Selection Comparison
#'
#' This function calculates the percentage of observations with a lower value
#' than the selected observation for a given variable and ranks the selected
#' observation within predefined groups (as a character extracted from the
#' `variables` table.)
#'
#' @param var <`character`> A variable code specifying the variable of interest. This
#' variable will be compared across observations.
#' @param data <`data.frame`> A data frame containing the variables and
#' observations. The data frame must have columns named var_left
#' and ID. The output of \code{\link{data_get}}.
#' @param select_id <`character`> The ID of the selected zone for which to
#' retrieve the ranking.
#' @param col <`character`> Which column of `data` should be selected to grab the
#' value information. Defaults to `var_left`, but could also be `var_right` or
#' `var_left_1` in delta.
#' @param ranks_override <`character vector`> A vector of length 5. Used if
#' the ranks in the `variables$rankings_chr` table should be overriden for the
#' `rank_chr` output. Used in `delta` mode where it's a change in percentage
#' over two years, and so the character rankings from the variables table do not
#' matter.
#' @param lang <`character`> Language the ranking character should be translated
#' to. Defaults to NULL for no translation.
#' @param time_col <`numeric`> Time at which to show the data.
#' @param schemas <`named list`> Current schema information. The additional widget
#' values that have an impact on which data column to pick. Usually `r[[id]]$schema()`.
#' @param larger <`logical`> Should we write 'larger than' or 'higher than' ?
#' @param val <`numeric`> If the value is not part of `data`. It happens on raster
#' data where we show region values for the highest resolution possible, but we still
#' want to allow user to select grid cells of lower resolutions. Defaults to NULL
#' for normal operations.
#'
#' @return A named list with two elements:
#' \itemize{
#' \item \code{higher_than}: A character string representing the proportion of
#' other observations with a lower value for the specified variable than the
#' selected observation, formatted as a percentage with one decimal place.
#' \item \code{rank_chr}: A character string representing the ranking category
#' of the selected observation in comparison to the other observations for the
#' specified variable.
#' \item \code{higher_than_num}: A numeric value representing the proportion of
#' other observations with a lower value for the specified variable than the
#' selected observation.
#' }
explore_text_selection_comparison <- function(var = NULL, data, select_id,
                                              col = "var_left",
                                              ranks_override = NULL,
                                              lang = NULL, time_col, schemas = NULL,
                                              larger = FALSE, val = NULL) {
  # Throw error if the selected ID is not in the data.
  if (is.null(val) & !select_id %in% data$ID) {
    stop(sprintf("`%s` is not in the data.", select_id))
  }

  rcol <- match_schema_to_col(data = data, time = time_col, col = col, schemas = schemas)
  # In the case of `delta`, the value to look at will be the variation (no years)
  if (length(rcol) == 2) rcol <- col

  # If val is supplied. In the case two values are supplied, it's a delta.
  current_val <- if (!is.null(val)) {
    if (length(val) == 2) (val[2] - val[1]) / val[1] else val
  } else data[[rcol]][data$ID == select_id]

  # The value is higher than X of other observations
  higher_than <- current_val > data[[rcol]]
  higher_than <- mean(higher_than, na.rm = TRUE)

  if (is.na(higher_than)) {
    return(list(
      higher_than = NA,
      rank_chr = NA
    ))
  }

  # Ranking as characters. We can't use q5 as it's built for breaks of
  # multiple years. Here we only compare with ONE year.
  quants <- 1:100 %% 20
  quants <- which(quants == 0) / 100
  rank <- findInterval(higher_than, quants) + 1
  ranks_chr <-
    if (is.null(ranks_override)) {
      var_get_info(var = var, what = "rankings_chr")[[1]]
    } else {
      ranks_override
    }
  rank_chr <- ranks_chr[[rank]]

  # Translate rank_chr and convert to bold
  rank_chr <- cc_t(rank_chr, lang = lang)
  rank_chr <- sprintf("<b>%s</b>", rank_chr)

  # Update which is we want to read: 'lower' or 'higher' than x ?
  if (higher_than < 0.5) {
    higher_lower <- cc_t(if (!larger) "lower" else "smaller", lang = lang)
    higher_than <- 1 - higher_than
  } else {
    higher_lower <- cc_t(if (!larger) "higher" else "larger", lang = lang)
  }
  higher_than_chr <- convert_unit.pct(x = higher_than, decimal = 0)

  # Return both
  return(list(
    higher_lower = higher_lower,
    higher_lower_than = higher_than_chr,
    rank_chr = rank_chr,
    higher_lower_than_num = higher_than
  ))
}

#' Helper function for exploring bivariate correlation between variables
#'
#' This function calculates bivariate correlation between two text variables
#' using either Pearson's correlation coefficient or Spearman's rank correlation
#' coefficient based on the type of variables passed as arguments. The result is
#' returned as a list containing the correlation coefficient and a formatted
#' string.
#'
#' @param vars <`character`> A list containing the variable names for which the
#' text needs to be generated. Usually the output of \code{\link{vars_build}}.
#' @param data <`data.frame`> A data frame containing the variables and
#' observations. The output of \code{\link{data_get}}.
#' @param time <`numeric named list`> The `time` at which data is displayed.
#' A list for var_left and var_right. The output of \code{\link{vars_build}}(...)$time.
#' @param lang <`character`> A string indicating the language in which to
#' translates the variable. Defaults to NULL.
#' @param schemas <`named list`> Current schema information. The additional widget
#' values that have an impact on which data column to pick. Usually `r[[id]]$schema()`.
#' @param ... Additional arguments to be passed.
#'
#' @return A list containing the correlation coefficient and a formatted string.
#' @export
explore_text_bivar_correlation_helper <- function(vars, data, time, lang = NULL,
                                                  schemas = NULL, ...) {
  UseMethod("explore_text_bivar_correlation_helper", vars)
}

#' @rdname explore_text_bivar_correlation_helper
#' @export
explore_text_bivar_correlation_helper.scalar <- function(vars, data, time,
                                                         lang = NULL,
                                                         schemas = NULL, ...) {
  # Match schema
  vl_col <- match_schema_to_col(data = data, time = time, col = "var_left", schemas = schemas)
  vr_col <- match_schema_to_col(data = data, time = time, col = "var_right", schemas = schemas)

  # If we're in delta_bivar, we shouldn't be grabbing the correlation with the
  # time, but the correlation between the two deltas variables.
  if (length(vl_col) == 2) vl_col <- "var_left"
  if (length(vr_col) == 2) vr_col <- "var_right"

  # Correlation
  corr <- stats::cor(data[[vl_col]], data[[vr_col]], use = "complete.obs")
  corr_string <- sprintf(
    cc_t("Pearson's r: %s", lang = lang),
    round(corr, digits = 2)
  )

  return(list(
    corr = corr,
    corr_string = corr_string
  ))
}

#' @rdname explore_text_bivar_correlation_helper
#' @export
explore_text_bivar_correlation_helper.ordinal <- function(vars, data, time,
                                                          lang = NULL,
                                                          schemas = NULL, ...) {
  # Match schema
  vl_col <- match_schema_to_col(data = data, time = time, col = "var_left", schemas = schemas)
  vr_col <- match_schema_to_col(data = data, time = time, col = "var_right", schemas = schemas)

  # If we're in delta_bivar, we shouldn't be grabbing the correlation with the
  # time, but the correlation between the two deltas variables.
  if (length(vl_col) == 2) vl_col <- "var_left"
  if (length(vr_col) == 2) vr_col <- "var_right"

  # Correlation
  corr <- stats::cor(data[[vl_col]], data[[vr_col]],
    use = "complete.obs",
    method = "spearman"
  )
  corr_string <- sprintf(
    cc_t("Spearman's rho: %s", lang = lang),
    round(corr, digits = 2)
  )

  return(list(
    corr = corr,
    corr_string = corr_string
  ))
}

#' Explore Text Color Function
#'
#' This function takes a text input x and assigns it a color based on the meaning
#' argument. The colors are determined using the global environment's color
#' schemes for bivariate and delta categories.
#'
#' @param x <`character`> A character string to which a color will be assigned.
#' @param meaning <`character`> A character string indicating the color meaning.
#' Can be one of the following: "left", "right", "increase", or "decrease".
#' Default is "left".
#'
#' @return A character string with an HTML span tag containing the input text x and its
#' assigned color as an inline style.
#' @export
explore_text_color <- function(x, meaning) {
  if (!meaning %in% c("left", "right", "increase", "decrease")) {
    stop('`meaning` must be one of c("left", "right", "increase", "decrease")')
  }

  # Grab the colours from the global environment
  colours <- colours_get()

  # Make the hex
  hex <- (\(x) if (meaning == "left") {
    clr <- colours$bivar$fill[colours$bivar$group == "3 - 1"]
    return(clr)
  } else if (meaning == "right") {
    clr <- colours$bivar$fill[colours$bivar$group == "1 - 3"]
    return(clr)
  } else if (meaning == "increase") {
    clr <- colours$delta$fill[colours$delta$group == "5"]
    return(clr)
  } else if (meaning == "decrease") {
    clr <- colours$delta$fill[colours$delta$group == "1"]
    return(clr)
  })()

  # Add the text in a span with the color
  x_colored <- sprintf("<span style='color:%s'>%s</span>", hex, x)

  # Add the color to the bullet point lists if there are
  x_colored <- gsub(
    "<li>",
    sprintf("<li style='color:%s'>", hex),
    x_colored
  )

  # Return
  return(x_colored)
}

#' Checks if data is NA for a given selection
#'
#' This function checks if 'var_left' or 'var_right' are NA for a given 'select_id'
#' in the provided data frame. If NA is found, a message is generated using the
#' 'explanation' element of the corresponding variable from 'vars' object. The
#' message is then combined with 'p_start' from 'context' object to form a
#' complete sentence.
#'
#' @param context <`list`> list that should include 'p_start' which is used as
#'   the start of the output sentence. The output of \code{\link{explore_context}}
#' @param data <`data.frame`> A data frame containing the variables and
#' observations. The output of \code{\link{data_get}}.
#' @param select_id <`character`> the current selected ID, usually
#' `r[[id]]$select_id()`. If there is a selection (select_id is not NA), the
#' name of the selected polygon will appear.
#' @param vars <`character`> A list containing the variable names for which the
#' text needs to be generated. Usually the output of \code{\link{vars_build}}.
#' @param time <`list of numeric vector`> The `time` at which data is displayed.
#' A list for var_left and var_right. The output of \code{\link{vars_build}}(...)$time.
#' @param lang <`character`> A string indicating the language in which to
#' translates the variable. Defaults to NULL.
#' @param schemas <`named list`> Current schema information. The additional widget
#' values that have an impact on which data column to pick. Usually `r[[id]]$schema()`.
#' @param val <`numeric`> If the value is not part of `data`. It happens on raster
#' data where we show region values for the highest resolution possible, but we still
#' want to allow user to select grid cells of lower resolutions.
#'
#' @return If there is NAs in the selection, returns a string that starts with
#' 'p_start' from the 'context' object and includes a message indicating that
#' the information for 'var_left' or 'var_right' is not available. Returns NULL
#' otherwise.
explore_text_check_na <- function(context, data, select_id, vars, time,
                                  lang = NULL, schemas = NULL, val = NULL) {
  # If there are no selection, returns NULL
  if (is.na(select_id)) {
    return(NULL)
  }

  # Construct the NA output text
  na_text <- \(var, var_left) {
    exp <- var_get_info(
      var = var, what = "explanation",
      translate = TRUE, lang = lang, schemas_col = schemas[[if (var_left) "var_left" else "var_right"]]
    )
    out <- sprintf(cc_t("we currently don't have information regarding %s",
                        lang = lang
    ), exp)
    sprintf("<p>%s, %s.", s_sentence(context$p_start), out)
  }

  # If there is a selection, and the value is already supplied
  if (!is.null(val)) {
    if (!any(sapply(val, is.na))) return(NULL)

    return(na_text(var = vars$var_left, var_left = TRUE))
  }

  # Construct the vl and vr columns. If in delta mode, we need to grab the
  # delta columns instead of the time columns.
  vl <- match_schema_to_col(data = data, time = time, col = "var_left", schemas = schemas)
  if (length(vl) == 2) vl <- "var_left"
  vr <- match_schema_to_col(data = data, time = time, col = "var_right", schemas = schemas)
  if (length(vl) == 2) vl <- "var_right"

  out <- lapply(c(vl, vr), \(col) {
    if (!col %in% names(data)) {
      return(NULL)
    }

    val <- data[[col]][data$ID == select_id]
    if (length(val) == 0) stop(sprintf("`%s` is not in the data.", select_id))

    if (!is.na(val)) {
      return(NULL)
    }

    var_left <- grepl("var_left", col)
    var <- if (var_left) vars$var_left else vars$var_right

    out <- na_text(var, var_left)

    return(out)
  })

  out <- out[!sapply(out, is.null)]
  out <- unique(out)
  out <- unlist(out)

  return(out)
}
