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
#' @param df <`character`> The combination of the region under study and the
#' scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param switch_DA <`logical`> Is the `df` part of the scales that should be
#' switched as DAs instead.
#'
#' @return A list containing multiple texts used for the explore text panel.
explore_context <- function(region, select_id, df, switch_DA) {
  # Grab the region dictionary
  regions_dictionary <- get_from_globalenv("regions_dictionary")
  region_df <- regions_dictionary[regions_dictionary$region == region, ]

  # Prepare a return when there is no selection
  region_only_return <- \(region_df) {
    # Grab the region text
    to_compare <- region_df$to_compare

    # Return as a sentence
    return(list(p_start = to_compare))
  }

  # If there is no selection, return the region text only
  if (is.na(select_id)) {
    return(region_only_return(region_df))
  }

  # Grab the right scale
  scales_dictionary <- get_from_globalenv("scales_dictionary")
  scale <- scales_dictionary[
    is_scale_df(scales_dictionary$scale, df = df, vectorized = TRUE),
  ]

  # Normal retrieval when the `df` is not part of the scales to treat as
  # DA.
  if (!switch_DA) {
    # Get the place heading and glue it
    dat <- get_from_globalenv(df)
    dat <- dat[dat$ID == select_id, ]
    name_2 <- dat$name_2
    name <- dat$name
    heading <- glue::glue(scale$place_heading)
  }

  # Tweaked retrieval when the `df` is part of the scales to treat as DAs.
  if (switch_DA) {
    # Grab the DA ID and the address from the SQL database
    sql_link <- eval(parse(text = (paste0(gsub(".*_", "", df), "_conn"))))
    bs <- DBI::dbGetQuery(
      sql_link,
      sprintf(
        "SELECT name, DA_ID FROM %s WHERE ID = '%s'",
        df, select_id
      )
    )

    # If the selection ID is not in the SQL database, return the region only text
    # with an empty NA. The text that will be showed is the basic one for the region.
    if (nrow(bs) == 0) {
      out <- region_only_return(region_df)
      out$select_id <- NA
      return(out)
    }

    # Get the heading
    name <- bs$name
    heading <- glue::glue(scale$place_heading)

    # Switch the select_id, to be able to use the data values of the `DA`
    select_id <- bs$DA_ID
  }

  # Get the sentence start (In Borough or In dissemination area XYZ, )
  p_start <- cc_t(tolower(scale$place_name))

  # Return
  return(list(
    heading = heading,
    p_start = cc_t("in {p_start}"),
    name = cc_t("in {name}"),
    to_compare_determ = region_df$to_compare_determ,
    to_compare_short = region_df$to_compare_short,
    scale_plur = scale$plur,
    select_id = select_id
  ))
}

#' Get parent title of a variable
#'
#' This function retrieves the title of the parent variable given the variable
#' name and returns it in lowercase.
#'
#' @param var <`character`> The variable name for which the parent title needs
#' to be retrieved.
#'
#' @return The title of the parent variable in lowercase.
explore_text_parent_title <- function(var) {
  # Get the parent_vec of the current variable
  parent_string <- var_get_info(var = var, what = "parent_vec")

  # Grab the title of that parent_vec
  # If the parent vector is all population/individuals or households, return it
  if (parent_string %in% c("individuals", "households")) {
    return(parent_string)
  }
  parent_string <- var_get_info(
    var = parent_string, what = "var_title",
    check_year = FALSE
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
#' @param ... Additional arguments for the \code{\link{explore_text_select_val}}
#' function: \itemize{
#'  \item{data <`data.frame`>}{The output of \code{\link{data_get}}.}
#'  \item{df <`character`>}{The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.}
#' }
#'
#' @return The resulting data frame after subsetting or list when there is a
#' selection.
explore_text_region_val_df <- function(var, region, select_id, col = "var_left", ...) {
  if (is.na(select_id)) {
    # Grab the region values dataframe
    region_values <- var_get_info(var = var, what = "region_values")[[1]]

    # Subset current region
    region_values <- region_values[region_values$region == region, ]

    # If year, filter the right row
    if ("year" %in% names(region_values)) {
      time <- var_get_time(var)
      if (is.na(time)) stop(sprintf("var `%s` needs an appended time.", var))
      region_values <- region_values[region_values[["year"]] == time, ]
    }

    # Return the values
    return(region_values)
  }

  return(explore_text_select_val(
    var = var,
    region = region,
    select_id = select_id,
    col = col,
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
#' @param df <`character`>The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param col <`character`> Which column of `data` should be selected to grab the
#' value information. Defaults to `var_left`, but could also be `var_right` or
#' `var_left_1` in delta.
#'
#' @return A vector containing the parent value for the zone.
explore_get_parent_data <- function(var, select_id, df, col = "var_left") {
  # Get the parent string
  parent_string <- var_get_info(var = var, what = "parent_vec")

  # Is there a time?
  time <- var_get_time(var)

  # If so, add it to the parent string
  if (!is.na(time)) parent_string <- paste(parent_string, time, sep = "_")

  # Grab the parent data
  parent_data <- data_get(parent_string, df)

  # Get the parent value for the zone
  all_count <- parent_data[[col]][parent_data$ID == select_id]

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
#' @param ... Additional arguments passed to the dispatched function.
#'
#' @return The resulting values
#' @export
explore_text_select_val <- function(var, ...) {
  UseMethod("explore_text_select_val", var)
}

#' @rdname explore_text_select_val
#'
#' @param select_id <`character`> the current selected ID, usually
#' `r[[id]]$select_id()`.
#' @param data <`data.frame`>The output of \code{\link{data_get}}.
#' @param df <`character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param col <`character`> Which column of `data` should be selected to grab the
#' value information. Defaults to `var_left`, but could also be `var_right` or
#' `var_left_1` in delta.
#'
#' @export
explore_text_select_val.pct <- function(var, select_id, data, df, col = "var_left",
                                        ...) {
  # Create empty vector
  out <- c()

  # Throw error if the selected ID is not in the data.
  if (!select_id %in% data$ID) {
    stop(sprintf("`%s` is not in the data.", select_id))
  }

  # Add the percentage value for the selection. Second column is always
  out$val <- data[[col]][data$ID == select_id]

  # Get the parent data
  all_count <- explore_get_parent_data(
    var = var, select_id = select_id,
    df = df
  )

  # Multiply the percentage by the count of parent in the zone
  out$count <- out$val * all_count

  # Round to the closest 5
  out$count <- round(out$count / 5) * 5

  # Return
  return(out)
}

#' @rdname explore_text_select_val
#'
#' @param select_id <`character`> the current selected ID, usually
#' `r[[id]]$select_id()`.
#' @param data <`data.frame`>The output of \code{\link{data_get}}.
#' @param col <`character`> Which column of `data` should be selected to grab the
#' value information. Defaults to `var_left`, but could also be `var_right` or
#' `var_left_1` in delta.
#'
#' @export
explore_text_select_val.dollar <- function(var, data, select_id, col = "var_left",
                                           ...) {
  # Create empty vector
  out <- c()

  # Throw error if the selected ID is not in the data.
  if (!select_id %in% data$ID) {
    stop(sprintf("`%s` is not in the data.", select_id))
  }

  # Add the value for the selection
  out$val <- data[[col]][data$ID == select_id]

  # Return
  return(out)
}

#' @rdname explore_text_select_val
#'
#' @param select_id <`character`> the current selected ID, usually
#' `r[[id]]$select_id()`.
#' @param data <`data.frame`>The output of \code{\link{data_get}}.
#' @param df <`character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param col <`character`> Which column of `data` should be selected to grab the
#' value information. Defaults to `var_left`, but could also be `var_right` or
#' `var_left_1` in delta.
#'
#' @export
explore_text_select_val.ind <- function(var, data, df, select_id, col = "var_left",
                                        ...) {
  # Create empty vector
  out <- c()

  # Throw error if the selected ID is not in the data.
  if (!select_id %in% data$ID) {
    stop(sprintf("`%s` is not in the data.", select_id))
  }

  # Get the group in which falls the selection
  rank <- data[[col]][data$ID == select_id]

  # Grab the rank name for the rank
  brks <- var_get_info(var = var, what = "breaks_q5")[[1]]
  brks <- brks[brks$df == df, ]
  out$val <- brks$rank_name[brks$rank == rank]

  # Lower letters
  out$val <- tolower(out$val)

  # Return
  return(out)
}

#' Explore Text Selection Comparison
#'
#' This function calculates the percentage of observations with a lower value
#' than the selected observation for a given variable and ranks the selected
#' observation within predefined groups (as a character extracted from the
#' `variables` table.
#'
#' @param var <`character`> A variable code specifying the variable of interest. This
#' variable will be compared across observations.
#' @param data <`data.frame`> A data frame containing the variables and
#' observations to be compared. The data frame must have columns named var_left
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
explore_text_selection_comparison <- function(var, data, select_id,
                                              col = "var_left",
                                              ranks_override = NULL) {
  # Throw error if the selected ID is not in the data.
  if (!select_id %in% data$ID) {
    stop(sprintf("`%s` is not in the data.", select_id))
  }

  # The value is higher than X of other observations
  higher_than <- data[[col]][data$ID == select_id] > data[[col]]
  higher_than <- mean(higher_than, na.rm = TRUE)
  if (is.na(higher_than)) {
    return(list(
      higher_than = NA,
      rank_chr = NA
    ))
  }
  higher_than_chr <- convert_unit.pct(x = higher_than, decimal = 0)

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

  # Return both
  return(list(
    higher_than = higher_than_chr,
    rank_chr = rank_chr,
    higher_than_num = higher_than
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
#' observations to be compared. The output of \code{\link{data_get}}.
#' @param lang <`character`> A string indicating the language in which to
#' translates the variable. Defaults to NULL.
#' @param ... Additional arguments to be passed.
#'
#' @return A list containing the correlation coefficient and a formatted string.
#' @export
explore_text_bivar_correlation_helper <- function(vars, data, lang, ...) {
  UseMethod("explore_text_bivar_correlation_helper", vars)
}

#' @rdname explore_text_bivar_correlation_helper
#' @export
explore_text_bivar_correlation_helper.scalar <- function(vars, data,
                                                         lang = NULL, ...) {

  # Correlation
  corr <- stats::cor(data$var_left, data$var_right, use = "complete.obs")
  corr_string <- sprintf("Pearson's r: %s", round(corr, digits = 2))
  corr_string <- cc_t(corr_string, lang = lang)

  return(list(corr = corr,
              corr_string = corr_string))
}

#' @rdname explore_text_bivar_correlation_helper
#' @export
explore_text_bivar_correlation_helper.ordinal <- function(vars, data,
                                                          lang = NULL, ...) {

  # Correlation
  corr <- stats::cor(data$var_left, data$var_right,
              use = "complete.obs", method = "spearman")
  corr_string <- sprintf("Spearman's rho: %s", round(corr, digits = 2))
  corr_string <- cc_t(corr_string, lang = lang)

  return(list(corr = corr,
              corr_string = corr_string))
}
