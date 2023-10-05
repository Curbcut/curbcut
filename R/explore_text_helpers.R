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
#' @param lang <`character`> Active language. "en" or "fr". Defaults to NULL
#' for no translation.
#'
#' @return A list containing multiple texts used for the explore text panel.
explore_context <- function(region, select_id, df, switch_DA, lang = NULL) {
  # Grab the region dictionary
  regions_dictionary <- get_from_globalenv("regions_dictionary")
  region_df <- regions_dictionary[regions_dictionary$region == region, ]

  # Prepare a return when there is no selection
  region_only_return <- \(region_df) {
    # Grab the region text
    to_compare <- cc_t(region_df$to_compare, lang = lang)

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
    is_scale_in(scales_dictionary$scale, df = df, vectorized = TRUE),
  ]

  # Normal retrieval when the `df` is not part of the scales to treat as
  # DA.
  if (!switch_DA) {
    # Get the place heading and glue it
    dat <- grab_row_from_bslike(
      df = df, select_id = select_id,
      cols = c("name", "name_2")
    )
    name <- dat$name
    name_2 <- cc_t(dat$name_2, lang = lang)
    heading <- cc_t(scale$place_heading, lang = lang)
    treated_df <- df
  }

  # Tweaked retrieval when the `df` is part of the scales to treat as DAs.
  if (switch_DA) {
    # Grab the DA ID and the address from the SQL database
    bs <- grab_row_from_bslike(
      df = df, select_id = select_id,
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
      cc_t(scale$place_heading, lang = lang)
    )

    # Switch the select_id, to be able to use the data values of the `DA`
    select_id <- bs$DA_ID

    # Switch df to DA
    treated_df <- sprintf("%s_DA", region)

    # Switch the scale
    scale <- scales_dictionary[is_scale_in(scales_dictionary$scale,
      treated_df,
      vectorized = TRUE
    ), ]
  }

  # Get the sentence start (In Borough or In dissemination area XYZ, )
  p_start <- cc_t(scale$place_name, lang = lang)

  # Return
  return(list(
    heading = heading,
    p_start = cc_t("in {p_start}", lang = lang),
    name = cc_t("in {name}", lang = lang),
    to_compare_determ = cc_t(region_df$to_compare_determ, lang = lang),
    to_compare_short = cc_t(region_df$to_compare_short, lang = lang),
    scale_plur = cc_t(scale$plur, lang = lang),
    select_id = select_id,
    treated_df = treated_df
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
explore_text_region_val_df <- function(var, region, select_id, col = "var_left",
                                       lang = NULL, ...) {
  if (is.na(select_id)) {
    # Grab the region values dataframe
    region_values <- var_get_info(var = var, what = "region_values")[[1]]

    # Subset current region
    region_values <- region_values[region_values$region == region, ]

    # If year, filter the right row
    if (all(region_values$year != "")) {
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
    lang = lang,
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

  # Grab the parent data, usually through data_get. If it fails, try to grab
  # the data from the global df in the global environment (this is useful for
  # place explorer generation.)
  parent_data <- tryCatch(data_get(parent_string, df),
    error = function(e) {
      data <- get_from_globalenv(df)
      if (!parent_string %in% names(data)) {
        return(print(paste0(parent_string, " not found in the data files.")))
      }
      data <- data[c("ID", parent_string)]
      names(data)[2] <- "var_left"
      data
    }
  )

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
#' @param df <`character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param col <`character`> Which column of `data` should be selected to grab the
#' value information. Defaults to `var_left`, but could also be `var_right` or
#' `var_left_1` in delta.
#' @param lang <`character`> Active language. `"en"` or `"fr"`
#'
#' @export
explore_text_select_val.ind <- function(var, data, df, select_id, col = "var_left",
                                        lang, ...) {
  # Create empty vector
  out <- c()

  # Throw error if the selected ID is not in the data.
  if (!select_id %in% data$ID) {
    stop(sprintf("`%s` is not in the data.", select_id))
  }

  # Get the group in which falls the selection
  rank <- data[[paste0(col, "_q5")]][data$ID == select_id]

  # Grab the rank name for the rank
  brks <- var_get_info(var = var, what = "breaks_q5")[[1]]
  brks <- brks[brks$df == df, ]
  out$val <- brks$rank_name[brks$rank == rank]

  # Lower letters
  out$val <- tolower(cc_t(out$val, lang = lang))

  if (!is.na(select_id)) {
    out$num <- data[[col]][data$ID == select_id]
  }

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
explore_text_select_val.default <- function(var, data, df, select_id, col = "var_left",
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
#' @param lang <`character`> Language the ranking character should be translated
#' to. Defaults to NULL for no translation.
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
                                              lang = NULL) {
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

  # Translate rank_chr and convert to bold
  rank_chr <- cc_t(rank_chr, lang = lang)
  rank_chr <- sprintf("<b>%s</b>", rank_chr)

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
explore_text_bivar_correlation_helper <- function(vars, data, lang = NULL, ...) {
  UseMethod("explore_text_bivar_correlation_helper", vars)
}

#' @rdname explore_text_bivar_correlation_helper
#' @export
explore_text_bivar_correlation_helper.scalar <- function(vars, data,
                                                         lang = NULL, ...) {
  # Correlation
  corr <- stats::cor(data$var_left, data$var_right, use = "complete.obs")
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
explore_text_bivar_correlation_helper.ordinal <- function(vars, data,
                                                          lang = NULL, ...) {
  # Correlation
  corr <- stats::cor(data$var_left, data$var_right,
    use = "complete.obs", method = "spearman"
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
#' observations to be compared. The output of \code{\link{data_get}}.
#' @param select_id <`character`> the current selected ID, usually
#' `r[[id]]$select_id()`. If there is a selection (select_id is not NA), the
#' name of the selected polygon will appear.
#' @param vars <`character`> A list containing the variable names for which the
#' text needs to be generated. Usually the output of \code{\link{vars_build}}.
#' @param lang <`character`> A string indicating the language in which to
#' translates the variable. Defaults to NULL.
#'
#' @return If there is NAs in the selection, returns a string that starts with
#' 'p_start' from the 'context' object and includes a message indicating that
#' the information for 'var_left' or 'var_right' is not available. Returns NULL
#' otherwise.
explore_text_check_na <- function(context, data, select_id, vars, lang = NULL) {
  # If there are no selection, returns NULL
  if (is.na(select_id)) {
    return(NULL)
  }

  # Check if var_left is NA
  val <- data$var_left[data$ID == select_id]
  if (is.na(val)) {
    exp <- var_get_info(
      var = vars$var_left[[1]], what = "explanation",
      translate = TRUE, lang = lang
    )
    out <- sprintf(cc_t("we currently don't have information regarding %s",
      lang = lang
    ), exp)
    out <- sprintf("<p>%s, %s.", s_sentence(context$p_start), out)
    return(out)
  }

  if ("var_right" %in% names(data)) {
    val <- data$var_right[data$ID == select_id]
    if (is.na(val)) {
      exp <- var_get_info(
        var = vars$var_right[[1]], what = "explanation",
        translate = TRUE, lang = lang
      )
      out <- sprintf(cc_t("we currently don't have information regarding %s",
        lang = lang
      ), exp)
      out <- sprintf("<p>%s, %s.", s_sentence(context$p_start), out)
      return(out)
    }
  }
}
