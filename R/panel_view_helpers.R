#' Prepare table to be shown to user
#'
#' @param vars <`named list`> Object built using the \code{\link{vars_build}}
#' function.
#' @param data <`reactive data.frame`> Data frame containing all the scale and
#' the `var_left` and `var_right`. The output of \code{\link{data_get}}.
#' @param df <`reactive character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param zoom_levels <`named numeric vector`> A named numeric vector of zoom
#' levels. Usually one of the `map_zoom_levels_x`, or the output of
#' \code{\link{zoom_get_levels}}. It needs to be `numeric` as the function
#' will sort them to make sure the lower zoom level is first, and the highest
#' is last (so it makes sense on an auto-zoom).
#' @param lang <`character`> The language to use for translating variable names.
#' Defaults to NULL for no translation
#'
#' @return Returns a list of two table. There is 'pretty table' ready to be
#' shown to the user, and another table that is for download.
table_view_prep_table <- function(vars, data, df, zoom_levels, lang = NULL) {
  # Reformat data -----------------------------------------------------------

  # Take out the `q3`, `q5` and `group`
  dat <- data
  dat <- dat[!grepl("_q3$|_q5$|group$", names(dat))]

  # Add `variation` if it is multi-year
  vars_ <- lapply(vars, \(x) {
    if (length(x) == 1) {
      return(x)
    }

    var_code <- var_get_info(x[[1]], what = "var_code")
    c(x, paste0(var_code, "_variation"))
  })

  # Rename the var_left and var_right
  names(dat)[grepl("var_left", names(dat))] <- vars_$var_left
  names(dat)[grepl("var_right", names(dat))] <- vars_$var_right

  # Bind the `df` data to the real data
  df_dat <- get_from_globalenv(df)
  df_dat <- df_dat[c("ID", "name", "name_2", "population", "households")]
  dat <- cbind(df_dat, dat[names(dat) != "ID"])

  # Order data by population
  dat <- dat[order(dat$population, decreasing = TRUE), ]


  # About the data information ----------------------------------------------

  text <- panel_view_prepare_text(vars = vars, df = df, dat = dat, lang = lang)

  # Update column names so it's more 'friendly' -----------------------------

  pretty_dat <- dat

  # Switch name_2 column name depending on `zoom_levels`
  # Switch df to scale as it can cause a bug where the region is the same as the
  # scale. centraide_CT catches both the `centraide` and the `CT` scale.
  scale <- gsub(".*_", "", df)
  which_zl <- which(is_scale_df(names(zoom_levels), scale, vectorized = TRUE))
  names(pretty_dat)[names(pretty_dat) == "name_2"] <-
    if (which_zl == 1) {
      "Scale"
    } else {
      zoom_get_name(names(zoom_levels)[1])
    }

  # Add a new column 'Scale' to be clear which scale the user is seeing
  if (which_zl > 1) {
    pretty_dat$Scale <- zoom_get_name(names(zoom_levels)[which_zl])
    first_cols <- c("ID", "Scale")
    rest_cols <- names(pretty_dat)[!names(pretty_dat) %in% first_cols]
    pretty_dat <- pretty_dat[, c(first_cols, rest_cols)]
  }

  # Start with the basic usual ones
  to_sentence_vector <-
    names(pretty_dat) %in% c("name", "population", "households")
  names(pretty_dat)[to_sentence_vector] <-
    sapply(s_sentence(names(pretty_dat)[to_sentence_vector]), cc_t, lang = lang)

  # Take out one of Name or ID if they are identical
  if (identical(pretty_dat$Name, pretty_dat$ID)) {
    pretty_dat <- pretty_dat[names(pretty_dat) != "Name"]
  }

  # Depending on the class of `vars`, update the other column names and
  # out as a datatable.
  pretty_dat <-
    panel_view_rename_cols(vars = vars, dat = pretty_dat, lang = lang, df = df)

  # Add population and households to the title vars so they also are formatted
  pretty_dat$title_vars <- c(
    pretty_dat$title_vars,
    list(structure(cc_t("Population", lang = lang), class = "count")),
    list(structure(cc_t("Households", lang = lang), class = "count"))
  )

  return(list(
    pretty_data = pretty_dat$data,
    data = dat,
    title_vars = pretty_dat$title_vars,
    text = text
  ))
}

#' Rename pretty columns in data frame for panel view
#'
#' This function renames columns in a data frame for use in a panel view. The
#' column names are modified to include similar labels than the ones from the
#' legend and ensures that information about the time period is being displayed.
#'
#' @param vars <`named list`> Object built using the \code{\link{vars_build}}
#' function.
#' @param dat <`data.frame`> The data frame containing the columns to be
#' renamed.
#' @param lang <`character`> The language to use for translating variable names.
#' Defaults to NULL for no translation
#' @param ... Additional arguments to be passed to the underlying methods.
#'
#' @return A list with data frame with renamed columns and a list of column
#' names that need styling.
#' @export
panel_view_rename_cols <- function(vars, dat, lang = NULL, ...) {
  UseMethod("panel_view_rename_cols", vars)
}

#' @rdname panel_view_rename_cols
#' @export
panel_view_rename_cols.q5 <- function(vars, dat, lang = NULL, ...) {
  # Update column name
  time <- var_get_time(vars$var_left)
  title <- legend_labels(vars, lang = lang, short_threshold = 5)[[1]]$x
  title <- sprintf("%s (%s)", title, time)
  names(dat)[names(dat) == vars$var_left] <- title

  # Prepare the colum names for styling
  title_vars <- structure(title, class = class(vars$var_left))

  return(list(data = dat, title_vars = list(title_vars)))
}

#' @rdname panel_view_rename_cols
#' @export
panel_view_rename_cols.delta <- function(vars, dat, lang = NULL, ...) {
  # Update column name
  time <- var_get_time(vars$var_left)
  vars_sep <- sapply(vars$var_left, var_get_info,
    what = "var_short",
    translate = TRUE, lang = lang
  )
  new_names <-
    c(
      sprintf("%s (%s)", vars_sep, time),
      legend_labels(vars, short_threshold = 5, lang = lang)[[1]]$x
    )

  var_code <- var_get_info(vars$var_left, what = "var_code")
  titles <- names(dat)[grepl(var_code, names(dat))]
  names(dat)[grepl(var_code, names(dat))] <- new_names

  # Prepare the colum names for styling
  title_vars <- structure(new_names[1:2], class = class(vars$var_left))
  variation <- structure(new_names[3], class = "pct")
  title_vars <- list(title_vars, variation)

  # Return
  return(list(data = dat, title_vars = title_vars))
}

#' @rdname panel_view_rename_cols
#' @export
panel_view_rename_cols.bivar <- function(vars, dat, lang = NULL, ...) {
  # Update column name
  time <- var_get_time(unlist(vars))
  vars_sep <- sapply(vars, var_get_info,
    what = "var_short",
    translate = TRUE, lang = lang
  )
  new_names <- sprintf("%s (%s)", vars_sep, time)

  names(dat)[names(dat) %in% vars] <- new_names

  # Prepare the column names for styling
  title_vars <- structure(new_names[1], class = class(vars$var_left))
  title_vars <- list(
    title_vars,
    structure(new_names[2], class = class(vars$var_right))
  )

  # Return
  return(list(data = dat, title_vars = title_vars))
}

#' @rdname panel_view_rename_cols
#' @export
panel_view_rename_cols.delta_bivar <- function(vars, dat, lang = NULL, ...) {
  # Update column name
  time <- lapply(vars, var_get_time)
  vars_sep <- lapply(vars, var_get_info,
    what = "var_short",
    translate = TRUE, lang = lang
  )
  new_names <- mapply(\(var, name, year) {
    sprintf("%s (%s)", name, year)
  }, vars, vars_sep, time, SIMPLIFY = FALSE)

  var_labels <- legend_labels(vars, lang = lang)[[1]]
  new_names[[2]] <- c(new_names[[2]], var_labels$x)
  new_names[[1]] <- c(new_names[[1]], var_labels$y)

  var_codes <- sapply(vars, var_get_info, what = "var_code")
  var_codes <- paste0(var_codes, collapse = "|")

  names(dat)[grepl(var_codes, names(dat))] <- unlist(new_names)

  # Prepare the colum names for styling
  title_vars1 <- structure(new_names$var_left[1:2], class = class(vars$var_left))
  variation1 <- structure(new_names$var_left[3], class = "pct")
  title_vars1 <- list(title_vars1, variation1)

  title_vars2 <- structure(new_names$var_right[1:2], class = class(vars$var_right))
  variation2 <- structure(new_names$var_right[3], class = "pct")
  title_vars2 <- list(title_vars2, variation2)

  title_vars <- c(title_vars1, title_vars2)

  # Return
  return(list(data = dat, title_vars = title_vars))
}

#' @rdname panel_view_rename_cols
#' @export
panel_view_rename_cols.bivar_ldelta_rq3 <- function(vars, dat, lang = NULL, ...) {
  # Update column name
  time <- lapply(vars, var_get_time)
  vars_sep <- lapply(vars, var_get_info,
    what = "var_short",
    translate = TRUE, lang = lang
  )
  new_names <- mapply(\(var, name, year) {
    sprintf("%s (%s)", name, year)
  }, vars, vars_sep, time, SIMPLIFY = FALSE)

  var_labels <- legend_labels(vars, lang = lang)[[1]]
  new_names[[1]] <- c(new_names[[1]], var_labels$y)

  var_codes <- sapply(vars, var_get_info, what = "var_code")
  var_codes <- paste0(var_codes, collapse = "|")

  names(dat)[grepl(var_codes, names(dat))] <- unlist(new_names)

  # Prepare the column names for styling
  title_vars1 <- structure(new_names$var_left[1:2], class = class(vars$var_left))
  variation1 <- structure(new_names$var_left[3], class = "pct")
  title_vars1 <- list(title_vars1, variation1)

  title_vars2 <- structure(new_names$var_right, class = class(vars$var_right))

  title_vars <- c(title_vars1, list(title_vars2))

  # Return
  return(list(data = dat, title_vars = title_vars))
}


#' Apply styling to columns of a DT::datatable based on data type
#'
#' This function applies styling to columns of a table based on their data type.
#'
#' @param var <`character`> One or more column names of `table`. It can be one
#' of the following classes: "pct" for percentages, "dollar" for currency, or
#' anything else for the default styling (none).
#' @param table <`datatable`> A \code{\link[DT]{datatable}} object.
#' @param ... Additional arguments to be passed to the formatting functions.
#'
#' @return A styled table object with formatting applied to the columns based on
#' their data type.
#' @export
panel_view_style_cols <- function(var, table, ...) {
  UseMethod("panel_view_style_cols", var)
}

#' @rdname panel_view_style_cols
#' @export
panel_view_style_cols.pct <- function(var, table, ...) {
  DT::formatPercentage(table, var, digits = 2)
}

#' @rdname panel_view_style_cols
#' @export
panel_view_style_cols.dollar <- function(var, table, ...) {
  DT::formatCurrency(table, var, digits = 0)
}

#' @rdname panel_view_style_cols
#' @export
panel_view_style_cols.count <- function(var, table, ...) {
  DT::formatCurrency(table, var, currency = "", digits = 0)
}

#' @rdname panel_view_style_cols
#' @export
panel_view_style_cols.default <- function(var, table, ...) {
  DT::formatRound(table, var, digits = 2)
}


#' Prepare the panel view text for the given variables.
#'
#' This function returns a panel view text including title and description for
#' given variables, considering the minimum, maximum, mean, and standard deviation
#' of the data points in the input dataset.
#'
#' @param vars <`named list`> Object built using the \code{\link{vars_build}}
#' function.
#' @param df <`reactive character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`.
#' @param dat <`data.frame`> The data frame containing the columns under analysis
#' @param lang <`character`> The language to use for translating the texts.
#' Defaults to NULL for no translation
#' @param ... Additional arguments to be passed to the underlying methods.
#'
#' @return A string containing the panel view text.
#' @export
panel_view_prepare_text <- function(vars, df, dat, lang = NULL, ...) {
  UseMethod("panel_view_prepare_text", vars)
}

#' @rdname panel_view_prepare_text
#' @export
panel_view_prepare_text.q5 <- function(vars, df, dat, lang = NULL, ...) {
  # Title
  time <- var_get_time(vars$var_left)
  title <- legend_labels(vars, lang = lang, short_threshold = 5)[[1]]$x
  title <- sprintf("%s (%s)", title, time)

  colours <- colours_get()$bivar
  title_color <- colours$fill[colours$group == "3 - 1"]

  # Grab the necesary values for the text
  explanation <- var_get_info(vars$var_left, what = "explanation")

  # Get the text for the single left variable
  out <- panel_view_prepare_text_helper(
    df = df,
    var = vars$var_left,
    dat = dat,
    title = title,
    explanation = explanation,
    title_color = title_color,
    lang = lang
  )

  # Return
  return(out)
}

#' @rdname panel_view_prepare_text
#' @export
panel_view_prepare_text.delta <- function(vars, df, dat, lang = NULL, ...) {
  # Titles
  time <- var_get_time(vars$var_left)
  vars_sep <- sapply(vars$var_left, var_get_info,
    what = "var_short",
    translate = TRUE, lang = lang
  )
  new_names <-
    c(
      sprintf("%s (%s)", vars_sep, time),
      legend_labels(vars, short_threshold = 5, lang = lang)[[1]]$x
    )

  var_code <- var_get_info(vars$var_left, what = "var_code")
  titles <- names(dat)[grepl(var_code, names(dat))]

  # Tweak a bit the explanation if it's the variation column
  explanations <- lapply(titles, \(x) {
    explanation <- var_get_info(vars$var_left, what = "explanation")

    if (!grepl("_variation$", x)) {
      return(var_get_info(vars$var_left, what = "explanation"))
    }
    explanation <- var_get_info(vars$var_left, what = "explanation_nodet")
    sprintf("the change in %s between %s and %s", explanation, time[1], time[2])
  })

  # Title colour
  colours <- colours_get()$bivar
  title_color <- colours$fill[colours$group == "3 - 1"]

  # Add classes for each of the columns
  title_vars_1 <- structure(titles[1], class = class(vars$var_left))
  title_vars_2 <- structure(titles[2], class = class(vars$var_left))
  variation <- structure(titles[3], class = "pct")
  title_vars <- list(title_vars_1, title_vars_2, variation)

  # Get the text for every columns
  titles_texts <- mapply(\(title, var, explanation) {
    panel_view_prepare_text_helper(
      df = df,
      var = var,
      dat = dat,
      title = title,
      explanation = explanation,
      title_color = title_color,
      lang = lang
    )
  }, new_names, title_vars, explanations)

  # Return
  return(titles_texts)
}

#' @rdname panel_view_prepare_text
#' @export
panel_view_prepare_text.bivar <- function(vars, df, dat, lang = NULL, ...) {
  # Title
  time <- var_get_time(unlist(vars))
  vars_sep <- sapply(vars, var_get_info,
    what = "var_short",
    translate = TRUE, lang = lang
  )
  new_names <- sprintf("%s (%s)", vars_sep, time)

  # Grab the column names
  var_codes <- c(vars$var_left, vars$var_right)

  # Grab the two colours
  colours <- colours_get()$bivar
  left_color <- colours$fill[colours$group %in% "3 - 1"]
  right_color <- colours$fill[colours$group %in% "1 - 3"]
  title_colours <- c(left_color, right_color)

  # Grab the necesary values for the text
  explanations <- lapply(var_codes, var_get_info, what = "explanation")

  # Get the text for the single left variable
  titles_texts <- mapply(\(title, var, explanation, title_color) {
    panel_view_prepare_text_helper(
      df = df,
      var = var,
      dat = dat,
      title = title,
      explanation = explanation,
      title_color = title_color,
      lang = lang
    )
  }, new_names, var_codes, explanations, title_colours)

  # Return
  return(titles_texts)
}

#' @rdname panel_view_prepare_text
#' @export
panel_view_prepare_text.delta_bivar <- function(vars, df, dat, lang = NULL, ...) {
  # Title
  time <- lapply(vars, var_get_time)
  vars_sep <- lapply(vars, var_get_info,
    what = "var_short",
    translate = TRUE, lang = lang
  )
  new_names <- mapply(\(var, name, year) {
    sprintf("%s (%s)", name, year)
  }, vars, vars_sep, time, SIMPLIFY = FALSE)

  var_labels <- legend_labels(vars, lang = lang)[[1]]
  new_names[[2]] <- c(new_names[[2]], var_labels$x)
  new_names[[1]] <- c(new_names[[1]], var_labels$y)

  # Grab the column names
  var_codes <- sapply(vars, var_get_info, what = "var_code")
  var_codes <- paste0(var_codes, collapse = "|")
  var_codes <- names(dat)[grepl(var_codes, names(dat))]

  # Prepare the colum names for styling
  title_vars1 <- lapply(var_codes[1:3], structure, class = class(vars$var_left))
  class(title_vars1[[3]]) <- "pct"
  title_vars2 <- lapply(var_codes[4:6], structure, class = class(vars$var_right))
  class(title_vars2[[3]]) <- "pct"
  title_vars <- c(title_vars1, title_vars2)

  # Grab the two colours
  colours <- colours_get()$bivar
  left_color <- colours$fill[colours$group %in% "3 - 1"]
  right_color <- colours$fill[colours$group %in% "1 - 3"]
  title_colours <- c(rep(left_color, 3), rep(right_color, 3))

  # Tweak a bit the explanation if it's the variation column
  explanations <- lapply(var_codes, \(x) {
    if (!grepl("_variation$", x)) {
      return(var_get_info(x, what = "explanation"))
    }

    code <- gsub("_variation", "", x)
    exp <- var_get_info(code, what = "explanation")
    times <- var_codes[grepl(code, var_codes)][1:2] |> var_get_time()
    sprintf("the change in %s between %s and %s", exp, times[1], times[2])
  })

  # Get the text for the single left variable
  titles_texts <- mapply(\(title, var, explanation, title_color) {
    panel_view_prepare_text_helper(
      df = df,
      var = var,
      dat = dat,
      title = title,
      explanation = explanation,
      title_color = title_color,
      lang = lang
    )
  }, unlist(new_names), title_vars, explanations, title_colours)

  # Return
  return(titles_texts)
}

#' Prepare Text Helper for Panel View
#'
#' This function creates a formatted HTML text containing statistics for a given
#' variable. It includes the minimum, maximum, mean, and standard deviation. The
#' title is displayed with a custom color, usually depends on wether it is a left
#' or right variable.
#'
#' @param df <`reactive character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`.
#' @param var <`character`> A character representing the variable name in the
#' dataset (e.g. 'housing_tenant') with the right class (pct, dollar, ind, ...).
#' @param dat <`data.frame`> The data frame containing the columns from which
#' to retrieve the values.
#' @param title <`character`> The pretty title of the variable, which must
#' match the column of the pretty table.
#' @param explanation <`character`> A character representing the explanation of
#' the variable. A piece of the `variables` table.
#' @param title_color <`character`> The color code for the title. Default is
#' #73AE80' for the left variable's green.
#' @param lang <`character`> The language to use for translating variable names.
#' Defaults to NULL for no translation
#'
#' @return A character string containing the formatted HTML text with statistics.
panel_view_prepare_text_helper <- function(df, var, dat, title, explanation,
                                           title_color = "#73AE80", lang = NULL) {
  # Title
  out_title <- shiny::h4(
    style = sprintf("color:%s !important", title_color),
    sprintf("%s: %s", title, explanation)
  )

  # Text variables
  min_val <- min(dat[[var]], na.rm = TRUE)
  max_val <- max(dat[[var]], na.rm = TRUE)
  avg <- mean(dat[[var]], na.rm = TRUE)
  std <- stats::sd(dat[[var]], na.rm = TRUE)

  values <- lapply(
    list(min_val, max_val, avg, std),
    \(x) convert_unit(var = var, x = x)
  )
  values <- lapply(values, shiny::strong)

  # Create the text
  text <-
    sprintf(
      paste0(
        "<p>The minimum and maximum values for %s are respectively %s and %s. ",
        "The data points have an average value (mean) of %s. Additionally, ",
        "the standard deviation, which measures the dispersion or spread ",
        "around this mean, is %s. (Approximately two thirds of data points ",
        "lie within one standard deviation of the mean.)</p>"
      ),
      explanation, values[[1]], values[[2]], values[[3]], values[[4]]
    )

  # Bind the title and the text
  out <- paste0(out_title, text)

  # Source
  source <- tryCatch(var_get_info(var, what = "source"), error = function(e) NULL)
  if (!is.null(source)) {
    source_bit <-
      if (source == "Canadian census") {
        date <- var_get_time(var)
        s <- sprintf(
          paste0(
            "The data comes from the %s Canadian census and has ",
            "been retrieved from <a href = 'https://censusma",
            "pper.ca/', target = '_blank'>censusmapper.ca</a> ",
            "using the R <a href = 'https://cran.r-project.org",
            "/web/packages/cancensus/', target = '_blank'>canc",
            "ensus</a> package."
          ),
          date
        )
        # Info on how we created the variable
        census_variables <- get_from_globalenv("census_variables")
        info <- lapply(
          census_variables[census_variables$var_code == var, ],
          unlist
        )
        par_exp <- var_get_parent_info(
          var = var, what = "explanation",
          translate = TRUE, lang = lang
        )
        source_vec <- paste0("<b>", info$vec, "</b> (", info$vec_label, ")",
          collapse = ", "
        )
        v <- if (length(info$vec) > 1) "vectors and their" else "vector and its"
        source_vec <- sprintf("%s %s", source_vec, v)

        total_vec <- paste0("<b>", info$parent_vec, "</b> (", info$parent_vec_label, ")",
          collapse = ", "
        )
        v <- if (length(info$parent_vec) > 1) "parent vectors" else "parent vector"
        total_vec <- sprintf("%s %s", v, total_vec)
        e <- sprintf(
          paste0(
            "To calculate %s, we extract the %s corresponding ",
            "%s. Here, the term 'parent vector' refers to ",
            "the data source that represents %s, which we use ",
            "as a basis to compute %s."
          ),
          explanation, source_vec, total_vec,
          par_exp, explanation
        )

        # Bind two parts
        sprintf("%s %s", s, e)
      } else {
        sprintf("The source of the data is `%s`.", source)
      }
    out <- paste0(out, sprintf("<p>%s</p>", source_bit))
  }

  # Interpolated? If so grab from what andmake the sentence
  inter <- tryCatch(var_get_info(var, what = "interpolated")[[1]],
    error = function(e) NULL
  )
  if (!is.null(inter)) {
    scale_inter <- inter$interpolated_from[inter$df == df]
    if (scale_inter != "FALSE") {
      scales_dictionary <- get_from_globalenv("scales_dictionary")
      scale_inter_str <-
        scales_dictionary$plur[scales_dictionary$scale == scale_inter]

      scale_inter_str <- tolower(cc_t(scale_inter_str, lang = lang))
      inter_str <- sprintf(
        "%s has been spatially interpolated from %s.",
        title, scale_inter_str
      )
      # Bind the title and the text
      out <- paste0(out, sprintf("<p>%s</p>", inter_str))
    }
  }

  # Return
  return(out)
}
