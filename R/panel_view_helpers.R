#' Prepare table to be shown to user
#'
#' @param vars <`named list`> Object built using the \code{\link{vars_build}}
#' function.
#' @param data <`data.frame`> Data frame containing all the scale and
#' the `var_left` and `var_right`. The output of \code{\link{data_get}}.
#' @param scale <`character`>
#' @param zoom_levels <`named numeric vector`> A named numeric vector of zoom
#' levels. Usually one of the `mzl_x`, or the output of
#' \code{\link{geography_server}}.
#' @param scale <`character`> Current scale. Usually `r[[id]]$scale()`.
#' @param time <`named list`> Named list of time, length 2 (var_left and var_right).
#' Usually `r[[id]]$time()`
#' @param schemas <`named list`> Current schema information. The additional widget
#' values that have an impact on which data column to pick. Usually `r[[id]]$schema()`.
#' @param lang <`character`> The language to use for translating variable names.
#' Defaults to NULL for no translation
#'
#' @return Returns a list of two table. There is 'pretty table' ready to be
#' shown to the user, and another table that is for download.
table_view_prep_table <- function(vars, data, scale, zoom_levels, time, schemas = NULL, lang = NULL) {
  # Reformat data -----------------------------------------------------------

  # Grab only the correct column, and rename it.
  dat <- data
  # Prepare for merge, keep attributes
  prev_attr <- attributes(dat)
  prev_attr <- prev_attr[!names(prev_attr) %in% c(
    "names", "row.names", "class",
    "quintiles", "breaks"
  )]

  # If var_right is in the data, subset it too
  if (sum(grepl("var_right_", names(dat))) > 0) {
    vl_col <- match_schema_to_col(data = dat, time = time, schemas = schemas)
    vl_col <- c(vl_col, "var_left") # Keep also `delta` if present
    vl_col <- vl_col[which(vl_col %in% names(dat))]
    vr_col <- match_schema_to_col(data = dat, time = time, col = "var_right", schemas = schemas)
    vr_col <- c(vr_col, "var_right") # Keep also `delta` if present
    vr_col <- vr_col[which(vl_col %in% names(dat))]
    dat <- dat[c("ID", vl_col, vr_col)]
    # names(dat)[c(2:3)] <- c("var_left", "var_right")
  } else {
    vl_col <- match_schema_to_col(data = dat, time = time, schemas = schemas)
    vl_col <- c(vl_col, "var_left") # Keep also `delta` if present
    vl_col <- vl_col[which(vl_col %in% names(dat))]
    dat <- dat[c("ID", vl_col)]
  }

  # Rename the var_left and var_right
  names(dat) <- gsub("var_left", vars$var_left, names(dat))
  names(dat)[vars$var_left == names(dat)] <- sprintf("%s_variation", vars$var_left)
  names(dat) <- gsub("var_right", vars$var_right, names(dat))
  names(dat)[vars$var_right == names(dat)] <- sprintf("%s_variation", vars$var_right)

  # Bind the `scale` data to the real data
  df_dat <- grab_df_from_bslike(scale)
  default_cols <- c("ID", "name", "name_2", "population", "households")
  default_cols <- names(df_dat)[names(df_dat) %in% default_cols]
  df_dat <- df_dat[default_cols]
  dat <- merge(df_dat, dat, by = "ID")

  # Keep the previous attributes
  for (i in names(prev_attr)) {
    attr(dat, i) <- prev_attr[[i]]
  }

  # Order data by population
  if ("population" %in% names(dat)) {
    dat <- dat[order(dat$population, decreasing = TRUE), ]
  }


  # About the data information ----------------------------------------------

  text <- panel_view_prepare_text(
    vars = vars, scale = scale, dat = dat, time = time,
    schemas = schemas, lang = lang
  )


  # Update column names so it's more 'friendly' -----------------------------

  pretty_dat <- dat

  # Switch name_2 column name depending on `zoom_levels`
  # Switch df to scale as it can cause a bug where the region is the same as the
  # scale. centraide_CT catches both the `centraide` and the `CT` scale.
  which_zl <- which(is_scale_in(names(zoom_levels), scale, vectorized = TRUE))
  names(pretty_dat)[names(pretty_dat) == "name_2"] <-
    if (which_zl == 1) {
      "Scale"
    } else {
      dat$name_2 <- fill_name_2(
        ID_scale = dat$ID,
        scale = scale,
        top_scale = names(zoom_levels)[1]
      )
      # Fill in name_2
      pretty_dat$name_2 <- fill_name_2(
        ID_scale =
          pretty_dat$ID,
        scale = scale,
        top_scale = names(zoom_levels)[1]
      )
      zoom_get_name(names(zoom_levels)[1], lang = lang)
    }

  # Add a new column 'Scale' to be clear which scale the user is seeing
  if (which_zl > 1) {
    pretty_dat$Scale <- zoom_get_name(names(zoom_levels)[which_zl], lang = lang)
    first_cols <- c("ID", "Scale")
    rest_cols <- names(pretty_dat)[!names(pretty_dat) %in% first_cols]
    pretty_dat <- pretty_dat[, c(first_cols, rest_cols)]
  } else {
    pretty_dat$Scale <- sapply(pretty_dat$Scale, cc_t, lang = lang)
  }

  # Start with the basic usual ones
  to_sentence_vector <-
    names(pretty_dat) %in% c("name", "population", "households", "Scale")
  names(pretty_dat)[to_sentence_vector] <-
    sapply(s_sentence(names(pretty_dat)[to_sentence_vector]), cc_t, lang = lang)

  # Take out one of Name or ID if they are identical
  if (identical(pretty_dat$Name, pretty_dat$ID)) {
    pretty_dat <- pretty_dat[names(pretty_dat) != "Name"]
  }

  # Depending on the class of `vars`, update the other column names and
  # out as a datatable.
  # Keep the previous attributes
  for (i in names(prev_attr)) {
    attr(pretty_dat, i) <- prev_attr[[i]]
  }
  pretty_dat <-
    panel_view_rename_cols(
      vars = vars, dat = pretty_dat, lang = lang,
      scale = scale, time = time, schemas = schemas
    )

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
#' @param time <`named list`> Named list of time, length 2 (var_left and var_right).
#' Usually `r[[id]]$time()`
#' @param schemas <`named list`> Current schema information. The additional widget
#' values that have an impact on which data column to pick. Usually `r[[id]]$schema()`.
#' @param lang <`character`> The language to use for translating variable names.
#' Defaults to NULL for no translation
#' @param ... Additional arguments to be passed to the underlying methods.
#'
#' @return A list with data frame with renamed columns and a list of column
#' names that need styling.
#' @export
panel_view_rename_cols <- function(vars, dat, time, schemas = NULL, lang = NULL, ...) {
  UseMethod("panel_view_rename_cols", vars)
}

#' @rdname panel_view_rename_cols
#' @export
panel_view_rename_cols.q5 <- function(vars, dat, time, schemas = NULL, lang = NULL, ...) {
  # Construct column name
  title <- legend_labels(vars, lang = lang, short_threshold = 5)[[1]]$x
  title <- sprintf("%s (%s)", title, time$var_left)

  # Rename column
  col <- match_schema_to_col(
    data = dat, time = time, col = vars$var_left,
    data_schema = attr(dat, "schema_var_left"), schemas = schemas
  )
  names(dat)[names(dat) == col] <- title

  # Prepare the colum names for styling
  title_vars <- structure(title, class = class(vars$var_left))

  return(list(data = dat, title_vars = list(title_vars)))
}

#' @rdname panel_view_rename_cols
#' @param time <`named list`>
#' @export
panel_view_rename_cols.delta <- function(vars, dat, time, schemas = NULL, lang = NULL, ...) {
  # Titles
  vars_sep <- var_get_info(
    var = vars$var_left,
    what = "var_short",
    translate = TRUE, lang = lang
  )
  new_names <-
    c(
      sprintf("%s (%s)", vars_sep, time$var_left),
      legend_labels(vars, short_threshold = 5, lang = lang, time = time)[[1]]$x
    )

  var_code <- var_get_info(vars$var_left, what = "var_code")
  titles <- names(dat)[grepl(var_code, names(dat))]
  names(dat)[grepl(var_code, names(dat))] <- new_names

  # Prepare the colum names for styling
  title_vars <- lapply(new_names[1:2], \(x) structure(x, class = class(vars$var_left)))
  variation <- structure(new_names[3], class = "pct")
  title_vars <- c(title_vars, list(variation))

  # Return
  return(list(data = dat, title_vars = title_vars))
}

#' @rdname panel_view_rename_cols
#' @export
panel_view_rename_cols.bivar <- function(vars, dat, time, schemas = NULL, lang = NULL, ...) {
  # Construct column name
  vars_sep <- sapply(vars, var_get_info,
    what = "var_short",
    translate = TRUE, lang = lang
  )
  new_names <- sprintf("%s (%s)", vars_sep, time)

  # Rename column
  col <- lapply(c("var_left", "var_right"), \(l_r) {
    match_schema_to_z_col(
      data = dat, time = time, col = vars[[l_r]], vl_vr = l_r,
      schemas = schemas
    )
  })

  names(dat)[names(dat) %in% col] <- new_names

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
panel_view_rename_cols.delta_bivar <- function(vars, dat, time, schemas = NULL, lang = NULL, ...) {
  # Update column name
  vars_sep <- sapply(vars, var_get_info,
    what = "var_short",
    translate = TRUE, lang = lang
  )
  new_names <- mapply(\(v, t, x_y) {
    c(
      sprintf("%s (%s)", v, t),
      legend_labels(vars, short_threshold = 5, lang = lang, time = time)[[1]][[x_y]]
    )
  }, vars_sep, time, c("y", "x"), SIMPLIFY = FALSE) |> unlist()

  var_codes <- sapply(vars, var_get_info, what = "var_code")
  var_codes <- paste0(var_codes, collapse = "|")
  names(dat)[grepl(var_codes, names(dat))] <- unlist(new_names)

  # Prepare the colum names for styling
  title_vars1 <- lapply(
    new_names[1:2],
    \(x) structure(x, class = class(vars$var_left))
  )
  variation1 <- structure(new_names[[3]], class = "pct")
  title_vars1 <- c(unname(title_vars1), list(variation1))

  title_vars2 <- lapply(
    new_names[4:5],
    \(x) structure(x, class = class(vars$var_right))
  )
  variation2 <- structure(new_names[[6]], class = "pct")
  title_vars2 <- c(unname(title_vars2), list(variation2))

  title_vars <- c(title_vars1, title_vars2)

  # Return
  return(list(data = dat, title_vars = title_vars))
}

#' @rdname panel_view_rename_cols
#' @export
panel_view_rename_cols.bivar_ldelta_rq3 <- function(vars, dat, time, schemas = NULL, lang = NULL, ...) {
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
  title_vars1 <- lapply(
    new_names$var_left[1:2],
    \(x) structure(x, class = class(vars$var_left))
  )
  variation1 <- structure(new_names$var_left[3], class = "pct")
  title_vars1 <- c(title_vars1, list(variation1))

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
#' @param scale <`character`> Current scale, e.g. `"DA"`.
#' @param dat <`data.frame`> The data frame containing the columns under analysis
#' @param time <`named list`> Object built using the \code{\link{vars_build}}
#' function. It contains the time for both var_left and var_right variables.
#' @param schemas <`named list`> Current schema information. The additional widget
#' values that have an impact on which data column to pick. Usually `r[[id]]$schema()`.
#' @param lang <`character`> The language to use for translating the texts.
#' Defaults to NULL for no translation
#' @param ... Additional arguments to be passed to the underlying methods.
#'
#' @return A string containing the panel view text.
#' @export
panel_view_prepare_text <- function(vars, scale, dat, time, schemas = NULL,
                                    lang = NULL, ...) {
  UseMethod("panel_view_prepare_text", vars)
}

#' @describeIn panel_view_prepare_text The method for q5.
#' @export
panel_view_prepare_text.q5 <- function(vars, scale, dat, time, schemas = NULL,
                                       lang = NULL, ...) {
  # Title
  title <- legend_labels(vars, lang = lang, short_threshold = 5)[[1]]$x
  title <- sprintf("%s (%s)", title, time$var_left)

  colours <- colours_get()$bivar
  title_color <- colours$fill[colours$group == "3 - 1"]

  # Column name
  col <- match_schema_to_z_col(
    data = dat, time = time, col = vars$var_left,
    vl_vr = "var_left", schemas = schemas
  )
  class(col) <- class(vars$var_left)

  # Grab the necesary values for the text
  explanation <- var_get_info(vars$var_left,
    what = "explanation",
    translate = TRUE, lang = lang,
    schemas_col = schemas$var_left
  )

  # Get the text for the single left variable
  out <- panel_view_prepare_text_helper(
    scale = scale,
    var = col,
    dat = dat,
    title = title,
    explanation = explanation,
    title_color = title_color,
    time_col = time$var_left,
    schemas_col = schemas$var_left,
    lang = lang
  )

  # Return
  return(out)
}

#' @describeIn panel_view_prepare_text The method for delta.
#' @export
panel_view_prepare_text.delta <- function(vars, scale, dat, time, schemas = NULL,
                                          lang = NULL, ...) {
  # Titles
  vars_sep <- var_get_info(
    var = vars$var_left,
    what = "var_short",
    translate = TRUE, lang = lang, schemas_col = NULL
  )
  new_names <-
    c(
      sprintf("%s (%s)", vars_sep, time$var_left),
      legend_labels(vars, short_threshold = 5, lang = lang, time = time)[[1]]$x
    )

  var_code <- var_get_info(vars$var_left, what = "var_code")
  titles <- names(dat)[grepl(var_code, names(dat))]

  # Tweak a bit the explanation if it's the variation column
  explanations <- lapply(titles, \(x) {
    explanation <- var_get_info(vars$var_left,
      what = "explanation",
      translate = TRUE, lang = lang
    )

    if (!grepl("_variation$", x)) {
      return(var_get_info(vars$var_left,
        what = "explanation",
        translate = TRUE, lang = lang
      ))
    }
    explanation <- var_get_info(vars$var_left,
      what = "explanation_nodet",
      translate = TRUE, lang = lang
    )
    sprintf(
      cc_t("the change in %s between %s and %s", lang = lang),
      explanation, time$var_left[1], time$var_left[2]
    )
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
  titles_texts <- mapply(\(title, var, explanation, t) {
    panel_view_prepare_text_helper(
      scale = scale,
      var = var,
      dat = dat,
      title = title,
      explanation = explanation,
      title_color = title_color,
      lang = lang,
      time_col = t
    )
  }, new_names, title_vars, explanations, c(time$var_left, ""))

  # Return
  return(titles_texts)
}

#' @describeIn panel_view_prepare_text The method for bivar.
#' @export
panel_view_prepare_text.bivar <- function(vars, scale, dat, time, schemas = NULL,
                                          lang = NULL, ...) {
  # Title
  vars_sep <- sapply(vars, var_get_info,
    what = "var_short",
    translate = TRUE, lang = lang
  )
  new_names <- sprintf(
    "%s (%s)",
    vars_sep[c("var_left", "var_right")],
    unlist(time[c("var_left", "var_right")])
  )

  # Grab the column names
  var_codes <- list(vars$var_left, vars$var_right)

  # Grab the two colours
  colours <- colours_get()$bivar
  left_color <- colours$fill[colours$group %in% "3 - 1"]
  right_color <- colours$fill[colours$group %in% "1 - 3"]
  title_colours <- c(left_color, right_color)

  # Grab the necesary values for the text
  explanations <- lapply(c("var_left", "var_right"), \(x) {
    var_get_info(vars[[x]],
      what = "explanation",
      translate = TRUE, lang = lang,
      schemas_col = schemas[[x]]
    )
  })

  # Get the text for the single left variable
  titles_texts <- mapply(\(title, var, explanation, title_color, time_col, l_r) {
    # Column name
    col <- match_schema_to_z_col(
      data = dat, time = time, col = vars[[l_r]],
      vl_vr = l_r, schemas = schemas
    )
    class(col) <- class(var)

    panel_view_prepare_text_helper(
      scale = scale,
      var = col,
      dat = dat,
      title = title,
      explanation = explanation,
      title_color = title_color,
      lang = lang,
      time_col = time_col,
      schemas_col = schemas[[vars[[l_r]]]]
    )
  }, new_names, var_codes, explanations, title_colours, time, c("var_left", "var_right"))

  # Return
  return(titles_texts)
}

#' @describeIn panel_view_prepare_text The method for delta_bivar.
#' @export
panel_view_prepare_text.delta_bivar <- function(vars, scale, dat, time, schemas = NULL,
                                                lang = NULL, ...) {
  # Titles
  vars_sep <- sapply(vars, var_get_info,
    what = "var_short",
    translate = TRUE, lang = lang
  )
  new_names <- mapply(\(v, t) {
    c(
      sprintf("%s (%s)", v, t),
      legend_labels(vars, short_threshold = 5, lang = lang, time = time)[[1]]$x
    )
  }, vars_sep, time, SIMPLIFY = FALSE) |> unlist()

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
      return(var_get_info(x,
        what = "explanation",
        translate = TRUE, lang = lang
      ))
    }

    code <- gsub("_variation", "", x)
    exp <- var_get_info(code,
      what = "explanation",
      translate = TRUE, lang = lang
    )

    v <- gsub("_variation$", "", x)
    t <- time[[which(v == vars)]]

    sprintf(
      cc_t("the change in %s between %s and %s", lang = lang),
      exp, t[1], t[2]
    )
  })

  # Get the text for every columns
  titles_texts <- mapply(
    \(title, var, explanation, title_color, t) {
      panel_view_prepare_text_helper(
        scale = scale,
        var = var,
        dat = dat,
        title = title,
        explanation = explanation,
        title_color = title_color,
        lang = lang,
        time_col = t
      )
    }, new_names, title_vars, explanations, title_colours,
    c(time$var_left, "", time$var_right, "")
  )

  # Return
  return(titles_texts)
}

#' @describeIn panel_view_prepare_text The default method.
#' @export
panel_view_prepare_text.default <- function(vars, scale, dat, time, schemas = NULL,
                                            lang = NULL, ...) {
  return(list(""))
}

#' Prepare Text Helper for Panel View
#'
#' This function creates a formatted HTML text containing statistics for a given
#' variable. It includes the minimum, maximum, mean, and standard deviation. The
#' title is displayed with a custom color, usually depends on wether it is a left
#' or right variable.
#'
#' @param scale <`character`>
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
#' @param time_col <`vector`> Vector of dates. Usually `r[[id]]$time()`, with
#' `var_left` or`var_right` subset.
#' @param schemas_col <`named list`> One subset of the current schema information.
#' The additional widget values that have an impact on which data column to pick.
#' Usually `r[[id]]$schema()`, with `var_left` or`var_right` subset.
#' @param lang <`character`> The language to use for translating variable names.
#' Defaults to NULL for no translation
#'
#' @return A character string containing the formatted HTML text with statistics.
panel_view_prepare_text_helper <- function(scale, var, dat, title, explanation,
                                           title_color = "#73AE80", time_col,
                                           schemas_col = NULL, lang = NULL) {
  # Title
  out_title <-
    if (!grepl("<ul>", explanation)) {
      shiny::h4(
        style = sprintf("color:%s !important", title_color),
        sprintf("%s: %s", title, explanation)
      )
    } else {
      shiny::h4(style = sprintf("color:%s !important", title_color), title)
    }

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

  # If there are bullet points, switch to generic
  if (grepl("<ul>", explanation)) {
    explanation <- "this value"
  }

  # Create the text
  text <-
    sprintf(
      cc_t(
        "<p>The minimum and maximum values for %s are respectively %s and %s. ",
        "The data points have an average value (mean) of %s. Additionally, ",
        "the standard deviation, which measures the dispersion or spread ",
        "around this mean, is %s. (Approximately two thirds of data points ",
        "lie within one standard deviation of the mean.)</p>",
        lang = lang
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
        s <- sprintf(
          cc_t(
            "The data comes from the %s Canadian census and has ",
            "been retrieved from <a href = 'https://censusma",
            "pper.ca/', target = '_blank'>censusmapper.ca</a> ",
            "using the R <a href = 'https://cran.r-project.org",
            "/web/packages/cancensus/', target = '_blank'>canc",
            "ensus</a> package.",
            lang = lang
          ),
          time_col
        )
        # Info on how we created the variable
        census_variables <- get_from_globalenv("census_variables")
        if (grepl(sprintf("%s$", time_col), var)) {
          var_time <- var
        } else {
          var_time <- sprintf("%s_%s", var, time_col)
        }
        info <- lapply(
          census_variables[census_variables$var_code == var_time, ],
          unlist
        )
        par_exp <- var_get_parent_info(
          var = var, what = "explanation",
          translate = TRUE, lang = lang, schemas_col = schemas_col
        )
        source_vec <- paste0("<b>", info$vec, "</b> (", info$vec_label, ")",
          collapse = ", "
        )
        v <- if (length(info$vec) > 1) "vectors and their" else "vector and its"
        v <- cc_t(v, lang = lang)
        source_vec <- cc_t("{source_vec} {v}", lang = lang)

        total_vec <- paste0("<b>", info$parent_vec, "</b> (", info$parent_vec_label, ")",
          collapse = ", "
        )
        v <- if (length(info$parent_vec) > 1) "parent vectors" else "parent vector"
        v <- cc_t(v, lang = lang)
        total_vec <- sprintf("%s %s", v, total_vec)
        e <- sprintf(
          cc_t(
            "To calculate %s, we extract the %s corresponding ",
            "%s. Here, the term 'parent vector' refers to ",
            "the data source that represents %s, which we use ",
            "as a basis to compute %s.",
            lang = lang
          ),
          explanation, source_vec, total_vec,
          par_exp, explanation
        )

        # Bind two parts
        sprintf("%s %s", s, e)
      } else {
        sprintf(
          cc_t("The source of the data is '%s'.", lang = lang),
          cc_t(source, lang = lang)
        )
      }
    out <- paste0(out, sprintf("<p>%s</p>", source_bit))
  }

  # Interpolated? If so grab from what andmake the sentence
  inter <- tryCatch(var_get_info(var, what = "interpolated")[[1]],
    error = function(e) NULL
  )
  if (!is.null(inter)) {
    scale_inter <- inter$interpolated_from[inter$scale == scale]
    if (scale_inter != "FALSE") {
      scales_dictionary <- get_from_globalenv("scales_dictionary")
      scale_inter_str <-
        scales_dictionary$plur[scales_dictionary$scale == scale_inter]
      if (length(scale_inter_str) == 0) scale_inter_str <- scale_inter

      scale_inter_str <- tolower(cc_t(scale_inter_str, lang = lang))
      inter_str <- sprintf(
        cc_t("%s has been spatially interpolated from %s.", lang = lang),
        title, scale_inter_str
      )
      # Bind the title and the text
      out <- paste0(out, sprintf("<p>%s</p>", inter_str))
    }
  }

  # Return
  return(out)
}
