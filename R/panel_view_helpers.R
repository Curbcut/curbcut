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
  # Take out the `q3`, `q5` and `group`
  dat <- data
  dat <- dat[!grepl("_q3$|_q5$|group$", names(dat))]

  # Add `variation` if it is multi-year
  vars_ <- lapply(vars, \(x) {
    if (length(x) == 1) return(x)

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

  # Update column names (More user friendly)
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
  if (identical(pretty_dat$Name, pretty_dat$ID))
    pretty_dat <- pretty_dat[names(pretty_dat) != "Name"]

  # Depending on the class of `vars`, update the other column names and
  # out as a datatable.
  pretty_dat <-
    panel_view_rename_cols(vars = vars, dat = pretty_dat, lang = lang, df = df)

  # Add population and households to the title vars so they also are formatted
  pretty_dat$title_vars <- c(pretty_dat$title_vars,
                             list(structure(cc_t("Population", lang = lang), class = "count")),
                             list(structure(cc_t("Households", lang = lang), class = "count")))

  return(list(pretty_data = pretty_dat$data,
              data = dat,
              title_vars = pretty_dat$title_vars))
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

  # # Update the column styles
  # var <- vars$var_left
  # table <- DT::datatable(dat)
  # dat <- panel_view_style_cols(var = var, table = table, title = title)

  # Prepare the colum names for styling
  title_vars <- structure(title, class = class(vars$var_left))

  return(list(data = dat, title_vars = list(title_vars)))
}

#' @rdname panel_view_rename_cols
#' @export
panel_view_rename_cols.delta <- function(vars, dat, lang = NULL, ...) {

  # Update column name
  time <- var_get_time(vars$var_left)
  vars_sep <- sapply(vars$var_left, var_get_info, what = "var_short",
                     translate = TRUE, lang = lang)
  new_names <-
    c(sprintf("%s (%s)", vars_sep, time),
      legend_labels(vars, short_threshold = 5, lang = lang)[[1]]$x)

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
  vars_sep <- sapply(vars, var_get_info, what = "var_short",
                     translate = TRUE, lang = lang)
  new_names <- sprintf("%s (%s)", vars_sep, time)

  names(dat)[names(dat) %in% vars] <- new_names

  # Prepare the column names for styling
  title_vars <- structure(new_names[1], class = class(vars$var_left))
  title_vars <- list(title_vars,
                     structure(new_names[2], class = class(vars$var_right)))

  # Return
  return(list(data = dat, title_vars = title_vars))
}

#' @rdname panel_view_rename_cols
#' @export
panel_view_rename_cols.delta_bivar <- function(vars, dat, lang = NULL, ...) {

  # Update column name
  time <- lapply(vars, var_get_time)
  vars_sep <- lapply(vars, var_get_info, what = "var_short",
                     translate = TRUE, lang = lang)
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
  vars_sep <- lapply(vars, var_get_info, what = "var_short",
                     translate = TRUE, lang = lang)
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
