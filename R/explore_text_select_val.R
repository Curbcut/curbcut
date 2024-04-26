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
explore_text_select_val <- function(var, select_id, data, scale, col = "var_left",
                                    time, schemas = NULL, ...) {
  UseMethod("explore_text_select_val", var)
}

#' @describeIn explore_text_select_val Method for data on postgresql
#' @export
explore_text_select_val.postgresql <- function(var, select_id, data, scale, col = "var_left",
                                               time, schemas = NULL, ...) {
  # Get the parent variable
  parent_string <- var_get_info(var, what = "parent_vec")
  if (parent_string == "population") parent_string <- "c_population"
  if (parent_string == "households") parent_string <- "private_households"

  # Which column breaks do we want to use
  col_schema <- match_schema_to_col(data, time = time, col = col, schemas = schemas)
  var_schema <- gsub(col, var, col_schema)
  parent_string_schema <- gsub(col, parent_string, col_schema)

  explore_text_select_val_postgresql(
    var = var, scale = scale, parent_string = parent_string,
    var_schema = var_schema,
    parent_string_schema = parent_string_schema,
    select_id = select_id, data = data, ...
  )
}

#' Method to generate values for the given variable and selection (for data
#' on the postgresql server)
#'
#' This function dispatches to the appropriate value-generating function based on
#' the variable type and returns the resulting values. It is only used when
#' there is a selection, and replaces the value of \code{\link{explore_text_region_val_df}}
#'
#' @param var <`character`> The code of the variable of interest, with its class
#' to dispatch to the right method.
#' @param scale <`character`> The scale of interest.
#' @param parent_string <`character`> The parent string as character ex. c_population
#' @param parent_string_schema <`character`> With the schema on ex. c_population_2021
#' @param var_schema <`character`> With the schema on ex. alp_2021
#' @param select_id <`character`> The selected identifier.
#' @param ... Additional arguments (e.g., data, time) used in specific methods.
#'
#' @return The resulting values
#' @export
explore_text_select_val_postgresql <- function(var, scale, parent_string, var_schema,
                                               parent_string_schema, select_id, ...) {
  UseMethod("explore_text_select_val_postgresql", var)
}

#' @describeIn explore_text_select_val_postgresql Method for pct
#' @export
explore_text_select_val_postgresql.pct <- function(var, scale, parent_string, var_schema,
                                                   parent_string_schema, select_id, ...) {
  explore_text_postgres_val(
    var = var, scale = scale, parent_string = parent_string,
    var_schema = var_schema,
    parent_string_schema = parent_string_schema
  )
}


#' @describeIn explore_text_select_val_postgresql Method for `ind`
#' @param data <`data.frame`> The data frame containing the data.
#' @param lang <`character`> Active language. `"en"` or `"fr"`
#' @export
explore_text_select_val_postgresql.ind <- function(var, scale, parent_string, var_schema,
                                                   parent_string_schema,
                                                   select_id, data, lang = NULL, ...) {
  explore_text_select_val_postgresql_ind(
    var = var, scale = scale, parent_string = parent_string, var_schema = var_schema,
    parent_string_schema = parent_string_schema, select_id = select_id,
    data = data, lang = lang, ...
  )
}

#' Generate values for the given `ind` variable and selection (for posgresql)
#'
#' @param var <`character`> The code of the variable of interest, with its class
#' to dispatch to the right method.
#' @param scale <`character`> The scale of interest.
#' @param parent_string <`character`> The parent string as character ex. c_population
#' @param parent_string_schema <`character`> With the schema on ex. c_population_2021
#' @param var_schema <`character`> With the schema on ex. alp_2021
#' @param data <`data.frame`> The data frame containing the data.
#' @param select_id <`character`> The selected identifier.
#' @param lang <`character`> Active language. `"en"` or `"fr"`
#' @param ... Additional arguments passed to the dispatched function.
#'
#' @return The resulting values
#' @export
explore_text_select_val_postgresql_ind <- function(var, scale, parent_string, var_schema,
                                                   parent_string_schema,
                                                   select_id, data, lang = NULL, ...) {
  UseMethod("explore_text_select_val_postgresql_ind", var)
}

#' @describeIn explore_text_select_val_postgresql_ind Method for scalar
#' @export
explore_text_select_val_postgresql_ind.scalar <- function(var, scale, parent_string, var_schema,
                                                          parent_string_schema, select_id,
                                                          data, lang = NULL, ...) {
  # Create empty vector
  out <- c()

  # Grab the value and its rank
  val <- db_get(
    select = var_schema, from = sprintf("%s_%s", scale, var),
    where = list(ID = select_id)
  )[[1]]
  rank <- findInterval(val, attr(data, "breaks_var_left"))

  # Grab the rank name for the rank
  rank_names <- var_get_info(var = var, what = "rank_name")[[1]]
  out$val <- rank_names[rank]

  # Lower letters
  out$val <- tolower(cc_t(out$val, lang = lang))

  out$num <- val

  # Return
  return(out)
}

#' @describeIn explore_text_select_val_postgresql_ind Method for ordinal
#' @export
explore_text_select_val_postgresql_ind.ordinal <- function(var, scale, parent_string, var_schema,
                                                           parent_string_schema, select_id,
                                                           data, lang = NULL, ...) {
  # Create empty vector
  out <- c()

  # Grab the value and its rank
  val <- db_get(
    select = var_schema, from = sprintf("%s_%s", scale, var),
    where = list(ID = select_id)
  )[[1]]
  rank <- findInterval(val, attr(data, "breaks_var_left"))

  # Grab the rank name for the rank
  rank_names <- var_get_info(var = var, what = "rank_name")[[1]]
  out$val <- rank_names[rank]

  # Lower letters
  out$val <- tolower(cc_t(out$val, lang = lang))

  out$num <- rank

  # Return
  return(out)
}


#' @describeIn explore_text_select_val_postgresql Default method
#' @export
explore_text_select_val_postgresql.default <- function(var, scale, parent_string, var_schema,
                                                       parent_string_schema, select_id, ...) {
  # Create empty vector
  out <- c()

  # Get the data point from the database
  val <- db_get(
    select = var_schema, from = sprintf("%s_%s", scale, var),
    where = list(ID = select_id)
  )[[1]]

  # Add the value for the selection
  out$val <- val

  # Return
  return(out)
}






#' @describeIn explore_text_select_val Method for ondisk data
#' @export
explore_text_select_val.ondisk <- function(var, select_id, data, scale, col = "var_left",
                                           time, schemas = NULL, ...) {
  explore_text_select_val_ondisk(
    var = var, select_id = select_id, data = data,
    scale = scale, col = col, time = time,
    schemas = schemas, ...
  )
}

#' Method to generate values for the given variable and selection (for data ondisk)
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
#' @param col <`character`> Which column of `data` should be selected to grab the
#' value information. Defaults to `var_left`, but could also be `var_right` or
#' `var_left_1` in delta.
#' @param time <`numeric named list`> The `time` at which data is displayed.
#' A list for var_left and var_right. The output of \code{\link{vars_build}}(...)$time.
#' @param schemas <`named list`> Current schema information. The additional widget
#' values that have an impact on which data column to pick. Usually `r[[id]]$schema()`.
#' @param ... Additional arguments passed to the dispatched function.
#'
#' @return The resulting values
#' @export
explore_text_select_val_ondisk <- function(var, select_id, data, scale, col = "var_left",
                                           time, schemas = NULL, ...) {
  UseMethod("explore_text_select_val_ondisk", var)
}

#' @describeIn explore_text_select_val Method for pct
#' @param data_path <`character`> A string representing the path to the
#' directory containing the QS files. Default is "data/".
#' @export
explore_text_select_val_ondisk.pct <- function(var, select_id, data, scale, col = "var_left",
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

#' @describeIn explore_text_select_val_ondisk Method for `ind`
#' @param lang <`character`> Active language. `"en"` or `"fr"`
#' @export
explore_text_select_val_ondisk.ind <- function(var, select_id, data, scale, col = "var_left",
                                               time, schemas = NULL, lang = NULL, ...) {
  explore_text_select_val_ondisk_ind(
    var = var, data = data, select_id = select_id,
    col = col, time = time, lang = lang, schemas = schemas,
    ...
  )
}

#' Generate values for the given `ind` variable and selection (for ondisk data)
#'
#' @param var <`character`> The variable code of the variable for which the
#' values need to be generated. Usually one element of the output of
#' \code{\link{vars_build}}.
#' @param select_id <`character`> The ID of the selected zone.
#' @param data <`data.frame`> A data frame containing the variables and
#' observations. The output of \code{\link{data_get}}.
#' @param scale <`character`> Current scale. The output of
#' \code{\link{update_scale}}.
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
explore_text_select_val_ondisk_ind <- function(var, select_id, data, scale, col = "var_left",
                                               time, schemas = NULL, lang = NULL, val = NULL, ...) {
  UseMethod("explore_text_select_val_ondisk_ind", var)
}

#' @describeIn explore_text_select_val_ondisk_ind Method for `scalar`
#' @export
explore_text_select_val_ondisk_ind.scalar <- function(var, select_id, data, scale, col = "var_left",
                                                      time, schemas = NULL, lang = NULL, val = NULL, ...) {
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

#' @describeIn explore_text_select_val_ondisk_ind Method for `ordinal`
#' @export
explore_text_select_val_ondisk_ind.ordinal <- function(var, select_id, data, scale, col = "var_left",
                                                       time, schemas = NULL, lang = NULL, val = NULL, ...) {
  # Create empty vector
  out <- c()

  # Throw error if the selected ID is not in the data.
  if (is.null(val) & !select_id %in% data$ID) {
    stop(sprintf("`%s` is not in the data.", select_id))
  }

  rank <- if (!is.null(val)) {
    val
  } else {
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

#' @describeIn explore_text_select_val_ondisk Default method
#' @export
explore_text_select_val_ondisk.default <- function(var, select_id, data, scale, col = "var_left",
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
