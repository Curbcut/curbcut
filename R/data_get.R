#' Retrieve data from a SQLite database table using SQL syntax
#'
#' This function retrieves data from a SQLite database table using SQL syntax.
#' It requires an established connection to the database, which should be stored
#' in the global environment with a specific naming convention: combination of
#' the region, the scale, and `_conn`, e.g. "CMA_DA_conn" would be a valid
#' SQLite connection.
#'
#' @param var <`character`> A string specifying the name of the table to retrieve
#' data from. Single variable (year) = single table. e.g. `housing_tenant_2016`
#' @param df <`character`> A string specifying the name of the database to retrieve
#' data from. Combination of the region and the scale, e.g. `CMA_DA`.
#' @param select A character vector specifying the names of the columns to
#' select from the table. Default is "*", which means all columns will be
#' selected. If select does not contain the "ID" column, it will be added to the query.
#'
#' @return A data.frame object with the selected data from the specified table.
data_get_sql <- function(var, df, select = "*") {
  if (select != "*") {
    if (!"ID" %in% select) select <- c("ID", select)
    select <- paste0(select, collapse = ", ")
  }

  # Grab connection from the .GlobalEnv
  conn <- paste0(df, "_conn")
  if (is.null(get0(conn, envir = .GlobalEnv))) {
    stop(glue::glue_safe(
      "Connection to the sqlite database `{conn}` does not ",
      "exist in the global environment."
    ))
  }

  # Get from the sqlite connection
  do.call(DBI::dbGetQuery, list(
    as.name(conn),
    glue::glue_safe("SELECT {select} FROM {var} ORDER BY ID")
  ))
}

#' Retrieve data from a QS file based on variable and scale
#'
#' This function takes in a variable code and the scale to retrieve data from a
#' QS file. The function constructs the file path based on the input, then reads
#' the data using the qs package's \code{\link[qs]{qread}} function.
#'
#' @param var <`character`> A string specifying the name of the table to
#' retrieve data from. Single variable = single table. e.g. `housing_tenant`
#' @param scale <`character`> A string specifying the scale at which to retrieve
#' data, corresponding to a path on disk, e.g. `DA` or `CSD`.
#' @param data_path <`character`> A string representing the path to the
#' directory containing the QS files. Default is "data/".
#'
#' @return A data.frame object with the selected data from the specified table.
data_get_qs <- function(var, scale, data_path = get_data_path()) {

  # Construct the file path
  path <- sprintf("%s%s/%s.qs", data_path, scale, var)

  # Read the data
  qs::qread(path)
}

#' Calculate the percentage change between two variables over two years
#'
#' This function takes two variables representing the same quantity measured two
#' years apart and calculates the percentage change between the two values.
#'
#' @param var <`character vector`> A var_code. The variable to get data for.
#' @param time_col <`character vector`> A character vector of length 2. The
#' two years for which the delta should be calculated.
#' @param df <`character`> A string specifying the name of the database to retrieve
#' data from. Combination of the region and the scale, e.g. `CMA_DA`.
#' @param data_path <`character`> A string representing the path to the directory
#' containing the QS files. Default is "data/".
#'
#' @return A data frame with the following columns: ID, var_1, var_2, and var.
#' `ID` is the ID column from the original data, `var_1` and `var_2` are the
#' values of the two variables being compared, and `var` is the percentage
#' change between the two variables.
data_get_delta <- function(var, time_col, scale, data_path = get_data_path()) {

  # Retrieve
  data <- data_get_qs(var, scale, data_path = data_path)

  # Calculate breaks for the right columns
  cols <- match_schema_to_col(data = data, time = time_col, col = var)
  keep_cols <- c("ID", cols, attr(data, "breaks_var")) # keep the breaks_var and use it to calculate breaks
  data <- data[unique(keep_cols)]

  # Append breaks
  data <- data_append_breaks(var = var,
                             data = data,
                             q3_q5 = "q5",
                             rename_col = "var_left")
  data <- data$data

  # Keep columns of the two years
  cols <- match_schema_to_col(data = data, time = time_col, col = "var_left")
  data <- data[c("ID", grep(paste0(cols, collapse = "|"), names(data), value = TRUE))]

  # Calculate the value
  data$var_left <- (data[[3]] - data[[2]]) / data[[2]]
  data$var_left <- replace(data$var_left, is.na(data$var_left), NA)
  data$var_left <- replace(data$var_left, is.infinite(data$var_left), NA)

  # Return
  return(data)
}

#' Get data
#'
#' This function retrieves data from QS files on disk or an SQLite database
#' using the appropriate method based on the class of the input vars object.
#' vars should be a named list with a class, built using the
#' \code{\link{vars_build}} function. Depending on the class of vars, different
#' methods will be used to retrieve and process the data.
#'
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link{vars_build}} function. The class of the vars object is
#' used to determine how to grab de data and output it.
#' @param scale <`character`> The scale of the data to be retrieved, e.g. `CSD`.
#' The output of \code{\link{update_scale}}.
#' @param region <`character vector`> A vector of IDs with which to filter the
#' retrieved data for a specific region, probably retrieved from
#' `regions_dictionary$scales`.
#' @param scales_as_DA <`character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By
#' default, their colour will be the one of their DA.
#' @param data_path <`character`> A string representing the path to the
#' directory containing the QS files. Default is "data/".
#' @param ... Additional arguments passed to methods.
#'
#' @return A dataframe containing the data according to the class of `vars`,
#' with an ID column, one column per year of data, and one `group` column per
#' year of data.
#' @export
data_get <- function(vars, scale, region,
                     scales_as_DA = c("building", "street"),
                     data_path = get_data_path(), ...) {
  UseMethod("data_get", vars)
}

#' @describeIn data_get The method for q5.
#' @export
data_get.q5 <- function(vars, scale, region = NULL,
                        scales_as_DA = c("building", "street"),
                        data_path = get_data_path(), ...) {

  # Treat certain scales as DA
  scale <- treat_to_DA(scales_as_DA = scales_as_DA, scale = scale)

  # Get data
  data <- data_get_qs(vars$var_left, scale = scale, data_path = data_path)

  # Filter to region
  data <- filter_region(data = data, scale = scale, region = region)

  # Append breaks
  data <- data_append_breaks(var = vars$var_left,
                             data = data,
                             q3_q5 = "q5",
                             rename_col = "var_left")

  # Return output
  return(data$data)

}

#' @describeIn data_get The method for bivar.
#' @export
data_get.bivar <- function(vars, scale, scales_as_DA = c("building", "street"),
                           data_path = get_data_path(), ...) {
  # Treat certain scales as DA
  scale <- treat_to_DA(scales_as_DA = scales_as_DA, scale = scale)

  # Get var_left and var_right data
  vl <- data_get_qs(vars$var_left, scale = scale, data_path = data_path)
  vr <- data_get_qs(vars$var_right, scale = scale, data_path = data_path)

  # Append breaks
  all_data <- mapply(
    \(var, data, rename_col) {
      data_append_breaks(
        var = var, data = data, q3_q5 = "q3",
        rename_col = rename_col
      )
    }, c(vars$var_left, vars$var_right),
    list(vl, vr),
    c("var_left", "var_right"),
    SIMPLIFY = FALSE
  )

  # Grab all the time of the var_left (which are going to be the possible
  # value if `time`, as time usually follows the left variable)
  time_regex <- unique(all_data[[1]]$attr$schema_var_left$time)
  possible_vl_times <- grep(time_regex, names(all_data[[1]]$data), value = TRUE)
  possible_vl_times <- s_extract(time_regex, possible_vl_times)

  # Keep left and right breaks
  breaks_vl <- attr(all_data[[1]]$data, "breaks_var_left")
  breaks_vr <- attr(all_data[[2]]$data, "breaks_var_right")

  # Merge
  data <- merge(all_data[[1]]$data, all_data[[2]]$data, by = "ID", all = TRUE)

  # Re-add breaks
  attr(data, "breaks_var_left") <- breaks_vl
  attr(data, "breaks_var_right") <- breaks_vr

  # Re-add the attributes
  for (i in names(all_data[[1]]$attr)) {
    attr(data, i) <- all_data[[1]]$attr[[i]]
  }
  for (i in names(all_data[[2]]$attr)) {
    attr(data, i) <- all_data[[2]]$attr[[i]]
  }

  # Make the group columns, with the years (group_2016, group_2021, ...)
  for (i in possible_vl_times) {
    vr_year <- var_closest_year(vars$var_right, i)$closest_year
    out <- paste(data[[sprintf("var_left_%s_q3", i)]],
                 data[[sprintf("var_right_%s_q3", vr_year)]],
                 sep = " - ")
    data[[sprintf("group_%s", i)]] <- out
  }

  # Return
  return(data)
}

#' @describeIn data_get The method for delta.
#' @param time <`named list`> Object built using the \code{\link{vars_build}}
#' function. It contains the time for both var_left and var_right variables.
#' @export
data_get.delta <- function(vars, scale, region, scales_as_DA = c("building", "street"),
                           data_path = get_data_path(), time, ...) {
  data_get_delta_fun(vars = vars, scale = scale, region = region,
                     scales_as_DA = scales_as_DA, data_path = data_path,
                     time = time, ...)
}

#' @title Inner function to get data based on the type of `vars`
#'
#' @description This function dispatches the data retrieval based on the class
#' of the `vars` object.
#'
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link{vars_build}} function.
#' @param scale <`character`> The scale at which the user is on.
#' @param scales_as_DA <`character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#' @param time <`named list`> Object built using the \code{\link{vars_build}}
#' function. It contains the time for both var_left and var_right variables.
#' @param data_path <`character`> A string representing the path to the directory
#' containing the QS files. Default is "data/".
#' @param ... Additional arguments passed to methods.
#'
#' @seealso \code{\link{data_get.delta}}
data_get_delta_fun <- function(vars, scale, region, scales_as_DA = c("building", "street"),
                               data_path = get_data_path(), time, ...) {
  UseMethod("data_get_delta_fun", vars)
}

#' @describeIn data_get_delta_fun The method for scalar variables.
data_get_delta_fun.scalar <- function(vars, scale, region, scales_as_DA = c("building", "street"),
                                      data_path = get_data_path(), time, ...) {
  # Treat certain scales as DA
  scale <- treat_to_DA(scales_as_DA = scales_as_DA, scale = scale)

  # Get data
  data <- data_get_delta(var = vars$var_left, time_col = time$var_left,
                         scale = scale, data_path = data_path)

  # Filter to region
  data <- filter_region(data = data, scale = scale, region = region)

  # Grab the breaks in the data
  breaks <- breaks_delta(vars = vars, scale = scale, character = FALSE, data = data)

  # Add the breaks attribute
  attr(data, "breaks_var_left") <- breaks

  # Add the `group` for the map colouring
  data$var_left_q5 <- 5
  data$var_left_q5[data$var_left < breaks[5]] <- 4
  data$var_left_q5[data$var_left < breaks[4]] <- 3
  data$var_left_q5[data$var_left < breaks[3]] <- 2
  data$var_left_q5[data$var_left < breaks[2]] <- 1
  data$var_left_q5[is.na(data$var_left)] <- NA
  data$group <- as.character(data$var_left_q5)

  # Return
  return(data)
}

#' @describeIn data_get_delta_fun The method for ordinal variables.
data_get_delta_fun.ordinal <- function(vars, scale, region, scales_as_DA = c("building", "street"),
                                       data_path = get_data_path(), time, ...) {
  # Treat certain scales as DA
  scale <- treat_to_DA(scales_as_DA = scales_as_DA, scale = scale)

  # Retrieve
  data <- data_get_delta(
    var = vars$var_left, scale = scale,
    data_path = data_path
  )
  names(data) <- c("ID", "var_left_1", "var_left_2", "var_left")

  # var_left_q5 will go off of bins change. 0 bin change vs 1 bin change vs multiple
  # bin changes.
  var_left_binchange <- data$var_left_2 - data$var_left_1

  # Add the `group` for the map colouring
  data$var_left_q5 <- 5
  data$var_left_q5[var_left_binchange == 1] <- 4
  data$var_left_q5[var_left_binchange == 0] <- 3
  data$var_left_q5[var_left_binchange == -1] <- 2
  data$var_left_q5[var_left_binchange < -1] <- 1
  data$var_left_q5[is.na(data$var_left)] <- NA
  data$group <- as.character(data$var_left_q5)

  # Return
  return(data)
}

#' @describeIn data_get The method for bivar.
#' @export
data_get.delta_bivar <- function(vars, scale, scales_as_DA = c("building", "street"),
                                 data_path = get_data_path(), ...) {
  # Treat certain scales as DA
  scale <- treat_to_DA(scales_as_DA = scales_as_DA, scale = scale)

  # Retrieve
  data_vl <- data_get_delta(
    var = vars$var_left, scale = scale,
    data_path = data_path
  )
  names(data_vl) <- c("ID", "var_left_1", "var_left_2", "var_left")
  data_vr <- data_get_delta(
    var = vars$var_right, scale = scale,
    data_path = data_path
  )[-1]
  names(data_vr) <- c("var_right_1", "var_right_2", "var_right")
  data <- cbind(data_vl, data_vr)

  # Add the `group` for the map colouring
  data$var_left_q3 <- ntile(data$var_left, 3)
  data$var_right_q3 <- ntile(data$var_right, 3)
  data$group <- paste(data$var_left_q3, "-", data$var_right_q3)

  # Return
  return(data)
}

#' @describeIn data_get The method for bivar_ldelta_rq3.
#' @export
data_get.bivar_ldelta_rq3 <- function(vars, scale, scales_as_DA = c("building", "street"),
                                      data_path = get_data_path(), ...) {
  # Treat certain scales as DA
  scale <- treat_to_DA(scales_as_DA = scales_as_DA, scale = scale)

  # Retrieve var_left and add a `q3 column`
  data_vl <- data_get_delta(
    var = vars$var_left, scale = scale,
    data_path = data_path
  )
  names(data_vl) <- c("ID", "var_left_1", "var_left_2", "var_left")
  data_vl$var_left_q3 <- ntile(data_vl$var_left, 3)

  # Normal retrieval for var_right (single value)
  data_vr <- data_get_qs(vars$var_right, scale, data_path = data_path)[2:3]
  names(data_vr) <- c("var_right", "var_right_q3")

  # Bind vl and vr
  data <- cbind(data_vl, data_vr)

  # Create the `group` column for map colouring
  data$group <- paste(data$var_left_q3, "-", data$var_right_q3)

  # Return
  return(data)
}

#' Filter data by region
#'
#' Filters a given data frame by the region and scale specified. The function
#' grabs a global variable \code{regions_dictionary} to identify the IDs
#' associated with the given region and scale.
#'
#' @param data <`data.frame`> Data.frame containing the data to be filtered.
#' It must have an \code{ID} column that matches the IDs in \code{regions_dictionary}.
#' @param scale <`character`> The scale at which to filter the data (e.g., 'CSD', 'CT').
#' @param region <`character`> The code of the region for filtering (e.g., 'CMA').
#'
#' @return Returns a filtered data frame containing only the rows corresponding
#' to the specified region and scale.
#' @export
filter_region <- function(data, scale, region) {

  # If no region supplied, return all
  if (is.null(region)) return(data)

  # Get the regions dictionary to grab the vector of IDs with which to filter
  # the retrieved data
  regions_dictionary <- get_from_globalenv("regions_dictionary")

  # Vector of IDs for the current region and scale
  scales <- regions_dictionary$scales[regions_dictionary$region == region][[1]]
  id_reg <- scales[[scale]]
  data <- data[data$ID %in% id_reg, ]

  # Return filtered data
  return(data)
}

#' @describeIn data_get The default method.
#' @export
data_get.default <- function(vars, scale, region = NULL,
                             scales_as_DA = c("building", "street"),
                             data_path = get_data_path(), ...) {

  # Check if `vars` has been entered without being subset from `vars_build()`
  if (all(c("vars", "time") %in% names(vars))) {
    stop("`vars` is invalid. Subset `$vars` from the output of `vars_build()`.")
  }

  # Treat certain scales as DA
  scale <- treat_to_DA(scales_as_DA = scales_as_DA, scale = scale)

  # Grab var and modify if necessary
  var <- vars[[1]]
  if (var == "population") var <- "c_population"
  if (var == "households") var <- "private_households"

  # Default method retrieves the data of the first element of `vars`
  data <- data_get_qs(var = var, scale = scale, data_path = data_path)

  # Filter by region
  data <- filter_region(data = data, scale = scale, region = region)

  # To keep it constant, rename with var_left
  names(data) <- gsub(var, "var_left", names(data))

  # Return
  return(data)
}
