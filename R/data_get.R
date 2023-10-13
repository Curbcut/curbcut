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
data_get_qs <- function(var, scale, data_path = "data/") {

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
#' @param var_two_years <`character vector`> A character vector of length 2,
#' where each element is the name of a variable to compare,
#' e.g. `c("housing_tenant_2006", "housing_tenant_2016")`
#' @param df <`character`> A string specifying the name of the database to retrieve
#' data from. Combination of the region and the scale, e.g. `CMA_DA`.
#' @param data_path <`character`> A string representing the path to the directory
#' containing the QS files. Default is "data/".
#'
#' @return A data frame with the following columns: ID, var_1, var_2, and var.
#' `ID` is the ID column from the original data, `var_1` and `var_2` are the
#' values of the two variables being compared, and `var` is the percentage
#' change between the two variables.
data_get_delta <- function(var_two_years, df, data_path = "data/") {

  #' NDS: This should change to take `vars`, `time`, and `scale` arguments. Or,
  #' in fact, maybe it doesn't exist any more, since we're always importing all
  #' years of data for a variable, and the data_get.delta method simply
  #' performs the additional calculations directly inside the method.

  # Retrieve
  data <- lapply(var_two_years, \(x) data_get_qs(x, df, data_path = data_path)[1:2])
  names(data[[1]])[2] <- "var_1"
  data[[1]]$var_2 <- data[[2]][[2]]
  data <- data[[1]]

  # Calculate the value
  data$var <- (data$var_2 - data$var_1) / data$var_1
  data$var <- replace(data$var, is.na(data$var), NA)
  data$var <- replace(data$var, is.infinite(data$var), NA)

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
                     data_path = "data/", ...) {
  UseMethod("data_get", vars)
}

#' Get data for a `q5` class
#'
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link{vars_build}} function. The class of the vars object is
#' used to determine how to grab de data and output it.
#' @param scale <`character`> The scale of the data to be retrieved, e.g. `CSD`.
#' The output of \code{\link{update_scale}}.
#' @param region <`character`> The region from which to grab the vector of IDs with
#' which to filter the retrieved data for a specific region. Defaults to NULL
#' if no filtering is needed.
#' @param scales_as_DA <`character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By
#' default, their colour will be the one of their DA.
#' @param data_path <`character`> A string representing the path to the
#' directory containing the QS files. Default is "data/".
#' @param ... Additional arguments passed to methods.
#'
#' @return A dataframe containing the data, with an ID column, one column per
#' year of data, and one `group` column per year of data.
#' @export
data_get.q5 <- function(vars, scale, region = NULL,
                        scales_as_DA = c("building", "street"),
                        data_path = "data/", ...) {

  # Treat certain scales as DA
  scale <- treat_to_DA(scales_as_DA = scales_as_DA, scale = scale)

  # Get data
  data <- data_get_qs(vars$var_left, scale, data_path = data_path)

  # Filter to region
  if (!is.null(region)) {
    # Get the regions dictionary to grab the vector of IDs with which to filter
    # the retrieved data
    regions_dictionary <- get_from_globalenv("regions_dictionary")
    # Vector of IDs for the current region and scale
    scales <- regions_dictionary$scales[regions_dictionary$region == region][[1]]
    id_reg <- scales[[scale]]
    data <- data[data$ID %in% id_reg, ]
  }

  # Calculate breaks
  data_val <- data[-1]
  data_vec <- data[[attr(data_val, "breaks_var")]]
  data_vec <- data_vec[!is.na(data_vec)]

  if (attr(data, "quintiles")) {
    breaks <- find_breaks_quintiles(data_vec, "q5")
  } else {
    breaks <- find_breaks_q5(min(data_vec), max(data_vec))
  }

  # Assemble output
  out <- as.data.frame(lapply(data_val, .bincode, breaks, include.lowest = TRUE))
  out <- setNames(out, sprintf("%s_q5", names(data_val)))
  data <- cbind(data, out) # bind the data
  data <- tibble::as_tibble(data)
  attr(data, "breaks") <- breaks

  # Rename fields
  names(data) <- gsub(vars$var_left, "var_left", names(data))

  # Return output
  return(data)

}

#' Get data from the SQLite database for a `bivar` class
#'
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link{vars_build}} function.
#' @param df <`character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_scale}}.
#' @param scales_as_DA <`character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#' @param data_path <`character`> A string representing the path to the directory
#' containing the QS files. Default is "data/".
#' @param ... Additional arguments passed to methods.
#'
#' @return A dataframe containing the two variables fresh out of the sqlite db,
#' binded in the same dataframe with an added `group` column for map colouring.
#' @export
data_get.bivar <- function(vars, df, scales_as_DA = c("building", "street"),
                           data_path = "data/", ...) {
  # Treat certain scales as DA
  df <- treat_to_DA(scales_as_DA = scales_as_DA, scale = scale)

  # Get var_left and rename
  data <- data_get_qs(vars$var_left, df, data_path = data_path)
  names(data) <- c("ID", "var_left", "var_left_q3", "var_left_q5")

  # Get var_right and rename
  vr <- data_get_qs(vars$var_right, df, data_path = data_path)
  names(vr) <- c("ID", "var_right", "var_right_q3", "var_right_q5")

  # Error check before binding
  if (!identical(data$ID, vr$ID)) {
    stop(glue::glue_safe(
      "The `ID` vector from the `{var_left}` table is not ",
      "identical to the `ID` vector in the `{var_right}` table in ",
      "the sqlite `{df}` connection. Tables can't be binded."
    ))
  }
  data <- cbind(data, vr[, -1])

  # Add the `group` for the map colouring
  data$group <- paste(data$var_left_q3, data$var_right_q3, sep = " - ")

  # Return
  return(data)
}

#' Get data from the SQLite database for a `delta` class
#'
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link{vars_build}} function.
#' @param df <`character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_scale}}.
#' @param scales_as_DA <`character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#' @param data_path <`character`> A string representing the path to the directory
#' containing the QS files. Default is "data/".
#' @param ... Additional arguments passed to methods.
#'
#' @return A dataframe containing the percentage change between two
#' years of the same variable. `q5` is calculated on the spot with and doubled
#' to the `group` column for map colouring.
#' @export
data_get.delta <- function(vars, df, scales_as_DA = c("building", "street"),
                           data_path = "data/", ...) {
  data_get_delta_fun(vars = vars, df = df, scales_as_DA = scales_as_DA,
                     data_path = data_path, ...)
}

#' @title Inner function to get data based on the type of `vars`
#'
#' @description This function dispatches the data retrieval based on the class
#' of the `vars` object.
#'
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link{vars_build}} function.
#' @param df <`character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_scale}}.
#' @param scales_as_DA <`character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#' @param data_path <`character`> A string representing the path to the directory
#' containing the QS files. Default is "data/".
#' @param ... Additional arguments passed to methods.
#'
#' @seealso \code{\link{data_get.delta}}
data_get_delta_fun <- function(vars, df,
                               scales_as_DA = c("building", "street"),
                               data_path = "data/", ...) {
  UseMethod("data_get_delta_fun", vars)
}

#' @title Retrieve `delta` data for scalar variables
#'
#' @description This function retrieves data for scalar variables
#' and performs additional operations for map coloring.
#'
#' @inheritParams data_get_delta_fun
#'
#' @seealso \code{\link{data_get.delta}}
data_get_delta_fun.scalar <- function(vars, df, scales_as_DA = c("building", "street"),
                                      data_path = "data/", ...) {
  # Treat certain scales as DA
  df <- treat_to_DA(scales_as_DA = scales_as_DA, scale = scale)

  # Retrieve
  data <- data_get_delta(
    var_two_years = vars$var_left, df = df,
    data_path = data_path
  )
  names(data) <- c("ID", "var_left_1", "var_left_2", "var_left")

  # Grab the breaks in the data
  breaks <- breaks_delta(vars = vars, df = df, character = FALSE, data = data)

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

#' @title Retrieve `delta` data for ordinal variables
#'
#' @description This function retrieves data for ordinal variables
#' and performs additional operations for map coloring.
#'
#' @inheritParams data_get_delta_fun
#'
#' @seealso \code{\link{data_get.delta}}
data_get_delta_fun.ordinal <- function(vars, df, scales_as_DA = c("building", "street"),
                                       data_path = "data/", ...) {
  # Treat certain scales as DA
  df <- treat_to_DA(scales_as_DA = scales_as_DA, scale = scale)

  # Retrieve
  data <- data_get_delta(
    var_two_years = vars$var_left, df = df,
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

#' Get data from the SQLite database for a `delta_bivar` class
#'
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link{vars_build}} function.
#' @param df <`character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_scale}}.
#' @param scales_as_DA <`character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#' @param data_path <`character`> A string representing the path to the directory
#' containing the QS files. Default is "data/".
#' @param ... Additional arguments passed to methods.
#'
#' @return A dataframe containing the percentage change between two
#' years of two variables. `q3`s are calculated on the spot along with the
#' `group` column for the map colouring.
#' @export
data_get.delta_bivar <- function(vars, df, scales_as_DA = c("building", "street"),
                                 data_path = "data/", ...) {
  # Treat certain scales as DA
  df <- treat_to_DA(scales_as_DA = scales_as_DA, scale = scale)

  # Retrieve
  data_vl <- data_get_delta(
    var_two_years = vars$var_left, df = df,
    data_path = data_path
  )
  names(data_vl) <- c("ID", "var_left_1", "var_left_2", "var_left")
  data_vr <- data_get_delta(
    var_two_years = vars$var_right, df = df,
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

#' Get data from the SQLite database for a `bivar_ldelta_rq3` class
#'
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link{vars_build}} function.
#' @param df <`character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_scale}}.
#' @param scales_as_DA <`character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#' @param data_path <`character`> A string representing the path to the directory
#' containing the QS files. Default is "data/".
#' @param ... Additional arguments passed to methods.
#'
#' @return A dataframe containing the percentage change between two
#' years of one variable with the static one year value of another variable.
#' `q3`for the percent change value is  calculated on the spot and used
#' to regroup with the `q3` column of the second value to create the `group`
#' column for map colouring.
#' @export
data_get.bivar_ldelta_rq3 <- function(vars, df, scales_as_DA = c("building", "street"),
                                      data_path = "data/", ...) {
  # Treat certain scales as DA
  df <- treat_to_DA(scales_as_DA = scales_as_DA, scale = scale)

  # Retrieve var_left and add a `q3 column`
  data_vl <- data_get_delta(
    var_two_years = vars$var_left, df = df,
    data_path = data_path
  )
  names(data_vl) <- c("ID", "var_left_1", "var_left_2", "var_left")
  data_vl$var_left_q3 <- ntile(data_vl$var_left, 3)

  # Normal retrieval for var_right (single value)
  data_vr <- data_get_qs(vars$var_right, df, data_path = data_path)[2:3]
  names(data_vr) <- c("var_right", "var_right_q3")

  # Bind vl and vr
  data <- cbind(data_vl, data_vr)

  # Create the `group` column for map colouring
  data$group <- paste(data$var_left_q3, "-", data$var_right_q3)

  # Return
  return(data)
}

#' Default data method
#'
#' This is the default data method, which simply returns the table taken out
#' from the sql database. It extracts the first element of `vars` as the `var`
#' argument for the  \code{\link{data_get_qs}} call. It directly outputs the
#' output.
#'
#' @param vars <`named list`> A list object with an unknown class. For this `default`
#' method, no need for vars to have a class. It will extract the first element of
#' `vars` as the `var` argument for the  \code{\link{data_get_qs}} call.
#' @param df <`character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_scale}}.
#' @param scales_as_DA <`character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#' @param data_path <`character`> A string representing the path to the directory
#' containing the QS files. Default is "data/".
#' @param ... Additional arguments passed to other functions.
#'
#' @return A data.frame containing the raw sql table for the first element of `vars`.
#' @export
data_get.default <- function(vars, scale, scales_as_DA = c("building", "street"),
                             data_path = "data/", ...) {
  # Treat certain scales as DA
  df <- treat_to_DA(scales_as_DA = scales_as_DA, scale = scale)

  # Default method retrieves the data of the first element of `vars`
  data <- data_get_qs(var = vars[[1]], scale = scale, data_path = data_path)

  # To keep it constant, rename with var_left
  names(data) <- gsub(vars[[1]], "var_left", names(data))

  # Return
  return(data)
}
