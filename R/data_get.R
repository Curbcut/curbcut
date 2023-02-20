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
    stop(glue::glue(
      "Connection to the sqlite database `{conn}` does not ",
      "exist in the global environment."
    ))
  }

  # Get from the sqlite connection
  do.call(DBI::dbGetQuery, list(
    rlang::sym(conn),
    glue::glue("SELECT {select} FROM {var} ORDER BY ID")
  ))
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
#'
#' @return A data frame with the following columns: ID, var_1, var_2, and var.
#' `ID` is the ID column from the original data, `var_1` and `var_2` are the
#' values of the two variables being compared, and `var` is the percentage
#' change between the two variables.
data_get_delta <- function(var_two_years, df) {
  # Retrieve
  data <- lapply(var_two_years, \(x) data_get_sql(x, df, x))
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

#' Get data from the SQLite database
#'
#' This function retrieves data from an SQLite database using the appropriate
#' method based on the class of the input vars object. vars should be a named
#' list with a class, built using the \code{\link[curbcut]{vars_build}} function.
#' Depending on the class of vars, different methods will be used to retrieve
#' and process the data.
#'
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link[curbcut]{vars_build}} function. The class of the vars object is
#' used to determine how to grab de data and output it.
#' @param df <`character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link[curbcut]{df_get}}.
#' @param ... Additional arguments passed to methods.
#'
#' @return A dataframe containing the data according to the class of `vars`
#' along with a `group` column for map colouring.
#' @export
data_get <- function(vars, df, ...) {
  UseMethod("data_get", vars)
}

#' Get data from the SQLite database for a `q5` class
#'
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link[curbcut]{vars_build}} function.
#' @param df <`character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link[curbcut]{df_get}}.
#' @param ... Additional arguments passed to methods.
#'
#' @return A dataframe containing the data fresh out of the sqlite db, with an
#' added `group` column for map colouring.
#' @export
data_get.q5 <- function(vars, df, ...) {
  # Get var_left and rename
  data <- data_get_sql(vars$var_left, df)
  names(data) <- c("ID", "var_left", "var_left_q3", "var_left_q5")

  # Add the `group` for the map colouring
  data$group <- data$var_left_q5

  # Return
  return(data)
}

#' Get data from the SQLite database for a `bivar` class
#'
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link[curbcut]{vars_build}} function.
#' @param df <`character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link[curbcut]{df_get}}.
#' @param ... Additional arguments passed to methods.
#'
#' @return A dataframe containing the two variables fresh out of the sqlite db,
#' binded in the same dataframe with an added `group` column for map colouring.
#' @export
data_get.bivar <- function(vars, df, ...) {
  # Get var_left and rename
  data <- data_get_sql(vars$var_left, df)
  names(data) <- c("ID", "var_left", "var_left_q3", "var_left_q5")

  # Get var_right and rename
  vr <- data_get_sql(vars$var_right, df)
  names(vr) <- c("ID", "var_right", "var_right_q3", "var_right_q5")

  # Error check before binding
  if (!identical(data$ID, vr$ID)) {
    stop(glue::glue(
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
#' \code{\link[curbcut]{vars_build}} function.
#' @param df <`character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link[curbcut]{df_get}}.
#' @param ... Additional arguments passed to methods.
#'
#' @return A dataframe containing the percentage change between two
#' years of the same variable. `q5` is calculated on the spot with and doubled
#' to the `group` column for map colouring.
#' @export
data_get.delta <- function(vars, df, ...) {
  # Retrieve
  data <- data_get_delta(var_two_years = vars$var_left, df = df)
  names(data) <- c("ID", "var_left_1", "var_left_2", "var_left")

  # Add the `group` for the map colouring
  data$var_left_q5 <- 5
  data$var_left_q5[data$var_left < 0.1] <- 4
  data$var_left_q5[data$var_left < 0.02] <- 3
  data$var_left_q5[data$var_left < -0.02] <- 2
  data$var_left_q5[data$var_left < -0.1] <- 1
  data$var_left_q5[is.na(data$var_left)] <- NA
  data$group <- as.character(data$var_left_q5)

  # Return
  return(data)
}

#' Get data from the SQLite database for a `delta_bivar` class
#'
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link[curbcut]{vars_build}} function.
#' @param df <`character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link[curbcut]{df_get}}.
#' @param ... Additional arguments passed to methods.
#'
#' @return A dataframe containing the percentage change between two
#' years of two variables. `q3`s are calculated on the spot along with the
#' `group` column for the map colouring.
#' @export
data_get.delta_bivar <- function(vars, df, ...) {
  # Retrieve
  data_vl <- data_get_delta(var_two_years = vars$var_left, df = df)
  names(data_vl) <- c("ID", "var_left_1", "var_left_2", "var_left")
  data_vr <- data_get_delta(var_two_years = vars$var_right, df = df)[-1]
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
#' \code{\link[curbcut]{vars_build}} function.
#' @param df <`character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link[curbcut]{df_get}}.
#' @param ... Additional arguments passed to methods.
#'
#' @return A dataframe containing the percentage change between two
#' years of one variable with the static one year value of another variable.
#' `q3`for the percent change value is  calculated on the spot and used
#' to regroup with the `q3` column of the second value to create the `group`
#' column for map colouring.
#' @export
data_get.bivar_ldelta_rq3 <- function(vars, df, ...) {
  # Retrieve var_left and add a `q3 column`
  data_vl <- data_get_delta(var_two_years = vars$var_left, df = df)
  names(data_vl) <- c("ID", "var_left_1", "var_left_2", "var_left")
  data_vl$var_left_q3 <- ntile(data_vl$var_left, 3)

  # Normal retrieval for var_right (single value)
  data_vr <- data_get_sql(vars$var_right, df)[2:3]
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
#' This is the default data method, which returns all possible columns filled
#' with NA values. It is intended to be used when no specific get data method
#' is available or necessary. In this case, `NA` takes place.
#'
#' @param vars <`named list`> A list object with an unknown class.
#' @param ... Additional arguments passed to other functions.
#'
#' @return A dataframe with `NA` values
#' @export
data_get.default <- function(vars, ...) {
  data <- data.frame(ID = NA)
  data$ID <- data$var_left <- data$var_left_q3 <- data$var_left_1 <-
    data$var_left_2 <- data$var_right <- data$var_right_q3 <-
    data$var_right_1 <- data$var_right_2 <- NA
  data$group <- "NA"
  return(data)
}
