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

  # The data exists?
  file.exists(path)

  # Read the data
  tryCatch(qs::qread(path), error = function(e) {
    stop(glue::glue_safe(
      "Could not read data from `{path}`. ",
      "Please check that the file exists and is readable."
    ))
  })
}

#' Calculate the percentage change between two variables over two years
#'
#' This function takes two variables representing the same quantity measured two
#' years apart and calculates the percentage change between the two values.
#'
#' @param vars <`character vector`> A var_code. The variable to get data for.
#' @param time <`character vector`> A character vector of length 2. The
#' two years for which the delta should be calculated.
#' @param scale <`character`> A string specifying the scale at which to retrieve
#' data, corresponding to a path on disk, e.g. `DA` or `CSD`.
#' @param vl_vr <`character`> Which of var_left or var_right is this delta supposed
#' to be for. Defaults to var_left.
#' @param data_path <`character`> A string representing the path to the directory
#' containing the QS files. Default is "data/".
#'
#' @return A data frame with the following columns: ID, var_1, var_2, and var.
#' `ID` is the ID column from the original data, `var_1` and `var_2` are the
#' values of the two variables being compared, and `var` is the percentage
#' change between the two variables.
data_get_delta <- function(vars, time, scale, vl_vr = "var_left",
                           data_path = get_data_path()) {
  # Grab the correct var/time
  var <- vars[[vl_vr]]
  time_col <- time[[vl_vr]]

  # Retrieve
  data <- data_get_qs(var, scale, data_path = data_path)

  # Calculate breaks for the right columns
  cols <- match_schema_to_col(data = data, time = time_col, col = var, schemas = NULL)
  keep_cols <- c("ID", cols, attr(data, "breaks_var")) # keep the breaks_var and use it to calculate breaks
  data <- data[unique(keep_cols)]

  # Append breaks
  data <- data_append_breaks(
    var = var,
    data = data,
    q3_q5 = "q5",
    rename_col = vl_vr
  )
  data <- data$data

  # Keep columns of the two years
  cols <- match_schema_to_col(data = data, time = time_col, col = vl_vr, schemas = NULL)
  data <- data[c("ID", grep(paste0(cols, collapse = "|"), names(data), value = TRUE))]

  # Calculate the relative difference
  result <- (data[[3]] - data[[2]]) / data[[2]]
  # Identify positions where data[[3]] is equal to data[[2]] and neither are NAs
  equal_non_na <- !is.na(data[[3]]) & !is.na(data[[2]]) & data[[3]] == data[[2]]
  # Set result to 0 where conditions are met
  result[equal_non_na] <- 0

  # Replace NaNs and infinite values with NA
  data[[vl_vr]] <- result
  data[[vl_vr]] <- replace(data[[vl_vr]], is.na(data[[vl_vr]]), NA)
  data[[vl_vr]] <- replace(data[[vl_vr]], is.infinite(data[[vl_vr]]), NA)

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
  data <- data_append_breaks(
    var = vars$var_left,
    data = data,
    q3_q5 = "q5",
    rename_col = "var_left"
  )

  # Return output
  return(data$data)
}

#' @describeIn data_get The method for bivar.
#' @param schemas <`named list`> Current schema information. The additional widget
#' values that have an impact on which data column to pick. Usually `r[[id]]$schema()`.
#' @export
data_get.bivar <- function(vars, scale, region,
                           scales_as_DA = c("building", "street"),
                           data_path = get_data_path(), schemas, ...) {
  # Treat certain scales as DA
  scale <- treat_to_DA(scales_as_DA = scales_as_DA, scale = scale)

  # Get var_left and var_right data
  vl <- data_get_qs(vars$var_left, scale = scale, data_path = data_path)

  # If data isn't present, throw an empty tibble
  if (!is_data_present_in_scale(var = vars$var_right, scale = scale)) {
    return(tibble::tibble())
  }
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
  possible_vl_times <- unique(possible_vl_times)
  possible_vl_times <- gsub("_", "", possible_vl_times)

  # Possible other vl_schemas
  other_vl_schemas <- all_data[[1]]$attr$schema_var_left
  other_vl_schemas <- other_vl_schemas[names(other_vl_schemas) != "time"]
  if (length(other_vl_schemas) > 0) {
    # possible_other_schemas <- NULL
    # for (i in names(other_vl_schemas)) {
    sch_rege <- "_\\d{1,2}_" # other_vl_schemas[[i]]
    possible_other_schemas <- grep(sch_rege, names(all_data[[1]]$data), value = TRUE)
    possible_other_schemas <- s_extract(sch_rege, possible_other_schemas)
    possible_other_schemas <- gsub("_", "", possible_other_schemas)
    # }
  }

  # Keep left and right breaks
  breaks_vl <- attr(all_data[[1]]$data, "breaks_var_left")
  breaks_vr <- attr(all_data[[2]]$data, "breaks_var_right")

  # Merge
  data <- merge(all_data[[1]]$data, all_data[[2]]$data, by = "ID", all = TRUE)

  # Filter to region
  data <- filter_region(data = data, scale = scale, region = region)

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
  # If there are possible_other_schemas:
  if (length(other_vl_schemas) > 0) {
    for (i in possible_vl_times) {
      for (s in possible_other_schemas) {
        vr_year <- var_closest_year(vars$var_right, i)$closest_year
        out <- paste(data[[sprintf("var_left_%s_%s_q3", s, i)]],
                     data[[sprintf("var_right_%s_q3", vr_year)]],
                     sep = " - "
        )
        data[[sprintf("group_%s_%s", s, i)]] <- out
      }
    }
  } else {
    for (i in possible_vl_times) {
      vr_year <- var_closest_year(vars$var_right, i)$closest_year
      out <- paste(data[[sprintf("var_left_%s_q3", i)]],
                   data[[sprintf("var_right_%s_q3", vr_year)]],
                   sep = " - "
      )
      data[[sprintf("group_%s", i)]] <- out
    }
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
  data_get_delta_fun(
    vars = vars, scale = scale, region = region,
    scales_as_DA = scales_as_DA, data_path = data_path,
    time = time, ...
  )
}

#' @title Inner function to get data based on the type of `vars`
#'
#' @description This function dispatches the data retrieval based on the class
#' of the `vars` object.
#'
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link{vars_build}} function.
#' @param scale <`character`> The scale at which the user is on.
#' @param region <`character vector`> A vector of IDs with which to filter the
#' retrieved data for a specific region, probably retrieved from
#' `regions_dictionary$scales`.
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
  data <- data_get_delta(
    vars = vars, time = time,
    scale = scale, data_path = data_path
  )

  # Filter to region
  data <- filter_region(data = data, scale = scale, region = region)

  # Is delta ONLY positive or ONLY negative? Inform which color scale to use
  vec <- data$var_left
  vec <- vec[!is.na(vec)]
  current <- "normal"
  if (all(vec >= 0)) current <- "positive" else if (all(vec <= 0)) current <- "negative"
  class(data) <- c(current, class(data))

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

  # Get data
  data <- data_get_delta(
    vars = vars, time = time,
    scale = scale, data_path = data_path
  )

  # Filter to region
  data <- filter_region(data = data, scale = scale, region = region)

  # Is delta ONLY positive or ONLY negative? Inform which color scale to use
  vec <- data$var_left
  vec <- vec[!is.na(vec)]
  current <- "normal"
  if (all(vec >= 0)) current <- "positive" else if (all(vec <= 0)) current <- "negative"
  class(data) <- c(current, class(data))

  # Grab the breaks in the data
  breaks <- breaks_delta(vars = vars, scale = scale, character = FALSE, data = data)

  # Add the breaks attribute
  attr(data, "breaks_var_left") <- breaks

  # var_left_q5 will go off of bins change. 0 bin change vs 1 bin change vs multiple
  # bin changes.
  var_left_binchange <- data[[3]] - data[[2]]

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
data_get.delta_bivar <- function(vars, scale, region, scales_as_DA = c("building", "street"),
                                 data_path = get_data_path(), time, ...) {
  # Treat certain scales as DA
  scale <- treat_to_DA(scales_as_DA = scales_as_DA, scale = scale)

  # Retrieve
  data_vl <- data_get_delta(
    vars = vars, time = time, vl_vr = "var_left",
    scale = scale, data_path = data_path
  )
  data_vr <- data_get_delta(
    vars = vars, time = time, vl_vr = "var_right",
    scale = scale, data_path = data_path
  )[-1]

  # Prepare for merge, keep attributes
  prev_attr_vl <- attributes(data_vl)
  prev_attr_vl <- prev_attr_vl[!names(prev_attr_vl) %in% c("names", "row.names", "class")]
  prev_attr_vr <- attributes(data_vr)
  prev_attr_vr <- prev_attr_vr[!names(prev_attr_vr) %in% c("names", "row.names", "class")]

  # Merge
  data <- cbind(data_vl, data_vr)

  # Keep the previous attributes
  for (i in names(prev_attr_vl)) {
    attr(data, i) <- prev_attr_vl[[i]]
  }
  for (i in names(prev_attr_vr)) {
    attr(data, i) <- prev_attr_vr[[i]]
  }

  # Filter to region
  data <- filter_region(data = data, scale = scale, region = region)

  # Add the `group` for the map colouring
  data$var_left_q3 <- ntile(data$var_left, 3)
  data$var_right_q3 <- ntile(data$var_right, 3)
  data$group <- paste(data$var_left_q3, "-", data$var_right_q3)

  # Return
  return(data)
}

#' @describeIn data_get The method for bivar_ldelta_rq3.
#' @export
data_get.bivar_ldelta_rq3 <- function(vars, scale, region, scales_as_DA = c("building", "street"),
                                      data_path = get_data_path(), time, ...) {
  # Reconstruct vars for delta
  vl_vars <- vars_build(var_left = vars$var_left, scale = scale, time = time$var_left)
  vl_time <- vl_vars$time
  vl_vars <- vl_vars$vars
  data_vl <- data_get(vl_vars,
                      scale = scale, time = vl_time, region = region,
                      scales_as_DA = scales_as_DA
  )
  data_vl$var_left_q3 <- ntile(data_vl$var_left, 3)

  # Reconstruct vars for q3
  vr_vars <- vars_build(var_left = vars$var_right, scale = scale, time = time$var_right)
  vr_time <- vr_vars$time
  vr_vars <- vr_vars$vars
  data_vr <- data_get(vr_vars,
                      scale = scale, time = vr_time, region = region,
                      scales_as_DA = scales_as_DA
  )
  cv <- match_schema_to_col(data_vr, time = vr_time, schemas = NULL)
  data_vr <- data_vr[cv]
  names(data_vr) <- gsub("var_left", "var_right", names(data_vr))
  data_vr$var_right_q3 <- ntile(data_vr[[1]], 3)

  # Prepare for merge, keep attributes
  prev_attr_vl <- attributes(data_vl)
  prev_attr_vl <- prev_attr_vl[!names(prev_attr_vl) %in% c("names", "row.names", "class")]
  prev_attr_vr <- attributes(data_vr)
  prev_attr_vr <- prev_attr_vr[!names(prev_attr_vr) %in% c("names", "row.names", "class")]
  names(prev_attr_vr) <- gsub("var_left", "var_right", names(prev_attr_vr))

  # Bind vl and vr
  data <- cbind(data_vl, data_vr)

  # Keep the previous attributes
  for (i in names(prev_attr_vl)) {
    attr(data, i) <- prev_attr_vl[[i]]
  }
  for (i in names(prev_attr_vr)) {
    attr(data, i) <- prev_attr_vr[[i]]
  }

  # Create the `group` column for map colouring
  data$group <- sprintf("%s - %s", data$var_left_q3, data$var_right_q3)

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
  if (is.null(region)) {
    return(data)
  }

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
#' @param vr_vl <`character`> Is the parent data coming from the var_left
#' or var_right? How should it be renamed.
#' @export
data_get.default <- function(vars, scale, region = NULL,
                             scales_as_DA = c("building", "street"),
                             data_path = get_data_path(), vr_vl, ...) {
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
  data <- if (var == "area") {
    get_from_globalenv(scale)[c("ID", "area")]
  } else {
    data_get_qs(var = var, scale = scale, data_path = data_path)
  }

  # Filter by region
  data <- filter_region(data = data, scale = scale, region = region)

  # To keep it constant, rename with var_left
  names(data) <- gsub(var, vr_vl, names(data))

  # Return
  return(data)
}
