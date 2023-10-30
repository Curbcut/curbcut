#' Assign Values to specific Variables in the environment
#'
#' This function is used to recreate a variable selection, data, df, ... in the
#' global environment, helping to test line-by-line functions.
#'
#' @param var_left <`reactive character`> Character string of the selected
#' variable, e.g. `alp`
#' @param var_right <`reactive character`> Character string of the selected
#' compared variable, e.g. `housing_value`. Defaults to what no compared
#' variable is represented by (" ").
#' @param region <`character`> The region. Defaults to CMA.
#' @param scale <`character`> The scale. Defaults to CSD.
#' @param time <`numeric named list`> Time the user would be interested in. (represents
#' the widget time).
#' @param select_id <`character`> The select_id to assign in the global environment.
#' Defaults to NA.
#' @param pos <`numeric`> An integer value indicating the position in the search list where
#' the environment to be used for assignment is located. Default is 1, which is
#' typically the global environment.
#' @param data_path <`character`> Path to a Curbcut repo data folder.
#' eg, `C:/.../curbcut-montreal/data/`. Defaults to grabbing
#' .curbcut_montreal_data from the environment.
#'
#' @details This function uses the vars_build function to create a new set of
#' variables using var_left, var_right, and df. It then retrieves the
#' corresponding data using the data_get function.
#' It then assigns each of these variables, along with df, region (which is
#' the piece before the underscode in `df`), and select_id to the specified environment.
#'
#' @return This function doesn't return a value. It modifies the specified
#' environment by assigning values to certain variables.
test_assign_any <- function(var_left = "housing_tenant", var_right = " ",
                            region = "CMA", scale = "CSD", time = 2021,
                            select_id = NA, pos = 1, data_path = get_data_path()) {
  if (is.null(data_path)) {
    stop("Set a path from which to grab the data (data folder of a Curbcut repo).")
  }

  # Assign necessary setup objects to the global environment
  test_setup(folder = data_path)

  # Subset the variables table
  variables <- qs::qread(sprintf("%svariables.qs", data_path))
  possible_vars <- c(var_left, var_right)
  parents <- variables$parent_vec[variables$var_code %in% possible_vars]
  variables <- variables[variables$var_code %in% c(possible_vars, parents), ]
  assign("variables", variables, envir = as.environment(pos))

  vars <- vars_build(var_left, var_right, scale = scale, time = time)
  time <- vars$time
  vars <- vars$vars
  data <- data_get(vars = vars, scale = scale, region = region, data_path = data_path)

  assign("scale", scale, envir = as.environment(pos))
  assign("region", region, envir = as.environment(pos))
  assign("vars", vars, envir = as.environment(pos))
  assign("time", time, envir = as.environment(pos))
  assign("data", data, envir = as.environment(pos))
  assign("select_id", select_id, envir = as.environment(pos))
  assign("scales_as_DA", c("building", "street"), envir = as.environment(pos))
  assign("lang", NULL, envir = as.environment(pos))
}

#' Setup objects
#'
#' Assigns objects in the global environment which are needed for testthat and
#' other testing functions.
#'
#' @param pos <`numeric`> An integer value indicating the position in the search list where
#' the environment to be used for assignment is located. Default is 1, which is
#' typically the global environment.
#' @param folder <`character`> Folder from which to grab the test resources. Defaults
#' to `tests/testthat/resources` from coding within the `curbcut` package repo.
#'
#' @return Assigns objects in the global environment which are needed for testthat and
#' other testing functions.
test_setup <- function(pos = 1, folder = get_data_path()) {
  load_data(data_folder = folder,
            pos = 1,
            site_name = "Curbcut Montréal",
            site_url = "https://montreal.curbcut.ca",
            stories_page = "Montréal stories",
            tileset_prefix = "mtl",
            mapbox_username = "curbcut",
            default_random_address = "845 Sherbrooke Ouest, Montréal, Quebec",
            map_zoom = 9.9,
            map_loc = c(lat = -73.70, lon = 45.53))
}
