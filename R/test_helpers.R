#' Create resources folder for testing in curbcut package
#'
#' This function copies a specified set of data files and associated metadata from
#' the "data" directory to a newly created "resources" directory. The resources
#' folder can then be cut in the curbcut test/ folder.
#'
#' @param additional_vars <`character vector`> Additional var codes to include
#' in the testing resources.
#'
#' @return NULL. The function operates by creating and modifying folders and files
#' in the working directory.
#' @export
test_resources_creation <- function(additional_vars = c()) {
  vars <- c(
    "housing_tenant", "housing_rent", "access_foot_20_food_grocery",
    "access_transit_nwd_30_cultural_artcentre", "climate_drought",
    "afford_tenant_sc30_total_total_total_count_2021",
    "alp", "alley_sqkm", "alley_per1k", "vac_rate_bachelor_bed",
    additional_vars
  )
  variables <- qs::qread("data/variables.qs")
  p_v <- variables$parent_vec[variables$var_code %in% vars]
  p_v <- p_v[!is.na(p_v)]
  vars <- c(p_v, vars)
  vars <- unique(vars)
  vars_pth <- paste0("\\/", vars)

  # Create the destination folder if it doesn't exist
  if (!dir.exists("resources")) {
    dir.create("resources")
  }
  regions_to_extract <- c("city", "grid", "cmhc")

  lapply(regions_to_extract, \(region) {
    path_to_data <- sprintf("data/%s", region)

    all_files <- list.files(path_to_data, recursive = T, full.names = TRUE)

    data_files <- all_files[grepl(paste0(vars_pth, collapse = "|"), all_files)]

    # Copy all the data files
    mapply(\(from, to) {
      dir_path <- dirname(to)
      if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE)
      }
      file.copy(from, to)
    }, data_files, gsub("data", "resources/data", data_files))
  })

  variables <- variables[variables$var_code %in% vars, ]

  other_files <- c(
    "census_variables.qs", "city.qsm", "map_zoom_levels.qsm",
    "grid.qsm", "cmhc.qsm", "modules.qs", "postal_codes.qs",
    "regions_dictionary.qs", "scales_dictionary.qs", "stories.qsm",
    "translation_df.qs", "colours_dfs.qs", "building.sqlite",
    "grid100.sqlite", "pe_main_card_data.qs"
  )

  # Save the variables
  qs::qsave(variables, "resources/variables.qs")

  # Save all the other files
  mapply(file.copy, paste0("data/", other_files), paste0("resources/", other_files))
}

#' Assign Values to specific Variables in the environment
#'
#' This function is used to recreate a variable selection, data, df, ... in the
#' global environment, helping to test line-by-line functions.
#'
#' @param var_left <`reactive character`> Character string of the selected
#' variable, e.g. `alp_2016` or `c("housing_tenant_2006", "housing_tenant_2016")`.
#' @param var_right <`reactive character`> Character string of the selected
#' compared variable, e.g. `housing_value_2016`. Defaults to what no compared
#' variable is represented by (" ").
#' @param df <`character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. Defaults to
#' `city_CSD`.
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
test_assign_any <- function(var_left, var_right = " ", df = "city_CSD",
                            select_id = NA, pos = 1, data_path = get0(".curbcut_montreal_data")) {
  if (is.null(data_path)) {
    stop("Set a path from which to grab the data (data folder of a Curbcut repo).")
  }

  # Assign necessary setup objects to the global environment
  test_setup(folder = data_path)

  # Subset the variables table
  variables <- qs::qread(sprintf("%svariables.qs", data_path))
  possible_vars <- unique(sapply(c(var_left, var_right), var_remove_time))
  parents <- variables$parent_vec[variables$var_code %in% possible_vars]
  variables <- variables[variables$var_code %in% c(possible_vars, parents), ]
  assign("variables", variables, envir = as.environment(pos))

  vars <- vars_build(var_left, var_right, df = df)
  data <- data_get(vars = vars, df = df, data_path = data_path)
  region <- gsub("_.*", "", df)

  assign("df", df, envir = as.environment(pos))
  assign("region", region, envir = as.environment(pos))
  assign("vars", vars, envir = as.environment(pos))
  assign("data", data, envir = as.environment(pos))
  assign("select_id", select_id, envir = as.environment(pos))
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
test_setup <- function(pos = 1, folder = "tests/testthat/resources") {
  # Variables present in the .GlobalEnv
  assign("all_choropleths",
    value = c(
      "CSD", "CT", "DA", "building", "grid50", "grid100", "grid250",
      "cmhczone"
    ),
    envir = as.environment(pos)
  )

  # Default random address
  assign("default_random_address",
    value = "845 Sherbrooke",
    envir = as.environment(pos)
  )

  # Default tileset info
  assign("tileset_prefix",
    value = "mtl",
    envir = as.environment(pos)
  )
  assign("mapbox_username",
    value = "sus-mcgill",
    envir = as.environment(pos)
  )

  # All qs and qsm files
  data_files <- list.files(folder, full.names = TRUE)
  invisible(lapply(data_files[grepl("qsm$", data_files)],
    qs::qload,
    env = as.environment(pos)
  ))
  invisible(lapply(
    data_files[grepl("qs$", data_files)],
    \(x) {
      object_name <- gsub(sprintf("(%s/)|(\\.qs)", folder), "", x)
      assign(object_name, qs::qread(x), envir = as.environment(pos))
    }
  ))

  # All sqlite files
  dbs <- list.files(folder, full.names = TRUE)
  dbs <- subset(dbs, grepl(".sqlite$", dbs))

  lapply(dbs, \(x) {
    connection_name <- paste0(s_extract("(?<=/).*?(?=\\.)", x), "_conn")
    assign(connection_name, DBI::dbConnect(RSQLite::SQLite(), x), envir = as.environment(pos))
  })

  conn <- DBI::dbConnect(RSQLite::SQLite(), paste0(folder, "/building.sqlite"))
  tbs <- DBI::dbListTables(conn)
  tbs <- tbs[!grepl("city_", tbs)]
  lapply(tbs, \(x) DBI::dbRemoveTable(conn, x))
  suppressWarnings(DBI::dbGetQuery(conn, "VACUUM"))

  return()
}
