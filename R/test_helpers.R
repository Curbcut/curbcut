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

#' Assign a ind ordinal delta vars/data to the global environment
#'
#' @param pos <`numeric`> Defaults to 1, the environment in which to assign the
#' values. This argument is to appease RMD check.
#'
#' @return Assigns `df`, `vars`, `data`
test_assign_delta_ind_ord <- function(pos = 1) {
  df <- "grid_grid250"
  vars <- vars_build(c("climate_drought_2015", "climate_drought_2022"), df = df)
  data <- data_get(vars = vars, df = df)

  assign("df", df, envir = as.environment(pos))
  assign("region", "grid", envir = as.environment(pos))
  assign("vars", vars, envir = as.environment(pos))
  assign("data", data, envir = as.environment(pos))
}

#' Assign a ind ordinal delta_bivar vars/data to the global environment
#'
#' @param pos <`numeric`> Defaults to 1, the environment in which to assign the
#' values. This argument is to appease RMD check.
#'
#' @return Assigns `df`, `vars`, `data`
test_assign_delta_bivar_ind_ord <- function(pos = 1) {
  df <- "grid_grid250"
  vars <- vars_build(c("climate_drought_2015", "climate_drought_2022"),
    c("housing_rent_2016", "housing_rent_2021"),
    df = df
  )
  data <- data_get(vars = vars, df = df)

  assign("df", df, envir = as.environment(pos))
  assign("region", "grid", envir = as.environment(pos))
  assign("vars", vars, envir = as.environment(pos))
  assign("data", data, envir = as.environment(pos))
}

#' Assign a ind ordinal q5 vars/data to the global environment
#'
#' @param pos <`numeric`> Defaults to 1, the environment in which to assign the
#' values. This argument is to appease RMD check.
#'
#' @return Assigns a constructed `df`, `vars`, `data` in the global environment
test_assign_q5_ind_ord <- function(pos = 1) {
  df <- "grid_grid250"
  vars <- vars_build(c("climate_drought_2022"), df = df)
  data <- data_get(vars = vars, df = df)

  assign("df", df, envir = as.environment(pos))
  assign("region", "grid", envir = as.environment(pos))
  assign("vars", vars, envir = as.environment(pos))
  assign("data", data, envir = as.environment(pos))
}

#' Assign a ind ordinal bivar vars/data to the global environment
#'
#' @param pos <`numeric`> Defaults to 1, the environment in which to assign the
#' values. This argument is to appease RMD check.
#'
#' @return Assigns a constructed `df`, `vars`, `data` in the global environment
test_assign_bivar_ind_ord <- function(pos = 1) {
  df <- "grid_grid250"
  vars <- vars_build(c("climate_drought_2022"), "housing_tenant_2021", df = df)
  data <- data_get(vars = vars, df = df)

  assign("df", df, envir = as.environment(pos))
  assign("region", "grid", envir = as.environment(pos))
  assign("vars", vars, envir = as.environment(pos))
  assign("data", data, envir = as.environment(pos))
}

#' Assign a avg q5 vars/data to the global environment
#'
#' @param pos <`numeric`> Defaults to 1, the environment in which to assign the
#' values. This argument is to appease RMD check.
#'
#' @return Assigns a constructed `df`, `vars`, `data` in the global environment
test_assign_q5_avg <- function(pos = 1) {
  df <- "city_CSD"
  vars <- vars_build("access_foot_20_food_grocery_2023", df = df)
  data <- data_get(vars = vars, df = df)

  assign("df", df, envir = as.environment(pos))
  assign("region", "city", envir = as.environment(pos))
  assign("vars", vars, envir = as.environment(pos))
  assign("data", data, envir = as.environment(pos))
}

#' Assign a avg bivar vars/data to the global environment
#'
#' @param pos <`numeric`> Defaults to 1, the environment in which to assign the
#' values. This argument is to appease RMD check.
#'
#' @return Assigns a constructed `df`, `vars`, `data` in the global environment
test_assign_bivar_avg <- function(pos = 1) {
  df <- "city_CSD"
  vars <- vars_build("access_foot_20_food_grocery_2023", "housing_tenant_2021", df = df)
  data <- data_get(vars = vars, df = df)

  assign("df", df, envir = as.environment(pos))
  assign("region", "city", envir = as.environment(pos))
  assign("vars", vars, envir = as.environment(pos))
  assign("data", data, envir = as.environment(pos))
}

#' Assign a sqkm bivar vars/data to the global environment
#'
#' @param pos <`numeric`> Defaults to 1, the environment in which to assign the
#' values. This argument is to appease RMD check.
#'
#' @return Assigns a constructed `df`, `vars`, `data` in the global environment
test_assign_q5_sqkm <- function(pos = 1) {
  df <- "city_CSD"
  vars <- vars_build("alley_sqkm_2023", df = df)
  data <- data_get(vars = vars, df = df)

  assign("df", df, envir = as.environment(pos))
  assign("region", "city", envir = as.environment(pos))
  assign("vars", vars, envir = as.environment(pos))
  assign("data", data, envir = as.environment(pos))
}

#' Assign a per1k q5 vars/data to the global environment
#'
#' @param pos <`numeric`> Defaults to 1, the environment in which to assign the
#' values. This argument is to appease RMD check.
#'
#' @return Assigns a constructed `df`, `vars`, `data` in the global environment
test_assign_q5_per1k <- function(pos = 1) {
  df <- "city_CSD"
  vars <- vars_build("alley_per1k_2023", df = df)
  data <- data_get(vars = vars, df = df)

  assign("df", df, envir = as.environment(pos))
  assign("region", "city", envir = as.environment(pos))
  assign("vars", vars, envir = as.environment(pos))
  assign("data", data, envir = as.environment(pos))
}

#' Assign a q5 count vars/data to the global environment
#'
#' @param pos <`numeric`> Defaults to 1, the environment in which to assign the
#' values. This argument is to appease RMD check.
#'
#' @return Assigns a constructed `df`, `vars`, `data` in the global environment
test_assign_q5_count <- function(pos = 1) {
  df <- "city_CSD"
  vars <- vars_build("afford_tenant_sc30_total_total_total_count_2021", df = df)
  data <- data_get(vars = vars, df = df)

  assign("df", df, envir = as.environment(pos))
  assign("region", "city", envir = as.environment(pos))
  assign("vars", vars, envir = as.environment(pos))
  assign("data", data, envir = as.environment(pos))
}

#' Assign a count bivar vars/data to the global environment
#'
#' @param pos <`numeric`> Defaults to 1, the environment in which to assign the
#' values. This argument is to appease RMD check.
#'
#' @return Assigns a constructed `df`, `vars`, `data` in the global environment
test_assign_bivar_count <- function(pos = 1) {
  df <- "city_CSD"
  vars <- vars_build("afford_tenant_sc30_total_total_total_count_2021",
    "housing_tenant_2021",
    df = df
  )
  data <- data_get(vars = vars, df = df)

  assign("df", df, envir = as.environment(pos))
  assign("region", "city", envir = as.environment(pos))
  assign("vars", vars, envir = as.environment(pos))
  assign("data", data, envir = as.environment(pos))
}

#' Assign a count delta vars/data to the global environment
#'
#' @param pos <`numeric`> Defaults to 1, the environment in which to assign the
#' values. This argument is to appease RMD check.
#'
#' @return Assigns a constructed `df`, `vars`, `data` in the global environment
test_assign_delta_count <- function(pos = 1) {
  df <- "city_CSD"
  vars <- vars_build(
    c(
      "afford_tenant_sc30_total_total_total_count_2016",
      "afford_tenant_sc30_total_total_total_count_2021"
    ),
    df = df
  )
  data <- data_get(vars = vars, df = df)

  assign("df", df, envir = as.environment(pos))
  assign("region", "city", envir = as.environment(pos))
  assign("vars", vars, envir = as.environment(pos))
  assign("data", data, envir = as.environment(pos))
}

#' Assign a dollar q5 vars/data to the global environment
#'
#' @param pos <`numeric`> Defaults to 1, the environment in which to assign the
#' values. This argument is to appease RMD check.
#'
#' @return Assigns `df`, `vars`, `data` corresponding to a delta index ordinal
#' variable to the global environment
test_assign_q5_dollar <- function(pos = 1) {
  df <- "city_CSD"
  vars <- vars_build("housing_rent_2021", df = df)
  data <- data_get(vars = vars, df = df)

  assign("df", df, envir = as.environment(pos))
  assign("region", "city", envir = as.environment(pos))
  assign("vars", vars, envir = as.environment(pos))
  assign("data", data, envir = as.environment(pos))
}

#' Assign a dollar bivar vars/data to the global environment
#'
#' @param pos <`numeric`> Defaults to 1, the environment in which to assign the
#' values. This argument is to appease RMD check.
#'
#' @return Assigns `df`, `vars`, `data`
test_assign_bivar_dollar <- function(pos = 1) {
  df <- "city_CSD"
  vars <- vars_build("housing_rent_2021", "alp_2021", df = df)
  data <- data_get(vars = vars, df = df)

  assign("df", df, envir = as.environment(pos))
  assign("region", "city", envir = as.environment(pos))
  assign("vars", vars, envir = as.environment(pos))
  assign("data", data, envir = as.environment(pos))
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
                            select_id = NA, pos = 1) {
  vars <- vars_build(var_left, var_right, df = df)
  data <- data_get(vars = vars, df = df)
  region <- gsub("_.*", "", df)

  assign("df", df, envir = as.environment(pos))
  assign("region", region, envir = as.environment(pos))
  assign("vars", vars, envir = as.environment(pos))
  assign("data", data, envir = as.environment(pos))
  assign("select_id", select_id, envir = as.environment(pos))
}
