#' Create resources folder for testing in curbcut package
#'
#' This function copies a specified set of data files and associated metadata from
#' the "data" directory to a newly created "resources" directory. The resources
#' folder can then be cut in the curbcut test/ folder.
#'
#' @return NULL. The function operates by creating and modifying folders and files
#' in the working directory.
#' @export
test_resources_creation <- function() {
  vars <- c("housing_tenant", "housing_rent", "access_foot_20_food_grocery",
            "climate_drought", "canale", "alley_sqkm", "alley_per1k", "vac_rate_bachelor_bed")
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

  other_files <- c("census_variables.qs", "city.qsm", "map_zoom_levels.qsm", "grid.qsm", "cmhc.qsm",
                   "modules.qs", "postal_codes.qs", "regions_dictionary.qs", "scales_dictionary.qs",
                   "stories.qsm", "translation_df.qs", "colours_dfs.qs")

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
#' @return Assigns `df`, `vars`, `data` corresponding to a delta index ordinal
#' variable to the global environment
test_assign_delta_ind_ord <- function(pos = 1) {
  df <- "grid_grid250"
  vars <- vars_build(c("climate_drought_2015", "climate_drought_2022"), df = df)
  data <- data_get(vars = vars, df = df)

  assign("df", df, envir = as.environment(pos))
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
  vars <- vars_build("access_foot_20_food_grocery", df = df)
  data <- data_get(vars = vars, df = df)

  assign("df", df, envir = as.environment(pos))
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
  vars <- vars_build("access_foot_20_food_grocery", "housing_tenant_2021", df = df)
  data <- data_get(vars = vars, df = df)

  assign("df", df, envir = as.environment(pos))
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
  assign("vars", vars, envir = as.environment(pos))
  assign("data", data, envir = as.environment(pos))
}

#' Assign a per1k bivar vars/data to the global environment
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
  assign("vars", vars, envir = as.environment(pos))
  assign("data", data, envir = as.environment(pos))
}
