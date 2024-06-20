#' Initialize Necessary Data in the Global Environment
#'
#' This function reads `.qs` and `.qsm` files located in the root of the data
#' folder specified by the `data_folder` argument. Additionally, it establishes
#' connections to postgres database. Finally, it sets default Mapbox
#' configurations and assigns other necessary variables to the global environment.
#'
#' @param data_folder <`character`> Specifies the folder containing the data
#' files and databases to be loaded. Default is "data".
#' @param pos <`numeric`> The environment position to which data and variables
#' will be assigned. Default is 1, referring to the global environment.
#' @param site_name <`character`> Name of the site. Example: "Curbcut Montréal"
#' @param site_url <`character`> URL of the site. Example: "https://montreal.curbcut.ca"
#' @param stories_page <`character`> Name of the stories page. Example: "Montréal stories"
#' @param inst_prefix <`character`> Prefix for the instance, used for both database
#' schema and tileset prefixes. Example: "mtl"
#' @param mapbox_username <`character`> Mapbox username. Example: "curbcut"
#' @param default_random_address <`character`> Default address placeholder for
#' location lock. Example: "845 Sherbrooke Ouest, Montréal, Quebec"
#' @param map_zoom <`numeric`> Default map zoom level. Example: 9.9
#' @param map_loc <`list`> List containing latitude and longitude for map.
#' Example: c(lat = -73.70, lon = 45.53)
#' @return NULL, but objects are loaded into the specified environment and
#' SQLite connections are established.
#' @export
load_data <- function(data_folder = "data", pos = 1,
                      site_name, site_url, stories_page,
                      inst_prefix, mapbox_username,
                      default_random_address, map_zoom, map_loc) {

  # FIX TO PROMSXP detected, replacing with NULL (see https://github.com/traversc/qs/issues/93)
  qs::set_trust_promises(TRUE)

  # Load all .qs and .qsm files that are in the root of the data folder
  data_files <- list.files(data_folder, full.names = TRUE)
  invisible(lapply(data_files[grepl("qsm$", data_files)],
    qs::qload,
    env = as.environment(pos)
  ))
  invisible(lapply(
    data_files[grepl("qs$", data_files)],
    \(x) {
      object_name <- gsub("(.*data/)|(\\.qs)", "", x)
      assign(object_name, qs::qread(x), envir = as.environment(pos))
    }
  ))

  # Map defaults
  map_token <- paste0(
    "pk.eyJ1IjoiY3VyYmN1dCIsImEiOiJjbGprYnVwOTQwaDAzM2xwaWdjbTB6bzdlIn0.Ks1cOI6v2i8jiIjk38s_kg"
  )
  map_base_style <- "mapbox://styles/curbcut/cljkciic3002h01qveq5z1wrp"

  first_level_choropleth <-
    sapply(ls(pos = 1)[grepl("mzl_", ls(pos = 1))], \(x) names(get(x)[1]),
      USE.NAMES = FALSE
    ) |> unique()

  all_choropleths <-
    sapply(ls(pos = 1)[grepl("mzl_", ls(pos = 1))], get, simplify = FALSE, USE.NAMES = TRUE) |>
    unname() |>
    unlist() |>
    names() |>
    unique()

  assign("map_token", map_token, envir = as.environment(pos))
  assign("map_base_style", map_base_style, envir = as.environment(pos))
  assign("first_level_choropleth", first_level_choropleth, envir = as.environment(pos))
  assign("all_choropleths", all_choropleths, envir = as.environment(pos))

  # Assign the additional variables
  assign("site_name", site_name, envir = as.environment(pos))
  assign("site_url", site_url, envir = as.environment(pos))
  assign("stories_page", stories_page, envir = as.environment(pos))
  assign("inst_prefix", inst_prefix, envir = as.environment(pos))
  assign("mapbox_username", mapbox_username, envir = as.environment(pos))
  assign("default_random_address", default_random_address, envir = as.environment(pos))
  assign("map_zoom", map_zoom, envir = as.environment(pos))
  assign("map_loc", map_loc, envir = as.environment(pos))

  # Creation of the pooled database connections, and the exit
  db_pool <- aws_pool()
  assign("db_pool", db_pool, envir = as.environment(pos))
  do.call(shiny::onStop, args = list(\() pool::poolClose(db_pool)),
          envir = as.environment(pos))

}
