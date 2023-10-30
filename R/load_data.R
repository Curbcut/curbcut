#' Initialize Necessary Data in the Global Environment
#'
#' This function reads `.qs` and `.qsm` files located in the root of the data
#' folder specified by the `data_folder` argument. Additionally, it establishes
#' connections to `.sqlite` databases present in the data folder. Finally, it
#' sets default Mapbox configurations and assigns other necessary variables to
#' the global environment.
#'
#' @param data_folder <`character`> Specifies the folder containing the data
#' files and databases to be loaded. Default is "data".
#' @param pos <`numeric`> The environment position to which data and variables
#' will be assigned. Default is 1, referring to the global environment.
#' @param site_name <`character`> Name of the site. Example: "Curbcut Montréal"
#' @param site_url <`character`> URL of the site. Example: "https://montreal.curbcut.ca"
#' @param stories_page <`character`> Name of the stories page. Example: "Montréal stories"
#' @param tileset_prefix <`character`> Prefix for tilesets. Example: "mtl"
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
                      tileset_prefix, mapbox_username,
                      default_random_address, map_zoom, map_loc) {

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

  # Connect to the dbs
  dbs <- list.files(data_folder, full.names = TRUE, recursive = FALSE)
  dbs <- subset(dbs, grepl(".sqlite$", dbs))

  lapply(dbs, \(x) {
    connection_name <- paste0(s_extract("(?<=data/).*?(?=\\.)", x), "_conn")
    assign(connection_name, DBI::dbConnect(RSQLite::SQLite(), x), envir = as.environment(pos))
  }) |> invisible()


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
  assign("tileset_prefix", tileset_prefix, envir = as.environment(pos))
  assign("mapbox_username", mapbox_username, envir = as.environment(pos))
  assign("default_random_address", default_random_address, envir = as.environment(pos))
  assign("map_zoom", map_zoom, envir = as.environment(pos))
  assign("map_loc", map_loc, envir = as.environment(pos))

}
