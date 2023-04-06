#' Retrieve default region and map zoom levels for the Place Explorer module
#'
#' This function retrieves the default region and map zoom levels for the Place
#' Explorer module. It does so by grabbing the modules from the global environment
#' and then getting the default region and map zoom levels based on the ID of the
#' Place Explorer module. The default region is used to retrieve the map zoom
#' levels from the global environment. We can also specify one or more map
#' scales to exclude from the returned map zoom levels, which by default are
#' buildings and streets.
#'
#' @param scales_as_DA <`character vector`> Map scales to exclude from the
#' returned map zoom levels. The default value is c("building", "street").
#'
#' @return A list with two elements: \code{default_regions} and
#' \code{map_zoom_levels}. \code{default_regions} is a character vector with the
#' default region for the Place Explorer module. \code{map_zoom_levels} is a named
#' character vector with the map zoom levels for the default region, excluding
#' any specified map scales.
place_explorer_vars <- function(scales_as_DA = c("building", "street")) {
  # Grab modules from the global environment
  modules <- get_from_globalenv("modules")

  # Get the default region and the map zoom levels
  default_region <- modules$regions[modules$id == "place_explorer"][[1]][1]
  map_zoom_levels <- get_from_globalenv(paste0("map_zoom_levels_", default_region))

  # Take out the scales_as_DA from the map_zoom_levels
  map_zoom_levels <- map_zoom_levels[!names(map_zoom_levels) %in% scales_as_DA]

  # Return
  return(list(
    default_region = default_region,
    map_zoom_levels = map_zoom_levels
  ))
}

#' Create and concatenate Place Explorer HTML files
#'
#' This function creates a place explorer HTML file by concatenating a header
#' file with a specific place explorer file, based on the provided df, select_ID
#' and language. It also generates a temporary file to store the concatenated
#' output.
#'
#' @param temp_folder <`character`> The temporary folder of the app. By default
#' will grab the `temp_folder` object as it's already supposed to have been assigned
#' in the `global.R` file
#' @param df <`character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`.
#' @param select_id <`character`> The ID of the selected polygon.
#' @param lang <`character`> String indicating the language of the place explorer
#' file (either "en" for English or "fr" for French). Default is NULL, which
#' will use English if not specified.
#'
#' @return A list containing the following elements:
#' * src: The relative path to the temporary file, to be used when grabbing
#' the static file for the web server.
#' * file: The absolute path to the temporary file.
place_explorer_html_links <- function(temp_folder, df, select_id, lang = NULL) {
  # Add the head to the place explorer file
  lang <- if (is.null(lang) || lang == "en") "en" else "fr"

  # The link to the place explorer HTML file
  pe_file <- sprintf("www/place_explorer/%s_%s_%s.html", df, select_id, lang)

  # Make a temporary file
  tmpfile <- tempfile(
    pattern = "placeex_tmp",
    tmpdir = temp_folder,
    fileext = ".html"
  )

  # Get the full paths of the style (header of HTML) and the place explorer HTML file
  head <- normalizePath("www/place_explorer/header.html")
  head <- gsub("/", "\\\\", head)
  pef <- normalizePath(pe_file)
  pef <- gsub("/", "\\\\", pef)

  # Concatenate both in the temporary file
  fun <- if (Sys.info()[["sysname"]] == "Windows") "type" else "cat"
  shell(sprintf("%s %s %s > %s", fun, head, pef, tmpfile))

  # Extract only the file name rather than all the path
  tmpfile <- s_extract("placeex_tmp.*$", tmpfile)

  # Return the `src` for when we want to grab the static file for the web
  # server. `file` is the absolute path to the temporary file
  return(list(
    src = file.path("temp_folder_shortcut", tmpfile),
    file = sprintf("%s/%s", temp_folder, tmpfile)
  ))
}
