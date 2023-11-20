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

  if (!"place_explorer" %in% modules$id) {
    stop("`place_explorer` is missing as an id in the modules table.")
  }

  # Get the default region and the map zoom levels
  default_region <- modules$regions[modules$id == "place_explorer"][[1]][1]
  # NDS
  # map_zoom_levels <- get_from_globalenv("mzl_CSD_CT_DA_building")

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
#' @param region <`character`> Region under study, e.g. `CMA`.
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
place_explorer_html_links <- function(temp_folder, region, df, select_id, lang = NULL) {
  shiny::withProgress(
    message = cc_t("Generating report", lang = lang),
    {
      # Show there is a progress happening
      shiny::incProgress(0.2)

      # Add the head to the place explorer file
      lang <- if (is.null(lang) || lang == "en") "en" else "fr"

      # The link to the place explorer HTML file
      pe_file <- sprintf("www/place_explorer/%s_%s_%s.html", df, select_id, lang)

      # Is place explorer already pre-processed?
      pe_docs <- get_from_globalenv("pe_docs")

      # Make a temporary file
      tmpfile <- tempfile(
        pattern = "placeex_tmp",
        tmpdir = temp_folder,
        fileext = ".html"
      )

      shiny::incProgress(0.3)

      if (pe_file %in% pe_docs) {
        # Get the full paths of the style (header of HTML) and the place explorer HTML file
        head <- normalizePath("www/place_explorer/header.html")
        pef <- normalizePath(pe_file)

        # Concatenate both in the temporary file
        if (Sys.info()[["sysname"]] == "Windows") {
          # the gsub() function is used to replace forward slashes with backslashes,
          # which is specific to Windows
          head <- gsub("/", "\\\\", head)
          pef <- gsub("/", "\\\\", pef)
          shell(sprintf("type %s %s > %s", head, pef, tmpfile))
        } else {
          system(sprintf("cat %s %s > %s", head, pef, tmpfile))
        }
      } else {
        # If the place explorer is not already pre-processed, process it on the spot.
        place_explorer_create_html(
          tmpfile = tmpfile, region = region, df = df,
          select_id = select_id, lang = lang
        )
      }

      shiny::incProgress(0.3)

      # Extract only the file name rather than all the path
      tmpfile <- s_extract("placeex_tmp.*$", tmpfile)

      shiny::incProgress(0.2)
    }
  )

  # Return the `src` for when we want to grab the static file for the web
  # server. `file` is the absolute path to the temporary file
  return(list(
    src = file.path("temp_folder_shortcut", tmpfile),
    file = sprintf("%s/%s", temp_folder, tmpfile)
  ))
}

#' Create HTML file for Place Explorer with dynamic content
#'
#' This function creates a temporary HTML file using data provided as parameters and
#' global variables. It first checks the version of the `bslib` package, which needs to
#' be the modified version: `devtools::install_github('bdbmax/bslib')` to allow
#' for tabs colouring. If not, throws a warning. It then retrieves
#' necessary global variables. The function also dynamically adjusts the map zoom level
#' based on the scale of the data.
#'
#' @param tmpfile <`character`> Temporary file location, an .html document.
#' @param region <`character`> Region under study, e.g. `CMA`.
#' @param df <`character`> Scale under study, e.g. `DA`.
#' @param select_id <`character`> Selected identifier for the selected combinasion
#' of `region` and `df`.
#' @param lang <`character`> Language that should be used to produce the main card
#' output. There need to be a function `placeex_main_card_prep_output_x` available
#' in that language. `en` or `fr`.
#'
#' @return The function creates an HTML document which represents the Place Explorer
#' for the given data point (specified by `select_id`). It is output at the location
#' of `tmpfile`
place_explorer_create_html <- function(tmpfile, region, df, select_id, lang) {
  if (utils::packageVersion("bslib") != "9.9.9.9999") {
    warning(paste0(
      "Wrong version of `bslib`. Place explorer tabs won't be ",
      "colored according to how much of an outlier the zone is for ",
      "every theme. To ignore, set `check_bslib_version = FALSE`. ",
      "To build with colored tabs, install ",
      "`devtools::install_github('bdbmax/bslib')`"
    ))
  }

  # Get the necessary globals
  pe_main_card_data <- get_from_globalenv("pe_main_card_data")
  scales_dictionary <- get_from_globalenv("scales_dictionary")
  regions_dictionary <- get_from_globalenv("regions_dictionary")
  tileset_prefix <- get_from_globalenv("tileset_prefix")
  mapbox_username <- get_from_globalenv("mapbox_username")

  # Input rmarkdown file
  inp <- system.file(paste0("place_explorer/pe_", lang, ".Rmd"),
    package = "curbcut"
  )

  # Setup all necessary input
  scale_df <- get_from_globalenv(df)
  scale_df <- scale_df[scale_df$ID == select_id, ]

  map_loc <- scale_df$centroid[[1]]
  title_card_data <-
    placeex_main_card_final_output(
      region = region, df = df, select_id = select_id,
      lang = lang, pe_main_card_data = pe_main_card_data,
      scales_dictionary = scales_dictionary,
      regions_dictionary = regions_dictionary
    )

  map_zoom <- (\(x) {
    if (is_scale_in("CT", df)) {
      return(11)
    }
    if (is_scale_in("DA", df)) {
      return(13)
    }
    # For first level
    return(10)
  })()

  # Add title
  scale <- scales_dictionary[
    is_scale_in(scales_dictionary$scale, scale = scale, vectorized = TRUE),
  ]
  # Get the place heading and glue it
  dat <- grab_row_from_bslike(
    scale = scale, select_id = select_id,
    cols = c("name", "name_2")
  )
  name <- dat$name
  name_2 <- cc_t(dat$name_2, lang = lang)
  title <- cc_t(scale$place_heading, lang = lang)

  # Render the HTML document
  rmarkdown::render(
    inp,
    output_file = tmpfile,
    params = list(
      wd = getwd(),
      title = title,
      select_id = select_id,
      region = region,
      df = df,
      scale_sing = cc_t(sprintf("%s (count)", scale$slider_title), lang = lang),
      tileset_prefix = tileset_prefix,
      map_loc = map_loc,
      map_zoom = map_zoom,
      mapbox_username = mapbox_username,
      title_card_data = title_card_data,
      variables = get_from_globalenv("variables"),
      scale_df = sf::st_drop_geometry(scale_df)
    ), envir = new.env(), quiet = TRUE,
    # Make sure the data gets extracted through the right directory
    knit_root_dir = getwd()
  )
}
