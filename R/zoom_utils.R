#' Generate zoom string based on given zoom level and region name
#'
#' Given a named numeric vector of zoom levels, a numeric value representing the current
#' zoom level, and a character string representing the name of the region, this
#' function returns a string representing the zoom level and region name,
#' separated by an underscore. The zoom level is determined by finding the
#' highest value in the zoom levels vector that is less than or equal to the
#' current zoom level.
#'
#' @param zoom <`numeric`> A numeric value representing the current zoom level
#' @param zoom_levels <`named numeric vector`>A numeric vector representing the
#' available zoom levels for the region under study.
#' @param region <`character`> A character string representing the name of the
#' region
#'
#' @return A character string representing the zoom level and region name,
#' separated by an underscore
#' @export
zoom_get_string <- function(zoom, zoom_levels, region) {
  zoom_levels <- sort(zoom_levels)
  out <- names(zoom_levels)[zoom >= zoom_levels]
  out <- out[length(out)]
  out <- paste(region, out, sep = "_")
  return(out)
}

#' Get the zoom name for a set of scale codes
#'
#' This function takes a set of scale codes ("CMA", "CT", "DA") and returns the
#' corresponding slider title ("Borough/City" , "Census tract", "Dissemination area")
#' in the same order in which it was fed.
#'
#' @param dfs <`character vector`> A character vector of scales codes
#' @param lang <`character`> String indicating the language to translate the
#' slider titles to. Defaults to `NULL`, which is no translation.
#'
#' @return A character vector of slider titles.
#' @export
zoom_get_name <- function(dfs, lang = NULL) {
  # Get the scales dictionary
  scales_dictionary <- get_from_globalenv("scales_dictionary")

  # Extract only the scale, remove the region
  scales <- gsub(".*_", "", dfs)

  # Error check
  if (sum(!scales %in% scales_dictionary$scale) > 0) {
    stop(glue::glue(
      "One ore multiple of `{paste0(scales, collapse = ',  ')}` ",
      "is not present in the `scales_dictionary$scale`."
    ))
  }

  # Get matching indices in desired order
  match_idx <- match(scales, scales_dictionary$scale)

  # Subset slider_title in desired order
  out <- scales_dictionary$slider_title[match_idx]

  # Return the translation
  return(sapply(out, cc_t, lang = lang, USE.NAMES = FALSE))
}

#' Get the zoom labels for a set of zoom levels
#'
#' This function takes a set of zoom levels ("CMA", "CT", "DA") and returns the
#' corresponding slider titles ("Borough/City" , "Census tract", "Dissemination area")
#'
#' @param zoom_levels <`named numeric vector`> A named numeric vector of zoom
#' levels. Usually one of the `map_zoom_levels_x`, or the output of
#' \code{\link[curbcut]{zoom_get_levels}}. It needs to be `numeric` as the function
#' will sort them to make sure the lower zoom level is first, and the highest
#' is last (so it makes sense on an auto-zoom).
#' @param lang <`character`> String indicating the language to translate the
#' slider titles to. Defaults to `NULL`, which is no translation.
#'
#' @return A character vector of slider titles.
#' @export
zoom_get_label <- function(zoom_levels, lang = NULL) {
  zl <- names(sort(zoom_levels))
  zl <- zoom_get_name(zl, lang)
  return(zl)
}

#' Get the scale code based on the name of the scales
#'
#' This function takes in the name of the scales and returns the corresponding
#' scale code. If a translation is needed, it uses the provided language to
#' translate the scale names before returning the right scale code.
#'
#' @param scales_name <`character vector`> The name of the scales, e.g.
#' `c("Borough/City", "Census Tracts", ...)`
#' @param lang <`character`> String indicating the language to translate the
#' slider titles to. Defaults to `NULL`, which is no translation.
#'
#' @return The corresponding scale code(s).
#' @export
zoom_get_code <- function(scales_name, lang = NULL) {
  # Get the scales dictionary
  scales_dictionary <- get_from_globalenv("scales_dictionary")

  # Return the scale code if no translation is needed
  if (is.null(lang) || lang == "en") {
    # Get matching indices in desired order
    match_idx <- match(scales_name, scales_dictionary$slider_title)
    # Return the subset scale code in desired order
    return(scales_dictionary$scale[match_idx])
  }

  # Translate the zoom_code if it's neither NULL nor "en"
  translation_df <- get_from_globalenv("translation_df")
  translated <-
    sapply(scales_name, \(x) translation_df$en[translation_df[[lang]] == x],
      USE.NAMES = FALSE
    )

  # Get matching indices in desired order
  match_idx <- match(translated, scales_dictionary$slider_title)
  # Return the subset scale code in desired order
  return(scales_dictionary$scale[match_idx])
}

#' Get map zoom levels for a given module and region
#'
#' This function retrieves the map zoom levels for a given module and region.
#' It retrieves the list of possible regions for the module and lets the
#' user provide a suffix to the zoom level to determine if there are
#' additional or fewer levels beyond or under the desired level. If the specified region
#' is not part of the available regions for the module, the function selects the
#' first region in the list of possible regions as they are ordered in priority.
#'
#' @param id <`character`> The id of the module to retrieve the zoom levels for,
#' e.g. `canale`.
#' @param region <`character`> The region to retrieve the zoom levels for,
#' usually `r$region()`.
#' @param suffix_zoom_levels <`character`> A suffix to the zoom level to determine
#' if there are additional or fewer levels beyond or under the desired level. If
#' the levels should stop at `CT`, then `_max_CT` would be a valid `suffi_zoom_levels`.
#' The zoom level needs to live as a `map_zoom_levels_x` in the global environment,
#' e.g. `map_zoom_levels_city_max_CT`.
#'
#' @return A list containing the zoom levels for the specified region and the
#' region itself.
#' @export
zoom_get_levels <- function(id, region, suffix_zoom_levels = "") {
  # Get the modules df
  modules <- get_from_globalenv("modules")

  # Error check
  if (!id %in% modules$id) {
    stop(glue::glue("`{id}` is not a valid `id` in the `modules` dataframe."))
  }

  # Grab the possible regions for the module
  possible_regions <- modules$regions[modules$id == id][[1]]

  # Declare a 'get map zoom level' function, and append to it the
  # `suffix_zoom_levels`, which lets the user decide if there's more to the
  # zoom level. Some modules can have a limit at CT and don't go to DA, meaning
  # the zoom level they are looking for might end with 'max_CT'.
  get_mzl <- \(reg) return(get_from_globalenv(paste0(
    "map_zoom_levels_", reg,
    suffix_zoom_levels
  )))

  # If the wanted region is not part of the available regions for the module,
  # grab the first in the list of possible regions as they are ordered in
  # priority
  region <- if (region %in% possible_regions) region else possible_regions[1]

  # Return both the region and the `map_zoom_levels_x`
  return(list(zoom_levels = get_mzl(region), region = region))
}
