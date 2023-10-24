#' Rounds down a zoom level to the nearest half-integer
#'
#' This function takes a zoom level as input and returns the zoom level rounded
#' down to the nearest half-integer. For example, a zoom level of 2.3 would be
#' rounded down to 2, while a zoom level of 3.7 would be rounded down to 3.5.
#'
#' @param zoom <`numeric`> a numeric value representing the map zoom state
#'
#' @return A numeric value representing the rounded-down zoom level
#' @export
zoom_get <- function(zoom) floor(zoom * 2) / 2

#' Generate zoom string based on given zoom level and region name
#'
#' Given a named numeric vector of zoom levels, a numeric value representing the current
#' zoom level, this function returns a string representing the zoom level
#' The zoom level is determined by finding the highest value in the zoom levels
#' vector that is less than or equal to the current zoom level.
#'
#' @param zoom <`numeric`> A numeric value representing the current zoom level
#' @param zoom_levels <`named numeric vector`> A named numeric vector of zoom
#' levels. Usually one of the `map_zoom_levels_x`, or the output of
#' \code{\link{geography_server}}. It needs to be `numeric` as the function
#' will sort them to make sure the lower zoom level is first, and the highest
#' is last (so it makes sense on an auto-scale).
#'
#' @return A character string representing the zoom level and region name,
#' separated by an underscore
#' @export
zoom_get_string <- function(zoom, zoom_levels) {
  zoom_levels <- sort(zoom_levels)
  out <- names(zoom_levels)[zoom >= zoom_levels]
  out <- out[length(out)]
  return(out)
}

#' Get the zoom name for a set of scale codes
#'
#' This function takes a set of scale codes ("CMA", "CT", "DA") and returns the
#' corresponding slider title ("Borough/City" , "Census tract", "Dissemination area")
#' in the same order in which it was fed.
#'
#' @param scales <`character vector`> A character vector of scales codes
#' @param lang <`character`> String indicating the language to translate the
#' slider titles to. Defaults to `NULL`, which is no translation.
#'
#' @return A character vector of slider titles.
#' @export
zoom_get_name <- function(scales, lang = NULL) {
  # Get the scales dictionary
  scales_dictionary <- get_from_globalenv("scales_dictionary")

  # Error check
  if (sum(!scales %in% scales_dictionary$scale) > 0) {
    scales_collapsed <- paste0(scales, collapse = ",  ")
    stop(glue::glue_safe(
      "One ore multiple of `{scales_collapsed}` ",
      "is not present in the `scales_dictionary$scale`."
    ))
  }

  # Get matching indices in desired order
  match_idx <- match(scales, scales_dictionary$scale)

  # Subset slider_title in desired order
  out <- scales_dictionary$slider_title[match_idx]

  # Translate only inside a reactive context (if not the slider titles start
  # with spans, giving a weird user experience)
  if (!is.null(shiny::getDefaultReactiveDomain())) {
    out <- sapply(out, cc_t, lang = lang, USE.NAMES = FALSE)
  }

  # Return
  return(out)
}

#' Get the zoom labels for a set of zoom levels
#'
#' This function takes a set of zoom levels ("CMA", "CT", "DA") and returns the
#' corresponding slider titles ("Borough/City" , "Census tract", "Dissemination area")
#'
#' @param zoom_levels <`named numeric vector`> A named numeric vector of zoom
#' levels. Usually one of the `map_zoom_levels_x`, or the output of
#' \code{\link{geography_server}}. It needs to be `numeric` as the function
#' will sort them to make sure the lower zoom level is first, and the highest
#' is last (so it makes sense on an auto-scale).
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
  translation_df <- get0("translation_df")
  if (is.null(translation_df)) {
    translation_df <- tibble::tibble(en = scales_name, fr = scales_name)
  }

  translated <-
    sapply(scales_name, \(x) {
      out <- translation_df$en[translation_df[[lang]] == x]
      if (length(out) == 0) {
        return(x)
      }
      return(out)
    },
    USE.NAMES = FALSE
    )

  # Get matching indices in desired order
  match_idx <- match(translated, scales_dictionary$slider_title)
  # Return the subset scale code in desired order
  return(scales_dictionary$scale[match_idx])
}
