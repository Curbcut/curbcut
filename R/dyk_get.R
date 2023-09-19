#' Get Did You Know Contents
#'
#' This function retrieves the content for the "Did You Know" (DYK) section of
#' a map page.
#'
#' @param id <`character`> The ID of the page in which the module will appear,
#' e.g. `alp`.
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link{vars_build}} function.
#' @param poi <`reactive`> (Optional) Point of interests. The output of
#' \code{\link{update_df}}. Default is NULL.
#' @param lang <`character`> A character string specifying the language to
#' translate the content. Defaults to NULL for no translation.
#' @param df <`reactive character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param select_id <`character`> A string indicating the ID of the currently
#' selected region (if any). Usually `r[[id]]$select_id()`
#' @param region <`character`> Character string specifying the name of the region.
#' Usually equivalent of `r$region()`.
#' @param zoom_levels <`named numeric vector`> A named numeric vector of zoom
#' levels. Usually one of the `map_zoom_levels_x`, or the output of
#' \code{\link{zoom_get_levels}}.
#' @param scales_as_DA <`character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#'
#' @return An HTML list of DYK content with the "links" attribute containing the
#' necessary arguments for the link function (except r, which is added subsequently).
#' @export
dyk_get <- function(id, vars, df, select_id, poi, region, zoom_levels,
                    scales_as_DA = scales_as_DA, lang = NULL) {
  # Start with a NULL output
  dyk_out <- NULL

  # If there are POIs, take them
  if (!is.null(poi)) dyk_out <- dyk_poi(id = id, poi = poi, lang = lang)

  # Otherwise take standard DYKs
  if (is.null(dyk_out)) {
    dyk_out <- dyk_text(
      vars = vars,
      df = df,
      select_id = select_id,
      region = region,
      zoom_levels = zoom_levels,
      scales_as_DA = scales_as_DA,
      lang = lang
    )
  }

  return(dyk_out)
}
