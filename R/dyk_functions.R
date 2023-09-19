#### DYK FUNCTIONS #############################################################

# dyk_poi -----------------------------------------------------------------

#' Get Did You Know Contents for Points of Interest
#'
#' This function retrieves POI content for the "Did You Know" (DYK) section of
#' a map page.
#'
#' @param id <`character`> The ID of the page in which the module will appear,
#' e.g. `alp`.
#' @param poi <`character`> TKTK
#' @param lang <`character`> A character string specifying the language to
#' translate the content. Defaults to NULL for no translation.
#'
#' @return An HTML list of DYK content with the "links" attribute containing the
#' necessary arguments for the link function (except r, which is added
#' subsequently).
#' @export
dyk_poi <- function(id, poi, lang = NULL) {

  # Get POIs; currently just Stories. Return nothing if the `stories` df is
  # missing.
  stories <- get0("stories", envir = .GlobalEnv)
  if (is.null(stories)) return(NULL)
  pois <- stories[c("ID", "name_id", "preview_en", "preview_fr")]

  # Grab two stories
  out <- pois[pois$name_id %in% poi, ]
  out <- out[min(1, nrow(out)):min(2, nrow(out)), ]

  # Construct the preview column
  preview_lang <- if (is.null(lang)) "en" else lang
  preview_col <- sprintf("preview_%s", preview_lang)

  # Text with link
  previews <- lapply(seq_along(out$name_id), \(x) {
    dyk_link(id = id, element_id = x, text = out[[preview_col]][x],
             page = "stories", lang = lang, select_id = out$ID[x])
  })

  return(previews)

}

