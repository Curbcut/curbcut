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
dyk_poi <- function(id, poi, lang) {

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

  # Make the a tag links as if they were action buttons
  previews_links <- lapply(seq_along(out$name_id), \(x) {
    button_id <- ns_doubled(page_id = id, element = sprintf("dyk_%s", x))

    shiny::tags$li(
      # Grab the preview column, in the correct language
      out[[preview_col]][x],
      shiny::tags$a(
        id = button_id,
        href = "#",
        class = "action-button shiny-bound-input",
        curbcut::cc_t("[LEARN MORE]", lang = lang)
      )
    )
  })

  # Arguments necessary for the `link` function (except `r` which is added
  # subsequently)
  link_attrs <- lapply(
    seq_along(out$name_id),
    \(x) list(page = "stories", select_id = out$ID[x])
  )

  # Construct the HTML list
  previews_links <- shiny::tags$ul(previews_links)

  # Flag that these are links
  attr(previews_links, "links") <- link_attrs

  # Return
  return(previews_links)

}

