#' Get Did You Know Contents
#'
#' This function retrieves the content for the "Did You Know" (DYK) section of
#' a map page.
#'
#' @param id <`character`> The ID of the page in which the module will appear,
#' e.g. `canale`.
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link{vars_build}} function.
#' @param poi <`reactive`> (Optional) Point of interests. The output of
#' \code{\link{update_df}}. Default is NULL.
#' @param lang <`character`> A character string specifying the language to
#' translate the content. Defaults to NULL for no translation.
#'
#' @return An HTML list of DYK content with the "links" attribute containing the
#' necessary arguments for the link function (except r, which is added subsequently).
#' @export
dyk_get <- function(id, vars, poi, lang = NULL) {

  # For the moment, only include POIs
  if (!is.null(poi)) {

    # Get POIs; currently just Stories. Return nothing if the `stories` df is
    # missing.
    stories <- get0("stories", envir = .GlobalEnv)
    if (is.null(stories)) {
      return(NULL)
    }
    pois <- stories[c("ID", "name_id", "preview")]

    # Grab two stories
    out <- pois[pois$name_id %in% poi, ]
    out <- out[min(1, nrow(out)):min(2, nrow(out)),]

    # Make the a tag links as if they were action buttons
    previews_links <- lapply(seq_along(out$name_id), \(x) {
      button_id <- ns_doubled(page_id = id, element = sprintf("dyk_%s", x))

      shiny::tags$li(cc_t(out$preview[x], lang = lang),
                       shiny::tags$a(id = button_id,
                                     href = "#",
                                     class = "action-button shiny-bound-input",
                                     curbcut::cc_t("[LEARN MORE]", lang = lang))
      )
    })

    # Arguments necessary for the `link` function (except `r` which is added
    # subsequently)
    link_attrs <- lapply(seq_along(out$name_id),
                         \(x) list(page = "stories", select_id = out$ID[x]))

    # Construct the HTML list
    previews_links <- shiny::tags$ul(previews_links)

    # Flag that these are links
    attr(previews_links, "links") <- link_attrs

    # Return
    return(previews_links)

  }

  return(NULL)

}
