#' Trigger the Curbcut map tutorial
#'
#' This function triggers a tutorial for a Curbcut map page. The tutorial
#' guides the user through the app using the rintrojs package, highlighting various
#' elements and providing custom instructions.
#'
#' @param id <`character`> A string representing the ID of the page for which
#' the tutorial will be shown.
#' @param session <`session`> A shiny session object from the server function.
#' The actual session of the module, should be  `session = session`
#' @param server_session <`session`> A session object passed to the server function. It is used
#' to manage cookies in the server-side of the Shiny application. Usually
#' `server_session = r$server_session()`.
#' @param skip_elements <`character vector`> A character vector specifying the IDs
#' of elements to be skipped in the tutorial. Default is NULL, meaning all elements
#' are included.
#' @param lang <`character`> A string specifying the language of the tutorial.
#' Defaults to NULL for no translation.
#'
#' @return The function does not explicitly return a value. It triggers the tutorial
#' using \code{\link[rintrojs]{introjs}} and sets a cookie recording the last time the tutorial
#' was seen.
#'
#' @details The function first builds a dataframe with elements to be highlighted
#' in the tutorial. Each element is identified by its ID, and associated with
#' an instruction and a title. If `skip_elements` is specified, these elements
#' are removed from the dataframe. The tutorial is then triggered using
#' \code{\link[rintrojs]{introjs}}, with instructions and titles drawn from the dataframe.
#' Finally, a cookie is set to record the last time the tutorial was seen.
tutorial_trigger <- function(id, session, server_session, skip_elements, lang = NULL) {
  # Build the correct namespace
  build_element <- function(element) {
    sprintf("#%s", session$ns(element))
  }

  # Default elements which need the tutorial
  default_elements <-
    data.frame(
      element = c(
        build_element("title_texts"),
        build_element("map_div"),
        build_element("left_widgets"),
        build_element("legend_div"),
        build_element("zoom_div"),
        build_element("compare_panel"),
        build_element("explore_content"),
        build_element("floating-panel-content"),
        build_element("tutorial")
      ),
      intro = c(
        cc_t("Curbcut is designed as a series of pages that explore ",
          "a given theme. Here you will find information about the ",
          "theme and the data used on the page.",
          lang = lang
        ),
        cc_t("All the maps within Curbcut are interactive and let users ",
          "scroll, zoom in and out, and click into areas for more ",
          "information.",
          lang = lang
        ),
        cc_t("The interactive nature of our maps means that you can ",
          "choose which variables you wish to explore through the ",
          "widgets located here.",
          lang = lang
        ),
        cc_t("The legend displays how the selected variable(s) is ",
          "being visually represented on the map with different ",
          "colours.",
          lang = lang
        ),
        cc_t("The level of zoom determines the spatial scale of what ",
          "you see or you can click off \u2018autozoom\u2019 and manually ",
          "choose the spatial scale with the slider.",
          lang = lang
        ),
        cc_t("This function allows you to select a variable to compare ",
          "with the one selected on the left-hand panel. We will show ",
          "you the potential relationship between the variables of ",
          "your choice.",
          lang = lang
        ),
        cc_t("You will see meaningful information here about the variables ",
          "selected and any potential relationships between them.",
          lang = lang
        ),
        cc_t("These buttons allow you to switch between map view and ",
          "data view. Both show the same information, either ",
          "spatialized or in table form.",
          lang = lang
        ),
        cc_t("Congratulations on completing the tutorial! If you wouldd like ",
          "to revisit any part of it, or run through the entire ",
          "tutorial again, simply click on this button.",
          lang = lang
        )
      ),
      position = c(
        "right", "auto", "right", "autto", "auto", "left", "left",
        "auto", "auto"
      ),
      title = sapply(
        c(
          "Title text", "Map", "Left-hand widgets", "Legend", "Zoom",
          "Compare menu", "Explore", "View switch", "Tutorial"
        ), cc_t,
        lang = lang, USE.NAMES = FALSE
      )
    )

  # Skip elements from the argument
  if (!is.null(skip_elements)) {
    elements_string <- paste0(skip_elements, collapse = "|")

    default_elements <-
      default_elements[!grepl(elements_string, default_elements$element), ]
  }

  # Trigger the tutorial
  rintrojs::introjs(session, options = list(
    steps = default_elements,
    nextLabel = cc_t("Next", lang = lang),
    prevLabel = cc_t("Back", lang = lang),
    doneLabel = cc_t("Done", lang = lang)
  ))

  # Set a cookie for the last time the tutorial was seen
  cookie_set(
    session = server_session, name = "tutorial_date",
    value = Sys.time()
  )
}
