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
#' @param window_size <`numeric vector`> Numeric vector, normally
#' `r$server_session()$input$dimension` giving window dimension. If on mobile (<768px),
#' the tutorial will not be shown.
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
tutorial_trigger <- function(id, session, server_session, skip_elements, lang = NULL,
                             window_size) {
  # Build the correct namespace
  build_element <- function(element) {
    sprintf("#%s", session$ns(element))
  }

  if (is.null(window_size)) {
    return(NULL)
  }
  if (window_size[1] <= 768) {
    return(NULL)
  }

  # Default elements which need the tutorial
  default_elements <-
    data.frame(
      element = c(
        build_element("map_div"),
        build_element("title_texts"),
        build_element("left_widgets"),
        build_element("compare_panel"),
        build_element("zoom_div"),
        build_element("legend_div"),
        build_element("explore_full"),
        build_element("floating-panel-content"),
        build_element("tutorial")
      ),
      intro = c(
        cc_t(
          "Curbcut maps are interactive: you can scroll, zoom in and out, and ",
          "click on areas for more information.",
          lang = lang
        ),
        cc_t(
          "A Curbcut page explores a specific theme. Here you'll find information",
          " about the theme and the data used on the page.",
          lang = lang
        ),
        cc_t(
          "Choose the variables and time frame you want, and the map will update.",
          lang = lang
        ),
        cc_t(
          "The legend shows how the selected variables are displayed on the map.",
          lang = lang
        ),
        cc_t(
          "Curbcut maps automatically update the scale as you zoom in and out. ",
          "If you want to control the scale manually, click off ",
          "\u2018Auto-scale\u2019 and move the slider yourself.",
          lang = lang
        ),
        cc_t(
          "If you want to compare your main variable with a second variable, ",
          "choose one here and the map will update to show the relationship ",
          "between the two.",
          lang = lang
        ),
        cc_t(
          "Explore patterns and relationships in the variables you've selected. ",
          "The text and graph here automatically update as you navigate the page.",
          lang = lang
        ),
        cc_t(
          "Use these buttons to switch between map view and table view. ",
          "Both show the same information!",
          lang = lang
        ),
        cc_t("Congratulations, you've completed the tutorial! You can revisit ",
          "it by clicking this button.",
          lang = lang
        )
      ),
      position = c(
        "auto", "right", "right", "auto", "auto", "left", "left",
        "auto", "auto"
      ),
      title = sapply(
        c(
          "Map", "Title text", "Variable selection", "Legend", "Scale",
          "Compare", "Explore", "Switch views", "Tutorial"
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
