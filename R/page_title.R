#' Update the web title of Curbcut
#'
#' This function is used to update the title of a Curbcut page based on the
#' ID of the active page. The function takes in a reactive object \code{r},
#' the parent session object \code{parent_session}, the ID of the active page
#' \code{active_page}, and the current site name \code{site_name}. The function
#' then calls the \code{\link{title_page_get}} function to update the site name
#' with the title of the active page, and sends a custom message to the parent
#' session to update the page title.
#'
#' @param id <`character`> A unique id for the module. Default is "page_update".
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file.
#' @param parent_session <`session`> The session of the parent module that the
#' bookmarking module is embedded in (`server.R`). Usually `parent_session = session`.
#' @param active_page <`reactive`> The ID of the active page. Usually `input$cc_page`
#' in a \code{\link[shiny]{reactive}}, e.g. `shiny::reactive(input$cc_page)`.
#' @param site_name <`character`> The current site name. Usually initiated in
#' `global.R`.
#'
#' @export
title_page_update <- function(id = "page_update", r, parent_session,
                              active_page, site_name) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      # Grab the update title
      site_name <- title_page_get(active_page = active_page(),
                                  site_name = site_name,
                                  lang = r$lang())
      # Change the title of the page
      parent_session$sendCustomMessage("changetitle", site_name)
    })
  })
}

#' Get the title of the current page
#'
#' This function returns the title of the current page based on the active_page
#' ID. If the active_page is a valid module ID, the function will append the
#' module's title to the site_name and return it.
#'
#' @param active_page <`character`> The ID of the current page
#' @param site_name <`character`> The name of the site, usually initiated
#' in `global.R`.
#' @param lang <`character`> The language used to translate the module's title
#' (optional). Defaults to NULL.
#'
#' @return The title of the current page
title_page_get <- function(active_page, site_name, lang = NULL) {

  # Get the modules table
  modules <- get_from_globalenv("modules")

  # If the active page is part of the modules table, add its title
  if (active_page %in% modules$id) {
    page_name <- cc_t(modules$nav_title[modules$id == active_page], lang = lang)
    site_name <- sprintf("%s - %s", site_name, page_name)
  }

  # Return
  return(site_name)
}
