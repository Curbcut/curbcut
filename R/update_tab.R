#' Update the Active Tab in a Shiny App
#'
#' This function updates the currently active tab in a Shiny app based on the
#' given parameters. It first resets the query string to blank and then updates
#' the tabset panel to reflect the desired tab.
#'
#' @param session <`session`> The current Shiny session object. Usually `r$server_session()`.
#' @param inputId <`character`> The ID of the tabset panel to be updated. Default is "cc_page".
#' @param selected <`character`> The tab value to be selected. One entry as `id` in
#' the modules table.
#'
#' @return Updates the tabset panel in the Curbcut app to reflect the specified tab.
#' @export
update_tab <- function(session, inputId = "cc_page", selected) {

  # Update the query string to blank
  shiny::updateQueryString(session = session, queryString = "")

  # Update the tab
  shiny::updateTabsetPanel(
    session = session, inputId = inputId,
    selected = selected
  )
}
