#' Trigger the `server` of pages
#'
#' This function creates a reactive environment for tracking the state of pages
#' opened in Curbcut and triggers the server module of a new page when it is
#' opened for the first time. The function takes a single argument cc_page,
#' which is a reactive expression that tracks the currently selected page.
#'
#' @param cc_page <`reactive`> Character of the currently active tab stored
#' in a \code{\link[shiny]{reactive}} to ensure reactivity on change. Usually
#' `cc_page = shiny::reactive(input$cc_page)`
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#'
#' @return Triggers the server module function if it hasn't already been
#' triggered in the same session.
#' @export
trigger_pages_server <- function(cc_page, r) {
  # Create a reactiveValues that will be used to detect which pages have
  # already been opened
  page_activity <- shiny::reactiveValues()

  # Make a list of all pages that already have been opened
  shiny::observeEvent(cc_page(), {
    page_activity$last_module <-
      unique(c(page_activity$current_module, page_activity$last_module))
    page_activity$current_module <- c(cc_page(), page_activity$last_module)
  })
  page_activity$previous_tabs <-
    shiny::eventReactive(cc_page(), page_activity$last_module)

  # Every time a page is opened, trigger the server module of the page (only
  # if the page has not been visited yet).
  shiny::observeEvent(cc_page(),
    {
      if (!cc_page() %in% page_activity$previous_tabs()) {
        do.call(paste0(cc_page(), "_server"), list(cc_page(), r = r))
      }

      # Update the URL
      shiny::updateQueryString("?")
    }
  )
}
