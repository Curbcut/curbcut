#' Server for tutorial triggering
#'
#' This function defines a Shiny module server for triggering the map tutorial.
#' The tutorial is triggered when the page is first loaded if a tutorial hasn't
#' been viewed in the last two weeks, or any time the "tutorial" element is
#' clicked.
#'
#' @param id <`character`> The ID of the page in which the legend will appear,
#' e.g. `alp`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' It's assumed that r has the following structure:
#'   - r$server_session: A server session object
#'   - r$lang: A string specifying the language for the tutorial
#' @param skip_elements <`reactive character vector`> A reactive expression that
#' returns a character vector specifying the IDs of elements to be skipped in
#' the tutorial. Default is a reactive expression that returns NULL, meaning
#' all elements are included.
#'
#' @return The function does not explicitly return a value. It creates a Shiny
#' module server that can be included in a Shiny app to manage triggering of
#' the tutorial.
#'
#' @details The module server retrieves the last tutorial date from a cookie. If
#' the tutorial hasn't been viewed in the last two weeks, it triggers the
#' tutorial after a delay of 1 second. Additionally, clicking on the "tutorial"
#' element will also trigger the tutorial. The module server uses an internal Curbcut
#' function to trigger the tutorial (`tutorial_trigger`)
#' .
#' @export
tutorial_server <- function(id, r, skip_elements = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    last_tutorial_data <- shiny::reactive(cookie_retrieve(
      input = r$server_session()$input,
      name = "tutorial_date"
    ))

    shiny::observeEvent(last_tutorial_data(),
      {
        if (is.null(last_tutorial_data())) {
          return(shinyjs::delay(
            1000,
            tutorial_trigger(
              id = id, session = session,
              server_session = r$server_session(),
              skip_elements = skip_elements(),
              lang = r$lang()
            )
          ))
        }

        # Show again after 6 months
        if (Sys.time() > (as.POSIXct(last_tutorial_data()) + (60 * 60 * 24 * 180))) {
          return(shinyjs::delay(
            1000,
            tutorial_trigger(
              id = id, session = session,
              server_session = r$server_session(),
              skip_elements = skip_elements(),
              lang = r$lang()
            )
          ))
        }
      },
      ignoreNULL = FALSE
    )

    shinyjs::onclick(
      "tutorial",
      tutorial_trigger(
        id = id, session = session,
        server_session = r$server_session(),
        skip_elements = skip_elements(),
        lang = r$lang()
      )
    )
  })
}

#' @describeIn tutorial_server Create the UI for the tutorial module
#' @export
tutorial_UI <- function(id) {
  shiny::tagList(
    rintrojs::introjsUI(),
    shiny::div(id = shiny::NS(id, "tutorial"), class = "tutorial-btn",
               shiny::icon("question", verify_fa = FALSE))
  )
}
