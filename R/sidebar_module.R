#' Shiny module server for the sidebar component of a Curbcut page
#'
#' This function creates a Shiny module server for the sidebar component of
#' Curbcut. The sidebar component provides title and explanation on the page,
#' and other widgets are included in the `...` of the UI. The `modules` object
#' must live in the global environment, as it is from where the title texts
#' are retrieved.
#'
#' @param id <`character`> The ID of the page in which the legend will appear,
#' e.g. `canale`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file.
#'
#' @return A Shiny module server for the sidebar component of Curbcut
#' @export
sidebar_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    modules <- get_from_globalenv("modules")

    # Get the right row
    title <- modules[modules$id == id, ]

    # More info
    shiny::observeEvent(input$more_info, {
      shinyjs::toggle("title_extra", condition = input$more_info %% 2 == 1)
      txt <- cc_t(
        lang = r$lang(),
        switch(input$more_info %% 2 + 1,
          "Learn more",
          "Hide"
        )
      )
      shiny::updateActionLink(session, "more_info", label = txt)
    })

    # Output the translated strings
    output$title <-
      shiny::renderUI(shiny::h3(cc_t(lang = r$lang(), title$title_text_title)))
    output$title_main <-
      shiny::renderUI(shiny::HTML(cc_t(lang = r$lang(), title$title_text_main)))
    output$title_extra <-
      shiny::renderUI(shiny::HTML(cc_t(lang = r$lang(), title$title_text_extra)))
  })
}

#' Create the UI for the sidebar component of Curbcut
#'
#' This function creates a Shiny module server for the sidebar component of
#' Curbcut. The sidebar component provides title and explanation on the page,
#' and other widgets are included in the `...` of the UI. in the `bottom`
#' argument of the `sidebar_UI` are the \code{\link[curbcut]{zoom_server}} and the
#' \code{\link[curbcut]{legend_server}}.
#'
#' @param id <`character`> The ID of the page in which the legend will appear,
#' e.g. `canale`.
#' @param ... Additional UI elements to be included in the sidebar (widgets), like
#' select_var_server, TKTK, etc.
#' @param bottom A list of UI elements to be included at the bottom of the sidebar,
#' normally the UI of \code{\link[curbcut]{zoom_server}} and the
#' \code{\link[curbcut]{legend_server}}.
#'
#' @return A tag list containing the UI elements for the sidebar component of
#' Curbcut.
#' @export
sidebar_UI <- function(id, ..., bottom = NULL) {
  shiny::tagList(
    shiny::div(class = "sus-map-sidebar-shadow"),
    shiny::div(
      id = "title_bar", class = "sus-map-sidebar",
      shiny::div(
        class = "sus-map-sidebar-container",
        shiny::div(
          class = "sus-map-sidebar-content sus-scroll",
          shiny::div(
            class = "sus-scroll-content",
            shiny::tagList(
              shiny::uiOutput(shiny::NS(id, "title")),
              shiny::p(shiny::uiOutput(shiny::NS(id, "title_main"))),
              shiny::p(shiny::actionLink(shiny::NS(id, "more_info"),
                class = "sus-small-link",
                cc_t(
                  "Learn more"
                )
              )),
              shinyjs::hidden(shiny::uiOutput(outputId = shiny::NS(id, "title_extra"))),
              shiny::div(class = "sus-sidebar-widgets", ...)
            )
          )
        ),
        shiny::div(class = "bottom_sidebar", bottom)
      )
    )
  )
}
