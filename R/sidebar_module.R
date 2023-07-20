#' Shiny module server for the sidebar component of a Curbcut page
#'
#' This function creates a Shiny module server for the sidebar component of
#' Curbcut. The sidebar component provides title and explanation on the page,
#' and other widgets are included in the `...` of the UI. The `modules` object
#' must live in the global environment, as it is from where the title texts
#' are retrieved.
#'
#' @param id <`character`> The ID of the page in which this module will appear,
#' e.g. `alp`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#'
#' @return A Shiny module server for the sidebar component of Curbcut
#' @export
sidebar_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    # Simple listener if there is a change in the theme dropdown menu
    theme_dropdown_server(id = id, r = r)

    # Setting button
    settings_server(id = id, r = r)

    # Add the title
    modules <- get_from_globalenv("modules")
    title <- modules[modules$id == id, ]
    output$title <-
      shiny::renderUI(shiny::h3(cc_t(lang = r$lang(), title$title_text_title)))

    # Get the right row
    modules <- get_from_globalenv("modules")
    page <- modules[modules$id == id, ]

    shiny::observeEvent(input$expand_title_text, {
      cc.landing::update_title_box(
        session = session,
        inputId = "title_box",
        configuration = list(
          # lang = r$lang(),
          show = "true"#,
          # title_text_title = cc_t(page$title_text_title, lang = r$lang()),
          # title_text_main = cc_t(page$title_text_main, lang = r$lang()),
          # title_text_extra = cc_t(page$title_text_extra, lang = r$lang())
        )
      )
    })

    shiny::observeEvent(r$lang(), {
      cc.landing::update_title_box(
        session = session,
        inputId = "title_box",
        configuration = list(
          lang = r$lang(),
          title_text_title = cc_t(page$title_text_title, lang = r$lang()),
          title_text_main = cc_t(page$title_text_main, lang = r$lang()),
          title_text_extra = cc_t(page$title_text_extra, lang = r$lang())
        )
      )
    })
  })
}

#' Create the UI for the sidebar component of Curbcut
#'
#' This function creates a Shiny module server for the sidebar component of
#' Curbcut. The sidebar component provides title and explanation on the page,
#' and other widgets are included in the `...` of the UI. in the `bottom`
#' argument of the `sidebar_UI` are the \code{\link{zoom_server}} and the
#' \code{\link{legend_server}}.
#'
#' @param id <`character`> The ID of the page in which this module will appear,
#' e.g. `alp`.
#' @param ... Additional UI elements to be included in the sidebar (widgets). To
#' be connected to the bookmarking, these must be Curbcut widgets, e.g.
#' \code{\link{picker_UI}}.
#' @param bottom A list of UI elements to be included at the bottom of the sidebar,
#' normally the UI of \code{\link{zoom_server}} and the
#' \code{\link{legend_server}}.
#'
#' @return A tag list containing the UI elements for the sidebar component of
#' Curbcut.
#' @export
sidebar_UI <- function(id, ..., bottom = NULL) {
  modules <- get_from_globalenv("modules")
  solo_id <- gsub("-.*$", "", id)
  page <- modules[modules$id == solo_id, ]

  shiny::tagList(
    shiny::div(
      class = "left-side-bar",
      theme_dropdown_UI(id = shiny::NS(id, id)),
      settings_UI(id = shiny::NS(id, id)),
      shiny::div(
        id = "title_bar", class = "sus-map-sidebar",
        shiny::div(
          class = "sus-map-sidebar-container scrollable-div",
          shiny::div(
            class = "sus-map-sidebar-content",
            shiny::div(
              shiny::tagList(
                shiny::div(
                  id = shiny::NS(id, "title_texts"),
                  shiny::div(
                    class = "title_text",
                    shiny::uiOutput(shiny::NS(id, "title")),
                    shiny::actionLink(
                      inputId = shiny::NS(id, "expand_title_text"),
                      label = NULL,
                      shiny::icon("info-circle")
                    )
                  ),
                ),
              )
            ),
            shiny::div(
              class = "sus-sidebar-widgets",
              id = shiny::NS(id, "left_widgets"), ...
            )
          ),
          shiny::div(class = "bottom_sidebar", bottom)
        )
      )
    ),
    shiny::div(
      class = "sus-title-box scrollable-div",
      cc.landing::title_box_input(
        inputId = shiny::NS(id, "title_box"),
        theme = page$theme,
        title_text_title = page$title_text_title,
        title_text_main = page$title_text_main,
        title_text_extra = page$title_text_extra
      )
    )
  )
}

