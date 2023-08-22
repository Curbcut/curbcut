#' Theme Dropdown Server
#'
#' This module creates an observer for detecting page clicks on the theme dropdown
#' in Curbcut. When a page is clicked, it updates the selected tab in
#' the 'cc_page' tabset panel according to the selected page.
#'
#' @param id <`character`> The ID of the page in which this module will appear,
#' e.g. `alp`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#'
#' @return This function does not return a value. It is used for its side effects
#' @export
theme_dropdown_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    # Detect page clicks on a page in the theme dropdown
    page_click <- shiny::reactive(cc.landing::get_theme_drop_click(
      inputId = shiny::NS(id, "theme_drop"),
      session = session
    ))

    shiny::observeEvent(page_click(),
      {
        shiny::updateTabsetPanel(
          session = r$server_session(), inputId = "cc_page",
          selected = page_click()
        )
      },
      ignoreNULL = TRUE
    )

    # If language changes, update the pages list
    shiny::observeEvent(r$lang(), {
      cc.landing::update_theme_drop_lang(
        session = session,
        inputId = shiny::NS(id, "theme_drop"),
        lang = r$lang())
    })

  })
}


#' Theme Dropdown UI
#'
#' This module generates a div container for a dropdown menu in Curbcut, with
#' menu items reflecting the 'theme' attributes of the different pages.
#'
#' @param id <`character`> The ID of the page in which this module will appear,
#' e.g. `alp`.
#' @return A `shiny::div` object that can be included in a Shiny UI.
#'
#' @export
theme_dropdown_UI <- function(id) {
  modules <- get_from_globalenv("modules")
  translation_df <- get_from_globalenv("translation_df")
  pages <- modules[c("id", "theme", "nav_title")]
  translation_df <- translation_df[translation_df$en %in% c(pages$theme, pages$nav_title), ]

  solo_id <- gsub("-.*$", "", id)
  theme <- pages$theme[pages$id == solo_id]

  shiny::div(
    class = "theme-dropdown",
    cc.landing::theme_drop_input(
      inputId = shiny::NS(id, "theme_drop"),
      pages = pages,
      theme = theme,
      translation_df = translation_df
    )
  )
}
