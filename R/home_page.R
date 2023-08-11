#' Create the home server
#'
#' This function generates the server for the home page.
#' It handles page clicks, active page monitoring, language switch,
#' and bookmarks.
#'
#' @param id <`character`> A unique identifier for the home server. Normally `home`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#'
#' @return A Shiny module server for the home page.
#' @export
home_server <- function(id = "home", r) {
  shiny::moduleServer(id, function(input, output, session) {
    # Detect page clicks on other pages and update the active page accordingly
    page_click <- shiny::reactive(cc.landing::get_landing_click("landing"))
    shiny::observeEvent(page_click(),
      {
        shiny::updateTabsetPanel(
          session = r$server_session(), inputId = "cc_page",
          selected = page_click()
        )
      },
      ignoreNULL = TRUE
    )

    # Update the landing input based on the active page
    shiny::observeEvent(r$server_session()$input$cc_page, {
      active_page <- r$server_session()$input$cc_page

      turn_on_off <- if (active_page == "home") "on" else "off"

      cc.landing::update_landing(
        session = session,
        inputId = "landing",
        configuration = list(
          turn = turn_on_off
        )
      )
    })


    ## DEAL WITH LANGUAGE
    # Switch the language on a new session if it's in a cookie
    lang_cookie <- shiny::reactive(cookie_retrieve(
      input = r$server_session()$input,
      name = "lang"
    ))
    shiny::observeEvent(lang_cookie(),
      {
        # Update the website language (span + r$lang)
        update_lang(r = r, lang = lang_cookie())

        # Update the language of the landing UI
        cc.landing::update_landing(
          session = session,
          inputId = "landing",
          configuration = list(
            lang = lang_cookie()
          )
        )
      },
      once = TRUE,
      ignoreNULL = TRUE
    )

    # Detect lang button click
    lang_click <- shiny::reactive(cc.landing::get_lang_click("landing"))
    shiny::observeEvent(lang_click(),
      {
        # Update the website language (span + r$lang)
        update_lang(r = r, lang_click())
        # Set the cookie
        cookie_set(
          session = r$server_session(), name = "lang",
          value = lang_click()
        )
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    # Bookmark
    bookmark_server(id = "home", r = r)
  })
}

#' Create the home user interface
#'
#' This function generates the user interface for the home page,
#' retrieving necessary data from the global environment and constructing
#' the landing page with it.
#'
#' @param id <`character`> A unique identifier for the home UI. Normally `home`.
#'
#' @return A Shiny UI object for the home page.
#' @export
home_UI <- function(id = "home") {
  # Get modules from the global environment
  modules <- get_from_globalenv("modules")
  pages <- modules[c("id", "theme", "nav_title")]

  # Get translations from the global environment and filter it
  translation_df <- get_from_globalenv("translation_df")
  translation_df <- translation_df[translation_df$en %in% unlist(pages), ]

  # Create landing page
  cc.landing::landing_input(
    inputId = shiny::NS(id, "landing"),
    pages = pages,
    c_city_svg = get_from_globalenv("c_city_svg"),
    news_cards = get_from_globalenv("news_cards"),
    discover_cards = get_from_globalenv("discover_cards"),
    team_cards = get_from_globalenv("team_cards"),
    contributors = get_from_globalenv("contributors"),
    translation_df = translation_df,
    collabs = get_from_globalenv("collabs"),
    lang = "fr"
  )
}
