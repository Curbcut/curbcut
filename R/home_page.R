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
    discover_cards <- get_from_globalenv("discover_cards")
    news_cards <- get_from_globalenv("news_cards")
    modules <- get_from_globalenv("modules")

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

    # Detect discover card click and update the active page accordingly
    discover_click <- shiny::reactive(cc.landing::get_landing_discover("landing"))
    shiny::observeEvent(discover_click(), {
      # Selected row
      disc_card <- discover_cards[discover_cards$id == discover_click(), ]

      # Which tab, which selection
      tab <- if (disc_card$type == "page") disc_card$id else if (disc_card$type == "stories") "stories"
      select_id <- disc_card$select_id

      # Update the active tab and the selection
      shiny::updateTabsetPanel(
        session = r$server_session(), inputId = "cc_page",
        selected = tab
      )
      shinyjs::delay(500, {
        r[[tab]]$select_id(select_id)
      })
    }, ignoreNULL = TRUE)

    # Detect discover card click and update the active page accordingly
    news_click <- shiny::reactive(cc.landing::get_landing_news("landing"))
    shiny::observeEvent(news_click(), {
      # Selected row
      news_card <- news_cards[news_cards$id == news_click(), ]

      if (grepl("^https://", news_card$link)) {
        session$sendCustomMessage(type = 'openURL', message = news_card$link)
      } else if (news_card$link %in% modules$id) {
        shiny::updateTabsetPanel(
          session = r$server_session(), inputId = "cc_page",
          selected = news_card$link
        )
      }

    }, ignoreNULL = TRUE)

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
#' @param placeholder_video_src <`character`> External link to a publicly available
#' mp4 video. The video will be used as the placeholder, until the user click on
#' to watch the intro video. e.g. `https://s3.amazonaws.com/curbcut.public.resources/mtl_vid_placeholder.mp4`
#' @param video_src <`character`> External link to a publicly available
#' mp4 video. This is the full length official video the user will see when
#' they click on to watch the intro video. e.g. `https://s3.amazonaws.com/curbcut.public.resources/mtl_vid_placeholder.mp4`
#' @param lang_init <`character`> Language which should be used to initiate the
#' app.
#'
#' @return A Shiny UI object for the home page.
#' @export
home_UI <- function(id = "home", placeholder_video_src, video_src, lang_init = "en") {
  # Get modules from the global environment
  modules <- get_from_globalenv("modules")
  pages <- modules[c("id", "theme", "nav_title")]

  # Get translations from the global environment and filter it
  translation_df <- get0("translation_df")
  translation_df <-  if (is.null(translation_df)) {
    tibble::tibble(en = unlist(pages), fr = unlist(pages))
  } else {
    translation_df[translation_df$en %in% unlist(pages), ]
  }

  # Subset discover cards to not send too much data to the landing UI
  discover_cards <-  get_from_globalenv("discover_cards")
  card_types <- unique(discover_cards$type)
  discover_cards <- lapply(card_types, \(type) {
    out <- discover_cards[discover_cards$type == type, ]
    out[sample(nrow(out), 2), ]
  })
  discover_cards <- Reduce(rbind, discover_cards)

  # Create landing page
  cc.landing::landing_input(
    inputId = shiny::NS(id, "landing"),
    pages = pages,
    c_city_svg = get_from_globalenv("c_city_svg"),
    news_cards = get_from_globalenv("news_cards"),
    discover_cards = discover_cards,
    team_cards = get_from_globalenv("team_cards"),
    contributors = get_from_globalenv("contributors"),
    translation_df = translation_df,
    collabs = get_from_globalenv("collabs"),
    lang = lang_init,
    placeholder_video_src = placeholder_video_src,
    video_src = video_src
  )
}
