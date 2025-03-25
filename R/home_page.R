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
    news_cards <- get_from_globalenv("news_cards", stop_if_missing = FALSE)
    modules <- get_from_globalenv("modules")

    # Detect page clicks on other pages and update the active page accordingly
    page_click <- shiny::reactive(cc.landing::get_landing_click("landing"))
    shiny::observeEvent(
      page_click(),
      {
        update_tab(session = r$server_session(), selected = page_click())
      },
      ignoreNULL = TRUE
    )

    # Detect discover card click and update the active page accordingly
    discover_click <- shiny::reactive(cc.landing::get_landing_discover(
      "landing"
    ))
    shiny::observeEvent(
      discover_click(),
      {
        # Selected row
        disc_card <- discover_cards[discover_cards$id == discover_click(), ]

        # Card type
        type <- disc_card$type

        if (type == "page") {
          update_tab(session = r$server_session(), selected = disc_card$id)
        }

        if (type == "stories") {
          update_tab(session = r$server_session(), selected = "stories")
          shinyjs::delay(500, {
            r[["stories"]]$select_id(disc_card$select_id)
          })
        }

        if (type == "dyk") {
          scale <- disc_card$scale
          scale <- if (is.na(scale)) NULL else scale

          modules <- get_from_globalenv("modules")
          default_comb <- modules$avail_scale_combinations[
            modules$id == disc_card$page
          ][[1]]
          if (!is.null(scale)) {
            default_comb <- grep(scale, default_comb, value = TRUE)
          }
          default_comb <- sprintf("mzl_%s", default_comb[1])
          zoom_levels <- get_from_globalenv(default_comb)

          link(
            r = r,
            page = disc_card$page,
            select_id = disc_card$select_id,
            date = disc_card$date[[1]],
            var_right = disc_card$var_right,
            var_left = disc_card$var_left,
            scale = scale,
            zoom_levels = zoom_levels
          )
        }
      },
      ignoreNULL = TRUE
    )

    # Detect discover card click and update the active page accordingly
    news_click <- shiny::reactive(cc.landing::get_landing_news("landing"))
    shiny::observeEvent(
      news_click(),
      {
        # Selected row
        news_card <- news_cards[news_cards$id == news_click(), ]

        if (grepl("^https://", news_card$link)) {
          session$sendCustomMessage(type = "openURL", message = news_card$link)
        } else if (news_card$link %in% modules$id) {
          update_tab(session = r$server_session(), selected = news_card$link)
        }
      },
      ignoreNULL = TRUE
    )

    # Update the landing input based on the active page
    shiny::observeEvent(r$server_session()$input$cc_page, {
      active_page <- r$server_session()$input$cc_page

      turn_on_off <- if (active_page == "home") "on" else "off"
      if (turn_on_off == "on") {
        shiny::updateQueryString(session = r$server_session(), queryString = "")
      }

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
    shiny::observeEvent(
      lang_cookie(),
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
    shiny::observeEvent(
      lang_click(),
      {
        # Update the website language (span + r$lang)
        update_lang(r = r, lang_click())
        # Set the cookie
        cookie_set(
          session = r$server_session(),
          name = "lang",
          value = lang_click()
        )
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )
  })
}

#' Create the home user interface
#'
#' This function generates the user interface for the home page,
#' retrieving necessary data from the global environment and constructing
#' the landing page with it.
#'
#' @param id <`character`> A unique identifier for the home UI. Normally `home`.
#' @param h1_first_line <`character`> A list, with language, of what will be on
#' the first line of the H1 header. EXPLORE THE `X` REGION. ex.
#' `list(en = "MONTREAL", fr = "RÉGION DE")`
#' @param h1_second_line <`character`> A list, with language, of what will be on
#' the second line of the H1 header. EXPLORE THE MONTREAL `X`, ex.
#' @param placeholder_video_src <`character`> External link to a publicly available
#' mp4 video. The video will be used as the placeholder, until the user click on
#' to watch the intro video. e.g. `https://s3.amazonaws.com/curbcut.public.resources/mtl_vid_placeholder.mp4`
#' @param video_src <`named list`> Every video must be named by the language it uses.
#' The videos are as external link to a publicly available mp4 video. This is the
#' full length official video the user will see when they click on to watch the
#' intro video. e.g. `list(en = "https://s3.amazonaws.com/curbcut.public.resources/mtl_vid_en.mp4")`
#' @param lang_init <`character`> Language which should be used to initiate the
#' app.
#' @param show_cities <`logical`> Should we be showing the list of Curbcut Cities
#' in the footer? Defaults to TRUE.
#'
#' @return A Shiny UI object for the home page.
#' @export
home_UI <- function(
  id = "home",
  h1_first_line,
  h1_second_line,
  placeholder_video_src,
  video_src,
  lang_init = "en",
  show_cities = TRUE,
  pages = get_from_globalenv("modules")[c("id", "theme", "nav_title")]
) {
  # Get translations from the global environment and filter it
  translation_df <- get0("translation_df")
  translation_df <- if (is.null(translation_df)) {
    tibble::tibble(en = unlist(pages), fr = unlist(pages))
  } else {
    translation_df[translation_df$en %in% unlist(pages), ]
  }

  # Get 'discover_cards' from the global environment
  discover_cards <- get_from_globalenv("discover_cards")

  # Initialize an empty data frame to store the final sample
  final_sample <- data.frame()

  # Filter 2 items of type 'stories'
  stories_df <- discover_cards[discover_cards$type == "stories", ]
  if (nrow(stories_df) >= 2) {
    stories_sample <- stories_df[sample(nrow(stories_df), 2), ]
    final_sample <- rbind(final_sample, stories_sample)
  } else {
    # If less than 2 'stories', take additional 'page' or 'dyk'
    num_needed <- 2 - nrow(stories_df)
    extra_df <- discover_cards[discover_cards$type %in% c("page", "dyk"), ]
    extra_sample <- extra_df[sample(nrow(extra_df), num_needed), ]
    final_sample <- rbind(final_sample, stories_df, extra_sample)
  }

  # Filter 1 item of type 'page'
  page_df <- discover_cards[discover_cards$type == "page", ]
  page_sample <- page_df[sample(nrow(page_df), 1), ]
  final_sample <- rbind(final_sample, page_sample)

  # Filter 1 item of type 'dyk' with different 'theme' from 'page'
  dyk_df <- discover_cards[
    discover_cards$type == "dyk" &
      !(discover_cards$theme %in% page_sample$theme),
  ]
  dyk_sample <- dyk_df[sample(nrow(dyk_df), 1), ]
  final_sample <- rbind(final_sample, dyk_sample)

  final_sample <- unique(final_sample)

  # Ensure there are 4 cards in the final sample
  if (nrow(final_sample) < 4) {
    num_needed <- 4 - nrow(final_sample)
    extra_df <- discover_cards[!discover_cards$id %in% final_sample$id, ]
    extra_sample <- extra_df[sample(nrow(extra_df), num_needed), ]
    final_sample <- rbind(final_sample, extra_sample)
  }

  # Randomize row placement
  final_sample <- final_sample[sample(nrow(final_sample)), ]

  # Create landing page
  cc.landing::landing_input(
    inputId = shiny::NS(id, "landing"),
    h1_first_line = h1_first_line,
    h1_second_line = h1_second_line,
    pages = pages,
    c_city_svg = get_from_globalenv("c_city_svg"),
    news_cards = get_from_globalenv("news_cards", stop_if_missing = FALSE),
    discover_cards = final_sample,
    team_cards = get_from_globalenv("team_cards"),
    contributors = get_from_globalenv("contributors"),
    translation_df = translation_df,
    collabs = get_from_globalenv("collabs"),
    lang = lang_init,
    placeholder_video_src = placeholder_video_src,
    video_src = video_src,
    show_cities = show_cities
  )
}
