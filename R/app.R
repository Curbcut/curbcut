#' Curbcut Shiny Server Function
#'
#' This function initiates a shiny server to render Curbcut, an urban
#' sustainability exploration platform. It includes reactive variables
#' initialization, title page updating, bookmarking, modules loading,
#' advanced settings handling, and a heartbeat function to keep the app alive.
#'
#' @param lang_init <`character`> Language which should be used to initiate the
#' app. Defaults to english.
#' @param show_lang_button <`logical`> Should there be a language button? Defaults
#' to FALSE (only english).
#'
#' @return Nothing. This function initiates the Shiny server for the
#' Curbcut application, and its results are side effects (i.e., launching the
#' application).
#' @export
server <- function(lang_init = "en", show_lang_button = FALSE) {
  shiny::shinyServer(function(input, output, session) {


    # # Create a reactive value to track whether the modal has already been shown
    # modalShown <- shiny::reactiveVal(FALSE)
    # # Set up a reactive timer that fires after 15 seconds (15000 ms)
    # autoInvalidate <- shiny::reactiveTimer(15000, session)
    # # Observer that triggers when the timer fires
    # shiny::observeEvent(autoInvalidate(), {
    #   # Depend on the reactive timer; this code runs after 15 seconds
    #
    #   # If the modal hasn’t been shown yet, show it now
    #   if (!modalShown()) {
    #     modalShown(TRUE)  # Mark the modal as shown so it doesn't repeat
    #     shiny::showModal(
    #       shiny::modalDialog(
    #         # Modal header with the main message
    #         shiny::h2(cc_t(
    #           lang = r$lang(),
    #           "Are you an active user of Curbcut? Your insights will drive our next big steps!",
    #           force_span = TRUE
    #         )),
    #         # Insert additional HTML content inside the modal
    #         HTML('<div id="mc_embed_shell">
    #   <div id="mc_embed_signup">
    #   <form action="https://curbcut.us11.list-manage.com/subscribe/post?u=b9df261ebcf34acc88a4aab38&amp;id=5187f08559&amp;f_id=0085d7e3f0" method="post" id="mc-embedded-subscribe-form" name="mc-embedded-subscribe-form" class="validate" target="_self" novalidate="">
    #   <div id="mc_embed_signup_scroll">
    #   <div class="indicates-required"><span class="asterisk">*</span> indicates required</div>
    #   <div class="mc-field-group"><label for="mce-EMAIL">Email Address <span class="asterisk">*</span></label><input type="email" name="EMAIL" class="required email" id="mce-EMAIL" required="" value=""></div><div class="mc-field-group"><label for="mce-FNAME">First Name </label><input type="text" name="FNAME" class=" text" id="mce-FNAME" value=""></div><div class="mc-field-group"><label for="mce-LNAME">Last Name </label><input type="text" name="LNAME" class=" text" id="mce-LNAME" value=""></div>
    #   <div hidden=""><input type="hidden" name="tags" value="10285982"></div>
    #   <div id="mce-responses" class="clear foot">
    #   <div class="response" id="mce-error-response" style="display: none;"></div>
    #   <div class="response" id="mce-success-response" style="display: none;"></div>
    #   </div>
    #   <div aria-hidden="true" style="position: absolute; left: -5000px;">
    #   /* real people should not fill this in and expect good things - do not remove this or risk form bot signups */
    #   <input type="text" name="b_b9df261ebcf34acc88a4aab38_5187f08559" tabindex="-1" value="">
    #   </div>
    #   <div class="optionalParent">
    #   <div class="clear foot">
    #   <input type="submit" name="subscribe" id="mc-embedded-subscribe" class="button" value="Help us shape the product!">
    #   </div>
    #   </div>
    #   </div>
    #   </form>
    #   </div>
    #   </div>'),
    #         # Allow closing by clicking outside or pressing Esc
    #         easyClose = TRUE,
    #         # Modal footer with an explicit Close button
    #         footer = shiny::modalButton(cc_t(
    #           lang = r$lang(),
    #           "Close",
    #           force_span = TRUE
    #         ))
    #       )
    #     )
    #   }
    # }, ignoreInit = TRUE)


    ## Reactive variables --------------------------------------------------------
    r <- r_init(
      server_session = session,
      lang_init = lang_init,
      prev_norm = shiny::reactiveVal(FALSE)
    )

    ## Page title change, depending on page visited ------------------------------
    site_name <- get_from_globalenv("site_name")
    title_page_update(
      r = r,
      active_page = shiny::reactive(input$cc_page),
      site_name = site_name
    )

    ## Bookmark ------------------------------------------------------------------
    use_bookmark(r = r)

    ## Modules -------------------------------------------------------------------
    trigger_pages_server(shiny::reactive(input$cc_page), r = r, output = output)

    ## Advanced options ----------------------------------------------------------
    settings_advanced(r = r, input = input, show_lang_button)

    ## Heartbeat function to keep app alive --------------------------------------
    heartbeat(r = r, input = input)

    ## Clean up 'tmp' folder on session end ------------------------------------
    session$onSessionEnded(function() {
      unlink(session$token, recursive = TRUE)
    })


  })
}

#' Generate User Interface for Ready Modules
#'
#' This function prepares a list of user interfaces (UIs) for all pages in the
#' modules data.frame. The module UIs are grouped by their theme and presented
#' in alphabetical order. Each module UI is created using its corresponding
#' key and title.
#'
#' @param modules <`data.frame`> Tibble of all the pages.
#'
#' @return A list of translated shiny UI elements.
#' @export
modules_panel <- function(modules = get_from_globalenv("modules")) {
  # Get unique themes and arrange them for each theme
  unique_themes <- unique(modules$theme)[!is.na(unique(modules$theme))]
  mods_rdy <-
    sapply(unique_themes, \(x) {
      thm <- modules[modules$theme == x, ]
      ids <- thm$id
      names(ids) <- thm$nav_title
      ids
    }, simplify = FALSE, USE.NAMES = TRUE)

  # Alphabetical order
  mods_rdy <- mods_rdy[names(mods_rdy)[order(names(mods_rdy))]]

  # Iterate over all the modules to create their
  list_args <-
    lapply(names(mods_rdy), function(theme) {
      c(
        theme,
        lapply(names(mods_rdy[[theme]]), function(module) {
          name <- curbcut::cc_t(module)
          key <- unname(mods_rdy[[theme]][module])
          shiny::tabPanel(
            name,
            do.call(paste0(key, "_UI"), list(key)),
            value = key
          )
        })
      )
    })

  # navbarMenu creation
  out <- lapply(list_args, \(x) do.call(shiny::navbarMenu, x))

  # Translate and return
  lapply(out, function(x) {
    x$title <- curbcut::cc_t(x$title)
    x$menuName <- curbcut::cc_t(x$menuName)
    x
  })
}

#' Generate Curbcut Shiny User Interface
#'
#' This function generates the main Shiny UI for the Curbcut application.
#' The UI includes elements such as the import of package dependencies,
#' application styles, Curbcut scripts, Google Analytics, metadata for sharing,
#' and the creation of the pages UI.
#'
#' @param site_name <`character`>¨Name of the site with the city, e.g. `Curbcut Montréal`.
#' @param h1_first_line <`character`> A list, with language, of what will be on
#' the first line of the H1 header. EXPLORE THE `X` REGION. ex.
#' `list(en = "MONTREAL", fr = "RÉGION DE")`
#' @param h1_second_line <`character`> A list, with language, of what will be on
#' the second line of the H1 header. EXPLORE THE MONTREAL `X`, ex.
#' @param web_description <`character`> The description of the website for metadata.
#' e.g. `"Curbcut est une plateforme d'exploration approfondie, dynamique et intuitive de la durabilité urbaine."`
#' @param web_title <`character`> The title of the website for metadata.
#' e.g. `Curbcut Montréal | Vers une ville durable`
#' @param placeholder_video_src <`character`> External link to a publicly available
#' mp4 video. The video will be used as the placeholder, until the user click on
#' to watch the intro video. e.g. `https://s3.amazonaws.com/curbcut.public.resources/mtl_vid_placeholder.mp4`
#' @param video_src <`character`> External link to a publicly available
#' mp4 video. This is the full length official video the user will see when
#' they click on to watch the intro video. e.g. `https://s3.amazonaws.com/curbcut.public.resources/mtl_vid_placeholder.mp4`
#' @param twitter_handler <`character`> The Twitter handle associated with the
#' website for metadata.e.g. `"@curbcutca"`
#' @param google_analytics <`character`> The Google Analytics script location
#' for tracking user activity. e.g. `"www/google_analytics.html"`. Defaults
#' to NULL for no analytics (during development, for example).
#' @param website_url <`character`> The URL of the website for metadata.
#' e.g. `"https://montreal.curbcut.ca"`
#' @param share_jpg <`character`> ABSOLUTE path to he URL of the image for sharing
#' on social media. No relative paths. e.g. `"https://montreal.curbcut.ca/share.jpg"`
#' @param apple_touch_icon <`character`> Absolute path to the 192x192px logo.
#' e.g. `"https://montreal.curbcut.ca/logo192.jpg"`
#' @param lang_init <`character`> Language which should be used to initiate the
#' app. Defaults to english.
#' @param show_lang_button <`logical`> Should there be a language button? Defaults
#' to FALSE (only english).
#' @param show_cities <`logical`> Should we be showing the list of Curbcut Cities
#' in the footer? Defaults to TRUE.
#' @param ... Additional functions
#'
#' @return A Shiny UI object that includes all elements of the Curbcut application interface.
#' @export
ui <- function(site_name, h1_first_line, h1_second_line, web_description, web_title, placeholder_video_src,
               video_src, twitter_handler, google_analytics = NULL, website_url,
               share_jpg, apple_touch_icon, lang_init = "en", show_lang_button = FALSE,
               show_cities = TRUE, ...) {
  modules_panel_calculated <- get0("modules_panel_calculated")

  shiny::tagList(

    # Import packages dependencies -----------------------------------------------
    shinyjs::useShinyjs(),

    # Remove the navbar -------------------------------------------------------
    shiny::tags$style(type = "text/css", ".navbar-shadow{display:none;}"),
    shiny::tags$style(type = "text/css", ".navbar{display:none;}"),
    if (!show_lang_button) shiny::tags$style(type = "text/css", ".language-switcher{display:none !important;}"),

    # Styling objects ------------------------------------------------------------
    use_curbcut_js(),
    use_curbcut_css(lang_init = lang_init),

    # Favicon -----------------------------------------------------------------
    shiny::tags$head(shiny::tags$link(rel = "icon", href = "favicon.ico")),

    # Google analytics --------------------------------------------------------
    if (!is.null(google_analytics)) shiny::tags$head(shiny::includeHTML(google_analytics)),

    # Sharing card ---------------------------------------------------------------
    shiny::tags$head(
      shiny::tags$meta(name = "og:url", content = website_url),
      shiny::tags$meta(name = "og:type", content = "website"),
      shiny::tags$meta(name = "og:title", content = web_title),
      shiny::tags$meta(name = "og:description", content = web_description),
      shiny::tags$meta(name = "description", content = web_description),
      shiny::tags$meta(name = "title", content = web_title),
      shiny::tags$meta(name = "og:image", content = share_jpg),
      shiny::tags$meta(name = "og:site_name", content = site_name),
      shiny::tags$meta(name = "twitter:card", content = "summary_large_image"),
      shiny::tags$meta(name = "twitter:site", content = twitter_handler),
      shiny::tags$meta(name = "twitter:creator", content = twitter_handler),
      shiny::tags$meta(name = "twitter:title", content = web_title),
      shiny::tags$meta(name = "twitter:description", content = web_description),
      shiny::tags$meta(name = "twitter:image", content = share_jpg),
      shiny::tags$link(rel = "apple-touch-icon", href = apple_touch_icon)
    ),

    # Navigation bar -------------------------------------------------------------
    shiny::actionButton("proxy_advanced_options", "", style = "display: none;"),
    do.call(
      shiny::navbarPage,
      c(
        list(
          id = "cc_page",
          windowTitle = site_name,
          title = shiny::actionLink("title", "Curbcut"),
          shiny::tabPanel(cc_t("Home"),
                          home_UI("home",
                                  placeholder_video_src = placeholder_video_src,
                                  video_src = video_src,
                                  lang_init = lang_init,
                                  show_cities = show_cities,
                                  h1_first_line = h1_first_line,
                                  h1_second_line = h1_second_line
                          ),
            value = "home"
          )
        ),
        if (!is.null(modules_panel_calculated)) modules_panel_calculated else modules_panel(),
        list(collapsible = TRUE)
      )
    )
  )
}
