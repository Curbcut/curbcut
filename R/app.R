#' Curbcut Shiny Server Function
#'
#' This function initiates a shiny server to render Curbcut, an urban
#' sustainability exploration platform. It includes reactive variables
#' initialization, title page updating, bookmarking, modules loading,
#' advanced settings handling, and a heartbeat function to keep the app alive.
#'
#' @return Nothing. This function initiates the Shiny server for the
#' Curbcut application, and its results are side effects (i.e., launching the
#' application).
#' @export
server <- function() {
  shiny::shinyServer(function(input, output, session) {

    ## Reactive variables --------------------------------------------------------
    r <- r_init(server_session = session,
                lang_init = "fr",
                prev_norm = shiny::reactiveVal(FALSE))

    ## Page title change, depending on page visited ------------------------------
    site_name <- get_from_globalenv("site_name")
    title_page_update(r = r,
                      active_page = shiny::reactive(input$cc_page),
                      site_name = site_name)

    ## Bookmark ------------------------------------------------------------------
    use_bookmark(r = r)

    ## Modules -------------------------------------------------------------------
    trigger_pages_server(shiny::reactive(input$cc_page), r = r)

    ## Advanced options ----------------------------------------------------------
    settings_advanced(r = r, input = input)

    ## Heartbeat function to keep app alive --------------------------------------
    heartbeat(input)

  })
}

#' Generate User Interface for Ready Modules
#'
#' This function prepares a list of user interfaces (UIs) for all pages in the
#' modules data.frame. The module UIs are grouped by their theme and presented
#' in alphabetical order. Each module UI is created using its corresponding
#' key and title.
#'
#' @return A list of translated shiny UI elements.
modules_panel <- function() {

  # Get unique themes and arrange them for each theme
  modules <- get_from_globalenv("modules")
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
          shiny::tabPanel(name,
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
#' @param web_description <`character`> The description of the website for metadata.
#' e.g. `"Curbcut est une plateforme d'exploration approfondie, dynamique et intuitive de la durabilité urbaine."`
#' @param web_title <`character`> The title of the website for metadata.
#' e.g. `Curbcut Montréal | Vers une ville durable`
#' @param twitter_handler <`character`> The Twitter handle associated with the
#' website for metadata.e.g. `"@curbcutca"`
#' @param google_analytics <`character`> The Google Analytics script location
#' for tracking user activity. e.g. `"www/google_analytics.html"`
#' @param website_url <`character`> The URL of the website for metadata.
#' e.g. `"https://montreal.curbcut.ca"`
#' @param share_jpg <`character`> The URL of the image for sharing on social media.
#' Not relative paths. e.g. `"https://montreal.curbcut.ca/share.jpg"`
#'
#' @return A Shiny UI object that includes all elements of the Curbcut application interface.
#' @export
ui <- function(web_description, web_title, twitter_handler, google_analytics,
               website_url, share_jpg) {
  shiny::tagList(
    # Import packages dependencies -----------------------------------------------
    shinyjs::useShinyjs(),

    # Remove the navbar -------------------------------------------------------
    shiny::tags$style(type = "text/css", ".navbar-shadow{display:none;}"),
    shiny::tags$style(type = "text/css", ".navbar{display:none;}"),

    # Styling objects ------------------------------------------------------------
    shiny::tags$head(shiny::tags$link(rel = "icon", href = "favicon.ico")),

    # Curbcut scripts
    curbcut::use_curbcut_js(),
    curbcut::use_curbcut_css(lang_init = TRUE),

    # Google analytics
    shiny::tags$head(shiny::includeHTML(google_analytics)),

    # Sharing card ---------------------------------------------------------------
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

    # Navigation bar -------------------------------------------------------------
    shiny::actionButton("proxy_advanced_options", "", style = "display: none;"),
    do.call(
      shiny::navbarPage,
      c(
        list(
          id = "cc_page",
          windowTitle = site_name,
          title = shiny::actionLink("title", "Curbcut"),
          shiny::tabPanel(curbcut::cc_t("Home"), curbcut::home_UI("home"), value = "home")
        ),
        modules_panel(),
        list(collapsible = TRUE)
      )
    )
  )
}
