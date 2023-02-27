#' Language Switching Module
#'
#' This module allows users to switch the language of Curbcut.
#'
#' @param id <`character`> A unique id for the module. Default is "language".
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file.
#' @param parent_session <`session`> The session of the parent module that the
#' langauge server is embedded in (`server.R`). Usually `parent_session = session`.
#'
#' @return A module server function that controls the behavior of the module.
#' @export
language_server <- function(id = "language", r, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {

    # Switch the language on a new session if it's in a cookie
    lang_cookie <- shiny::reactive(cookie_retrieve(input = parent_session$input,
                                                   name = "lang"))

    shiny::observeEvent(lang_cookie(),
                        update_lang(r = r, lang = lang_cookie()),
                        once = TRUE, ignoreNULL = TRUE)

    # Update language and set a cookie
    shiny::observeEvent(input$language_button, {

      # Get the new language
      new_lang <- if (r$lang() == "en") "fr" else "en"

      # Update JS and Shiny
      update_lang(r = r, lang = new_lang)

      # Set the cookie
      cookie_set(session = parent_session, name = "lang", value = new_lang)

    }, ignoreInit = TRUE)

    # Update label of the button based on current language
    shiny::observeEvent(r$lang(), {
      new_label <- if (r$lang() == "fr") "English" else "Français"
      shiny::updateActionLink(session = session,
                              inputId = "language_button",
                              label = lang_button_label(new_label))
    })

  })
}

#' Create a UI element for selecting a language
#'
#' This function creates a UI element for selecting a language. It returns an
#' action link that can be clicked to activate the language selector.
#'
#' @param id <`character`> The ID of the language selector, defaults to "language".
#'
#' @return An action link that can be clicked to activate the language selector.
#' @export
language_UI <- function(id = "language") {
  shiny::actionLink(
    inputId = shiny::NS(id, "language_button"),
    style = "min-width: 112px;",
    label = "")
}

#' Create a language button label
#'
#' This function creates a label for a language button, which includes an icon
#' and a text label.
#'
#' @param text <`character`> A character string containing the text to be
#' displayed next to the icon.
#'
#' @return A character string representing the language button label.
lang_button_label <- function(text) {
  as.character(shiny::tags$span(icon_material("language"),
                                shiny::tags$span(text)))
}

