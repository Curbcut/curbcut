#' Advanced options UI and server interactions
#'
#' This function creates an advanced options modal UI with regional settings and
#' provides server-side handling of user interactions for regional preferences
#' and default location selections. It only gets triggered when the phantom
#' button "proxy_advanced_options" is clicked (which is when the settings button
#' is clicked on any page).
#'
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param input A list of all the input values from a Shiny application.
#' Usually `input = input` from server.R.
#' @param show_lang_button <`logical`> Should there be a language button? Defaults
#' to FALSE (only english).
#'
#' @return NULL. This function is called for its side effects.
#' @export
settings_advanced <- function(r = r, input = input, show_lang_button = FALSE) {
  # Advanced options UI - Opens a modal.
  shinyjs::onclick("proxy_advanced_options", {
    id <- "settings"

    shiny::showModal(shiny::modalDialog(
      if (show_lang_button)
        shiny::radioButtons(
          inputId = "language_change",
          label = cc_t(lang = r$lang(), "Change language", force_span = TRUE),
          inline = TRUE,
          selected = r$lang(),
          choiceNames = c("English", "Fran\u00e7ais"),
          choiceValues = c("en", "fr")
        ),
      if (show_lang_button) shiny::hr(),
      # Change the region
      adv_opt_region(id = id, region = r$region(), lang = r$lang()),
      shiny::hr(),
      adv_opt_lock_selection_UI(id = id, lang = r$lang()),
      title = cc_t(lang = r$lang(), "Advanced options", force_span = TRUE),
      footer = shiny::modalButton(cc_t(lang = r$lang(), "Close", force_span = TRUE))
    ))
  })

  # If the region cookie is already in and it differs from default. Intended
  # to run only once at startup.
  region_cookie <- shiny::reactive(cookie_retrieve(
    input = r$server_session()$input,
    name = "region"
  ))
  shiny::observeEvent(region_cookie(), r$region(region_cookie()),
    once = TRUE, ignoreNULL = TRUE
  )

  # Change the default region and save the cookie at a change of the region.
  shiny::observeEvent(input$region_change, {
    r$region(input$region_change)
    cookie_set(session = r$server_session(), name = "region", value = input$region_change)
  })

  # When the button is clicked, grab the content of the search box,
  # get the IDs for that location and save it in the reactive value.
  shiny::observeEvent(input$lock_search_button, {
    if (input$lock_address_searched == "" | is.null(input$lock_address_searched)) {
      return(NULL)
    }
    IDs <- adv_opt_lock_selection(
      address = input$lock_address_searched,
      lang = r$lang()
    )
    r$default_select_ids(IDs)
  })

  # Clear default location on the button click
  shiny::observeEvent(input$cancel_lock_location, {
    r$default_select_ids(NULL)
    shiny::showNotification(
      cc_t(lang = r$lang(), paste0("Default location successfully cleared")),
      type = "default"
    )
  })

  # Change language when the radio buttons are clicked
  shiny::observeEvent(input$language_change, {
    update_lang(r = r, lang = input$language_change)
  })
}
