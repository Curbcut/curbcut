#' Server logic for the settings UI module
#'
#' This function contains the server-side logic for the settings UI module.
#' It connects to the \code{settings_UI} function through the module ID and a
#' reactive values object, and handles various events triggered by user
#' interactions with the UI. Specifically, it opens a modal dialog for advanced
#' options, updates the region cookie if a user changes the default region,
#' retrieves the IDs for a searched location and saves it in a reactive value,
#' and clears the default location when a button is clicked.
#'
#' @param id <`character`> The ID of the module, used to connect the UI and server
#' functions. Defaults to `"settings"`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param parent_session <`session`> The session of the parent module that the
#' langauge server is embedded in (`server.R`). Usually `parent_session = session`.
#'
#' @seealso \code{\link{settings_UI}}
#' @export
settings_server <- function(id = "settings", r, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {
    # Advanced options UI - Opens a modal.
    shinyjs::onclick("advanced_options", {
      shiny::showModal(shiny::modalDialog(
        # Change the region
        adv_opt_region(id = id, region = r$region(), lang = r$lang()),
        shiny::hr(),
        adv_opt_lock_selection_UI(id = id, lang = r$lang()),
        title = cc_t(lang = r$lang(), "Advanced options"),
        footer = shiny::modalButton(cc_t(lang = r$lang(), "Dismiss"))
      ))
    })

    # If the region cookie is already in and it differs from default. Intended
    # to run only once at startup.
    shiny::observeEvent(cookie_retrieve(input, "region"),
      {
        r$region(cookie_update_value(
          input = input, name = "region",
          current_value = r$region()
        ))
      },
      once = TRUE
    )

    # Change the default region and save the cookie at a change of the region.
    shiny::observeEvent(input$region_change, {
      r$region(input$region_change)
      cookie_set(session = parent_session, name = "region", value = input$region_change)
    })

    # When the button is clicked, grab the content of the search box,
    # get the IDs for that location and save it in the reactive value.
    shiny::observeEvent(input$lock_search_button, {
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
  })
}

#' Create a settings menu UI for Curbcut
#'
#' This function creates a settings menu for a Shiny app, with several action
#' links for bookmarking, providing feedback, exporting data, subscribing to a
#' newsletter, and accessing advanced options. The UI is returned as a dropdown
#' button object from the shinyWidgets package.
#'
#' @param id A character string specifying the input ID for the settings menu.
#' Defaults to `"settings"`.
#' @param contact_email A character string specifying the email address for
#' providing feedback. Defaults to `"contact@curbcut.ca"`.
#'
#' @return A dropdown button object from the shinyWidgets package representing
#' the settings menu UI.
#'
#' @seealso \code{\link{settings_server}}
#'
#' @export
settings_UI <- function(id = "settings", contact_email = "contact@curbcut.ca") {
  # Put together the contact email in an action
  contact <- glue::glue("window.open('mailto:{contact_email}', '_blank')")

  # Return the settings menu
  icon_material_button(
    shinyWidgets::dropdownButton(
      inputId = "settings",
      shiny::a(
        id = "bookmark",
        class = "action-button shiny-bound-input",
        role = "menuitem",
        href = "#",
        shiny::icon("link", verify_fa = FALSE),
        cc_t("Bookmark"),
        onclick = copy_current_url()
      ),
      shiny::actionLink(
        inputId = shiny::NS(id, "contact"),
        label = cc_t("Contact/feedback"),
        shiny::icon("comment", verify_fa = FALSE),
        onclick = contact
      ),
      shiny::actionLink(
        inputId = shiny::NS(id, "download_data"),
        label = cc_t("Export data"),
        shiny::icon("download", verify_fa = FALSE)
      ),
      shiny::actionLink(
        inputId = shiny::NS(id, "subscribe"),
        label = cc_t("Newsletter"),
        shiny::icon("rectangle-list", verify_fa = FALSE)
      ),
      shiny::actionLink(
        inputId = shiny::NS(id, "advanced_options"),
        label = cc_t("Advanced options"),
        shiny::icon("gear", verify_fa = FALSE)
      )
    ), "more_horiz"
  )
}
