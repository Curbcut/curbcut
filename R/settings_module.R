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
#'
#' @seealso \code{\link{settings_UI}}
#' @export
settings_server <- function(id = "settings", r) {
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
    region_cookie <- shiny::reactive(cookie_retrieve(
      input = r$server_session()$input,
      name = "region"
    ))
    shiny::observeEvent(region_cookie(), r$region(region_cookie()),
      once = TRUE
    )

    # Change the default region and save the cookie at a change of the region.
    shiny::observeEvent(input$region_change, {
      r$region(input$region_change)
      cookie_set(session = r$server_session(), name = "region", value = input$region_change)
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
#' This function returns a settings menu in the form of an action link button
#' with a gear icon.The button triggers advanced options modal.
#'
#' @param id A character string specifying the input ID for the settings menu.
#' Defaults to `"settings"`.
#'
#' @return A Shiny actionLink wrapped inside an icon_material_button with a
#' gear icon.
#'
#' @seealso \code{\link{settings_server}}
#'
#' @export
settings_UI <- function(id = "settings") {
  # Return the settings menu
  shiny::actionLink(
    inputId = shiny::NS(id, "advanced_options"),
    label = NULL,
    shiny::icon("gear", verify_fa = FALSE)
  )
}
