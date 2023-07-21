#' Server logic for the settings UI module
#'
#' This function contains the server-side logic for the settings UI module. It
#' only triggers a phantom button, which triggers the opening of the advanced
#' settings modal.
#'
#' @param id <`character`> The ID of the module, used to connect the UI and server
#' functions. Defaults to `"settings"`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#'
#' @seealso \code{\link{settings_UI}}
#' @export
settings_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    shinyjs::onclick(shiny::NS(id, "advanced_options"), {
      shinyjs::click("proxy_advanced_options", asis = TRUE)
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
  shiny::div(
    class = "settings-gear",
    shiny::actionLink(
      inputId = shiny::NS(id, "advanced_options"),
      label = NULL,
      shiny::icon("gear", verify_fa = FALSE)
    )
  )
}
