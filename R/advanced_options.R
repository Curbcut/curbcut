#' Create a shiny radio button input for selecting a geographical region.
#'
#' This function creates a shiny radio button input for selecting a geographical
#' region from a predefined list of regions. The df of regions is obtained
#' from a global environment variable named `regions_dictionary`. This list
#' contains all the regions that can be picked, with the following columns:
#' `pickable` if the user can select them in the advance options, the `name`
#' column, and the `region` code.
#'
#' @param region <`character`> A character string specifying the currently
#' selected region.
#' @param lang <`character`> A character string specifying the language in
#' which the names of the regions should be translated. Defaults to `NULL`
#' which means no translation.
#'
#' @return A shiny radio button input object.
#' @export
adv_opt_region <- function(region, lang = NULL) {

  # Get the regions dictionary from the global environment
  regions_dictionary <- get_from_globalenv("regions_dictionary")
  pickable_regions <- regions_dictionary[regions_dictionary$pickable, ]

  # Translate and get the text labels + the values
  choices_txt <-
    sapply(unname(pickable_regions$name), cc_t, lang = lang, USE.NAMES = FALSE)
  choices_values <- pickable_regions$region

  # Get the shiny input
  shiny::radioButtons("region_change",
                      label = cc_t(lang = lang, "Change default geometry"),
                      inline = TRUE,
                      selected = region,
                      choiceNames = choices_txt,
                      choiceValues = choices_values)
}

#' Create a UI element for selecting and locking a default location
#'
#' This function creates a Shiny UI element that allows the user to select
#' and lock in a default location (postal code or address) for further use in
#' any Curbcut map page. If a location is locked, then at every zoom level on
#' a map the location will be automatically, as a default, the zone in which
#' the locked location falls in.
#'
#' @param lang A string indicating the language of the UI element.
#'
#' @return A Shiny UI element for selecting and locking a default location.
#' @export
adv_opt_lock_selection <- function(lang) {

  # Get the default location from the global environment
  default_random_address <- get_from_globalenv("default_random_address")

  shiny::tagList(
    # Lock in address of zone for select_ids
    shiny::strong(cc_t(lang = lang, "Enter and save a default location (postal ",
                       "code or address)")),
    shiny::HTML("<br><i>", cc_t(lang = lang,
                                "Default location will be saved until ",
                                "manually cleared from advanced options"), "</i>"),
    shiny::HTML(paste0('<div class="shiny-split-layout">',
                       '<div style="width: 80%; margin-top: var(--padding-v-md); ',
                       'width:auto;">',
                       shiny::textInput(
                         inputId = "lock_address_searched",
                         label = NULL,
                         placeholder = default_random_address),
                       '</div><div style="width: 20%">',
                       shiny::actionButton(
                         inputId = "lock_search_button",
                         label = shiny::icon("check", verify_fa = FALSE),
                         style = "margin-top: var(--padding-v-md);"),
                       '</div></div>',
                       shiny::actionButton(
                         inputId = "cancel_lock_location",
                         label = cc_t(lang = lang, "Clear default location"),
                         icon = shiny::icon("xmark", verify_fa = FALSE),
                         style = "margin-top: var(--padding-v-md);")))
  )
}
