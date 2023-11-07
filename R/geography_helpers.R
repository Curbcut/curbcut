#' Calculate the map zoom level based on top scale value
#'
#' @param top_scale <`character`> The current top scale value.
#' @param avail_scale_combinations <`character vector`> Available scale combinations
#' for the top scale.
#' @return A numeric vector representing the map zoom levels.
calculate_map_zoom_level <- function(top_scale, avail_scale_combinations) {

  # Function to return the got mzl
  return_got_mzl <- \(x) get_from_globalenv(sprintf("mzl_%s", x))

  # Filter only the map zoom levels (mzl) that have the top scale
  contains <- grep(sprintf("(_%s_)|(^%s_)|(_%s$)", top_scale, top_scale, top_scale),
                   avail_scale_combinations, value = TRUE)

  # If there's only one choice, return it
  if (length(contains) == 1) return(return_got_mzl(contains))

  # If not, pick the map zoom levels which offers the highest number of
  # total scales on the autozoom
  underscore_nb <- sapply(contains, function(x) length(gregexpr("_", x)[[1]]))

  # Grab the string with the highest number of scales
  scales_string <- names(which.max(underscore_nb))[[1]]

  # Grab the mzl corresponding, and its value
  mzl_final <- return_got_mzl(scales_string)

  # Return
  return(mzl_final)

}

#' Show a temporary message with an undo button
#'
#' This function shows a message to the user in the `container_div`,
#' and provides an undo button. The message and the button will fade away
#' after a pre-defined time.
#'
#' @param id <`character`> The ID of the page in which the bookmark_server function
#' will appear, e.g. `alp`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param input <`shiny input`> Shiny input object in the module. Usually `input`.
#' @param message <`character`> The message to be displayed.
#' @param undo_fun <`function`> Function to execute if undo button is clicked.
#' @param container_div <`character`> ID of the div where the message
#' will be inserted. This div needs to have been previously created.
#' @param undo_btn_id <`character`> Optional ID for the undo button.
#' Defaults to "undo_btn".
#' @param duration_ms <`numeric`> Duration the message is displayed for,
#' in milliseconds. Defaults to 6000.
#' @param fading_s <`numeric`> Duration the fade of the message takes, in seconds.
#' Directly fed to `time` of shinyjs::hide(). Defaults to 0.5.
#'
#' @return Places a message in the `container_div` with an undo button which,
#' when clicked on, triggers the `undo_fun`. The message stays for `duration_ms`.
#' @export
show_message <- function(id, r, input, message, undo_fun, container_div,
                         undo_btn_id = "undo_btn", duration_ms = 8000,
                         fading_s = 0.5) {
  # Selectors
  div_selector <- paste0("#", curbcut:::ns_doubled(page_id = id, container_div))

  # content must have a unique ID' so that the shinyjs::hide of previous call
  # to this function do not impact the future ones.
  content_id <- sprintf("%s_%s", container_div, as.numeric(Sys.time()))
  div_content_id <- curbcut:::ns_doubled(page_id = id, content_id)

  # Add a container so that we can directly remove the previous message if
  # there is a new message to show.
  div_container_id <- curbcut:::ns_doubled(page_id = id, sprintf("%s_container", container_div))

  # Button ID
  undo_button_id <- curbcut:::ns_doubled(page_id = id, undo_btn_id)

  # Place the message with an undo button
  shiny::removeUI(selector = paste0("#", div_container_id))
  shiny::insertUI(
    selector = div_selector,
    where = "afterBegin",
    ui = shiny::div(id = div_container_id,
                    shiny::div(
                      id = div_content_id,
                      shiny::p(class = "sus-sidebar-widget-warning-text", shiny::HTML(message)),
                      shiny::actionButton(inputId = undo_button_id, label = cc_t("Undo", lang = r$lang()),
                                          class = "sus-sidebar-widget-warning-text-button")
                    )))
  shinyjs::show(id = content_id,
                anim = TRUE,
                animType = "fade",
                time = fading_s)

  # Observe undo button. Do not use shiny::observeEvent because it will
  # trigger the undo_fun() anytime the button is re-initialized.
  shinyjs::onclick(id = undo_btn_id, {
    undo_fun()
    shinyjs::hide(id = content_id,
                  anim = TRUE,
                  animType = "fade",
                  time = fading_s)
  })

  # Remove the message
  shinyjs::delay(duration_ms, {
    shinyjs::hide(id = content_id,
                  anim = TRUE,
                  animType = "fade",
                  time = fading_s)
  })
  shinyjs::delay(duration_ms + (fading_s * 100 * 2), {
    shiny::removeUI(selector = paste0("#", div_content_id))
  })

}
