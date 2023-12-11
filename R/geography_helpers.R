#' Internal function for dispatching `zoom_level_selection`.
#'
#' @param var_right <`character`> Character string of the selected
#' compared variable, e.g. `housing_value`. It will be used to check on all
#' possible scale combinations which combinations have scales that all have
#' valid data.
#' @param top_scale <`character`>
#' @param avail_scale_combinations <`character vector`>
#' @param scales_as_DA <`character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. They won't
#' be taken into account when all the scales need to hold data.
#' @param ... Additional arguments to be passed to the methods.
#'
#' @return A ggplot object that represents the `delta` legend.
#' @export
zoom_level_selection <- function(var_right, top_scale, avail_scale_combinations,
                                 scales_as_DA = c("building", "street"), ...) {

  if (var_right == " ") {
    return(longest_scale_combination(top_scale, avail_scale_combinations))
  }

  # In most cases, we should be only looking at the longest string of scales,
  # as there is most likely data available for all scales.
  scale_comb_init <- longest_scale_combination(top_scale, avail_scale_combinations)
  scale_comb <- strsplit(scale_comb_init, split = "_")[[1]]
  # Remove scales that are going to be replaced by DAs
  scale_comb <- scale_comb[!scale_comb %in% scales_as_DA]
  avail_dat <- sapply(scale_comb, \(x) sapply(var_right, is_data_present_in_scale, scale = x))
  # If data is available for all the scales, return it as the wanted zoom level
  if (all(avail_dat)) return(scale_comb_init)

  # Go over all other possibilities
  # Filter only the combinations that fit with the top scale
  possible_scale_comb <- grep(
    sprintf(
      "(_%s_)|(^%s_)|(_%s$)|(^%s$)", top_scale, top_scale,
      top_scale, top_scale
    ),
    avail_scale_combinations,
    value = TRUE
  )

  # Separate scales
  scale_combs <- strsplit(possible_scale_comb, split = "_")

  # Remove scales that are going to be replaced by DAs
  scale_combs <- lapply(scale_combs, \(x) x[!x %in% scales_as_DA])

  # Which ones have all the data available in all the scales?
  scale_combs <- lapply(scale_combs, sapply, \(x) sapply(var_right, is_data_present_in_scale, scale = x))

  # If it's not all the scales that have the data, make sure it's not picked.
  # If not, sum the number of TRUEs (the highest will be the scale combinations
  # with the most scales available)
  scale_combs <- sapply(scale_combs, \(x) if (!all(x)) 0 else sum(x))

  # If they're all equal to 0, return the top scale as a single scale (single tileset)
  if (sum(scale_combs) == 0) return(top_scale)

  # Return the scale combinations with the scale combinations having the most
  # scales and all containing data
  return(possible_scale_comb[which.max(scale_combs)])
}

#' Find the longest scale combination
#'
#' This function identifies the longest scale combination string for a given
#' top scale within available scale combinations. It selects the combination
#' that contains the highest number of scales in its string.
#'
#' @param top_scale <`character`> The top scale to search for in the combinations.
#' @param avail_scale_combinations <`character vector`> A vector of available
#' scale combinations.
#' @return <`character`> The longest scale combination string for the specified
#' top scale. If there's only one matching scale, it is returned. If
#' there are multiple, the one with the most scales is returned.
longest_scale_combination <- function(top_scale, avail_scale_combinations) {
  contains <- grep(
    sprintf(
      "(_%s_)|(^%s_)|(_%s$)|(^%s$)", top_scale, top_scale,
      top_scale, top_scale
    ),
    avail_scale_combinations,
    value = TRUE
  )
  # If there's only one choice, return it
  if (length(contains) == 1) {
    return(contains)
  }

  # If not, pick the map zoom levels which offers the highest number of
  # total scales on the autozoom
  underscore_nb <- sapply(contains, function(x) length(gregexpr("_", x)[[1]]))

  # Grab the string with the highest number of scales
  scales_string <- names(which.max(underscore_nb))[[1]]

  return(scales_string)
}

#' Map single scales to their longest combinations
#'
#' This function maps each unique top scale in the available scale combinations
#' to its longest scale combination. It processes each top scale and finds the
#' combination with the most scales included.
#'
#' @param avail_scale_combinations <character vector> A vector of available
#' scale combinations.
#' @return <list> A list where each element represents the longest scale
#' combination for a single top scale.
single_scales_combination <- function(avail_scale_combinations) {

  # Grab the top scales
  top_scales <- gsub("_.*", "", avail_scale_combinations)
  top_scales <- unique(top_scales)

  # Loop over all top scales to only grab the combinations of the top scale
  # that includes the most scales (priority to CSD_CT_DA_building over CSD_CT).
  sapply(top_scales, longest_scale_combination, avail_scale_combinations,
         USE.NAMES = FALSE)

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
  div_selector <- paste0("#", ns_doubled(page_id = id, container_div))

  # content must have a unique ID' so that the shinyjs::hide of previous call
  # to this function do not impact the future ones.
  content_id <- sprintf("%s_%s", container_div, as.numeric(Sys.time()))
  div_content_id <- ns_doubled(page_id = id, content_id)

  # Add a container so that we can directly remove the previous message if
  # there is a new message to show.
  div_container_id <- ns_doubled(page_id = id, sprintf("%s_container", container_div))

  # Button ID
  undo_button_id <- ns_doubled(page_id = id, undo_btn_id)

  # Place the message with an undo button
  shiny::removeUI(selector = paste0("#", div_container_id))
  shiny::insertUI(
    selector = div_selector,
    where = "afterBegin",
    ui = shiny::div(
      id = div_container_id,
      shiny::div(
        id = div_content_id,
        shiny::p(class = "sus-sidebar-widget-warning-text", shiny::HTML(message)),
        shiny::actionButton(
          inputId = undo_button_id, label = cc_t("Undo", lang = r$lang()),
          class = "sus-sidebar-widget-warning-text-button"
        )
      )
    )
  )
  shinyjs::show(
    id = content_id,
    anim = TRUE,
    animType = "fade",
    time = fading_s
  )

  # Observe undo button. Do not use shiny::observeEvent because it will
  # trigger the undo_fun() anytime the button is re-initialized.
  shinyjs::onclick(id = undo_btn_id, {
    undo_fun()
    shinyjs::hide(
      id = content_id,
      anim = TRUE,
      animType = "fade",
      time = fading_s
    )
  })

  # Remove the message
  shinyjs::delay(duration_ms, {
    shinyjs::hide(
      id = content_id,
      anim = TRUE,
      animType = "fade",
      time = fading_s
    )
  })
  shinyjs::delay(duration_ms + (fading_s * 100 * 2), {
    shiny::removeUI(selector = paste0("#", div_content_id))
  })
}
