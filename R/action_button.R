#' Create an action button with additional styling, icon support, and text
#' customization
#'
#' This function creates a custom action button for use in Curbcut. The
#' button supports additional CSS classes, as well as the inclusion of a Material
#' Design icon, custom icon styling, and customizable text styling.
#'
#' It is a reemplementation of \code{\link[shiny]{actionButton}}.
#'
#' @param id <`character`> A unique identifier for the button.
#' @param classes <`character vector`> Additional CSS classes to apply to the
#' button. Default is an empty character vector.
#' @param icon <`character`> A character string specifying the name of the
#' Material Design icon to be used in the button. Default is NULL for no icons. The
#' icon is made through \code{\link{icon_material}}.
#' @param icon_style <`character`> Additional CSS classes to apply to the icon.
#' Default is NULL for no additional classes.
#' @param text_class <`character`> Additional CSS classes to apply to the button
#' text. Default is NULL for no additional classes.
#' @param text <`character`> Button text. Defaults to NULL for no text.
#'
#' @return A Shiny tags$button (with the `action-button` class making it
#' essentially an \code{\link[shiny]{actionButton}}) element with the specified
#' attributes and styling.
#' @export
action_button <- function(classes = c(), id, icon = NULL, icon_style = NULL,
                          text_class = NULL, text = NULL) {
  # Paste all classes together
  all_classes <- c("action-button", classes)
  classes_collapsed <- paste0(all_classes, collapse = " ")

  value <- shiny:::restoreInput(id = id, default = NULL)
  shiny::tags$button(
    id = id,
    class = classes_collapsed,
    `data-val` = value,
    icon_material(icon),
    shiny::tags$div(class = text_class, text),
    type = "button"
  )
}

#' Create a task button for launching longer-running operations with UI feedback
#'
#' This function is a reimplementation of \code{bslib::input_task_button},
#' tailored for the Curbcut platform. It creates a button designed for triggering
#' actions or recomputations that might take some time, such as data processing
#' or loading operations. Like \code{bslib::input_task_button}, it integrates with
#' \code{shiny::bindEvent()} (or \code{shiny::eventReactive()} and
#' \code{shiny::observeEvent()}) to manage reactive operations. The button prevents
#' additional clicks during its operation, displaying a customizable progress message
#' and icon, and automatically resets after completion.
#'
#' @param id <`character`> The input slot that will be used to access the value.
#' @param classes <`character vector`> Additional CSS classes to apply to the
#' button, enhancing its default appearance. Default is an empty character vector.
#' @param icon <`character`> An optional Material Design icon name for the button's
#' ready state. Defaults to NULL for no icon. Utilizes \code{\link{icon_material}} for
#' icon creation.
#' @param icon_style <`character`> Additional CSS classes for customizing the icon's
#' appearance. Default is NULL.
#' @param text_class <`character`> Additional CSS classes for customizing the button
#' text appearance. Default is NULL.
#' @param text <`character`> Label of the button while in its ready (clickable) state.
#' Default is NULL.
#' @param text_busy <`character`> Label of the button while it is processing, providing
#' feedback that an operation is in progress. Defaults to "Processing...".
#' @param auto_reset <`logical`> Whether the button should automatically revert to its
#' initial state after the operation completes. Defaults to TRUE, enhancing user
#' experience by clearly indicating the operation's end.
#'
#' @return A Shiny button element with enhanced UI feedback for indicating operation
#' progress. It behaves similarly to \code{bslib::input_task_button}, but with
#' customization options for icon and text styling, and integrates seamlessly within
#' the Curbcut platform's UI.
#' @export
action_button_task <- function(classes = c(), id, icon = NULL, icon_style = NULL,
                               text_class = NULL, text = NULL, text_busy = cc_t("Processing..."),
                               auto_reset = TRUE) {

  # Paste all classes together
  all_classes <- c("action-button", "bslib-task-button", classes)
  classes_collapsed <- paste0(all_classes, collapse = " ")

  # Automatically put the button back to ready state
  ready_state <- if (isTRUE(auto_reset)) {
    NA
  } else {
    NULL
  }

  tags$button(
    id = id, class = classes_collapsed,
    type = "button", `data-auto-reset` = ready_state,
    bslib:::component_dependencies(), htmltools::tag(
      "bslib-switch-inline",
      rlang::list2(case = "ready", shiny::span(style = "display:flex;",
        slot = "ready", icon_material(icon), shiny::tags$div(class = text_class, text)
      ), shiny::span(style = "display:flex; opacity: 0.5",
        slot = "busy", icon_material("refresh"), shiny::tags$div(class = text_class, text_busy)
      ))
    )
  )
}
