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

  shiny::tags$button(
    class = classes_collapsed,
    id = id,
    icon_material(icon),
    shiny::tags$div(class = text_class, text)
  )
}
