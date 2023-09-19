#### NECESSARY STYLESHEETS
# necessary for google fonts
# tags$head(tags$link(rel = "stylesheet", type = "text/css",
#                     href = paste0("https://fonts.googleapis.com/icon?",
#                                   "family=Material+Icons"))),


#' Create a Material Design icon.
#'
#' This function creates a Material Design icon using the "material-icons" class
#' from the Google Material Design system. The "material-icons" class is a
#' predefined class that is used to style HTML elements as icons, and is in this case
#' applied to a "span" element. The Material Design system provides a set of predefined
#' icons that can be used with the "material-icons" class, and these icons can
#' be customized using CSS.
#'
#' To view the available icons that can be used with the "material-icons" class,
#' please visit \href{https://material.io/resources/icons/}{Material Symbols and Icons - Google Fonts}.
#'
#' @param icon <`character`> The name of the icon to be displayed.
#' @param style <`character`> Added CSS styling like `"color:white;"`
#'
#' @return A "span" element with the "material-icons" class and the specified icon.
#' @export
icon_material <- function(icon, style = NULL) {
  shiny::span(class = "material-icons", style = style, icon)
}

#' Create a Material Design icon for a title in the right panel.
#'
#' This function creates a Material Design icon for a title, using the
#' \code{\link{icon_material}} function. It applies a predefined set of CSS
#' styles to the icon, such as font size, color, vertical alignment, and margin.
#'
#' @param icon <`character`> The name of the icon to be displayed.
#'
#' @return A "span" element with the "material-icons" class and the specified
#' icon, styled for a title.
#' @export
icon_material_title <- function(icon) {
  icon_material(icon, style = paste0(
    "font-size: 17px; ",
    "color: var(--theme-color); ",
    "vertical-align:sub; ",
    "margin-right:.3rem"
  ))
}

#' Create a custom icon element
#'
#' This function creates a custom icon element styled in the same way as the
#' Google Material icons created by the \code{\link{icon_material}} function.
#' The icon parameter specifies the name or identifier of the custom icon to
#' be used. To add a custom icon, add it in www/icons/custom
#'
#' @param icon <`character`> The name or identifier of the custom icon to be
#' used, location in the www/icons/custom folder.
#'
#' @return An HTML <span> tag representing the custom icon element.
#' @export
icon_custom <- function(icon) {
  shiny::span(class = "custom-icons", data_custom_icon = icon)
}

#' Create a Material Design icon button.
#'
#' This function is a wrapper around the \code{\link{icon_material}} function,
#' which creates the actual icon element. The returned "tag" object can be used
#' directly in a Shiny UI to create a button with a Material Design icon.
#'
#' To view the available icons that can be used with the "material-icons" class,
#' please visit \href{https://material.io/resources/icons/}{Material Symbols and Icons - Google Fonts}.
#'
#' @param tag <`HTML element`> An HTML tag object (a list that represents an HTML
#' element, e.g. \code{\link[shinyWidgets]{dropdownButton}}) to be modified into
#' a button with a Material Design icon.
#' @param icon <`character`> A string indicating the name of the icon to be displayed.
#'
#' @return The modified "tag" object with a button and a Material Design icon.
#' @export
icon_material_button <- function(tag, icon) {
  shiny::tagSetChildren(tag,
    .cssSelector = "button",
    icon_material(icon)
  )
}
