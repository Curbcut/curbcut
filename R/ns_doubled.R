#' Create a double nested namespace for an element
#'
#' This function generates a double nested namespace string which is useful for
#' managing any module that's created in a 'map page'. Ex. that way we could access
#' potentially the dropdown widget of housing directly from the `server.R`, which
#' is `housing-housing-ccpicker_var` (page_id-page_id-element)
#'
#' @param page_id <`character`> A character string representing the page namespace.
#' This is typically the page ID.
#' @param element <`character`> A character string representing the Shiny element's ID.
#'
#' @return A character string representing the double nested namespace for the
#' Shiny element, in the format 'page_id-page_id-element'.
ns_doubled <- function(page_id, element) {
  # Make a double nested namespace
  namespaced <- sprintf("%s-%s-%s", page_id, page_id, element)

  # Return
  return(namespaced)
}
