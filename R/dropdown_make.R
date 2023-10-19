#' Generate a dropdown list of variables (for use by the widget_picker)
#'
#' This function generates a dropdown list of variables based on the input. The
#' output is a named list of variable codes grouped by theme for each category in
#' the dropdown.
#'
#' @param vars <`character vector`> The codes of variables to include.
#' @param compare <`logical`> Indicates if the dropdown is for use by the
#' compare panel. In that case, the first option will be an empty selection
#' meaning nothing (no comparison to display).
#'
#' @return A named list of variable codes grouped by theme for each category in
#' the dropdown.
#' @export
dropdown_make <- function(vars, compare = FALSE) {
  # Grab the `variables` table from the global environment
  variables <- get_from_globalenv("variables")

  # Select only the variables we are interested in
  vars <- variables[variables$var_code %in% vars, ]

  # Iterate over all the themes
  out <-
    lapply(
      stats::setNames(
        unique(vars$theme),
        unique(vars$theme)
      ),
      \(cat) {
        cat_vecs <-
          vars[vars$theme == cat, c("var_code", "var_title")]

        unique_title <- unique(cat_vecs$var_title)

        lapply(unique_title, \(name) {
          cat_vecs$var_code[cat_vecs$var_title == name]
        }) |>
          stats::setNames(unique_title)
      }
    )

  # In the case the dropdown is to be located in the compare dropdown,
  # add a 'No comparison' possibility
  if (compare) out <- c("----" = " ", out)

  # Return
  return(out)
}
