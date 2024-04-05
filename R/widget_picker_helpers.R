#' Creates divs with explanations on hover for a given list of variables.
#'
#' This function creates a list of divs, where each div contains a variable's
#' label and an explanation that appears on hover. The variables are specified
#' using their codes, which are used to look up the corresponding explanations in
#' the variables table retrieved from the global environment. If a
#' language is specified, the explanations are translated into that language
#' using the \code{\link{cc_t}} function. It is intended to be used by the
#' `widget_picker`.
#'
#' @param var_list <`named list`> A named list of variable codes. The output
#' of \code{\link{dropdown_make}} already translated.
#' @param lang <`character`> The language to translate the explanations into (optional).
#' @return A list containing a single element named 'content'. The 'content' element is a
#' character vector containing the HTML for the divs.
picker_hover_divs <- function(var_list, lang = NULL) {
  # If var_list is empty, return NULL
  if (length(var_list) == 0) {
    return(NULL)
  }

  # Grab the variables table
  variables <- get_from_globalenv("variables")

  # Get the variable list as a character vector
  vars <- unname(unlist(var_list))

  # If vars is not in `variables`, return no hover (NULL)
  if (sum(!vars %in% variables$var_code) > 1) {
    return(NULL)
  }

  # Grab the explanation
  text_hover <- sapply(vars, \(x) {
    # Get the explanation
    exp <- variables$explanation[variables$var_code == x]

    # If no explanation (unexistant variable), return an empty string
    if (length(exp) == 0) {
      return(" ")
    }

    # Return the explanation
    return(exp)
  }, USE.NAMES = FALSE)

  # In the case there is a language, translate
  if (!is.null(lang)) {
    text_hover <- sapply(text_hover, cc_t, lang = lang, USE.NAMES = FALSE)
  }

  # Get the label
  var_list_label <- (\(x) {
    # Take the themes out before grabbing the labels
    unthemed <- unname(var_list)
    labels <- unlist(sapply(unthemed, names))

    # Plug in the no comparison value if needed, and return
    c(if ("----" %in% names(var_list)) "----", labels)
  })()

  # Get the variable codes
  value <- unname(unlist(var_list))

  # Return the divs with the explanations on hover
  return(list(
    content = sprintf(
      '<div title="%s" value="%s" style="width: 100%%;">%s</div>',
      text_hover, value, var_list_label
    )
  ))
}

#' Disables variables that are not available for the maximum number of years
#'
#' Given a list of variables and a boolean flag indicating whether to disable
#' variables, this function disables variables that are not available for the
#' maximum number of years. If the disable flag is not set, then the function
#' returns a logical vector of FALSE values.
#'
#' @param id <`character`> The ID of the page in which the widget will appear,
#' e.g. `alp`.
#' @param var_list <`named list`> A named list of variable codes. The output
#' of \code{\link{dropdown_make}}.
#' @param disable <`logical`> A flag indicating whether to disable variables.
#'
#' @return A logical vector of the same length as the input variable list. The
#' elements of the output vector indicate whether each variable should be
#' disabled or not.
picker_multi_year_disable <- function(id, var_list, disable) {
  # If var_list is empty, return NULL
  if (length(var_list) == 0) {
    return(NULL)
  }

  # If we don't disable anything, returns FALSE (no disabling)
  if (!disable) {
    return(rep(FALSE, sum(lengths(var_list))))
  }

  # Get the variable list as a character vector
  var_unlist <- unname(unlist(var_list))

  # Get all the vars except the empty comparison value
  vars <- var_unlist[var_unlist != " "] # Drop the 'no' comparison value

  # If vars are not in the variables table, return no disabling
  variables <- get_from_globalenv("variables")
  if (sum(!vars %in% variables$var_code) > 0) {
    return(rep(FALSE, length(vars)))
  }

  # Get module dates
  modules <- get_from_globalenv("modules")
  module_dates <- modules$dates[modules$id == id][[1]]
  module_dates <- as.numeric(module_dates)

  # Get all the years
  all_years <- lapply(vars, \(x) var_get_info(x, what = "dates")[[1]])

  nb_dates <- length(module_dates)
  vec <- sapply(all_years, \(years_list) {

    # Convert list of years to numeric
    years_numeric <- as.numeric(years_list)

    # Compute the differences between each year in years_list and each module_date,
    # then check if any absolute difference is less than 3 (i.e., within +/-2 years)
    diff <- sapply(years_numeric, \(x) abs(x - module_dates) < 3, simplify = FALSE)

    # If there are enough dates that are +/- 2 years from the module years
    sum(sapply(diff, any)) >= nb_dates
  })

  # Reattach the 'no' comparison value
  if (" " %in% var_unlist) vec <- c(TRUE, vec)

  # Return the opposite as TRUE means disable
  return(!vec)
}
