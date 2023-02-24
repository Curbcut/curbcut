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
  if (length(var_list) == 0) return(NULL)

  # Grab the variables table
  variables <- get_from_globalenv("variables")

  # Get the variable list as a character vector
  vars <- unname(unlist(var_list))

  # If at least one variable is not in the `variables` table, return the
  # `var_list` as is.
  if (sum(!vars %in% variables$var_code) > 0) return(var_list)

  # Grab the explanation
  text_hover <- sapply(vars, \(x) {
    # Get the explanation
    exp <- variables$explanation[variables$var_code == x]

    # If no explanation (unexistant variable), return an empty string
    if (length(exp) == 0) return(" ")

    # Return the explanation
    return(exp)
  }, USE.NAMES = FALSE)

  # In the case there is a language, translate
  if (!is.null(lang))
    text_hover <- sapply(text_hover, cc_t, lang = lang, USE.NAMES = FALSE)

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
#' @param var_list <`named list`> A named list of variable codes. The output
#' of \code{\link{dropdown_make}}.
#' @param disable <`logical`> A flag indicating whether to disable variables.
#'
#' @return A logical vector of the same length as the input variable list. The
#' elements of the output vector indicate whether each variable should be
#' disabled or not.
picker_multi_year_disable <- function(var_list, disable) {

  # If var_list is empty, return NULL
  if (length(var_list) == 0) return(NULL)

  # If we don't disable anything, returns FALSE (no disabling)
  if (!disable) return(rep(FALSE, sum(lengths(var_list))))

  # Get all the years
  vars <- unname(unlist(var_list))
  all_years <- lapply(vars, \(x) var_get_info(x, what = "dates")[[1]])

  # We select the variables that are available the same amount as the variables
  # present at the most years
  max_years <- max(lengths(all_years))
  vec <- sapply(lengths(all_years), `==`, max_years)

  # Return the opposite as TRUE means disable
  return(!vec)

}

#' Return variable based on time and input
#'
#' This function returns a variable code based on the given \code{time} and
#' \code{input}. If \code{time} is \code{NULL}, it returns the \code{input} as
#' is. If \code{input} is an empty string (" "), it returns an empty string.
#' Otherwise, it finds the closest year in which the variable is available, and
#' attaches it to the variable code.
#'
#' @param input <`character`> A character string representing the variable code.
#' @param time <`numeric`> A numeric value indicating the time in years for
#' which the variable is needed. If \code{NULL}, the function returns the
#' \code{input} as is.
#'
#' @return A character string representing the variable code with the closest
#' year attached, or the original \code{input} if \code{time} is \code{NULL} or
#' \code{input} is an empty string.
#'
#' @details The function uses the \code{\link{var_get_info}} function to obtain the
#' dates at which the variable is available, and then finds the closest year to
#' the given \code{time} value.
picker_return_var <- function(input, time) {

  # If `time` is NULL, return the input
  if (is.null(time)) return(input)

  # If empty input (for a compare dropdown), return empty
  if (input == " ") return(" ")

  # Grab the dates at which the variable is available
  dates <- as.numeric(var_get_info(input, what = "dates")[[1]])
  # If no dates, return the input
  if (all(is.na(dates))) return(input)
  # Get time as numeric
  time <- as.numeric(time)

  # Get the closest years
  closest_year <- sapply(time, \(x) dates[which.min(abs(dates - x))],
                         USE.NAMES = FALSE)

  # Attach it to the variable code
  var <- paste(input, closest_year, sep = "_")

  # Flag wrong year with an attribute
  # if (time != closest_year) attr(var, "wrong_year") <- TRUE
  # TKTK Isn't functional in a sapply. The output vector of the sapply needs to
  # get the attribute. But only one date can be wrong. The other option is to
  # get a list output.

  # Return the var
  return(var)

}


