#' Extracts the `time` component from variable codes
#'
#' @param var <`character vector`> String representing the variable codes.
#'
#' @return A character string representing the year component of the variable
#' name.
#' @export
#'
#' @examples
#' var_get_time("housing_tenant_2016") # 2016
#' var_get_time(c("housing_tenant_2016", "housing_tenant_2021")) # "2016" "2021"
var_get_time <- function(var) {
  s_extract("(?<=_)\\d{4}$", var)
}

#' Remove the time component from variable codes
#'
#' This function removes the time component from variable codes The time
#' component is identified by the "_YYYY" format at the end of the variable name.
#'
#' @param var <`character vector`> String representing the variable codes.
#'
#' @return A character string with the variable code without the time component.
#' If the input was a variable code duplicate of different year, the output is
#' the unique variable code.
#' @export
#'
#' @examples
#' var_remove_time("housing_tenant_2016") # "housing_tenant"
#' var_remove_time(c("housing_tenant_2016", "housing_tenant_2021")) # "housing_tenant"
var_remove_time <- function(var) {
  unique(sub("_\\d{4}$", "", var))
}

#' Get information about a variable from the `variables` table
#'
#' This function retrieves information about a given variable from the `variables`
#' table in the global environment. The `variables` table must have columns for
#' `var_code` and the requested information specified by `what`. The function
#' first removes the time component from the variable name using
#' \code{\link{var_remove_time}}, hen looks up the variable code in
#' the `variables` table and returns the requested information for the variable.
#'
#' @param var <`character vector`> String representing the code of the variable
#' to retrieve information for.
#' @param what <`character`> String indicating the column name in the
#' `variables` table to retrieve information from. Defaults to `"var_title"`.
#' @param translate <`logical`> Indicating whether or not to translate the retrieved
#' information using the \code{\link{cc_t}} function. Defaults to `FALSE`.
#' @param lang <`character`> String indicating the language to translate to, if
#' `translate` is TRUE. If not specified, the function will not attempt to translate.
#' @param check_year <`logical`> Should the year be removed from `var` to grab
#' variable's info? Defaults to TRUE
#'
#' @return The requested information about the variable, with optional translation
#' using the \code{\link{cc_t}} function.
#' @export
var_get_info <- function(var, what = "var_title", translate = FALSE,
                         lang = NULL, check_year = TRUE) {
  variables <- get_from_globalenv("variables")

  if (!what %in% names(variables)) {
    stop(glue::glue_safe("`{what}` is not a column of the `variables` table."))
  }

  subset_vector <- if (check_year) {
    sub <- variables$var_code == var_remove_time(var)

    # If the latter is not present in the `variables` table
    if (sum(sub) == 0) {
      stop(glue::glue_safe(
        "`{var_remove_time(var)}` is not a variable code in the ",
        "`variables` table."
      ))
    }

    sub
  } else {
    sub <- which(variables$var_code == var)

    # If the latter is not present in the `variables` table
    if (length(sub) == 0) {
      stop(glue::glue_safe(
        "`{var}` is not a variable code in the ",
        "`variables` table."
      ))
    }
    sub
  }

  out <- variables[[what]][subset_vector]
  if (translate) out <- cc_t(out, lang = lang)

  return(out)
}

#' Get the title of a variable code
#'
#' Given a variable name, get the title of the variable from the 'variables' table.
#' If a 'short_treshold' is provided, return the short title if the title length
#' exceeds the treshold. The returned title can be translated to a different
#' language using the 'lang' argument.
#'
#' @param var <`character`> String representing the code of the variable
#' to retrieve the title for.
#' @param short_treshold <`numeric`> An optional threshold for the title length,
#' above which the short title will be returned.
#' @param translate <`logical`> Indicates whether to translate the title to a
#' different language.
#' @param lang <`character`> String indicating the language to translate the title
#' to. Defaults to `NULL`, which is no translation.
#'
#' @return A character string representing the variable title.
#' @export
var_get_title <- function(var, short_treshold = NULL,
                          translate = FALSE, lang = NULL) {
  # In the case where this is the non-selected comparison
  if (var[1] == " ") return(NULL)

  title <-
    var_get_info(
      var = var, what = "var_title",
      translate = TRUE, lang = lang
    )
  if (!is.null(short_treshold) && nchar(title) > short_treshold) {
    title <-
      var_get_info(
        var = var, what = "var_short",
        translate = TRUE, lang = lang
      )
  }

  return(title)
}

#' Get pretty or raw variable breaks
#'
#' This function returns the breaks for a given variable, region, scale and date
#' for q3 (directly extracted from `var`) as either raw or pretty. Raw breaks are
#' always given in the base unit of the variable. Pretty breaks are returned in an
#' appropriate format for display using \code{\link{convert_unit}}, i.e.,
#' with the appropriate unit prefix and separator depending on their magnitude.
#' The pretty formatting is optional, and the function defaults to returning pretty
#' breaks.
#'
#' @param var <`character`> String representing the code of the variable
#' to retrieve the breaks for.
#' @param df <`character`> Indicates the combination of the region and scale
#' of interest, e.g. `"CMA_DA"`
#' @param q3_q5 <`character`> String indicating whether to return Q3 or Q5 breaks.
#' Defaults to "q5".
#' @param break_col <`character`> Which column in the breaks_qx column of the
#' `variables` table should the break be taken from. Defaults to `var` which
#' is the numeric breaks for any numeric variable. Other possibility is
#' `var_name` or `var_name_short` for qualitative variables.
#' @param pretty <`logical`> Indicates whether to return pretty-formatted breaks
#' using the \code{\link{convert_unit}} function. Defaults to `TRUE`.
#' @param compact <`logical`> Indicates whether to compact the number representation
#' of breaks. Only applies when pretty is `TRUE`. Defaults to `TRUE`.
#' @param lang <`character`> String indicating the language to translate the
#' breaks to. Defaults to `NULL`, which is no translation.
#'
#' @return A vector of breaks for the specified variable, region, and scale.
#' If pretty is TRUE, the breaks are returned as characters and formatted in an
#' appropriate format for display.
#'
#' @export
var_get_breaks <- function(var, df, q3_q5 = "q5", break_col = "var",
                           pretty = TRUE, compact = TRUE, lang = NULL) {
  # Grab the breaks
  breaks <- var_get_info(var = var, what = paste0("breaks_", q3_q5))[[1]]
  breaks <-
    if (q3_q5 == "q5") {
      breaks[[break_col]][breaks$df == df]
    } else if (q3_q5 == "q3") {
      date <- var_get_time(var)
      if (is.na(date)) {
        breaks[[break_col]][breaks$df == df]
      } else {
        breaks[[break_col]][breaks$df == df & breaks$date == date]
      }
    }

  # Translate if necessary
  if (is.character(breaks) && !is.null(lang)) {
    breaks <- sapply(breaks, cc_t, lang = lang, USE.NAMES = FALSE)
  }

  # Get pretty breaks
  if (pretty & break_col == "var") {
    breaks <- convert_unit(var = var, x = breaks, compact = compact)
  }

  # Return
  return(breaks)
}

#' Returns the row number of a given variable in the `variables` table.
#'
#' This function takes a variable code and returns the corresponding row number
#' in the `variables` table. If the variable is not found in the `variables`
#' table, it returns the original value of the input. It first removes any
#' appended time values from the input, and then looks up the variable in
#' the `variables` table.
#'
#' @param var <`character`> Representing the variable code to look up.
#'
#' @return If \code{var} is found in the `variables` table, returns its
#' corresponding row number. If not found, returns the original value of
#' \code{var}.
var_row_index <- function(var) {
  # Take out the appended time
  var <- unique(var_remove_time(var = var))

  # Grab the `variables` table
  variables <- get_from_globalenv("variables")

  # If var is not in the `variables` table, return its value
  if (!var %in% variables$var_code) {
    return(var)
  }

  # Get the row number of `var`
  return(which(variables$var_code == var))
}

#' Get information about the parent variable of a variable from the `variables`
#' table
#'
#' This function retrieves information about the parent variable of a given
#' variable from the `variables` table in the global environment.
#'
#' It is using the \code{\link{var_get_info}} function to retrieve the correct
#' information.
#'
#' @param var <`character vector`> String representing the code of the variable
#' to retrieve the parent variable information for.
#' @param what <`character`> String indicating the column name in the
#' `variables` table to retrieve information from. Defaults to `"var_title"`.
#' @param translate <`logical`> Indicating whether or not to translate the retrieved
#' information using the \code{\link{cc_t}} function. Defaults to `FALSE`.
#' @param lang <`character`> String indicating the language to translate to, if
#' `translate` is TRUE. If not specified, the function will not attempt to translate.
#' @param check_year <`logical`> Should the year be removed from `var` to grab
#' variable's info? Defaults to TRUE
#'
#' @return The requested information about the parent variable, with optional
#' translation using the \code{\link{cc_t}} function.
#' @export
var_get_parent_info <- function(var, what = "explanation", translate = FALSE,
                                lang = NULL, check_year = TRUE) {
  parent <- var_get_info(var, what = "parent_vec")
  parent_info <- var_get_info(parent,
    what = what, translate = translate,
    lang = lang, check_year = check_year
  )

  return(parent_info)
}
