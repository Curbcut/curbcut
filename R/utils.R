#' Generate English or French ordinal form of a number
#'
#' This function returns the English or French ordinal form of a number. The
#' ordinal form can be selected by the lang argument, which takes either
#' "en" for English or "fr" for French.
#'
#' @param lang <`character`> String indicating the language for the ordinal form,
#' either "en" or "fr".
#' @param x <`numeric`> The number to be transformed into ordinal form.
#' @param en_first <`character`> The English word for first
#' (default is `"first"`). Useful for place explorer where `ranks 'best'` is more
#' suitable.
#'
#' @return A character string, the ordinal form of x in the selected language.
#'
#' @examples
#' ordinal_form("en", 1) # "first"
#' ordinal_form("fr", 2) # "deuxième"
#' ordinal_form("en", 23) # "23rd"
#' @export
ordinal_form <- function(lang, x, en_first = "first") {
  if (!lang %in% c("en", "fr")) {
    stop("Only languages supported are `en` and `fr`")
  }
  if (!is.numeric(x)) {
    stop("`x` must be numeric")
  }

  # French
  if (lang == "fr") {
    return(switch(as.character(x),
      "1" = "",
      "2" = "deuxième",
      "3" = "troisième",
      paste0(as.character(x), "ième")
    ))
  }

  # English
  if (x > 20) {
    if (x %% 100 %in% c(11, 12, 13)) {
      form <- "th "
    } else {
      form <- switch(as.character(x %% 10),
        "1" = "st",
        "2" = "nd",
        "3" = "rd",
        "th"
      )
    }
    paste0(x, form)
  } else {
    switch(as.character(x),
      "1" = en_first,
      "2" = "second",
      "3" = "third",
      "4" = "fourth",
      "5" = "fifth",
      "6" = "sixth",
      "7" = "seventh",
      "8" = "eighth",
      "9" = "ninth",
      "10" = "tenth",
      paste0(as.character(x), "th")
    )
  }
}

#' Compact big marks
#'
#' This function takes a numeric value and returns a compact representation of
#' the value in millions (M), thousands (K), or billions (B) based on its
#' magnitude, depending on the magnitude of the smallest significant digit.
#' The scale function used to format the output can be customized.
#' It is a helper function for the \code{\link[curbcut]{convert_unit}} function
#' and so needs pre-calculated `min_dig`.
#'
#' @param x <`numeric vector`> Numeric values.
#' @param min_dig <`integer`> An integer indicating the minimum number of
#' significant digits. It is used to determine the scale factor for the input
#' vector.
#' @param scale_fun <`function`> The scale function to be used to format the
#' output. The default is `scales::comma`.
#'
#' @return A string representation of the input value with a suffix of M, K,
#' or B as appropriate.
compact_big_marks <- function(x, min_dig, scale_fun = scales::comma) {
  if (min_dig >= 10) {
    return(do.call(scale_fun, list(x, 1, scale = 1 / 1e+09, suffix = "B")))
  }
  if (min_dig >= 7) {
    return(do.call(scale_fun, list(x, 1, scale = 1 / 1e+06, suffix = "M")))
  }
  if (min_dig >= 4) {
    return(do.call(scale_fun, list(x, 1, scale = 1 / 1e+03, suffix = "K")))
  }

  return(do.call(scale_fun, list(x, 1)))
}

#' Convert a numeric vector to appropriate display units
#'
#' This function takes a vector of numbers and returns a character vector that
#' represents the numbers in an appropriate format for display. If the \code{var}
#' argument is supplied, the function looks up the variable type and returns the
#' numbers in the appropriate format (e.g., as percentages or dollars).
#'
#' @param x <`numeric vector`> Numeric values.
#' @param var A variable code present in the `variables` table. If supplied, the
#' function looks up the variable type and returns the numbers in the appropriate
#' format. This argument is optional.
#' @param compact A logical value indicating whether to use compact notation for
#' large numbers. If \code{TRUE} and a number has at least 4 significant digits,
#' the function will use compact notation (e.g., 1.2M instead of 1200000). This
#' argument is optional and defaults to \code{FALSE}.
#'
#' @return A character vector of the same length as \code{x} representing the
#' numbers in an appropriate format for display.
#'
#' @details The function uses the \code{scales} package to format the numbers.
#' By default, it formats numbers with commas and a varying number of decimal
#' places depending on the magnitude of the numbers. If the \code{var} argument
#' is supplied and the variable type is \code{"pct"} or \code{"dollar"}, the
#' function uses the corresponding function from the \code{scales} package to
#' format the numbers.
#' @export
convert_unit <- function(x, var = NULL, compact = FALSE) {
  if (length(x) == 0) {
    return(x)
  }
  if (length(x) == 1 && is.na(x)) {
    return(x)
  }

  # Get the minimum number of significant digit
  min_dig <- setdiff(x, 0)
  min_dig <- min(abs(min_dig), na.rm = TRUE)
  min_dig <- ceiling(log10(min_dig))

  # Function used to return when no `var` is supplied or type is outside of
  # `pct` and `dollar`
  basic_return <- \(x, min_dig) {
    if (compact && min_dig >= 4) {
      return(compact_big_marks(x, min_dig))
    }
    if (max(abs(x)) >= 100 || all(round(x) == x)) {
      return(scales::comma(x, 1))
    }
    if (max(abs(x)) >= 10) {
      return(scales::comma(x, 0.1))
    }
    return(scales::comma(x, 0.01))
  }

  # If no `var` supplied
  if (is.null(var)) {
    return(basic_return(x, min_dig))
  }

  # If `var` supplied, search for types and return what is appropriate
  types <- unlist(var_get_info(var, what = "type"))
  if ("pct" %in% types) {
    return(paste0(round(x * 100, 1), "%"))
  }
  if ("dollar" %in% types) {
    if (compact && min_dig >= 4) {
      return(compact_big_marks(x, min_dig, scales::dollar))
    }

    return(scales::dollar(x, 1))
  }

  # Return normal formatting
  return(basic_return(x, min_dig))
}

#' Compute the depth of a nested data structure
#'
#' This function returns the depth of a nested data structure. The depth of a
#' vector is the number of lists it is nested within. Atomic vectors have a
#' depth of 1, and NULL has a depth of 0. It is a reimplementation of
#' \code{\link[purrr]{vec_depth}} in base R.
#'
#' @param x A vector, a list or a NULL.
#'
#' @return An integer indicating the depth of the data structure.
#'
#' @examples
#' vec_dep(NULL) # 0
#' vec_dep(1:10) # 1
#' vec_dep(list(1, list(2, 3))) # 2
#'
#' @seealso The purrr package provides a vec_depth function with similar
#' functionality.
#'
#' @export
vec_dep <- function(x) {
  if (is.null(x)) {
    0L
  } else if (is.atomic(x)) {
    1L
  } else if (is.list(x)) {
    depths <- vapply(x, vec_dep, vector("integer", 1))
    1L + max(depths, 0L)
  } else {
    stop("`x` must be a vector")
  }
}

#' Test if x scale is under study
#'
#' @param scales <`character vector`> All scales to test if it is part of `df`.
#' @param df <`character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`.
#'
#' @return Returns TRUE or FALSE
#' @export
#'
#' @examples
#' is_scale_df(scales = c("CSD", "CT", "DA"), df = "CMA_DA") # TRUE
#' is_scale_df(scales = c("CSD", "CT"), df = "CMA_DA") # FALSE
is_scale_df <- function(scales, df) {
  scls <- paste0(scales, "$", collapse = "|")
  grepl(scls, df)
}

#' Extract substrings from a character vector that matches a regular
#' expression pattern
#'
#' It is a reimplementation of \code{\link[stringr]{str_extract}} in base R.
#'
#' @param x <`character`> A vector from which to extract the substring
#' @param pattern <`character`> A regular expression pattern to match
#'
#' @return The substrings from \code{x} that matches \code{pattern}, or
#' \code{NA} if no matches are found
#' @export
#'
#' @examples
#' s_extract("hello", c("hello world", "goodbye")) # hello"
#' s_extract("[0-9]+", c("123abc", "def456")) # "123", "456"
s_extract <- function(pattern, x) {
  sapply(regmatches(x, regexec(pattern, x, perl = TRUE)), `[`, 1)
}

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
#' \code{\link[curbcut]{var_remove_time}}, hen looks up the variable code in
#' the `variables` table and returns the requested information for the variable.
#'
#' @param var <`character vector`> String representing the code of the variable
#' to retrieve information for.
#' @param what <`character`> String indicating the column name in the
#' `variables` table to retrieve information from. Defaults to `"var_title"`.
#' @param translate <`logical`> Indicating whether or not to translate the retrieved
#' information using the \code{\link[curbcut]{cc_t}} function. Defaults to `FALSE`.
#' @param lang <`character`> String indicating the language to translate to, if
#' `translate` is TRUE. If not specified, the function will not attempt to translate.
#'
#' @return The requested information about the variable, with optional translation
#' using the \code{\link[curbcut]{cc_t}} function.
#' @export
var_get_info <- function(var, what = "var_title", translate = FALSE,
                         lang = NULL) {
  variables <- get0("variables", envir = .GlobalEnv)
  if (is.null(variables)) {
    stop("`variables` table not found in the global environment.")
  }

  if (!what %in% names(variables)) {
    stop(glue::glue("`{what}` is not a column of the `variables` table."))
  }

  subset_vector <- variables$var_code == var_remove_time(var)
  if (sum(subset_vector) == 0) {
    stop(glue::glue(
      "`{var_remove_time(var)}` is not a variable code in the ",
      "`variables` table."
    ))
  }

  out <- variables[[what]][subset_vector]
  if (translate) out <- cc_t(lang = lang, out)

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
  title_left <-
    var_get_info(
      var = var, what = "var_title",
      translate = TRUE, lang = lang
    )
  if (!is.null(short_treshold) && nchar(title_left) > short_treshold) {
    title_left <-
      var_get_info(
        var = var, what = "var_short",
        translate = TRUE, lang = lang
      )
  }

  return(title_left)
}

#' Get pretty or raw variable breaks
#'
#' This function returns the breaks for a given variable, region, scale and date
#' for q3 (directly extracted from `var`) as either raw or pretty. Raw breaks are
#' always given in the base unit of the variable. Pretty breaks are returned in an
#' appropriate format for display using \code{\link[curbcut]{convert_unit}}, i.e.,
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
#' using the \code{\link[curbcut]{convert_unit}} function. Defaults to `TRUE`.
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
        breaks[[break_col]][breaks$df == df
        ]
      } else {
        breaks[[break_col]][breaks$df == df & breaks$date == date
        ]
      }
    }

  # Translate if necessary
  if (is.character(breaks) && !is.null(lang)) {
    breaks <- sapply(breaks, cc_t, lang = lang, USE.NAMES = FALSE)
  }

  # Get pretty breaks
  if (pretty & break_col == "var") {
    breaks <- convert_unit(breaks, var, compact)
  }

  # Return
  return(breaks)
}
