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

  if (!lang %in% c("en", "fr"))
    stop("Only languages supported are `en` and `fr`")
  if (!is.numeric(x))
    stop("`x` must be numeric")

  # French
  if (lang == "fr") {
    return(switch(as.character(x), "1" = "", "2" = "deuxième",
                  "3" = "troisième", paste0(as.character(x), "ième")))
  }

  # English
  if (x > 20) {
    if (x %% 100 %in% c(11 , 12, 13)) {
      form <- "th "
    } else {
      form <- switch(as.character(x %% 10), "1" = "st", "2" = "nd",
                     "3" = "rd", "th")
    }
    paste0(x, form)
  } else {
    switch(as.character(x), "1" = en_first, "2" = "second",
           "3" = "third", "4" = "fourth", "5" = "fifth",
           "6" = "sixth",  "7" = "seventh", "8" = "eighth",
           "9" = "ninth", "10" = "tenth",
           paste0(as.character(x), "th"))
  }

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
  if (is.null(variables))
    stop("`variables` table not found in the global environment.")

  if (!what %in% names(variables))
    stop(glue::glue("`{what}` is not a column of the `variables` table."))

  out <- variables[[what]][variables$var_code == var_remove_time(var)]
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
#' @param var <`character string`> String representing the code of the variable
#' to retrieve the title for.
#' @param short_treshold <`numeric`> An optional threshold for the title length,
#' above which the short title will be returned.
#' @param translate <`logical`> Indicates whether to translate the title to a
#' different language.
#' @param lang <`character`> String indicating the language to translate the title to.
#'
#' @return A character string representing the variable title.
#' @export
var_get_title <- function(var, short_treshold = NULL,
                          translate = FALSE, lang = NULL) {
  title_left <-
    var_get_info(var = var, what = "var_title",
                 translate = TRUE, lang = lang)
  if (!is.null(short_treshold) & nchar(title_left) > short_treshold)
    title_left <-
      var_get_info(var = var, what = "var_short",
                   translate = TRUE, lang = lang)

  return(title_left)
}
