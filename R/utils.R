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
#' depth of 1, and NULL has a depth of 0. It is a reimplemntation of
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
