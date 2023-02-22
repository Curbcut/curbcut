#' Translate names of elements in a nested list
#'
#' This function translates the names of elements in a nested list using a
#' translation data.frame
#'
#' @param x <`list`> A nested list where the names of elements will be translated.
#' @param translation_df <`data.frame`> A data.frame with two columns: en and fr.
#' It contains the original names in English and the corresponding translated
#' names in French.
#'
#' @return The same nested list with the names of elements translated.
#'
#' @examples
#' translation_df <- data.frame(
#'   en = c("apple", "banana", "cherry", "fruits", "vegetables"),
#'   fr = c("pomme", "banane", "cerise", "fruits", "légumes")
#' )
#'
#' x <- list(
#'   fruits = list(apple = 1, banana = 2),
#'   vegetables = list(carrot = 3, lettuce = 4)
#' )
#'
#' cc_t_list(x, translation_df)
#' @export
cc_t_list <- function(x, translation_df) {
  # translate name of lists
  names(x) <- sapply(names(x), \(y) {
    if (is.null(y)) {
      NULL
    } else {
      out <- translation_df$fr[translation_df$en == y]

      if (length(out) == 0 || is.na(out)) {
        warning("No translation text found for `", y, "`.", call. = FALSE)
        out <- y
      }

      out
    }
  })

  # Re-iterate in list depth to translate every name
  if (vec_dep(x) > 2) {
    x <- lapply(x, \(y) if (vec_dep(y) > 1) cc_t_list(y, translation_df) else (y))
  }

  x
}

#' Translate an object between English and French
#'
#' Translate input object from English to French depending on the provided
#' language and whether the function is running in a Shiny environment.
#'
#' @param ... <`character objects`> Any objects, including lists or atomic vectors
#' @param .envir The parent environment for evaluating the expressions in
#' \code{...}. Necessary to grab the correct value of variables handled by
#' \code{\link[glue]{glue}}. Defaults to `parent.frame()`
#' @param lang <`character`> Language to use for translation. Must be one of
#' en' or 'fr'. Defaults to NULL which is no translation.
#'
#' @return If running in a Shiny context (UI), then return spans in both languages.
#' If in a Shiny context and in server side, returns translation depending on
#' `lang`. Outside a Shiny context, returns the input as is.
#'
#' @seealso \code{\link{cc_t_list}} for translating lists of input
#' objects
#'
#' @export
cc_t <- function(..., .envir = parent.frame(), lang = NULL) {
  # Helper functions only used for translation
  cc_glue <- function(x) {
    glue::glue(x, .na = character(1), .null = character(1), .envir = .envir)
  }
  return_raw <- function(x) {
    if (is.list(x)) {
      return(x)
    }
    cc_glue(x)
  }
  french_translation <- function(x) {
    # French
    if (is.list(x)) {
      return(cc_t_list(x, translation_df))
    }

    # Character
    translated <- translation_df$fr[translation_df$en == x]
    # In case there is no translations:
    if (length(translated) == 0 || is.na(translated)) {
      return({
        warning("No translation text found for `", x, "`.", call. = FALSE)
        # x <- sub("<<.>>", "", x)
        cc_glue(x)
      })
    }

    # For vectors with names (such as used for x axis of some modules' graph)
    if (!is.null(names(x))) names(translated) <- names(x)

    return(cc_glue(translated))
  }

  # Error if we provide lists + character vectors unintentionally
  args <- list(...)
  error_check <- sapply(args, inherits, "list")
  stopifnot(length(error_check) == sum(error_check) || sum(error_check) == 0)

  # Collapse the vectors together
  x <- c(...)
  if (!is.list(x)) x <- paste0(..., collapse = "")

  # # Return input if there lang is NULL
  # if (is.null(lang)) {
  #   return(return_raw(x))
  # }

  # Grab translation_df and return input if missing
  translation_df <- get0("translation_df", .GlobalEnv)
  if (is.null(translation_df)) {
    return({
      if (is.null(shiny::getDefaultReactiveDomain())) {
        # UI side
        shiny::tagList(shiny::span(class = "lang-en", x))
      } else {
        # Server side
        return_raw(x)
      }
    })
  }

  # If Shiny isn't running, still return depending on translation language given
  if (!shiny::isRunning()) {
    if (is.null(lang) || lang == "en") {
      return(return_raw(x))
    } else {
      return(french_translation(x))
    }
  }

  # If not in a reactive shiny context (is in UI), return 2 spans.
  if (is.null(shiny::getDefaultReactiveDomain())) {
    return(
      shiny::tagList(
        shiny::span(class = "lang-en", x),
        shiny::span(class = "lang-fr", {
          translated <- translation_df[translation_df$en == x, ]$fr
          if (length(translated) != 0 && !is.na(translated)) {
            translated
          } else {
            warning("No translation text found for `", x, "`.",
              call. = FALSE
            )
            x
          }
        })
      )
    )
  }

  if (all(x == "")) {
    return("")
  }

  # English
  if (lang == "en") {
    return(return_raw(x))
  }

  # Return french
  return(french_translation(x))
}
