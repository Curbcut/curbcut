#' Compact big marks
#'
#' This function takes a numeric value and returns a compact representation of
#' the value in millions (M), thousands (K), or billions (B) based on its
#' magnitude, depending on the magnitude of the smallest significant digit.
#' The scale function used to format the output can be customized.
#' It is a helper function for the \code{\link{convert_unit}} function
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

#' Get the minimum number of significant digits before the decimal point in a
#' vector of numbers
#'
#' This function takes a vector of numbers and returns the minimum number of
#' significant digits before the decimal point in that vector. For example, if
#' the vector contains the numbers 123.456, 0.000789, 0.01, 1000, and 0.5, the
#' minimum number of significant digits before the decimal point is 1
#' (corresponding to the number of digits in the number 0.5).
#'
#' @param x <`numeric`> A numeric vector
#' @return An integer representing the minimum number of significant digits
#' before the decimal point in x
#'
#' @examples
#' x <- c(123.456, 0.000789, 0.01, 1000, 0.5)
#' min_sig_digits(x) # 3
min_sig_digits <- function(x) {
  # Find the minimum absolute value in x that is greater than 0
  min_abs_val <- min(abs(x[x != 0]))

  # Calculate the number of digits before the decimal point
  sig_digits <- floor(log10(min_abs_val)) + 1

  # Return the number of digits before the decimal point
  return(sig_digits)
}

#' Convert a numeric vector into a character vector with a specified unit
#'
#' This function converts a numeric vector into a character vector with a
#' specified unit based on the variable type specified. It dispatches to methods
#' based on the class of \code{x}. Supported unit types are percentage, dollar,
#' and default (comma-separated with varying decimal points). If \code{compact}
#' is \code{TRUE}, the function will attempt to compact large numbers.
#'
#' @param var <`character`> String specifying the variable type. Currently
#' supported types are "pct" (percentage), "dollar" (dollar), and "default"
#' (comma-separated with varying decimal points).
#' @param x <`numeric`> Vector to be converted into a character vector with a
#' specified unit.
#' @param ... Additional arguments to be passed to the methods.
#'
#' @return A character vector with a specified unit based on the variable type
#' specified.
#'
#' @examples
#' convert_unit("pct", 0.25)
#' convert_unit("dollar", 1000, compact = TRUE)
#' convert_unit("default", c(10.2, 10000, 5.67))
#' @export
convert_unit <- function(var = NULL, x, ...) {

  if (length(x) == 0) {
    return(x)
  }
  if (length(x) == 1 && is.na(x)) {
    return(x)
  }

  UseMethod("convert_unit", var)
}

#' Convert a numeric vector into a character vector with a percentage sign
#'
#' @param var Same as in the generic function.
#' @param x Same as in the generic function.
#' @param decimal <`numeric`> How many decimal numbers should there be. Defaults
#' to 2.
#' @param ... Additional arguments to be passed to the method.
#'
#' @return A character vector with a percentage sign.
#'
#' @method convert_unit pct
#' @export
convert_unit.pct <- function(var, x, decimal = 2, ...) {
  paste0(round(x * 100, 2), "%")
}

#' Convert a numeric vector into a character vector with a dollar sign
#'
#' @param var Same as in the generic function.
#' @param x Same as in the generic function.
#' @param compact <`logical`> Whether to attempt to compact large numbers.
#' @param ... Additional arguments to be passed to the method.
#'
#' @return A character vector with a dollar sign.
#'
#' @method convert_unit dollar
#' @export
convert_unit.dollar <- function(var, x, compact, ...) {

  # Get the minimum number of significant digit
  min_dig <- min_sig_digits(x)

  # If compact and large digit
  if (compact && min_dig >= 4) {
    return(compact_big_marks(x, min_dig, scales::dollar))
  }

  # Non-compact or small digit
  return(scales::dollar(x, 1))
}

#' Convert a numeric vector into a character vector with a default separator
#'
#' @param var Same as in the generic function.
#' @param x Same as in the generic function.
#' @param compact <`logical`> Whether to attempt to compact large numbers.
#' @param ... Additional arguments to be passed to the method.
#'
#' @return A character vector with a default separator.
#'
#' @method convert_unit default
#' @export
convert_unit.default <- function(var, x, compact = FALSE, ...) {

  # Get the minimum number of significant digit
  min_dig <- min_sig_digits(x)

  if (compact && min_dig >= 4) {
    return(compact_big_marks(x, min_dig, scales::comma))
  }
  if (max(abs(x)) >= 100 || all(round(x) == x)) {
    return(scales::comma(x, 1))
  }
  if (max(abs(x)) >= 10) {
    return(scales::comma(x, 0.1))
  }
  return(scales::comma(x, 0.01))
}
