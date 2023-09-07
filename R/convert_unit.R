#' Compact big marks
#'
#' This function takes a numeric value and returns a compact representation of
#' the value in millions (M), thousands (K), or billions (B) based on its
#' magnitude, depending on the magnitude of the smallest significant digit.
#' The scale function used to format the output can be customized.
#' It is a helper function for the \code{\link{convert_unit}} function
#' and so needs pre-calculated `min_dig`. If the output is not unique, it
#' adds decimals until a maximum of three is attained.
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
  adjust_if_unique <- function(x, scale_fun, scale, suffix) {
    # Initialize the variables
    result <- do.call(scale_fun, list(x, 1, scale = scale, suffix = suffix))
    unique_length <- length(unique(result))

    # Initialize decimal places
    dec_places <- 1

    # Increase decimal places until all elements are unique or a max of 3 decimals reached
    while (unique_length != length(x) & dec_places <= 3) {
      result <- do.call(scale_fun, list(x, 10^(-dec_places), scale = scale, suffix = suffix))
      unique_length <- length(unique(result))
      dec_places <- dec_places + 1
    }

    return(result)
  }

  if (min_dig >= 10) {
    return(adjust_if_unique(x, scale_fun = scale_fun, scale = 1 / 1e+09, suffix = "B"))
  }
  if (min_dig >= 7) {
    return(adjust_if_unique(x, scale_fun = scale_fun, scale = 1 / 1e+06, suffix = "M"))
  }
  if (min_dig >= 4) {
    return(adjust_if_unique(x, scale_fun = scale_fun, scale = 1 / 1e+03, suffix = "K"))
  }

  return(do.call(scale_fun, list(x, 1)))
}

#' Round a numeric vector and apply a scale function based on its value magnitude
#'
#' This function rounds a numeric vector and applies a scale function to format
#' it based on its value magnitude. The scale function is applied using the
#' \code{scales} package, which provides several options for formatting numeric
#' values, such as adding commas or dollar signs.
#'
#' @param x <`numeric vector`> Numeric values to be rounded and scaled.
#' @param min_dig <`numeric`> An integer specifying the minimum number of digits to be
#' displayed after rounding. If the value magnitude is larger than or equal to
#' 6, 5, or 4, \code{accuracy} is set to \code{1e+03}, \code{1e+02}, or
#' \code{1e+01}, respectively, to round the value accordingly. Otherwise,
#' \code{accuracy} is set to \code{1} and \code{min_dig} is used to set the
#' rounding accuracy. `min_dig` is usually the output of \code{\link{min_sig_digits}}
#' @param scale_fun <`function`> A scale function to be applied to the rounded values. This
#' should be a function from the \code{scales} package that takes a numeric
#' vector as input and returns a character vector with formatted values. The
#' default is \code{scales::comma}, which adds commas to separate thousands
#' and rounds values to two decimal places by default.
#'
#' @return A character vector with the rounded and scaled values.
round_big_marks <- function(x, min_dig, scale_fun = scales::comma) {
  if (min_dig >= 7) {
    return(do.call(scale_fun, list(x, 1, accuracy = 1e+03)))
  }
  if (min_dig >= 5) {
    return(do.call(scale_fun, list(x, 1, accuracy = 1e+02)))
  }
  if (min_dig >= 4) {
    return(do.call(scale_fun, list(x, 1, accuracy = 1e+01)))
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
min_sig_digits <- function(x) {
  # If zero, return zero
  if (all(x == 0)) {
    return(rep(0, length(x)))
  }

  # Take out NAs if there are
  x <- x[!is.na(x)]

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
  paste0(round(x * 100, digits = decimal), "%")
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
convert_unit.dollar <- function(var, x, compact = FALSE, ...) {
  # Get the minimum number of significant digit
  min_dig <- min_sig_digits(x)

  # If compact and large digit
  if (compact && min_dig >= 4) {
    return(compact_big_marks(
      x = x,
      min_dig = min_dig,
      scale_fun = scales::dollar
    ))
  }

  # Non-compact
  out <- round_big_marks(x = x, min_dig = min_dig, scale_fun = scales::dollar)
  return(out)
}

#' Convert a numeric vector into a character vector with a degrees sign
#'
#' @param var Same as in the generic function.
#' @param x Same as in the generic function.
#' @param ... Additional arguments to be passed to the method.
#'
#' @return A character vector with a percentage sign.
#'
#' @method convert_unit degree
#' @export
convert_unit.degree <- function(var, x, ...) {
  paste0(scales::comma(x, 0.1), "\u00B0C")
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
    return(compact_big_marks(
      x = x,
      min_dig = min_dig,
      scale_fun = scales::comma
    ))
  }
  if (max(abs(x)) >= 1000) {
    return(round_big_marks(
      x = x,
      min_dig = min_dig,
      scale_fun = scales::comma
    ))
  }
  if (max(abs(x)) >= 100 || all(round(x) == x)) {
    return(scales::comma(x, 1))
  }
  if (max(abs(x)) >= 10) {
    return(scales::comma(x, 0.1))
  }
  return(scales::comma(x, 0.01))
}
