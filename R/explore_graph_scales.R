#' Explore graph scale based on the variable type
#'
#' This function provides a set of ggplot2 x-axis scales based on the variable type,
#' simplifying the process of creating axis scales for different types of data.
#'
#' @param var <`character`> The variable with its type as a class, either "pct",
#' "dollar", or any other type for the default scale.
#' @param x_y <`character`> x or y axis?
#' @param ... Additional arguments passed to specific methods.
#'
#' @return A list containing ggplot2 x and y-axis scale functions.
#' @export
explore_graph_scale <- function(var, x_y, ...) {
  stopifnot(x_y %in% c("x", "y"))
  UseMethod("explore_graph_scale", var)
}

#' @describeIn explore_graph_scale The method for percentage values. Applies a
#' percentage scale to the x-axis.
#' @export
explore_graph_scale.pct <- function(var, x_y, ...) {
  scale <- sprintf("ggplot2::scale_%s_continuous", x_y)
  list(do.call(eval(parse(text = scale)), list(labels = scales::percent)))
}

#' @describeIn explore_graph_scale The method for dollar values. Applies a
#' dollar scale to the x-axis with appropriate suffixes based on the magnitude
#' of the data.
#' @param data_vals A numeric vector containing data values, required only for the
#' "dollar" method.
#' @export
explore_graph_scale.dollar <- function(var, x_y, data_vals, ...) {

  # Get the minimum number of significant digit
  min_dig <- min_sig_digits(data_vals)

  # Denpending on the magnitude of the number, compact differently
  scl <- if (min_dig >= 10) {
    scales::label_dollar(scale = 1 / 1e+09, suffix = "B")
  } else if (min_dig >= 7) {
    scales::label_dollar(scale = 1 / 1e+06, suffix = "M")
  } else if (min_dig >= 4) {
    scales::label_dollar(scale = 1 / 1e+03, suffix = "K")
  } else {
    scales::label_dollar()
  }

  # Return the ggplot2 scale
  scale <- sprintf("ggplot2::scale_%s_continuous", x_y)
  list(do.call(eval(parse(text = scale)), list(labels = scl)))
}

#' @describeIn explore_graph_scale The default method for unspecified variable
#' types. Applies a comma scale to the x-axis.
#' @export
explore_graph_scale.default <- function(var, x_y, ...) {
  scale <- sprintf("ggplot2::scale_%s_continuous", x_y)
  list(do.call(eval(parse(text = scale)), list(labels = scales::comma)))
}

