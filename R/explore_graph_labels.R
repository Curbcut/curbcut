#' Generic legend label function for Curbcut legends
#'
#' `explore_graph_labels` is a generic function used to produce a list of
#' \code{\link[ggplot2]{labs}} for the Curbcut explore graph. The function invokes
#' particular methods which depend on the class of the `vars` argument.
#'
#' @param vars <`named list`> A list object with a pre-determined class. The
#' output of \code{\link{vars_build}}.
#' @param lang <`character`> A character string indicating the language to
#' translate variable titles to.
#' @param ... Arguments to be passed to the methods, e.g. optionally `lang`
#'
#' @return It returns an output of \code{\link[ggplot2]{labs}}.
#' @export
explore_graph_labels <- function(vars, lang = NULL, ...) {
  UseMethod("explore_graph_labels", vars)
}

#' @rdname explore_graph_labels
#' @param time <`named list`> A named list with the time variables.
#' @export
explore_graph_labels.delta <- function(vars, lang = NULL, time, ...) {

  title <- var_get_title(
    var = vars$var_left, short_treshold = 16,
    translate = TRUE, lang = lang
  )

  title_y <- paste0(title, " (", time$var_left[2], ")")
  title_x <- paste0(title, " (", time$var_left[1], ")")

  return(ggplot2::labs(x = title_x, y = title_y))
}

#' @rdname explore_graph_labels
#' @export
explore_graph_labels.default <- function(vars, lang = NULL, time, ...) {
  legend_labels(vars = vars, lang = lang, time = time, ...)[[1]]
}
