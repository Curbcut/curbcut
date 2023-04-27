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
#' @export
explore_graph_labels.delta <- function(vars, lang = NULL, ...) {
  # For the delta, use the bivar legend function as the delta legend label only
  # uses the x axis
  legend_labels.bivar(vars = list(var_left = vars$var_left[2],
                                  var_right = vars$var_left[1]))[[1]]
}

#' @rdname explore_graph_labels
#' @export
explore_graph_labels.default <- function(vars, lang = NULL, ...) {
  legend_labels(vars = vars, lang = lang)[[1]]
}

