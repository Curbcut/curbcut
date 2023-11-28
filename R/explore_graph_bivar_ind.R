#' Explore Graph Function for bivariate `INDEX` type
#'
#' This function creates a ggplot for the explore panel of Curbcut, based on the
#' input parameters, for the index type variables.
#'
#' @param vars <`named list`> A list object of variable codes with classes. The
#' output of \code{\link{vars_build}}.
#' @param select_id <`character`> A string indicating the ID of the currently
#' selected region (if any). Usually `r[[id]]$select_id()`
#' @param scale <`character`> Current scale.
#' @param data <`data.frame`> A data frame containing the variables and
#' observations. The output of \code{\link{data_get}}.
#' @param time <`numeric named list`> The `time` at which data is displayed.
#' A list for var_left and var_right. The output of \code{\link{vars_build}}(...)$time.
#' @param schemas <`named list`> Current schema information. The additional widget
#' values that have an impact on which data column to pick. Usually `r[[id]]$schema()`.
#' @param scales_as_DA <`character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their graph will be the one of their DA.
#' @param lang <`character`> A character string indicating the language to
#' translate variable titles to.
#' @param font_family <`character`> A string specifying the font family for the
#' plot, default is "acidgrotesk-book".
#' @param ... Additional arguments passed to the specific method.
#'
#' @return A ggplot2 object representing the plot.
#' @export
explore_graph_bivar_ind <- function(vars, select_id, scale, data, time, schemas,
                                    scales_as_DA = c("building", "street"), lang = NULL,
                                    font_family = "acidgrotesk-book", ...) {
  UseMethod("explore_graph_bivar_ind", vars)
}

#' @describeIn  explore_graph_bivar_ind Scalar method
#' @export
explore_graph_bivar_ind.scalar <- function(vars, select_id, scale, data, time, schemas,
                                           scales_as_DA = c("building", "street"),
                                           lang = NULL,
                                           font_family = "acidgrotesk-book", ...) {
  explore_graph.bivar(
    vars = vars, select_id = select_id, scale = scale, data = data, time = time,
    schemas = schemas, scales_as_DA = scales_as_DA, lang = lang,
    font_family = "acidgrotesk-book", ...
  )
}

#' @describeIn explore_graph_bivar_ind Ordinal method
#' @export
explore_graph_bivar_ind.ordinal <- function(vars, select_id, scale, data, time, schemas,
                                            scales_as_DA = c("building", "street"),
                                            lang = NULL,
                                            font_family = "acidgrotesk-book", ...) {
  # Appease R CMD check
  var_left <- var_right <- NULL

  # Grab the shared info between the graphs
  shared_info <- explore_graph_info(
    vars = vars, font_family = font_family,
    scales_as_DA = scales_as_DA, select_id = select_id,
    data = data, lang = lang, scale = scale
  )

  # Colors
  clr_df <- shared_info$colours_dfs$left_5
  clr <- clr_df$fill[1:6]

  # Get the scales ggplot function
  x_scale <- explore_graph_scale(
    var = vars$var_left,
    x_y = "x",
    data_vals = data$var_left,
    df = shared_info$treated_scale,
    lang = lang,
    limits = attr(data, "breaks_var_left")
  )
  y_scale <- explore_graph_scale(
    var = vars$var_right,
    x_y = "y",
    data_vals = data$var_right,
    limits = attr(data, "breaks_var_right")
  )

  # Update labels (wrong axis)
  labs <- shared_info$labs
  labs$x <- shared_info$labs$y
  labs$y <- shared_info$labs$x

  # Draw the plot
  plot <-
    data[!is.na(data$var_left) & !is.na(data$var_right), ] |>
    remove_outliers_df(cols = c("var_left", "var_right")) |>
    ggplot2::ggplot(ggplot2::aes(as.factor(var_left), var_right)) +
    ggplot2::geom_boxplot(ggplot2::aes(fill = as.factor(var_left))) +
    ggplot2::scale_fill_manual(breaks = 0:5, values = clr) +
    x_scale +
    y_scale +
    labs +
    shared_info$theme_default

  # Add selection
  if (!is.na(shared_info$select_id)) {
    val <- data[data$ID == shared_info$select_id, ]
    if (!any(is.na(val))) {
      plot <-
        plot +
        ggplot2::geom_point(
          data = val, shape = 21, colour = "white",
          fill = "black", size = 4
        )
    }
  }

  # Return
  return(plot)
}
