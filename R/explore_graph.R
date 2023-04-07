#' Explore Graph Function
#'
#' This function creates a ggplot for the explore panel of Curbcut, based on the
#' input parameters.
#'
#' @param vars <`named list`> A list object of variable codes with classes. The
#' output of \code{\link{vars_build}}.
#' @param select_id <`character`> A string indicating the ID of the currently
#' selected region (if any). Usually `r[[id]]$select_id()`
#' @param data <`data.frame`> A data frame containing the variables and
#' observations to be compared. The output of \code{\link{data_get}}.
#' @param df <`character`> The combination of the region under study and the
#' scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param scales_as_DA <`character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their graph will be the one of their DA.
#' @param lang <`character`> A character string indicating the language to
#' translate variable titles to.
#' @param font_family <`character`> A string specifying the font family for the
#' plot, default is "SourceSansPro".
#' @param ... Additional arguments passed to the specific method.
#'
#' @return A ggplot2 object representing the plot.
#' @export
explore_graph <- function(vars, select_id, df, data,
                          scales_as_DA = c("building", "street"), lang = NULL,
                          font_family = "SourceSansPro", ...) {
  UseMethod("explore_graph", vars)
}

#' @rdname explore_graph
#' @export
explore_graph.q5_ind <- function(vars, select_id, df, data, scales_as_DA, lang = NULL,
                                 font_family = "SourceSansPro", ...) {
  # x_scale <- list(ggplot2::scale_x_continuous(
  #   limits = c(min(0, as.numeric(names(var_left_label))),
  #              max(0, as.numeric(names(var_left_label)))),
  #   breaks = as.numeric(names(var_left_label)),
  #   labels = var_left_label))
}

#' @rdname explore_graph
#' @export
explore_graph.q5 <- function(vars, select_id, df, data, scales_as_DA, lang = NULL,
                             font_family = "SourceSansPro", ...) {

  # Appease R CMD check
  var_left <- x <- NULL

  # Grab the shared info between the graphs
  shared_info <- explore_graph_info(vars = vars, font_family = font_family,
                                    scales_as_DA = scales_as_DA, select_id = select_id,
                                    data = data, lang = lang, df = df)

  # Color as function
  clr_df <- shared_info$colours_dfs$left_5
  clr <- \(x) clr_df$fill[2:6]

  # Get the scales ggplot function
  x_scale <- explore_graph_scale(var = vars$var_left,
                                 x_y = "x",
                                 data_vals = data$var_left)

  # Graph an appropriate number of bins
  var_left_num <- length(unique(data$var_left))
  bin_number <- max(15, min(25, var_left_num))

  # Get the breaks
  vals <- var_get_breaks(var = vars$var_left, df = shared_info$treated_df,
                         q3_q5 = "q5", pretty = FALSE, compact = FALSE)
  vals[1] <- -Inf
  vals[length(vals)] <- Inf

  # Draw the plot
  plot <-
    data[!is.na(data$var_left), ] |>
    ggplot2::ggplot(ggplot2::aes(var_left)) +
    ggplot2::geom_histogram(ggplot2::aes(fill = ggplot2::after_stat(x)),
                            bins = bin_number) +
    ggplot2::binned_scale(aesthetics = "fill",
                          scale_name = "stepsn",
                          palette = clr,
                          breaks = vals) +
    x_scale + shared_info$labs + shared_info$theme_default

  # Add selection
  if (!is.na(shared_info$select_id)) {
    plot <-
      plot +
      ggplot2::geom_vline(
        xintercept = data$var_left[data$ID == shared_info$select_id],
        colour = "black", linewidth = 1.5)
  }

  # Return
  return(plot)

}

#' @rdname explore_graph
#' @export
explore_graph.bivar <- function(vars, select_id, df, data, scales_as_DA, lang = NULL,
                                font_family = "SourceSansPro", ...) {

  # Appease R CMD check
  var_left <- var_right <-group <-  NULL

  # Grab the shared info between the graphs
  shared_info <- explore_graph_info(vars = vars, font_family = font_family,
                                    scales_as_DA = scales_as_DA, select_id = select_id,
                                    data = data, lang = lang, df = df)
  # Color as function
  clr_df <- shared_info$colours_dfs$bivar

  # Get the scales ggplot function
  x_scale <- explore_graph_scale(var = vars$var_right,
                                 x_y = "x",
                                 data_vals = data$var_right)
  y_scale <- explore_graph_scale(var = vars$var_left,
                                 x_y = "y",
                                 data_vals = data$var_left)

  # Get the stat smooth line opacity
  opac_line <- abs(stats::cor(data$var_left, data$var_right, use = "complete.obs"))

  # Get the point size
  point_size <- if (nrow(data) > 1000) {
    0.5
  } else if (nrow(data) > 500) {
    1
  } else 2

  # Draw the plot
  plot <-
    data |>
    remove_outliers_df(cols = c("var_left", "var_right")) |>
    ggplot2::ggplot(ggplot2::aes(var_right, var_left)) +
    ggplot2::geom_point(ggplot2::aes(colour = group), size = point_size) +
    ggplot2::stat_smooth(geom = "line", se = FALSE, method = "loess", span = 1,
                         formula = y ~ x, alpha = opac_line) +
    ggplot2::scale_colour_manual(values = stats::setNames(
      clr_df$fill, clr_df$group)) +
    x_scale + y_scale + shared_info$labs + shared_info$theme_default

  # Add selection
  if (!is.na(shared_info$select_id)) {
    plot <-
      plot +
      ggplot2::geom_point(
        data = data[data$ID == shared_info$select_id,], shape = 21,
        colour = "white", fill = "black", size = 4)
  }

  # Return
  return(plot)

}


#' @rdname explore_graph
#' @export
explore_graph.delta <- function(vars, select_id, df, data, scales_as_DA, lang = NULL,
                                font_family = "SourceSansPro", ...) {

  # Appease R CMD check
  var_left_1 <- var_left_2 <- group <- NULL

  # Grab the shared info between the graphs
  shared_info <- explore_graph_info(vars = vars, font_family = font_family,
                                    scales_as_DA = scales_as_DA, select_id = select_id,
                                    data = data, lang = lang, df = df)

  # Color as function
  clr_df <- shared_info$colours_dfs$delta

  # Get the scales ggplot function
  x_scale <- explore_graph_scale(var = structure(vars$var_left[1],
                                                 class = class(vars$var_left)),
                                 x_y = "x",
                                 data_vals = data$var_left_1)
  y_scale <- explore_graph_scale(var = structure(vars$var_left[2],
                                                 class = class(vars$var_left)),
                                 x_y = "y",
                                 data_vals = data$var_left_2)

  # Get the stat smooth line opacity
  opac_line <- abs(stats::cor(data$var_left_1, data$var_left_2,
                              use = "complete.obs"))

  # Get the point size
  point_size <- if (nrow(data) > 1000) {
    0.5
  } else if (nrow(data) > 500) {
    1
  } else 2

  # Draw plot
  plot <-
    data |>
    remove_outliers_df(cols = c("var_left_1", "var_left_2")) |>
    ggplot2::ggplot(ggplot2::aes(var_left_1, var_left_2)) +
    ggplot2::geom_smooth(se = FALSE, method = "lm", formula = y ~ x,
                colour = "black", size = 0.5) +
    ggplot2::geom_point(ggplot2::aes(colour = group)) +
      ggplot2::scale_colour_manual(values = stats::setNames(
        clr_df$fill, clr_df$group)) +
    x_scale + y_scale + shared_info$labs + shared_info$theme_default

  # Add selection
  if (!is.na(shared_info$select_id)) {
    plot <-
      plot +
      ggplot2::geom_point(
        data = data[data$ID == shared_info$select_id,], shape = 21,
        colour = "white", fill = "black", size = 4)
  }

  return(plot)

}

#' @rdname explore_graph
#' @export
explore_graph.delta_bivar <- function(vars, select_id, df, data, scales_as_DA, lang = NULL,
                                      font_family = "SourceSansPro", ...) {

  # Appease R CMD check
  var_left <- var_right <- group <- NULL

  # Grab the shared info between the graphs
  shared_info <- explore_graph_info(vars = vars, font_family = font_family,
                                    scales_as_DA = scales_as_DA, select_id = select_id,
                                    data = data, lang = lang, df = df)

  # Color as function
  clr_df <- shared_info$colours_dfs$bivar

  # Get the scales ggplot function
  x_scale <- explore_graph_scale.pct(x_y = "x")
  y_scale <- explore_graph_scale.pct(x_y = "y")

  # Get the stat smooth line opacity
  opac_line <- abs(stats::cor(data$var_left, data$var_right, use = "complete.obs"))

  # Get the point size
  point_size <- if (nrow(data) > 1000) {
    0.5
  } else if (nrow(data) > 500) {
    1
  } else 2

  # Draw the plot
  plot <-
    data |>
    remove_outliers_df(cols = c("var_left", "var_right")) |>
    ggplot2::ggplot(ggplot2::aes(var_right, var_left)) +
    ggplot2::geom_point(ggplot2::aes(colour = group), size = point_size) +
    ggplot2::stat_smooth(geom = "line", se = FALSE, method = "loess", span = 1,
                         formula = y ~ x, alpha = opac_line) +
    ggplot2::scale_colour_manual(values = stats::setNames(
      clr_df$fill, clr_df$group)) +
    x_scale + y_scale + shared_info$labs + shared_info$theme_default

  # Add selection
  if (!is.na(shared_info$select_id)) {
    plot <-
      plot +
      ggplot2::geom_point(
        data = data[data$ID == shared_info$select_id,], shape = 21,
        colour = "white", fill = "black", size = 4)
  }

  # Return
  return(plot)

}
