#' Explore Graph Function for `INDEX` type
#'
#' This function creates a ggplot for the explore panel of Curbcut, based on the
#' input parameters, for the index type variables.
#'
#' @param vars <`named list`> A list object of variable codes with classes. The
#' output of \code{\link{vars_build}}.
#' @param select_id <`character`> A string indicating the ID of the currently
#' selected region (if any). Usually `r[[id]]$select_id()`
#' @param time <`numeric named list`> The `time` at which data is displayed.
#' A list for var_left and var_right. The output of \code{\link{vars_build}}(...)$time.
#' @param schemas <`named list`> Current schema information. The additional widget
#' values that have an impact on which data column to pick. Usually `r[[id]]$schema()`.
#' @param scale <`character`> Current scale.
#' @param data <`data.frame`> A data frame containing the variables and
#' observations. The output of \code{\link{data_get}}.
#' @param scales_as_DA <`character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their graph will be the one of their DA.
#' @param lang <`character`> A character string indicating the language to
#' translate variable titles to.
#' @param font_family <`character`> A string specifying the font family for the
#' plot, default is "acidgrotesk-book".
#' @param val <`numeric`> If the value is not part of `data`. It happens on raster
#' data where we show region values for the highest resolution possible, but we still
#' want to allow user to select grid cells of lower resolutions.
#' @param ... Additional arguments passed to the specific method.
#'
#' @return A ggplot2 object representing the plot.
#' @export
explore_graph_q5_ind <- function(vars, select_id, scale, data, time, schemas,
                                 scales_as_DA = c("building", "street"), lang = NULL,
                                 font_family = "acidgrotesk-book",
                                 val = NULL, ...) {
  UseMethod("explore_graph_q5_ind", vars)
}

#' @rdname explore_graph_q5_ind
#' @export
explore_graph_q5_ind.scalar <- function(vars, select_id, scale, data, time, schemas,
                                        scales_as_DA = c("building", "street"),
                                        lang = NULL,
                                        font_family = "acidgrotesk-book",
                                        val = NULL, ...) {
  # Appease R CMD check
  var_left <- x <- ..count.. <- NULL

  # Grab the shared info between the graphs
  shared_info <- explore_graph_info(
    vars = vars, font_family = font_family,
    scales_as_DA = scales_as_DA, select_id = select_id,
    data = data, lang = lang, scale = scale
  )

  # Color as function
  clr_df <- shared_info$colours_dfs$left_5
  clr <- \(x) clr_df$fill[2:6]

  rcol <- sprintf("var_left_%s", time$var_left)

  # Keep the data inside the breaks
  vl_breaks <- attr(data, "breaks_var_left")

  # Get the scales ggplot function
  x_scale <- explore_graph_scale(
    var = vars$var_left,
    x_y = "x",
    data_vals = data[[rcol]],
    scale = shared_info$treated_scale,
    lang = lang
  )

  # Graph an appropriate number of bins
  var_left_num <- length(unique(data[[rcol]]))
  bin_number <- min(15, ceiling(0.8 * var_left_num))

  # Get the breaks
  vals <- attr(data, "breaks_var_left")
  vals[1] <- -Inf
  vals[length(vals)] <- Inf

  # Draw the plot
  plot <-
    data[!is.na(data[[rcol]]), rcol] |>
    # remove_outliers_df(cols = c("var_left")) |>
    ggplot2::ggplot(ggplot2::aes(!!ggplot2::sym(rcol))) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(..count.. / sum(..count..)),
                                         fill = ggplot2::after_stat(x)),
                            bins = bin_number
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::binned_scale(
      aesthetics = "fill",
      palette = clr,
      breaks = vals
    ) +
    x_scale +
    shared_info$labs +
    shared_info$theme_default

  # Add selection
  if (!is.na(shared_info$select_id) | !is.null(val)) {
    val <- if (!is.null(val)) val else data[[rcol]][data$ID == shared_info$select_id]
    if (!any(is.na(val))) {
      plot <-
        plot +
        ggplot2::geom_vline(
          xintercept = val,
          colour = "black", linewidth = 1.5
        )
    }
  }

  # Return
  return(plot)
}

#' @rdname explore_graph_q5_ind
#' @export
explore_graph_q5_ind.ordinal <- function(vars, select_id, scale, data, time, schemas,
                                         scales_as_DA = c("building", "street"),
                                         lang = NULL,
                                         font_family = "acidgrotesk-book",
                                         val = NULL, ...) {

  # Appease R CMD check
  var_left <- occ <- NULL

  # Grab the shared info between the graphs
  shared_info <- explore_graph_info(
    vars = vars, font_family = font_family,
    scales_as_DA = scales_as_DA, select_id = select_id,
    data = data, lang = lang, scale = scale
  )

  # Color as function
  clr_df <- shared_info$colours_dfs$left_5
  clr <- clr_df$fill[1:6]

  rcol <- sprintf("var_left_%s", time$var_left)

  # Keep the data inside the breaks
  vl_breaks <- attr(data, "breaks_var_left")

  # Get the scales ggplot function
  x_scale <- explore_graph_scale(
    var = vars$var_left,
    x_y = "x",
    data_vals = data[[rcol]],
    scale = shared_info$treated_scale,
    lang = lang,
    breaks = 0:5
  )

  # Construct the data to make sure all breaks are represented
  dat <- data[!is.na(data[[rcol]]), ]
  dat[[rcol]] <- factor(dat[[rcol]], levels = 0:5)
  dat <- table(dat[[rcol]])
  dat <- data.frame(
    var_left = names(dat),
    occ = as.numeric(dat)
  )
  dat$occ <- dat$occ / sum(dat$occ)

  # Draw the plot
  plot <-
    dat |>
    ggplot2::ggplot(ggplot2::aes(x = var_left, y = occ)) +
    ggplot2::geom_bar(ggplot2::aes(fill = var_left), stat = "identity", width = 1) +
    ggplot2::scale_fill_manual(breaks = 0:5, values = clr, na.translate = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    x_scale +
    shared_info$labs +
    shared_info$theme_default

  # Add selection
  if (!is.na(shared_info$select_id) | !is.null(val)) {
    if (is.null(val)) {
      data[[rcol]][data$ID == shared_info$select_id]
    }
    if (!any(is.na(val))) {
      plot <-
        plot +
        ggplot2::geom_vline(
          xintercept = val + 1,
          colour = "black", linewidth = 1.5
        )
    }
  }

  return(plot)
}
