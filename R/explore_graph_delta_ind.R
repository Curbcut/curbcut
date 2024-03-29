#' Explore Graph Function for delta `INDEX` type
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
#' @param val <`numeric`> If the value is not part of `data`. It happens on raster
#' data where we show region values for the highest resolution possible, but we still
#' want to allow user to select grid cells of lower resolutions.
#' @param ... Additional arguments passed to the specific method.
#'
#' @return A ggplot2 object representing the plot.
#' @export
explore_graph_delta_ind <- function(vars, select_id, scale, data, time, schemas,
                                    scales_as_DA = c("building", "street"), lang = NULL,
                                    font_family = "acidgrotesk-book",
                                    val = NULL, ...) {
  UseMethod("explore_graph_delta_ind", vars)
}

#' @describeIn explore_graph_delta_ind Scalar method
#' @export
explore_graph_delta_ind.scalar <- function(vars, select_id, scale, data, time, schemas,
                                           scales_as_DA = c("building", "street"),
                                           lang = NULL,
                                           font_family = "acidgrotesk-book",
                                           val = NULL, ...) {
  explore_graph.delta(
    vars = vars, select_id = select_id, scale = scale, data = data, time = time,
    schemas = schemas,
    scales_as_DA = scales_as_DA, lang = lang, font_family = font_family,
    val = val, ...
  )
}

#' @describeIn explore_graph_delta_ind Ordinal method
#' @export
explore_graph_delta_ind.ordinal <- function(vars, select_id, scale, data, time, schemas,
                                            scales_as_DA = c("building", "street"),
                                            lang = NULL,
                                            font_family = "acidgrotesk-book",
                                            val = NULL, ...) {
  # Appease R CMD check
  var_left_1 <- var_left_2 <- group <- frequency <- NULL

  # Grab the shared info between the graphs
  shared_info <- explore_graph_info(
    vars = vars, font_family = font_family,
    scales_as_DA = scales_as_DA, select_id = select_id,
    data = data, lang = lang, scale = scale, time = time
  )

  # Color as function
  clr_df <- shared_info$colours_dfs$delta
  clr_df$fill[2] <- clr_df$fill[1]
  clr_df$fill[4] <- clr_df$fill[5]

  # Get the scales ggplot function
  ycol <- match_schema_to_col(data = data, time = time$var_left[2], col = "var_left", schemas = schemas)
  xcol <- match_schema_to_col(data = data, time = time$var_left[1], col = "var_left", schemas = schemas)


  x_scale <- explore_graph_scale(
    var = structure(vars$var_left,
      class = class(vars$var_left)
    ),
    x_y = "x",
    scale = shared_info$treated_scale,
    breaks = 0:5
  )

  y_scale <- explore_graph_scale(
    var = structure(vars$var_left,
      class = class(vars$var_left)
    ),
    scale = shared_info$treated_scale,
    x_y = "y",
    breaks = 0:5
  )


  # Calculate frequency to use as opacity (except when var_left_1 == var_left_2)
  dat <- data
  dat$frequency <- 1

  # Create a formula for the aggregate function dynamically
  formula_text <- sprintf("frequency ~ %s + %s + group", xcol, ycol)
  agg_formula <- stats::as.formula(formula_text)

  # Use the dynamically created formula in the aggregate function
  dat <- stats::aggregate(agg_formula,
    data = dat,
    FUN = sum
  )

  # Unchanged vs changed
  unchanged <- dat[dat[[xcol]] == dat[[ycol]], ]
  changed <- dat[dat[[xcol]] != dat[[ycol]], ]

  # Rescale frequency; skip rows with frequency = 0
  changed$frequency <- scales::rescale(changed$frequency)^(0.4)
  unchanged$frequency <- scales::rescale(unchanged$frequency)^(0.4)
  dat <- rbind(changed, unchanged)

  # Draw plot
  plot <-
    dat |>
    ggplot2::ggplot(ggplot2::aes(as.factor(!!ggplot2::sym(xcol)), as.factor(!!ggplot2::sym(ycol)))) +
    ggplot2::geom_tile(ggplot2::aes(fill = group, alpha = frequency)) +
    ggplot2::scale_fill_manual(values = stats::setNames(
      clr_df$fill, clr_df$group
    )) +
    x_scale +
    y_scale +
    shared_info$labs +
    shared_info$theme_default +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())

  # Add selection
  if (!is.na(shared_info$select_id) | !is.null(val)) {
    dat <- if (!is.null(val)) {
      dat <- data.frame(matrix(ncol = 2, nrow = 1))
      names(dat) <- c(xcol, ycol)
      dat[1, xcol] <- val[1]
      dat[1, ycol] <- val[2]
      dat
    } else data[data$ID == shared_info$select_id, ]
    if (!any(is.na(dat))) {
      plot <-
        plot +
        ggplot2::geom_tile(
          data = dat,
          color = "white", fill = "transparent", linewidth = 1.5
        )
    }
  }

  return(plot)
}
