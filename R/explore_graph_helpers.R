#' Explore Graph Info Function
#'
#' This function returns shared information between the graphs, such as the
#' default theme and colors.
#'
#' @param vars <`named list`> A list object of variable codes with classes. The
#' output of \code{\link{vars_build}}.
#' @param font_family <`character`> A string specifying the font family for the
#' plot, default is "acidgrotesk-book".
#' @param lang <`character`> A character string indicating the language to
#' translate variable titles to.
#' @param scale <`reactive character`> Current scale. The output of
#' \code{\link{update_scale}}.
#' @param scales_as_DA <`character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their graph will be the one of their DA.
#' @param select_id <`character`> A string indicating the ID of the currently
#' selected region (if any). Usually `r[[id]]$select_id()`
#' @param data <`data.frame`> A data frame containing the variables and
#' observations. The output of \code{\link{data_get}}.
#' @param time <`numeric named list`> The `time` at which data is displayed.
#' A list for var_left and var_right. The output of \code{\link{vars_build}}(...)$time.
#' @param ... Additional arguments passed to the specific method.
#'
#' @return A list containing the default theme and color data frames.
explore_graph_info <- function(vars, font_family = "acidgrotesk-book", lang = NULL,
                               scale, scales_as_DA, select_id, data, time, ...) {
  # Create the theme
  theme_default <- list(
    ggplot2::theme_minimal(),
    ggplot2::theme(
      text = ggplot2::element_text(family = font_family, size = 12),
      legend.position = "none",
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    )
  )

  # Use the legend labels
  labs <- explore_graph_labels(vars = vars, lang = lang)

  # Grab the colours
  colours_dfs <- colours_get()

  # In the case where the selected ID must be updated
  if (is_scale_in(scales_as_DA, scale) & !is.na(select_id)) {
    select_id <- grab_DA_ID_from_bslike(scale = scale, select_id = select_id)
  }

  # df treatment if it's in the scales as DA
  treated_scale <- treat_to_DA(scales_as_DA = scales_as_DA, scale = scale)

  # In the case where the selected ID is not in data, clear the selection
  if (!is.na(select_id) & !select_id %in% data$ID) {
    select_id <- NA
  }

  # Return
  return(list(
    theme_default = theme_default,
    colours_dfs = colours_dfs,
    labs = labs,
    select_id = select_id,
    treated_scale = treated_scale
  ))
}

#' Custom ggplot2 jitter function
#'
#' This function applies a jitter effect to a ggplot2 scatter plot to better
#' visualize overlapping points. The jitter effect is dependent on the
#' proportion of unique values to total observations in the data.
#'
#' @param dat <`data.frame`> A data frame or tibble object that contains the data.
#' @param cols <`character vector`> A character vector of length 2 specifying the
#' column names of 'dat' that will be used for the x and y coordinates, respectively.
#'
#' @param ... Additional arguments passed to the ggplot2::geom_jitter function.
#' Examples include 'alpha' for transparency, 'size' for point size, or 'color'
#' for point color. `ggplot2::aes()` also works here.
#'
#' @return A ggplot2::geom_jitter layer with customized width and height.
#' If the proportion of unique values to total observations in a given column
#' is less than 0.1, jitter will be applied along that axis (width for x,
#' height for y) to improve visualization. Otherwise, no jitter will be applied
#' along that axis.
explore_graph_point_jitter <- function(dat, cols, ...) {
  vals_obs_ratio_x <- length(unique(dat[[cols[1]]])) / nrow(dat)
  vals_obs_ratio_y <- length(unique(dat[[cols[2]]])) / nrow(dat)

  width <- if (vals_obs_ratio_x < 0.1) 0.4 else 0
  height <- if (vals_obs_ratio_y < 0.1) 0.4 else 0

  return(list(ggplot2::geom_jitter(width = width, height = height, ...)))
}
