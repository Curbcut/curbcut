#' Get information (labels, breaks, themes, ...) for legend creation
#'
#' Given variables and optional arguments, this function computes the legend
#' labels and breaks, and returns them with a default theme for the legend.
#'
#' @param vars <`named list`> A list object with a pre-determined class. The
#' output of \code{\link{vars_build}}.
#' @param font_family <`character`> Which font family should be used to render
#' the legend (breaks, axis titles, ...). Defaults to `acidgrotesk-book`. To use
#' the default font family og ggplot2, use `NULL`.
#' @param scales_as_DA <`reactive character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#' @param df <`reactive character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param ... Additional arguments to be passed to \code{\link{legend_labels}}
#' and \code{\link{legend_breaks}}, such as `lang`, `df`, ...
#'
#' @return A list with the legend labels and breaks computed by
#' \code{legend_labels()} and \code{legend_breaks()}, and a default theme for
#' the legend.
legend_get_info <- function(vars, font_family = "acidgrotesk-book", scales_as_DA,
                            df, ...) {
  df <- treat_to_DA(scales_as_DA = scales_as_DA, df = df)

  labs_xy <- legend_labels(vars, ...)
  break_labs <- legend_breaks(vars, df = df, ...)
  theme_default <- list(
    ggplot2::theme_minimal(),
    ggplot2::theme(
      text = ggplot2::element_text(family = font_family, size = 12),
      legend.position = "none",
      panel.grid = ggplot2::element_blank()
    )
  )
  colours_dfs <- colours_get()

  return(list(
    labs_xy = labs_xy, break_labs = break_labs,
    theme_default = theme_default,
    colours_dfs = colours_dfs,
    df = df
  ))
}

#' Generic legend render function for Curbcut legends
#'
#' `legend_render` is a generic function used to produce the legend for the
#' Curbcut map pages. The function invokes particular methods which depend on
#' the class of the `vars` argument.
#'
#' @param vars <`named list`> A list object with a pre-determined class. The
#' output of \code{\link{vars_build}}.
#' @param font_family <`character`> Which font family should be used to render
#' the legend (breaks, axis titles, ...). Defaults to `acidgrotesk-book`. To use
#' the default font family og ggplot2, use `NULL`.
#' @param scales_as_DA <`reactive character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#' @param df <`reactive character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#'
#' @param ... Arguments to be passed to the methods, e.g. optionally `lang`
#'
#' @return It returns a ggplot object
#' @export
legend_render <- function(vars, font_family = "acidgrotesk-book",
                          scales_as_DA = c("building", "street"), df, ...) {
  UseMethod("legend_render", vars)
}

#' Render the legend for the q5 class
#'
#' This function generates a plot which includes the NA category. It uses the
#' \code{\link{legend_get_info}} function to obtain the necessary
#' information about the legend, and then modifies the
#' breaks to add the NA category to the legend. It looks at the variable `type`
#' to detect wether the variable has character breaks with the `qual` type in
#' the variables table. If so, breaks appear in the middle of the lines.
#' Finally, it creates the plot using the ggplot2 package and the obtained
#' information.
#'
#' @param vars <`named list`> A list object with a `q5` class. The
#' output of \code{\link{vars_build}}.
#' @param font_family <`character`> Which font family should be used to render
#' the legend (breaks, axis titles, ...). Defaults to `acidgrotesk-book`. To use
#' the default font family og ggplot2, use `NULL`.
#' @param scales_as_DA <`reactive character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#' @param df <`reactive character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param ... additional arguments to be passed to \code{\link{legend_get_info}}.
#'
#' @return A plot generated using ggplot2.
#' @export
legend_render.q5 <- function(vars, font_family = "acidgrotesk-book",
                             scales_as_DA = c("building", "street"), df, ...) {
  # NULL out problematic variables for the R CMD check (no visible binding for
  # global variable)
  group <- y <- fill <- xmin <- xmax <- NULL

  # Get all necessary information
  leg_info <- legend_get_info(vars,
    font_family = font_family, scales_as_DA = scales_as_DA,
    df = df, ...
  )

  # Adapt breaks to add the `NA` bar
  leg <- leg_info$colours_dfs$left_5[2:6, 2:3]

  # Get real q5 breaks
  brks <- var_get_info(vars$var_left, what = "breaks_q5")[[1]]
  brks <- brks$var[brks$df == df]

  # If all NA, don't bother draw a legend. Return NULL
  if (all(is.na(brks))) {
    return(NULL)
  }

  # Complete the xmin and xmax
  leg$xmin <- brks[1:(length(brks) - 1)]
  leg$xmax <- brks[2:(length(brks))]

  # Tweak the min and max breaks so that they take a minimum of 15% of the
  # plot space
  rect_size <- leg$xmax - leg$xmin
  size_pct <- rect_size / sum(rect_size)

  # Go over each value (that isn't the blank space or the NA) and make sure it
  # takes at least 15% of the plot space. Reduce the size of the other rectangle
  # that are larger than 15% of the plot space.
  rect_pct_vals <- size_pct

  if ((sum(rect_pct_vals < 0.15) > 0)) {
    while (sum(rect_pct_vals < 0.15) > 0) {
      for (i in which(rect_pct_vals < 0.15)) {
        # How much it's increased
        inc <- 0.15 - rect_pct_vals[i]

        # Its new value
        rect_pct_vals[i] <- 0.15

        # Which other vars can be increased
        can_reduce <- which(rect_pct_vals > 0.15)
        can_reduce_vals <- rect_pct_vals[can_reduce]
        for (c in can_reduce) {
          prop_reduce <- rect_pct_vals[c] / sum(rect_pct_vals[can_reduce])

          rect_pct_vals[c] <- rect_pct_vals[c] - (inc * prop_reduce)
        }
      }
    }
  }

  # The new values for each rectangle that leads to a minimum of 15% plot space
  values_updated <- rect_pct_vals * (brks[length(brks)] - brks[1])
  # cumsum the values to get the xmax and xmax
  values_updated <- cumsum(values_updated)
  leg$xmax <- values_updated
  leg$xmin <- c(0, values_updated[1:(length(values_updated) - 1)])

  # Blank space addition
  blank <- (brks[length(brks)] - brks[1]) / 16
  leg <- rbind(
    data.frame(
      y = 1,
      fill = "#FFFFFFFF",
      xmin = leg$xmin[1] - blank,
      xmax = leg$xmin[1]
    ),
    leg
  )

  # NA (grey) space addition
  leg <- rbind(
    data.frame(
      y = 1,
      fill = "#B3B3BB",
      xmin = leg$xmin[1] - blank,
      xmax = leg$xmin[1]
    ),
    leg
  )

  # Breaks placement
  breaks_placement <- c(-(blank + (blank / 2)), 0, values_updated)

  # Grab labels to check length
  breaks_label <- leg_info$break_labs[!is.na(leg_info$break_labs)]
  breaks_label <- c("NA", breaks_label)
  if (length(breaks_placement) != length(breaks_label)) {
    warning(paste0(
      "The number of breaks is not the same as the number of break ",
      "labels. For a `q5` map, there needs to be 6 breaks in the ",
      "`variables$break_q5` table (for ranks 0:5)."
    ))
    return(NULL)
  }

  # Make the plot
  leg |>
    ggplot2::ggplot(
      ggplot2::aes(
        xmin = xmin, xmax = xmax, ymin = y - 1,
        ymax = y, fill = fill
      )
    ) +
    ggplot2::geom_rect() +
    ggplot2::scale_x_continuous(
      breaks = breaks_placement,
      labels = breaks_label
    ) +
    ggplot2::scale_y_continuous(labels = NULL) +
    ggplot2::scale_fill_manual(values = stats::setNames(leg$fill, leg$fill)) +
    leg_info$labs_xy +
    leg_info$theme_default
}

#' Render the legend for the q5 class
#'
#' This function generates a plot which includes the NA category. It uses the
#' \code{\link{legend_get_info}} function to obtain the necessary
#' information about the legend, and then modifies the
#' breaks to add the NA category to the legend. It looks at the variable `type`
#' to detect wether the variable has character breaks with the `qual` type in
#' the variables table. If so, breaks appear in the middle of the lines.
#' Finally, it creates the plot using the ggplot2 package and the obtained
#' information.
#'
#' @param vars <`named list`> A list object with a `q5` class. The
#' output of \code{\link{vars_build}}.
#' @param font_family <`character`> Which font family should be used to render
#' the legend (breaks, axis titles, ...). Defaults to `acidgrotesk-book`. To use
#' the default font family og ggplot2, use `NULL`.
#' @param scales_as_DA <`reactive character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#' @param df <`reactive character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param ... additional arguments to be passed to \code{\link{legend_get_info}}.
#'
#' @return A plot generated using ggplot2.
#' @export
legend_render.q5_ind <- function(vars, font_family = "acidgrotesk-book",
                                 scales_as_DA = c("building", "street"), df, ...) {
  # NULL out problematic variables for the R CMD check (no visible binding for
  # global variable)
  group <- y <- fill <- NULL

  # Get all necessary information
  leg_info <- legend_get_info(vars,
    font_family = font_family, scales_as_DA = scales_as_DA,
    df = df, ...
  )

  # Adapt breaks to add the `NA` bar
  leg <- leg_info$colours_dfs$left_5[1:6, ]
  leg$group <- suppressWarnings(as.double(leg$group))
  leg[1, ]$group <- 0.5
  leg[seq(2 + 1, nrow(leg) + 1), ] <- leg[seq(2, nrow(leg)), ]
  leg[2, ] <- list(x = 0.75, y = 1, fill = "#FFFFFFFF")

  # Adjust break placements if breaks are characters
  breaks_placement <- c(-0.375, c(0:4) + 0.5)

  # Grab labels to check length
  breaks_label <- c("NA", leg_info$break_labs)
  if (length(breaks_placement) != length(breaks_label)) {
    warning(paste0(
      "The number of breaks is not the same as the number of break ",
      "labels.  For a `q5` map with a variable whose breaks are ",
      "characters, there must  be 5 breaks in the `variables$break_q5` ",
      "(for ranks 1:5) with the exception of rank 0 that can be NA, ",
      "which will be filtered out automatically."
    ))
    return(NULL)
  }

  # Make the plot
  leg |>
    ggplot2::ggplot(
      ggplot2::aes(
        xmin = group - 1, xmax = group, ymin = y - 1,
        ymax = y, fill = fill
      )
    ) +
    ggplot2::geom_rect() +
    ggplot2::scale_x_continuous(
      breaks = breaks_placement,
      labels = breaks_label
    ) +
    ggplot2::scale_y_continuous(labels = NULL) +
    ggplot2::scale_fill_manual(values = stats::setNames(leg$fill, leg$fill)) +
    leg_info$labs_xy +
    leg_info$theme_default
}

#' Render the legend for the qualitative class
#'
#' This function generates a plot with a qualitative color legend using the
#' ggplot2 package. It uses the \code{\link{legend_get_info}} function
#' to obtain the necessary information about the legend and cuts the qualitative
#' color table (\code{colours$qual}) to the number of breaks. The function then
#' generates the plot using the obtained information.
#'
#' @param vars <`named list`> A list object with a `qual` class. The
#' output of \code{\link{vars_build}}.
#' @param font_family <`character`> Which font family should be used to render
#' the legend (breaks, axis titles, ...). Defaults to `acidgrotesk-book`. To use
#' the default font family og ggplot2, use `NULL`.
#' @param scales_as_DA <`reactive character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#' @param df <`reactive character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param ... Additional arguments to be passed to \code{\link{legend_get_info}}.
#'
#' @return A plot generated using ggplot2.
#' @export
legend_render.qual <- function(vars, font_family = "acidgrotesk-book",
                               scales_as_DA = c("building", "street"), df, ...) {
  # NULL out problematic variables for the R CMD check (no visible binding for
  # global variable)
  group <- y <- fill <- NULL

  # Get all necessary information
  leg_info <- legend_get_info(vars,
    font_family = font_family, scales_as_DA = scales_as_DA,
    df = df, ...
  )

  # Cut for the number of breaks
  leg_info$break_labs <- leg_info$break_labs[!is.na(leg_info$break_labs)]
  if (length(leg_info$break_labs) < nrow(leg_info$colours_dfs$qual)) {
    stop(paste0(
      "There are not enough colours in the qualitative colours ",
      "table `colours$qual`."
    ))
  }
  colours_qual <- leg_info$colours_dfs$qual[1:length(leg_info$break_labs), ]

  # Switch the `group` character vector to a numeric
  colours_qual$group <- suppressWarnings(as.double(colours_qual$group))

  # Make the plot
  colours_qual |>
    ggplot2::ggplot(ggplot2::aes(
      xmin = group - 1, xmax = group, ymin = y - 1,
      ymax = y, fill = fill
    )) +
    ggplot2::geom_rect() +
    ggplot2::scale_x_continuous(
      breaks = colours_qual$group - 0.5,
      labels = leg_info$break_labs
    ) +
    ggplot2::scale_y_continuous(labels = NULL) +
    ggplot2::scale_fill_manual(values = stats::setNames(
      colours_qual$fill, colours_qual$fill
    )) +
    leg_info$labs_xy +
    leg_info$theme_default
}

#' Render the legend for the bivariate class
#'
#' This function generates a plot with a bivariate color legend using the
#' ggplot2 package. It uses the \code{\link{legend_get_info}} function
#' to show the relationship between two variables. The legend consists of a grid
#' of colored squares with text labels indicating the meaning of each square.
#'
#' @param vars <`named list`> A list object with a `bivar` class. The
#' output of \code{\link{vars_build}}.
#' @param font_family <`character`> Which font family should be used to render
#' the legend (breaks, axis titles, ...). Defaults to `acidgrotesk-book`. To use
#' the default font family og ggplot2, use `NULL`.
#' @param lang <`character`> The language to use for the text labels. Defaults
#' to NULL for no translation.
#' @param scales_as_DA <`reactive character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#' @param df <`reactive character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param ... Additional arguments passed to other functions.
#'
#' @return A ggplot object that represents the bivariate legend.
#' @export
legend_render.bivar <- function(vars, font_family = "acidgrotesk-book",
                                scales_as_DA = c("building", "street"),
                                df, lang = NULL, ...) {
  # NULL out problematic variables for the R CMD check (no visible binding for
  # global variable)
  x <- y <- fill <- label <- label_colour <- NULL

  # Get all necessary information
  leg_info <- legend_get_info(vars,
    lang = lang, font_family = font_family,
    scales_as_DA = scales_as_DA, df = df, ...
  )

  # Prepare the grid's labels location and the colours
  leg <- leg_info$colours_dfs$bivar[1:9, ]
  leg$label <-
    c(
      cc_t(lang = lang, "Both low"), " ",
      paste0(leg_info$labs_xy$y_short, "\n", cc_t(lang = lang, "high only")),
      " ", " ", " ",
      paste0(leg_info$labs_xy$x_short, "\n", cc_t(lang = lang, "high only")),
      " ",
      cc_t(lang = lang, "Both high")
    )
  leg$label_colour <- c(rep("black", 8), "white")
  leg$x <- leg$x - 0.5
  leg$y <- leg$y - 0.5

  # Maker the plot
  ggplot2::ggplot(leg, ggplot2::aes(y, x, fill = fill)) +
    ggplot2::geom_raster() +
    ggplot2::geom_text(ggplot2::aes(y, x, label = label, colour = label_colour),
      inherit.aes = FALSE, size = 3
    ) +
    ggplot2::scale_x_continuous(breaks = 0:3, labels = leg_info$break_labs$x) +
    ggplot2::scale_y_continuous(breaks = 0:3, labels = leg_info$break_labs$y) +
    ggplot2::scale_fill_manual(values = stats::setNames(
      leg_info$colours_dfs$bivar$fill[1:9], leg_info$colours_dfs$bivar$fill[1:9]
    )) +
    ggplot2::scale_colour_manual(values = c("black" = "black", "white" = "white")) +
    leg_info$labs_xy[[1]] +
    leg_info$theme_default
}

#' Render the legend for a `delta` class (variation between two years).
#'
#' The legend displays a color scale going from red to blue, representing the
#' changes in the data between two years. The function generates a plot with a
#' rectangle for each color and a label indicating the percentage change
#' associated with the color. The first rectangle is a neutral gray color, with
#' a label indicating that it represents missing data.
#'
#' @param vars <`named list`> A list object with a `delta` class. The
#' output of \code{\link{vars_build}}.
#' @param font_family <`character`> Which font family should be used to render
#' the legend (breaks, axis titles, ...). Defaults to `acidgrotesk-book`. To use
#' the default font family og ggplot2, use `NULL`.
#' @param scales_as_DA <`reactive character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#' @param df <`reactive character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param ... Additional arguments passed to other functions.
#'
#' @return A ggplot object that represents the `delta` legend.
#' @export
legend_render.delta <- function(vars, font_family = "acidgrotesk-book",
                                scales_as_DA = c("building", "street"), df, ...) {
  # NULL out problematic variables for the R CMD check (no visible binding for
  # global variable)
  group <- y <- fill <- NULL

  # Get all necessary information
  leg_info <- legend_get_info(vars,
                              font_family = font_family,
                              scales_as_DA = scales_as_DA,
                              df = df, ...)

  # Adapt breaks to add the `NA` bar
  leg <- leg_info$colours_dfs$delta[1:5, 2:3]

  # Get breaks as numeric
  brks <- breaks_delta(vars = vars, df = df, character = FALSE)

  # If all NA, don't bother draw a legend. Return NULL
  if (all(is.na(brks))) {
    return(NULL)
  }

  # Complete the xmin and xmax
  leg$xmin <- brks[1:(length(brks) - 1)]
  leg$xmax <- brks[2:(length(brks))]

  # Tweak the min and max breaks so that they take a minimum of 15% of the
  # plot space
  rect_size <- leg$xmax - leg$xmin
  size_pct <- rect_size / sum(rect_size)

  # Go over each value (that isn't the blank space or the NA) and make sure it
  # takes at least 15% of the plot space. Reduce the size of the other rectangle
  # that are larger than 15% of the plot space.
  rect_pct_vals <- size_pct

  if ((sum(rect_pct_vals < 0.15) > 0)) {
    while (sum(rect_pct_vals < 0.15) > 0) {
      for (i in which(rect_pct_vals < 0.15)) {
        # How much it's increased
        inc <- 0.15 - rect_pct_vals[i]

        # Its new value
        rect_pct_vals[i] <- 0.15

        # Which other vars can be increased
        can_reduce <- which(rect_pct_vals > 0.15)
        can_reduce_vals <- rect_pct_vals[can_reduce]
        for (c in can_reduce) {
          prop_reduce <- rect_pct_vals[c] / sum(rect_pct_vals[can_reduce])

          rect_pct_vals[c] <- rect_pct_vals[c] - (inc * prop_reduce)
        }
      }
    }
  }

  # The new values for each rectangle that leads to a minimum of 15% plot space
  values_updated <- rect_pct_vals * (brks[length(brks)] - brks[1])
  # cumsum the values to get the xmax and xmax
  values_updated <- cumsum(values_updated)
  leg$xmax <- values_updated
  leg$xmin <- c(0, values_updated[1:(length(values_updated) - 1)])

  # Blank space addition
  blank <- (brks[length(brks)] - brks[1]) / 16
  leg <- rbind(
    data.frame(
      y = 1,
      fill = "#FFFFFFFF",
      xmin = leg$xmin[1] - blank,
      xmax = leg$xmin[1]
    ),
    leg
  )

  # NA (grey) space addition
  leg <- rbind(
    data.frame(
      y = 1,
      fill = "#B3B3BB",
      xmin = leg$xmin[1] - blank,
      xmax = leg$xmin[1]
    ),
    leg
  )

  # Breaks placement
  breaks_placement <- c(-(blank + (blank / 2)), 0, values_updated)

  # Grab labels to check length
  breaks_label <- leg_info$break_labs[!is.na(leg_info$break_labs)]
  breaks_label <- c("NA", breaks_label)
  if (length(breaks_placement) != length(breaks_label)) {
    warning(paste0(
      "The number of breaks is not the same as the number of break ",
      "labels. For a `q5` map, there needs to be 6 breaks in the ",
      "`variables$break_q5` table (for ranks 0:5)."
    ))
    return(NULL)
  }

  # Make the plot
  leg |>
    ggplot2::ggplot(
      ggplot2::aes(
        xmin = xmin, xmax = xmax, ymin = y - 1,
        ymax = y, fill = fill
      )
    ) +
    ggplot2::geom_rect() +
    ggplot2::scale_x_continuous(
      breaks = breaks_placement,
      labels = breaks_label
    ) +
    ggplot2::scale_y_continuous(labels = NULL) +
    ggplot2::scale_fill_manual(values = stats::setNames(leg$fill, leg$fill)) +
    leg_info$labs_xy +
    leg_info$theme_default
}


#' Render the legend for a `q100` class
#'
#' This function generates a legend displaying a variation in viridis colour
#' scales with labels going from `Low` to `High`.
#'
#' @param vars <`named list`> A list object with a `q100` class. The
#' output of \code{\link{vars_build}}.
#' @param font_family <`character`> Which font family should be used to render
#' the legend (breaks, axis titles, ...). Defaults to `acidgrotesk-book`. To use
#' the default font family og ggplot2, use `NULL`.
#' @param scales_as_DA <`reactive character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#' @param df <`reactive character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param ... Additional arguments passed to other functions.
#'
#' @return A ggplot object that represents the `q100` legend.
#' @export
legend_render.q100 <- function(vars, font_family = "acidgrotesk-book",
                               scales_as_DA = c("building", "street"), df, ...) {
  # NULL out problematic variables for the R CMD check (no visible binding for
  # global variable)
  group <- y <- fill <- NULL

  # Get all necessary information
  leg_info <- legend_get_info(vars,
    font_family = font_family, scales_as_DA = scales_as_DA,
    df = df, ...
  )

  # Adapt breaks
  leg <- leg_info$colours_dfs$viridis
  leg$group <- as.double(leg$group)

  # Make the plot
  leg |>
    ggplot2::ggplot(ggplot2::aes(
      xmin = group - 1, xmax = group, ymin = y - 1,
      ymax = y, fill = fill
    )) +
    ggplot2::geom_rect() +
    ggplot2::scale_x_continuous(
      breaks = 0:10,
      labels = leg_info$break_labs
    ) +
    ggplot2::scale_y_continuous(labels = NULL) +
    ggplot2::scale_fill_manual(values = stats::setNames(leg$fill, leg$fill)) +
    leg_info$labs_xy[[1]] +
    leg_info$theme_default
}

#' Render the legend for a `delta_bivar` class
#'
#' This function generates a plot with a bivariate color legend using the
#' ggplot2 package. It uses the \code{\link{legend_get_info}} function
#' to show the relationship between the variations of two variables. The legend
#' consists of a grid of colored squares with text labels indicating the meaning
#' of each square.
#'
#' @param vars <`named list`> A list object with a `delta_bivar` class. The
#' output of \code{\link{vars_build}}.
#' @param font_family <`character`> Which font family should be used to render
#' the legend (breaks, axis titles, ...). Defaults to `acidgrotesk-book`. To use
#' the default font family og ggplot2, use `NULL`.
#' @param lang <`character`> The language to use for the text labels. Defaults
#' to NULL for no translation.
#' @param scales_as_DA <`reactive character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#' @param df <`reactive character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param ... Additional arguments passed to other functions.
#'
#' @return A ggplot object that represents the `delta_bivar` legend.
#' @export
legend_render.delta_bivar <- function(vars, font_family = "acidgrotesk-book",
                                      scales_as_DA = c("building", "street"),
                                      df, lang = NULL, ...) {
  # NULL out problematic variables for the R CMD check (no visible binding for
  # global variable)
  x <- y <- fill <- label <- label_colour <- NULL

  # Get all necessary information
  leg_info <- legend_get_info(vars,
    lang = lang, font_family = font_family,
    scales_as_DA = scales_as_DA, df = df, ...
  )

  # Prepare the grid's labels location and the colours
  leg <- leg_info$colours_dfs$bivar[1:9, ]
  leg$label <- c(
    cc_t(lang = lang, "Both low"), " ",
    paste0(leg_info$labs_xy$y_short, "\n", cc_t(lang = lang, "high only")),
    " ", " ", " ",
    paste0(leg_info$labs_xy$x_short, "\n", cc_t(lang = lang, "high only")),
    " ",
    cc_t(lang = lang, "Both high")
  )
  leg$label_colour <- c(rep("black", 8), "white")
  leg$x <- leg$x - 0.5
  leg$y <- leg$y - 0.5

  # Make the plot
  ggplot2::ggplot(leg, ggplot2::aes(y, x, fill = fill)) +
    ggplot2::geom_raster() +
    ggplot2::geom_text(ggplot2::aes(y, x, label = label, colour = label_colour),
      inherit.aes = FALSE, size = 3
    ) +
    ggplot2::scale_x_continuous(breaks = 0:3, labels = leg_info$break_labs$x) +
    ggplot2::scale_y_continuous(breaks = 0:3, labels = leg_info$break_labs$y) +
    ggplot2::scale_fill_manual(values = stats::setNames(
      leg_info$colours_dfs$bivar$fill[1:9], leg_info$colours_dfs$bivar$fill[1:9]
    )) +
    ggplot2::scale_colour_manual(values = c("black" = "black", "white" = "white")) +
    leg_info$labs_xy[[1]] +
    leg_info$theme_default
}

#' Render the legend for a `bivar_ldelta_rq3` class
#'
#' This function generates a plot with a bivariate color legend using the
#' ggplot2 package. It uses the \code{\link{legend_get_info}} function
#' to show the relationship between the variations of the first variable with the
#' value of a static year of the second variable. The legend consists of a grid
#' of colored squares with text labels indicating the meaning of each square.
#'
#' @param vars <`named list`> A list object with a `bivar_ldelta_rq3` class. The
#' necessary objects in the list are `var_left` and `var_right`, with the first
#' of length 2 (two years) and the second of length 1 (one year).
#' 2 (two years each) The output of \code{\link{vars_build}}.
#' @param font_family <`character`> Which font family should be used to render
#' the legend (breaks, axis titles, ...). Defaults to `acidgrotesk-book`. To use
#' the default font family og ggplot2, use `NULL`.
#' @param lang <`character`> The language to use for the text labels. Defaults
#' to NULL for no translation.
#' @param scales_as_DA <`reactive character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#' @param df <`reactive character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param ... Additional arguments passed to other functions.
#'
#' @return A ggplot object that represents the `bivar_ldelta_rq3` legend.
#' @export
legend_render.bivar_ldelta_rq3 <- function(vars, font_family = "acidgrotesk-book",
                                           scales_as_DA = c("building", "street"),
                                           df, lang = NULL, ...) {
  # NULL out problematic variables for the R CMD check (no visible binding for
  # global variable)
  x <- y <- fill <- label <- label_colour <- NULL

  # Get all necessary information
  leg_info <- legend_get_info(vars,
    lang = lang, font_family = font_family,
    scales_as_DA = scales_as_DA, df = df, ...
  )

  # Prepare the grid's labels location and the colours
  leg <- leg_info$colours_dfs$bivar[1:9, ]
  leg$label <- c(
    cc_t(lang = lang, "Both low"), " ",
    paste0(leg_info$labs_xy$y_short, "\n", cc_t(lang = lang, "high only")),
    " ", " ", " ",
    paste0(leg_info$labs_xy$x_short, "\n", cc_t(lang = lang, "high only")),
    " ",
    cc_t(lang = lang, "Both high")
  )
  leg$label_colour <- c(rep("black", 8), "white")
  leg$x <- leg$x - 0.5
  leg$y <- leg$y - 0.5

  # Make the plot
  ggplot2::ggplot(leg, ggplot2::aes(y, x, fill = fill)) +
    ggplot2::geom_raster() +
    ggplot2::geom_text(ggplot2::aes(y, x, label = label, colour = label_colour),
      inherit.aes = FALSE, size = 3
    ) +
    ggplot2::scale_x_continuous(breaks = 0:3, labels = leg_info$break_labs$x) +
    ggplot2::scale_y_continuous(breaks = 0:3, labels = leg_info$break_labs$y) +
    ggplot2::scale_fill_manual(values = stats::setNames(
      leg_info$colours_dfs$bivar$fill[1:9], leg_info$colours_dfs$bivar$fill[1:9]
    )) +
    ggplot2::scale_colour_manual(values = c("black" = "black", "white" = "white")) +
    leg_info$labs_xy[[1]] +
    leg_info$theme_default
}

#' Default legend rendering method
#'
#' This is the default legend rendering method, which returns a \code{NULL}
#' value. It is intended to be used when no specific legend rendering method
#' is available or necessary. In this case, there will be no legend.
#'
#' @param vars <`named list`> A list object with an unknown class.
#' @param ... Additional arguments passed to other functions.
#'
#' @return \code{NULL}
#' @export
legend_render.default <- function(vars, ...) {
  return(NULL)
}
