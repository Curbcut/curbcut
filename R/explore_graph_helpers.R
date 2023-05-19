#' Explore Graph Info Function
#'
#' This function returns shared information between the graphs, such as the
#' default theme and colors.
#'
#' @param vars <`named list`> A list object of variable codes with classes. The
#' output of \code{\link{vars_build}}.
#' @param font_family <`character`> A string specifying the font family for the
#' plot, default is "SourceSansPro".
#' @param lang <`character`> A character string indicating the language to
#' translate variable titles to.
#' @param df <`character`> The combination of the region under study and the
#' scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param scales_as_DA <`character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their graph will be the one of their DA.
#' @param select_id <`character`> A string indicating the ID of the currently
#' selected region (if any). Usually `r[[id]]$select_id()`
#' @param data <`data.frame`> A data frame containing the variables and
#' observations to be compared. The output of \code{\link{data_get}}.
#' @param ... Additional arguments passed to the specific method.
#'
#' @return A list containing the default theme and color data frames.
explore_graph_info <- function(vars, font_family = "SourceSansPro", lang = NULL,
                               df, scales_as_DA, select_id, data, ...) {

  # Create the theme
  theme_default <- list(
    ggplot2::theme_minimal(),
    ggplot2::theme(text = ggplot2::element_text(family = font_family, size = 12),
                   legend.position = "none",
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank()))

  # Use the legend labels
  labs <- explore_graph_labels(vars = vars, lang = lang)

  # Grab the colours
  colours_dfs <- colours_get()

  # In the case where the selected ID must be updated
  if (curbcut::is_scale_df(scales_as_DA, df) & !is.na(select_id)) {
   select_id <- grab_DA_ID_from_bslike(df = df, select_id = select_id)
  }

  # df treatment if it's in the scales as DA
  treated_df <- treat_to_DA(scales_as_DA = scales_as_DA, df = df)

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
    treated_df = treated_df
  ))
}
