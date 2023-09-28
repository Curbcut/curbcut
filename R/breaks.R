#' Calculate Breaks for Delta Legends
#'
#' This function calculates the break points for delta legends based on the data.
#' It also handles special cases, such as when all data is within a small range.
#'
#' @param vars <`named list`> A list object with a pre-determined class. The
#' output of \code{\link{vars_build}}.
#' @param df <`reactive character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`. The output of
#' \code{\link{update_df}}.
#' @param character <`Logical`>, if `TRUE`, the breaks are returned as characters.
#' @param data <`data.frame`> Optional, a data frame to use instead of recovering it
#' from the file.
#'
#' @return A vector of break points. Numeric if `character = FALSE`, otherwise
#' character.
#' @export
breaks_delta <- function(vars, df, character = FALSE, data = NULL) {

  if (is.null(data)) {
    data <- data_get(vars = vars, df = df)
  }

  # Data as absolute
  data_vl <- abs(data$var_left)

  # Remove outliers
  which_out <- find_outliers(x = data_vl)
  if (length(which_out) > 0) data_vl <- data_vl[-which_out]

  # Use the 50th and 100th percentile for the last bracket
  breaks <- stats::quantile(data_vl, na.rm = TRUE)[c(3, 5)]
  breaks <- c(0.02, breaks)

  # If the last break is lower than 2% (which is the forced middle bracket),
  # override the last bracket to 10%
  if (breaks[2] <= 0.03) {
    breaks[2] <- 0.1
    breaks[3] <- 0.15
  }

  # Convert to character
  if (character) breaks <- convert_unit.pct(x = breaks, decimal = 0)

  # Construct the output
  out <- c(sprintf("-%s", breaks[3]),
           sprintf("-%s", breaks[2]),
           sprintf("-%s", breaks[1]),
           breaks[1],
           breaks[2],
           breaks[3])
  out <- unname(out)

  if (!character) return(as.numeric(out))
  out
}
