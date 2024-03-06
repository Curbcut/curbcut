#' Retrieves data from a PostgreSQL database based on dynamic conditions
#'
#' This function dynamically retrieves data from a PostreSQL database based on
#' user-defined variables, a grid mode, comparison mode, zoom level, the highest
#' granularity level, a selected ID, and a time range. It is written for the``
#' grid pages in CUrbcut.
#'
#' @param vars <`list`> A list of variables used for data retrieval, including
#' the left variable for the time comparison.
#' @param grid <`logical`> A boolean indicating if the grid mode is active.
#' @param grid_compare <`logical`> A boolean indicating if the grid comparison
#' mode is active.
#' @param rv_zoom_string <`character`> A string representing the current zoom
#' level.
#' @param highest_grd <`character`> A string representing the highest granularity
#' of grid cells available.
#' @param select_id <`character`> The ID of the selected element for which data
#' is being retrieved.
#' @param time <`list`> A list containing the left time variable for data
#' retrieval.
#' @return <`mixed`> Returns the retrieved value(s) from the database if any of
#' the conditions are met, otherwise returns NULL.
#' @export
val_get_db <- function(vars, grid, grid_compare, rv_zoom_string, highest_grd,
                       select_id, time) {

  # Return nothing if we're not in grid mode
  if (!grid) return(NULL)
  if (grid_compare) return(NULL)
  if (rv_zoom_string == highest_grd) return(NULL)
  if (is.na(select_id)) return(NULL)

  # Build the call to get the value
  val_get_db_helper(vars, select_id, time = time, scale = rv_zoom_string)

}

#' A helper function to dispatch the DB data retrieval process
#'
#' This function acts as a dispatcher for the 'val_get_db' function, allowing
#' for method overloading based on the class of the 'vars' object.
#'
#' @param vars <`list`> A list of variables used for data retrieval, including
#' the left variable for the time comparison.
#' @param select_id <`character`> The ID of the selected element for which data
#' is being retrieved.
#' @param time <`list`> A list containing the left time variable for data
#' retrieval.
#' @param scale <`character`> The scale for which data should be extracted
#'
#' @return <`mixed`> Returns the data retrieved from the database.
val_get_db_helper <- function(vars, select_id, time, scale) {
  UseMethod("val_get_db_helper")
}

#' @describeIn val_get_db_helper The method for q5.
#' @export
val_get_db_helper.q5 <- function(vars, select_id, time, scale) {
  # Specific logic for class 'q5'
  var <- vars$var_left
  var_date <- sprintf("%s_%s", var, time$var_left)
  tn <- sprintf("%s_%s", scale, var)

  # Execute the query
  out <- db_get(select = var_date, from = tn, where = list(ID = select_id))[[1]]

  if (length(out) == 0) NULL else out
}

#' @describeIn val_get_db_helper The method for delta.
#' @export
val_get_db_helper.delta <- function(vars, select_id, time, scale) {
  # Specific logic for class 'delta'
  var <- vars$var_left
  var_dates <- sprintf("%s_%s", var, time$var_left)
  tn <- sprintf("%s_%s", scale, var)

  out <- sapply(var_dates, \(x) {
    db_get(select = x, from = tn, where = list(ID = select_id))[[1]]
  }, simplify = TRUE, USE.NAMES = FALSE)

  if (length(out[[1]]) == 0) NULL else out
}
