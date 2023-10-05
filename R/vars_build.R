#' Determine the data type (class) of different `var_left`, `var_right` and `df`
#' combinations
#'
#' @param var_left <`reactive character`> Character string of the selected
#' variable, e.g. `alp_2016` or `c("housing_tenant_2006", "housing_tenant_2016")`.
#' @param var_right <`reactive character`> Character string of the selected
#' compared variable, e.g. `housing_value_2016`. Defaults to what no compared
#' variable is represented by (" ").
#' @param df <`character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`.
#' @param scales_as_DA <`character vector`> A character vector of `scales` that
#' should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#' @param check_choropleth <`logical`> Check if the scale is one of the main
#' choropleth scales on the app, by grabbind the `all_choropleth` object. If
#' the `df` is not in `all_choropleth`, it is returned as a class (instead of `q5`,
#' `bivar`, ...). Defaults to TRUE.
#' @param variables <`data.frame`> The `variables` df. Defaults to grabbing it
#' from the global environment using \code{\link{get_from_globalenv}}.
#'
#' @return A named list containing both `var_left` and `var_right` variables with
#' a class attached.
#'
#' @export
vars_build <- function(var_left, var_right = " ", df,
                       scales_as_DA = c("building", "street"),
                       check_choropleth = TRUE,
                       variables = get_from_globalenv("variables")) {

  #' NDS: The function needs to start accepting missing inputs for all arguments
  #' and returning NULL when a valid set of variables can't be constructed. It
  #' also needs to get a `time` argument which it uses to decide on `delta`
  #' status and to handle cases where data isn't available for the same year for
  #' var_left and var_right. Finally, the `df` argument should be removed and
  #' replaced with `region` and `scale` arguments. (Verify if we need both, or
  #' maybe just `scale`.)

  # If the `var` displays twice the same year
  var_left <- unique(var_left)
  var_right <- unique(var_right)

  # Switch scales to DA if necessary
  df <- treat_to_DA(scales_as_DA, df)

  # Add var left and right measurement variable as classes
  var_left_m <- var_get_info(var_left[[1]], "var_measurement",
    variables = variables
  )[[1]]
  var_left_m <- var_left_m$measurement[var_left_m$df == df]
  class(var_left) <- c(var_left_m, class(var_left))
  if (var_right[[1]] != " ") {
    var_right_m <- var_get_info(var_right[[1]], "var_measurement",
      variables = variables
    )[[1]]
    var_right_m <- var_right_m$measurement[var_right_m$df == df]
    class(var_right) <- c(var_right_m, class(var_right))
  } else {
    var_right_m <- " "
  }

  # Add var left and right types as classes
  class(var_left) <- c(
    unlist(var_get_info(var_left[[1]], "type", variables = variables)),
    class(var_left)
  )
  if (var_right[[1]] != " ") {
    class(var_right) <- c(
      unlist(var_get_info(var_right[[1]], "type", variables = variables)),
      class(var_right)
    )
  }

  # Grab the class
  z <- (\(x) {
    # General cases
    if (is_scale_df("raster", df)) {
      return("q100")
    }
    if (is_scale_df(c("heatmap", "point"), df)) {
      return("point")
    }
    if (is_scale_df("qual", var_left[1])) {
      return("qual")
    }

    # If not part of the normal `choropleths` map
    if (check_choropleth) {
      choropleths <- get_from_globalenv("all_choropleths")

      if (!is_scale_df(choropleths, df)) {
        return(df)
      }
    }

    # Impossible cases
    if (length(var_left) == 2 && var_left[1] == var_left[2]) {
      return("NA")
    }

    # Normal choropleth possible classes
    if (length(var_right) == 2 && var_right[1] == var_right[2]) {
      return("bivar_ldelta_rq3")
    }
    if (length(var_left) == 2 && length(unique(var_right)) == 1 &&
      var_right[1] != " ") {
      return("bivar_ldelta_rq3")
    }
    if (length(var_left) == 1 && var_right[1] == " ") {
      if ("ind" %in% class(var_left)) {
        return(c("q5_ind", "q5"))
      }
      return("q5")
    }
    if (length(var_left) == 1 && length(var_right) == 1 && var_right != " ") {
      if ("ind" %in% class(var_left)) {
        return(c("bivar_ind", "bivar"))
      }
      return("bivar")
    }
    if (length(var_left) == 2 && length(var_right) == 2) {
      return("delta_bivar")
    }
    if (length(var_left) == 2 && var_right[1] == " ") {
      if ("ind" %in% class(var_left)) {
        return(c("delta_ind", "delta"))
      }
      return("delta")
    }

    # Return `df` if nothing found
    return(df)
  })()

  # Add the measurement variable as a class to the main output, in order.
  # The following is the priority levels:
  measurement_order <- c("nominal", "ordinal", "scalar")
  current_measurement_var <- c(var_left_m, var_right_m)
  meas <- which.min(factor(current_measurement_var, levels = measurement_order))

  out_class <- c(z, current_measurement_var[meas])

  # Return
  return(structure(list(var_left = var_left, var_right = var_right),
    class = out_class
  ))
}
