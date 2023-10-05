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
vars_build <- function(var_left, var_right = " ", scale, time,
                       scales_as_DA = c("building", "street"),
                       check_choropleth = TRUE,
                       variables = get_from_globalenv("variables")) {

  # Use picker_return_var() to add the `time` to var_left and var_right
  vl <- picker_return_var(var_left, time)
  vr <- picker_return_var(var_right, time)

  # Switch scales to DA if necessary
  scale <- treat_to_DA(scales_as_DA, scale)

  # Add var left and right measurement variable as classes
  var_left_m <- var_get_info(var_left, "var_measurement",
    variables = variables
  )[[1]]
  # var_left_m <- var_left_m$measurement[var_left_m$df == df]
  var_left_m <- var_left_m$measurement[var_left_m$scale == scale]
  class(vl) <- c(var_left_m, class(var_left))

  if (var_right != " ") {
    var_right_m <- var_get_info(var_right, "var_measurement",
      variables = variables
    )[[1]]
    var_right_m <- var_right_m$measurement[var_right_m$scale == scale]
    class(vr) <- c(var_right_m, class(var_right))
  } else {
    var_right_m <- " "
  }

  # Add var left and right types as classes
  class(vl) <- c(
    unlist(var_get_info(var_left, "type", variables = variables)),
    class(vl)
  )
  if (var_right != " ") {
    class(vr) <- c(
      unlist(var_get_info(var_right, "type", variables = variables)),
      class(vr)
    )
  }

  # Grab the class
  z <- (\(x) {
    # General cases
    if (is_scale_in("raster", scale)) {
      return("q100")
    }
    if (is_scale_in(c("heatmap", "point"), scale)) {
      return("point")
    }
    if (is_scale_in("qual", var_left)) {
      return("qual")
    }

    # If not part of the normal `choropleths` map
    if (check_choropleth) {
      choropleths <- get_from_globalenv("all_choropleths")

      if (!is_scale_in(choropleths, scale)) {
        return(scale)
      }
    }

    # bivariate
    if (all(vr != " ")) {
      # 2 time
      if (length(time) == 2) {
        # single possible right value
        if (length(vr$closest_year) == 1) {
          return("bivar_ldelta_rq3")
        }
        # propertly delta_bivar (bivar, 2 time, 2 possible valid time)
        return("delta_bivar")
      }

      # bivar, one time
      if ("ind" %in% class(vl)) {
        return(c("bivar_ind", "bivar"))
      }
      return("bivar")
    }

    # single variable, two time
    if (length(time) == 2) {
      if ("ind" %in% class(vl)) {
        return(c("delta_ind", "delta"))
      }
      return("delta")
    }

    # Single variable, 1 time
    if ("ind" %in% class(vl)) {
      return(c("q5_ind", "q5"))
    }
    return("q5")

  })()

  # Add the measurement variable as a class to the main output, in order.
  # The following is the priority levels:
  measurement_order <- c("nominal", "ordinal", "scalar")
  current_measurement_var <- c(var_left_m, var_right_m)
  meas <- which.min(factor(current_measurement_var, levels = measurement_order))

  out_class <- c(z, current_measurement_var[meas])

  # Output vars and time
  vars <- structure(list(var_left = var_left, var_right = var_right),
                    class = out_class
  )
  time <- list(var_left = vl$closest_year, var_right = vr$closest_year)

  # Return
  return(list(vars = vars, time = time))
}
