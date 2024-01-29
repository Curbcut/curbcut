#' Helper function for `data_get_colours`
#'
#' This function retrieves the colors of all ID of a specified region by matching
#' the \code{group} column in the data with the colors in the correct \code{colours}
#' data frame. The resulting data frame contains only the \code{ID} and \code{fill}
#' columns, to use in an `cc.map::map_choropleth_fill_fun`-like  function.
#'
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link{vars_build}} function.
#' @param region <`character`> Region for which to get the `map_zoom_levels` and
#' retrieve all the according colours of all the IDs. One of the regions available
#' in the `regions_dictionary`. Usually one of the output of \code{\link{geography_server}}.
#' @param time <`numeric`> The `time` at which data is displayed. A subset of the list
#' `time`. Only the numeric vector.
#' @param schemas <`named list`> Current schema information. The additional widget
#' values that have an impact on which data column to pick. Usually `r[[id]]$schema()`.
#' @param zoom_levels <`named numeric vector`> A named numeric vector of zoom
#' levels. Usually one of the `mzl_*`, or the output of
#' \code{\link{geography_server}}.
#' @param colours_table <`character`> Fromn which colour table should the colour
#' be matched to the `group` column of the retrieved data. For `q5` class would be
#' `left_5`, for `bivar` class would be `bivar`, etc. One of the names of the
#' `colours_dfs` list present in the global environment.
#' @param scales_as_DA <`character vector`> A character vector of `scales` that
#' should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#' @param data_path <`character`> A string representing the path to the
#' directory containing the QS files. Default is "data/".
#' @param ... Additional arguments passed to methods.
#'
#' @return A data frame with the columns \code{ID} and \code{fill} to use in
#' an `cc.map::map_choropleth_fill_fun`-like scale function.
data_get_colours_helper <- function(vars, region, time, zoom_levels, colours_table,
                                    schemas = NULL,
                                    scales_as_DA = c("building", "street"),
                                    data_path = get_data_path(), ...) {
  # Grab colours
  colours <- colours_get()
  if (!colours_table %in% names(colours)) {
    stop(glue::glue_safe(
      "{colours_table} is not a table part of the `colours_dfs` ",
      "list."
    ))
  }

  # Region and all possible `df`
  dfs <- names(zoom_levels)[!names(zoom_levels) %in% scales_as_DA]
  # Get all the data
  data_r <- sapply(dfs, \(x) tryCatch(
    data_get(vars,
             scale = x, time = time, region = region,
             data_path = data_path
    ), error = function(e) tibble::tibble()),
    simplify = FALSE,
    USE.NAMES = TRUE
  )
  data <- Reduce(rbind, data_r)

  # If it's delta, maybe change the colour table to use only negatives/positives!
  colour_table <- if (colours_table == "delta") {
    delta_which_colors(data)
  } else {
    colours[[colours_table]]
  }

  # Is group already calculated?
  group <- match_schema_to_z_col(
    data = data, time = time, col = "group", vl_vr = "var_left",
    schemas = schemas
  )

  if ("group" %in% names(data)) {
    group <- "group"
  } else if (length(group) == 0 || !group %in% names(data)) {
    group <- sprintf("group_%s", time$var_left)

    if (!group %in% names(data)) {
      # Grab the correct column from which to use colors on
      var <- match_schema_to_col(
        data = data, time = time, col = "var_left",
        schemas = schemas
      )
      group <- sprintf("%s_q5", var)
    }
  }

  # Deal with colours
  mapping <- match(data[[group]], colour_table$group)
  data$fill <- colour_table$fill[mapping]
  data <- data[c("ID", "fill")]
  names(data)[1] <- c("ID_color")

  # Switch NA to the right "NA" colour
  data$fill[is.na(data$fill)] <- colour_table$fill[grepl("^NA", colour_table$group)][[1]]

  # Return
  return(data)
}

#' Retrieve colors for all IDs of a specified region
#'
#' This function retrieves the colors of all ID of a specified region by matching
#' the \code{group} column in the data with the colors in the correct \code{colours}
#' data frame. The resulting data frame contains only the \code{ID} and \code{fill}
#' columns, to use in an `cc.map::map_choropleth_fill_fun`-like scale function.
#'
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link{vars_build}} function.
#' @param region <`character`> Region for which to get the `map_zoom_levels` and
#' retrieve all the according colours of all the IDs. One of the regions available
#' in the `regions_dictionary`. Usually one of the output of \code{\link{geography_server}}.
#' @param time <`numeric named list`> The `time` at which data is displayed.
#' A list for var_left and var_right. The output of \code{\link{vars_build}}(...)$time.
#' @param zoom_levels <`named numeric vector`> A named numeric vector of zoom
#' levels. Usually one of the `mzl_*`, or the output of
#' \code{\link{geography_server}}. It needs to be `numeric` as the function
#' will sort them to make sure the lower zoom level is first, and the highest
#' is last (so it makes sense on an auto-scale).
#' @param scales_as_DA <`character vector`> A character vector of `scales` that
#' should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#' @param schemas <`named list`> Current schema information. The additional widget
#' values that have an impact on which data column to pick. Usually `r[[id]]$schema()`.
#' @param data_path <`character`> A string representing the path to the
#' directory containing the QS files. Default is "data/".
#' @param ... Additional arguments passed to methods.
#'
#' @return A data frame with the columns \code{ID} and \code{fill} to use in
#' an `cc.map::map_choropleth_fill_fun`-like scale function.
#' @export
data_get_colours <- function(vars, region, time, zoom_levels,
                             scales_as_DA = c("building", "street"),
                             schemas, data_path = get_data_path(), ...) {
  UseMethod("data_get_colours", vars)
}

#' @describeIn data_get_colours The method for q5.
#' @export
#' @seealso \code{\link{data_get_colours}}
data_get_colours.q5 <- function(vars, region, time, zoom_levels,
                                scales_as_DA = c("building", "street"),
                                schemas, data_path = get_data_path(), ...) {
  data_get_colours_helper(
    vars = vars, region = region, time = time, schemas = schemas,
    zoom_levels = zoom_levels, colours_table = "left_5",
    scales_as_DA = scales_as_DA, data_path = data_path
  )
}

#' @describeIn data_get_colours The method for bivar.
#' @export
#' @seealso \code{\link{data_get_colours}}
data_get_colours.bivar <- function(vars, region, time, zoom_levels,
                                   scales_as_DA = c("building", "street"),
                                   schemas, data_path = get_data_path(), ...) {
  data_get_colours_helper(
    vars = vars, region = region, time = time, schemas = schemas,
    zoom_levels = zoom_levels, colours_table = "bivar",
    scales_as_DA = scales_as_DA, data_path = data_path
  )
}

#' @describeIn data_get_colours The method for delta.
#' @export
#' @seealso \code{\link{data_get_colours}}
data_get_colours.delta <- function(vars, region, time, zoom_levels,
                                   scales_as_DA = c("building", "street"),
                                   schemas, data_path = get_data_path(), ...) {
  data_get_colours_helper(
    vars = vars, region = region, time = time, schemas = schemas,
    zoom_levels = zoom_levels, colours_table = "delta",
    scales_as_DA = scales_as_DA, data_path = data_path
  )
}

#' @describeIn data_get_colours The method for delta_bivar.
#' @export
#' @seealso \code{\link{data_get_colours}}
data_get_colours.delta_bivar <- function(vars, region, time, zoom_levels,
                                         scales_as_DA = c("building", "street"),
                                         schemas, data_path = get_data_path(), ...) {
  data_get_colours_helper(
    vars = vars, region = region, time = time, schemas = schemas,
    zoom_levels = zoom_levels, colours_table = "bivar",
    scales_as_DA = scales_as_DA, data_path = data_path
  )
}

#' @describeIn data_get_colours The method for bivar_ldelta_rq3.
#' @export
#' @seealso \code{\link{data_get_colours}}
data_get_colours.bivar_ldelta_rq3 <- function(vars, region, time, zoom_levels,
                                              scales_as_DA = c("building", "street"),
                                              schemas, data_path = get_data_path(), ...) {
  data_get_colours_helper(
    vars = vars, region = region, time = time, schemas = schemas,
    zoom_levels = zoom_levels, colours_table = "bivar",
    scales_as_DA = scales_as_DA, data_path = data_path
  )
}

#' @describeIn data_get_colours The default method.
#' @export
#' @seealso \code{\link{data_get_colours}}
data_get_colours.default <- function(vars, region, time, zoom_levels,
                                     scales_as_DA = c("building", "street"),
                                     schemas, data_path = get_data_path(), ...) {
  data <- data.frame(ID = "NA")
  data$fill <- "#B3B3BB"
  return(data)
}
