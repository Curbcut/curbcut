#' Helper function for `data_get_colours`
#'
#' This function retrieves the colors of all ID of a specified region by matching
#' the \code{group} column in the data with the colors in the correct \code{colours}
#' data frame. The resulting data frame contains only the \code{ID} and \code{fill}
#' columns, to use in an `rdeck` scale function.
#'
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link[curbcut]{vars_build}} function.
#' @param region <`character`> Region for which to get the `map_zoom_levels` and
#' retrieve all the according colours of all the IDs. One of the regions available
#' in the `regions_dictionary`.
#' @param colours_table <`character`> Fromn which colour table should the colour
#' be matched to the `group` column of the retrieved data. For `q5` class would be
#' `left_5`, for `bivar` class would be `bivar`, etc. One of the names of the
#' `colours_dfs` list present in the global environment.
#' @param scales_as_DA <`character vector`> A character vector of `scales` that
#' should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#' @param ... Additional arguments passed to methods.
#'
#' @return A data frame with the columns \code{ID} and \code{fill} to use in
#' an `rdeck` scale function.
data_get_colours_helper <- function(vars, region, colours_table,
                                    scales_as_DA = c("building", "street"),
                                    ...) {
  # Region and all possible `df`
  mzl <- get0(paste("map_zoom_levels", region, sep = "_"), envir = .GlobalEnv)
  if (is.null(mzl)) {
    stop(glue::glue("{paste('map_zoom_levels', region, sep = '_')} not found ",
                    " in the global environment."))
  }

  dfs <- names(mzl)[!names(mzl) %in% scales_as_DA]
  dfs <- paste(region, dfs, sep = "_")

  # Get all the data
  data <- sapply(dfs, \(x) data_get(vars, x), simplify = FALSE,
                 USE.NAMES = TRUE)
  data <- Reduce(rbind, data)

  # Deal with colours
  colours <- colours_get()
  if (!colours_table %in% names(colours))
    stop(glue::glue("{colours_table} is not a table part of the `colours_dfs` ",
                    "list."))
  colour_table <- colours[[colours_table]]
  mapping <- match(data$group, colour_table$group)
  data$fill <- colour_table$fill[mapping]
  data <- data[c("ID", "fill")]

  # Return
  return(data)
}

#' Retrieve colors for all IDs of a specified region
#'
#' This function retrieves the colors of all ID of a specified region by matching
#' the \code{group} column in the data with the colors in the correct \code{colours}
#' data frame. The resulting data frame contains only the \code{ID} and \code{fill}
#' columns, to use in an `rdeck` scale function.
#'
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link[curbcut]{vars_build}} function.
#' @param region <`character`> Region for which to get the `map_zoom_levels` and
#' retrieve all the according colours of all the IDs. One of the regions available
#' in the `regions_dictionary`.
#' @param colours_table <`character`> Fromn which colour table should the colour
#' be matched to the `group` column of the retrieved data. For `q5` class would be
#' `left_5`, for `bivar` class would be `bivar`, etc. One of the names of the
#' `colours_dfs` list present in the global environment.
#' @param scales_as_DA <`character vector`> A character vector of `scales` that
#' should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their colour will be the one of their DA.
#' @param ... Additional arguments passed to methods.
#'
#' @return A data frame with the columns \code{ID} and \code{fill} to use in
#' an `rdeck` scale function.
#' @export
data_get_colours <- function(vars, region, colours_table,
                             scales_as_DA = c("building", "street"), ...) {
  UseMethod("data_get_colours", vars)
}

#' @rdname data_get_colours
#' @export
#' @seealso \code{\link[curbcut]{data_get_colours}}
data_get_colours.q5 <- function(vars, region, colours_table,
                                scales_as_DA = c("building", "street"), ...) {
  data_get_colours_helper(vars = vars, region = region, colours_table = "left_5",
                          scales_as_DA = scales_as_DA)
}

#' @rdname data_get_colours
#' @export
#' @seealso \code{\link[curbcut]{data_get_colours}}
data_get_colours.bivar <- function(vars, region, colours_table,
                                   scales_as_DA = c("building", "street"), ...) {
  data_get_colours_helper(vars = vars, region = region, colours_table = "bivar",
                          scales_as_DA = scales_as_DA)
}

#' @rdname data_get_colours
#' @export
#' @seealso \code{\link[curbcut]{data_get_colours}}
data_get_colours.delta <- function(vars, region, colours_table,
                                   scales_as_DA = c("building", "street"), ...) {
  data_get_colours_helper(vars = vars, region = region, colours_table = "delta",
                          scales_as_DA = scales_as_DA)
}

#' @rdname data_get_colours
#' @export
#' @seealso \code{\link[curbcut]{data_get_colours}}
data_get_colours.delta_bivar <- function(vars, region, colours_table,
                                         scales_as_DA = c("building", "street"), ...) {
  data_get_colours_helper(vars = vars, region = region, colours_table = "bivar",
                          scales_as_DA = scales_as_DA)
}

#' @rdname data_get_colours
#' @export
#' @seealso \code{\link[curbcut]{data_get_colours}}
data_get_colours.bivar_ldelta_rq3 <- function(vars, region, colours_table,
                                              scales_as_DA = c("building", "street"), ...) {
  data_get_colours_helper(vars = vars, region = region, colours_table = "bivar",
                          scales_as_DA = scales_as_DA)
}

#' @rdname data_get_colours
#' @export
#' @seealso \code{\link[curbcut]{data_get_colours}}
data_get_colours.default <- function(vars, region, colours_table,
                                     scales_as_DA = c("building", "street"), ...) {
  data <- data.frame(ID = NA)
  data$fill <- "NA"
  return(data)
}
