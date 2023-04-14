#' Generate English or French ordinal form of a number
#'
#' This function returns the English or French ordinal form of a number. The
#' ordinal form can be selected by the lang argument, which takes either
#' "en" for English or "fr" for French.
#'
#' @param lang <`character`> String indicating the language for the ordinal form,
#' either "en" or "fr".
#' @param x <`numeric`> The number to be transformed into ordinal form.
#' @param en_first <`character`> The English word for first
#' (default is `"first"`). Useful for place explorer where `ranks 'best'` is more
#' suitable.
#'
#' @return A character string, the ordinal form of x in the selected language.
#'
#' @examples
#' ordinal_form("en", 1) # "first"
#' ordinal_form("fr", 2) # "deuxième"
#' ordinal_form("en", 23) # "23rd"
#' @export
ordinal_form <- function(lang, x, en_first = "first") {
  if (!lang %in% c("en", "fr")) {
    stop("Only languages supported are `en` and `fr`")
  }
  if (!is.numeric(x)) {
    stop("`x` must be numeric")
  }

  # French
  if (lang == "fr") {
    return(switch(as.character(x),
      "1" = "",
      "2" = "deuxième",
      "3" = "troisième",
      paste0(as.character(x), "ième")
    ))
  }

  # English
  if (x > 20) {
    if (x %% 100 %in% c(11, 12, 13)) {
      form <- "th "
    } else {
      form <- switch(as.character(x %% 10),
        "1" = "st",
        "2" = "nd",
        "3" = "rd",
        "th"
      )
    }
    paste0(x, form)
  } else {
    switch(as.character(x),
      "1" = en_first,
      "2" = "second",
      "3" = "third",
      "4" = "fourth",
      "5" = "fifth",
      "6" = "sixth",
      "7" = "seventh",
      "8" = "eighth",
      "9" = "ninth",
      "10" = "tenth",
      paste0(as.character(x), "th")
    )
  }
}

#' Compute the depth of a nested data structure
#'
#' This function returns the depth of a nested data structure. The depth of a
#' vector is the number of lists it is nested within. Atomic vectors have a
#' depth of 1, and NULL has a depth of 0. It is a reimplementation of
#' \code{\link[purrr]{vec_depth}} in base R.
#'
#' @param x A vector, a list or a NULL.
#'
#' @return An integer indicating the depth of the data structure.
#'
#' @examples
#' vec_dep(NULL) # 0
#' vec_dep(1:10) # 1
#' vec_dep(list(1, list(2, 3))) # 2
#'
#' @seealso The purrr package provides a vec_depth function with similar
#' functionality.
#'
#' @export
vec_dep <- function(x) {
  if (is.null(x)) {
    0L
  } else if (is.atomic(x)) {
    1L
  } else if (is.list(x)) {
    depths <- vapply(x, vec_dep, vector("integer", 1))
    1L + max(depths, 0L)
  } else {
    stop("`x` must be a vector")
  }
}

#' Calculate the ntile for a given vector of values
#'
#' This function divides a vector into n bins of equal size and returns the
#' bin number for each element in the vector. It is a reimplementation of
#' \code{\link[dplyr]{ntile}} in base R.
#'
#' @param x <`numeric vector`> Vector of numeric values
#' @param n <`integer`> An integer specifying the number of quantiles to
#' calculate
#'
#' @return An integer vector indicating the bin number for each element in x
#'
#' @seealso The dplyr package provides a ntile function with similar
#' functionality.
#'
#' @export
ntile <- function(x, n) {
  x <- rank(x, ties.method = "first", na.last = "keep")
  len <- length(x) - sum(is.na(x))
  if (len == 0L) {
    rep(NA_integer_, length(x))
  } else {
    n <- as.integer(floor(n))
    n_larger <- as.integer(len %% n)
    n_smaller <- as.integer(n - n_larger)
    size <- len / n
    larger_size <- as.integer(ceiling(size))
    smaller_size <- as.integer(floor(size))
    larger_threshold <- larger_size * n_larger
    bins <- ifelse(x <= larger_threshold,
      (x + (larger_size - 1L)) / larger_size,
      (x + (-larger_threshold + smaller_size - 1L)) /
        smaller_size + n_larger
    )
    as.integer(floor(bins))
  }
}

#' Test if x scale is under study
#'
#' @param scales <`character vector`> All scales to test if it is part of `df`.
#' @param df <`character`> The combination of the region under study
#' and the scale at which the user is on, e.g. `CMA_CSD`.
#' @param vectorized <`logical`> Should all elements of `scales` be evaluated
#' and return a logical vector the same length of `scales`?
#'
#' @return Returns TRUE or FALSE
#' @export
#'
#' @examples
#' is_scale_df(scales = c("CSD", "CT", "DA"), df = "CMA_DA") # TRUE
#' is_scale_df(scales = c("CSD", "CT"), df = "CMA_DA") # FALSE
is_scale_df <- function(scales, df, vectorized = FALSE) {
  if (!vectorized) {
    scls <- paste0(scales, "$", collapse = "|")
    return(grepl(scls, df))
  }

  sapply(scales, grepl, df, USE.NAMES = FALSE)
}

#' Extract substrings from a character vector that matches a regular
#' expression pattern
#'
#' It is a reimplementation of \code{\link[stringr]{str_extract}} in base R.
#'
#' @param x <`character`> A vector from which to extract the substring
#' @param pattern <`character`> A regular expression pattern to match
#'
#' @return The substrings from \code{x} that matches \code{pattern}, or
#' \code{NA} if no matches are found
#' @export
#'
#' @examples
#' s_extract("hello", c("hello world", "goodbye")) # hello"
#' s_extract("[0-9]+", c("123abc", "def456")) # "123", "456"
s_extract <- function(pattern, x) {
  sapply(regmatches(x, regexec(pattern, x, perl = TRUE)), `[`, 1)
}

#' Extract all occurrences of a regular expression pattern from a character vector
#'
#' It is a reimplementation of \code{\link[stringr]{str_extract_all}} in base R.
#'
#' @param pattern <`character`> A regular expression pattern to match
#' @param x <`character`> A vector from which to extract the substring
#'
#' @return A character vector containing all occurrences of the regular
#' expression pattern found in the input vector.
#'
#' @details The function uses the `gregexpr` function to locate all occurrences of
#' the specified regular expression pattern in the input character vector `x`.
#' The resulting match positions are then passed to the `regmatches` function to
#' extract the matching substrings.
#'
#' @export
s_extract_all <- function(pattern, x) {
  unlist(regmatches(x, gregexpr(pattern, x, perl = TRUE)))
}

#' Capitalize the first letter of a sentence
#'
#' This function takes a string as input and returns the same string with the
#' first etter capitalized, assuming it is the first letter of a sentence. It does this by
#' extracting the first letter, capitalizing it, extracting the rest of the string,
#' and then combining the two parts.
#'
#' @param x <`character`> A character string to capitalize
#'
#' @return A character string with the first letter capitalized
#' @export
#'
#' @examples
#' s_sentence("hello world") # "Hello world"
s_sentence <- function(x) {
  # Extract the first letter and capitalize it
  first_letter <- toupper(substring(x, 1, 1))

  # Extract the rest of the string and leave it as is
  rest_of_string <- substring(x, 2)

  # Combine the first letter and the rest of the string
  capitalized_string <- paste(first_letter, rest_of_string, sep = "")

  # Return the result
  return(capitalized_string)
}

#' Retrieve the `colours_dfs` object
#'
#' This function retrieves the `colours_dfs` object from the global environment.
#' The `colours_dfs` object contains pre-defined color schemes that can be used
#' in various plots across the package.
#'
#' @return The `colours` object from the global environment
colours_get <- function() {
  colours_dfs <- get0("colours_dfs", envir = .GlobalEnv)
  if (is.null(colours_dfs)) {
    stop(paste0(
      "Object `colours_dfs` must live in the global environment. Run ",
      "`cc.buildr::build_colours()` and save it in the data/ ",
      "folder."
    ))
  }
  return(colours_dfs)
}

#' Treat data frames as a DA (DA) scale
#'
#' This function takes a list of scales that should be treated like DAs.
#' If they should, the function appends "_DA" to the `df` string instead of the
#' current scale, and returns the new name. If not, the original `df` is returned.
#'
#' @param scales_as_DA <`character vector`> dfs to check if they should be
#' treated like a DA scale
#' @param df <`character`> The `df` to check if it is a DA scale
#'
#' @return If the current `df` is part of the scales that should be treated
#' as a DA, thye function appends "_DA" to the `df` string instead of the
#' current scale, and returns the new name. If not, the original `df` is returned.
#' @export
treat_to_DA <- function(scales_as_DA, df) {
  if (is_scale_df(scales_as_DA, df)) {
    return(paste0(s_extract(".*(?=_)", df), "_DA"))
  }
  return(df)
}

#' Get object from global environment
#'
#' This function retrieves an object from the global environment by the name of
#' the object. If the object is not found in the global environment, an error is
#' thrown.
#'
#' @param x <`character`> The name of the object to retrieve from the global
#' environment.
#'
#' @return The requested object from the global environment.
get_from_globalenv <- function(x) {
  out <- get0(x, envir = .GlobalEnv)
  if (is.null(out)) {
    stop(glue::glue("`{x}` object not found in the global environment."))
  }
  return(out)
}

#' Calculate the distance between two points on the Earth's surface using the
#' Haversine formula
#'
#' The function calculates the distance between two points on the Earth's
#' surface using the Haversine formula. If \code{x} is a matrix or data frame,
#' the function extracts the longitude and latitude vectors for each point.
#' The latitude values are converted from degrees to radians, and the function
#' calculates the central angle between the two points using the Haversine formula.
#' The distance between the points in meters is then calculated and returned.
#'
#' @param x A vector or matrix/data frame containing the longitude and latitude
#' of one or more points
#' @param y A vector containing the longitude and latitude of a single point
#'
#' @return The distance between the two points in meters
#' @export
get_dist <- function(x, y) {
  # For consistent indexing
  if (!is.null(dim(x))) x <- as.matrix(x)
  # If x is matrix or df, take lon/lat vectors
  lon_1 <- if (!is.null(dim(x))) x[, 1] else x[1]
  lat_1 <- if (!is.null(dim(x))) x[, 2] else x[2]
  lon_2 <- y[1]
  lat_1_r <- lat_1 * pi / 180
  lat_2 <- y[2]
  lat_2_r <- lat_2 * pi / 180
  delta_lat <- (lat_2 - lat_1) * pi / 180
  delta_lon <- (lon_2 - lon_1) * pi / 180
  a_dist <- sin(delta_lat / 2) * sin(delta_lat / 2) + cos(lat_1_r) *
    cos(lat_2_r) * sin(delta_lon / 2) * sin(delta_lon / 2)
  c_dist <- 2 * atan2(sqrt(a_dist), sqrt(1 - a_dist))
  6371e3 * c_dist
}

#' Generate a Mapbox Tile JSON
#'
#' Given a Mapbox username, tileset prefix, and tile name, this function
#' generates a Mapbox Tile JSON using \code{\link[rdeck]{tile_json}}. If the
#' specified tile is not found, a warning message is displayed and NULL is
#' returned. This prevents the app from crashing.
#'
#' @param mapbox_username <`character`> string representing the Mapbox username.
#' @param tileset_prefix <`character`> Prefix attached to every tileset. Should
#' correspond to the Curbcut city, e.g. `mtl`.
#' @param tile <`character`> The tile name to be fetched.
#'
#' @return A JSON list if succesfull. If missing tile, returns NULL preventing
#' the app from crashing. If the tile is missing and it's a _building tile,
#' grab the first region of the regions_dictionary and show buildings for those.
#' @export
tilejson <- function(mapbox_username, tileset_prefix, tile) {
  tile_link <- paste0(mapbox_username, ".", tileset_prefix, "_", tile)
  out <- tryCatch(
    suppressWarnings(rdeck::tile_json(tile_link)),
    error = function(e) {
      if (curbcut::is_scale_df("building", tile)) {
        regions_dictionary <- get_from_globalenv("regions_dictionary")
        base_building_tile <-
          sprintf("%s.%s_%s_building", mapbox_username, tileset_prefix,
                  regions_dictionary$region[1])
        rdeck::tile_json(base_building_tile)
      } else {
        print(e)
        NULL
      }
    }
  )
  return(out)
}

#' Verify widget ID validity
#'
#' This function checks the validity of a given widget ID by verifying if it
#' meets the following criteria:
#' \itemize{
#' \item The widget ID cannot be an empty string
#' \item The widget ID does not contain more than one underscore.
#' \item The widget ID is no longer than 3 characters.
#' \item The widget ID does not interfere with known codes and short codes.
#' }
#'
#' @param widget_id <`character`>  the widget ID to be verified.
#'
#' @return The original widget ID if it passes all verification criteria.
#' @export
widget_id_verif <- function(widget_id) {
  # Is it an empty string
  if (widget_id == "" || is.null(widget_id)) {
    stop("The widget ID can't be an empty string")
  }

  # Are there more than one underscore
  if (length(strsplit(widget_id, split = "_")[[1]]) > 1) {
    stop("No underscore can be used in a widget ID.")
  }

  # Widget ID must be as small as possible
  if (nchar(widget_id) > 3) {
    stop(paste0(
      "Widget ID can contain no more than 3 characters to reduce ",
      "bookmark URL size."
    ))
  }

  # Can the ID interfere with known codes and short code
  detect <- grepl(widget_id, c(curbcut::bookmark_codes, curbcut::bookmark_shorts))
  if (sum(detect) > 0) {
    stop(paste0(
      "Widget ID can not be the same as a value of ",
      "`curbcut::bookmark_codes` or `curbcut::bookmark_shorts` ",
      "to limit interference with bookmark codes."
    ))
  }

  return(widget_id)
}

#' Whether a value can be transformed to numeric
#'
#' Purpose mostly for the bookmark. To detect if "2" can be numeric.
#'
#' @param x <`character`> String to detect if a transformation to numeric
#' is possible.
#'
#' @return Logical indicating whether the value can be numeric or not.
is_numeric <- function(x) {
  # Convert the input value to a numeric type using as.numeric()
  # Check whether the conversion was successful using is.na()
  # If the conversion was successful, the value is numeric
  !is.na(suppressWarnings(as.numeric(x)))
}

#' Find and remove outliers in specified columns of a data frame.
#'
#' This function package consists of two functions: find_outliers() and
#' remove_outliers_df(). find_outliers() identifies outliers in a numeric vector
#' using the 1.5 * IQR rule. remove_outliers_df() removes outliers in specified columns
#' of a data frame, preserving the original row indices.
#'
#' @param x <`numeric`> A numeric vector for which to find outliers.
#'
#' @return For find_outliers(): A vector of indices of the outliers in x.
#' For remove_outliers_df(): A data frame with outliers removed from specified
#' columns.
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5, 100)
#' find_outliers(x)
#'
#' df <- data.frame(a = c(1, 2, 3, 4, 5, 100), b = c(1, 2, 3, 4, 5, 6))
#' remove_outliers_df(df, cols = "a")
#'
#' @export
find_outliers <- function(x) {
  q1 <- stats::quantile(x, 0.25, na.rm = TRUE)
  q3 <- stats::quantile(x, 0.75, na.rm = TRUE)
  iqr <- (q3 - q1) * 1.5
  which(x < q1 - iqr | x > q3 + iqr)
}

#' @rdname find_outliers
#' @param df <`data.frame`> A data frame from which to remove outliers.
#' @param cols <`character vector`> Column names in df for which to remove o
#' utliers.
#' @export
remove_outliers_df <- function(df, cols) {
  outliers <- lapply(cols, \(c) {
    nas <- which(is.na(df[[c]]))
    outliers <- find_outliers(df[[c]])
    remove <- unique(c(nas, outliers))
    return(remove)
  })

  remove <- Reduce(c, outliers)
  remove <- unique(remove)

  out <- if (length(remove) > 0) df[-remove, ] else df

  return(out)
}

