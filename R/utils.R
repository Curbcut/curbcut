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
      "1" = "premier",
      "2" = "deuxi\u00e8me",
      "3" = "troisi\u00e8me",
      paste0(as.character(x), "i\u00e8me")
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
#' @param scales <`character vector`> All scales to test if `scale` is in them..
#' @param scale <`character`> Scale at which the user is on.
#' @param vectorized <`logical`> Should all elements of `scales` be evaluated
#' and return a logical vector the same length of `scales`?
#'
#' @return Returns TRUE or FALSE
#' @export
is_scale_in <- function(scales, scale, vectorized = FALSE) {
  if (!vectorized) {
    return(scale %in% scales)
  }

  scales == scale
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
#' @param scale <`character`> The `scale` to check if it is a DA scale
#'
#' @return If the current `df` is part of the scales that should be treated
#' as a DA, thye function appends "_DA" to the `df` string instead of the
#' current scale, and returns the new name. If not, the original `df` is returned.
#' @export
treat_to_DA <- function(scales_as_DA, scale) {
  if (length(scales_as_DA) == 0) {
    return(scale)
  }
  if (is_scale_in(scales_as_DA, scale)) {
    return("DA")
  }
  return(scale)
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
#' @export
get_from_globalenv <- function(x) {
  out <- get0(x, envir = .GlobalEnv)
  if (is.null(out)) {
    stop(glue::glue_safe("`{x}` object not found in the global environment."))
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
#' @param return_error <`logical`> Print the error if the tileset isn't found.
#'
#' @return A JSON list if succesfull. If missing tile, returns NULL preventing
#' the app from crashing. If the tile is missing and it's a _building tile,
#' grab the first region of the regions_dictionary and show buildings for those.
#'
#' @export
tilejson <- function(mapbox_username, tileset_prefix, tile, return_error = FALSE) {
  # urltools is necessary for tile_json use
  requireNamespace("urltools", quietly = TRUE)
  tile_link <- paste0(mapbox_username, ".", tileset_prefix, "_", tile)
  out <- tryCatch(
    suppressWarnings(rdeck::tile_json(tile_link)),
    error = function(e) {
      if (curbcut::is_scale_in("building", tile)) {
        regions_dictionary <- get_from_globalenv("regions_dictionary")
        base_building_tile <-
          sprintf(
            "%s.%s_%s_building", mapbox_username, tileset_prefix,
            regions_dictionary$region[1]
          )
        rdeck::tile_json(base_building_tile)
      } else {
        if (return_error) print(e)
        return(NULL)
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
  q1 <- stats::quantile(x, 0.02, na.rm = TRUE)
  q3 <- stats::quantile(x, 0.98, na.rm = TRUE)
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

#' Grab DA_ID from a building-street-like dataframe (large!)
#'
#' This function fetches the DA_ID matching with the selected ID from a given
#' dataframe `scale` based on a provided `select_id`. It can also fetch the DA_ID
#' from a connected database when the `df` is not found in the global environment
#' but is identified as a scales_as_DA'. In such cases, it uses the established
#' connection and fetches the DA_ID via a SQL query.
#'
#' @param scale <`character`> A string, the name of the dataframe in which to look
#' for row. The dataframe should be in the global environment, and if it isn't,
#' there must be an established sqlite connection to it.
#' @param select_id <`character`> A value representing the ID that needs to be
#' selected in order to fetch the corresponding DA_ID.
#'
#' @return The DA_ID corresponding to the given `select_id`. If
#' `select_id` is found in the `df` in the global environment, the corresponding
#' DA_ID is returned. If `df` is a 'scales_as_DA' and not in the global
#' environment, the function fetches DA_ID from the connected database.
grab_DA_ID_from_bslike <- function(scale, select_id) {
  dat <- get0(scale, envir = .GlobalEnv)
  # If it's a 'scales_as_DA', and the `df` is not in the global environment,
  # search for a connection.
  # NDS: Rework this with new building sql db.
  if (is.null(dat)) {
    db_df <- sprintf("%s_conn", scale)
    call <- sprintf("SELECT DA_ID FROM %s WHERE ID = '%s'", df, select_id)
    out <- do.call(DBI::dbGetQuery, list(as.name(db_df), call))
    out <- unname(unlist(out))
  } else {
    out <- dat$DA_ID[dat$ID == select_id]
  }

  return(out)
}

#' Grab row from a building-street-like dataframe (large!)
#'
#' This function fetches the row matching with the selected ID from a given
#' dataframe `scale` based on a provided `select_id`. It can also fetch the row
#' from a connected database when the `df` is not found in the global environment
#' but is identified as a scales_as_DA'. In such cases, it uses the established
#' connection and fetches the row via a SQL query.
#'
#' @param scale <`character`> A string, the name of the dataframe in which to look
#' for row. The dataframe should be in the global environment, and if it isn't,
#' there must be an established sqlite connection to it.
#' @param select_id <`character`> A value representing the ID that needs to be
#' selected in order to fetch the corresponding row.
#' @param cols <`character vector`> Which columns to retrieve. Defaults to `'*'` for
#' all columns.
#'
#' @return The row corresponding to the given `select_id`. If
#' `select_id` is found in the `df` in the global environment, the corresponding
#' row is returned. If `df` is a 'scales_as_DA' and not in the global
#' environment, the function fetches row from the connected database.
grab_row_from_bslike <- function(scale, select_id, cols = "*") {
  dat <- get0(scale, envir = .GlobalEnv)
  # If it's a 'scales_as_DA', and the `df` is not in the global environment,
  # search for a connection.
  if (is.null(dat)) {
    # NDS: this needs to work for buildings. single table in the DB?
    scale <- gsub(".*_", "", df)
    db_df <- sprintf("%s_conn", scale)
    cols <- paste0(cols, collapse = ", ")
    call <- sprintf("SELECT %s FROM %s WHERE ID = '%s'", cols, df, select_id)
    out <- do.call(DBI::dbGetQuery, list(as.name(db_df), call))
  } else {
    out <- dat[dat$ID == select_id, ]
  }

  return(out)
}

#' Grab full dataframe from a building-street-like dataframe (large!)
#'
#' This function fetches the full dataframe matching with `df`. It can also fetch
#' the full dataframe from a connected database when the `df` is not found in the
#' global environment.
#'
#' @param df <`character`>  A string, the name of the dataframe in which to look
#' for full dataframe. The dataframe should be in the global environment, and if it isn't,
#' there must be an established sqlite connection to it.
#'
#' @return The full datraframe corresponding to the given `df`. If
#' `select_id` is found in the `df` in the global environment, the corresponding
#' table is returned. If `df` is not in the global
#' environment, the function fetches the dataframe from the connected database.
grab_df_from_bslike <- function(df) {
  dat <- get0(df, envir = .GlobalEnv)
  if (!is.null(dat)) {
    return(dat)
  }

  # If not in the global environment
  scale <- gsub(".*_", "", df)
  db_df <- sprintf("%s_conn", scale)
  call <- sprintf("SELECT * FROM %s", df)
  out <- do.call(DBI::dbGetQuery, list(as.name(db_df), call))

  return(out)
}

#' Retrieve a specific page
#'
#' This function retrieves a specific page based on the provided `ns_id`.
#' It substracts the namespace doubling noise and retrieves the page from
#' the modules stored in the global environment.
#'
#' @param ns_id <`character`> ID of the page identifier including the namespace.
#'
#' @return A subset of the `modules` global data frame matching the `ns_id`.
#' @export
page_get <- function(ns_id) {
  # Grab the modules table
  modules <- get_from_globalenv("modules")

  # Grab only the `id` (take out namespace doubling noise)
  solo_id <- gsub("-.*$", "", ns_id)

  # Subset the current page
  page <- modules[modules$id == solo_id, ]

  # Return
  return(page)
}

#' Convert a HEX6 color code to an RGB color string
#'
#' This function takes a HEX6 color code and converts it to an RGB string. The
#' HEX6 code must include the hash symbol (#) at the beginning.
#'
#' @param hex6 <`character`> A string representing the HEX6 color code (e.g.
#' "#FF5733").
#'
#' @return A string representing the RGB color (e.g. "rgb(255, 87, 51)").
#' @export
hex6_to_rgb <- function(hex6) {
  # Extract the RGB components
  r <- strtoi(substr(hex6, 2, 3), 16L)
  g <- strtoi(substr(hex6, 4, 5), 16L)
  b <- strtoi(substr(hex6, 6, 7), 16L)

  # Return as rgb string
  return(paste0("rgb(", r, ", ", g, ", ", b, ")"))
}

#' Convert a HEX8 color code to an RGBA color string
#'
#' This function takes a HEX8 color code (including alpha channel) and converts
#' it to an RGBA string. The HEX8 code must include the hash symbol (#) at the
#' beginning.
#'
#' @param hex8 <`character`> A string representing the HEX8 color code (e.g.
#' "#FF5733FF").
#'
#' @return A string representing the RGBA color (e.g. "rgba(255, 87, 51, 1)").
#' @export
hex8_to_rgba <- function(hex8) {
  # Extract the RGB and alpha components
  r <- strtoi(substr(hex8, 2, 3), 16L)
  g <- strtoi(substr(hex8, 4, 5), 16L)
  b <- strtoi(substr(hex8, 6, 7), 16L)
  a <- strtoi(substr(hex8, 8, 9), 16L)

  # Convert alpha from 0-255 to 0-1 scale
  a <- round(a / 255, 2)

  # Return as rgba string
  return(paste0("rgba(", r, ", ", g, ", ", b, ", ", a, ")"))
}

#' Convert a HEX color code to an RGB or RGBA color string
#'
#' This function takes a HEX color code and converts it to either an RGB or
#' RGBA string based on its length. HEX6 codes are converted to RGB, while
#' HEX8 codes are converted to RGBA.
#'
#' @param hex <`character`> A string representing the HEX color code. It can be
#' either HEX6 (e.g. "#FF5733") or HEX8 (e.g. "#FF5733FF").
#'
#' @return A string representing the RGB or RGBA color.
#' @export
hex_to_rgb_or_rgba <- function(hex) {
  # Determine if the input is HEX6 or HEX8
  len <- nchar(hex)

  if (len == 7) { # HEX6
    return(hex6_to_rgb(hex))
  } else if (len == 9) { # HEX8
    return(hex8_to_rgba(hex))
  } else {
    stop("Invalid HEX code length. Must be HEX6 or HEX8.")
  }
}
