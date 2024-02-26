#' Reverse geocode a location using `https://photon.komoot.io/`
#'
#' @param lon <`numeric`> Represents the longitude of the location
#' @param lat <`numeric`> Represents the latitude of the location
#' @param timeout <`numeric`> After how many seconds should the function
#' stop trying to access the `photon` server. Defaults to 2 seconds.
#'
#' @return A character vector of the reverse geocoded location (address)
#' @export
#'
#' @examples
#' rev_geocode(12.4924, 41.8902) # "Piazza del Colosseo, Roma"
rev_geocode <- function(lon, lat, timeout = 2) {
  link <- paste0(
    "photon.komoot.io/reverse?lon=", lon, "&lat=",
    lat
  )
  out <- tryCatch(httr::content(httr::GET(link, httr::timeout(timeout))),
    error = function(e) NULL
  )
  if (is.null(out$features) || length(out$features) == 0) {
    return(NA_character_)
  }
  out <- out$features[[1]]$properties
  third <- (function(out) {
    if (!is.null(out$city)) {
      return(gsub(" \\(\\d{2}\\)$", "", out$city))
    }
    if (!is.null(out$locality)) {
      return(out$locality)
    }
    if (!is.null(out$district)) {
      return(out$district)
    }
    if (!is.null(out$town)) {
      return(out$town)
    }
    if (!is.null(out$village)) {
      return(out$village)
    }
    if (!is.null(out$suburb)) {
      return(out$suburb)
    }
    if (!is.null(out$region)) {
      return(out$region)
    }
    if (!is.null(out$county)) {
      return(out$county)
    }
  })(out)
  second <- (function(out) {
    if (is.null(out$street)) {
      return(third)
    }
    return(paste(out$street, third, sep = ", "))
  })(out)
  name <- (function(out) {
    if (is.null(out$housenumber)) {
      return(second)
    }
    return(paste(out$housenumber, second, sep = " "))
  })(out)
  if (is.null(name)) {
    return(NA_character_)
  }
  return(name)
}

#' Geocode an address using the Geogratis API
#'
#' This function takes an address as input and returns the corresponding
#' geographic coordinates (latitude and longitude) using the
#' \href{https://geogratis.gc.ca/}{Geogratis API}.
#' The address is converted to raw bytes and used as a query to the Geogratis
#' API to obtain the geographic coordinates. The result is returned as a named
#' vector containing the latitude and longitude values.
#'
#' @param address <`character`> A character string specifying the address to geocode.
#'
#' @return A named vector containing the latitude and longitude values of the
#' geocoded address.
#' @export
geocode <- function(address) {
  # Return NULL if address isn't a character
  if (!is.character(address)) {
    return(NULL)
  }

  # Convert the string to raw bytes
  add <- paste0("%", charToRaw(address), collapse = "")

  # Make the address
  add <- paste0(
    "https://geogratis.gc.ca/services/geolocation/en/locate?q=",
    add
  )

  # Get and grab content
  get <- httr::GET(add)
  val <- httr::content(get)

  # If nothing is found, return NULL
  if (length(val) == 0) {
    return(NULL)
  }

  # If something is found, grab the coordinates of the first element
  val <- val[[1]]

  # Return the coordinates
  return(c(
    lat = val$geometry$coordinates[[1]],
    lon = val$geometry$coordinates[[2]]
  ))
}
