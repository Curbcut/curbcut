#' Reverse geocode a location using `https://photon.komoot.io/`
#'
#' @param lon <`numeric`> Represents the longitude of the location
#' @param lat <`numeric`> Represents the latitude of the location
#'
#' @return A character vector of the reverse geocoded location (address)
#' @export
#'
#' @examples
#' rev_geocode(12.4924, 41.8902) # "Piazza del Colosseo, Roma"
rev_geocode <- function(lon, lat) {
    link <- paste0("photon.komoot.io/reverse?lon=", lon, "&lat=",
                   lat)
    out <- tryCatch(httr::content(httr::GET(link, httr::timeout(2))),
                    error = function(e) NULL)
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
    if (is.null(name)) return(NA_character_)
    return(name)
}
