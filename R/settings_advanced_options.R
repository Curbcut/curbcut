#' Create a shiny radio button input for selecting a geographical region.
#'
#' This function creates a shiny radio button input for selecting a geographical
#' region from a predefined list of regions. The df of regions is obtained
#' from a global environment variable named `regions_dictionary`. This list
#' contains all the regions that can be picked, with the following columns:
#' `pickable` if the user can select them in the advance options, the `name`
#' column, and the `region` code.
#'
#' @param id <`character`> The ID of the module, in which this UI bit will
#' be used. Necessary for namespacing reasons.
#' @param region <`character`> A character string specifying the currently
#' selected region.
#' @param lang <`character`> A character string specifying the language in
#' which the names of the regions should be translated. Defaults to `NULL`
#' which means no translation.
#'
#' @return A shiny radio button input object.
#' @export
adv_opt_region <- function(id, region, lang = NULL) {
  # Get the regions dictionary from the global environment
  regions_dictionary <- get_from_globalenv("regions_dictionary")
  pickable_regions <- regions_dictionary[regions_dictionary$pickable, ]

  # Translate and get the text labels + the values
  choices_txt <-
    lapply(unname(pickable_regions$name), cc_t, lang = lang, force_span = TRUE)
  choices_values <- pickable_regions$region

  # Get the shiny input
  shiny::radioButtons(
    inputId = "region_change",
    label = cc_t(lang = lang, "Change default region", force_span = TRUE),
    inline = TRUE,
    selected = region,
    choiceNames = choices_txt,
    choiceValues = choices_values
  )
}

#' Create a UI element for selecting and locking a default location
#'
#' This function creates a Shiny UI element that allows the user to select
#' and lock in a default location (postal code or address) for further use in
#' any Curbcut map page. If a location is locked, then at every zoom level on
#' a map the location will be automatically, as a default, the zone in which
#' the locked location falls in.
#'
#' @param id <`character`> The ID of the module, in which this UI bit will
#' be used. Necessary for namespacing reasons.
#' @param lang A string indicating the language of the UI element. Defaults to
#' `NULL` which means no translation.
#'
#' @return A Shiny UI element for selecting and locking a default location.
#' @export
adv_opt_lock_selection_UI <- function(id, lang = NULL) {
  # Get the default location from the global environment
  default_random_address <- get_from_globalenv("default_random_address")

  shiny::tagList(
    # Lock in address of zone for select_ids
    shiny::strong(cc_t(
      lang = lang, "Enter and save a default location (postal ",
      "code or address)", force_span = TRUE
    )),
    shiny::HTML("<br>"),
    cc_t(
      lang = lang,
      "Default location will be saved until ",
      "manually cleared from advanced options", force_span = TRUE
    ),
    shiny::HTML(paste0(
      '<div class="shiny-split-layout">',
      '<div style="width: 80%; margin-top: var(--padding-v-md); ',
      'width:auto;">',
      shiny::textInput(
        inputId = "lock_address_searched",
        label = NULL,
        placeholder = default_random_address
      ),
      '</div><div style="width: 20%">',
      shiny::actionButton(
        inputId = "lock_search_button",
        label = shiny::icon("check", verify_fa = FALSE),
        style = "margin-top: var(--padding-v-md);"
      ),
      "</div></div>",
      shiny::actionButton(
        inputId = "cancel_lock_location",
        label = cc_t(lang = lang, "Clear default location", force_span = TRUE),
        icon = shiny::icon("xmark", verify_fa = FALSE),
        style = "margin-top: var(--padding-v-md);"
      )
    ))
  )
}

#' Select all the IDs for a given address or postal code
#'
#' This function takes an address or postal code as input and returns the
#' corresponding IDs of all the scales that fit around the address or postal code.
#' If the input is a postal code, the function checks if it is within an available
#' region and returns the corresponding zones. If the input is an address, the function
#' geocodes the address and checks the closest dissemination areas for the
#' geocoded point, returning the corresponding IDs. The output is a vector of
#' IDs that fit with the address or postal code.
#'
#' @param address <`character`> A character string specifying the address or
#' postal code to search for.
#' @param lang <`character`> A character string specifying the language to use
#' for the error messages. Defaults to NULL which is no translation.
#'
#' @return A vector of IDs for all scales that fit with the input address or
#' postal code.
#'
#' @seealso \code{\link{settings_server}} for the server code that uses this
#' function.
#' @seealso \code{\link{adv_opt_lock_selection_UI}} That creates the UI for this
#' function.
#'
#' @export
adv_opt_lock_selection <- function(address, lang = NULL) {
  # Grab the postal codes df from the global environment
  postal_codes <- get_from_globalenv("postal_codes")
  regions_dictionary <- get_from_globalenv("regions_dictionary")

  # Switch the address to a postal code to check if it is one
  postal_c <- tolower(address)
  postal_c <- gsub("[^[:alnum:]]", "", postal_c)
  is_pc <- grepl("[a-z][0-9][a-z][0-9][a-z][0-9]", postal_c)

  # In the case it is a poscal code
  if (is_pc) {
    subset_vec <- postal_codes$postal_code == postal_c
    # If no postal code is found, return a notification and NULL
    if (sum(subset_vec) == 0) {
      if (!is.null(shiny::getDefaultReactiveDomain())) {
        shiny::showNotification(
          cc_t(
            lang = lang,
            "Postal code `{postal_c}` isn't within an available region."
          ),
          type = "error"
        )
      }
      return(NULL)
    } else {
      DA_ID <- postal_codes$DA_ID[subset_vec]
      # Postal code found
      # Check all IDs that fit with the DA around the postal code
      all_ids <- sapply(regions_dictionary$region, \(x) {
        dat <- get0(paste0(x, "_DA"), envir = .GlobalEnv)
        if (is.null(dat)) {
          return("")
        }
        dat <- dat[dat$ID == DA_ID, ]
        unlist(dat[grepl("_ID$", names(dat))])
      }, simplify = FALSE, USE.NAMES = TRUE)

      # Return all the IDs
      if (!is.null(shiny::getDefaultReactiveDomain())) {
        shiny::showNotification(
          cc_t(lang = lang, "Postal code `{postal_c}` saved as default."),
          type = "default"
        )
      }
      return(unique(unlist(all_ids)))
    }
  }

  # If it's not a postal code
  if (!is_pc) {
    # Geocode the address
    lat_lon <- geocode(address)
    if (is.null(lat_lon)) {
      if (!is.null(shiny::getDefaultReactiveDomain())) {
        shiny::showNotification(
          cc_t(
            lang = lang,
            "Search `{address}` wasn't found within an available region."
          ),
          type = "error"
        )
      }
      return(NULL)
    }

    # Grab all the DA df and get their distance to the point
    all_ids <- sapply(regions_dictionary$region, \(x) {
      dat <- get0(paste0(x, "_DA"), envir = .GlobalEnv)
      if (is.null(dat)) {
        return("")
      }

      # Get the distance from the selected location to all DAs
      dat$lat <- sapply(dat$centroid, `[`, 1)
      dat$lon <- sapply(dat$centroid, `[`, 2)
      dist_vec <- get_dist(dat[c("lat", "lon")], lat_lon)
      min_dist <- min(dist_vec)

      # Grab the closest DA
      dat <- dat[which(dist_vec == min_dist), ]

      # Grab all the IDs
      list(
        IDs = unlist(dat[grepl("_ID$", names(dat))]),
        min_dist = min_dist
      )
    }, simplify = FALSE, USE.NAMES = TRUE)

    # Grab only the regions that have IDs
    all_ids <- all_ids[sapply(all_ids, length) > 1]

    # If no region has IDs, return a notification and NULL
    if (length(all_ids) == 0) {
      if (!is.null(shiny::getDefaultReactiveDomain())) {
        shiny::showNotification(
          cc_t(
            lang = lang,
            "Address `{address}` isn't within an available region."
          ),
          type = "error"
        )
      }
      return(NULL)
    } else {
      # Grab the minimum of all minimum distance
      min_of_min_dist <- min(unlist(sapply(all_ids, `[`, "min_dist")))

      # If no DA has been found in a 1km radius, return a notification and NULL
      if (min_of_min_dist > 1000) {
        if (!is.null(shiny::getDefaultReactiveDomain())) {
          shiny::showNotification(
            cc_t(
              lang = lang,
              "No zone has been found in a 1km radius of the provided address."
            ),
            type = "error"
          )
        }
        return(NULL)
      } else {
        if (!is.null(shiny::getDefaultReactiveDomain())) {
          shiny::showNotification(
            cc_t(lang = lang, "Address `{address}` saved as default."),
            type = "default"
          )
        }

        # Do not double `CSD_ID`. Keep only the one closer to the address
        ids <- lapply(all_ids, `[[`, 1)
        dist <- lapply(all_ids, `[[`, 2)

        scale_ids_available <-
          unique(unlist(sapply(ids, names, simplify = FALSE)))

        # For all scales with an ID available, which is the closer one to the address
        scales_keep <- sapply(scale_ids_available, \(scale) {
          # Include only the region that share the ID
          includes <- sapply(ids, \(n) scale %in% names(n))
          includes <- includes[includes]

          # Filter in only the distance for the region that share the ID
          d <- dist[names(dist) %in% names(includes)]

          # Which region is closer to the address
          names(includes[which(unlist(d) == min(unlist(d)))])
        }, USE.NAMES = TRUE)

        # Filter
        out <- mapply(\(id, scale) {
          ids[[scale]][[id]]
        }, names(scales_keep), scales_keep, USE.NAMES = FALSE)

        return(out)
      }
    }
  }
}
