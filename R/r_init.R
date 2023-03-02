#' Initialize reactive values for Curbcut
#'
#' This function creates a set of reactive values used all around Curbcut (region,
#' lang, ...). It also sets up reactive alues for each module defined in the
#' \code{modules} object.
#'
#' @param lang_init <`character`> An optional character string specifying the
#' initial language for the Curbcut. Default is \code{NULL}.
#' @param default_region <`character`> The default region for the app. By default,
#' this value is retrieved from the \code{default_region} variable in the global
#' environment.
#' @param map_zoom <`numeric`> The default zoom level for the map. By default,
#' this value is retrieved from the \code{map_zoom} variable in the global environment.
#' @param map_loc <`numeric vector`> The default location for the map. By default,
#' this value is retrieved from the \code{map_loc} variable in the global environment.
#' @param ... Additional arguments to be passed to the reactive values of
#' each module. Arguments needs to be reactives, e.g. `other_id = shiny::reactiveVal(NA)`
#'
#' @return A reactive values object with settings for Curbcut
#'
#' @details This function implements the "Stratégie du petit r" (the strategy of
#' the little r), which is a programming style that emphasizes the use of local
#' variables and small functions to improve code clarity and maintainability.
#' This function creates a local variable \code{r} that contains all the reactive
#' values for the app, and it loops over each module to add the rest of the
#' needed reactive values. The result is a set of reactive values that are
#' organized and easy to maintain, and passed down through every module. This way,
#' every module has access to this set of reactives.
#' @export
r_init <- function(lang_init = NULL,
                   default_region = get_from_globalenv("default_region"),
                   map_zoom = get_from_globalenv("map_zoom"),
                   map_loc = get_from_globalenv("map_loc"),
                   ...) {
  # Initate
  r <- shiny::reactiveValues(
    lang = shiny::reactiveVal(lang_init),
    region = shiny::reactiveVal(default_region),
    default_select_ids = shiny::reactiveVal(NULL),
    stories = shiny::reactiveValues(select_id = shiny::reactiveVal(NA)),
    place_explorer = shiny::reactiveValues(
      select_id = shiny::reactiveVal(NA),
      df = shiny::reactiveVal("DA")
    )
  )

  # Loop over all modules to add the rest of the needed reactives
  modules <- get_from_globalenv("modules")

  for (i in modules$id) {
    df <- modules$regions[modules$id == i]
    df <- unlist(df)[[1]]

    # If there are no `regions` entry in the modules table, don't bother
    # saving a `df` value.
    if (is.null(df)) {
      r[[i]] <- shiny::reactiveValues(
        select_id = shiny::reactiveVal(NA),
        zoom = shiny::reactiveVal(zoom_get(map_zoom)),
        coords = shiny::reactiveVal(map_loc),
        poi = shiny::reactiveVal(NULL),
        ...
      )
    } else {
      # Grab the first zoom level to be the default `df`
      first_mzl <- get_from_globalenv(paste0("map_zoom_levels_", df))[1]
      first_mzl <- names(first_mzl)
      df <- paste(first_mzl, df, sep = "_")

      r[[i]] <- shiny::reactiveValues(
        select_id = shiny::reactiveVal(NA),
        df = shiny::reactiveVal(df),
        zoom = shiny::reactiveVal(zoom_get(map_zoom)),
        coords = shiny::reactiveVal(map_loc),
        poi = shiny::reactiveVal(NULL),
        ...
      )
    }
  }

  # Return r
  return(r)
}
