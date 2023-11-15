#' Initialize reactive values for Curbcut
#'
#' This function creates a set of reactive values used all around Curbcut (region,
#' lang, ...). It also sets up reactive alues for each module defined in the
#' \code{modules} object.
#'
#' @param server_session <`ShinySession`> The server session from `server.R`, used
#' tu update the tab and other necessary actions taken from modules to the server
#' itself.
#' @param lang_init <`character`> An optional character string specifying the
#' initial language for the Curbcut. Default is \code{NULL}.
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
r_init <- function(server_session,
                   lang_init = NULL,
                   map_zoom = get_from_globalenv("map_zoom"),
                   map_loc = get_from_globalenv("map_loc"),
                   ...) {

  regions_dictionary <- get_from_globalenv("regions_dictionary")

  # Initate
  r <- shiny::reactiveValues(
    server_session = shiny::reactiveVal(server_session),
    lang = shiny::reactiveVal(lang_init),
    region = shiny::reactiveVal(regions_dictionary$region[1]),
    default_select_ids = shiny::reactiveVal(NULL),
    place_explorer = shiny::reactiveValues(
      select_id = shiny::reactiveVal(NA),
      df = shiny::reactiveVal("DA")
    )
  )

  # Loop over all modules to add the rest of the needed reactives
  modules <- get_from_globalenv("modules")

  # NDS: We need to add `region` and `scale`, and there shouldn't be any
  # special casing. So every id gets every variable, and unused ones get NULL
  # values. This means modifying vars_build() to return NULL when it gets
  # missing inputs I guess.

  for (i in modules$id) {
    reg <- modules$regions[modules$id == i]
    reg <- unlist(reg)[1]

    # If there are no `regions` entry in the modules table, don't bother
    # saving a `df` value.
    if (is.null(reg)) {
      r[[i]] <- shiny::reactiveValues(
        select_id = shiny::reactiveVal(NA),
        zoom = shiny::reactiveVal(zoom_get(map_zoom)),
        coords = shiny::reactiveVal(map_loc),
        poi = shiny::reactiveVal(NULL),
        ...
      )
    } else {
      # Grab the first zoom level to be the default `df`
      page <- modules[modules$id == i, ]
      default_var <- page$default_var
      avail_scale_combinations <- page$avail_scale_combinations[[1]]
      mzl <- get_from_globalenv(sprintf("mzl_%s", avail_scale_combinations[1]))
      scale <- names(mzl)[1]
      # If there are var_left in the `modules`
      if (!is.na(default_var)) {
        time <- page$dates[[1]]
        region <- page$regions[[1]][1]
        if (!is.null(time)) {
          time <- max(time)
          default_var_yr <- sprintf("%s_%s", default_var, time)
        }
        vars <- vars_build(default_var, scale = scale, time = time)
        time <- vars$time
        vars <- vars$vars
        schemas <- page$additional_schemas[[1]]

        r[[i]] <- shiny::reactiveValues(
          vars = shiny::reactiveVal(vars),
          var_left_force = shiny::reactiveVal(default_var),
          time = shiny::reactiveVal(time),
          schemas = shiny::reactiveVal(list(var_left = schemas)),
          select_id = shiny::reactiveVal(NA),
          region = shiny::reactiveVal(region),
          zoom_levels = shiny::reactiveVal(mzl),
          scale = shiny::reactiveVal(scale),
          zoom = shiny::reactiveVal(zoom_get(map_zoom)),
          coords = shiny::reactiveVal(map_loc),
          poi = shiny::reactiveVal(NULL),
          ...
        )
      } else {
        r[[i]] <- shiny::reactiveValues(
          select_id = shiny::reactiveVal(NA),
          scale = shiny::reactiveVal(scale),
          zoom = shiny::reactiveVal(zoom_get(map_zoom)),
          coords = shiny::reactiveVal(map_loc),
          poi = shiny::reactiveVal(NULL),
          ...
        )
      }
    }
  }

  # Return r
  return(r)
}
