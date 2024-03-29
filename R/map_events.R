#' Get the Map View State as a Reactive
#'
#' This function retrieves the map view state by observing changes in the
#' `cc.map::get_map_viewstate` function. The  map view state is then stored and
#' returned as a reactive value.
#'
#' @param map_ID <`character`> The unique identifier of the map whose view state
#' is to be retrieved. This ID is used to monitor changes in the map's view state.
#'
#' @return A reactive value containing the most recent state of the map. This reactive value
#' updates whenever the  `cc.map::get_map_viewstate` function produces a non-null output.
#' @export
get_viewstate <- function(map_ID) {
  # Get the map view state as a reactive
  map_view_state <- shiny::reactiveVal(NULL)

  shiny::observeEvent(cc.map::get_map_viewstate(map_ID),
    {
      map_view_state(cc.map::get_map_viewstate(map_ID))
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  return(map_view_state)
}

#' Get the Map Click as a Reactive
#'
#' This function retrieves the map click by observing changes in the
#' `cc.map::get_map_click` function. The map view state is then stored and
#' returned as a reactive value.
#'
#' @param map_ID <`character`> The unique identifier of the map whose view state
#' is to be retrieved. This ID is used to monitor changes in the map's view state.
#'
#' @return A reactive value containing the most recent click event on the map. This reactive value
#' updates whenever the `cc.map::get_map_click` function produces a non-null output.
#' @export
get_click <- function(map_ID) {
  # Get the map click as a reactive
  map_click <- shiny::reactiveVal(NULL)
  observed_click <- shiny::reactive(cc.map::get_map_click(map_ID))

  shiny::observeEvent(observed_click(),
    {
      # In some cases, map_click() can ALREADY be NA. In that case, force a
      # reaction by writing a character NA and, in the function `update_select_id`,
      # turn it back to a logical NA
      if (!is.null(map_click())) {
        if (is.na(map_click()$ID) & is.na(observed_click()$ID)) {
          return(map_click(list(ID = "NA")))
        }
      }
      map_click(observed_click())
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  return(map_click)
}
