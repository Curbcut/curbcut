#' Get the Map View State as a Reactive
#'
#' This function retrieves the map view state by observing changes in the `rdeck::get_view_state`
#' and `cc.map::get_map_viewstate` functions, whichever is used on the page. The
#' map view state is then stored and returned as a reactive value.
#'
#' @param map_ID <`character`> The unique identifier of the map whose view state
#' is to be retrieved. This ID is used to monitor changes in the map's view state.
#'
#' @return A reactive value containing the most recent state of the map. This reactive value
#' updates whenever the `rdeck::get_view_state` or `cc.map::get_map_viewstate` function
#' produces a non-null output.
#' @export
get_viewstate <- function(map_ID) {
  # Get the map view state as a reactive
  map_view_state <- shiny::reactiveVal(NULL)

  shiny::observeEvent(rdeck::get_view_state(map_ID), {
    map_view_state(rdeck::get_view_state(map_ID))
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  shiny::observeEvent(cc.map::get_map_viewstate(map_ID), {
    map_view_state(cc.map::get_map_viewstate(map_ID))
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  return(map_view_state)
}

#' Get the Map Click as a Reactive
#'
#' This function retrieves the map click by observing changes in the `rdeck::get_clicked_object`
#' and `cc.map::get_map_click` functions, whichever is used on the page. The
#' map view state is then stored and returned as a reactive value.
#'
#' @param map_ID <`character`> The unique identifier of the map whose view state
#' is to be retrieved. This ID is used to monitor changes in the map's view state.
#'
#' @return A reactive value containing the most recent click event on the map. This reactive value
#' updates whenever the `rdeck::get_clicked_object` or `cc.map::get_map_click` function
#' produces a non-null output.
#' @export
get_click <- function(map_ID) {
  # Get the map click as a reactive
  map_click <- shiny::reactiveVal(NULL)

  shiny::observeEvent(rdeck::get_clicked_object(map_ID), {
    map_click(rdeck::get_clicked_object(map_ID))
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  shiny::observeEvent(cc.map::get_map_click(map_ID), {
    map_click(cc.map::get_map_click(map_ID))
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  return(map_click)
}
