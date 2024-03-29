#' Update the saved zoom string for a given region based on a current zoom level
#'
#' This function takes in a previously saved zoom string and updates it based on
#' the current zoom level for a given region. If the new zoom string is
#' different from the previously saved one, it returns the new zoom string.
#' Otherwise, it returns the old one.
#'
#' @param rv_zoom_string <`character`> A character string representing a previously
#' saved zoom level for a given region
#' @param zoom <`numeric`> A numeric value representing the current zoom level
#' @param zoom_levels <`named numeric vector`> A named numeric vector of zoom
#' levels. Usually one of the `mzl_*`, or the output of
#' \code{\link{geography_server}}.
#'
#' @return A character string representing the updated zoom level for the given region
update_zoom_string <- function(rv_zoom_string, zoom, zoom_levels) {
  # Get the zoom string that would fit in the zoom level
  new <- zoom_get_string(
    zoom = zoom,
    zoom_levels = zoom_levels
  )

  # If the new zoom string is different, return it.
  if (new != rv_zoom_string) {
    return(new)
  }

  # If the same, return the old
  return(rv_zoom_string)
}

#' Get nearby points of interests (POIs) based on map view state
#'
#' This function retrieves the nearby POIs based on the current view state of a
#' map specified by the \code{map_id} parameter. The function filters the POIs
#' based on their proximity to the center of the map, keeping only those POIs
#' that are within 2000 meters. Currently only returns the points of interests
#' of the `stories` data.frame.
#'
#' @param id <`character`> The ID of the page in which this function will appear,
#' e.g. `alp`.
#' @param poi <`character vector`> The current POIs showing on the map.
#' @param map_viewstate <`list`> The map viewstate. Usually the output of the
#' \code{\link{map_js_server}}.
#'
#' @return A character vector of nearby POIs if new POIs are found; otherwise,
#' returns the input POI vector.
#' @export
update_poi <- function(id, poi, map_viewstate) {
  # Initialize objects
  out <- NULL
  zoom <- map_viewstate$zoom
  lat <- map_viewstate$latitude
  lon <- map_viewstate$longitude

  # Exit early if the map isn't sufficiently zoomed in
  if (zoom < 13) {
    return(NULL)
  }

  # Get POIs; currently just Stories. Return nothing if the `stories` df is
  # missing.
  stories <- get0("stories", envir = .GlobalEnv)
  if (is.null(stories)) {
    return(NULL)
  }

  points <- stories[c("name_id", "lon", "lat")]

  # Find distance from map centre to the POIs
  dist <- get_dist(points[c("lon", "lat")], c(lon, lat))

  # If any POI is within 2000 m of centre, filter it. If not, return NULL
  new_pois <- points$name_id[dist < 2000]
  if (length(new_pois) == 0) {
    return(NULL)
  }

  # If the new pois are the same as the ones currently showing, do not update
  # them.
  if (length(poi) == length(new_pois)) {
    if (all(new_pois == poi)) {
      return(poi)
    }
  }

  # If they are different, return the new points of interests
  return(new_pois)
}

#' Update Select ID module
#'
#' This module function updates the selected ID on a Curbcut map page. It uses the
#' \code{\link{get_click}} function to get the ID of the clicked map.
#' It looks if the selection has been made on a 'stories' bubble and if so,
#' links to the stories page.
#' If a new ID is selected, the function updates the `select_id` reactive to
#' the newly selected ID. If the same is selected twice, it returns NA. It also
#' updates the select_id reactive if a match is found with the IDs from
#' `r$default_select_ids()` if the user has decided to lock in a default location
#' in the advanced options.
#' The function also takes care of if a stories is clicked on the map. It displays
#' a modal with the HTML file of the stories in it, and listens to the click of a
#' button in the modal to redirect the user to the stories page.
#'
#' @param id <`character`> Indicates the ID of the current page.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param data <`reactive data.frame`> A data frame containing the `ID` column to be
#' checked for any match with the `r$default_select_ids()` changed in
#' the advanced options.
#' @param id_map <`character`> Indicates the ID of the object map, usually
#' created by \code{\link{map_js_server}}. Defaults to `"map"` as it is
#' the default of the `map_server` function.
#'
#' @return This function does not return a value. Instead, it updates the
#' `select_id` reactive in the provided reactive environment.
#' @export
update_select_id <- function(id, r, data = shiny::reactive(NULL),
                             id_map = "map") {
  # Must be its own module for the use of the button on the stories modal
  shiny::moduleServer(id, function(input, output, session) {
    click_init <- get_click(id_map)

    # Redirect to stories?
    click <- shiny::eventReactive(click_init(),
      {
        stories_link <- if ("layerName" %in% names(click_init())) {
          (grepl("_stories$", click_init()$layerName) & sprintf("%s-%s", id, id_map) != "stories-map")
        } else {
          FALSE
        }
        id <- click_init()$ID
        if (!is.na(id) & id == "NA") id <- NA
        attr(id, "stories_link") <- stories_link
        return(id)
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    # If it is a stories click, show the modal with the story
    shiny::observeEvent(click(), {
      if (attr(click(), "stories_link")) {
        # Get the stories HTML document source
        stories <- get_from_globalenv("stories")
        story_name <- stories$name_id[stories$ID == click()]
        story_src <- sprintf("stories/%s_%s.html", story_name, r$lang())

        modules <- get_from_globalenv("modules")
        stories_page <- cc_t(modules$nav_title[modules$id == "stories"], lang = r$lang())

        # Popup the modal
        shiny::showModal(shiny::modalDialog(
          # Hack the namespace of the button so that it's detectable from within
          # this module (nested in another page, so double ns)
          action_button(
            classes = c("floating-bar-btn", "visit-place-ex"),
            id = shiny::NS(id, shiny::NS(id, "go_stories")),
            icon = "auto_stories",
            text_class = "floating-panel-text",
            text = cc_t("Visit the {stories_page} page", lang = r$lang())
          ),
          shiny::tags$iframe(
            style = "width:100%;height:calc(100vh - 260px)",
            title = "story",
            src = story_src,
            frameborder = 0
          ),
          footer = shiny::modalButton(cc_t(lang = r$lang(), "Close")),
          size = "xl",
          easyClose = TRUE
        ))
      }
    })

    # If the user click on the 'visit the stories page' button from the previous modal
    shiny::observeEvent(input$go_stories, {
      shiny::removeModal()
      link(r = r, page = "stories")
    })

    # Grab the new selected ID (in the case it's not a stories link)
    new_ID <- shiny::eventReactive(click(), {
      if (!attr(click(), "stories_link")) {
        id <- click()
        # Get rid of the attribute
        attr(id, "stories_link") <- NULL
        return(id)
      } else {
        return(NA)
      }
    })

    # If a click has been made, change then `select_id` reactive
    shiny::observeEvent(new_ID(), {
      # If the same ID has been selected twice, return NA. If not, return the
      # newly selected ID
      out <- update_select_id_helper(
        new_ID = new_ID(),
        select_id = r[[id]]$select_id()
      )

      # Save the new selected ID in the reactive.
      r[[id]]$select_id(out)
    })

    # Update selected ID if there are default selections (from the advanced options,
    # stored in `r$default_select_ids()`)
    shiny::observe({
      if (is.null(data())) {
        return(NULL)
      }

      # At the current `data()`, which is the ID that fits
      out <- update_select_id_from_default(
        data = data(),
        default_select_ids = r$default_select_ids(),
        select_id = shiny::isolate(r[[id]]$select_id())
      )

      # Save the new selected ID in the reactive.
      r[[id]]$select_id(out)

      # With it, update the map (This does not come from a click on the map!)
      if (!is.na(out)) {
        cc.map::map_choropleth_update_selection(
          session = r$server_session(),
          map_ID = ns_doubled(
            page_id = id,
            element = "map"
          ),
          select_id = out
        )
      }

    })
  })
}

#' Update Select ID Helper
#'
#' @param new_ID <`character`> A character vector indicating the new ID that's
#' been selected.
#' @param select_id <`character`> A character indicating the current selected ID.
#'
#' @return If the same ID gets selected twice, the function returns NA.
#' Otherwise, it returns the new ID.
update_select_id_helper <- function(new_ID, select_id) {
  # Make sure the new ID is valid
  if (is.na(new_ID)) {
    return(NA)
  }

  # If the same ID gets selected twice, deactivate selection
  if (!is.na(select_id) && new_ID == select_id) {
    return(NA)
  }

  # Return new ID
  return(new_ID)
}

#' Update Select ID Based on the default ID (Location lock in advanced settings)
#'
#' This function updates the selected ID based on the provided default IDs
#' created through \code{\link{adv_opt_lock_selection}} present in the advanced
#' options, if any. If the provided default IDs are not present in the data,
#' the original select ID is returned.
#'
#' @param data <`data.frame`> A data frame containing the `ID` column to be
#' checked for any match with the `default_select_ids`
#' @param default_select_ids <`character vector`> Vector of default IDs created
#' through  \code{\link{adv_opt_lock_selection}} present in the advanced
#' options, usually `r$default_select_ids()`
#' @param select_id <`character`> the current selected ID, usually `r[[id]]$select_id()`
#'
#' @return The updated selected ID based on the provided default IDs
update_select_id_from_default <- function(data, default_select_ids, select_id) {
  if (is.null(default_select_ids)) {
    return(select_id)
  }

  which_row <- which(data$ID %in% default_select_ids)
  if (length(which_row) == 0) {
    return(select_id)
  }

  return(data$ID[which_row][[1]])
}

#' Get or update the `df` rv output
#'
#' The \code{update_scale} function returns the zoom string if the tile is on auto-scale,
#' otherwise it returns the tile.
#'
#' @param tile <`character`> a character string indicating the tile, the output
#' of the \code{\link{zoom_server}}
#' @param zoom_string <`character`> a character string indicating the zoom string,
#' the output of \code{\link{zoom_get_string}}
#'
#' @return a character string indicating the string of the data the user is
#' looking at. The combination of the region and the scale, e.g. `CMA_CSD`
#'
#' @export
update_scale <- function(tile, zoom_string) {
  # If on auto-scale (when tile contains an underscore declaring it's a combination
  # of scales), simply return the zoom_string
  if (grepl("_", tile)) {
    return(zoom_string)
  }

  # Outside of autozoom, return the tile
  return(tile)
}

#' Update the vars reactive value in a given reactive list r based on changes
#' in var_left and var_right
#'
#' This function is used to update the vars reactive value in a given reactive
#' list r whenever the input var_left, var_right and/or df changes. It uses the
#' \code{\link{vars_build}} function to determine the new vars value, and updates
#' it only if the new value is different from the current value.
#'
#' @param id <`character`> The id of the element in the reactive list r where
#' the vars reactive value is stored.
#' @param r <`reactive list`> A reactive list where the element with the given
#' id (the current page) contains a named list with a vars reactive value, which stores the
#' output of the curbcut::vars_build function.
#' @param var_left <`reactive character`> A reactive character string of the
#' selected variable, e.g. alp_2016 or c("housing_tenant_2006",
#' #' "housing_tenant_2016").
#' @param var_right <`reactive character`> A reactive character string of the
#' selected compared variable, e.g. `housing_value`.
#' @param scale <`reactive character`> A string specifying the scale at which to update
#' vars. e.g. `DA` or `CSD`.
#' @param widget_time <`reactive vector`> The time selected by the user on the
#' time widget. Length 1 (single year) or 2 (compare years).
#'
#' @return An observer that updates the vars reactive value `r[[id]]$vars`
#' whenever var_left, var_right and or df changes.
#' @export
update_vars <- function(id, r, var_left, var_right, scale, widget_time) {
  vr <- shiny::reactive({
    vars_build(
      var_left = var_left(),
      var_right = var_right(),
      scale = scale(),
      time = widget_time()
    )
  })

  # Update vars and time only if they actually change
  update_rv(id, r, rv_name = "vars", new_val = shiny::reactive(vr()$vars))
  update_rv(id, r, rv_name = "time", new_val = shiny::reactive(vr()$time))
}

#' Update a reactive value object
#'
#' This function updates a reactive value object and does not trigger a change
#' in the reactive value if the new value is the same as the previous value. It
#' observes a new value, and if this new value is different from the current one,
#' the function updates the rv. It uses the reactive programming features of Shiny.
#'
#' @param id <`character`> The ID of the page in which this module will appear,
#' e.g. `alp`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param rv_name <`character`> A character string specifying the name of the
#' reactive value to create and update.
#' @param new_val <`reactive`> A reactive expression returning the new value to
#' assign to the rv. If the new value is the same as the current one, the function
#' does nothing.
#' @param default_val <`vector`> A vector of length one. This parameter is used
#' to initialize the rv. Defaults to NULL for variables already initialized.
#'
#' @return
#' This function does not return a value. It updates the reactive value in place.
#' The updated value can be accessed using the rv object and the rv_name parameter.
#' If the new value is the same as the current one, the function does nothing and
#' returns NULL.
#' @export
update_rv <- function(id, r, rv_name, new_val, default_val = NULL) {
  if (!is.null(default_val)) {
    r[[id]][[rv_name]] <- shiny::reactiveVal(default_val)
  }

  shiny::observeEvent(new_val(), {
    if (identical(new_val(), r[[id]][[rv_name]]())) {
      return(NULL)
    }
    r[[id]][[rv_name]](new_val())
  }, ignoreNULL = TRUE)
}

#' Update the page's `region` reactive value in the reactive list `r`
#'
#' This function observes changes in `new_region` and updates the `region`
#' reactive value within the reactive list `r` identified by the given `id`.
#' The update is triggered only if `new_region` is different from the current
#' `region` value stored in `r[[id]]`.
#'
#' @param id <`character`> The ID of the element in the reactive list `r` where
#' the `region` reactive value is stored.
#' @param r <`reactive list`> A reactive list where the element identified by
#' `id` contains a named list with a `region` reactive value. The output
#' of \code{\link{r_init}}.
#' @param new_region <`reactive`> A reactive expression that returns the new
#' value for the `region`. The first output of the \code{\link{geography_server}}
#' function.
#'
#' @return An observer that updates the `region` reactive value in `r[[id]]`
#' whenever `new_region` changes.
#' @export
update_region <- function(id, r, new_region) {
  update_rv(id, r, "region", new_val = new_region)
}

#' Update the page's `zoom_levels` reactive value in the reactive list `r`
#'
#' This function observes changes in `new_zl` and updates the `zoom_levels`
#' reactive value within the reactive list `r` identified by the given `id`.
#' The update is triggered only if `new_zl` is different from the current
#' `zoom_levels` value stored in `r[[id]]`.
#'
#' @param id <`character`> The ID of the element in the reactive list `r` where
#' the `region` reactive value is stored.
#' @param r <`reactive list`> A reactive list where the element identified by
#' `id` contains a named list with a `zoom_levels` reactive value. The output
#' of \code{\link{r_init}}.
#' @param new_zl <`reactive`> A reactive expression that returns the new
#' value for the `zoom_levels`. The second output of the \code{\link{geography_server}}
#' function.
#'
#' @return An observer that updates the `zoom_levels` reactive value in `r[[id]]`
#' whenever `new_zl` changes.
#' @export
update_zoom_levels <- function(id, r, new_zl) {
  update_rv(id, r, "zoom_levels", new_val = new_zl)
}

#' Track Previous Reactive Values
#'
#' This function tracks and stores previous and current values of a reactive
#' expression. It returns an eventReactive object that holds the previous value.
#'
#' @param reactive_expr <`reactive expression`> The reactive expression to track.
#'
#' @return <`eventReactive`> An eventReactive object containing the previous value.
#' @export
track_previous_reactive <- function(reactive_expr) {
  # Create reactiveValues to store current and previous values
  value_tracker <- shiny::reactiveValues()

  # Observe changes in reactive_expr and update value_tracker
  shiny::observeEvent(reactive_expr(), {
    # Do not update the values if they are the same
    if (!is.null(value_tracker$current_value)) {
      if (reactive_expr() == value_tracker$current_value) {
        return(NULL)
      }
    }

    value_tracker$prev_value <- value_tracker$current_value
    value_tracker$current_value <- reactive_expr()
  })

  # Create eventReactive to keep track of previous values
  previous_value_reactive <- shiny::eventReactive(reactive_expr(), {
    value_tracker$prev_value
  })

  # Return the eventReactive containing previous value
  return(previous_value_reactive)
}
