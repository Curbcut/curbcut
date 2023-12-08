#' Geography module for CUrbcut
#'
#' This function creates a shiny module server to handle geography settings.
#' It initializes two dropdown menus for selecting regions and top scale.
#' A calculated map zoom level is also provided.
#'
#' @param id <`character`> The ID of the page in which this module will appear,
#' e.g. `alp`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link{vars_build}} function. The class of the vars object is
#' used to determine which type of legend to draw.
#' @param regions <`character vector`> Possible available regions for selection.
#' @param avail_scale_combinations <`character vector`> Available scale combinations
#' for the top scale.
##' @param scales_as_DA <`reactive character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`.
#'
#' @return A reactive list containing selected region and calculated zoom levels.
#' @export
geography_server <- function(id, r, vars, regions, avail_scale_combinations,
                             scales_as_DA = shiny::reactive(c("building", "street"))) {
  shiny::moduleServer(id, function(input, output, session) {
    # Region default
    regions_dictionary <- get_from_globalenv("regions_dictionary")
    names(regions) <- sapply(regions,
      \(x) regions_dictionary$name[regions_dictionary$region == x],
      simplify = TRUE, USE.NAMES = FALSE
    )
    regions_list <- shiny::reactive({
      names(regions) <- sapply(names(regions), cc_t, lang = r$lang())
      return(list("Region" = regions))
    })

    # Top scale default
    top_scales <- gsub("_.*", "", avail_scale_combinations)
    top_scales <- unique(top_scales)
    scales_dictionary <- get_from_globalenv("scales_dictionary")
    names(top_scales) <- sapply(top_scales, function(x) {
      scales_dictionary$plur[scales_dictionary$scale == x]
    }, simplify = TRUE, USE.NAMES = FALSE)

    scales_subtext_pre <- sapply(top_scales, function(x) {
      out <- scales_dictionary$subtext[scales_dictionary$scale == x]
    }, simplify = TRUE, USE.NAMES = FALSE)
    nb_residents <- sapply(top_scales, \(x) {
      pop <- get_from_globalenv(x)$population
      pop <- pop[-find_outliers(pop, lower_bracket = 0.25, higher_bracket = 0.75,
                                iqr_multiplier = 0)]
      min_val <- convert_unit(x = min(pop, na.rm = TRUE))
      max_val <- convert_unit(x = max(pop, na.rm = TRUE))

      sprintf("%s-%s", min_val, max_val)
    })

    scales_subtext <- shiny::reactive({
      out <- sapply(scales_subtext_pre, cc_t, lang = r$lang())
      res <- cc_t("residents", lang = r$lang())
      sprintf("%s. %s %s.", out, nb_residents, res)
    })

    top_scales_list <- shiny::reactive({
      names(top_scales) <- sapply(names(top_scales), cc_t, lang = r$lang())
      names(top_scales) <- s_sentence(names(top_scales))
      return(list("Main scale" = top_scales))
    })

    # If there's a default region, chane the ger to use it.
    shiny::observeEvent(
      r$region(), {
        if (!r$region() %in% regions) {
          return(NULL)
        }

        update_ger_val(NULL)
        update_ger_val(r$region())
      } # , once = TRUE
    )

    # Grab the value of the dropdowns
    reg_out <- picker_server(
      id = "ger", r = r,
      var_list = regions_list,
      selected = update_ger_val
    )

    tp_out <- picker_server(
      id = "get", r = r,
      var_list = top_scales_list,
      selected = update_get_val,
      subtext = scales_subtext
    )

    # Keep track of previous values when they change. This will be needed
    # if the user wants to undo a geography change.
    previous_reg <- track_previous_reactive(reactive_expr = reg_out)
    previous_tp <- track_previous_reactive(reactive_expr = tp_out)

    # When the region changes, update the top scale (if necessary)
    update_get_val <- shiny::reactiveVal(NULL)
    shiny::observeEvent(reg_out(), {
      # For this newly picked region, does the current top scale works?
      tp_works <- vapply(scales_dictionary$regions, \(x) reg_out() %in% x, logical(1))
      # If it works, return
      if (tp_out() %in% names(tp_works)[tp_works]) {
        return()
      }

      # If the top scale does not correspond to the newly picked region, update
      # the top scale picker to the highest scale in order of priority (first in
      # the dictionary).
      new_top_scale <- top_scales[[which(tp_works)[1]]]

      # Inform the message
      reg_name <- regions_dictionary$name[regions_dictionary$region == reg_out()]
      tp_name <- scales_dictionary$plur[scales_dictionary$scale == new_top_scale] |>
        s_sentence()

      # Assign older values to inform the undo button function
      static_previous_tp <- tp_out()
      # Show temporary message
      show_message(
        id = id,
        r = r,
        input = input,
        container_div = "geography_message",
        message = sprintf(
          cc_t("Changing the region to <b>%s</b> required changing the ",
            "main scale to <b>%s</b>.",
            lang = r$lang()
          ),
          cc_t(reg_name, lang = r$lang()),
          cc_t(tp_name, lang = r$lang())
        ),
        undo_fun = \() {
          update_get_val(NULL)
          update_ger_val(NULL)
          update_get_val(static_previous_tp)
          update_ger_val(previous_reg())
        }
      )

      update_get_val(NULL)
      update_get_val(new_top_scale)
    })

    # When the top scale changes, update the region (if necessary)
    update_ger_val <- shiny::reactiveVal(NULL)
    shiny::observeEvent(tp_out(), {
      # Grab the possible regions
      possible_regions <- scales_dictionary$regions[[tp_out()]]

      # If the top scale is already in the possible regions, return
      if (reg_out() %in% possible_regions) {
        return()
      }

      # If the region does not correspond anymore to the newly picked scale, update
      # the region picker to the highest scale in order of priority (first in
      # the dictionary).
      new_region <- possible_regions[1]

      # Inform the message
      tp_name <- scales_dictionary$plur[scales_dictionary$scale == tp_out()] |>
        s_sentence()
      reg_name <- regions_dictionary$name[regions_dictionary$region == new_region]

      # Assign older values to inform the undo button function
      static_previous_reg <- reg_out()
      # Show temporary message
      show_message(
        id = id,
        r = r,
        input = input,
        container_div = "geography_message",
        message = sprintf(
          cc_t("Changing the main scale to <b>%s</b> required changing the ",
            "region to <b>%s</b>.",
            lang = r$lang()
          ),
          cc_t(tp_name, lang = r$lang()),
          cc_t(reg_name, lang = r$lang())
        ),
        undo_fun = \() {
          update_ger_val(NULL)
          update_get_val(NULL)
          update_ger_val(static_previous_reg)
          update_get_val(previous_tp())
        }
      )

      update_ger_val(NULL)
      update_ger_val(new_region)
    })

    # Grab a zoom level out of the top scale value
    mzl <- shiny::reactive({
      scale_comb <- zoom_level_selection(
        vars = vars(),
        top_scale = tp_out(),
        avail_scale_combinations = avail_scale_combinations,
        scales_as_DA = scales_as_DA())
      get_from_globalenv(sprintf("mzl_%s", scale_comb))
    })

    # observe(print(input$geo_hover))

    # Return the pickers
    return(shiny::reactive(list(
      region = reg_out(),
      zoom_levels = mzl()
    )))
  })
}


#' @describeIn geography_server Create the UI for the zoom module
#' @export
geography_UI <- function(id, regions, avail_scale_combinations) {
  # Region default
  regions_dictionary <- get_from_globalenv("regions_dictionary")
  names(regions) <- sapply(regions,
    \(x) regions_dictionary$name[regions_dictionary$region == x],
    simplify = TRUE, USE.NAMES = FALSE
  )
  regions_list <- list("Region" = regions)

  # Top scale default
  top_scales <- gsub("_.*", "", avail_scale_combinations)
  top_scales <- unique(top_scales)
  scales_dictionary <- get_from_globalenv("scales_dictionary")
  names(top_scales) <- sapply(top_scales, function(x) {
    scales_dictionary$plur[scales_dictionary$scale == x] |>
      s_sentence()
  }, simplify = TRUE, USE.NAMES = FALSE)
  top_scales_list <- list("Main scale" = top_scales)

  out <- shiny::tagList(
    # # Add the necessary piece of JS to inform a hover event. HOWEVER, IT DOES
    # # NOT WORK AS THE CONTAINER FOR OPENED DROPDOWN IS 'BODY' IN PICKER_UI.
    # shiny::tags$head(
    #   shiny::tags$script(
    #     shiny::HTML(
    #       paste0(
    #         "$(document).on('mouseenter', '",
    #         "#", shiny::NS(id, "geography_div"),
    #         " .dropdown-menu.inner li', function() {",
    #         "var hoveredText = $(this).text().trim();",
    #         "Shiny.setInputValue('", shiny::NS(id, "geo_hover"), "', hoveredText, {priority: 'event'});",
    #         "});")))
    # ),

    shiny::div(
      class = "geography-panel",
      id = shiny::NS(id, "geo_div"),
      shiny::hr(),
      shiny::div(
        class = "shiny-split-layout sidebar-section-title",
        shiny::div(
          style = "width: 9%",
          icon_material_title("layers")
        ),
        shiny::div(
          style = "width: 80%",
          cc_t_span("Geography")
        )
      ),
      shiny::div(
        id = shiny::NS(id, "geography_div"),
        class = "sus-sidebar-control",
        picker_UI(
          id = shiny::NS(id, "ger"),
          var_list = regions_list,
          # label = cc_t("Region"),
          div_style = "display: inline-block; width: 50%;"
        ),
        picker_UI(
          id = shiny::NS(id, "get"),
          var_list = top_scales_list,
          # label = cc_t("Main scale"),
          div_style = "display: inline-block; width: 48%;"
        )
      ),
      # Location of a short-lived message
      shiny::div(
        id = shiny::NS(id, "geography_message"),
        class = "sus-sidebar-control"
      )
    )
  )

  # If there's only one option, do not show the module
  if (length(top_scales) == 1 & length(regions) == 1) {
    return(shinyjs::hidden(out))
  } else {
    return(out)
  }
}
