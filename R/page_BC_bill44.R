auckland_scenario_nb <- function(time) {
  if (time == "050") {
    "2.055"
  } else if (time == "100") {
    "4.11"
  } else if (time == "150") {
    "6.165"
  }
}

#' @export
explore_text_bill44 <- function(vars, region, select_id, scale, time, data,
                                schemas, zoom_levels,
                                scales_as_DA = c("building", "street"),
                                lang = NULL, ...) {
  UseMethod("explore_text_bill44", vars)
}

#' @export
explore_text_bill44.q5 <- function(vars, region, select_id, scale, time, data,
                                   schemas, zoom_levels,
                                   scales_as_DA = c("building", "street"),
                                   lang = NULL, ...) {

  # Detect if we should switch the scale for DAs in the case the `scale` is part
  # of the `scales_as_DA` argument.
  switch_DA <- is_scale_in(scales_as_DA, scale)

  # Adjust the selected ID in the case where the selection is not in `data`
  if (!switch_DA && !select_id %in% data$ID) select_id <- NA

  context <- explore_context(
    region = region, select_id = select_id, scale = scale,
    zoom_levels = zoom_levels, switch_DA = switch_DA, lang = "en"
  )

  # The context might have used a scale in the `scales_as_DA` argument, and
  # the select_id needs to be switched to that of the dissemination area.
  if ("select_id" %in% names(context)) select_id <- context$select_id

  # Check for NAs in the selected value. Return NA message if it is the case
  na_check <- curbcut:::explore_text_check_na(
    context = context, data = data,
    select_id = select_id, vars = vars,
    time = time, lang = lang, schemas = schemas
  )
  if (!is.null(na_check)) {
    return(na_check)
  }

  # Under which scenario
  scn <- time_chr(var = vars$var_left, time_val = time)
  scn <- if (time$var_left == "000") "In 2021" else sprintf("<p>Under the '%s' scenario", scn)

  # Value
  value <- explore_text_values_q5(
    var = vars$var_left, region = region,
    select_id = select_id, data = data,
    scale = context$treated_scale, lang = lang,
    time = time, schemas = schemas
  )

  # Sentence form
  type <- var_get_info(vars$var_left, what = "type")[[1]]
  verb_tense <- if (time$var_left == "000") {
    if ("count" %in% type) "were" else "was"
  } else {
    "would be"
  }
  form <- if ("count" %in% type) {
    "%s, there %s %s dwelling units %s."
  } else {
    "%s, the density of dwelling units %s %s dwellings per square kilometre %s."
  }
  out <- sprintf(form, scn, verb_tense, value$val, context$p_start)

  # If it's not status quo, add a parenthesis
  if (time$var_left != "000") {

    # COUNT INFORMATION
    time_status_quo_compare <- time
    time_status_quo_compare$var_left <- c("000",  time_status_quo_compare$var_left)

    schemas_status_quo_compare <- schemas
    schemas_status_quo_compare$var_left$time <- c("000", schemas_status_quo_compare$var_left$time)

    count_vars <- vars_build("bill44_count", scale = context$treated_scale, time = time$var_left)$vars
    count_data <- data_get(vars = count_vars, scale = context$treated_scale, region = region)

    if ("count" %in% class(vars$var_left)) {

      exp_vals <- curbcut:::explore_text_delta_exp(
        var = vars$var_left, region = region,
        select_id = select_id, data = count_data,
        scale = context$treated_scale,
        left_right = "left", lang = lang,
        time = time_status_quo_compare, schemas = schemas_status_quo_compare
      )

      change_string <- explore_text_delta_change(
        var = vars$var_left, exp_vals = exp_vals, lang = lang
      )

      inc_pct <- curbcut:::convert_unit.pct(x = change_string$pct_change)
      current_val <- curbcut:::convert_unit(x = exp_vals$region_vals[[2]])

      out <- sprintf("%s This is a %s increase over the initial (2021) %s dwelling units .",
                     out, inc_pct, current_val)
    }

    if ("sqkm" %in% class(vars$var_left)) {

      count_exp_vals <- curbcut:::explore_text_delta_exp(
        var = count_vars$var_left, region = region,
        select_id = select_id, data = count_data,
        scale = context$treated_scale,
        left_right = "left", lang = lang,
        time = time_status_quo_compare, schemas = schemas_status_quo_compare
      )

      count_new_val <- convert_unit(x = count_exp_vals$region_vals[[1]])
      count_current_val <- convert_unit(x = count_exp_vals$region_vals[[2]])

      # SQKM INFORMATION
      exp_vals <- curbcut:::explore_text_delta_exp(
        var = vars$var_left, region = region,
        select_id = select_id, data = data,
        scale = context$treated_scale,
        left_right = "left", lang = lang,
        time = time_status_quo_compare, schemas = schemas_status_quo_compare
      )

      change_string <- explore_text_delta_change(
        var = vars$var_left, exp_vals = exp_vals, lang = lang
      )

      inc_pct <- curbcut:::convert_unit.pct(x = change_string$pct_change)
      current_val <- convert_unit(x = exp_vals$region_vals[[2]])

      tot_dw <- sprintf(" (%s total dwellings).", count_new_val)
      out <- gsub("\\.$", tot_dw, out)
      out <- sprintf(paste0("%s This is a %s increase over the initial %s ",
                            "dwellings per square kilometre (%s total dwellings)."),
                     out, inc_pct, current_val, count_current_val)
    }

  }

  # Add the second paragraph if there is a selection
  if (!is.na(select_id) && !value$na) {
    # Add header
    out <- sprintf("<p><b>%s</b>%s", context$heading, out)

    # Get the information on how the selection compares
    relat <- curbcut:::explore_text_selection_comparison(
      var = vars$var_left, data = data,
      select_id = select_id, lang = lang,
      time_col = time$var_left, schemas = schemas
    )

    # Make the first sentence of the paragraph
    verb <- if (time$var_left == "000") "was" else "would be"
    first_step <- sprintf(
      cc_t("This %s %s for %s", lang = lang), verb, relat$rank_chr,
      context$to_compare_determ
    )

    # Grab the explanation and capitalize the first letter
    exp <- var_get_info(vars$var_left,
                        what = "explanation", translate = TRUE,
                        lang = lang, schemas_col = schemas$var_left
    ) |>
      s_sentence()

    # Plug the right elements for the final sentence
    second_step <- sprintf(
      cc_t("%s %s %s %s than in %s of other %s %s", lang = lang), exp,
      context$p_start, verb, relat$higher_lower, relat$higher_lower_than, context$scale_plur,
      context$to_compare_short
    )

    # Bind it all
    out <- sprintf("%s<p>%s. %s.", out, first_step, second_step)

  }

  if (time$var_left == "000") return(out)

  # Reduced, complete, expanded
  rce <- if (time$var_left == "050") {
    "a reduced"
  } else if (time$var_left == "100") {
    "an exact"
  } else if (time$var_left == "150") {
    "an exceeding"
  }

  additional_inc <- auckland_scenario_nb(time$var_left)

  out <- sprintf(paste0(
    "%s<p>This scenario represents the potential future increase in housing supp",
    "ly following the assumption that construction trends follow %s ",
    "version (%s%%) of the outcomes in Auckland. The projection includes the",
    " historical rate of housing growth from 2016-2021, based on Census dat",
    "a, and the assumption that this rate remains unchanged from 2021-2029.",
    " Following the implementation of Bill 44, from 2024-2029, this scenari",
    "o projects an evenly distributed additional %s%% increase in housing",
    " growth based on Auckland’s precedent."
  ), out, rce, as.numeric(time$var_left), additional_inc)

  # Return the text
  return(out)

}

#' @export
explore_text_bill44.delta <- function(vars, region, select_id, scale, time, data,
                                      schemas, zoom_levels,
                                      scales_as_DA = c("building", "street"),
                                      lang = NULL, ...) {
  # Detect if we should switch the scale for DAs in the case the `scale` is part
  # of the `scales_as_DA` argument.
  switch_DA <- is_scale_in(scales_as_DA, scale)

  # Adjust the selected ID in the case where the selection is not in `data`
  if (!switch_DA && !select_id %in% data$ID) select_id <- NA

  # Grab the shared info
  context <- explore_context(
    region = region, select_id = select_id, scale = scale,
    zoom_levels = zoom_levels, switch_DA = switch_DA, lang = lang
  )

  # The context might have used a scale in the `scales_as_DA` argument, and
  # the select_id needs to be switched to that of the dissemination area.
  if ("select_id" %in% names(context)) select_id <- context$select_id

  # Check for NAs in the selected value. Return NA message if it is the case
  na_check <- curbcut:::explore_text_check_na(
    context = context, data = data,
    select_id = select_id, vars = vars,
    lang = lang, time = time, schemas = schemas
  )
  if (!is.null(na_check)) {
    return(na_check)
  }

  # Grab the explanation and region values
  exp_vals <- curbcut:::explore_text_delta_exp(
    var = vars$var_left, region = region,
    select_id = select_id, data = data,
    scale = context$treated_scale,
    left_right = "left", lang = lang,
    time = time, schemas = schemas
  )

  exp_nodet <- if (vars$var_left == "bill44_count") {
    "number of dwelling units"
  } else {
    "number of dwelling units per square kilometre"
  }

  # If it remains the same, return it directly
  if (exp_vals$region_vals_strings[[1]] == exp_vals$region_vals_strings[[2]]) {
    out <- sprintf(paste0("<p>%s, the %s between these ",
                          "two scenarios would remain the same"),
                   s_sentence(context$p_start), exp_nodet)
    return(out)
  }

  out <- sprintf(paste0("<p>%s, the %s would differ from %s ",
                        "under the '%s' scenario to %s under the '%s' scenario."),
                 s_sentence(context$p_start), exp_nodet,
                 exp_vals$region_vals_strings[[2]],
                 time_chr(vars$var_left, time_val = exp_vals$times[[1]]),
                 exp_vals$region_vals_strings[[1]],
                 time_chr(vars$var_left, time_val = exp_vals$times[[2]]))


  # Get the necessary information for the second paragraph
  change_string <- explore_text_delta_change(
    var = vars$var_left,
    exp_vals = exp_vals,
    lang = lang
  )

  # Did it increase or decrease? put in color
  inc_dec <- if (change_string$pct_change > 0) {
    cc_t("an increase", lang = lang) |>
      explore_text_color(meaning = "increase")
  } else {
    cc_t("a decrease", lang = lang) |>
      explore_text_color(meaning = "decrease")
  }

  out <- sprintf(paste0("%s<p>The difference between these two scenarios would ",
                        "be %s of %s dwellings"), out, inc_dec, change_string$text)
  out <- if (vars$var_left == "bill44_count") {
    sprintf("%s.", out)
  } else {
    sprintf("%s per square kilometre.", out)
  }

  # Return the first paragraph if there are no selections
  if (!is.na(select_id)) {

    # Craft the second paragraph
    relat <- curbcut:::explore_text_selection_comparison(
      var = vars$var_left,
      data = data,
      select_id = select_id,
      col = "var_left",
      ranks_override = c(
        "exceptionally small", "unusually small",
        "just about average", "unusually large",
        "exceptionally large"
      ),
      lang = lang,
      time_col = time$var_left,
      schemas = schemas,
      larger = TRUE
    )

    inc_dec <- if (change_string$pct_change > 0) {
      cc_t("increase", lang = lang) |>
        explore_text_color(meaning = "increase")
    } else {
      cc_t("decrease", lang = lang) |>
        explore_text_color(meaning = "decrease")
    }

    first_part <- sprintf(
      cc_t("This %s is %s for %s.", lang = lang), inc_dec,
      relat$rank_chr, context$to_compare_deter
    )

    second_part <-
      sprintf(
        paste0("The change in %s between the '%s' and the ",
               "'%s' scenarios is %s than in %s of other %s between the same scenarios."),
        exp_nodet, time_chr(vars$var_left, exp_vals$times[1]),
        time_chr(vars$var_left, exp_vals$times[2]),
        relat$higher_lower, relat$higher_lower_than, context$scale_plur
      )

    out <- sprintf("%s<p>%s %s", out, first_part, second_part)

  }

  # Separe the following into two sentences.
  premier <- if (time$var_left[[1]] == "000") {
    sprintf(paste0(
      "The '%s' scenario represents the existing dwelling inform",
      "ation from the latest Canadian census without any zoning changes."
    ), time_chr(vars$var_left, exp_vals$times[1]))
  } else {
    additional_inc <- auckland_scenario_nb(time$var_left[[1]])
    sprintf(paste0(
      "The '%s' scenario projects a business as usual growth in dw",
      "ellings units (based on the historical rate of housing growth from 201",
      "6-2021) with an additional %s%% increase in evenly distributed develo",
      "pment in areas impacted by Bill 44 from 2024-2029."
    ), time_chr(vars$var_left, exp_vals$times[1]), additional_inc)
  }

  deuxieme <-  if (time$var_left[[1]] == "000") {
    additional_inc <- auckland_scenario_nb(time$var_left[[2]])
    sprintf(paste0(
      "Whereas the '%s' scenario projects a business as usual growth in dw",
      "ellings units (based on the historical rate of housing growth from 201",
      "6-2021) with an additional %s%% increase in evenly distributed develo",
      "pment in areas impacted by Bill 44 from 2024-2029."
    ), time_chr(vars$var_left, exp_vals$times[2]), additional_inc)
  } else {
    additional_inc <- auckland_scenario_nb(time$var_left[[2]])
    sprintf(paste0(
      "Whereas the '%s' scenario projects an additional %s%% increase."
    ), time_chr(vars$var_left, exp_vals$times[2]), additional_inc)
  }

  # Scenarios explanation
  out <- sprintf("%s<p>%s %s", out, premier, deuxieme)

  # Return
  return(out)
}


#' @export
bill44_UI <- function(id) {
  modules <- get_from_globalenv("modules")
  page <- modules[modules$id == id, ]
  regions <- page$regions[[1]]
  if (is.null(regions)) {
    stop(sprintf(paste0(
      "Page `%s` does not have available regions. Please ",
      "check the `regions` column in the `modules` ",
      "dataframe."
    ), id))
  }
  avail_scale_combinations <- page$avail_scale_combinations[[1]]
  mzp <- get_from_globalenv(sprintf("mzl_%s", avail_scale_combinations[1]))
  theme_lowercased <- gsub(" .*", "", tolower(page$theme))
  stories <- get_from_globalenv("stories", stop_if_missing = FALSE)

  # Grab the possible regions for the module
  possible_regions <- page$regions[[1]][1]

  shiny::tagList(
    # Sidebar
    shiny::div(
      `data-theme` = theme_lowercased,
      sidebar_UI(
        id = shiny::NS(id, id),
        autovars_UI(shiny::NS(id, id)),
        warnuser_UI(shiny::NS(id, id)),
        compare_UI(
          id = shiny::NS(id, id),
          var_list = dropdown_make(vars = " ", compare = TRUE)
        ),
        geography_UI(shiny::NS(id, id),
                     regions = regions,
                     avail_scale_combinations = avail_scale_combinations
        ),
        shiny::hr(),
        zoom_UI(shiny::NS(id, id), zoom_levels = mzp),
        bottom = shiny::tagList(
          legend_UI(shiny::NS(id, id))
        )
      ),

      # Map
      map_js_UI(shiny::NS(id, id)),

      # Change view (Map/Data/Place explorer)
      panel_view_UI(id = shiny::NS(id, id)),

      # Right panel
      right_panel(
        id = shiny::NS(id, id),
        explore_UI(shiny::NS(id, id)),
        dyk_UI(shiny::NS(id, id))
      )
    )
  )
}

#' @export
bill44_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {

    modules <- get_from_globalenv("modules")
    page <- modules[modules$id == id, ]
    regions <- page$regions[[1]]
    if (is.null(regions)) {
      stop(sprintf(paste0(
        "Page `%s` does not have available regions. Please ",
        "check the `regions` column in the `modules` ",
        "dataframe.", id
      )))
    }
    avail_scale_combinations <- page$avail_scale_combinations[[1]]
    mzp <- get_from_globalenv(sprintf("mzl_%s", avail_scale_combinations[1]))

    main_dropdown_title <- page$main_dropdown_title
    default_year <- page$dates[[1]]
    default_year <- if (is.null(default_year)) NULL else max(default_year)
    vars_right <- page$var_right[[1]]
    stories <- get_from_globalenv("stories", stop_if_missing = FALSE)

    map_zoom <- get_from_globalenv("map_zoom")
    map_loc <- get_from_globalenv("map_loc")
    tileset_prefix <- get_from_globalenv("tileset_prefix")
    map_token <- get_from_globalenv("map_token")
    map_base_style <- get_from_globalenv("map_base_style")
    mapbox_username <- get_from_globalenv("mapbox_username")

    # Initiate the map.
    output[[shiny::NS(id, "map_ph")]] <- shiny::renderUI({
      cc.map::map_input(
        map_ID = shiny::NS(id, shiny::NS(id, "map")),
        username = mapbox_username,
        token = map_token,
        longitude = map_loc[["lat"]],
        latitude = map_loc[["lon"]],
        zoom = map_zoom,
        map_style_id = map_base_style,
        tileset_prefix = tileset_prefix,
        stories = stories,
        stories_min_zoom = 13
      )
    })

    # Initial zoom string reactive value
    rv_zoom_string <- shiny::reactiveVal(
      zoom_get_string(
        zoom = map_zoom,
        zoom_levels = mzp
      )
    )

    # Zoom and POI reactives when the view state of the map changes.
    shiny::observeEvent(map_viewstate(),
                        {
                          r[[id]]$zoom(zoom_get(zoom = map_viewstate()$zoom))
                          r[[id]]$poi(update_poi(
                            id = id, poi = r[[id]]$poi(),
                            map_viewstate = map_viewstate()
                          ))
                        },
                        ignoreInit = TRUE
    )

    # Right variable / compare panel
    var_right <- compare_server(
      id = id,
      r = r,
      var_list = shiny::reactive(dropdown_make(
        vars = vars_right,
        compare = TRUE
      )),
      # If there are no time in the page, use the latest census for date of
      # comparisons
      time = if (r[[id]]$time() != "") r[[id]]$time else shiny::reactive(2021),
      show_panel = shiny::reactive(FALSE)
    )

    # Region and zoom levels change depending on the geography widget
    zl <- geography_server(
      id = id,
      r = r,
      var_right = var_right,
      regions = regions,
      avail_scale_combinations = avail_scale_combinations
    )

    update_region(id = id, r = r, new_region = shiny::reactive(zl()$region))
    update_zoom_levels(id = id, r = r, new_zl = shiny::reactive(zl()$zoom_levels))

    # Zoom string reactive
    shiny::observe({
      rv_zoom_string({
        zoom_get_string(
          zoom = r[[id]]$zoom(),
          zoom_levels = r[[id]]$zoom_levels()
        )
      })
    })

    # Update selected ID
    update_select_id(id = id, r = r, data = data)

    # Choose tileset
    tile <- zoom_server(
      id = id,
      r = r,
      zoom_string = rv_zoom_string,
      region = r[[id]]$region,
      zoom_levels = r[[id]]$zoom_levels,
      hide_if_one_zoom_level = shiny::reactive({
        # If there is one scale combinations that only includes ONE scale,
        # allow the mechanic to hide the zoom div if that scale combination
        # is active.
        sac <- single_scales_combination(avail_scale_combinations)
        all(!grepl("_", sac))
      })
    )

    # Get scale
    shiny::observeEvent(
      {
        tile()
        rv_zoom_string()
      },
      {
        r[[id]]$scale(update_scale(
          tile = tile(),
          zoom_string = rv_zoom_string()
        ))
      }
    )

    # Construct the left-hand UIs / servers automatically
    autovars <-
      autovars_server(
        id = id,
        r = r,
        main_dropdown_title = main_dropdown_title,
        default_year = default_year,
        time_div_label = shiny::reactive("Scenario"),
        time_div_icon = shiny::reactive("timeline"),
        compare_label = shiny::reactive("Compare scenarios")
      )

    var_left <- shiny::reactive(autovars()$var)
    widget_time <- shiny::reactive(if (is.null(autovars()$time)) "" else autovars()$time)

    # Update the `r[[id]]$vars` reactive
    update_vars(
      id = id, r = r, var_left = var_left,
      var_right = var_right, widget_time = widget_time
    )

    # Sidebar
    sidebar_server(id = id, r = r)

    # Data
    data <- shiny::reactive(data_get(
      vars = r[[id]]$vars(),
      scale = r[[id]]$scale(),
      region = r[[id]]$region(),
      time = r[[id]]$time(),
      schemas = r[[id]]$schemas()
    ))

    # Data for tile coloring
    data_colours <- shiny::reactive(data_get_colours(
      vars = r[[id]]$vars(),
      region = r[[id]]$region(),
      time = r[[id]]$time(),
      zoom_levels = r[[id]]$zoom_levels(),
      schemas = r[[id]]$schemas()
    ))

    # Warn user
    warnuser_server(
      id = id,
      r = r,
      vars = r[[id]]$vars,
      time = r[[id]]$time,
      widget_time = widget_time,
      data = data
    )

    # Legend
    legend_server(
      id = id,
      r = r,
      vars = r[[id]]$vars,
      data = data,
      scale = r[[id]]$scale,
      time = r[[id]]$time
    )

    # Did-you-know panel
    dyk_server(
      id = id,
      r = r,
      vars = r[[id]]$vars,
      scale = r[[id]]$scale,
      region = r[[id]]$region,
      select_id = r[[id]]$select_id,
      time = r[[id]]$time,
      poi = r[[id]]$poi,
      zoom_levels = r[[id]]$zoom_levels
    )

    # Update map in response to variable changes or zooming
    map_viewstate <- map_js_server(
      id = id,
      r = r,
      tile = tile,
      select_id = r[[id]]$select_id,
      coords = r[[id]]$coords,
      zoom = r[[id]]$zoom,
      vars = r[[id]]$vars,
      # NDS: data_colours is going to need to be updated to work with `time`
      data_colours = data_colours,
      stories = stories
    )

    # Explore panel
    explore_server(
      id = id,
      r = r,
      data = data,
      region = r[[id]]$region,
      vars = r[[id]]$vars,
      scale = r[[id]]$scale,
      select_id = r[[id]]$select_id,
      time = r[[id]]$time,
      zoom_levels = r[[id]]$zoom_levels,
      schemas = r[[id]]$schemas,
      table_fun = shiny::reactive(explore_text_bill44)
    )

    # Bookmarking
    bookmark_server(
      id = id,
      r = r,
      select_id = r[[id]]$select_id,
      map_viewstate = map_viewstate
    )

    # Change view
    panel_view_server(
      id = id,
      r = r,
      region = r[[id]]$region,
      scale = r[[id]]$scale,
      vars = r[[id]]$vars,
      data = data,
      zoom_levels = r[[id]]$zoom_levels,
      time = r[[id]]$time,
      schemas = r[[id]]$schemas
    )
  })
}
