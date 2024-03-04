#' Auto-vars module
#'
#' This Shiny module allows for the automatic generation and control of pickers and
#' sliders within Curbcut. It uses the entries in the `modules` table to draw
#' the necessary pickers and sliders. It prepares global settings, creates
#' common widgets, handles their reactivity, and sets up the main dropdown.
#' It also updates the UI dynamically based on selected values, and finally,
#' generates the final variable code using all of these values.
#'
#' @param id <`character`> The ID of the page in which the legend will appear,
#' e.g. `alp`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param main_dropdown_title <`character`> A character string to be used as the
#' title of the main dropdown selector.
#' @param default_year <`numeric`> An optional numeric value specifying the default
#' year for time widgets. If not provided, these widgets will not be initialized.
#' @param time_div_label <`reactive character`> The label of the time div. Defaults
#' to `Time`.
#' @param time_div_icon <`reactive character`> Material ion name for the time
#' selection UI, defaults to "date_range".
#' @param compare_label <`reactive character`> The label of the compare checkbox.
#' Defaults to `Compare dates`.
#'
#' @return A reactive list with the final variable ('var') and the selected
#' time ('time').
#' @export
autovars_server <- function(id, r, main_dropdown_title, default_year,
                            time_div_label = shiny::reactive("Time"),
                            time_div_icon = shiny::reactive("date_range"),
                            compare_label = shiny::reactive("Compare dates")) {
  shiny::moduleServer(id, function(input, output, session) {
    # Global preparation ------------------------------------------------------

    # Selector function. Retrieve the namespace function associated with the
    # current module's session. Add a '#' to use it as a selector
    html_ns <- function(css_id) {
      sprintf("#%s", session$ns(css_id))
    }
    widget_ns <- session$ns
    additional_picker_count <- shiny::reactiveVal(0)

    # Some widgets must be placed in the advanced controls
    page <- page_get(id)
    adv <- unlist(page$add_advanced_controls)
    schemas <- names(page$additional_schemas[[1]])

    default_var <- autovars_placeholder_var(id = id)
    out_var <- shiny::reactiveVal(default_var)

    # Make a reactiveVal to keep IDs of schema widgets
    schema_reactive <- shiny::reactiveVal(NULL)

    advanced_div_selector <- html_ns("indicators_label-advanced_controls_div")

    # Common widgets ----------------------------------------------------------

    common_widgets <- shiny::reactiveVal(autovars_common_widgets(id = id))

    # If some widgets are alike in all the variables to be picked through,
    # make these widgets in top in priority. e.g. `time`
    shiny::observe({
      # Time widgets, if there are date times
      shiny::insertUI(
        # selector = html_ns("autovars"),
        # where = "beforeEnd",
        # place after the compare panel
        selector = html_ns("compare_panel"),
        where = "afterEnd",
        ui = {
          if (!is.null(default_year)) {
            autovars_time_ui(id = id,
                             times = common_widgets()$time,
                             widget_ns = widget_ns,
                             time_div_label(), time_div_icon(), compare_label())
          }
        }
      )
    })
    shiny::observe({
      raw_wdgs <- common_widgets()$widgets

      # Other widgets that are common between all groups, if there are any
      if (length(raw_wdgs) > 0) {
        # Remove the div if it was already present (when language changes, it gets redrawn)
        shiny::removeUI(selector = html_ns("common_widgets_in"))
        shiny::removeUI(selector = html_ns("common_widgets_in_adv"))

        # Grab the default selections
        tb <- page$var_left[[1]]
        default_selection <- tb$group_diff[tb$var_code == default_var][[1]]

        # Common widgets UIs function. wdgs are the widgest to add, ns is
        # the name of the div
        UIs <- \(wdgs, ns) {
          shiny::div(
            id = widget_ns(ns),
            do.call(shiny::tagList, mapply(
              function(w, n) {
                # Keep the order!
                l <- which(names(raw_wdgs) == n)

                maybe_schema <- tolower(n)
                maybe_schema <- gsub(" ", "", maybe_schema)

                ## ADD HERE, FLAG THE ID OF WHAT WILL BE SCHEMA???????

                if ("slider_text" %in% class(w)) {
                  selected <- w[[default_selection[[n]]]]
                  w_id <- sprintf("st%s", l)
                  if (maybe_schema %in% schemas) {
                    cur <- shiny::isolate(schema_reactive())
                    full_id <- sprintf("ccpicker_%s", w_id)
                    schema_reactive(c(cur, stats::setNames(full_id, maybe_schema)))
                  }

                  slider_text_UI(
                    id = widget_ns(id),
                    slider_text_id = w_id,
                    choices = w,
                    selected = selected,
                    label = cc_t(n, force_span = TRUE)
                  )
                } else if ("slider" %in% class(w)) {
                  w_id <- sprintf("s%s", l)
                  if (maybe_schema %in% schemas) {
                    cur <- shiny::isolate(schema_reactive())
                    full_id <- sprintf("ccslider_%s", w_id)
                    schema_reactive(c(cur, stats::setNames(full_id, maybe_schema)))
                  }

                  vals <- unlist(w)
                  vals <- as.numeric(vals)
                  min_ <- min(vals)
                  max_ <- max(vals)
                  step_ <- unique(diff(vals))[1]
                  selected <- default_selection[[n]]

                  if (length(selected) > 1) {
                    # Special case for accss
                    if (all(vals == which(1:60 %% 5 == 0))) {
                      selected <- 20
                    } else {
                      selected <- vals[{
                        (length(vals)) / 2
                      } |> floor()]
                    }
                  }

                  slider_UI(
                    id = widget_ns(id),
                    slider_id = w_id,
                    step = step_,
                    min = min_,
                    max = max_,
                    value = selected,
                    label = cc_t(n, force_span = TRUE)
                  )
                } else {
                  w_id <- sprintf("p%s", l)
                  if (maybe_schema %in% schemas) {
                    cur <- shiny::isolate(schema_reactive())
                    full_id <- sprintf("ccslidertext_%s", w_id)
                    schema_reactive(c(cur, stats::setNames(full_id, maybe_schema)))
                  }

                  names(w) <- w
                  names(w) <- sapply(names(w), cc_t, lang = r$lang())
                  selected <- default_selection[[n]]

                  picker_UI(
                    id = widget_ns(id),
                    picker_id = w_id,
                    var_list = w,
                    label = cc_t(n, force_span = TRUE),
                    selected = selected
                  )
                }
              }, wdgs, names(wdgs),
              SIMPLIFY = FALSE
            ))
          )
        }

        # The Check for the widgets that should be placed in the advanced
        # options div. Filter them out of wdgs after.
        if (length(adv) != 0) {
          wdgs_adv <- raw_wdgs[names(raw_wdgs) %in% adv]
          other_wdgs <- raw_wdgs[!names(raw_wdgs) %in% adv]

          if (length(wdgs_adv) != 0) {
            shiny::insertUI(
              selector = advanced_div_selector,
              where = "beforeEnd",
              ui = UIs(wdgs_adv, ns = "common_widgets_in_adv")
            )
          }
        } else {
          other_wdgs <- raw_wdgs
        }

        # Place the rest of the wdgs at their spot
        shiny::insertUI(
          selector = html_ns("indicators_label-common_widgets"),
          where = "afterBegin",
          ui = UIs(other_wdgs, ns = "common_widgets_in")
        )
      }
    })
    # Grab the time values
    # If numeric time
    slider_uni_num <- slider_server(id = id, slider_id = "slu")
    slider_bi_num <- slider_server(id = id, slider_id = "slb")
    # If character time
    slider_uni_text <- slider_text_server(id = id, slider_text_id = "slu")
    slider_bi_text <- slider_text_server(id = id, slider_text_id = "slb")
    slider_switch <- checkbox_server(
      id = id, r = r,
      label = shiny::reactive("Compare dates")
    )
    # Enable or disable first and second slider
    shiny::observeEvent(slider_switch(), {
      single_year <- length(common_widgets()$time) == 1
      # Slider numeric
      shinyjs::toggle(shiny::NS(id, "ccslider_slu"), condition = !slider_switch() & !single_year)
      shinyjs::toggle(shiny::NS(id, "ccslider_slb"), condition = slider_switch() & !single_year)
      # Slider text
      shinyjs::toggle(shiny::NS(id, "ccslidertext_slu"), condition = !slider_switch() & !single_year)
      shinyjs::toggle(shiny::NS(id, "ccslidertext_slb"), condition = slider_switch() & !single_year)
      # Checkbox
      shinyjs::toggle(shiny::NS(id, "cccheckbox_cbx"), condition = !single_year)

      # Hide the whole div if there's only one year of data
      shinyjs::toggle("year_sliders", condition = !single_year)

      # If there's a single year or there are no common widgets, or if there are
      # no main widgets.
      main_widgets <- autovars_groupnames(id = id, pres = TRUE)

      # On slider switch event, change the label
      if (slider_switch()) {
        shinyjs::html(shiny::NS(id, "year_label"), "Select two years")
      } else {
        shinyjs::html(shiny::NS(id, "year_label"), "Select a year")
      }
    })

    # Grab the right time
    time <- shiny::reactive({
      # In the case the UIs are not initiated.
      if (is.null(default_year)) {
        return(NULL)
      }
      if (is.null(slider_switch())) {
        return(default_year)
      }

      # Which type of slider?
      times <- common_widgets()$time
      slider_type <- if (is.null(names(times))) "slider" else "slider_text"

      # If it's a numeric slider
      if (slider_type == "slider") {
        tm <- if (slider_switch()) slider_bi_num() else slider_uni_num()
      }

      # If it's a character slider
      if (slider_type == "slider_text") {
        # The selected time must be the 'real' value (the one selected by the
        # user), not the pretty front-facing one (the name of the dates vector).
        slider_val <- if (slider_switch()) slider_bi_text() else slider_uni_text()
        tm <- times[which(names(times) %in% slider_val)]
      }

      return(tm)
    })

    # Grab the common widgets' value
    common_vals <- shiny::reactive({
      common_vals <- list()
      for (i in seq_along(common_widgets()$widgets)) {
        picker_id <- sprintf("ccpicker_p%s", i)
        val <- input[[shiny::NS(id, picker_id)]]
        if (!is.null(val)) common_vals[[i]] <- val
      }
      for (i in seq_along(common_widgets()$widgets)) {
        slider_id <- sprintf("ccslider_s%s", i)
        val <- input[[shiny::NS(id, slider_id)]]
        if (!is.null(val)) common_vals[[i]] <- val
      }
      for (i in seq_along(common_widgets()$widgets)) {
        slider_text_id <- sprintf("ccslidertext_st%s", i)
        val <- input[[shiny::NS(id, slider_text_id)]]
        if (!is.null(val)) common_vals[[i]] <- val
      }

      if (length(common_vals) > 0) names(common_vals) <- names(common_widgets()$widgets)
      return(unlist(common_vals))
    })

    # Main dropdown -----------------------------------------------------------

    trigger_hide <- shiny::reactiveVal(NULL)

    # Draw and get value from the first dropdown
    shiny::observe({
      shiny::removeUI(selector = html_ns("main_drop"))

      # If `mnd` is in the list of widgets that should be placed in the advanced
      # options, switch selector
      if (length(adv) != 0 && "mnd" %in% adv) {
        selector <- advanced_div_selector
        where <- "beforeEnd"
      } else {
        selector <- html_ns("indicators_label-common_widgets")
        where <- "beforeEnd"
      }

      shiny::insertUI(
        selector = selector,
        where = where,
        ui = {
          if (is.na(main_dropdown_title)) main_dropdown_title <- NULL
          # Translate the content of the dropdown
          w <- autovars_groupnames(id = id)

          # If the list is of length one, do not keep it as a list, so we can
          # remove the dropddown title.
          if (!is.list(w) || length(w) == 1) {
            w <- {
              w <- unname(w)
              w <- unlist(w)
              if (is.null(names(w))) {
                names(w) <- w
              }
              names(w) <- sapply(names(w), cc_t, lang = shiny::isolate(r$lang()))
              w
            }
          } else {
            w <- shiny::isolate(cc_t(w, lang = r$lang()))
          }

          # Default selection
          tb <- page$var_left[[1]]
          default_selection <- if (is.data.frame(tb)) {
            tb$group_name[tb$var_code == r[[id]]$var_left_force()]
          } else {
            r[[id]]$var_left_force()
          }

          shinyjs::hidden(shiny::div(
            id = widget_ns("main_drop"),
            picker_UI(
              id = widget_ns(id),
              picker_id = "mnd",
              var_list = w,
              selected = default_selection,
              label = if (is.null(main_dropdown_title)) NULL else cc_t_span(main_dropdown_title)
            )
          ))
        }
      )
    })

    # Grab the main dropdown's info
    mnd_list <- autovars_groupnames(id = id)
    # If it's a list already formated using `dropdown_make(), use it as is. If
    # not, format it for the picker updates.
    mnd_list <- (\(x) {
      if (is.list(mnd_list)) {
        mnd_list
      } else {
        mnd_list_list <- list(mnd_list)
        # picker server can ONLY take a list. The list must be named. In the case
        # there is to be no label on the dropdown, return mnd_list as a list of
        # all the choices. If not, name the dropdown according to main_drop_title.
        if (!is.null(main_dropdown_title)) {
          if (is.na(main_dropdown_title)) return(sapply(mnd_list, list))
          names(mnd_list_list) <- main_dropdown_title
        }
        mnd_list_list
      }
    })()

    mnd <- picker_server(
      id = id, r = r, picker_id = "mnd",
      var_list = shiny::reactive(mnd_list),
      # Use `time` from little `r` to inform if options of the dropdown should
      # be greyed out.
      time = r[[id]]$time
    )

    # Detect the variables that are under the main dropdown value. Only update them
    # if there are actual changes in the new widgets (not only when common_vals change)
    widgets <- shiny::reactiveVal(list())
    shiny::observe({
      new_widgets <- autovars_widgets(
        id = id, group_name = mnd(),
        common_vals = common_vals()
      )

      if (!identical(widgets(), new_widgets)) {
        return(widgets(new_widgets))
      }
    })

    # If there's only one option in the var_left, hide it
    shiny::observeEvent(mnd(),
      {
        modules <- get_from_globalenv("modules")
        var_lefts <- modules$var_left[modules$id == id][[1]]

        hide <- is.character(var_lefts) & length(var_lefts) == 1

        shinyjs::toggle(id = "main_drop", condition = !hide)
        shinyjs::toggle(id = "hr_compare_panel", condition = !hide)
        shinyjs::toggle(id = "indicators_label-indicator_label", condition = !hide)
      },
      ignoreInit = TRUE
    )

    # Additional widgets ------------------------------------------------------

    # Show or hide the advanced controls with the checkbox
    label_indicators_server(id = "indicators_label", r = r)

    shiny::observe({
      # Remove the content of the previous div
      shiny::removeUI(selector = html_ns("additional_widgets"))

      # If there are additional widgets, and the common number of widgets is 3
      # or less, placed them in advanced controls.
      advanced_controls_avail <- (length(widgets()) + length(adv) > 2)
      if (!advanced_controls_avail) {
        selector <- html_ns("indicators_label-common_widgets")
        where <- "beforeEnd"
      } else {
        selector <- advanced_div_selector
        where <- "beforeEnd"
      }
      # Show and hide the advanced div checkbox
      shinyjs::toggle("indicators_label-cb_adv_opt_div", condition = advanced_controls_avail)

      if (length(widgets()) > 0) {
        tb <- page$var_left[[1]]
        default_selection <- tb$group_diff[tb$var_code == r[[id]]$var_left_force()][[1]]

        # Other widgets (dropdowns)
        shiny::insertUI(
          selector = selector,
          where = where,
          ui = {
            shiny::tags$div(
              id = widget_ns("additional_widgets"),
              do.call(shiny::tagList, mapply(
                function(w, l, n) {
                  names(w) <- w
                  names(w) <- sapply(names(w), cc_t, lang = r$lang())
                  selected <- default_selection[[n]]
                  w_id <- sprintf("p%s", l)

                  # Flag if it's part of schema
                  maybe_schema <- tolower(n)
                  maybe_schema <- gsub(" ", "", maybe_schema)

                  if (maybe_schema %in% schemas) {
                    cur <- shiny::isolate(schema_reactive())
                    full_id <- sprintf("ccpicker_%s", w_id)
                    schema_reactive(c(cur, stats::setNames(full_id, maybe_schema)))
                  }

                  picker_UI(
                    id = widget_ns(id),
                    picker_id = sprintf("p%s", l),
                    var_list = w,
                    selected = selected,
                    label = cc_t(n, force_span = TRUE)
                  )
                }, widgets(), seq_along(widgets()) + length(common_widgets()$widgets),
                names(widgets()),
                SIMPLIFY = FALSE
              ))
            )
          }
        )
      }
      # Update the number of picker values to be retrieved
      additional_picker_count(length(widgets()))
    })


    # Make the final variable -------------------------------------------------

    # Retrieve the values of ALL the pickers and sliders
    picker_vals <- shiny::reactive({
      picker_vals <- list()
      length_all <- length(common_widgets()$widgets) + additional_picker_count()
      for (i in seq_len(length_all)) {
        picker_id <- sprintf("ccpicker_p%s", i)
        val <- input[[shiny::NS(id, picker_id)]]
        if (!is.null(val)) picker_vals[[i]] <- val
      }
      for (i in seq_len(length_all)) {
        slider_id <- sprintf("ccslider_s%s", i)
        val <- input[[shiny::NS(id, slider_id)]]
        if (!is.null(val)) picker_vals[[i]] <- val
      }
      for (i in seq_len(length_all)) {
        slider_text_id <- sprintf("ccslidertext_st%s", i)
        val <- input[[shiny::NS(id, slider_text_id)]]
        if (!is.null(val)) picker_vals[[i]] <- val
      }
      return(unlist(picker_vals))
    })

    # Using all the values of pickers and sliders, get the final value
    shiny::observe({
      z <- autovars_final_value(
        id = id, group_name = mnd(),
        picker_vals = picker_vals(),
        previous_var = out_var()
      )
      out_var(z[[1]])
    })

    # Update the schemas!
    shiny::observe({
      # if (is.null(schemas)) {
      #   return(NULL)
      # }
      # if (is.null(schema_reactive())) {
      #   return(NULL)
      # }

      final_schemas <- page$additional_schemas[[1]]
      for (i in names(schema_reactive())) {
        input_name <- schema_reactive()[[i]]
        input_val <- input[[shiny::NS(id, input_name)]]
        if (is.null(input_val)) next
        final_schemas[[i]] <- input_val
      }
      # if (is.null(final_schemas)) {
      #   return(NULL)
      # }

      # Grab the 'real' time (the one the user will see) so it can accurately
      # inform the schema,
      final_schemas <- c(final_schemas, list(time = time()))
      final_schemas <- list(var_left = final_schemas)

      update_rv(
        id = id, r = r, rv_name = "schemas",
        new_val = shiny::reactive(final_schemas)
      )
    })

    return(shiny::reactive(list(var = out_var(), time = time())))
  })
}

#' @describeIn autovars_server Create the UI for the autovars module
#' @param ... UIs to be inserted in the advanced controls div.
#' @export
autovars_UI <- function(id, ...) {
  shiny::tagList(
    shiny::div(
      id = shiny::NS(id, "autovars"),
      label_indicators_UI(id = shiny::NS(id, "indicators_label"), ...),
    )
  )
}
