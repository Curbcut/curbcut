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
#'
#' @return A reactive list with the final variable ('var') and the selected
#' time ('time').
#' @export
autovars_server <- function(id, r, main_dropdown_title, default_year) {
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

    default_var <- autovars_placeholder_var(id = id)
    out_var <- shiny::reactiveVal(default_var)

    advanced_div_selector <- html_ns("advanced_controls_div")

    # Common widgets ----------------------------------------------------------

    common_widgets <- shiny::reactiveVal(autovars_common_widgets(id = id))

    # If some widgets are alike in all the variables to be picked through,
    # make these widgets in top in priority. e.g. `time`
    shiny::observe({
      # Time widgets, if there are date times
      shiny::insertUI(
        selector = html_ns("autovars"),
        where = "beforeEnd",
        ui = {
          if (!is.null(default_year)) {
            min_ <- common_widgets()$time |> min()
            max_ <- common_widgets()$time |> max()
            step_ <- unique(diff(common_widgets()$time))[1]
            double_value_ <- common_widgets()$time[ceiling(length(common_widgets()$time) / 2)]
            double_value_ <- c(double_value_, max_)
            length_ <- common_widgets()$time |> length()
            # If there are less than 10 options, slim sliders
            ys_classes <- if (length_ < 10) "year-slider-slim" else "year-slider"

            shiny::tagList(
              shiny::div(
                id = widget_ns("year_sliders"),
                class = ys_classes,
                shiny::hr(id = widget_ns("above_year_hr")),
                shiny::div(
                  class="shiny-split-layout sidebar-section-title",
                  shiny::div(style = "width: 9%",
                             icon_material_title("date_range")),
                  shiny::div(style = "width: 24%",
                             shiny::tags$span(id = shiny::NS(id, "year_label"),
                                              cc_t("Time", force_span = TRUE))),
                  shiny::div(style = "width: 64%; margin:0px !important; text-align: right;",
                             checkbox_UI(
                               id = widget_ns(id),
                               label = cc_t("Compare dates", force_span = TRUE),
                               value = FALSE
                             )))
                ,
                slider_UI(
                  id = widget_ns(id), slider_id = "slu", min = min_, max = max_,
                  step = step_, label = NULL
                ),
                slider_UI(
                  id = widget_ns(id), slider_id = "slb", min = min_, max = max_,
                  step = step_, label = NULL,
                  value = double_value_
                )
              )
            )
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

                if ("slider_text" %in% class(w)) {
                  selected <- w[[default_selection[[n]]]]

                  slider_text_UI(
                    id = widget_ns(id),
                    slider_text_id = sprintf("st%s", l),
                    choices = w,
                    selected = selected,
                    label = cc_t(n, force_span = TRUE)
                  )
                } else if ("slider" %in% class(w)) {
                  vals <- unlist(w)
                  vals <- as.numeric(vals)
                  min_ <- min(vals)
                  max_ <- max(vals)
                  step_ <- unique(diff(vals))[1]
                  selected <- default_selection[[n]]

                  slider_UI(
                    id = widget_ns(id),
                    slider_id = sprintf("s%s", l),
                    step = step_,
                    min = min_,
                    max = max_,
                    value = selected,
                    label = cc_t(n, force_span = TRUE)
                  )
                } else {
                  names(w) <- w
                  names(w) <- sapply(names(w), cc_t, lang = r$lang())
                  selected <- default_selection[[n]]

                  picker_UI(
                    id = widget_ns(id),
                    picker_id = sprintf("p%s", l),
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
          selector = html_ns("common_widgets"),
          where = "beforeBegin",
          ui = UIs(other_wdgs, ns = "common_widgets_in")
        )

      }
    })
    # Grab the time values
    slider_uni <- slider_server(id = id, slider_id = "slu")
    slider_bi <- slider_server(id = id, slider_id = "slb")
    slider_switch <- checkbox_server(
      id = id, r = r,
      label = shiny::reactive("Compare dates")
    )
    # Enable or disable first and second slider
    shiny::observeEvent(slider_switch(), {
      single_year <- length(common_widgets()$time) == 1
      shinyjs::toggle(shiny::NS(id, "ccslider_slu"), condition = !slider_switch() & !single_year)
      shinyjs::toggle(shiny::NS(id, "ccslider_slb"), condition = slider_switch() & !single_year)
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
      if (slider_switch()) slider_bi() else slider_uni()
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
        selector <- html_ns("common_widgets")
        where <- "afterEnd"
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
          if (!is.list(w) || length(w) == 1) w <- {
            w <- unname(w)
            w <- unlist(w)
            if (is.null(names(w))) {
              names(w) <- w
            }
            names(w) <- sapply(names(w), cc_t, lang = r$lang())
            w
          } else {
            w <- cc_t(w, lang = r$lang())
          }

          # Default selection
          tb <- page$var_left[[1]]
          default_selection <- if (is.data.frame(tb)) {
            tb$group_name[tb$var_code == default_var]
          } else {
            default_var
          }

          shiny::div(
            id = widget_ns("main_drop"),
            picker_UI(
              id = widget_ns(id),
              picker_id = "mnd",
              var_list = w,
              selected = default_selection,
              label = if (is.null(main_dropdown_title)) NULL else cc_t(main_dropdown_title, force_span = TRUE)
            )
          )
        }
      )
    })

    # Grab the main dropdown's info
    mnd_list <- autovars_groupnames(id = id)
    # If it's a list already formated using `dropdown_make(), use it as is. If
    # not, format it for the picker updates.
    mnd_list <- if (is.list(mnd_list)) {
      mnd_list
    } else {
      mnd_list <- list(mnd_list)
      names(mnd_list) <- main_dropdown_title
      mnd_list
    }
    mnd <- picker_server(
      id = id, r = r, picker_id = "mnd",
      var_list = shiny::reactive(mnd_list)
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
                          if (is.character(var_lefts) & length(var_lefts) == 1) {
                            shinyjs::hide(id = shiny::NS(id, "ccpicker_mnd"))
                            shinyjs::hide(id = "above_year_hr")
                            shinyjs::hide(id = "indicators_label")
                          }
                        },
                        ignoreInit = TRUE
    )

    # Additional widgets ------------------------------------------------------

    # Show or hide the advanced controls with the checkbox
    advanced_controls_server(id = id, r = r)

    shiny::observe({
      # Remove the content of the previous div
      shiny::removeUI(selector = html_ns("additional_widgets"))

      # If there are additional widgets, and the common number of widgets is 3
      # or less, placed them in advanced controls.
      advanced_controls_avail <- (length(widgets()) + length(adv) > 2)
      if (!advanced_controls_avail) {
        selector = html_ns("hr_additional_widgets")
        where = "afterEnd"
      } else {
        selector <- advanced_div_selector
        where <- "beforeEnd"
      }
      # Show and hide the whole advanced controls div
      shinyjs::toggle("advanced_controls", condition = advanced_controls_avail)

      if (length(widgets()) > 0) {
        tb <- page$var_left[[1]]
        default_selection <- tb$group_diff[tb$var_code == default_var][[1]]

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
      for (i in seq_along(common_widgets()$widgets)) {
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

    # If there is a `time` value, attach it to the variable
    final_var <- shiny::reactive({
      if (is.null(time())) {
        return(out_var())
      }
      picker_return_var(input = out_var(), time = time())
    })

    return(shiny::reactive(list(var = final_var(), time = time())))
  })
}

#' @describeIn autovars_server Create the UI for the autovars module
#' @export
autovars_UI <- function(id) {
  shiny::tagList(
    shiny::div(
      id = shiny::NS(id, "autovars"),
      label_indicators(id = shiny::NS(id, "indicators_label")),
      shinyjs::hidden(shiny::hr(id = shiny::NS(id, "common_widgets"))),
      shinyjs::hidden(shiny::hr(id = shiny::NS(id, "hr_additional_widgets"))),
      shinyjs::hidden(advanced_controls_UI(id = id))
    )
  )
}
