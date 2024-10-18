#' Creates a UI label element with an icon and a title
#'
#' This function generates a UI element for a Shiny application. The output
#' is a div with two child divs. One contains a Material Design icon, the
#' other contains the 'Select an indicator' title.
#'
#' @param id A character string specifying the id attribute for the outer div.
#'
#' @return A shiny::div object that includes the specified icon and title.
#' @export
label_indicators <- function(id) {
  shiny::div(
    id = id,
    class = "shiny-split-layout sidebar-section-title",
    shiny::div(
      style = "width: 9%",
      icon_material_title("tune")
    ),
    shiny::div(
      style = "width: 88%",
      cc_t_span("Indicator")
    )
  )
}


#' Retrieve Group Names for a Specific Module
#'
#' This function retrieves the group names associated with a specific module ID.
#' The group names are retrieved from a global environment object named 'modules'.
#' They are used to populate the main dropdown.
#'
#' @param id <`character`> Identifying the module in modules$id
#' @param pres <`logical`> If TRUE, will return a logical on if the there should
#' be a widgets existing at all. Defaults to FALSE, which returns the default
#' return of this function.
#'
#' If 'var_lefts' (the variables left to be selected) is a data frame,
#' it returns the unique group names from the 'group_name' column.
#'
#' If 'var_lefts' is a character vector, it returns a fully prepared dropdown list.
#'
#' @return A character vector of group names or a list prepared for a dropdown selection,
#' depending on the type of 'var_lefts'.
autovars_groupnames <- function(id, pres = FALSE) {
  modules <- get_from_globalenv("modules")

  var_lefts <- modules$var_left[modules$id == id][[1]]

  # If it is a dataframe. Just supply the group name as a character vector.
  if (is.data.frame(var_lefts)) {
    # If want to know about presence of a widget, return TRUE
    if (pres) {
      return(TRUE)
    }
    options <- unique(var_lefts$group_name)
    options <- options[order(options)]
    return(options)
  }

  # If want to know about presence of a widget,
  if (pres) {
    if (length(var_lefts) == 1) {
      return(FALSE)
    }
    return(TRUE)
  }

  # If it's a character vector, supply a full dropdown list
  return(dropdown_make(vars = var_lefts))
}

#' Get the Common Widgets for a Specific Module ID
#'
#' This function retrieves the common widgets (time and other widgets)
#' associated with a specific module ID. The information is retrieved from
#' global environment objects named 'modules' and 'variables'.
#'
#' @param id <`character`> Identifying the module in modules$id
#'
#' If 'var_lefts' (the variables left to be selected) is a data frame,
#' it retrieves the variable list from the 'var_code' column.
#'
#' It retrieves time information from 'variables' object and other widgets
#' by subsetting the 'group_diff' column of the 'variables' object.
#'
#' @return A list with two components. The first component is a numeric vector
#' of unique time points, and the second component is a list of other widgets.
#' The list of other widgets will be empty if no other widgets are common to all variables.
autovars_common_widgets <- function(id) {
  modules <- get_from_globalenv("modules")
  variables <- get_from_globalenv("variables")

  # Get the list of variables
  var_list <- modules$var_left[modules$id == id][[1]]

  # If it is a dataframe. get the variable list by subsetting the `var_code` column
  if (is.data.frame(var_list)) {
    var_list <- var_list$var_code
  }

  # Unique time, keep names if there are
  poss_dates <- variables$dates[variables$var_code %in% var_list]
  time <- unlist(poss_dates)
  unique_time <- unique(time)
  names(unique_time) <- names(time)[match(unique_time, time)]
  time <- unique_time
  # If at least one is already numeric, then convert all to numeric. This
  # prevents from switching a slider text time slider to go numeric and ignore
  # such as "000" to convert it to 0.
  if (is.null(names(time))) {
    time <- as.numeric(time)
  }

  # Keep track of if every variable is only available at a single time. If it's
  # the case, we will just hide the time widget.
  single_time <- all(lengths(poss_dates) == 1)
  attr(time, "single_time") <- single_time

  # What happens when there is no time?

  # Other widgets
  tb <- modules$var_left[modules$id == id][[1]]
  # Fish for other widgets only when `tb` is a list
  if (is.list(tb)) {
    groups <- variables$group_diff[variables$var_code %in% var_list]
    wdg_names <- lapply(groups, names) |>
      unlist() |>
      unique()

    widgets <- sapply(wdg_names, \(x) {
      # All entries that need this widget
      values <- lapply(groups, `[[`, x)
      values <- values[!sapply(values, is.null)]

      # Does ALL variable need this widget?
      if (length(values) != length(groups)) {
        return(NULL)
      }

      # Grab the unique values. If it's a numeric or factor (slider), output
      # it with a class
      values <- unique(values)
      unlisted_vals <- unlist(values)

      if (!is.null(attr(values[[1]], "levels"))) {
        out <- attr(values[[1]], "levels")
        # If the class slider is appended to the values, make it a slider
        if (!is.null(attr(values[[1]], "class")) & attr(values[[1]], "class") == "slider") {
          return(structure(out, class = "slider_text"))
        }
        return(out)
      }

      if (all(is_numeric(unlisted_vals))) {
        return(structure(as.numeric(unlisted_vals), class = "slider"))
      }

      return(unlisted_vals)
    },
    simplify = FALSE, USE.NAMES = TRUE
    )

    widgets <- widgets[!sapply(widgets, is.null)]

    return(list(time = time, widgets = widgets))
  }

  # When tb is a character vector, just return `time` and an empty widgets list
  return(list(time = time, widgets = list()))
}

#' Get the Widgets for a Specific Module ID and Group Name
#'
#' This function retrieves the widgets associated with a specific module ID and
#' group name. The widgets are retrieved from a global environment object named
#' 'modules'. Only widgets that share the same values as the common widgets are
#' returned.
#'
#' @param id <`character`> Identifying the module in modules$id
#' @param group_name <`character`> String identifying the group name to only filter
#' the widgets of the same group.
#' @param common_vals <`named list`> A list of common widget values. Usually
#' `common_vals()` from the autovars module.
#'
#' @return A list of widgets. If `tb` is not a data frame, an empty list is returned.
autovars_widgets <- function(id, group_name, common_vals) {
  modules <- get_from_globalenv("modules")

  # Grab the correct tibble
  tb <- modules$var_left[modules$id == id][[1]]

  # If `tb` is not a dataframe, return an empty list. There are no additional
  # widgets to be added.
  if (!is.data.frame(tb)) {
    return(list())
  }

  # TKTKTK If it is to ever crash, return no additional widgets (must be a temporary
  # thing only and the module will rise back)
  tryCatch(
    {
      # Grab the difference between the variables
      groups <- tb$group_diff[tb$group_name == group_name]

      # Filter in only the groups that share the common widgets' values
      if (!is.null(common_vals) & !is.null(groups)) {
        share_common_values_index <- sapply(groups, \(g) {
          # For all the values in `common_vals`, which are the same as the observed
          # row?
          share_common_values <- sapply(names(common_vals), \(cv) {
            same_feat_val <- g[names(g) == cv]

            if ("levels" %in% names(attributes(same_feat_val[[1]]))) {
              f_lvl <- which(attributes(same_feat_val[[1]])$levels == common_vals[cv])
              return(unlist(same_feat_val) == f_lvl)
            }
            # IF THERE ARE LEVELS, THEN THE OUTPUT IS NUMERIC (USING THE LEVEL)
            common_vals[cv] %in% unlist(same_feat_val)
          })

          # If ALL the values are the same
          all(share_common_values)
        })

        # Filter in only the observations sharing the values of the common values
        groups <- groups[share_common_values_index]
      }

      # Return no additional widgets if groups has a length of zero
      if (length(groups) == 0) {
        return(list())
      }

      # Filter out groups that are already part of the common widgets
      groups <- lapply(groups, \(gr) gr[!names(gr) %in% names(common_vals)])
      groups <- unlist(groups)

      widgets <- sapply(unique(names(groups)), \(n) {
        options <- unique(unname(groups[names(groups) == n]))
        options <- options[order(options)]
        # If there is a total, put it first
        if ("Total" %in% options) options <- c("Total", options[options != "Total"])
        options
      }, simplify = FALSE, USE.NAMES = TRUE)

      return(widgets)
    },
    error = function(e) {
      return(list())
    }
  )
}

#' Determine Final Value
#'
#' This function returns a final value based on an id, group name, selected values
#' from a picker, and a previously selected variable. It operates in the context
#' of a global object named 'modules'.
#'
#' @param id <`character`> Identifying the module in modules$id
#' @param group_name <`character`> String identifying the group name to only filter
#' the widgets of the same group.
#' @param picker_vals <`list`> Selected values from pickers and sliders in a list.
#' Used to check which of the 'group_diff' values are included.
#' @param previous_var <`character`> A previously selected variable. It's returned
#' if the tibble is not a dataframe or there are no fits for the picker values.
#'
#' @return If the retrieved tibble is not a data frame, the function simply returns
#' the group name (if it isn't null), or the previous variable. If the tibble is a
#' data frame, the function returns the 'var_code' associated with
#' the maximum sum of fits.
autovars_final_value <- function(id, group_name, picker_vals, previous_var) {
  modules <- get_from_globalenv("modules")

  # Grab the correct tibble
  tb <- modules$var_left[modules$id == id][[1]]

  # If it's not a dataframe, then the output value is simply the choice of the
  # first dropdown.
  if (!is.data.frame(tb)) {
    return(if (is.null(group_name)) previous_var else group_name)
  }

  # Grab the difference between the variables
  groups <- tb$group_diff[tb$group_name == group_name]
  var_codes <- tb$var_code[tb$group_name == group_name]

  # Keep only the groups that are the same length as the picked values
  if (length(groups) == 0) {
    return(previous_var)
  }
  # Which of the groups have the right amount of options?
  right_amount_ind <- which(sapply(groups, \(x) length(x) == length(picker_vals)))
  groups <- groups[right_amount_ind]
  var_codes <- var_codes[right_amount_ind]
  if (all(lengths(groups) == 0)) {
    return(previous_var)
  }

  # Which
  ordered_val_fit <- mapply(\(val, i) {
    sapply(groups, \(x) {
      v <- x[[i]]
      if (!is.null(attr(v, "levels"))) v <- attr(v, "levels")[[v]]
      return(val %in% v)
    })
  }, picker_vals, seq_along(picker_vals))
  if (length(ordered_val_fit) == 0) {
    return(previous_var)
  }
  sum_fits <- rowSums(as.data.frame(ordered_val_fit))
  if (length(sum_fits) == 0) {
    return(previous_var)
  }
  # Is the one that matches, matches EVERYTHING?
  if (length(picker_vals) != max(sum_fits)) {
    return(previous_var)
  }
  out <- var_codes[which.max(sum_fits)]

  # Return()
  return(out)
}

#' Generate Placeholder Variable
#'
#' This function retrieves the first variable of a specified module.
#' The module is selected by its ID from a global object named 'modules'.
#'
#' @param id <`character`> Identifying the module in modules$id#'
#'
#' @return If the 'var_left' of the module is a data frame, the function returns
#' the first element of the 'var_code' column. If 'var_left' is a character
#' vector, the function returns its first element.
autovars_placeholder_var <- function(id) {
  modules <- get_from_globalenv("modules")

  # Grab the default variable
  default_var <- modules$default_var[modules$id == id]

  # Return it
  return(default_var)
}

#' Create UI for time variable selection
#'
#' This function creates a user interface element for selecting time variables.
#' It allows for the creation of either a slider or
#' a slider with text, depending on the structure of the time data provided.
#'
#' @param id <`character`> The namespace ID of the page.
#' @param times <`vector`> Vector of times (Usually common_widgets$time())
#' @param widget_ns <`function`> A function for namespacing widget IDs.
#' @param time_div_label <`character`> A function that returns the label for the
#' time division.
#' @param time_div_icon <`character`> Material ion name for the time selection UI, defaults
#' to "date_range".
#' @param compare_label <`character`> Label of the checkbox used to compare dates
#'
#' @return <`shiny.tagList`> A tag list containing the UI elements for the
#' time variable selection.
#' @export
autovars_time_ui <- function(id, times, widget_ns,
                             time_div_label = "Time",
                             time_div_icon = "date_range",
                             compare_label = "Compare dates") {

  # Is it named? If so, slider text must be used.
  slider_type <- if (is.null(names(times))) "slider" else "slider_text"

  if (slider_type == "slider") {
    min_ <- times |> min()
    max_ <- times |> max()
    step_ <- abs(unique(diff(times))[1])
    double_value_ <- times[ceiling(length(times) / 2)]
    double_value_ <- c(double_value_, max_)
    length_ <- times |> length()
    # If there are less than 10 options, slim sliders
    ys_classes <- if (length_ < 10) "year-slider-slim" else "year-slider"

    sliders <- shiny::tagList(
      slider_UI(
        id = widget_ns(id), slider_id = "slu", min = min_, max = max_,
        step = step_, label = NULL
      ),
      shinyjs::hidden(slider_UI(
        id = widget_ns(id), slider_id = "slb", min = min_, max = max_,
        step = step_, label = NULL,
        value = double_value_
      )
      )
    )
  }

  if (slider_type == "slider_text") {
    dates_names <- names(times)

    sliders <- shiny::tagList(
      slider_text_UI(
        id = widget_ns(id), slider_text_id = "slu", label = NULL,
        choices = dates_names, selected = dates_names[1]
      ),
      shinyjs::hidden(slider_text_UI(
        id = widget_ns(id), slider_text_id = "slb", label = NULL,
        choices = dates_names, selected = dates_names[1:2]
      ))
    )
  }

  full_out <- shiny::tagList(
    shiny::div(
      id = widget_ns("year_sliders"),
      # class = ys_classes,
      shiny::hr(id = widget_ns("above_year_hr")),
      shiny::div(
        class = "shiny-split-layout sidebar-section-title",
        shiny::div(
          style = "width: 9%",
          icon_material_title(time_div_icon)
        ),
        shiny::div(
          style = "width: 24%",
          shiny::tags$span(
            id = shiny::NS(id, "year_label"),
            cc_t(time_div_label, force_span = TRUE)
          )
        ),
        shiny::div(
          id = widget_ns("compare_dates"),
          style = "width: 64%; margin:0px !important; text-align: right;",
          checkbox_UI(
            id = widget_ns(id),
            label = cc_t(compare_label, force_span = TRUE),
            value = FALSE
          )
        )
      ),
      sliders
    )
  )

  return(full_out)

}
