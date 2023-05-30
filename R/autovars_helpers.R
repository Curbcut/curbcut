#' Retrieve Group Names for a Specific Module
#'
#' This function retrieves the group names associated with a specific module ID.
#' The group names are retrieved from a global environment object named 'modules'.
#' They are used to populate the main dropdown.
#'
#' @param id <`character`> Identifying the module in modules$id
#'
#' If 'var_lefts' (the variables left to be selected) is a data frame,
#' it returns the unique group names from the 'group_name' column.
#'
#' If 'var_lefts' is a character vector, it returns a fully prepared dropdown list.
#'
#' @return A character vector of group names or a list prepared for a dropdown selection,
#' depending on the type of 'var_lefts'.
autovars_groupnames <- function(id) {
  modules <- get_from_globalenv("modules")

  var_lefts <- modules$var_left[modules$id == id][[1]]

  # If it is a dataframe. Just supply the group name as a character vector.
  if (is.data.frame(var_lefts)) {
    return(unique(var_lefts$group_name))
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

  # Time?
  time <- unlist(variables$dates[variables$var_code %in% var_list])
  time <- unique(time)
  time <- as.numeric(time)
  # What happens when there is no time?

  # Other widgets
  tb <- modules$var_left[modules$id == id][[1]]
  # Fish for other widgets only when `tb` is a list
  if (is.list(tb)) {
    groups <- variables$group_diff[variables$var_code %in% var_list]
    wdg_names <- names(unlist(groups)) |> unique()

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

  # Grab the difference between the variables
  groups <- tb$group_diff[tb$group_name == group_name]

  # Filter in only the groups that share the common widgets' values
  if (!is.null(common_vals)) {
    share_common_values_index <- sapply(groups, \(g) {
      # For all the values in `common_vals`, which are the same as the observed
      # row?
      share_common_values <- sapply(names(common_vals), \(cv) {
        same_feat_val <- g[names(g) == cv]
        unlist(same_feat_val) == common_vals[cv]
      })

      # If ALL the values are the same
      all(share_common_values)
    })

    # Filter in only the observations sharing the values of the common values
    groups <- groups[share_common_values_index]
  }
  groups <- unlist(groups)

  # Filter out groups that are already part of the common widgets
  groups <- groups[!names(groups) %in% names(common_vals)]

  widgets <- sapply(unique(names(groups)), \(n) {
    unique(unname(groups[names(groups) == n]))
  }, simplify = FALSE, USE.NAMES = TRUE)

  return(widgets)
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
  groups <- groups[sapply(groups, \(x) length(x) == length(picker_vals))]
  if (all(lengths(groups) == 0)) {
    return(previous_var)
  }

  # Which
  ordered_val_fit <- mapply(\(val, i) {
    sapply(groups, \(x) {
      v <- x[[i]]
      if (!is.null(attr(v, "levels"))) v <- attr(v, "levels")[[v]]
      return(v == val)
    })
  }, picker_vals, seq_along(picker_vals))
  if (length(ordered_val_fit) == 0) {
    return(previous_var)
  }
  sum_fits <- rowSums(ordered_val_fit)
  if (length(sum_fits) == 0) {
    return(previous_var)
  }
  out <- var_codes[which(sum_fits == max(sum_fits))]

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

  var_lefts <- modules$var_left[modules$id == id][[1]]

  # If it is a dataframe. Just grab the first element of the `var_code` column
  if (is.data.frame(var_lefts)) {
    return(var_lefts$var_code[[1]])
  }

  # If it's a character vector, grab the first element
  return(var_lefts[[1]])
}
