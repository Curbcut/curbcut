#' Generic legend label function for Curbcut legends
#'
#' `legend_labels` is a generic function used to produce a list of
#' \code{\link[ggplot2]{labs}} for the Curbcut legends. The function invokes
#' particular methods which depend on the class of the `vars` argument.
#'
#' @param vars <`named list`> A list object with a pre-determined class. The
#' output of \code{\link{vars_build}}.
#' @param lang <`character`> A character string indicating the language to
#' translate variable titles to.
#' @param ... Arguments to be passed to the methods, e.g. optionally `lang`
#'
#' @return It returns an output of \code{\link[ggplot2]{labs}}.
#' @export
legend_labels <- function(vars, lang, ...) {
  UseMethod("legend_labels", vars)
}

#' @describeIn legend_labels q5 method
#' @export
legend_labels.q5 <- function(vars, lang = NULL, ...) {
  title_left <-
    var_get_title(
      var = vars$var_left, short_treshold = 25,
      translate = TRUE, lang = lang
    )
  return(list(ggplot2::labs(x = title_left, y = NULL)))
}

#' @describeIn legend_labels q100 method
#' @export
legend_labels.q100 <- function(vars, lang = NULL, ...) {
  title_left <-
    var_get_title(
      var = vars$var_left, short_treshold = 25,
      translate = TRUE, lang = lang
    )
  return(list(ggplot2::labs(x = title_left, y = NULL)))
}

#' @describeIn legend_labels qual method
#' @export
legend_labels.qual <- function(vars, lang = NULL, ...) {
  title_left <-
    var_get_title(
      var = vars$var_left, short_treshold = 25,
      translate = TRUE, lang = lang
    )
  return(list(ggplot2::labs(x = title_left, y = NULL)))
}

#' @describeIn legend_labels bivar_ldelta_rq3 method
#' @param time <`named list`> Object built using the \code{\link{vars_build}}
#' function. It contains the time for both var_left and var_right variables.
#' @export
legend_labels.bivar_ldelta_rq3 <- function(vars, lang = NULL, time, ...) {
  title_left <-
    var_get_title(
      var = vars$var_left, short_treshold = 12,
      translate = TRUE, lang = lang
    )
  title_left <- legend_labels_delta_text(title = title_left, time_vrvl = time$var_left)

  title_right <-
    var_get_title(
      var = vars$var_right, short_treshold = 16,
      translate = TRUE, lang = lang
    )
  title_right <- paste0(title_right, " (", time$var_right, ")")

  title_left_short <- var_get_info(
    var = vars$var_left, what = "var_short",
    translate = TRUE, lang = lang
  )
  title_right_short <- var_get_info(
    var = vars$var_right, what = "var_short",
    translate = TRUE, lang = lang
  )

  return(list(ggplot2::labs(x = title_right, y = title_left),
    x_short = title_right_short, y_short = title_left_short
  ))
}

#' @describeIn legend_labels delta method
#' @export
legend_labels.delta <- function(vars, lang = NULL, time, ...) {
  title_left <-
    var_get_title(
      var = vars$var_left, short_treshold = 25,
      translate = TRUE, lang = lang
    )
  title_left <- legend_labels_delta_text(title = title_left, time_vrvl = time$var_left)

  return(list(ggplot2::labs(x = title_left, y = NULL)))
}

#' @describeIn legend_labels bivar method
#' @export
legend_labels.bivar <- function(vars, lang = NULL, ...) {
  title_left <-
    var_get_title(
      var = vars$var_left, short_treshold = 16,
      translate = TRUE, lang = lang
    )
  title_right <-
    var_get_title(
      var = vars$var_right, short_treshold = 16,
      translate = TRUE, lang = lang
    )

  title_left_short <- var_get_info(
    var = vars$var_left, what = "var_short",
    translate = TRUE, lang = lang
  )
  title_right_short <- var_get_info(
    var = vars$var_right, what = "var_short",
    translate = TRUE, lang = lang
  )

  return(list(ggplot2::labs(x = title_right, y = title_left),
    x_short = title_right_short, y_short = title_left_short
  ))
}

#' @describeIn legend_labels delta_bivar method
#' @export
legend_labels.delta_bivar <- function(vars, lang = NULL, time, ...) {
  title_left <-
    var_get_title(
      var = vars$var_left, short_treshold = 12,
      translate = TRUE, lang = lang
    )
  title_left <- legend_labels_delta_text(title = title_left, time_vrvl = time$var_left)

  title_right <-
    var_get_title(
      var = vars$var_right, short_treshold = 12,
      translate = TRUE, lang = lang
    )
  title_right <- legend_labels_delta_text(title = title_right, time_vrvl = time$var_right)

  title_left_short <- var_get_info(
    var = vars$var_left,
    what = "var_short", translate = TRUE,
    lang = lang
  )
  title_right_short <- var_get_info(
    var = vars$var_right,
    what = "var_short", translate = TRUE,
    lang = lang
  )

  return(list(ggplot2::labs(x = title_right, y = title_left),
    x_short = title_right_short, y_short = title_left_short
  ))
}

#' Generate a Legend Label with Delta Time
#'
#' This function creates a legend label that includes a delta time notation.
#' It handles both named and unnamed time variables. For unnamed time variables,
#' the label will include the delta time interval. For named time variables, it
#' will simply denote a delta without specific time.
#'
#' @param title <`character`> The title for the legend label.
#' @param time_vrvl <`numeric vector`> A numeric vector representing time.
#' This vector can be named or unnamed. If unnamed, the function will
#' concatenate the time values with a dash. If named, the specific time
#' values are disregarded, and only a delta symbol is added to the title.
#'
#' @return <`character`> A character string representing the formatted legend label.
legend_labels_delta_text <- function(title, time_vrvl) {

  # If the time is unnamed, return Title (delta time_1 - time_2)
  if (is.null(names(time_vrvl))) {
    time_binded <- paste(time_vrvl, collapse = " - ")
    out <- paste0(title, " (\u0394 ", time_binded, ")")
    return(out)
  }

  # If the time is named, then the time code is meaningless. Return Title (delta)
  paste(title, "(\u0394)")

}
