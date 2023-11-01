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
#' @export
legend_labels.bivar_ldelta_rq3 <- function(vars, lang = NULL, ...) {
  title_left <-
    var_get_title(
      var = vars$var_left, short_treshold = 12,
      translate = TRUE, lang = lang
    )
  date_left <- var_get_time(var = vars$var_left)
  date_left <- paste(date_left, collapse = " - ")
  title_left <- paste0(title_left, " (\u0394 ", date_left, ")")

  title_right <-
    var_get_title(
      var = vars$var_right, short_treshold = 16,
      translate = TRUE, lang = lang
    )
  date_right <- var_get_time(var = vars$var_right)
  title_right <- paste0(title_right, " (", date_right, ")")

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
  date_left <- paste(time$var_left, collapse = " - ")
  title_left <- paste0(title_left, " (\u0394 ", date_left, ")")
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
  date_left <- paste(time$var_left, collapse = " - ")
  title_left <- paste0(title_left, " (\u0394 ", date_left, ")")

  title_right <-
    var_get_title(
      var = vars$var_right, short_treshold = 12,
      translate = TRUE, lang = lang
    )
  date_right <- paste(time$var_right, collapse = " - ")
  title_right <- paste0(title_right, " (\u0394 ", date_right, ")")

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
