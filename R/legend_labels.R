#' Generic legend label function for Curbcut legends
#'
#' `legend_labels` is a generic function used to produce a list of
#' \code{\link[ggplot2]{labs}} for the Curbcut legends. The function invokes
#' particular methods which depend on the class of the `vars` argument.
#'
#' @param vars <`named list`> A list object with a pre-determined class. The
#' output of \code{\link{vars_build}}.
#' @param ... Arguments to be passed to the methods, e.g. optionally `lang`
#'
#' @return It returns an output of \code{\link[ggplot2]{labs}}.
#' @export
legend_labels <- function(vars, ...) {
  UseMethod("legend_labels", vars)
}

#' Get a list of label titles for Curbcut legends containing 5 breaks
#'
#' \code{legend_labels.q5} is a method for \code{\link{legend_labels}},
#' which takes a named list of variables and an optional language argument, which
#' it passes to a helper function \code{\link{var_get_info}}
#' to extract the title of the left variable. The resulting list contains a
#' label for the x-axis, with a NULL value for the y-axis.
#'
#' @param vars <`named list`> A list object of class `q5`. The
#' output of \code{\link{vars_build}}.
#' @param lang <`character`> A character string indicating the language to
#' translate variable titles to.
#' @param ... Additional arguments passed to methods.
#'
#' @return A list of label titles returned from \code{\link[ggplot2]{labs}}.
#' @export
legend_labels.q5 <- function(vars, lang = NULL, ...) {
  title_left <-
    var_get_title(
      var = vars$var_left, short_treshold = 25,
      translate = TRUE, lang = lang
    )
  return(list(ggplot2::labs(x = title_left, y = NULL)))
}

#' Get a list of label titles for Curbcut legends containing 100 breaks
#'
#' \code{legend_labels.q100} is a method for \code{\link{legend_labels}}
#' which takes a named list of variables and an optional language
#' argument, which it passes to a helper function \code{\link{var_get_info}}
#' to extract the title of the left variable. The resulting list contains a
#' label for the x-axis, with a NULL value for the y-axis.
#'
#' @param vars <`named list`> A named list object of class \code{q100}. The
#' output of \code{\link{vars_build}}.
#' @param lang <`character`> A character string indicating the language to
#' translate variable titles to.
#' @param ... Additional arguments passed to methods.
#'
#' @return A list of label titles returned from \code{\link[ggplot2]{labs}}.
#' @export
legend_labels.q100 <- function(vars, lang = NULL, ...) {
  title_left <-
    var_get_title(
      var = vars$var_left, short_treshold = 25,
      translate = TRUE, lang = lang
    )
  return(list(ggplot2::labs(x = title_left, y = NULL)))
}

#' Get a list of label titles for qualitative Curbcut legends
#'
#' \code{legend_labels.qual} is a method for \code{\link{legend_labels}},
#' which takes a named list of variables with class \code{qual} and an optional
#' language argument. It passes the \code{var_left} object in \code{vars} to a helper
#' function \code{\link{var_get_info}} to extract the title of the
#' variable, and returns a list containing a label for the x-axis and NULL for
#' the y-axis.
#'
#' @param vars <`named list`> A named list object of class \code{qual}. The
#' output of \code{\link{vars_build}}.
#' @param lang <`character`> A character string indicating the language to
#' translate variable titles to.
#' @param ... Additional arguments passed to methods.
#'
#' @return A list of label titles returned from \code{\link[ggplot2]{labs}}.
#' @export
legend_labels.qual <- function(vars, lang = NULL, ...) {
  title_left <-
    var_get_title(
      var = vars$var_left, short_treshold = 25,
      translate = TRUE, lang = lang
    )
  return(list(ggplot2::labs(x = title_left, y = NULL)))
}

#' Get a list of label titles for bivariate xdelta yq3 Curbcut legends
#'
#' The `bivariate_xdelta_yq3` class is used when there are multiple year selected
#' for the left-hand variables, but it can only be matched to one year of the
#' right-hand variable. We show a variation on the left-hand, and a static
#' year score on the right-hand.
#'
#' \code{legend_labels.bivar_ldelta_rq3}is a method for \code{\link{legend_labels}},
#' takes a named list of variables and an optional language argument, which it
#' passes to helper functions to extract the titles of the left and right variables.
#' It also retrieves the short titles of each variable. The resulting list
#' contains a label for the x-axis, a label for the y-axis, and the short titles
#' for each axis.
#'
#' @param vars <`named list`> A list object of class `bivar_ldelta_rq3`. The
#' output of \code{\link{vars_build}}.
#' @param lang <`character`> A character string indicating the language to
#' translate variable titles to.
#' @param ... Additional arguments passed to methods.
#'
#' @return A list of label titles returned from \code{\link[ggplot2]{labs}}.
#' The list contains the x-axis label, the y-axis label, and the short titles for
#' both axes.
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

#' Get a list of label titles for delta Curbcut legends
#'
#' The `delta` class is used when we compare two scores of the same left-hand
#' variables but in different years, e.g. "housing_tenant_2016" and
#' "housing_tenant_2021".
#'
#' \code{legend_labels.delta} takes a named list of variables and an optional language
#' argument, which it passes to a helper function \code{\link{var_get_title}}
#' to extract the title of the left variable. The resulting list contains a
#' label for the x-axis, with a NULL value for the y-axis.
#'
#' @param vars <`named list`> A list object of class `delta`. The
#' output of \code{\link{vars_build}}.
#' @param lang <`character`> A character string indicating the language to
#' translate variable titles to.
#' @param ... Additional arguments passed to methods.
#'
#' @return A list of label titles returned from \code{\link[ggplot2]{labs}}.
#' @export
legend_labels.delta <- function(vars, lang = NULL, ...) {
  title_left <-
    var_get_title(
      var = vars$var_left, short_treshold = 25,
      translate = TRUE, lang = lang
    )
  date_left <- var_get_time(var = vars$var_left)
  date_left <- paste(date_left, collapse = " - ")
  title_left <- paste0(title_left, " (\u0394 ", date_left, ")")
  return(list(ggplot2::labs(x = title_left, y = NULL)))
}

#' Get a list of label titles for bivariate Curbcut legends
#'
#' The `bivar` class is used when we compare the two variables together.
#'
#' \code{legend_labels.bivar} takes a named list of variables and an optional
#' language argument, which it passes to a helper function \code{\link{var_get_info}}
#' to extract the title of the left and right variables, as well as their respective dates.
#' The resulting list contains a label for the x-axis, with the title of the right variable
#' and a label for the y-axis with the title of the left variable along with the date of each
#' variable.
#'
#' @param vars <`named list`> A list object of class `bivar`. The
#' output of \code{\link{vars_build}}.
#' @param lang <`character`> A character string indicating the language to
#' translate variable titles to.
#' @param ... Additional arguments passed to methods.
#'
#' @return A list of label titles returned from \code{\link[ggplot2]{labs}}.
#' @export
legend_labels.bivar <- function(vars, lang = NULL, ...) {
  title_left <-
    var_get_title(
      var = vars$var_left, short_treshold = 16,
      translate = TRUE, lang = lang
    )
  title_right <-
    var_get_title(
      var = vars$var_right, short_treshold = 25,
      translate = TRUE, lang = lang
    )

  date_left <- var_get_time(var = vars$var_left)
  date_right <- var_get_time(var = vars$var_right)

  if (!is.na(date_left)) {
    title_left <- paste0(title_left, " (", date_left, ")")
  }
  if (!is.na(date_right)) {
    title_right <- paste0(title_right, " (", date_right, ")")
  }

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

#' Get a list of label titles for delta bivariate Curbcut legends
#'
#' The `delta_bivar` class is used when we compare two variations together.
#'
#' \code{legend_labels.delta_bivar} takes a named list of variables and an optional
#' language argument, which it passes to a helper function \code{\link{var_get_info}}
#' to extract the title of the left and right variables, as well as their respective dates.
#' The resulting list contains a label for the x-axis, with the title of the right variable
#' and a label for the y-axis with the title of the left variable along with the date of each
#' variable.
#'
#' @param vars <`named list`> A list object of class `delta_bivar`. The
#' output of \code{\link{vars_build}}.
#' @param lang <`character`> A character string indicating the language to
#' translate variable titles to.
#' @param ... Additional arguments passed to methods.
#'
#' @return A list of label titles returned from \code{\link[ggplot2]{labs}}.
#' @export
legend_labels.delta_bivar <- function(vars, lang = NULL, ...) {
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
      var = vars$var_right, short_treshold = 12,
      translate = TRUE, lang = lang
    )
  date_right <- var_get_time(var = vars$var_right)
  date_right <- paste(date_right, collapse = " - ")
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
