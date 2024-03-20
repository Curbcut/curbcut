#' Function for exploring bivariate correlation between two variables
#'
#' This function calculates bivariate correlation between two variables and
#' returns a list containing the correlation coefficient, a boolean indicating
#' whether the correlation is positive or negative, a text string describing the
#' strength and direction of the correlation, and a text string describing the
#' relationship between the variables.
#'
#' @param vars <`character`> A list containing the variable names for which the
#' text needs to be generated. Usually the output of \code{\link{vars_build}}.
#' @param data <`data.frame`> A data frame containing the variables and
#' observations. The output of \code{\link{data_get}}.
#' @param time <`numeric named list`> The `time` at which data is displayed.
#' A list for var_left and var_right. The output of \code{\link{vars_build}}(...)$time.
#' @param lang <`character`> A string indicating the language in which to
#' translates the variable. Defaults to NULL.
#' @param schemas <`named list`> Current schema information. The additional widget
#' values that have an impact on which data column to pick. Usually `r[[id]]$schema()`.
#'
#' @return A list containing the correlation coefficient, a boolean indicating
#' whether the correlation is positive or negative, a text string describing
#' the strength and direction of the correlation, and a text string describing
#' the relationship between the variables.
explore_text_bivar_correlation <- function(vars, data, time, lang = NULL, schemas = NULL) {
  # Get correlation and method string
  corr <- explore_text_bivar_correlation_helper(
    vars = vars,
    data = data,
    time = time,
    lang = lang,
    schemas = schemas
  )

  # Is the correlation positive
  positive <- corr$corr > 0
  positive_string <- if (positive) "positive" else "negative"
  positive_string <- cc_t(positive_string, lang = lang)

  # Correlation strings
  absolute <- abs(corr$corr)
  # Flag the correlation as NOT strong to start with.
  strong <- FALSE
  # Flag the correlation as inexistant to start with
  no_correlation <- FALSE

  if (absolute > 0.7) {
    relation_text <- cc_t("almost always have", lang = lang)
    strength <- cc_t("strong", lang = lang)
    strong <- TRUE

    corr_strength <- sprintf(
      cc_t("%s %s correlation", lang = lang), strength,
      positive_string
    )
  } else if (absolute > 0.3) {
    relation_text <- cc_t("tend to have", lang = lang)
    strength <- cc_t("moderate", lang = lang)

    corr_strength <- sprintf(
      cc_t("%s %s correlation", lang = lang), strength,
      positive_string
    )
  } else if (absolute > 0.1) {
    relation_text <- cc_t("often have _X_, although with many exceptions",
      lang = lang
    )
    strength <- cc_t("weak", lang = lang)

    corr_strength <- sprintf(
      cc_t("%s %s correlation", lang = lang), strength,
      positive_string
    )
  } else {
    relation_text <- cc_t("effectively no relationship", lang = lang)
    strength <- cc_t("effectively no correlation", lang = lang)
    no_correlation <- TRUE

    corr_strength <- strength
  }

  return(list(
    corr = corr$corr_string,
    strong = strong,
    positive = positive,
    no_correlation = no_correlation,
    relation_text = relation_text,
    corr_strength = corr_strength
  ))
}

#' Helper function for generating adjective to describe bivariate relationship
#' between text variables
#'
#' This function generates a text string containing an adjective to describe the
#' bivariate relationship between two variables based on whether the relationship
#' is positive or negative.
#'
#' @param var <`character`> The variable code for which the text needs to be
#' generated. `vars$var_left` or `vars$var_right`
#' @param left <`logical>` Whether the `var` supplied is the var_left
#' or the `var_right`. If `var_left`, TRUE.
#' @param positive <`logical`> Whether the bivariate relationship is positive
#' or negative. One of the output of
#' \code{\link{explore_text_bivar_correlation}}.
#' @param style <`logical`> Whether the output should have text styling (e.g.
#' <b>).
#' @param lang <`character`> A string indicating the language in which to
#' translates the variable. Defaults to NULL.
#' @param ... Additional arguments to be passed to methods.
#'
#' @return A text string containing an adjective to describe the bivariate
#' relationship.
#' @export
explore_text_bivar_adjective <- function(var, left, positive, style = TRUE,
                                         lang = NULL, ...) {
  UseMethod("explore_text_bivar_adjective", var)
}

#' @rdname explore_text_bivar_adjective
#' @export
explore_text_bivar_adjective.dollar <- function(var, left, positive,
                                                style = TRUE, lang = NULL, ...) {
  string <- (\(x) {
    if (left) {
      return(cc_t("higher", lang = lang))
    }
    if (positive) {
      return(cc_t("higher", lang = lang))
    }
    return(cc_t("lower", lang = lang))
  })()

  start <- ifelse(style, "<b>%s</b>", "%s")

  return(sprintf(start, string))
}

#' @rdname explore_text_bivar_adjective
#' @export
explore_text_bivar_adjective.default <- function(var, left, positive,
                                                 style = TRUE, lang = NULL, ...) {
  string <- (\(x) {
    if (left) {
      return(cc_t("a higher", lang = lang))
    }
    if (positive) {
      return(cc_t("a higher", lang = lang))
    }
    return(cc_t("a lower", lang = lang))
  })()

  start <- ifelse(style, "<b>%s</b>", "%s")

  return(sprintf(start, string))
}

#' Update Schema Placeholders in Right Variable Explanation Text
#'
#' This function updates placeholders within the explanation text for the
#' "right" variable in bivariate analysis, based on the default schema values
#' specified in the data's attributes. It handles special formatting for time
#' values and ensures that schema placeholders like "__transportationtime__" are
#' replaced with actual values from the data's attributes.
#'
#' @param right_exp <`character`> A single character string containing the
#' explanation text for the right variable. This text may include placeholders
#' for schema values, formatted as "__transportationtime__".
#' @param data <`data.frame`> A data frame that contains the variables and
#' attributes necessary for the analysis. This data frame must have a
#' `schema_var_right` attribute that lists schema names and their corresponding
#' default values, and a `breaks_var_var_right` attribute that specifies how to
#' extract specific values from the schema.
#'
#' @details The function first checks if the explanation text contains any
#' schema placeholders. If so, it iterates through each schema defined in the
#' data's `schema_var_right` attribute, extracts the appropriate value based on
#' the `breaks_var_var_right` attribute, and replaces the placeholder in the
#' explanation text with the actual value. For time schemas, it also converts
#' numeric time values to a character representation. The function performs
#' these replacements for all occurrences of each schema placeholder found in
#' the text.
#'
#' @return <`character`> The modified explanation text with all schema
#' placeholders replaced by their corresponding values.
explore_text_bivar_right_var_default_schema <- function(right_exp, data) {
  if (grepl("__.*__", right_exp)) {
    schemas_col <- attributes(data)$schema_var_right
    for (sch in names(schemas_col)) {
      value <- schemas_col[[sch]]
      value <- s_extract(value, attributes(data)$breaks_var_var_right)
      value <- gsub("_", "", value)

      # Special case if time needs to be seen as character
      if (sch == "time") {
        value <- time_chr(var, value)
      }

      scm <- sprintf("__%s__", sch)

      # Determine the number of occurrences to replace
      num_replacements <- min(length(value), gregexpr(scm, right_exp)[[1]] |> length())

      # Loop through each occurrence and replace with corresponding value
      for (i in 1:num_replacements) {
        right_exp <- sub(scm, value[i], right_exp, fixed = TRUE)
      }
    }
  }
  return(right_exp)
}
