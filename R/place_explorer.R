#' Prepare data for a title card
#'
#' This function prepares data for a title card for a specific region and indicator.
#' It returns information about the data value, data date, data rank and color.
#'
#' @param data <`list`> Contains data about the indicator in its region
#' and scale, e.g. `pe_main_card$main_card_data$no2$CMA$CSD`.
#' @param dict <`data.frame`> Row from the dictionary for the data
#' (title, text, etc.), e.g.
#' `pe_main_card$main_card_dict[pe_main_card$main_card_dict$name == "no2", ]`.
#' @param lang <`character`> Language to use for translation. Must be one of
#' en' or 'fr'.
#' @param translation <`data.frame`> Data frame containing translation data.
#' @param region <`character`> The geographic region.
#' @param df <`data.frame`> The scale, e.g. `DA`, `CT`, ...
#' @param select_id <`character`> The ID of the selected area.
#' @param scales_dictionary <`data.frame`> A dictionary containing information
#' about the scale of data.
#' @param regions_dictionary <`data.frame`> A dictionary containing information
#' about the regions.
#'
#' @return A list of information about the data, including the data value,
#' data date, data rank (in text), and color category (1-5).
#'
#' @export
placeex_prep_title_card <- function(data, dict, lang, translation, region, df,
                                    select_id, scales_dictionary,
                                    regions_dictionary) {

  # Setup ----------------------------------------------------------------------

  df_scale <- cc_t(lang = lang, translation = translation, "The ",
                   scales_dictionary$sing[scales_dictionary$scale == df])
  df_scales <- cc_t(lang = lang, translation = translation,
                    scales_dictionary$plur[scales_dictionary$scale == df])

  # To what it compares
  to_compare <-
    cc_t(lang = lang, translation = translation,
         regions_dictionary$to_compare[regions_dictionary$geo == region])

  # Prepare list to store all data
  info <- list()

  # Get data value
  data_s <- data[data$ID == select_id, ]
  if (length(data_s$var) == 0 || is.na(data_s$var)) return(NULL)


  # pretty_data_var ------------------------------------------------------------

  info$pretty_data_var <- if (dict$percent) {
    scales::percent(data_s$var)
  } else round(data_s$var, digits = dict$val_digit)


  # Data date ------------------------------------------------------------------

  info$data_date <- dict$date


  # Data rank ------------------------------------------------------------------

  info$data_rank <-
    # If the dataset is small
    if (nrow(data) < 40) {

      # How many non-na entries in the dataset
      df_row <- sum(!is.na(data$var))
      # If high is good, then last rank means 1st. Inverse!
      data_rank <- if (dict$high_is_good) df_row - data_s$rank + 1 else data_s$rank

      ordinal <- (\(x) {
        # if ranks in the bottom third
        if (data_rank > (2 / 3 * df_row)) return({
          cc_t(lang = lang, translation = translation,
               "relatively low at {ordinal_form(lang = lang, data_rank)}")
        })
        # if ranks in the second third
        if (data_rank > (1 / 3 * df_row))
          return(ordinal_form(lang = lang, data_rank))
        # else
        return(cc_t(lang = lang, translation = translation,
                    paste0("{ordinal_form(lang = lang, data_rank, ",
                           "en_first = '')} best")))
      })()

      cc_t(lang = lang, translation = translation,
           "It ranks {ordinal} {to_compare}")


      # If the dataset is large
    } else {

      (\(x) {
        if (data_s$percentile > 0.75) return({
          paste0(cc_t(lang = lang, translation = translation,
                      "{df_scale} ranks in the top "),
                 if (abs(data_s$percentile - 1) < 0.01) "1%" else
                   scales::percent(abs(data_s$percentile - 1)))
        })

        if (data_s$percentile < 0.25) return({
          paste0(
            cc_t(lang = lang, translation = translation,
                 "{df_scale} ranks in the bottom "),
            if (data_s$percentile < 1) "1%" else scales::percent(data_s$percentile))
        })

        pretty_perc <- scales::percent(data_s$percentile)

        if (dict$high_is_good) {
          cc_t(lang = lang, translation = translation,
               "Its value is worse than {pretty_perc} ",
               "of {df_scales} {to_compare}")
        } else {
          cc_t(lang = lang, translation = translation,
               "Its value is higher than {pretty_perc} ",
               "of {df_scales} {to_compare}")
        }

      })()
    }


  # Colour ---------------------------------------------------------------------

  colours_which <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  if (!dict$high_is_good) colours_which <- rev(colours_which)

  info$hex_cat <- which.min(abs(colours_which - data_s$percentile))

  # In case it's higher than the threshold of 53 for Air Quality
  if (dict$name == "no2" && data_s$var >= 53) info$hex_cat <- 1


  # Percentile -----------------------------------------------------------------

  info$percentile <-
    if (data_s$percentile > 0.50) {
      per <- scales::percent(abs(data_s$percentile - 1))
      if (per == "0%") per <- "1%"
      paste0(cc_t(lang = lang, translation = translation, "Top {per}"))
    } else {
      per <- scales::percent(abs(data_s$percentile))
      if (per == "0%") per <- "1%"
      paste0(cc_t(lang = lang, translation = translation, "Bottom {per}"))
    }


  # Return ------------------------------------------------------------------

  return(info)

}


#' Final function for the place explorer main card
#'
#' @param pe_main_card <`list`> Place explorer data for the maincard. Usually
#' the output of \code{\link[cc.buildr]{placeex_main_card}} saved in the
#' data folder.
#' @param region <`character`> Region under study, e.g. `CMA`.
#' @param df <`character`> Scale under study, e.g. `DA`.
#' @param select_id <`character`> Selected identifier for the selected combinasion
#' of `region` and `df`.
#' @param lang <`character`> Language to use for translation. Must be one of
#' en' or 'fr'.
#' @param translation <`data.frame`> Data frame containing translation data.
#'
#' @return Returns a list of all the main card variables and how the selected
#' id compares in its dataset.
#' @export
placeex_main_card <- function(pe_main_card, region, df, select_id, lang,
                              translation) {

  ## Generate output grid ------------------------------------------------------

  to_grid <- lapply(pe_main_card$main_card_dict$name, \(x) {

    data <- pe_main_card$main_card_data[[x]][[region]][[df]]
    dict <- pe_main_card$main_card_dict[pe_main_card$main_card_dict$name == x, ]

    z <- curbcut::placeex_prep_title_card(
      data = data,
      dict = dict,
      lang = lang,
      translation = translation,
      region = region,
      df = df,
      select_id = select_id,
      scales_dictionary = scales_dictionary,
      regions_dictionary = regions_dictionary
    )

    if (is.null(z)) return(cc_t(lang = lang,
                                translation = translation,
                                "No data."))

    # Exception - additional text for no2 if over the threshold of 53 ppm
    if (x == "no2") higher_than_threshold <-
      if (z$pretty_data_var > 53) {
        cc_t(lang = lang, translation = translation,
             "Its value is higher than the WHO's guideline value of 53. ")
      } else ""

    list(row_title = dict$title,
         percentile = z$percentile,
         text = cc_t(lang = lang,
                     translation = translation,
                     dict$text),
         hex_cat = z$hex_cat,
         bs_icon = dict$bs_icon,
         data = data)
  })

  names(to_grid) <- pe_main_card$main_card_dict$name

  to_grid[sapply(to_grid, is.null)] <- NULL

  to_grid

}
