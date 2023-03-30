#' Use Bookmark
#'
#' This function is a module server that updates reactive values and UI elements
#' based on URL query string. The function is designed enable bookmarking and sharing
#' of a specific state of the application. It is designed to be placed only once,
#' in the `server.R` file.
#'
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#'
#' @return handles updating the state of the app based on the URL query string.
#' @export
use_bookmark <- function(r) {
  shiny::observeEvent(
    shiny::parseQueryString(r$server_session()$clientData$url_search),
    {
      # Get the URL search query
      query <- shiny::parseQueryString(r$server_session()$clientData$url_search)

      # If no query, do no action
      if (length(query) == 0) {
        return(NULL)
      }

      # Start by updating app-wide reactives
      if ("reg" %in% names(query)) {
        reg <- query$reg
        # As we convert an ID in the region table to a numeric for smaller URL
        # bookmarking, retrieve the region string using its index.
        regions_dictionary <- get_from_globalenv("regions_dictionary")
        reg <- regions_dictionary$region[as.numeric(reg)]
        r$region(reg)
      }
      if ("lng" %in% names(query)) r$lang(query$lng)

      # The rest are tab dependent.
      # Grab the tab
      if (!"tb" %in% names(query)) {
        return(NULL)
      }

      # As we convert an ID in the modules table to a numeric for smaller URL
      # bookmarking, retrieve the page ID using its index.
      tab <- query$tb
      if (is_numeric(tab)) {
        modules <- get_from_globalenv("modules")
        tab <- modules$id[as.numeric(tab)]
      }
      # Update the current tab
      shiny::updateTabsetPanel(
        session = r$server_session(),
        inputId = "cc_page",
        selected = tab
      )

      # Start by the map viewstate (if a map module)
      if ("zm" %in% names(query)) r[[tab]]$zoom(as.numeric(query$zm))
      if ("crds" %in% names(query)) {
        coords <- query$crds
        coords <- strsplit(coords, ";")[[1]]
        r[[tab]]$coords(coords)
      }

      # Update widgets
      wgt <- query$wgt
      # If no widgets, do nothing
      if (is.null(wgt)) {
        return(NULL)
      }

      # Make a NS minimal function
      ns <- function(widget_id, tb = tab, double = T) {
        if (double) {
          return(paste(tb, tb, widget_id, sep = "-"))
        }
        return(paste(tb, widget_id, sep = "-"))
      }

      # Delay first, then update all the widgets
      shinyjs::delay(500, {
        widgets <- bookmark_widget_helper(
          wgt = wgt,
          lang = r$lang()
        )

        # Start by the checkboxes
        lapply(widgets$cbox, \(widget) {
          shiny::updateCheckboxInput(
            session = r$server_session(),
            inputId = ns(widget[[1]]),
            value = as.logical(widget[[2]])
          )
        })

        # Followed by the sliders
        lapply(widgets$s_text, \(widget) {
          shinyWidgets::updateSliderTextInput(
            session = r$server_session(),
            inputId = ns(widget[[1]]),
            selected = widget[[2]]
          )
        })
        lapply(widgets$slider, \(widget) {
          # If there are multiple values, split at every `-`
          value <- strsplit(widget[[2]], split = "-")[[1]]
          shiny::updateSliderInput(
            session = r$server_session(),
            inputId = ns(widget[[1]]),
            value = as.numeric(value)
          )
        })

        # Finish with the pickers, with a delay to make sure the rest is
        # well updated first
        shinyjs::delay(500, {
          lapply(widgets$picker, \(widget) {
            shinyWidgets::updatePickerInput(
              session = r$server_session(),
              inputId = ns(widget[[1]]),
              selected = widget[[2]]
            )
          })
        })
      })

      # Finish by the ID selection
      if ("sid" %in% names(query)) {
        new_id <- if (query$sid %in% c("", "NA")) NA else query$sid
        r[[tab]]$select_id(new_id)
      }
    },
    priority = -5
  )
}


#' Helper function to separate and process bookmarked Shiny widgets
#'
#' This function takes a bookmarked widget string and separates the widgets into
#' two groups: those corresponding to \code{\link{bookmark_codes}} and those to
#' \code{\link{bookmark_shorts}} It then processes the widget IDs and values as
#' needed and returns them as a list to be used by the \code{\link{use_bookmark}}
#' function.
#'
#' @param wgt <`character`> A character string representing the widget inputs
#' (the `wgt` part of the bookmarked URL).
#' @param lang <`character`> An optional character string specifying the language
#' used on the app. Default is NULL.
#'
#' @return A list of processed bookmarked Curbcut widgets separated into three
#' categories: "cbox" (checkboxes), "s_text" (slider text), and "picker" (pickers).
#' @export
bookmark_widget_helper <- function(wgt, lang = NULL) {
  # Separate widgets between bookmark_codes and bookmark_shorts
  widgets <- strsplit(wgt, ";")[[1]]
  widgets <- sapply(widgets, strsplit, ":", USE.NAMES = FALSE)

  n_char <- lapply(widgets, nchar)
  n_char <- lapply(n_char, `[[`, 1)
  from_codes <- widgets[n_char == 2]
  from_short <- widgets[n_char > 2]

  # Grab the real widget ID from bookmark_codes
  from_codes <- lapply(from_codes, \(widget) {
    code <- widget[[1]]
    which_widget <- which(curbcut::bookmark_codes == code)
    return(c(names(curbcut::bookmark_codes)[which_widget], widget[[2]]))
  })

  # Grab the real widget ID from bookmark_shorts
  from_short <- lapply(from_short, \(widget) {
    code <- widget[[1]]
    short <- substr(code, start = 1, stop = 2)
    id <- substr(code, start = 3, stop = nchar(code))

    which_widget <- which(grepl(short, curbcut::bookmark_shorts))
    code <- paste0(names(curbcut::bookmark_shorts)[which_widget], id)
    return(c(code, widget[[2]]))
  })

  # Start with the well known widgets (bookmark_codes)
  cbox <- from_codes[grepl("cccheckbox_", lapply(from_codes, `[[`, 1))]
  s_text <- from_codes[grepl("ccslidertext_", lapply(from_codes, `[[`, 1))]
  picker <- from_codes[grepl("ccpicker_", lapply(from_codes, `[[`, 1))]
  slider <- from_codes[grepl("ccslider_", lapply(from_codes, `[[`, 1))]

  # Process the widget value as needed
  scales_dictionary <- get_from_globalenv("scales_dictionary")
  s_text <- lapply(s_text, \(widget) {
    if (widget[[2]] %in% scales_dictionary$scale) {
      widget[[2]] <- zoom_get_name(widget[[2]], lang = lang)
    }
    return(widget)
  })
  variables <- get_from_globalenv("variables")
  picker <- lapply(picker, \(widget) {
    if (is_numeric(widget[[2]])) {
      widget[[2]] <- variables$var_code[as.numeric(widget[[2]])]
    }
    return(widget)
  })
  codes <- list(cbox = cbox, s_text = s_text, picker = picker, slider = slider)

  ## Continue with the additional widgets
  cbox <- from_short[grepl("cccheckbox_", lapply(from_short, `[[`, 1))]
  s_text <- from_short[grepl("ccslidertext_", lapply(from_short, `[[`, 1))]
  picker <- from_short[grepl("ccpicker_", lapply(from_short, `[[`, 1))]
  slider <- from_short[grepl("ccslider_", lapply(from_short, `[[`, 1))]

  # Process the widget value as needed
  picker <- lapply(picker, \(widget) {
    if (is_numeric(widget[[2]])) {
      widget[[2]] <- variables$var_code[as.numeric(widget[[2]])]
    }
    return(widget)
  })
  shorts <- list(cbox = cbox, s_text = s_text, picker = picker, slider = slider)

  # Return the list of widgets
  return(list(
    cbox = c(codes$cbox, shorts$cbox),
    s_text = c(codes$s_text, shorts$s_text),
    picker = c(codes$picker, shorts$picker),
    slider = c(codes$slider, shorts$slider)
  ))
}
