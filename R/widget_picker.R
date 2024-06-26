#' Shiny module for a variable picker
#'
#' This function creates a Shiny module that allows users to pick a variable from a
#' list of options, and returns the selected variable as a reactive expression.
#' The list of options can be dynamically updated based on the input data and the
#' user's choices, and options can be disabled based on user input (time).
#' It consists of a dropdown menu with variable options, with the variable
#' explanation text that appears when the user hovers over each option. The menu
#' can be dynamically updated based on the user's input and the data available
#' in \code{var_list}, \code{df}, and \code{time}. The module can also disable
#' certain options based on the `time` argument, for example if the user is
#' comparing multiple years and some variables are not present in all years.
#'
#' @param id <`character`> The ID of the page in which the widget will appear,
#' e.g. `alp`. Or other, like `compare` if it's for the compare widget.
#' @param page_id <`character`> The ID of the page in which the widget will appear,
#' e.g. `alp`. Useful for `compare` widget. Defaults to NULL if it's not a
#' `compare` widget.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param picker_id <`character`> A character string giving the identifier for
#' the picker input object. This will be used as the input's `inputId` and will
#' inform the bookmarking. It needs to be unique in the page. The default value
#' is `"var"`.
#' @param var_list <`named list`> Choices to display in the picker input. Normally
#' made using \code{\link{dropdown_make}}.
#' @param time <`reactive numeric vector`> Vector of time values to use for
#' appending a time to the variables picked. The returned vector will be the
#' same length as this argument.
#' @param selected <`reactive character`> The value for which to update the picker.
#' @param subtext <`reactive character`> Additional text for every option. Will
#' be added to choicesOpt in \code{\link[shinyWidgets]{updatePickerInput}}.
#' @param classNames <`reactive character vector`> Same number of variables as
#' there are variables in `var_list`. It's used to add a class to every option
#' in the dropdown widget, like custom background color for a few options.
#' @param disable_options <`reactive logical vector`> Vector of logical of
#' the same length as the number of variables in `var_list`. Should x variable
#' be disabled or not? Default to a reactive NULL, which means nothing is disabled.
#' @param ... Additional arguments to pass to \code{\link[shinyWidgets]{updatePickerInput}}
#'
#' @return A reactive expression that returns the selected variable with `time`
#' appended. The length of the output is the same length as the `time` argument.
#' @seealso \code{\link{picker_UI}}
#' @export
picker_server <- function(id, page_id = NULL, r, picker_id = "var", var_list,
                          time = shiny::reactive(NULL), selected = shiny::reactive(NULL),
                          subtext = shiny::reactive(NULL), classNames = shiny::reactive(NULL),
                          disable_options = shiny::reactive(NULL), ...) {
  stopifnot(shiny::is.reactive(time))
  stopifnot(shiny::is.reactive(var_list))
  stopifnot(shiny::is.reactive(subtext))
  stopifnot(shiny::is.reactive(selected))
  stopifnot(shiny::is.reactive(classNames))
  stopifnot(shiny::is.reactive(disable_options))

  shiny::moduleServer(id, function(input, output, session) {
    # Fails if `var_list` isn't a list.
    if (!is.list(var_list())) stop("`var_list` must be a list.")

    # Reformat the picker_id to make it obvious it's a picker (for bookmark)
    picker_id <- paste0("ccpicker_", picker_id)

    # Translate var_list
    var_list_t <- shiny::reactive(cc_t(var_list(), lang = r$lang()))

    # Get the `divs` with the explanation on hover (translated)
    hovers <- shiny::reactive(picker_hover_divs(
      var_list = var_list_t(),
      lang = r$lang()
    ))

    # If the picking is made while in a `delta` mode (comparing two years),
    # we disable the variables that are not present at all years.
    multi_year_compare <- shiny::reactiveVal(FALSE)
    # Make sure we don't create unwanted reactivity that triggers the reset
    # of the dropdown.
    shiny::observeEvent(time(), multi_year_compare({
      multi_year <- length(time()[[1]]) > 1
      multi_year & !is.null(page_id)
    }))
    disable_yr <- shiny::reactive(picker_multi_year_disable(
      id = page_id,
      var_list = var_list(),
      disable = multi_year_compare()
    ))

    # Combine argument with disable due to years
    disable <- shiny::reactive({
      if (is.null(disable_options())) return(disable_yr())
      disable_yr() | disable_options()
    })

    # Style the disable (dark gray for the unpickable)
    disable_style <- shiny::reactive({
      sapply(disable(), ifelse, "color: rgba(119, 119, 119, 0.5);", "")
    })

    # Record current value to keep it as current value when other things change
    current_value <- shiny::reactive(input[[picker_id]])

    # Update dropdown menu if there are disabled choices
    shiny::observe({
      current_val <- shiny::isolate(current_value())

      # If the selected variable is to be disabled, reset the dropdown
      disabled_options <- unlist(var_list_t())[disable()]
      anything_disabled <- length(disabled_options) > 0
      if (anything_disabled) {
        if (current_val %in% disabled_options) {
          current_val <- NULL
        }
      }

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = picker_id,
        choices = var_list_t(),
        selected = current_val,
        choicesOpt = c(
          if (!is.null(hovers())) hovers(),
          list(
            disabled = disable(),
            style = disable_style(),
            subtext = subtext(),
            classNames = classNames()
          )
        ),
        options = shinyWidgets::pickerOptions(container = "body"),
        ...
      )
    })

    # Update the selected variable if it changes
    shiny::observeEvent(selected(),
      {
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = picker_id,
          selected = selected(),
          options = shinyWidgets::pickerOptions(container = "body")
        )
      },
      ignoreNULL = TRUE
    )

    # # # If the dropdown is a compare, highlight differently the background
    # # # of the options that have a strong correlation
    # shiny::observe({
    #   if (!is.null(identifier)) {
    #     options <- sapply(var_list_t(), \(vars) {
    #       # FILTER HERE WHICH VARS TO KEEP (the output is the label of the options, e.g. 'Tenant-occupied (%)')
    #
    #     }) |> unname() |> unlist()
    #     highlight_dropdown(dropdown_identifier = identifier,
    #                        options = "Median household income ($)")
    #     }
    # })

    # Append the `time`
    var <- shiny::reactive(input[[picker_id]])

    # Return the picked variable as a reactive
    return(var)
  })
}

#' Create a picker input control in an optionally styled div
#'
#' This function creates a \code{\link[shinyWidgets]{pickerInput}} control in a
#' `div` container. The `pickerInput` control allows users to select one item
#' from a list of variables choices.
#'
#' @param id <`character`> The ID of the page in which the widget will appear,
#' e.g. `alp`.
#' @param picker_id <`character`> string giving the identifier for the picker input
#' object. This will be used as the input's `inputId` and will inform the bookmarking.
#' It needs to be unique in the page. The default value is `"var"`.
#' @param var_list <`reactive list`> Choices to display in the picker input.
#' Normally made using \code{\link{dropdown_make}}.
#' @param label A character string giving the label for the picker input control.
#' If `NULL`, no label will be displayed.
#' @param width <`character`> A character string giving the width of the picker
#' input control. The width of the input : 'auto', 'fit', '100px', '75%'.
#' @param div_style <`character`> CSS styles to apply to the `div` container.
#' The default value is `NULL` for no additional styling.
#' @param selected A character vector of a value to preselect. The default value
#' is `NULL` (the first value will be selected).
#' @param open_left <`logical`> Should the dropdown open to the left? Any dropdown
#' placed on the left-side of the screen should do see, and it is the default.
#' Compare dropdown should open right, and this can be changed.
#' @param identifier <`character`> Unique identifier that will be used by
#' \code{\link{highlight_dropdown}} to change the background color of options.
#' Defaults to NULL to not set any.
#' @param live_search <`logical`> Argument sent to \code{\link[shinyWidgets]{pickerOptions}}.
#' @param ... Additional arguments to pass to \code{\link[shinyWidgets]{pickerInput}}
#'
#' @return A `div` container containing a \code{\link[shinyWidgets]{pickerInput}}
#' control.
#' @seealso \code{\link{picker_server}}
#' @export
picker_UI <- function(id, picker_id = "var", var_list, label = NULL,
                      width = "100%", div_style = NULL, selected = NULL,
                      open_left = TRUE, identifier = NULL, live_search = FALSE, ...) {
  # Verify if the widget ID will interfere with bookmark
  picker_id <- widget_id_verif(widget_id = picker_id)

  # Reformat the picker_id to make it obvious it's a picker (for bookmark)
  picker_id <- paste0("ccpicker_", picker_id)

  # Non-numeric to not interfere with bookmark
  if (is.numeric(var_list)) {
    stop("A picker should not be numeric. It will interfere with bookmarking.")
  }

  # If forgot to drop parent vectors from the list of variables to pick
  variables <- get_from_globalenv("variables")
  if (all(unlist(var_list) %in% variables$var_code)) {
    are_parents <- unlist(var_list) %in% variables$parent_vec
    if (sum(are_parents) > 0) {
      stop(sprintf(paste0(
        "Parent vectors were included in the variable list ",
        "for the picker `%s-%s`. They can't be used ",
        "front-facing yet."
      ), id, picker_id))
    }
  }

  # Declare the input in an optionally styled div
  shiny::div(
    style = div_style,
    shinyWidgets::pickerInput(
      inputId = shiny::NS(id, picker_id),
      label = label,
      choices = var_list,
      selected = selected,
      width = width,
      choicesOpt = picker_hover_divs(var_list),
      options = shinyWidgets::pickerOptions(
        dropdownAlignRight = !open_left,
        container = "body",
        identifier = identifier,
        liveSearch = live_search
      ),
      ...
    )
  )
}
