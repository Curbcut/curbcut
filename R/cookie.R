#' Load JavaScript cookie library and custom cookie script for Curbcut
#'
#' This function generates an HTML tag list that loads the necessary JavaScript
#' files for using cookies in the curbcut package. It adds a script tag for a
#' custom cookie.js script and a script tag for the js-cookie library from
#' cdn.jsdelivr.net. It needs to be placed in the `ui` function in `ui.R` for
#' the cookies function to work.
#'
#' @return A tag list object with the necessary JavaScript files for using cookies
#' in curbcut package. Must be placed in the `ui` function in `ui.R`.
#' @export
use_curbut_cookie <- function() {
  shiny::tagList(
    shiny::tags$head(shiny::tags$script(src = system.file("js_scripts/cookie.js",
      package = "curbcut"
    ))),
    shiny::tags$script(src = paste0(
      "https://cdn.jsdelivr.net/npm/js-cookie@rc/",
      "dist/js.cookie.min.js"
    ))
  )
}

#' Set and save a cookie value
#'
#' This function sets a cookie value by sending a custom message to the
#' client-side JavaScript. The cookie name and value are specified as arguments
#' to the function and can be retrieved in ulterior session of the same user.
#'
#' @param session This parameter represents the session of the app. It allows the
#' module to interact with the app and perform various actions, such as updating
#' the UI or accessing user information.Should be `session = session`, as it's the
#' same `session` than in the shinyServer and moduleServer functions.
#' @param name <`character`> A character string specifying the name of the
#' cookie to be set, e.g. `lang`.
#' @param value <`string`> The value to be set for the cookie.
#'
#' @export
cookie_set <- function(session, name, value) {
  session$sendCustomMessage("cookie-set", list(name = name, value = value))
}

#' Retrieve the value of a cookie for the current Shiny session
#'
#' This function retrieves the value of a cookie with the specified name from
#' the current Shiny session. The name of the cookie is specified as an argument
#' to the function. The value of the cookie is returned.
#'
#' @param input This parameter represents the reactive list of all input values.
#' It allows the module to access and use the input values provided by the user
#' in the app. Should be `input = input`, as it's the same `input` than in the
#' shinyServer and moduleServer functions.
#' @param name <`character`> A character string specifying the name of the
#' cookie to be retrieved.
#'
#' @return The value of the specified cookie, or NULL if the cookie is not set.
#' @export
cookie_retrieve <- function(input, name) {
  input$cookie[[name]]
}

#' Update the value of a cookie and return the updated value
#'
#' This function updates the value of a reactive variable. The current value of the
#' variable is passed as an argument to the function. If there is no cookie with
#' the specified name, the current value of the variable is returned
#' is returned. If the cookie exists and has a different value than the current
#' value, the cookie value is returned. This function is intended to be used to
#' update a reactive value in a Shiny app.
#'
#' @param input This parameter represents the reactive list of all input values.
#' It allows the module to access and use the input values provided by the user
#' in the app. Should be `input = input`, as it's the same `input` than in the
#' shinyServer and moduleServer functions.
#' @param name <`character`> A character string specifying the name of the cookie
#' to retrieve.
#' @param current_value <`character`> The current value of the reactive value
#' to update.
#'
#' @return The value of the cookie, or the `current value` if there is no cookie
#' or if the current value of the variable is the same as the one of the cookie.
#' @export
cookie_update_value <- function(input, name, current_value) {
  # Get the cookie value
  cookie <- cookie_retrieve(input, name)

  # If there is no cookie for that name, return the current value
  if (is.null(cookie)) {
    return(current_value)
  }

  # If there's a cookie and it's different from the current value
  if (cookie != current_value) {
    return(cookie)
  }

  # Return current value if the cookie is the same
  return(current_value)
}
