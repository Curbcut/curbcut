#' Advanced Controls (server, although NOT A MODULE)
#'
#' This function defines the server-side behavior of the advanced controls module.
#' The advanced controls feature a checkbox that, when toggled, will show or
#' hide additional widgets.
#'
#' @param id <`character`> The ID of the page in which the legend will appear,
#' e.g. `alp`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param label <`character`> Label for the checkbox. Must be a reactive.
#' Defaults to `shiny::reactive("Advanced controls")`
#'
#' @return None
#' @export
label_indicators_server <- function(id, r, label = shiny::reactive("Advanced controls")) {
  shiny::moduleServer(id, function(input, output, session) {
    # Checkbox to show additional widgets
    show_advanced <- checkbox_server(
      id = "cb_advanced_controls",
      r = r,
      label = label
    )

    # If the checkbox is clicked, slide open the div
    shiny::observeEvent(show_advanced(), {
      shinyjs::toggle("advanced_controls_div",
        condition = show_advanced(),
        anim = TRUE, animType = "slide"
      )
    })

    return(show_advanced)
  })
}

#' Create a UI Element with Advanced Controls
#'
#' This function creates a Shiny UI element with a section for common widgets and
#' a hidden section for advanced controls. The advanced section can be toggled
#' with a checkbox. Additional arguments can be passed to customize both the
#' main label and the advanced controls sections.
#'
#' @param id <`character`> A unique identifier for the UI element.
#' @param label <`character`> Label for the checkbox that toggles the display of
#' advanced controls. Default is 'Advanced controls'.
#' @param main_UIs <`list`> A list of additional arguments for the main
#' indicator label div. Default is an empty list.
#' @param adv_UIs <`list`> A list of additional arguments for the advanced
#' controls div. Default is an empty list.
#'
#' @return A `shiny::tagList` object containing the UI elements.
#' @export
label_indicators_UI <- function(id, label = cc_t("Advanced controls"),
                                main_UIs = list(), adv_UIs = list()) {

  shiny::tagList(
    shiny::div(
      id = shiny::NS(id, "indicator_label"),
      class = "shiny-split-layout sidebar-section-title",
      shiny::div(
        style = "width: 9%",
        icon_material_title("tune")
      ),
      shiny::div(
        style = "width: 30%",
        cc_t("Indicator")
      ),
      shiny::div(
        id = shiny::NS(id, "cb_adv_opt_div"),
        style = "width: 60%; margin:0px !important; text-align: right; overflow: hidden;",
        checkbox_UI(
          id = shiny::NS(id, "cb_advanced_controls"),
          label = label,
          value = FALSE
        )
      )
    ),
    shiny::div(id = shiny::NS(id, "common_widgets"), main_UIs),
    shinyjs::hidden(shiny::div(
      id = shiny::NS(id, "advanced_controls_div"),
      shiny::hr(id = shiny::NS(id, "hr_advanced_controls")),
      shiny::div(
        class = "shiny-split-layout sidebar-section-title",
        shiny::div(
          style = "width: 9%",
          icon_material_title("precision_manufacturing")
        ),
        shiny::div(
          style = "width: 89%",
          cc_t("Advanced controls")
        )
      ),
      shiny::div(id = shiny::NS(id, "additional_widgets_div")),
      adv_UIs
    ))
  )

}
