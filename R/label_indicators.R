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

#' Advanced Controls - UI
#'
#' This function creates the user interface for the advanced controls module.
#' It is designed to be used in tandem with the advanced_controls_server function.
#'
#' @param id <`character`> The ID of the page in which the legend will appear,
#' e.g. `alp`. NOT A MODULE, NO NAMESPACING.
#' @param label <`character`> Label for the checkbox. Defaults to `cc_t("Advanced controls")`
#' @param ... <`args`> UIs to be inserted in the advanced controls div.
#'
#' @return A Shiny UI definition
#' @export
label_indicators_UI <- function(id, label = cc_t("Advanced controls"), ...) {
  shiny::tagList(
    shinyjs::hidden(shiny::div(
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
      shinyjs::hidden(
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
    )),
    shiny::div(id = shiny::NS(id, "common_widgets")),
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
      ...
    ))
  )
}
