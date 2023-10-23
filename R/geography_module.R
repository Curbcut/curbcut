geography_server <- function(id, r = r) {

  shiny::moduleServer(id, function(input, output, session) {

  })

}


#' @describeIn geography_server Create the UI for the zoom module
#' @export
geography_UI <- function(id, regions, avail_scale_combinations) {
  # Region default
  regions_dictionary <- get_from_globalenv("regions_dictionary")
  names(regions) <- regions_dictionary$name[regions_dictionary$region %in% regions]
  regions <- list("Region" = regions)

  # Top scale default
  top_scales <- gsub("_.*", "", avail_scale_combinations)
  top_scales <- unique(top_scales)
  scales_dictionary <- get_from_globalenv("scales_dictionary")
  names(top_scales) <- sapply(top_scales, function(x) {
    scales_dictionary$plur[scales_dictionary$scale == x] |>
      s_sentence()
  }, simplify = TRUE, USE.NAMES = FALSE)
  regions <- list("Top scale" = top_scales)

  shiny::tagList(
    shiny::div(
      class = "geography-panel",
      id = shiny::NS(id, "geo_div"),
      shiny::div(
        class = "shiny-split-layout sidebar-section-title",
        shiny::div(
          style = "width: 9%",
          icon_material_title("layers")
        ),
        shiny::div(
          style = "width: 80%",
          cc_t_span("Geography")
        )
      ),
      shiny::div(
        id = shiny::NS(id, "geography_div"),
        class = "sus-sidebar-control",
        picker_UI(id = shiny::NS(id, "ger"),
                  var_list = regions,
                  div_style = "display: inline-block; width: 50%;"),
        picker_UI(id = shiny::NS(id, "get"),
                  var_list = top_scales,
                  div_style = "display: inline-block; width: 48%;")
      )
    )
  )
}
