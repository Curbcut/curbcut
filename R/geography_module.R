#' Geography module for CUrbcut
#'
#' This function creates a shiny module server to handle geography settings.
#' It initializes two dropdown menus for selecting regions and top scale.
#' A calculated map zoom level is also provided.
#'
#' @param id <`character`> The ID of the page in which this module will appear,
#' e.g. `alp`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param regions <`character vector`> Possible available regions for selection.
#' @param avail_scale_combinations <`character vector`> Available scale combinations
#' for the top scale.
#'
#' @return A reactive list containing selected region and calculated zoom levels.
#' @export
geography_server <- function(id, r = r, regions, avail_scale_combinations) {
  shiny::moduleServer(id, function(input, output, session) {
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
    top_scales <- list("Top scale" = top_scales)

    # Grab the value of the dropdowns
    reg_out <- picker_server(id = "ger", r = r, var_list = shiny::reactive(regions))
    tp_out <- picker_server(id = "get", r = r, var_list = shiny::reactive(top_scales))

    # Grab a zoom level out of the top scale value
    mzl <- shiny::reactive({

      # Function to return the got mzl
      return_got_mzl <- \(x) get_from_globalenv(sprintf("mzl_%s", x))

      # Filter only the map zoom levels (mzl) that have the top scale
      contains <- grep(tp_out(), avail_scale_combinations, value = TRUE)

      # If there's only one choice, return it
      if (length(contains) == 1) return(return_got_mzl(contains))

      # If not, pick the map zoom levels which offers the highest number of
      # total scales on the autozoom
      underscore_nb <- sapply(contains, function(x) length(gregexpr("_", x)[[1]]))

      # Grab the string with the highest number of scales
      scales_string <- names(which.max(underscore_nb))[[1]]

      # Grab the mzl corresponding, and its value
      mzl_final <- return_got_mzl(scales_string)

      # Return
      return(mzl_final)
    })

    # Return the pickers
    return(shiny::reactive(list(region = reg_out(),
                                zoom_levels = mzl())))
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
  top_scales <- list("Top scale" = top_scales)

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
