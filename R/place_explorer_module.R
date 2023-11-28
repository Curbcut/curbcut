#' Launches the place explorer module server
#'
#' This function initializes the server logic for the place explorer module,
#' which is a shiny module that allows users to explore geographical areas and
#' retrieve data about them. The module consists of a map, a sidebar, and a main
#' panel that displays detailed information about a selected area.
#'
#' @param id <`character`> The ID of the page in which this module will appear,
#' e.g. `alp`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param scales_as_DA <`reactive character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. These
#' scales won't be selectable by the user.
#' @param map_zoom <`numeric`> The default zoom level for the map. By default,
#' this value is retrieved from the \code{map_zoom} variable in the global environment.
#' @param map_loc <`numeric vector`> The default location for the map. By default,
#' this value is retrieved from the \code{map_loc} variable in the global environment.
#' @param mapbox_username <`character`> Mapbox account username. Defaults to
#' grabbing the `mapbox_username` object from the global environment.
#' @param tileset_prefix <`character`> Prefix attached to every tileset. Should
#' correspond to the Curbcut city, e.g. `mtl`. Defaults to grabbing the
#' `tileset_prefix` object from the global environment.
#' @param map_base_style <`character`> The mapbox basemap style url.
#' See https://docs.mapbox.com/api/maps/#mapbox-styles
#' @param temp_folder <`character`> The temporary folder of the app. By default
#' will grab the `temp_folder` object as it's already supposed to have been assigned
#' in the `global.R` file
#'
#' @export
place_explorer_server <- function(id, r,
                                  scales_as_DA = shiny::reactive(
                                    c("building", "street")
                                  ),
                                  map_zoom = get_from_globalenv("map_zoom"),
                                  map_loc = get_from_globalenv("map_loc"),
                                  mapbox_username = get_from_globalenv("mapbox_username"),
                                  tileset_prefix = get_from_globalenv("tileset_prefix"),
                                  map_base_style = get_from_globalenv("map_base_style"),
                                  temp_folder = get_from_globalenv("temp_folder")) {
  shiny::moduleServer(id, function(input, output, session) {
    map_token <- get_from_globalenv("map_token")
    modules <- get_from_globalenv("modules")
    page <- modules[modules$id == id, ]
    regions <- page$regions[[1]]
    if (is.null(regions)) {
      stop(sprintf(paste0(
        "Page `%s` does not have available regions. Please ",
        "check the `regions` column in the `modules` ",
        "dataframe.", id
      )))
    }
    avail_scale_combinations <- page$avail_scale_combinations[[1]]
    mzp <- get_from_globalenv(sprintf("mzl_%s", avail_scale_combinations[1]))

    # Declare the map id
    id_map <- paste0(id, "-map")

    # Initiate the map.
    output[[shiny::NS(id, "map_ph")]] <- shiny::renderUI({
      cc.map::map_input(
        map_ID = shiny::NS(id, shiny::NS(id, "map")),
        username = mapbox_username,
        token = map_token,
        longitude = map_loc[1],
        latitude = map_loc[2],
        zoom = map_zoom,
        map_style_id = map_base_style,
        tileset_prefix = tileset_prefix,
        stories = NULL
      )
    })

    # Region and zoom levels change depending on the geography widget
    zl <- geography_server(
      id = id,
      r = r,
      regions = regions,
      avail_scale_combinations = avail_scale_combinations
    )

    tile <- shiny::reactive(names(zl()$zoom_levels)[[1]])

    # Misc --------------------------------------------------------------------

    # Sidebar
    sidebar_server(id = "place_explorer", r = r)


    # Postal code search ------------------------------------------------------

    shiny::observeEvent(input$search_button, {
      postal_c <- tolower(input$address_searched)
      postal_c <- s_extract_all("\\w|\\d", postal_c)
      postal_c <- paste(postal_c, collapse = "")

      postal_codes <- get_from_globalenv("postal_codes")
      DA_id <- postal_codes$DA_ID[postal_codes$postal_code == postal_c]

      if (length(DA_id) == 0) {
        address <- input$address_searched
        shiny::showNotification(
          cc_t(
            lang = r$lang(),
            paste0("No postal code found for `{address}`")
          ),
          type = "error"
        )
      } else {
        DA_table <- get_from_globalenv("DA")
        scale <- r[[id]]$scale()
        right_id <- DA_table[[paste0(scale, "_ID")]][DA_table$ID == DA_id][[1]][1]
        r[[id]]$select_id(right_id)
      }
    })


    # Map ---------------------------------------------------------------------

    dat <- shiny::reactive({
      data <- get_from_globalenv(tile())
      data <- filter_region(data = data, scale = tile(), region = zl()$region)
      data <- data[c("ID")]
      names(data) <- "ID_color"
      data$fill <- hex8_to_rgba("#AAB6CF90")
      data
    })

    map_js_server(
      id = id,
      r = r,
      tile = tile,
      data_colours = dat,
      coords = r[[id]]$coords,
      zoom = r[[id]]$zoom,
      stories = NULL
    )

    # Map click
    update_select_id(id = id, r = r)


    # Select ID behavior ------------------------------------------------------

    # Hide main panel when "Go back to map" button is clicked
    shiny::observeEvent(input$back, r[[id]]$select_id(NA))
    shiny::observeEvent(r[[id]]$select_id(), {
      shinyjs::toggle("back", condition = !is.na(r[[id]]$select_id()))
      shinyjs::toggle("place_exp_main_panel", condition = !is.na(r[[id]]$select_id()))
      shinyjs::toggle("zoom_slider", condition = is.na(r[[id]]$select_id()))
    })


    # Main panel --------------------------------------------------------------

    # Add a loader page, which is an empty document with white background that
    # appears with an animation of 2 seconds. It gives time for the place
    # explorer to be fully concatenated and shown.
    main_panel <- shiny::reactive({
      if (!is.na(r[[id]]$select_id())) {
        # If selection outside region
        if (!r[[id]]$select_id() %in% dat()$ID_color) {
          return(NULL)
        }

        pe_links <- place_explorer_html_links(
          temp_folder = temp_folder,
          region = zl()$region,
          scale = tile(),
          select_id = r[[id]]$select_id(),
          lang = r$lang()
        )

        # Show the file
        return(list(
          div = shiny::div(
            class = "main_panel_popup",
            shiny::div(
              class = "back-to-map",
              shiny::actionLink(
                shiny::NS(id, "back"), "X"
              )
            ),
            shiny::downloadButton(
              class = "cc-download-btn",
              style = "right:75px;position:absolute;top:15px;min-height:auto;",
              outputId = shiny::NS(id, "download_portrait"),
              label = cc_t("Download regional portrait", lang = r$lang())
            ),
            shiny::tags$iframe(
              style = "width:100%;height: calc(100% - 38px); margin-top: 38px;",
              title = "place_ex",
              src = pe_links$src,
              frameborder = 0
            )
          ),
          # temp_folder_shortcut is tempdir()
          file = pe_links$file
        ))
      } else {
        NULL
      }
    })

    output$main_panel <- shiny::renderUI(main_panel()$div)


    # Download ----------------------------------------------------------------

    output$download_portrait <-
      shiny::downloadHandler(
        filename = shiny::reactive(paste0(
          r[[id]]$df(), "_",
          r[[id]]$select_id(), ".html"
        )),
        content = function(file) {
          html_content <- readLines(main_panel()$file)
          writeLines(html_content, file)
        }, contentType = "text/html"
      )


    # Bookmarking -------------------------------------------------------------

    bookmark_server(
      id = id,
      r = r,
      select_id = r[[id]]$select_id
    )
  })
}

#' @describeIn place_explorer_server Create the UI for the place explorer module
#' @export
place_explorer_UI <- function(id, scales_as_DA = c("building", "street")) {
  # Get default values for the place explorer
  modules <- get_from_globalenv("modules")
  page <- modules[modules$id == id, ]
  regions <- page$regions[[1]]
  if (is.null(regions)) {
    stop(sprintf(paste0(
      "Page `%s` does not have available regions. Please ",
      "check the `regions` column in the `modules` ",
      "dataframe."
    ), id))
  }
  avail_scale_combinations <- page$avail_scale_combinations[[1]]
  mzp <- get_from_globalenv(sprintf("mzl_%s", avail_scale_combinations[1]))
  theme_lowercased <- gsub(" .*", "", tolower(page$theme))

  shiny::tagList(
    shiny::div(
      `data-theme` = theme_lowercased,

      # Sidebar
      sidebar_UI(
        shiny::NS(id, id),
        # Search box
        shiny::strong(cc_t("Enter postal code or click on the map")),
        # Imitate a split layout which only works this way on iOS
        shiny::HTML(paste0(
          '<div class="shiny-split-layout">
                     <div style="width: 80%;">',
          shiny::textInput(
            inputId = shiny::NS(id, "address_searched"),
            label = NULL, placeholder = "H3A 2T5"
          ),
          '</div><div style="width: 20%;">',
          shiny::actionButton(
            inputId = shiny::NS(id, "search_button"),
            label = shiny::icon("search", verify_fa = FALSE),
          ),
          "</div></div>"
        )),
        shiny::hr(),
        geography_UI(shiny::NS(id, id),
          regions = regions,
          avail_scale_combinations = avail_scale_combinations
        ),
      ),

      # Map
      map_js_UI(id = shiny::NS(id, id)),

      # Main panel
      shinyjs::hidden(
        shiny::div(
          id = shiny::NS(id, "place_exp_main_panel"),
          shiny::htmlOutput(shiny::NS(id, "main_panel")),
        )
      )
    )
  )
}
