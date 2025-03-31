#' Renders a floating panel with a map view and data view buttons.
#'
#' This function renders a Shiny server module for the `panel_view_UI` UI
#' element which displays a floating panel with buttons to toggle between the map
#' an table/data info. It shows a map when the map button is clicked and a table
#' with data when the data button is clicked. When there is a selection, it also
#' shows a magnifying glass linking to the place explorer.
#'
#' @param id <`character`> The ID of the page in which the panel will appear,
#' e.g. `alp`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#' @param region <`reactive character`> Character string specifying the name of the region.
#' Usually equivalent of `r$region()`.
#' @param scale <`reactive character`> Current scale. The output of
#' \code{\link{update_scale}}.
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link{vars_build}} function, `vars` subset.. The class of the vars object is
#' used to determine which type of legend to draw.
#' @param data <`reactive data.frame`> Data frame containing all the scale and
#' the `var_left` and `var_right`. The output of \code{\link{data_get}}.
#' @param zoom_levels <`named numeric vector`> A named numeric vector of zoom
#' levels. Usually one of the `mzl_*`, or the output of
#' \code{\link{geography_server}}.
#' @param time <`reactive named list`> Named list of the time for both left and
#' right variables. The second output of \code{\link{vars_build}}. Usually
#' should be `r[[id]]$time()`.
#' @param schemas <`reactive named list`> Current schema information. The additional widget
#' values that have an impact on which data column to pick. Usually `r[[id]]$schema()`.
#' @param temp_folder <`character`> The temporary folder of the app. By default
#' will grab the `temp_folder` object as it's already supposed to have been assigned
#' in the `global.R` file
#' @param scales_as_DA <`reactive character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' the data shown will be `DA`.
#'
#' @return Panel view module
#' @export
panel_view_server <- function(
  id,
  r,
  region,
  scale,
  vars,
  data,
  zoom_levels,
  time,
  schemas,
  temp_folder = get_from_globalenv("temp_folder"),
  scales_as_DA = shiny::reactive(c("building", "street"))
) {
  stopifnot(shiny::is.reactive(data))
  stopifnot(shiny::is.reactive(region))
  stopifnot(shiny::is.reactive(vars))
  stopifnot(shiny::is.reactive(zoom_levels))
  stopifnot(shiny::is.reactive(scales_as_DA))

  shiny::moduleServer(id, function(input, output, session) {
    # Switch scales to DA if necessary
    treated_scale <-
      shiny::reactive(treat_to_DA(
        scales_as_DA = scales_as_DA(),
        scale = scale()
      ))

    # Show the map when the right button is clicked
    shiny::observeEvent(input$panel_map, {
      shinyjs::show(id = "map_div", anim = TRUE, animType = "fade")
      shinyjs::hide(id = "view_data", anim = TRUE, animType = "fade")
      shinyjs::removeClass(id = "panel_data", class = "selection")
      shinyjs::addClass(id = "panel_map", class = "selection")

      # Show the tutorial bubble
      shinyjs::show(id = "tutorial")
    })

    # Hide the map and show the data when the right button is clicked
    shiny::observeEvent(input$panel_data, {
      shinyjs::hide(id = "map_div", anim = TRUE, animType = "fade")
      shinyjs::show(id = "view_data", anim = TRUE, animType = "fade")
      shinyjs::removeClass(id = "panel_map", class = "selection")
      shinyjs::addClass(id = "panel_data", class = "selection")

      # Hide the tutorial bubble
      shinyjs::hide(id = "tutorial")
    })

    # Bring the user to the place explorer when there is a selection and that
    # selection is in `data`
    shiny::observe({
      pe_docs <- get_from_globalenv("pe_docs")
      # When to show the place portrait button?
      show <- sprintf(
        "www/place_explorer/%s_%s_%s_%s.html",
        region(),
        scale(),
        r[[id]]$select_id(),
        r$lang()
      ) %in%
        pe_docs

      shinyjs::toggle(
        id = "panel_selection",
        condition = show,
        anim = TRUE,
        animType = "fade"
      )
    })

    # If the 'Portrait' button is clicked, show the portrait and invite to
    # the place explorer
    shiny::observeEvent(input$panel_selection, {
      # Get the place explorer HTML document
      pe_src <- place_explorer_html_links(
        temp_folder = r$temp_folder,
        region = region(),
        scale = scale(),
        select_id = r[[id]]$select_id(),
        lang = r$lang()
      )$src

      # Popup the modal
      shiny::showModal(shiny::modalDialog(
        # Hack the namespace of the button so that it's detectable from within
        # this module (nested in another page, so double ns)
        action_button(
          classes = c("floating-bar-btn", "visit-place-ex"),
          id = shiny::NS(id, shiny::NS(id, "go_pe")),
          icon = "search",
          text_class = "floating-panel-text",
          text = cc_t("Visit the place explorer", lang = r$lang())
        ),
        shiny::tags$iframe(
          style = "width:100%;height:calc(100vh - 260px)",
          title = "place_ex",
          src = pe_src,
          frameborder = 0
        ),
        footer = shiny::modalButton(cc_t(lang = r$lang(), "Close")),
        size = "xl",
        easyClose = TRUE
      ))
    })

    # If the user click on the 'visit the place explorer' button from the modal
    shiny::observeEvent(input$go_pe, {
      shiny::removeModal()
      link(r = r, page = "place_explorer")
    })

    # If the data is private
    private_data <- shiny::reactive({
      vars_ <- vars()[vars() != " "]
      private <- all(sapply(vars_, var_get_info, what = "private"))
      return(private)
    })

    # Toggle the table depending if the info is private
    shiny::observeEvent(private_data(), {
      shinyjs::toggle(id = "data_table", condition = !private_data())
    })

    # Hide the data if it is private. Prepare the data to be shown and to
    # be downloaded
    datas <- shiny::reactive({
      # If the dataset is private, return an empty dataframe
      if (private_data()) {
        return(data.frame())
      }

      # Prepare the pretty table and the download table
      dat <- table_view_prep_table(
        vars = vars(),
        data = data(),
        scale = treated_scale(),
        zoom_levels = zoom_levels(),
        lang = r$lang(),
        time = time(),
        schemas = schemas()
      )

      # Return
      return(dat)
    })

    # Show the text information
    output$data_info <- shiny::renderUI({
      modules <- get_from_globalenv("modules")
      # Spatial organization of data
      scale <-
        tolower(curbcut::cc_t(lang = r$lang(), zoom_get_name(scale())))
      scale <- sprintf(
        cc_t(
          "The spatial organization of the data is the %s scale.",
          lang = r$lang()
        ),
        scale
      )

      shiny::tagList(
        shiny::h4(cc_t("Overview", lang = r$lang())),
        # About the module
        shiny::HTML(cc_t(
          modules$dataset_info[modules$id == id],
          lang = r$lang()
        )),
        shiny::p(scale),
        # About the variables
        shiny::HTML(datas()$text)
      )
    })

    # Place the selection first in the pretty_data
    pretty_data <- shiny::reactive({
      if (is.na(r[[id]]$select_id())) {
        return(datas()$pretty_data)
      }

      # Place the selection first
      dat <- datas()$pretty_data
      s_id <- which(dat$ID == r[[id]]$select_id())
      no_s_id <- which(dat$ID != r[[id]]$select_id())
      dat <- dat[c(s_id, no_s_id), ]

      return(dat)
    })

    # If there is a selection, pre-select it
    update_selection_list <- shiny::reactive({
      if (is.na(r[[id]]$select_id())) {
        return("single")
      }
      sel <- which(pretty_data()$ID == r[[id]]$select_id())
      list(mode = "single", selected = sel, target = "row")
    })

    # Make the data a `DT::datatable` and style every column
    datatable_styled <- shiny::reactive({
      dat <- DT::datatable(
        pretty_data(),
        selection = update_selection_list(),
        options = list(
          autoWidth = TRUE,
          language = list(
            url = sprintf(
              "//cdn.datatables.net/plug-ins/1.13.4/i18n/%s-%s.json",
              tolower(r$lang()),
              toupper(r$lang())
            )
          )
        ),
        rownames = FALSE
      )

      for (i in datas()$title_vars) {
        # Is the column in the data? (Sometimes population and households are
        # missing, but they are automatically added as counts as the default is
        # that they will be present)
        if (i %in% names(dat$x$data)) {
          dat <- panel_view_style_cols(var = i, table = dat)
        }
      }

      return(dat)
    })

    # Show the table
    output$data_table <- DT::renderDT(
      {
        # Recalculate every time the button is pressed
        input$panel_data
        datatable_styled()

        # Test server FALSE for Ajax problem
      },
      server = FALSE
    )

    # If there is a selection in the table, update the selection
    shiny::observeEvent(
      input$data_table_rows_selected,
      {
        # If deselected from the table, return an NA selection
        if (is.null(input$data_table_rows_selected)) {
          return(r[[id]]$select_id(NA))
        }

        # If there is a selection, update the selected id
        new_id <- pretty_data()$ID[input$data_table_rows_selected]
        r[[id]]$select_id(new_id)

        # If there is a selection, update the central coordinates of the map
        df_data <- grab_df_from_bslike(scale = treated_scale())
        # Skip the zoom update if the 'centroid' is not in the df
        if (!"centroid" %in% names(df_data)) {
          return(NULL)
        }
        coords <- df_data$centroid[df_data$ID == new_id][[1]]
        coords <- sapply(coords, round, digits = 2)
        cc.map::map_viewstate(
          session = session,
          map_ID = "map",
          longitude = as.numeric(coords[1]),
          latitude = as.numeric(coords[2]),
          zoom = r[[id]]$zoom()
        )
        cc.map::map_choropleth_update_selection(
          session = session,
          map_ID = "map",
          select_id = new_id
        )
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    # When the user clicks to download the .csv
    output$download_csv <-
      shiny::downloadHandler(
        filename = paste0(id, "_data.csv"),
        content = function(file) {
          data <- datas()$data
          utils::write.csv(data, file, row.names = FALSE)
        },
        contentType = "text/csv"
      )

    # When the user clicks to download the .shp
    output$download_shp <-
      shiny::downloadHandler(
        filename = shiny::reactive(paste0(id, "_shp.zip")),
        content = function(file) {
          # Add progress as it's not instant
          shiny::withProgress(
            message = cc_t("Exporting data", lang = r$lang()),
            {
              shiny::incProgress(0.4)

              # Prepare data by attaching geometries
              requireNamespace("sf", quietly = TRUE)
              geo <- qs::qread(sprintf(
                "data/geometry_export/%s.qs",
                treated_scale()
              ))
              geo <- geo["ID"]
              data <- merge(datas()$data, geo, by = "ID")
              data <- sf::st_as_sf(data)

              shiny::incProgress(0.3)

              # Set file names
              tmp_path <- dirname(file)
              name_base <- file.path(tmp_path, paste0(id, "_data"))
              name_glob <- paste0(name_base, ".*")
              name_shp <- paste0(name_base, ".shp")
              name_zip <- paste0(name_base, ".zip")

              # Remove any previously generated files with the same name
              if (length(Sys.glob(name_glob)) > 0)
                file.remove(Sys.glob(name_glob))

              # Write the data to a shapefile
              sf::st_write(
                data,
                dsn = name_shp,
                driver = "ESRI Shapefile",
                quiet = TRUE
              )

              # Zip the shapefile and copy to the desired location
              zip::zipr(zipfile = name_zip, files = Sys.glob(name_glob))
              shiny::req(file.copy(name_zip, file))

              shiny::incProgress(0.3)

              # Remove any temporary files
              if (length(Sys.glob(name_glob)) > 0)
                file.remove(Sys.glob(name_glob))
            }
          )
        },
        contentType = "application/zip"
      )
  })
}

#' @describeIn panel_view_server Create the UI for the legend module
#' @export
panel_view_UI <- function(id) {
  shiny::tagList(
    shiny::tags$div(
      class = "floating-panel",
      shiny::tags$div(
        class = "floating-panel-content",
        id = shiny::NS(id, "floating-panel-content"),
        # Map
        action_button(
          classes = c(
            "floating-bar",
            "floating-bar-btn",
            "map-btn",
            "selection"
          ),
          id = shiny::NS(id, "panel_map"),
          icon = "map",
          text_class = "floating-panel-text",
          text = cc_t("Map", force_span = T)
        ),
        # Data
        action_button(
          classes = c("floating-bar", "floating-bar-btn", "data-btn"),
          id = shiny::NS(id, "panel_data"),
          icon = "table_view",
          text_class = "floating-panel-text",
          text = cc_t("Table", force_span = T)
        ),
        # Explore data link
        shinyjs::hidden(action_button_task(
          classes = c(
            "floating-bar",
            "floating-bar-btn",
            "portrait-btn"
          ),
          id = shiny::NS(id, "panel_selection"),
          icon = "search",
          text_class = "floating-panel-text",
          text = cc_t("Portrait", force_span = T)
        ))
      )
    ),

    # To accompany the panel data button, create the div
    shinyjs::hidden(
      shiny::div(
        class = "panel_view scrollable-div",
        id = shiny::NS(id, "view_data"),
        shiny::div(
          style = "margin-bottom:20px;",
          DT::DTOutput(
            outputId = shiny::NS(id, "data_table")
          )
        ),
        shiny::div(
          style = "text-align:right",
          shiny::downloadButton(
            class = "cc-download-btn",
            outputId = shiny::NS(id, "download_csv"),
            label = cc_t("Download '.csv'", force_span = T)
          ),
          shiny::downloadButton(
            class = "cc-download-btn",
            outputId = shiny::NS(id, "download_shp"),
            label = cc_t("Download '.shp'", force_span = T)
          )
        ),
        shiny::htmlOutput(
          outputId = shiny::NS(id, "data_info"),
          fill = TRUE
        )
      )
    )
  )
}
