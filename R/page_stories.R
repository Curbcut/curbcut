#' Stories Server
#'
#' This function is responsible for the server-side logic for handling stories
#' in the application. It manages the interaction, including sidebars, maps,
#' clicks, rendering stories, handling images and various reactive behaviors.
#'
#' @param id <`character`> The ID of the page in which the legend will appear,
#' e.g. `alp`.
#' @param r <`reactiveValues`> The reactive values shared between modules and
#' pages. Created in the `server.R` file. The output of \code{\link{r_init}}.
#'
#' @export
stories_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    stories <- get_from_globalenv("stories")
    id_map <- paste0(id, "-map")
    themes_raw <- unique(unlist(stories$themes))
    themes <- shiny::reactive(list(Themes = sapply(themes_raw, list)))
    mapbox_username <- get_from_globalenv("mapbox_username")
    map_token <- get_from_globalenv("map_token")
    map_loc <- get_from_globalenv("map_loc")
    map_zoom <- get_from_globalenv("map_zoom")
    map_base_style <- get_from_globalenv("map_base_style")
    inst_prefix <- get_from_globalenv("inst_prefix")

    # Sidebar
    sidebar_server(id = id, r = r)

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
        inst_prefix = inst_prefix,
        stories = stories,
        stories_min_zoom = 2
      )
    })

    # Map
    map_js_server(
      id = id,
      r = r,
      tile = shiny::reactive(NULL),
      coords = shiny::reactive(NULL),
      zoom = shiny::reactive(NULL),
      stories = stories,
      stories_min_zoom = 2
    )

    # Click reactive
    update_select_id(id = id, r = r)

    # Render the story in question
    content <- shiny::reactive({
      if (is.na(r[[id]]$select_id())) {
        return(NULL)
      }

      rmd_name <- stories$name_id[stories$ID == r[[id]]$select_id()]
      story_link <- paste0("stories/", rmd_name, "_", r$lang(), ".html")

      shiny::tags$iframe(
        style = "width:100%;height:100%;",
        title = "stories",
        src = story_link,
        frameborder = 0
      )
    })
    show_popup <- shiny::reactive(!is.na(r[[id]]$select_id()))
    popup_server(
      id = id,
      content = content,
      show_popup = show_popup
    )

    # Prepare reactive values to get which stories are available
    photo_ids_rv <- shiny::reactiveVal(character())
    list_photos_rv <- shiny::reactiveVal(character())

    shiny::observe({
      if (is.na(r[[id]]$select_id())) {
        return(NULL)
      }

      # Get photos ID + links
      name_id <- stories$name_id[stories$ID == r[[id]]$select_id()]
      list_photos <- list.files(sprintf("www/stories/photos/%s", name_id),
        full.names = TRUE
      )
      list_photos <- gsub("www/", "", list_photos)
      photo_ids <- gsub(sprintf(".*/%s/", name_id), "", list_photos)

      # Set the data as reactive values so event listeners can be attached to
      # all of them
      photo_ids_rv(photo_ids)
      names(list_photos) <- photo_ids
      list_photos_rv(list_photos)
    })

    # Right panel output when a stories is selected
    output$rp_output <- shiny::renderUI({
      if (is.na(r[[id]]$select_id())) {
        return(NULL)
      }

      shiny::tagList(
        shiny::div(
          id = shiny::NS(id, "rp_stories_full"),
          # shiny::div(id = shiny::NS(id, "exploratory_walk"),
          #            ),
          # shiny::hr(),
          shiny::div(
            id = shiny::NS(id, "image_gallery_title"),
            shiny::fluidRow(
              shiny::column(
                width = 7,
                shiny::h4(
                  icon_material_title("photo_camera"),
                  cc_t("Image gallery", lang = r$lang())
                )
              )
            )
          ),
          shiny::div(
            id = shiny::NS(id, "images"),
            shiny::tagList(
              mapply(\(id, photo_link) {
                sprintf(
                  "<img src = '%s' id = 'stories-%s' style = 'width:20%%; margin:5px; cursor:pointer'>",
                  photo_link, id
                ) |>
                  shiny::HTML()
              }, photo_ids_rv(), list_photos_rv(), SIMPLIFY = FALSE)
            )
          )
        )
      )
    })

    # Set listeners for every active photos
    shiny::observe({
      lapply(photo_ids_rv(), function(photo_id) {
        shinyjs::onclick(photo_id, {
          shiny::showModal(shiny::modalDialog(
            title = "Photo",
            shiny::HTML(paste0('<img src="', list_photos_rv()[photo_id], '" width = 100%>')),
            easyClose = TRUE,
            size = "l",
            footer = shiny::modalButton(cc_t("Close", lang = r$lang()))
          ))
        })
      })
    })

    # When to show the right panel
    shiny::observe({
      show_panel <- (\(x) {
        if (is.na(r[[id]]$select_id())) {
          return(FALSE)
        }
        if (length(photo_ids_rv()) == 0) {
          return(FALSE)
        }
        return(TRUE)
      })()

      shinyjs::toggle("right_panel", condition = show_panel)
    })


    # Add stories on the left-hand panel and react on a click
    themes_c <- picker_server(
      id = id,
      picker_id = "var",
      r = r,
      var_list = themes,
      selected = shiny::reactive(unlist(themes()))
    )

    shiny::observe({
      in_theme <-
        stories$ID[which(
          sapply(sapply(stories$themes, `%in%`, themes_c()), sum) > 0
        )]

      show_stories <- stories$short_title[stories$ID %in% in_theme]
      show_stories <- show_stories[order(show_stories)]

      shiny::removeUI(selector = "#stories-bullet_points")
      shiny::insertUI(paste0("#stories-hr"),
        where = "afterEnd",
        shiny::tags$ul(
          id = "stories-bullet_points",
          lapply(show_stories, \(x) {
            shiny::tags$li(
              cc_t(x, lang = r$lang()),
              style = "cursor: pointer; text-decoration: none;",
              title = stories[[sprintf("preview_%s", r$lang())]][stories$short_title == x],
              onclick = paste0(
                "Shiny.setInputValue(`",
                shiny::NS(id, "clicked_linked"),
                "`, '",
                stories$ID[stories$short_title == x],
                "');"
              ),
              onmouseover = "$(this).css('text-decoration', 'underline');",
              onmouseout = "$(this).css('text-decoration', 'none');"
            )
          })
        )
      )
    })

    # If language changes, update the selection to NA
    shiny::observeEvent(r$lang(), r[[id]]$select_id(NA), ignoreInit = TRUE)

    # If there's a click on the story in the sidebar
    shiny::observeEvent(input$clicked_linked, {
      r[[id]]$select_id(input$clicked_linked)
    })

    # Update the select_id if clicked on a story title in the top navigation panel
    shiny::observeEvent(input$select_nav, {
      r[[id]]$select_id(input$select_nav)
    })

    # When the popup is closed, return the selection to NA
    shiny::observeEvent(input[["stories-back"]], {
      r[[id]]$select_id(NA)
    })

    # Bookmarking
    bookmark_server(
      id = id,
      r = r,
      select_id = r[[id]]$select_id,
      exclude_input = "ccpicker_var"
    )
  })
}

#' Stories UI
#'
#' This function creates the UI for handling stories within the application.
#' It includes the sidebar, map and main panel configurations and dynamically
#' sets themes and other attributes based on the provided stories data.
#'
#' @param id <`character`> The ID of the page in which the legend will appear,
#' e.g. `alp`.
#'
#' @return A tagList object containing the UI elements.
#' @export
stories_UI <- function(id) {
  stories <- get_from_globalenv("stories")
  modules <- get_from_globalenv("modules")
  themes <- unique(unlist(stories$themes))
  themes <- list(Themes = stats::setNames(themes, themes))
  page <- modules[modules$id == id, ]
  theme_lowercased <- gsub(" .*", "", tolower(page$theme))

  shiny::tagList(
    shiny::div(
      `data-theme` = theme_lowercased,
      # Sidebar
      sidebar_UI(
        id = shiny::NS(id, id),
        picker_UI(
          id = shiny::NS(id, id),
          label = cc_t("Choose themes:"),
          var_list = themes,
          selected = unlist(themes),
          multiple = TRUE
        ),
        shiny::hr(id = shiny::NS(id, "hr"))
      ),

      # Map
      map_js_UI(id = shiny::NS(id, id)),

      # Main panel
      popup_UI(id = shiny::NS(id, id)),

      # Right panel when the stories are shown
      shinyjs::hidden(
        right_panel(
          id = id,
          style = "top: var(--side-bar-top) !important;",
          shiny::htmlOutput(shiny::NS(id, "rp_output"))
        )
      )
    )
  )
}
