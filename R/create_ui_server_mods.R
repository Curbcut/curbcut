#' Assigns UI and server functions in the global environment for all pages in `modules`
#'
#' This function takes the `modules` dataframe as input and creates UI and server
#' functions for every page it can. The UI function it assigns to the global environment
#' generates the layout of the page, while the server function handles the logic
#' behind it. The function then assigns the generated UI and server functions
#' to the global environment, using the page ID as the function name.
#'
#' @param modules <`data.frame`> The data.frame containing all the pages information.
#'
#' @return This function assigns UI and server functions to the global
#' environment and returns \code{invisible()}.
#' @export
create_ui_server_mods <- function(modules) {

  # Create the `basic` function
  ui <- function(id) {
    default_region <- modules$regions[modules$id == id][[1]][1]
    mzp <- eval(parse(text = paste0("map_zoom_levels_", default_region)))

    shiny::tagList(
      # Sidebar
      curbcut::sidebar_UI(
        id = shiny::NS(id, id),
        curbcut::autovars_UI(shiny::NS(id, id)),
        curbcut::warnuser_UI(shiny::NS(id, id)),
        bottom = shiny::tagList(
          curbcut::legend_UI(shiny::NS(id, id)),
          curbcut::zoom_UI(shiny::NS(id, id), zoom_levels = mzp)
        )
      ),

      # Map
      curbcut::map_UI(NS(id, id)),

      # Change view (Map/Data/Place explorer)
      curbcut::panel_view_UI(id = NS(id, id)),

      # Right panel
      curbcut::right_panel(
        id = id,
        curbcut::compare_UI(
          id = NS(id, id),
          var_list = curbcut::dropdown_make(vars = " ", compare = TRUE)
        ),
        curbcut::explore_UI(NS(id, id)),
        curbcut::dyk_UI(NS(id, id))
      )
    )
  }

  # Create the basic server function
  server <- function(id, r) {
    shiny::moduleServer(id, function(input, output, session) {
      default_region <- modules$regions[modules$id == id][[1]][1]
      mzp <- eval(parse(text = paste0("map_zoom_levels_", default_region)))
      main_dropdown_title <- modules$main_dropdown_title[modules$id == id]
      default_year <- modules$dates[modules$id == id][[1]]
      default_year <- max(default_year)

      # Initial reactives
      rv_zoom_string <- shiny::reactiveVal(
        curbcut::zoom_get_string(
          zoom = map_zoom,
          zoom_levels = mzp,
          region = default_region
        )
      )

      # Zoom and POI reactives when the view state of the map changes.
      shiny::observeEvent(map_viewstate(), {
        r[[id]]$zoom(curbcut::zoom_get(zoom = map_viewstate()$zoom))
        r[[id]]$poi(curbcut::update_poi(
          id = id, poi = r[[id]]$poi(),
          map_viewstate = map_viewstate()
        ))
      })

      # Map zoom levels change depending on r$region()
      zoom_levels <-
        shiny::reactive(curbcut::zoom_get_levels(id = id, region = r$region()))

      # Zoom string reactive
      shiny::observe({
        rv_zoom_string({
          curbcut::zoom_get_string(
            zoom = r[[id]]$zoom(),
            zoom_levels = zoom_levels()$zoom_levels,
            region = zoom_levels()$region
          )
        })
      })

      # Update selected ID
      curbcut::update_select_id(id = id, r = r, data = data)

      # Choose tileset
      tile <- curbcut::zoom_server(
        id = id,
        r = r,
        zoom_string = rv_zoom_string,
        zoom_levels = zoom_levels
      )

      # Get df
      shiny::observeEvent(
        {
          tile()
          rv_zoom_string()
        },
        {
          r[[id]]$df(curbcut::update_df(
            tile = tile(),
            zoom_string = rv_zoom_string()
          ))
        }
      )

      # Construct the left-hand UIs / servers automatically
      autovars <-
        curbcut::autovars_server(
          id = id,
          r = r,
          main_dropdown_title = main_dropdown_title,
          default_year = default_year)

      var_left <- shiny::reactive(autovars()$var)
      time <- shiny::reactive(autovars()$time)

      # Right variable / compare panel
      var_right <- curbcut::compare_server(
        id = id,
        r = r,
        var_list = curbcut::dropdown_make(
          vars = c(
            "inc_median_income", "inc_50", "inc_100",
            "inc_high", "inc_limat", "iden_imm",
            "iden_imm_new", "iden_vm", "iden_aboriginal",
            "trans_car", "trans_walk_or_bike", "trans_transit",
            "trans_t_15", "trans_t_45", "trans_t_45_plus",
            "family_children", "family_one_person", "lang_french_only",
            "lang_eng_only", "lang_french_eng", "lang_no_official",
            "age_0_14", "age_15_64", "age_65_plus",
            "edu_bachelor_above", "edu_no_degree"
          ),
          compare = TRUE
        ),
        time = time
      )

      # Update the `r[[id]]$vars` reactive
      curbcut::update_vars(id = id, r = r, var_left = var_left,
                           var_right = var_right)

      # Sidebar
      curbcut::sidebar_server(id = id, r = r)

      # Data
      data <- reactive(curbcut::data_get(
        vars = r[[id]]$vars(),
        df = r[[id]]$df()
      ))

      # Data for tile coloring
      data_colours <- shiny::reactive(curbcut::data_get_colours(
        vars = r[[id]]$vars(),
        region = zoom_levels()$region,
        zoom_levels = zoom_levels()$zoom_levels
      ))

      # Warn user
      curbcut::warnuser_server(
        id = id,
        r = r,
        vars = r[[id]]$vars,
        time = time,
        data = data
      )

      # Legend
      curbcut::legend_server(
        id = id,
        r = r,
        vars = r[[id]]$vars,
        data = data,
        df = r[[id]]$df
      )

      # Did-you-know panel
      curbcut::dyk_server(
        id = id,
        r = r,
        vars = r[[id]]$vars,
        poi = r[[id]]$poi,
        df = r[[id]]$df
      )

      # Update map in response to variable changes or zooming
      map_viewstate <- curbcut::map_server(
        id = id,
        tile = tile,
        data_colours = data_colours,
        select_id = r[[id]]$select_id,
        zoom_levels = reactive(zoom_levels()$zoom_levels),
        zoom = r[[id]]$zoom,
        coords = r[[id]]$coords
      )

      # Update map labels
      curbcut::label_server(
        id = id,
        tile = tile,
        zoom = r[[id]]$zoom,
        zoom_levels = reactive(zoom_levels()$zoom_levels),
        region = reactive(zoom_levels()$region)
      )

      # Explore panel
      curbcut::explore_server(
        id = id,
        r = r,
        data = data,
        region = reactive(zoom_levels()$region),
        vars = r[[id]]$vars,
        df = r[[id]]$df,
        select_id = r[[id]]$select_id
      )

      # Bookmarking
      curbcut::bookmark_server(
        id = id,
        r = r,
        select_id = r[[id]]$select_id,
        map_viewstate = map_viewstate
      )

      # Change view
      curbcut::panel_view_server(
        id = id,
        r = r,
        vars = r[[id]]$vars,
        data = data,
        zoom_levels = reactive(zoom_levels()$zoom_levels)
      )
    })
  }

  # Only keep the modules that can be worked using autovars (with var_left)
  create <- sapply(modules$id, \(i) !is.null(modules$var_left[modules$id == i][[1]]))
  ids <- names(create)

  # Iterate over the ids to assign the functions in the global environment
  lapply(ids, \(id) assign(sprintf("%s_UI", id), ui, envir = .GlobalEnv))
  lapply(ids, \(id) assign(sprintf("%s_server", id), server, envir = .GlobalEnv))

  return(invisible())
}
