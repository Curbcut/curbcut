#' Assigns UI and server functions in the global environment for all pages in `modules`
#'
#' This function takes the `modules` dataframe as input and creates UI and server
#' functions for every page it can. The UI function it assigns to the global environment
#' generates the layout of the page, while the server function handles the logic
#' behind it. The function then assigns the generated UI and server functions
#' to the global environment, using the page ID as the function name.
#'
#' @param modules <`data.frame`> The data.frame containing all the pages information.
#' @param pos <`numeric`> Environment position in which to assign the UIs and
#' servers. Defaults to 1, the global environment. This is an argument to appease RMD
#' check.
#'
#' @return This function assigns UI and server functions to the global
#' environment and returns \code{invisible()}.
#' @export
create_ui_server_mods <- function(modules, pos = 1) {
  # Create the `basic` function
  ui <- function(id) {
    default_region <- modules$regions[modules$id == id][[1]][1]
    mzp <- eval(parse(text = paste0("map_zoom_levels_", default_region)))
    page <- modules[modules$id == id, ]
    theme_lowercased <- gsub(" .*", "", tolower(page$theme))

    shiny::tagList(
      # Sidebar
      shiny::div(
        `data-theme` = theme_lowercased,
        curbcut::sidebar_UI(
          id = shiny::NS(id, id),
          curbcut::autovars_UI(shiny::NS(id, id)),
          curbcut::warnuser_UI(shiny::NS(id, id)),
          curbcut::compare_UI(
            id = shiny::NS(id, id),
            var_list = curbcut::dropdown_make(vars = " ", compare = TRUE)
          ),
          shiny::hr(),
          curbcut::zoom_UI(shiny::NS(id, id), zoom_levels = mzp),
          bottom = shiny::tagList(
            curbcut::legend_UI(shiny::NS(id, id))
          )
        ),

        # Map
        curbcut::map_js_UI(shiny::NS(id, id)),

        # Tutorial
        curbcut::tutorial_UI(id = shiny::NS(id, id)),

        # Change view (Map/Data/Place explorer)
        curbcut::panel_view_UI(id = shiny::NS(id, id)),

        # Right panel
        curbcut::right_panel(
          id = id,
          curbcut::explore_UI(shiny::NS(id, id)),
          curbcut::dyk_UI(shiny::NS(id, id))
        )
      )
    )
  }

  # Create the basic server function
  server <- function(id, r) {
    shiny::moduleServer(id, function(input, output, session) {
      map_zoom <- get_from_globalenv("map_zoom")
      default_region <- modules$regions[modules$id == id][[1]][1]
      mzp <- eval(parse(text = paste0("map_zoom_levels_", default_region)))
      main_dropdown_title <- modules$main_dropdown_title[modules$id == id]
      default_year <- modules$dates[modules$id == id][[1]]
      default_year <- if (is.null(default_year)) NULL else max(default_year)
      vars_right <- modules$var_right[modules$id == id][[1]]
      suffix_zoom_levels <- modules$suffix_zoom_levels[modules$id == id]

      # Initial zoom string reactive value
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
        shiny::reactive(curbcut::zoom_get_levels(
          id = id,
          region = r$region(),
          suffix_zoom_levels = suffix_zoom_levels
        ))

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
        zoom_levels = zoom_levels,
        suffix_zoom_levels = suffix_zoom_levels
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
          default_year = default_year
        )

      var_left <- shiny::reactive(autovars()$var)
      time <- shiny::reactive(if (is.null(autovars()$time)) "" else autovars()$time)

      # Right variable / compare panel
      var_right <- curbcut::compare_server(
        id = id,
        r = r,
        var_list = shiny::reactive(curbcut::dropdown_make(
          vars = vars_right,
          compare = TRUE
        )),
        # If there are no time in the page, use the latest census for date of
        # comparisons
        time = if (time() != "") time else shiny::reactive(2021)
      )

      # Update the `r[[id]]$vars` reactive
      curbcut::update_vars(
        id = id, r = r, var_left = var_left,
        var_right = var_right
      )

      # Sidebar
      curbcut::sidebar_server(id = id, r = r)

      # Data
      data <- shiny::reactive(curbcut::data_get(
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

      # Tutorial
      curbcut::tutorial_server(
        id = id,
        r = r
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
      map_viewstate <- curbcut::map_js_server(
        id = id,
        r = r,
        tile = tile,
        data_colours = data_colours
      )

      # Update map labels
      curbcut::label_server(
        id = id,
        tile = tile,
        zoom = r[[id]]$zoom,
        zoom_levels = shiny::reactive(zoom_levels()$zoom_levels),
        region = shiny::reactive(zoom_levels()$region)
      )

      # Explore panel
      curbcut::explore_server(
        id = id,
        r = r,
        data = data,
        region = shiny::reactive(zoom_levels()$region),
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
        region = shiny::reactive(zoom_levels()$region),
        vars = r[[id]]$vars,
        data = data,
        zoom_levels = shiny::reactive(zoom_levels()$zoom_levels)
      )
    })
  }

  # Only keep the modules that can be worked using autovars (with var_left)
  create <- sapply(modules$id, \(i) !is.null(modules$var_left[modules$id == i][[1]]))
  create <- create[create]
  ids <- names(create)

  # Iterate over the ids to assign the functions in the global environment
  lapply(ids, \(id) assign(sprintf("%s_UI", id), ui, envir = as.environment(pos)))
  lapply(ids, \(id) assign(sprintf("%s_server", id), server, envir = as.environment(pos)))

  return(invisible())
}
