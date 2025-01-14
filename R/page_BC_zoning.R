#' @export
zoning_UI <- function(id) {
  modules <- get_from_globalenv("modules")
  page <- modules[modules$id == id, ]
  theme_lowercased <- gsub(" .*", "", tolower(page$theme))

  shiny::tagList(
      shiny::tags$head(
        shiny::tags$style(shiny::HTML("
      .greyed-out {
        color: grey !important;
        background-color: rgba(211, 211, 211, 0.3);
        pointer-events: none;
      }
    "))
      ),
    shiny::div(
      `data-theme` = theme_lowercased,
      # Sidebar
      sidebar_UI(
        id = shiny::NS(id, id),
        checkbox_UI(shiny::NS(id, id), label = "Explore residential areas and upcoming zoning changes under SSMUH legislation (Bill 44)",
                    value = FALSE),
        shiny::br(),
        shiny::hr(),
        shiny::div(
          class = "historical-comp-panel greyed-out",
          id = shiny::NS(id, "histor_comp_panel"),
          shiny::div(
            class = "shiny-split-layout sidebar-section-title",
            shiny::div(
              style = "width: 9%",
              icon_material_title("date_range")
            ),
            shiny::div(
              style = "width: 65%",
              cc_t_span("Historical comparison")
            ),
            shiny::div(
              style = "width: 35%; margin:0px !important; text-align: right;",
              shiny::div(
                checkbox_UI(
                  id = shiny::NS(id, id),
                  checkbox_id = "bcm",
                  label = cc_t("Compare"),
                  value = FALSE
                )
              )
            )
          ),
          shiny::div(
            id = shiny::NS(id, "zoom_slider_div"),
            class = "sus-sidebar-control",
            slider_text_UI(shiny::NS(id, id), slider_text_id = "abb",
                           choices = c("Before Bill 44", "After Bill 44"),
                           label = NULL,
                           selected = "Before Bill 44"),
            shinyjs::hidden(slider_text_UI(shiny::NS(id, id), slider_text_id = "abc",
                                           choices = c("Before Bill 44", "After Bill 44"),
                                           label = NULL,
                                           selected = c("Before Bill 44", "After Bill 44"))
            ))
        ),
        bottom = shiny::tagList(
          legend_UI(shiny::NS(id, id)),
        )
      ),

      # Map
      map_js_UI(shiny::NS(id, id)),

      # Right panel
      right_panel(
        id = id,
        explore_UI(shiny::NS(id, id))
      )
    )
  )
}

#' @export
zoning_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {

    mapbox_username <- get_from_globalenv("mapbox_username")
    map_token <- get_from_globalenv("map_token")
    # Map location and zoom different for this page
    sc <- get_from_globalenv("scales_dictionary")$scale[[1]]
    sc <- get_from_globalenv(sc)
    map_loc <- sc$centroid[which.max(sc$population)][[1]]
    map_zoom <- 12
    map_base_style <- get_from_globalenv("map_base_style")
    inst_prefix <- get_from_globalenv("inst_prefix")

    zoning_lots <- get_from_globalenv("zoning_lots")
    zones <- get_from_globalenv("zones")
    zones_residential <- get_from_globalenv("zones_residential")
    zoning_lots_residential <- get_from_globalenv("zoning_lots_residential")
    zones_compare <- get_from_globalenv("zones_compare")


    zoning_colour_pal <- tibble::tibble(group = c("COMMERCIAL", "RESIDENTIAL", "OTHER", "OPEN SPACE",
                                                  "PUBLIC AND INSTITUTIONAL", "AGRICULTURAL / RURAL",
                                                  "COMPREHENSIVE DEVELOPMENT", "INDUSTRIAL"),
                                        group_short = c("Commercial", "Residential", "Other", "Open",
                                                        "Public", "Agricultural", "Comprehensive", "Industrial"),
                                        fill = c("#A3B0D1", "#E08565", "#73AD80", "#C9C3FA",
                                                 "#F5D574", "#ADB033", "#CD718C", "#9E9090"))
    zones <- zones[order(zones$zoning), ]
    zoning_colour_pal <- zoning_colour_pal[order(zoning_colour_pal$group_short), ]
    zoning_colour_pal_res <- tibble::tibble(
      group = c("Single", "Single + Secondary suite/ADU", "Duplex",
                "Multiresidential (3 units)", "Multiresidential (4 units)",
                "Multiresidential (6 units)",
                "Multiresidential"),
      group_short = c("Single", "Single + Suite", "Duplex", "Multi (3)", "Multi (4)",
                      "Multi (6)",
                      "Multi"),
      fill = c("#ADB033", "#F5D574", "#A3B0D1", "#CD718C", "#E08565", "#C9C3FA",
               "#73AD80"))
    colours_dfs <- get_from_globalenv("colours_dfs")
    zoning_colour_pal_compare <- tibble::tibble(group = c("None",
                                                          "+1 unit",
                                                          paste0("+", 2:5, " units")),
                                                group_short = c("None",
                                                                "+1 unit",
                                                                paste0("+", 2:5, " units")),
                                                fill = colours_dfs$left_5$fill[1:6])

    # Zoning special functions
    zoning_info_table <- function(case, select_id) {
      if (case == "all") {
        df <- zoning_lots
        z <- zones
        group <- "ICI_ZONE"
        pre_text <- "This parcel is zoned as"
        intro <- "<p>The zoning distribution across different areas is as follows:"
      }
      if (case == "before_bill") {
        df <- zoning_lots_residential
        z <- zones_residential[zones_residential$before, ]
        group <- "res_category_before"
        pre_text <- #"Before Bill 44,
          "The permitted use on this parcel was"
        intro <- #"<p>Before Bill 44,
          "<p>The residential zoning distribution across different areas was as follows:"
      }
      if (case == "after_bill") {
        df <- zoning_lots_residential
        z <- zones_residential[!zones_residential$before, ]
        group <- "res_category_after"
        pre_text <- "After Bill 44, the permitted use on this parcel will be"
        intro <- "<p>The Small-Scale, Multi-Unit Housing (SSMUH) legislation, or Bill 44, aims to increase housing supply, create more diverse housing choices, and over time, contribute to more affordable housing across BC.<p>After Bill 44, the residential zoning distribution across different areas will be as follows:"
      }
      if (case == "compare") {
        df <- zoning_lots_residential
        z <- zones_compare
        group <- "additional_units"
        pre_text <- "After Bill 44, the additional number of units allowed on this parcel will be"
        intro <- "<p>The Small-Scale, Multi-Unit Housing (SSMUH) legislation, or Bill 44, aims to increase housing supply, create more diverse housing choices, and over time, contribute to more affordable housing across BC.<p>After Bill 44, the additional number of units allowed across different parcels will be as follows:"
      }

      if (!is.na(select_id)) {
        row <- df[df$ID == select_id, ]
        if (nrow(row) != 0) {
          out <-
            sprintf("<p><b>%s</b></p><p>%s '%s'. The parcel covers an area of %s square meters.",
                    row$address, pre_text, tolower(row[[group]]), convert_unit(x = row$area))
          if (case %in% c("after_bill", "before_bill")) {
            if (row$res_category_after == "Multiresidential") {
              out <- sprintf("%s<p>This category of zoning includes a minimum of 3 dwellings but the upper limit is unknown.",
                             out)
            }
          }

          return(out)
        }
      }

      # Creating bullet points for each zoning type
      if (case == "compare") {
        z$zoning <- gsub(" unit", " additional unit", z$zoning)
        z$zoning <- gsub("None", "No additional units", z$zoning)
        z$zoning <- gsub("\\+", "", z$zoning)
      }
      bullet_points <- apply(z, 1, function(row) {
        sprintf("<li>'<b>%s</b>' zones cover %s square kilometres (%s  of zoned area).",
                row["zoning"], row["area_km2"], row["area_pct"])
      })

      # Combine bullet points into a single text
      bullet_text <- paste(bullet_points, collapse = "\n")

      # Final message
      return(paste(intro, "<br><ul>", bullet_text, sep = ""))
    }

    # Grab when on borough summary
    zoning_graph <- function(case, select_id) {

      if (case == "all") {
        df <- zoning_lots
        z <- zones
        z$zoning <- toupper(z$zoning)
        zoning_col <- zoning_colour_pal
        group <- "ICI_ZONE"
      }
      if (case == "before_bill") {
        df <- zoning_lots_residential
        z <- zones_residential[zones_residential$before, ]
        zoning_col <- zoning_colour_pal_res
        group <- "res_category_before"
      }
      if (case == "after_bill") {
        df <- zoning_lots_residential
        z <- zones_residential[!zones_residential$before, ]
        zoning_col <- zoning_colour_pal_res
        group <- "res_category_after"
      }
      if (case == "compare") {
        df <- zoning_lots_residential
        z <- zones_compare
        zoning_col <- zoning_colour_pal_compare
        group <- "additional_units"

        df$additional_units <- gsub(0, "None", df$additional_units)
        df$additional_units <- gsub(1, "+1 unit", df$additional_units)
        df$additional_units <- gsub(2, "+2 units", df$additional_units)
        df$additional_units <- gsub(3, "+3 units", df$additional_units)
        df$additional_units <- gsub(4, "+4 units", df$additional_units)
        df$additional_units <- gsub(5, "+5 units", df$additional_units)
      }

      theme_default <- list(
        ggplot2::theme_minimal(),
        ggplot2::theme(
          text = ggplot2::element_text(family = "acidgrotesk-book", size = 12),
          legend.position = "none",
          panel.grid.minor.x = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank()
        )
      )

      zoning_for_gg <- z
      zoning_for_gg$zoning_short <- sapply(zoning_for_gg$zoning, \(x) {
        zoning_col$group_short[zoning_col$group == x]
      }, USE.NAMES = FALSE)
      zoning_for_gg$area_km2 <- as.numeric(zoning_for_gg$area_km2)
      # Set the levels of 'zoning_short' according to their order in the dataframe
      zoning_for_gg$zoning_short <- factor(zoning_for_gg$zoning_short,
                                           levels = unique(zoning_for_gg$zoning_short))

      # Prepare the color mapping to match with the levels of zoning_short
      color_mapping <- stats::setNames(zoning_col$fill, zoning_col$group_short)

      # Create the ggplot
      plot <-
        zoning_for_gg |>
        ggplot2::ggplot(ggplot2::aes(x = !!ggplot2::sym("zoning_short"),
                                     y = !!ggplot2::sym("area_km2"),
                                     fill = !!ggplot2::sym("zoning_short"))) +
        ggplot2::geom_col() +
        ggplot2::scale_fill_manual(values = color_mapping) +
        ggplot2::labs(title = NULL,
                      x = NULL,
                      y = "Area (km2)") +
        theme_default +
        ggplot2::coord_flip()

      if (!is.na(select_id)) {
        val <- df[[group]][df$ID == select_id]
        val <- zoning_col$group_short[zoning_col$group == val]
        if (!any(is.na(val))) {
          plot <-
            plot +
            ggplot2::geom_vline(
              xintercept = val,
              colour = "black", linewidth = 1.5
            )
        }
      }

      plot
    }

    # Fill when on observational data
    scale_fill_zoning <- function(case, ...) {
      if (case == "all") {
        zoning_col <- zoning_colour_pal
        group <- "ICI_ZONE"
      }
      if (case == "before_bill") {
        zoning_col <- zoning_colour_pal_res[c("group", "fill")]
        group <- "res_category_before"
      }
      if (case == "after_bill") {
        zoning_col <- zoning_colour_pal_res[c("group", "fill")]
        group <- "res_category_after"
      }
      if (case == "compare") {
        zoning_col <- zoning_colour_pal_compare[c("group", "fill")]
        zoning_col$group <- s_extract("\\d", zoning_col$group)
        zoning_col$group <- ifelse(is.na(zoning_col$group), 0, zoning_col$group)
        group <- "additional_units"
      }

      cc.map::map_choropleth_fill_fun(
        df = unique(zoning_col[c("group", "fill")]),
        get_col = group,
        fallback = "#B3B3BB")
    }

    zoning_legend <- function(case, ...) {
      if (case == "all") {
        zoning_col <- zoning_colour_pal
      }
      if (case == "before_bill") {
        zoning_col <- zoning_colour_pal_res
      }
      if (case == "after_bill") {
        zoning_col <- zoning_colour_pal_res
      }
      if (case == "compare") {
        zoning_col <- zoning_colour_pal_compare
      }

      # Grab labels
      breaks <- zoning_col[[if (case == "all") "group" else "group_short"]]
      if (case == "all") breaks <- gsub(" .*", "", s_sentence(breaks))

      # Construct the dataframe
      half <- {
        n <- nrow(zoning_col)
        if (case == "all") {
          c(rep(1, 3), rep(2, 3), rep(3, 2))
        } else if (n <= 6) {
          rep(1, n)
        } else {
          mid_point <- n %/% 2 + n %% 2
          c(rep(1, mid_point), rep(2, n - mid_point))[1:n]
        }
      }

      df <- tibble::tibble(group = seq_len(nrow(zoning_col)), y = 1,
                           fill = zoning_col$fill,
                           half = half)

      # Make the plot
      df |>
        ggplot2::ggplot(ggplot2::aes(
          xmin = !!ggplot2::sym("group") - 1, xmax = !!ggplot2::sym("group"),
          ymin = !!ggplot2::sym("y") - 1,
          ymax = !!ggplot2::sym("y"), fill = !!ggplot2::sym("fill")
        )) +
        ggplot2::geom_rect() +
        ggplot2::scale_x_continuous(
          breaks = df$group - 0.5,
          labels = breaks
        ) +
        ggplot2::scale_y_continuous(labels = NULL) +
        ggplot2::scale_fill_manual(values = stats::setNames(
          df$fill, df$fill
        )) +
        ggplot2::theme_minimal() +
        ggplot2::theme(text = ggplot2::element_text(family = "acidgrotesk-book", size = 12),
                       legend.position = "none",
                       panel.grid = ggplot2::element_blank()) +
        ggplot2::facet_wrap(~half, scales = "free_x", ncol = 1,
                            labeller = ggplot2::as_labeller(function(half) ""))
    }


    # Special zoning servers --------------------------------------------------

    legend_zoning_server <- function(id, r, legend_fun, legend_args, height = shiny::reactive(120)) {
      shiny::moduleServer(id, function(input, output, session) {

        # Output legend
        output$legend_render <- shiny::renderUI({
          output$legend <- shiny::renderPlot(do.call(legend_fun(), legend_args()))
          # Weird hack to get legend plot to inherit full namespace
          shiny::plotOutput(session$ns("legend"),
                            height = height(),
                            width = "100%"
          )
        })
      })
    }

    explore_zoning_server <- function(id, r, select_id, table_fun, table_args,
                                      graph_fun, graph_args) {

      shiny::moduleServer(id, function(input, output, session) {
        # Make info table. If fails, returns NULL
        table_out <- shiny::reactive(
          tryCatch(
            do.call(table_fun(), table_args()),
            error = function(e) {
              print(e)
              return(NULL)
            }
          )
        )

        # Display info table
        output$info_table <- shiny::renderUI(shiny::HTML(table_out()))

        # Make graph
        graph_out <- shiny::reactive(
          tryCatch(
            do.call(graph_fun(), graph_args()),
            error = function(e) {
              print(e)
              return(NULL)
            }
          )
        )

        # Display graph
        output$explore_graph <- shiny::renderPlot(graph_out())

      })
    }


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
        stories = NULL,
        stories_min_zoom = 13
      )
    })

    # Zoom and POI reactives when the view state of the map changes.
    shiny::observeEvent(map_viewstate(), {
      r[[id]]$zoom(zoom_get(zoom = map_viewstate()$zoom))
      r[[id]]$poi(update_poi(
        id = id, poi = r[[id]]$poi(),
        map_viewstate = map_viewstate()
      ))
    }, ignoreInit = TRUE)

    # Residential only checkbox
    res <- checkbox_server(id = id, r = r,
                           label = shiny::reactive("Explore residential areas"))# and upcoming zoning changes under SSMUH legislation (Bill 44)"))
    b_a_bill <- slider_text_server(id = id, r = r, slider_text_id = "abb")
    # When not on res, hide the slider_text!
    shiny::observeEvent(res(), {
      if (!res()) shinyjs::addClass(id = "histor_comp_panel", class = "greyed-out")
      if (res()) shinyjs::removeClass(id = "histor_comp_panel", class = "greyed-out")
      shinyjs::toggleState("histor_comp_panel", condition = res())
    })
    # Compare before and after bill
    compare_bill <- checkbox_server(id = id, r = r, checkbox_id = "bcm",
                                    label = shiny::reactive("Compare"))
    shiny::observeEvent(compare_bill(), {
      shinyjs::toggle(shiny::NS(id, "ccslidertext_abb"), condition = !compare_bill())
      shinyjs::toggle(shiny::NS(id, "ccslidertext_abc"), condition = compare_bill())
    })

    # Update selected ID
    update_select_id(id = id, r = r)

    tile <- shiny::reactive("zoning")

    # Sidebar
    sidebar_server(id = id, r = r)

    # Legend
    legend_height <- shiny::reactive({
      if (!res()) return(150)
      #if (compare_bill())
      return(60)
      #return(120)
    })
    legend_zoning_server(
      id = id,
      r = r,
      legend_fun = shiny::reactive(zoning_legend),
      legend_args = table_graph_args,
      height = legend_height
    )

    # Update map in response to variable changes or zooming
    map_viewstate <- map_js_server(
      id = id,
      r = r,
      tile = shiny::reactive("zoning"),
      select_id = r[[id]]$select_id,
      zoom = r[[id]]$zoom,
      coords = r[[id]]$coords,
      vars = shiny::reactive(NULL),
      fill_fun = shiny::reactive(scale_fill_zoning),
      fill_fun_args = table_graph_args,
      outline_color = shiny::reactive("white"),
      outline_width = shiny::reactive(0.1)
    )

    # Explore panel
    table_graph_args <- shiny::reactive({
      case <- (\(x) {
        if (!res()) return("all")
        if (compare_bill()) return("compare")
        if (b_a_bill() == "Before Bill 44") return("before_bill")
        return("after_bill")
      })()

      list(case = case, select_id = r[[id]]$select_id())
    })
    explore_zoning_server(
      id = id,
      r = r,
      table_fun = shiny::reactive(zoning_info_table),
      table_args = table_graph_args,
      graph_fun = shiny::reactive(zoning_graph),
      graph_args = table_graph_args
    )

    # Bookmarking
    bookmark_server(
      id = id,
      r = r,
      select_id = r[[id]]$select_id,
      map_viewstate = map_viewstate
    )

  })
}
