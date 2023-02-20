map_token <- paste0("pk.eyJ1Ijoic3VzLW1jZ2lsbCIsImEiOiJjbDBxMTcyNWwyNTl0M2",
                    "RtZzRremNxOHA3In0.V2Ah5lxy-3RZlF2QKOvIjg")
options(rdeck.mapbox_access_token = map_token)
map_base_style <- "mapbox://styles/sus-mcgill/cl0reqoz4000z15pekuh48ld6"
map_style_building <- "mapbox://styles/sus-mcgill/cl2bwtrsp000516rwyrkt9ior"
tileset_prefix <- "mtl"
mapbox_username <- "sus-mcgill"
map_loc <- c(-73.58, 45.53)

shiny::shinyApp(
  ui = shiny::fluidPage(
    # LANGUAGE
    map_UI(id = "map")
  ),
  server = function(input, output, session) {

    vars <- shiny::reactive(vars_build("housing_tenant_2016", df = "city_CSD"))
    data_colours <- shiny::reactive(data_get_colours(vars = vars(),
                                     region = "city",
                                     zoom_levels = map_zoom_levels_city))
    zoom <- reactive(rdeck::get_view_state("map-map")$zoom)

    map_server(id = "map",
               tile = reactive("city_CSD"),
               data_colours = data_colours,
               select_id = reactive("24660001"),
               zoom_levels = reactive(map_zoom_levels_city),
               zoom = zoom)
  }
)
