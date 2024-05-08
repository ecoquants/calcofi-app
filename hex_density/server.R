shinyServer(function(input, output, session) {
  
  # calcofi4r::cc_places |> 
  #   filter(category == "CalCOFI Zones") |> 
  #   st_union() |> 
  #   st_bbox()
  #       xmin       ymin       xmax       ymax 
  # -135.23008   18.42757 -105.77692   49.23891 
  
  output$map <- renderRdeck({
    
    # tj <- tile_json("mapbox://mapbox.mapbox-streets-v8,mapbox.mapbox-terrain-v2")
    # tj$vector_layers <- tj$vector_layers |>
    #   discard(~ .$id == "water")
    
    # [Arc Layer : Dynamic changes on Click event · Issue #72 · qfes/rdeck](https://github.com/qfes/rdeck/issues/72#issuecomment-1150792238)
    # const map = rdeck.getWidgetById("map");
    # map.state.deckgl.onClick = (info, event) => { /* do something */ };
    rdeck(
      id             = "rdeck-map",
      map_style      = mapbox_dark(),
      theme          = "light",
      initial_bounds = st_bbox(
        c(xmin=-136, ymin=18, xmax=-106, ymax=50),
        crs = st_crs(4326)))  |> 
      add_mvt_layer(
        name              = "hexagons",
        data              = "https://tile.calcofi.io/public.hexagons/{z}/{x}/{y}.pbf",
        auto_highlight    = T,
        pickable          = T,
        tooltip           = c(i, j, n_ctd_casts),
        max_zoom          = 10,
        visibility_toggle = T,
        get_fill_color    = scale_color_linear(
          col      = n_ctd_casts,
          na_color = "#000000",
          palette  = viridis(6, alpha=0.5),
          limits   = c(1, input$num_max)))  |> 
      # add_tile_layer(
      #   name              = "land",
      #   opacity           = 1,
      #   data              = "mapbox://styles/bdbest/clqe2yvck005j01rd6fqz34dv",
      #   visibility_toggle = T) |> 
      # add_mvt_layer(
      #   name              = "land",
      #   visibility_toggle = T,
      #   opacity           = 1,
      #   data              = tj)|>
      add_mvt_layer(
        name               = "points",
        data               = "https://tile.calcofi.io/public.ctd_casts/{z}/{x}/{y}.pbf",
        point_radius_scale = 4,
        point_radius_units = "pixels",
        point_antialiasing = TRUE,
        point_type         = "circle",
        get_fill_color     = "#FF0000",
        auto_highlight     = T,
        highlight_color    = "#FFFF00",
        pickable           = T,
        tooltip            = c(castid, datetime),
        max_zoom           = 10,
        visibility_toggle  = T)
    
    # * [Place an MVT layer under a layer present in the basemap · Issue #58 · qfes/rdeck](https://github.com/qfes/rdeck/issues/58)
    # rdeck(
    #   map_style = "mapbox://styles/mapbox/streets-v11",
    #   initial_view_state = view_state(
    #     center = c(-73.58, 45.53), 
    #     zoom = 10.1)) |> 
    #   add_mvt_layer(
    #     id = "test", 
    #     data = mvt_url("mapbox.mapbox-streets-v8"),
    #     pickable = TRUE, tooltip = TRUE, 
    #     get_text = rlang::sym("name"),
    #     get_fill_color = "#FF0000",
    #     point_type = "text")
    
    
    # [MVTs not showing on deployed website · Issue #78 · qfes/rdeck](https://github.com/qfes/rdeck/issues/78#issuecomment-1198756996)
    # layers <- tibble::tibble(
    #   count = c(988L, 1000L, 1000L, 1000L, 1000L, 1000L, 24L, 89L, 5L),
    #   attribute = c("Bicycle (Baseline)", "Bicycle (Decarbonise)", "Bicycle (Demand reduction)", "Bicycle (Ebike)", "Bicycle (Go Dutch)", "Bicycle (Near market)", "Gradient", "Quietness", "col"),
    #   type = c("number", "number", "number", "number", "number", "number", "number", "number", "string"),
    #   min = c(1L, 1L, 1L, 1L, 1L, 1L, 0L, 3L, NA), 
    #   max = c(2949L, 4020L, 4076L, 6493L, 4959L, 3447L, 46L, 100L, NA), 
    #   values = list(NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, c("#CC6677", "#882255", "#44AA99", "#117733", NA))
    # )
    # my_map <- rdeck(layer_selector = TRUE)
    # add_bicycle_layer <- function(rdeck, layer_info, ...) {
    #   rdeck |>
    #     add_mvt_layer(
    #       ...,
    #       name = layer_info$attribute,
    #       get_line_color = scale_color_power(
    #         col = !!layer_info$attribute,
    #         limits = layer_info$limits
    #       )
    #     )
    # }
    # my_map_with_layers <-
    #   layers |>
    #   dplyr::mutate(limits = cbind(min, max)) |>
    #   dplyr::rowwise() |>
    #   dplyr::group_split() |>
    #   purrr::reduce(
    #     add_bicycle_layer,
    #     .init = my_map,
    #     visible = FALSE,
    #     # common params
    #   )
    # my_map_with_layers
    
  })
  
})
