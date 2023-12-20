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
    
    rdeck(
      map_style = mapbox_dark(),
      theme = "light",
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
  })
  
})
