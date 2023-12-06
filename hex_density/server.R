shinyServer(function(input, output, session) {
  
  # calcofi4r::cc_places |> 
  #   filter(category == "CalCOFI Zones") |> 
  #   st_union() |> 
  #   st_bbox()
  #       xmin       ymin       xmax       ymax 
  # -135.23008   18.42757 -105.77692   49.23891 
  
  output$map <- renderRdeck({
    rdeck(
      map_style = mapbox_dark(),
      initial_bounds = st_bbox(
        c(xmin=-136, ymin=18, xmax=-106, ymax=50),
        crs = st_crs(4326)))
  })
  
  observe({

    req(input$num_max)

    mvt_url <- "https://tile.calcofi.io/public.hexagons/{z}/{x}/{y}.pbf"
    
    rdeck_proxy(id = "map") %>%
      add_mvt_layer(
        id   = "hexagons",
        name = "n_ctd_casts",
        data = mvt_url,
        get_fill_color = scale_color_linear(
          col      = n_ctd_casts,
          na_color = "#000000",
          palette  = viridis(6, alpha=0.5),
          limits   = c(1, input$num_max)),
        auto_highlight = TRUE,
        pickable = TRUE,
        tooltip = c(i, j, n_ctd_casts),
        max_zoom = 10)
  })
    
})
