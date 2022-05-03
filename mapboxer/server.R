shinyServer(function(input, output) {
  output$map <- renderMapboxer({
    mapboxer(
      # style = basemaps$Carto$positron,
      style = "mapbox://styles/bdbest/cl216kpw100cb15oz3ch44zui",
      width = "100vw", height = "100vh",
      center = c(0, 0), zoom = 2, 
      # setting minZoom requires subsequent fit_bounds
      minZoom = 2,
      # disable all rotation and pitch 
      pitchWithRotate = F,
      dragRotate = F,
      touchZoomRotate = F) %>%
      fit_bounds(c(-180, -90, 180, 90), animate = F) %>% 
      # add_draw_control() %>% 
      add_source(mapbox_source(
        type = "vector",
        url  = "mapbox://bdbest.99btin43"),   # hex_res1
        id   = "src_hexagons") %>%
      add_layer(
        style = list(
          "id"           = "hexagons",
          "type"         = "fill",
          "source"       = "src_hexagons",
          "source-layer" = "abnj_hex_res1-59uxw0",   # hex_res1
          "paint" = list(
            "fill-color"   = "blue",
            "fill-opacity" = 0.6)),
        popup = "
      <b>hexid:</b> {{hexid}}</br>
      <b>lon:</b> {{lon}}</br>
      <b>lat:</b> {{lat}}</br>
      <b>on_dtln:</b> {{on_dtln}}")
  })
  
  observeEvent(input$map_onclick, {
    # browser()
    # mapboxer_proxy("map") %>%
    #   set_filter(LAYER_ID, list("==", "injured", input$slider)) %>%
    #   update_mapboxer()
  })
})
