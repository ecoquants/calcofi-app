shinyServer(function(input, output, session) {
  
  values <- reactiveValues(
    ply_draw = NULL)
  
  # map ----
  output$map <- renderLeaflet({
    
    # base map
    m <- leaflet() %>%
      addProviderTiles(
        providers$Stamen.TonerLite,
        options = providerTileOptions(noWrap = TRUE))
    
    # add stations
    m <- m %>%
      addCircleMarkers(
        data = pts_stations,
        radius = 2, stroke = F,
        label = ~sta_id)
      
    # add draw toolbar
    m <- m %>%
      leaflet.extras::addDrawToolbar(
        targetGroup = "ply_draw",
        editOptions = leaflet.extras::editToolbarOptions(
          # edit = F,
          # remove = T,
          selectedPathOptions = selectedPathOptions()),
        circleOptions = F,
        circleMarkerOptions = F,
        markerOptions = F,
        polylineOptions = F,
        singleFeature = T) 
    
    ply_draw <- values$ply_draw
    if (!is.null(ply_draw)){
      bb <- sf::st_bbox(ply_draw)
      
      m <- m %>% 
        addPolygons(data = ply_draw, group = "ply_draw") # %>%
      # flyToBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])
    }
    
    m })
  
  # * map_draw_all_features ----
  observe({
    #use the draw_stop event to detect when users finished drawing
    # https://github.com/bhaskarvk/leaflet.extras/blob/master/inst/examples/shiny/draw-events/app.R
    req(input$map_draw_all_features)
    
    #message("observe output$mapeditor_draw_all_features - beg")
    
    feature <- isolate(input$map_draw_all_features$features[[1]])
    
    ply_json <- geojsonio::as.json(feature$geometry)
    ply <- st_read(ply_json, quiet = T)
    #ply_wkt <- st_as_text(st_geometry(ply))
    values$ply_draw <- ply
    
    leafletProxy("map") %>%
      clearShapes()
    
    if (!is.null(ply)){
      bb <- sf::st_bbox(ply)
      
      leafletProxy("map") %>%
        addPolygons(data = ply, group = "ply_draw_aoi") %>% 
        flyToBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])
    }
    
    #message("observe output$mapeditor_draw_all_features - end")
  })
  
  # get_timeseries_data() ----
  get_timeseries_data <- reactive({

    # TODO: translate input$sel_stats
    input_stats <- c("mean", "sd")
    
    calcofi4r::get_timeseries(
      variable    = input$sel_var,
      aoi_wkt     = switch(
        is.null(values$ply_draw), 
        NULL, 
        st_as_text(values$ply_draw)),
      depth_m_min = input$sel_depth_range[1], 
      depth_m_max = input$sel_depth_range[2],
      date_beg    = input$sel_date_range[1], 
      date_end    = input$sel_date_range[2], 
      time_step   = input$sel_time_step,
      stats       = input_stats)
    
  })
  
  # plot ----
  output$plot <- renderDygraph({

    d <- get_timeseries_data()
    
    # browser()
    v <- d_vars %>% 
      filter(table_field == input$sel_var)
    names(d) <- c("time", "avg", "sd", "n")
    d$lwr <- d$avg - d$sd
    d$upr <- d$avg + d$sd
    
    d %>% 
      select(time, avg, lwr, upr) %>% 
      dygraph(main = v$plot_title) %>%
      dySeries(c("lwr", "avg", "upr"), label = v$plot_label) # %>%
    # dyOptions(drawGrid = input$showgrid)
  })
  
})
