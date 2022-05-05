shinyServer(function(input, output, session) {
  
  values <- reactiveValues(
    ply_draw = NULL,
    map_r    = NULL)

  # map_aoi ----
  output$map_aoi <- renderLeaflet({
    
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
          selectedPathOptions = selectedPathOptions()),
        circleOptions = F,
        circleMarkerOptions = F,
        markerOptions = F,
        polylineOptions = F,
        singleFeature = T) 
    
    m })
  
  # * save drawn poly ----
  observe({
    #use the draw_stop event to detect when users finished drawing
    # https://github.com/bhaskarvk/leaflet.extras/blob/master/inst/examples/shiny/draw-events/app.R
    req(input$map_aoi_draw_all_features)
    
    feature <- isolate(input$map_aoi_draw_all_features$features[[1]])
    ply_json <- geojsonio::as.json(feature$geometry)
    ply <- st_read(ply_json, quiet = T)
    values$ply_draw <- ply

    leafletProxy("map_aoi") %>%
      clearShapes()
    
    if (!is.null(ply)){
      bb <- sf::st_bbox(ply)
      
      leafletProxy("map_aoi") %>%
        addPolygons(data = ply, group = "ply_draw") %>% 
        flyToBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])
    }
    
  })
  
  # map_r ----
  output$map_r <- renderLeaflet({
    
    # base map
    b <- st_bbox(pts_stations)
    m <- leaflet() %>%
      addProviderTiles(
        providers$Stamen.TonerLite,
        options = providerTileOptions(noWrap = TRUE)) %>% 
      fitBounds(b[['xmin']], b[['ymin']], b[['xmax']], b[['ymax']])
    
    m
  })
  # * map raster ----
  observeEvent(input$btn_r, {
    req(input$sel_cruise != "", input$sel_var)
    
    api_args <- list(
      variable = input$sel_var,
      cruise_id = input$sel_cruise,
      depth_m_min = input$sel_depth_range[1], 
      depth_m_max = input$sel_depth_range[2])
    hash    <- digest(api_args, algo="crc32")
    f_tif   <- glue("{dir_cache}/api_raster_{hash}.tif")
    
    if (!file.exists(f_tif)){
      message(glue("f_tif missing, get_raster()"))
      do.call(calcofi4r::get_raster, c(api_args, list(out_tif=f_tif)))
      stopifnot(file.exists(f_tif))
    }

    message(glue("raster({basename(f_tif)})"))
    r <- raster(f_tif) # raster::plot(r)
    r <- raster("/tmp/api_raster_08a528bf.tif")
    
    b <- st_bbox(extent(projectExtent(r, crs = 4326)))
    r_v <- values(r)
    pal <- colorNumeric("Spectral", r_v)
    v <- d_vars %>% 
      filter(table_field == input$sel_var)

    leafletProxy("map_r") %>% # m %>% 
      clearImages() %>% 
      clearControls() %>% 
      addRasterImage(
        r, project = F,
        colors = "Spectral", opacity=0.7) %>% 
      # TODO: add log/log10 option
      addLegend(
        pal = pal, values = r_v,
        title = v$plot_label) %>% 
      flyToBounds(b[['xmin']], b[['ymin']], b[['xmax']], b[['ymax']])
    
  })
  
  # get_timeseries_data() ----
  get_timeseries_data <- reactive({

    # handle special inputs
    input_stats <- c("mean", "sd")
    # TODO: translate input$sel_stats
    if (is.null(values$ply_draw)){
      aoi_wkt <- NULL
    } else {
      aoi_wkt <- st_as_text(values$ply_draw$geometry)
    }
    
    calcofi4r::get_timeseries(
      variable    = input$sel_var,
      aoi_wkt     = aoi_wkt,
      depth_m_min = input$sel_depth_range[1], 
      depth_m_max = input$sel_depth_range[2],
      date_beg    = input$sel_date_range[1], 
      date_end    = input$sel_date_range[2], 
      time_step   = input$sel_time_step,
      stats       = input_stats)
    
  })
  
  # plot_ts ----
  output$plot_ts <- renderDygraph({

    if (is.null(values$ply_draw)){
      aoi_wkt <- NULL
      message(glue("renderDygraph() aoi_wkt: NULL"))
    } else {
      aoi_wkt <- st_as_text(values$ply_draw$geometry)
      message(glue("renderDygraph() aoi_wkt: {aoi_wkt}"))
    }
    d <- get_timeseries_data()
    
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
