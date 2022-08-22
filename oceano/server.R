shinyServer(function(input, output, session) {
  
  addResourcePath("cache", dir_cache)
  
  values <- reactiveValues(
    ply_draw = NULL,
    map_r    = NULL)

  # map_aoi ----
  output$map_aoi <- renderLeaflet({
    
    # base map
    m <- leaflet() %>%
      addProviderTiles(
        group = "Gray basemap",
        providers$Stamen.TonerLite,
        options = providerTileOptions(noWrap = TRUE)) %>% 
      addProviderTiles(
        group = "Ocean basemap",
        providers$Esri.OceanBasemap,
        options = providerTileOptions(noWrap = TRUE))
    
    # add stations
    m <- m %>%
      addCircleMarkers(
        group = "Stations",
        data = pts_stations,
        radius = 2, stroke = F,
        label = ~sta_id) %>% 
      addLayersControl(
          baseGroups = c("Gray basemap", "Ocean basemap"),
          overlayGroups = c("Stations"),
          options = layersControlOptions(collapsed = T))
    
    # add selectable features
    if (!is.null(input$sel_aoi_category)){
      # input = list(sel_aoi_category = "aoi_fed_sanctuaries")
      
      if (input$sel_aoi_category == "cc_stations"){
        m <- m %>%
          addCircleMarkers(
            data = pts_stations,
            radius = 2, stroke = F,
            label = ~sta_id,
            layerId = ~sta_id,
            group = "Features")
      } else {
        f <- st_read(con, input$sel_aoi_category)
        m <- m %>%
          hideGroup("Stations") %>% 
          addPolygons(
            data = f,
            label = ~sanctuary,
            layerId = ~sanctuary,
            group = "Features")
      }
      m <- m %>%
        removeLayersControl() %>% 
        addLayersControl(
          baseGroups = c("Gray basemap", "Ocean basemap"),
          overlayGroups = c("Stations", "Features"),
          options = layersControlOptions(collapsed = T))
    }
    
    # map_side ----
    output$map_side <- renderLeaflet({
      message("output$map_side - beg")
      
      m <- leaflet(
        options = leafletOptions(
          zoomControl        = F,
          attributionControl = F)) %>%
        addProviderTiles(providers$Esri.OceanBasemap) %>% 
        setView(-93.4, 37.4, 2)
      
      message("output$map_side - end")
      m
    })
    
    # add draw toolbar
    if (input$sel_aoi_draw){
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
    }
    
    m })
  
  # * clicked feature ----
  # observe({
  #   
  #   m <- leafletProxy("map_aoi")
  #   
  #   if (input$sel_aoi_category == "cc_stations"){
  #     req(input$map_aoi_marker_click)
  #     clk <- input$map_aoi_marker_click
  #     
  #     m %>%
  #       clearGroup("selected") %>% 
  #       addCircleMarkers(
  #         data = pts_stations %>% 
  #           filter(sta_id == clk$id),
  #         # radius = 2, stroke = F, 
  #         color = "yellow", #fillColor="yellow",
  #         group = "selected",
  #         label = ~sta_id,
  #         labelOptions = labelOptions(
  #           permanent = T),
  #         layerId = ~glue("sel_{sta_id}"))
  #   } else {
  #     req(input$map_aoi_shape_click)
  #     clk <- input$map_aoi_shape_click
  #     
  #     f <- st_read(con, input$sel_aoi_category)
  #     
  #     m %>% 
  #       clearGroup("selected") %>% 
  #       addPolygons(
  #         data = f %>% 
  #           filter(sanctuary == clk$id),
  #         color = "yellow", 
  #         opacity = 0.8, 
  #         group = "selected",
  #         label = ~sanctuary,
  #         labelOptions = labelOptions(
  #           permanent = T),
  #         layerId = ~glue("sel_{sanctuary}"))
  #   }
  # })
  
  # *observe btn_mod_map ----
  observeEvent(input$btn_mod_map, {
    
    #message("observe btn_mod_map - beg")
    
    showModal(modalDialog(
      title     = "Modify Location",
      footer    = modalButton("Close"),
      easyClose = T,
      size      = "l",
      leafletOutput("mapeditor")))
    
    #message("observe btn_mod_map - end")
  })
  
  # *mapeditor ----
  output$mapeditor <- renderLeaflet({
    librarian::shelf(leaflet.extras)
    
    message("output$mapeditor - beg")
    
    # m <- map_edit
    m <- leaflet(
      options = leafletOptions(
        zoomControl = T,
        attributionControl = F)) %>% 
      addProviderTiles(providers$Esri.OceanBasemap) %>% 
      # addPolygons(data = ply_editable_0, group = "ply_editable") %>% 
      setView(-93.4, 37.4, 4)
    
    m <- m %>% 
      leaflet.extras::addDrawToolbar(
        targetGroup = "ply_editable",
        editOptions = leaflet.extras::editToolbarOptions(
          # edit = F,
          # remove = T,
          selectedPathOptions = selectedPathOptions()),
        circleOptions = F,
        circleMarkerOptions = F,
        markerOptions = F,
        polylineOptions = F,
        singleFeature = T) 
    
    ply <- values$ply
    if (!is.null(ply)){
      bb <- sf::st_bbox(ply)
      
      m <- m %>% 
        addPolygons(data = ply, group = "ply_editable") # %>%
      # flyToBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])
    }
    
    message("output$mapeditor - end")
    m
  })
  
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
  
  
  output$download_r <- renderUI({
    f_tif <- get_r_path()
    tagList(
      "Download raster data: ", 
      a(basename(f_tif), href=f_tif))
  })
  output$r_condition <- reactive({
    file.exists(get_r_path())
  })
  outputOptions(output, "r_condition", suspendWhenHidden = FALSE)
  get_r_args <- reactive({
    list(
      variable = input$sel_var,
      cruise_id = input$sel_cruise,
      depth_m_min = input$sel_depth_range[1], 
      depth_m_max = input$sel_depth_range[2])
  })
  get_r_path <- reactive({
    req(input$sel_cruise, input$sel_var)
    
    api_args <- get_r_args()
    hash    <- digest(api_args, algo="crc32")
    f_tif   <- glue("{dir_cache}/api_raster_{hash}.tif")
    f_tif
  })
  
  
  # * map raster ----
  observeEvent(input$btn_r, {
    req(input$sel_cruise, input$sel_var)
    
    api_args <- get_r_args()
    f_tif    <- get_r_path()
    
    if (!file.exists(f_tif)){
      message(glue("f_tif missing, get_raster()"))
      # browser() # dput(api_args))
      # variable = "ctd_bottles.t_degc", cruise_id = "2020-01-05-C-33RL", depth_m_min = 0L, depth_m_max = 5351L
      do.call(calcofi4r::get_raster, c(api_args, list(out_tif=f_tif)))
      stopifnot(file.exists(f_tif))
    }

    message(glue("raster({basename(f_tif)})"))
    r <- raster(f_tif) # raster::plot(r)
    # r <- raster("/tmp/api_raster_08a528bf.tif")
    
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
  
  # * download tif ----
  output$dl_tif <- downloadHandler(
    filename = function() {
      get_r_path()
    },
    content = function(file) {
      f_tif <- get_r_path()
      # TODO: fix, not working
      readBin(f_tif, "raw") %>% 
        writeBin(file)
    }
  )
  
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
    
    # d <- read_csv(here("../api/tmp_larvae.csv"))
    # v <- tbl(con, "field_labels") %>% 
    #   filter(table_field == "larvae_counts.count")
    
    names(d) <- c("time", "avg", "sd", "n")
    d$lwr <- d$avg - d$sd
    d$upr <- d$avg + d$sd
    
    d %>% 
      select(time, avg, lwr, upr) %>% 
      dygraph(main = v$plot_title) %>%
      dySeries(
        c("lwr", "avg", "upr"), 
        label = v$plot_label, color = v$plot_color) # %>%
    # dyOptions(drawGrid = input$showgrid)
  })
  
  # * download csv ----
  output$dl_csv <- downloadHandler(
    filename = function() {
      glue("calcofi_timeseries_{input$sel_var}.csv")
    },
    content = function(file) {
      write_csv(get_timeseries_data(), file)
    }
  )
  
})
