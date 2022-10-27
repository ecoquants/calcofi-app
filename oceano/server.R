shinyServer(function(input, output, session) {
  
  addResourcePath("cache", dir_cache)
  
  values <- reactiveValues(
    ply_draw = NULL,
    map_r    = NULL)
  
  # ui_stats ----
  output$ui_stats <- renderUI({
    # tagList(
    #   sliderInput("n", "N", 1, 1000, 500),
    #   textInput("label", "Label")
    # )
    
    sel_qtr_m <- selectInput(
      "sel_qtr",
      "Season",
      c(Winter = 1, 
        Spring = 2,
        Summer = 3,
        Fall   = 4),
      selected = 1:2,
      multiple = T)
    # TODO: sel_qtr_s single for 'Transect profile'
    
    if (input$tabs == "time"){
      ui <- tagList(
        selectInput(
          "sel_stats",
          "Statistics (to summarize)",
          c("avg +/- sd",
            "avg +/- 45%",
            "avg +/- max",
            "median +/- 40%")),
        sel_qtr_m,
        selectInput(
          "sel_time_step",
          "Temporal resolution",
          c("decade",
            "year",
            "year.quarter","year.month","year.week",
            "date",
            # climatic
            "quarter","month","week","julianday","hour"),
          "year") )
    } else {
      ui <- tagList(
        selectInput(
          "sel_val",
          "Value",
          c("avg", "sd", "min", "max", "n_obs")),
        sel_qtr_m,
        selectInput(
          "sel_res_km",
          "Spatial Resolution (km)",
          c(5, 20, 50), selected = 50) )
    }
      
    ui
  })
  
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
  
  # map_side ----
  output$map_side <- renderLeaflet({
    # message("output$map_side - beg")
    
    r <- raster(here("../scripts/data/r_sta-cnt_50km-mer.tif"))
    # pal <- colorNumeric("Spectral", values(r), na.color = "transparent")
    pal <- colorNumeric(
      gray.colors(10, start=0.01, end = 0.99), values(r), na.color = "transparent")
    # gray.colors(n, start = 0.3, end = 0.9, gamma = 2.2, alpha, rev = FALSE)
    
    
    m <- leaflet(
      options = leafletOptions(
        zoomControl        = F,
        attributionControl = F)) %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>% 
      # addProviderTiles(providers$Stamen.TonerLite) %>% 
      addRasterImage(
        r, project = F,
        colors = pal) %>% 
      addLegend(
        pal = pal, opacity = 0.9,
        values = values(r), title = "n_obs")
      # setView(-93.4, 37.4, 2)
    
    # message("output$map_side - end")
    m
  })
  
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
    
    r <- raster(here("../scripts/data/r_sta-cnt_50km-mer.tif"))
    pal <- colorNumeric(
      gray.colors(10, start=0.01, end = 0.99), values(r), na.color = "transparent")
    
    # m <- map_edit
    m <- leaflet(
      options = leafletOptions(
        zoomControl = T,
        attributionControl = F)) %>% 
      addProviderTiles(providers$Esri.OceanBasemap) %>% 
      addRasterImage(
        r, project = F,
        colors = pal, opacity = 0.5) %>% 
      addLegend(
        pal = pal, opacity = 0.5,
        values = values(r), title = "n_obs")
    
      # addPolygons(data = ply_editable_0, group = "ply_editable") %>% 
      # setView(-93.4, 37.4, 4)
    
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
    
    ply <- values$ply_draw
    if (!is.null(ply)){
      bb <- sf::st_bbox(ply)
      
      m <- m %>% 
        addPolygons(data = ply, group = "ply_editable") # %>%
      # flyToBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])
      
      updateActionButton(
        session,
        "btn_mod_map", "Modify", icon=icon("cog"))
    }
    
    message("output$mapeditor - end")
    m
  })
  
  # * save drawn poly ----
  # observe({
  #   #use the draw_stop event to detect when users finished drawing
  #   # https://github.com/bhaskarvk/leaflet.extras/blob/master/inst/examples/shiny/draw-events/app.R
  #   req(input$map_aoi_draw_all_features)
  #   
  #   feature <- isolate(input$map_aoi_draw_all_features$features[[1]])
  #   ply_json <- geojsonio::as.json(feature$geometry)
  #   ply <- st_read(ply_json, quiet = T)
  #   values$ply_draw <- ply
  # 
  #   leafletProxy("map_aoi") %>%
  #     clearShapes()
  #   
  #   if (!is.null(ply)){
  #     bb <- sf::st_bbox(ply)
  #     
  #     leafletProxy("map_aoi") %>%
  #       addPolygons(data = ply, group = "ply_draw") %>% 
  #       flyToBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])
  #   }
  #   
  # })
  observe({
    #use the draw_stop event to detect when users finished drawing
    # https://github.com/bhaskarvk/leaflet.extras/blob/master/inst/examples/shiny/draw-events/app.R
    req(input$mapeditor_draw_all_features)
    
    feature <- isolate(input$mapeditor_draw_all_features$features[[1]])
    ply_json <- geojsonio::as.json(feature$geometry)
    ply <- st_read(ply_json, quiet = T)
    values$ply_draw <- ply

    leafletProxy("map_side") %>%
      clearShapes()
    
    if (!is.null(ply)){
      bb <- sf::st_bbox(ply)
      
      leafletProxy("map_side") %>%
        addPolygons(data = ply, group = "ply_draw") %>% 
        flyToBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])
    }
    
  })
  
  # map_r ----
  # output$map_r <- renderLeaflet({
  #   
  #   # base map
  #   b <- st_bbox(pts_stations)
  #   m <- leaflet() %>%
  #     addProviderTiles(
  #       providers$Stamen.TonerLite,
  #       options = providerTileOptions(noWrap = TRUE)) %>% 
  #     fitBounds(b[['xmin']], b[['ymin']], b[['xmax']], b[['ymax']])
  #   
  #   m
  # })
  
  # map ----
  output$map <- renderLeaflet({
    req(input$sel_val)
    
    # TODO: values$ply_draw
    
    # variable = "ctd_bottles.t_deg_c", 
    # depth_m_min = 0, depth_m_max = 100){
    # cruise_id = "2020-01-05-C-33RL", 
      # @serializer tiff
      # test values
      # variable = "ctd_bottles.t_deg_c"; cruise_id = "2020-01-05-C-33RL"; depth_m_min = 0; depth_m_max = 10
      # variable = "ctd_bottles.t_deg_c"; cruise_id = "2020-01-05-C-33RL"; depth_m_min = 0; depth_m_max = 100
      # variable = "ctd_bottles_dic.bottle_o2_mmol_kg"; cruise_id = "1949-03-01-C-31CR"; variable = ""; depth_m_min = 0; depth_m_max = 1000
      # variable = "ctd_bottles.t_degc"; cruise_id = "2020-01-05-C-33RL"; depth_m_min = 0L; depth_m_max = 5351L
      # check input arguments
      
      # args_in <- as.list(match.call(expand.dots=FALSE))[-1]
    
    # DEBUG
    # values = list(ply_draw = NULL)
    
    if (is.null(values$ply_draw)){
      aoi_wkt <- NULL
    } else {
      aoi_wkt <- st_as_text(values$ply_draw$geometry)
    }
    
    # DEBUG
    # input = list(
    #   sel_var         = "ctd_bottles.t_degc",
    #   sel_val         = "n_obs",
    #   sel_res_km      = 20,
    #   sel_depth_range = c(0, 5351),
    #   sel_date_range  = as.Date(c("1949-02-28", "2020-01-26")))
    
    variable    = input$sel_var
    value       = input$sel_val
    res_km      = input$sel_res_km
    depth_m_min = input$sel_depth_range[1]
    depth_m_max = input$sel_depth_range[2]
    date_beg    = input$sel_date_range[1] 
    date_end    = input$sel_date_range[2]
    args_in <- list(
      variable    = variable,
      value       = value,
      res_km      = res_km,
      aoi_wkt     = aoi_wkt,
      depth_m_min = depth_m_min, 
      depth_m_max = depth_m_max,
      date_beg    = date_beg, 
      date_end    = date_end)
      
    hash    <- digest(args_in, algo="crc32")
    f_tif   <- glue("{dir_cache}/map_{hash}.tif")
    # f_tif <- "/tmp/api_raster_dd83f5a7.tif"
    
    # variable
    v <- tbl(con, "field_labels") %>% 
      filter(table_field == !!input$sel_var) %>% # ctd_bottles.t_degc
      collect() %>% 
      separate(table_field, into=c("tbl", "fld"), sep="\\.", remove=F)
    stopifnot(nrow(v) == 1)
    
    if (file.exists(f_tif)){
      message(glue("reading from cache: {basename(f_tif)}"))
      # r <- readBin(f_tif, "raw", n = file.info(f_tif)$size) # %>% 
      #return()
    } else {
      
      # construct SQL
      q_from <- case_when(
        v$tbl == "ctd_bottles" ~ "ctd_casts JOIN ctd_bottles USING (cast_count)",
        v$tbl == "ctd_dic"     ~ "ctd_casts JOIN ctd_bottles USING (cast_count) JOIN ctd_dic USING (btl_cnt)")
      
      q_where_depth = case_when(
        !is.null(depth_m_min) & !is.null(depth_m_max) ~ glue2("depthm >= {depth_m_min} AND depthm <= {depth_m_max}"),
        is.null(depth_m_min) & !is.null(depth_m_max) ~ glue2("depthm <= {depth_m_max}"),
        !is.null(depth_m_min) &  is.null(depth_m_max) ~ glue2("depthm >= {depth_m_min}"),
        TRUE ~ "TRUE")
      
      q_where_date = glue("date >= '{date_beg}' AND date <= '{date_end}'")
      
      q_where_aoi = ifelse(
        !is.null(aoi_wkt),
        glue("ST_Intersects(ST_GeomFromText('{aoi_wkt}', 4326), ctd_casts.geom)"),
        "TRUE")
      
      q <- glue(
        "SELECT 
          AVG({v$fld})    AS avg, 
          STDDEV({v$fld}) AS sd, 
          MIN({v$fld})    AS min, 
          MIN({v$fld})    AS max, 
          COUNT(*)        AS n_obs,
          geom
        FROM {q_from}
        WHERE 
          {q_where_depth} AND
          {q_where_date} AND
          {q_where_aoi}
        GROUP BY geom")
      message(q)
      pts_gcs <- st_read(con, query=q)
      
      # TODO: figure out why all points are repeating and if that makes sense
      # cruise_id = "2020-01-05-C-33RL"; variable = "ctd_bottles.t_deg_c"; depth_m_min = 0; depth_m_max = 10
      # table(pts$n_obs)
      #    3  4  5  6 
      #   52 36 13  2
      
      # transform from geographic coordinate system (gcs) 
      #   to web mercator (mer) for direct use with leaflet::addRasterImage()
      pts_mer <- st_transform(pts_gcs, 3857)
      # h <- st_convex_hull(st_union(pts_mer)) %>% st_as_sf() %>% mutate(one = 1)
      # library(mapview); mapviewOptions(fgb = F); mapview(h)
      # r <- raster(as_Spatial(h), res=1000, crs=3857)
      r <- raster(here(glue("../scripts/data/r_sta-cnt_{res_km}km-mer.tif")))
      
      fxn <- function(x, ...){
        y <- switch(
          value,
          avg   = mean(x, na.rm = T), # TODO: fix based on n_obs
          sd    = mean(x, na.rm = T), # TODO: fix based on n_obs
          min   = min(x, na.rm = T),
          max   = max(x, na.rm = T),
          n_obs = sum(x, na.rm = T))
        # message(glue("{y}: {paste(x, collapse = ', ')}"))
        as.double(y)}
      # value = "n_obs"
      z <- raster::rasterize(as_Spatial(pts_mer), r, value, fun=fxn)

      message(glue("writing to: {basename(f_tif)}")) # api_raster_a0f732d3.tif
      writeRaster(z, f_tif, overwrite=T)
    }
    # DEBUG
    # message(glue("f_tif: {f_tif}"))
    # message(glue("value: {value}"))
    # value = "avg"
    # v = list(
    #   active= TRUE,
    #   category= "Oceanographic",
    #   table_field= "ctd_bottles.t_degc",
    #   tbl= "ctd_bottles",
    #   fld= "t_degc",
    #   plot_title= "Temperature",
    #   plot_label= "Temperature (C)",
    #   plot_color= "red",
    #   color_palette = "Reds")
    # f_tif = "/tmp/map_74bb9e9a.tif"
    
    r <- raster(f_tif)
    
    # * effort circles ----
    pts_sta <- tbl(con, "stations_order") %>% 
      left_join(
        tbl(con, "ctd_casts"),
        by = c(
          "LINE" = "rptline", 
          "STA"  = "rptsta")) %>% 
      rename(
        sta_line = LINE, 
        sta_sta  = STA,
        lon      = `LON (DEC)`,
        lat      = `LAT (DEC)`,
        order    = `ORDER OCC`) %>% 
      group_by(sta_line, sta_sta, lon, lat) %>% 
      summarize(
        n_obs    = n(),
        date_min = min(date, na.rm=T),
        date_max = max(date, na.rm=T), 
        .groups = "drop") %>% 
      collect() %>% 
      st_as_sf(
        coords = c("lon", "lat"), remove = F,
        crs = 4326)
    eff_symbols <- makeSymbolsSize(
      values = pts_sta$n_obs,
      shape = "circle",
      color = "black",
      fillColor = F,
      opacity = .5,
      baseSize = 10)
    pts_sta$n_obs <- as.numeric(pts_sta$n_obs)
    # mapview::mapView(pts_sta, cex = "n_obs")

    # base map
    # b <- st_bbox(pts_stations)
    # m <- leaflet() %>%
    #   addProviderTiles(
    #     providers$Stamen.TonerLite,
    #     options = providerTileOptions(noWrap = TRUE)) %>% 
    #   fitBounds(b[['xmin']], b[['ymin']], b[['xmax']], b[['ymax']])
    
    alpha = 0.8
    b <- st_bbox(raster::projectExtent(r, crs = sp::CRS(leaflet:::epsg4326)))
    pal <- colorNumeric(v$color_palette, values(r), na.color = "transparent") # , alpha = alpha)
    
    m <- leaflet(
      options = leafletOptions(
        attributionControl = F)) %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "B&W") %>% 
      addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean") %>% 
      addRasterImage(
        r, project = F, group = glue("grid of {value}"),
        colors = pal, opacity = alpha) %>% 
      addSymbolsSize(
        data = pts_sta, lat = ~lat, lng = ~lon,        
        group = "circles of effort",
        values = ~n_obs,
        shape = "circle", baseSize = 10,
        color = "black", opacity = .5, fillOpacity = 0) %>%
      addLegend(pal = pal, values = values(r), title = value) %>% 
      addLegendSize(
        values = pts_sta$n_obs,
        title = "effort (n_obs)", position = "topright", orientation = "vertical",
        shape = "circle", baseSize = 5, breaks = 5,
        color = "black", opacity = .5, fillOpacity = 0) %>% 
      addLayersControl(
        position = "topleft",
        baseGroups = c("B&W", "Ocean"),
        overlayGroups = c(
          glue("grid of {value}"), 
          "circles of effort"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      fitBounds(b[['xmin']], b[['ymin']], b[['xmax']], b[['ymax']])
    
    ply <- values$ply_draw
    if (!is.null(ply)){
      bb <- sf::st_bbox(ply)
      
      m <- m %>%
        addPolygons(data = ply, group = "ply_draw", fill=F) %>% 
        flyToBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])
    }
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
  
  # *get_map_args ----
  get_map_args <- reactive({
    # handle special inputs
    if (is.null(values$ply_draw)){
      aoi_wkt <- NULL
    } else {
      aoi_wkt <- st_as_text(values$ply_draw$geometry)
    }
    
    calcofi4r::get_timeseries(
      variable    = input$sel_var,
      value       = input$sel_val,
      aoi_wkt     = aoi_wkt,
      depth_m_min = input$sel_depth_range[1], 
      depth_m_max = input$sel_depth_range[2],
      date_beg    = input$sel_date_range[1], 
      date_end    = input$sel_date_range[2], 
      time_step   = input$sel_time_step,
      stats       = input_stats)
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
