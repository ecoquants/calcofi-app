shinyServer(function(input, output, session) {
  
  rxvals <- reactiveValues(
    map_init = F,
    aoi_keys = aoi_keys,
    aoi_ewkt = NULL)
  
  # map ----
  output$map <- renderLeaflet({

    plys <- isolate(get_map_data(
      variable      = input$sel_var,
      value         = input$sel_val,
      aoi_keys      = rxvals$aoi_keys, # TODO: change to optional aoi_wkt, aoi_keys (sorted for hash); default cc_* (all but historical)
      aoi_ewkt      = rxvals$aoi_ewkt, 
      depth_m_min   = input$sel_depth_range[1],
      depth_m_max   = input$sel_depth_range[2],
      date_qrtr     = input$sel_qtr,
      date_beg      = input$sel_date_range[1],
      date_end      = input$sel_date_range[2],
      return_type   = "polygons"))
    
    title = d_vars |> 
      filter(table_field == isolate(input$sel_var)) |> 
      pull(plot_label) |> 
      paste(glue("<br>{isolate(input$sel_val)}"))
    
    rxvals$map_init <- T
    
    map_base(
      options = leafletOptions(
        attributionControl = F)) |>
      add_contours(plys, title)
  })
  
  # map_side ----
  output$map_side <- renderLeaflet({
    map_base(
      options = leafletOptions(
        zoomControl        = F,
        attributionControl = F))
  })
  
  # observeEvent btn_update ----
  observeEvent(input$btn_update, {

    plys <- get_map_data(
      variable      = input$sel_var,
      value         = input$sel_val,
      aoi_keys      = rxvals$aoi_keys, # TODO: change to optional aoi_wkt, aoi_keys (sorted for hash); default cc_* (all but historical)
      aoi_ewkt      = rxvals$aoi_ewkt, 
      depth_m_min   = input$sel_depth_range[1],
      depth_m_max   = input$sel_depth_range[2],
      date_qrtr     = input$sel_qtr,
      date_beg      = input$sel_date_range[1],
      date_end      = input$sel_date_range[2],
      return_type   = "polygons")
    
    title = d_vars |> 
      filter(table_field == input$sel_var) |> 
      pull(plot_label) |> 
      paste(glue("<br>{input$sel_val}"))
    
    leafletProxy("map") |>
      add_contours(plys, title)
  })
  
  # observe aoi ----
  observe({
    req(rxvals$map_init)
    
    aoi <- get_aoi(
      aoi_keys = rxvals$aoi_keys,
      aoi_ewkt = rxvals$aoi_ewkt)
    
    if (nrow(aoi) == 0){
      leafletProxy("map") |>
        clearGroup("aoi")
    }

    b <- st_bbox(aoi) |> as.numeric()
    
    leafletProxy("map") |>
      clearGroup("aoi") |> 
      addPolygons(
        data = aoi, group = "aoi",
        fill = F,
        color = "black") |> 
      fitBounds(b[1], b[2], b[3], b[4])
    
    leafletProxy("map_side") |>
      clearGroup("aoi") |> 
      addPolygons(
        data = aoi, group = "aoi",
        fill = T,
        color = "black") |> 
      fitBounds(b[1], b[2], b[3], b[4])
  })
  
  # observeEvent btn_aoi ----
  observeEvent(input$btn_aoi, {
    
    showModal(modalDialog(
      title     = "Modify Area",
      leafletOutput("mapedit"),
      footer    = modalButton("Close"),
      easyClose = T))
    
  })
  
  # mapedit ----
  output$mapedit <- renderLeaflet({
    library(leaflet.extras)
    
    m <- map_base(
      options = leafletOptions(
        zoomControl        = T,
        attributionControl = F)) |>
      setView(-93.4, 37.4, 4)
    
    m <- m |> 
      addDrawToolbar(
        targetGroup = "ply_edit",
        # editOptions = editToolbarOptions(),
          # edit   = T,
          # remove = T),
          #selectedPathOptions = selectedPathOptions()),
        circleOptions       = F,
        circleMarkerOptions = F,
        markerOptions       = F,
        polylineOptions     = F,
        singleFeature       = T)
    
    if (!is.null(rxvals$aoi_ewkt)){
      
      ply <- rxvals$aoi_ewkt |> st_as_sfc() |> st_as_sf() |> st_set_geometry("geom")

      bb <- sf::st_bbox(ply)
      
      m <- m %>% 
        addPolygons(data = ply, group = "ply_edit") |> 
        flyToBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])
    }
    
    m
  })
  
  # observe mapedit_draw ----
  observeEvent(input$mapedit_draw_all_features, {
    # use the draw_stop event to detect when users finished drawing
    #   https://github.com/bhaskarvk/leaflet.extras/blob/master/inst/examples/shiny/draw-events/app.R
    
    feature <- input$mapedit_draw_all_features$features[[1]]
    
    rxvals$aoi_ewkt <- feature$geometry |> 
      geojsonio::as.json() |> 
      st_read(quiet = T) |> 
      st_geometry() |> 
      st_as_text(EWKT=T)
    rxvals$aoi_keys <- NULL
    
  })

})
