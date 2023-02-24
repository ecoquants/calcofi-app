shinyServer(function(input, output, session) {
  
  rxvals <- reactiveValues(map_init = F)
  
  # map ----
  output$map <- renderLeaflet({
    rxvals$map_init <- T
    map_base()
  })
  
  # map_contours ----
  observe({
    req(rxvals$map_init)

    p <- get_contour(
      variable      = input$sel_var,
      value         = "avg",
      aoi_pattern   = input$ck_pattern,
      aoi_shore     = input$ck_shore,
      depth_m_min   = input$sel_depth_range[1],
      depth_m_max   = input$sel_depth_range[2],
      date_qrtr     = input$sel_qtr,
      date_beg      = input$sel_date_range[1],
      date_end      = input$sel_date_range[2])

    title = d_vars |> 
      filter(table_field == input$sel_var) |> 
      pull(plot_label)
    
    leafletProxy("map") |>
      # map_base() |> 
      add_contours(p, title)
  })
  
  # map_aoi ----
  observe({
    req(rxvals$map_init)
    
    aois <- cc_grid_zones |> 
      filter(
        sta_pattern %in% input$ck_pattern,
        sta_shore   %in% input$ck_shore)
    
    if (nrow(aois) == 0){
      leafletProxy("map") |>
        clearGroup("aoi")
      return()
    }
    # aoi <- st_union(aois)
  
    # aoi_pattern = c("standard", "extended")
    aoi <- st_read(con, query = glue(
      "SELECT ST_Union(geom) AS geom
      FROM effort_zones 
      WHERE
        sta_pattern IN ('{paste(input$ck_pattern, collapse = '\\',\\'')}') AND
        sta_shore   IN ('{paste(input$ck_shore  , collapse = '\\',\\'')}') "))
    
    b <- st_bbox(aoi) |> as.numeric()
    
    leafletProxy("map") |>
      # map_base() |> 
      clearGroup("aoi") |> 
      addPolygons(
        data = aoi, group = "aoi",
        fill = F,
        color = "black") |> 
      fitBounds(b[1], b[2], b[3], b[4])
  })

  # map ----
  # observe()
  
})
