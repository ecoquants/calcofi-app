function(input, output, session) {
  
  # bs_themer()
  
  # get_lyr() ----
  rx_lyr <- reactive({
    var <- input$sel_var
    
    get_lyr(var)
  })
  
  # observe rx_lyr() ----
  observe({
    lyr       <- rx_lyr()
    sld_time  <- input$sld_time
    sld_depth <- input$sld_depth
  
    # * ∆ sld_time ----
    updateSliderInput(
      session, "sld_time",
      min = min(lyr$dates),
      max = max(lyr$dates))
  
    # * ∆ map ----
    TileMatrixSet <- lyr$TileMatrixSets[2]
    Style         <- lyr$Styles[1]
    
    date          <- sld_time |> as_datetime() |> lubridate::format_ISO8601()
    elevation     <- lyr$depths[[
      which.min(abs(
        sld_depth - as.double(names(lyr$depths)) ))]]
    wms_url <- glue("http://wmts.marine.copernicus.eu/teroWmts/?service=WMTS&version=1.0.0&request=GetTile&tilematrixset={TileMatrixSet}&style={Style}&tilematrix={{z}}&tilerow={{y}}&tilecol={{x}}&layer={lyr$layer}&time={date}&elevation={elevation}")
    if (verbose)
      cat(wms_url)
    
      leafletProxy("map") |>
      addWMSTiles(
        baseUrl = wms_url,
        layers  = lyr$layer,
        layerId = "copernicus",
        options = WMSTileOptions(
          opacity     = 0.3,
          format      = "image/png",
          transparent = TRUE)) 
    
  })
  
  output$map <- renderLeaflet({
    leaflet() |> 
      addProviderTiles(
        providers$CartoDB.DarkMatter) |>
      addCalcofiStations(
        style = list(
          fill        = TRUE,
          fillColor   = "white",
          fillOpacity = 0.2,
          color       = "white",
          opacity     = 0.8,
          weight      = 0.1,
          radius      = 0.8) ) |> 
      setView(-122.4, 33.8, 4)
  })
  
  
  
}
