shinyUI(fluidPage(
  titlePanel("CalCOFI Oceanographic App"),

  sidebarLayout(
    
    sidebarPanel(
      selectInput(
        "sel_var",
        "Variable",
        setNames(
          d_vars$table_field,
          d_vars$plot_title)),
      sliderInput(
        "sel_depth_range",
        "Depth(m) range",
        min = 0, max = ctdcast_depths$max,
        step = 10,
        value = c(0, ctdcast_depths$max)),
      dateRangeInput(
        "sel_date_range",
        "Date range",
        ctdcast_dates$min,
        ctdcast_dates$max,
        startview = "year"),
      selectInput(
        "sel_time_step",
        "Time step (to summarize)",
        c("decade",
          "year",
          "year.quarter","year.month","year.week",
          "date",
          # climatic
          "quarter","month","week","julianday","hour"),
        "year"),
      selectInput(
        "sel_stats",
        "Statistics (to summarize)",
        c("average +/- standard deviation",
          "average + 95% - 5%",
          "average + maximum - minimum",
          "median + 90% - 10%")) ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Draw AOI",
          leafletOutput("map_aoi")),
        tabPanel(
          "Plot TimeSeries",
          dygraphOutput("plot_ts")),
        tabPanel(
          "Map Raster",
          selectInput(
            "sel_cruise",
            "Cruise ID",
            d_cruises$cruise_id),
          leafletOutput("map_r")) ))
  )))