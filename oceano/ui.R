shinyUI(fluidPage(
  tags$head(tags$link(rel="stylesheet", type="text/css", href="styles.css")),
  titlePanel("CalCOFI Oceanographic App"),

  sidebarLayout(
    
    sidebarPanel(
      selectInput(
        "sel_var",
        "Variable",
        setNames(
          d_vars$table_field,
          d_vars$plot_label)),
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
          "Area",
          # selectInput(
          #   "sel_aoi_method",
          #   "Method", c("Draw", "Select")),
          helpText("Define the area of interest by drawing a polygon or selecting 
             an existing feature."),
          switchInput(
            "sel_aoi_draw",
            "Method", F,
            onLabel = "Draw",
            offLabel = "Select"
          ),
          selectInput(
            "sel_aoi_category",
            "Category", 
            list(
              # `CalCOFI` = c(
              #   "Stations" = "cc_stations"),
              `Federal` = c(
                "National Sanctuaries" = "aoi_fed_sanctuaries"))),
              # `State`   = c(
              #   "MPA Regions"          = "aoi_ca_mpargns"))),
          leafletOutput("map_aoi")),
        tabPanel(
          "Time",
          dygraphOutput("plot_ts"),
          downloadLink("dl_csv", "Download data (*.csv)")),
        tabPanel(
          "Space",
          selectInput(
            "sel_cruise",
            "Cruise",
            sort(d_cruises$cruiseid, decreasing=T)),
          actionButton("btn_r", "Interpolate Variable from Cruise (within Depth range)"),
          br(),
          leafletOutput("map_r"),
          # TODO: fix dl_tif, not working
          #downloadLink("dl_tif", "Download data (*.tif)")
        ),
        tabPanel(
          "Depth",
          "TODO")
        ))
  )))