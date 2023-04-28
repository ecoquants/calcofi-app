dashboardPage(
  dashboardHeader(
    title="CalCOFI Contour App"),
  
  dashboardSidebar(
    
    selectInput(
      "sel_var",
      "Variable",
      setNames(
        d_vars$table_field,
        d_vars$plot_label),
      "ctd_bottles.t_degc"),
    
    selectInput(
      "sel_val",
      "Value",
      c("avg", "min", "max", "stddev")),
    # TODO: try PERCENTILE_CONT(0.5) for median and other, eg p10 or p90
    #   * [How to Calculate Median in PostgreSQL - Ubiq BI](https://ubiq.co/database-blog/calculate-median-postgresql/)
    #   * [PostgreSQL: Documentation: 15: 9.21. Aggregate Functions](https://www.postgresql.org/docs/15/functions-aggregate.html#FUNCTIONS-ORDEREDSET-TABLE)
    
    sliderInput(
      "sel_depth_range",
      "Depth(m) range",
      min = 0, 
      max = rng_depths$max,
      step = 10,
      value = c(0, 515)),
    
    sel_qtr <- selectInput(
      "sel_qtr",
      "Season",
      c(Winter = 1, 
        Spring = 2,
        Summer = 3,
        Fall   = 4),
      selected = 1:2,
      multiple = T),
    
    dateRangeInput(
      "sel_date_range",
      "Date range",
      min   = rng_dates$min,
      max   = rng_dates$max,
      start = as.Date(today() - dyears(5)),
      end   = rng_dates$max,
      startview = "year"),
    
    actionButton(
      "btn_update", "Update", icon = icon("rotate"), width = "200px"),
    
    div(
      class="shiny-input-container",
      leafletOutput("map_side", height = "150px")),
    actionButton(
      "btn_aoi", "Area", icon = icon("edit"), width = "200px")
  ),
  
  dashboardBody(
    tags$head(tags$link(rel="stylesheet", type="text/css", href="styles.css")),
    tabsetPanel(
      id = "tabs",
      tabPanel(
        title = "Map", value = "map",
        leafletOutput("map")
        # TODO: add download links
        #downloadLink("dl_tif", "Download data (*.tif)")
      ),
      
      # tabPanel(
      #   title = "Time series", value = "time",
      #   dygraphOutput("plot_ts"),
      #   downloadLink("dl_csv", "Download data (*.csv)")),
      # 
      # tabPanel(
      #   title = "Depth Profile", value = "depth",
      #   "Interactive Depth Profiles coming soon...",br(),
      #   "similar to ", a(href="https://shiny.calcofi.io/capstone/", "Capstone app"),br(),
      #   img(src='depth_profile_capstone.png')) # , align = "right"),
      
    ))
)