dashboardPage(
  dashboardHeader(
    title="CalCOFI Oceanographic App"),

  dashboardSidebar(
    selectInput(
      "sel_var",
      "Variable",
      setNames(
        d_vars$table_field,
        d_vars$plot_label)),
    uiOutput("ui_stats"),
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
      "sel_rgn",
      "Location",
      list(
        "CalCOFI" = c(
          `Extended stations` = "cc_ext",
          `Core stations`     = "cc_cor",
          `All stations`      = "cc_all"),
        "National Marine Sanctuaries" = c(
          `Channel Islands`   = "onms_ci",
          `Monterey Bay`      = "onms_mb"))),
    
    #wellPanel(
    div(
      class="shiny-input-container",
      # htmltools::tag(
      #   "label", 
      # htmltools::tagList(
      # icon("map-marked"), 
      "Location (effort)",
      leafletOutput("map_side", height = 200),
      actionButton(
        "btn_mod_map", "Add", icon = icon("plus"), width = "200px"))
  ),
  
  dashboardBody(
    tags$head(tags$link(rel="stylesheet", type="text/css", href="styles.css")),
    tabsetPanel(
      id = "tabs",
      # tabPanel(
      #   "Area",
      #   # selectInput(
      #   #   "sel_aoi_method",
      #   #   "Method", c("Draw", "Select")),
      #   helpText("Define the area of interest by drawing a polygon or selecting 
      #      an existing feature."),
      #   switchInput(
      #     "sel_aoi_draw",
      #     "Method", F,
      #     onLabel = "Draw",
      #     offLabel = "Select"
      #   ),
      #   selectInput(
      #     "sel_aoi_category",
      #     "Category", 
      #     list(
      #       # `CalCOFI` = c(
      #       #   "Stations" = "cc_stations"),
      #       `Federal` = c(
      #         "National Sanctuaries" = "aoi_fed_sanctuaries"))),
      #       # `State`   = c(
      #       #   "MPA Regions"          = "aoi_ca_mpargns"))),
      #   leafletOutput("map_aoi")),
      tabPanel(
        title = "Map", value = "map",
        # selectInput(
        #   "sel_cruise",
        #   "Cruise",
        #   sort(d_cruises$cruiseid, decreasing=T)),
        # actionButton("btn_r", "Interpolate Variable from Cruise (within Depth range)"),
        # br(),
        # leafletOutput("map_r")
        leafletOutput("map")
        # TODO: fix dl_tif, not working
        #downloadLink("dl_tif", "Download data (*.tif)")
      ),
      tabPanel(
        title = "Time series", value = "time",
        dygraphOutput("plot_ts"),
        downloadLink("dl_csv", "Download data (*.csv)")),
      tabPanel(
        title = "Depth Profile", value = "depth",
        "Interactive Depth Profiles coming soon...",br(),
        "similar to ", a(href="https://shiny.calcofi.io/capstone/", "Capstone app"),br(),
        img(src='depth_profile_capstone.png') # , align = "right"),
        )
    ))
)