# libraries ----
librarian::shelf(
  dplyr, dygraphs, here, 
  leaflet, lubridate, readr, 
  sf, shiny, shinydashboard)
options(readr.show_col_types = F)

# surveys ----
gdb <- here("data/obis_seamap_dataset_507_gdb_36682/obis_seamap_dataset_507.gdb")
# rgdal::ogrListLayers(gdb) 
lyr_pts <- "obis_seamap_dataset_507_points"
lyr_lns <- "obis_seamap_dataset_507_lines" 
#pts    <- read_sf(gdb, lyr_pts) # 70,705 features and 20 fields; 1987-05-02 to 2006-11-04
lns     <- read_sf(gdb, lyr_lns) # 49,213 features and 15 fields; 1987-05-02 to 2006-11-04
yrs_lns <- year(lns$datetime_begin) %>% unique() %>% sort(decreasing = T)
bb_lns <- st_bbox(lns) %>% as.vector()

map_survey_yr <- function(yr){
  
  lns_yr <- lns %>% 
    filter(year(datetime_begin) == !!yr)
  # pts_yr <- pts %>% 
  #   filter(year(date_time) == yr)
  
  leaflet(
    lns_yr,
    options = leafletOptions(
      attributionControl = F)) %>% 
    addProviderTiles(providers$Esri.OceanBasemap) %>% 
    addPolylines() #%>% 
    #addCircleMarkers(data = pts_yr)
}
  
# sst ----

# https://coastwatch.pfeg.noaa.gov/erddap/wms/jplMURSST41mday/index.html
sst_dates <- seq(as.Date("2002-06-16"), as.Date("2021-10-16"), by = "months") %>% rev()

map_sst_date <- function(date){
  
  date <- as.Date(date)
  
  leaflet(
    options = leafletOptions(
      crs = leafletCRS(crsClass = "L.CRS.EPSG4326"))) %>%
    # basemap from GBIF in 4326
    addTiles("//tile.gbif.org/4326/omt/{z}/{x}/{y}@1x.png?style=gbif-geyser") %>%
    # sst
    addWMSTiles(
      baseUrl = 'https://coastwatch.pfeg.noaa.gov/erddap/wms/jplMURSST41mday/request?',
      layers = "jplMURSST41mday:sst",
      options = WMSTileOptions(
        version = "1.3.0", format = "image/png", transparent = T, opacity = 0.7,
        time = format(date,"%Y-%m-%dT00:00:00Z")))  %>%
    addLegend(
      position="bottomright",
      title = paste0("SST (°C)<br>", format(date,"%Y-%m-%d")),
      colorNumeric("Spectral", c(0,32), reverse=T), seq(0,32)) %>% 
    fitBounds(bb_lns[1], bb_lns[2], bb_lns[3], bb_lns[4])
}
 
plot_sst <- function(){
  
  sst_csv <- here("data/statistics_sst_cinms.csv")
  
  d <- read_csv(sst_csv) %>% 
    select(date, average_sst, quantile5_sst, quantile95_sst)
  x <- xts::xts(x = d %>% select(-date), order.by = d$date)
  
  dygraph(
    x,
    main = "Sea Surface Temperature",
    xlab = "Date", ylab = "Temperature (°C)") %>%
    dySeries(
      c("quantile5_sst", "average_sst", "quantile95_sst"), 
      label = "Temperature (°C)", color = "Red")%>%
    dyRangeSelector(fillColor = " #FFFFFF", strokeColor = "#FFFFFF")
}

