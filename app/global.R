librarian::shelf(
  dplyr, here, 
  leaflet, lubridate, sf, shiny, shinydashboard)

gdb <- here("data/obis_seamap_dataset_507_gdb_36682/obis_seamap_dataset_507.gdb")
# rgdal::ogrListLayers(gdb) 
lyr_pts <- "obis_seamap_dataset_507_points"
lyr_lns <- "obis_seamap_dataset_507_lines" 
#pts    <- read_sf(gdb, lyr_pts) # 70,705 features and 20 fields; 1987-05-02 to 2006-11-04
lns     <- read_sf(gdb, lyr_lns) # 49,213 features and 15 fields; 1987-05-02 to 2006-11-04
yrs_lns <- year(lns$datetime_begin) %>% unique() %>% sort(decreasing = T)

cc_map_yr <- function(yr){
  
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
  
 