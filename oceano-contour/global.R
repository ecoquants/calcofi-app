# packages ----
# devtools::install_local("/share/github/calcofi4r")
librarian::shelf(
  calcofi/calcofi4r,
  digest, dygraphs, glue, geojsonio, gstat, here, httr2, 
  leaflegend, leaflet, leaflet.extras, 
  raster, readr, scales, sf, shiny, shinydashboard, shinyWidgets, tidyr)
# remotes::install_github("calcofi/calcofi4r", force=T)   # install remote
# devtools::install_local(here::here("../calcofi4r"), force=T)  # install local
# devtools::load_all(here("../calcofi4r"))                # debug
select <- dplyr::select
addLegend <- leaflet::addLegend
options(readr.show_col_types = F)
#shinyOptions(cache = cachem::cache_disk(PATH))
# touch /share/github/apps_dev-demo/oceano/restart

source(here("libs/db.R"))

dir_cache <- "/tmp" # sudo mkdir /tmp/cache; sudo chmod -R 777 /tmp/cache

# pts_stations
# pts_stations <- st_read(con, "stations"). # n = 50,856 
pts_stations <- st_read(                    # n =  6,343 
  con, 
  query = "
    SELECT DISTINCT ON (sta_id)
      line || ' ' || station AS sta_id, line, station, longitude, latitude, geom
    FROM stations
    WHERE 
      line >= 76.7 AND 
      line <= 93.3")

# ctdcast_ranges
# TODO: add indexes in db to:
#  ctdcast.date, ctdcast_bottle.depthm, ctdcast_bottle_dic.depth_m
ctdcast_dates <- dbGetQuery(
  con, "SELECT MIN(date) min, MAX(date) max FROM ctd_casts")
ctdcast_depths <- dbGetQuery(
  con, 
  "SELECT MAX(depthm) max 
   FROM ctd_bottles")
# UNION
# SELECT depthm FROM ctdcast_bottle_dic) x")


# d_vars
d_vars    <- calcofi4r::get_variables()
d_cruises <- calcofi4r::get_cruises()