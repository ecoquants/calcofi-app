# packages
librarian::shelf(
  calcofi/calcofi4r,
  dygraphs, glue, here, httr2, leaflet, leaflet.extras, 
  readr, sf, shiny)
# devtools::install_local(here("../calcofi4r")) # install local
# devtools::load_all(here("../calcofi4r"))      # debug
options(readr.show_col_types = F)

source(here("../scripts/libs/db.R")) # calcofi/scripts repo

# pts_stations
pts_stations <- st_read(con, "stations")

# ctdcast_ranges
# TODO: add indexes in db to:
#  ctdcast.date, ctdcast_bottle.depthm, ctdcast_bottle_dic.depth_m
ctdcast_dates <- dbGetQuery(
  con, "SELECT MIN(date) min, MAX(date) max FROM ctdcast")
ctdcast_depths <- dbGetQuery(
  con, 
  "SELECT MAX(depth_m) max FROM (
    SELECT depth_m FROM ctdcast_bottle
    UNION
    SELECT depth_m FROM ctdcast_bottle_dic) x")

# d_vars
d_vars <- calcofi4r::get_variables()