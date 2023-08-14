# TODO:
# - show: points, raster, polygons
# - play: animate window of time on bottom of map

librarian::shelf(
  calcofi/calcofi4r,
  dplyr, DT, dygraphs, ggplot2, glue, here, htmltools, htmlwidgets, leaflet, lubridate, 
  plotly, png, readr, shiny, shinydashboard, shinyjs, stringr, webshot)
# remotes::install_github("calcofi/calcofi4r", force=T)   # install remote
# devtools::install_local(here::here("../calcofi4r"), force=T)  # install local
# devtools::load_all(here("../calcofi4r"))                # debug

source(here("libs/db.R"))         # con: database connection 
source(here("libs/functions.R"))

dir_cache <- switch(
  Sys.info()["sysname"],
  Linux  = "/share/data/cache_idw",
  Darwin = here("cache"))
# TODO: add check in get_map_data() for existence of files and if not, 
#  recreate (since caches differ on server vs laptop); consider + cache_system column to track which cache
url_cache <- "https://file.calcofi.io/cache_idw"

# ranges for date and depth
rng_dates <- dbGetQuery(
  con, "SELECT MIN(date) min, MAX(date) max FROM ctd_casts")
rng_depths <- dbGetQuery(
  con, "SELECT MAX(depth_m) max FROM ctd_bottles")

# data table of variables
d_vars  <- tbl(con, "field_labels") |> 
  filter(active) |> 
  arrange(plot_title) |> 
  collect()

aoi_cat_init <- "CalCOFI Zones"
aoi_keys_init <- c(
  "cc_nearshore-extended",
  "cc_offshore-extended",
  "cc_nearshore-standard",
  "cc_offshore-standard")
d_places <- cc_places |> 
  st_drop_geometry() |> 
  filter(category == aoi_cat_init)
aoi_rows_init <- which(d_places$key %in% aoi_keys_init)
  
# workaround Warning:
#   sf layer has inconsistent datum (+proj=longlat +ellps=WGS84 +no_defs).
#   Need '+proj=longlat +datum=WGS84'
cc_places <- st_transform(cc_places, 4326)