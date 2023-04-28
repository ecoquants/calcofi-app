# TODO:
# - show: points, raster, polygons
# - download: points, raster, polygons
# - play: animate window of time on bottom of map

librarian::shelf(
  # calcofi/calcofi4r,
  dplyr, DT, glue, here, leaflet, lubridate, 
  shiny, shinydashboard)
# remotes::install_github("calcofi/calcofi4r", force=T)   # install remote
# devtools::install_local(here("../calcofi4r"), force=T)  # install local
devtools::load_all(here("../calcofi4r"))                # debug

source(here("libs/db.R"))         # con: database connection 
source(here("libs/functions.R"))

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
  