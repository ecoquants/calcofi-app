librarian::shelf(
  # calcofi/calcofi4r,
  dplyr, glue, here, leaflet, lubridate, 
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

# aoi: default Area of Interest
aoi_keys <- c(
  "cc_nearshore-extended",
  "cc_offshore-extended",
  "cc_nearshore-standard",
  "cc_offshore-standard")
aoi <- cc_places |>
  filter(
    key %in% aoi_keys) |> 
  st_union()
# mapview::mapView(aoi)
