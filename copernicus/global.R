# devtools::install_local("~/Github/oceanmetrics/leaftiles")
librarian::shelf(
  bslib, 
  calcofi/calcofi4r, dplyr,
  glue, here, httr2, leaflet, leaflet.extras, oceanmetrics/leaftiles, lubridate, 
  purrr, shiny, shinyWidgets, tibble, xml2)

verbose = T

# TODO: selectInput(): product, dataset, variable
product_id = "GLOBAL_MULTIYEAR_PHY_001_030"
dataset_id = "cmems_mod_glo_phy_my_0.083deg_P1D-m_202311"
variable   = "thetao"

w_url <- glue("https://wmts.marine.copernicus.eu/teroWmts/{product_id}")
dir_cache <- here("copernicus/cache")

w_gc <- request(w_url) |> 
  req_url_query(request = "GetCapabilities") |> 
  req_cache(dir_cache, debug = TRUE) |> 
  req_perform() |> 
  resp_body_xml()

w_ns <- xml_ns(w_gc) |> 
  xml_ns_rename(d1 = "wmts")

# variables ----
vars <- tibble(
  name = w_gc |>   
    xml_find_all(
      glue("//wmts:Layer[@queryable='1']/ows:Title"), w_ns) |> 
    xml_text() |> 
    strsplit(" - ") |> 
    map_chr(2),
  value = w_gc |>   
      xml_find_all(
        glue("//wmts:Layer[@queryable='1']/ows:Identifier"), w_ns) |> 
      xml_text() |> 
      strsplit("/") |> 
      map_chr(3)) |> 
  distinct() |> 
  deframe()
# TODO: exclude less relevant vars
# deptho_lev 
# "Model level number at sea floor (deptho_lev)" 
# mask 
# "Land-sea mask: 1 = sea ; 0 = land (mask)" 
# e1t 
# "Cell dimension along X axis (e1t)" 
# e2t 
# "Cell dimension along Y axis (e2t)" 
# e3t 
# "Cell dimension along Z axis (e3t)" 
var_default <- "thetao" # names(vars)[vars == var_default]: "Temperature (thetao)"

get_lyr <- function(var){
  
  lyr   <- list()
  
  layer <- glue("{product_id}/{dataset_id}/{var}")
  lyr$layer <- layer
  
  # * dates ----
  time_interval <- w_gc |>   
    xml_find_all(
      glue("//wmts:Layer[ows:Identifier='{layer}']/wmts:Dimension[ows:Identifier='time']/wmts:Value"), w_ns) |> 
    xml_text()
  # split time interval in ISO 8601
  #   https://en.wikipedia.org/wiki/ISO_8601#Time_intervals
  ti <- time_interval |> strsplit("/") |> unlist()
  stopifnot(ti[3] == "P1D") # confirm daily periodicity
  # TODO: handle monthly
  lyr$dates <- (as_date(ti[1]):as_date(ti[2])) |> as_date()
  
  # * elevations ----
  lyr$elevations <- w_gc |>   # https://en.wikipedia.org/wiki/ISO_8601#Time_intervals
    xml_find_all(
      glue("//wmts:Layer[ows:Identifier='{layer}']/wmts:Dimension[ows:Identifier='elevation']/wmts:Value"), w_ns) |> 
    xml_text() |> 
    rev()
  
  depth_names <- lyr$elevations |> (\(x) round(as.double(x) * -1, 1) )()
  lyr$depths <- setNames(lyr$elevations, depth_names)
  
  # lyr0$depths <- lyr$depths
  
  # * TileMatrixSets ----
  lyr$TileMatrixSets <- w_gc |>   # https://en.wikipedia.org/wiki/ISO_8601#Time_intervals
    xml_find_all(
      glue("//wmts:Layer[ows:Identifier='{layer}']/wmts:TileMatrixSetLink"), w_ns) |> 
    xml_text()
  # TileMatrixSets
  # [1] "EPSG:3857"    "EPSG:3857@2x" "EPSG:3857@3x" 
  # [4] "EPSG:4326"    "EPSG:4326@2x" "EPSG:4326@3x"
  
  # * Styles ----
  lyr$Styles <- w_gc |>   # https://en.wikipedia.org/wiki/ISO_8601#Time_intervals
    xml_find_all(
      glue("//wmts:Layer[ows:Identifier='{layer}']/wmts:Style/ows:Identifier"), w_ns) |> 
    xml_text()
  
  lyr
}

lyr0 <- get_lyr(var_default)

addCalcofiStations <- function(map, ...){
  
  map |> 
    addVectorTiles(
      server  = "https://tile.calcofi.io",
      layer   = "public.stations",
      layerId = "stationid",
      ...)
}