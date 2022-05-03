# packages ----
librarian::shelf(
  bslib, mapboxer, shiny)

# paths ----
mapbox_token_txt <- '~/.mapbox_token_bdbest.txt'

stopifnot(file.exists(mapbox_token_txt))
Sys.setenv("MAPBOX_API_TOKEN" = readLines(mapbox_token_txt))
