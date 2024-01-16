# remotes::install_github("qfes/rdeck")
if (!require("librarian"))
  install.packages("librarian")

librarian::shelf(
  bsicons, bslib, dplyr, glue, here, httr2, plotly, purrr, qfes/rdeck, readr, sf,
  shiny, shinyWidgets, stringr, tibble, tidyr, tidyselect, viridis)
options(readr.show_col_types = F)

source(here("libs/db.R")) # con: database connection 

# mapbox token ----

dir_private <- switch(
  Sys.info()[["sysname"]],
  "Darwin" = "/Users/bbest/My Drive/private",
  "Linux"  = "/share/private")

mb_token_txt <- glue("{dir_private}/mapbox_token_bdbest.txt")
# plys_csv     <- here("../workflows/data/ingest_blocks_spatial-paths.csv")
# db_r         <- here("../workflows/libs/db.r")

# stopifnot(all(file.exists(c(mb_token_txt, plys_csv, db_r))))
stopifnot(all(file.exists(c(mb_token_txt))))

# source(db_r)

mb_token <- readLines(mb_token_txt)
options(rdeck.mapbox_access_token = mb_token)

# ranges for date and depth
rng_dates <- dbGetQuery(
  con, "SELECT MIN(date) min, MAX(date) max FROM ctd_casts")
rng_depths <- dbGetQuery(
  con, "SELECT MAX(depth_m) max FROM ctd_bottles")

# nspp_tif <- "/Users/bbest/Github/marinebon/aquamapsduckdb/inst/app/data/am_nspp_3857.tif"
# r_nspp   <- terra::rast(nspp_tif)
# range(terra::values(r_nspp, na.rm=T))  # 1 9405

# plys ----

# tbl(con, "places")

# d_aois <- read_csv(plys_csv) |>
#   bind_rows(
#     tribble(
#       ~schema,        ~tbl, ~shelf,    ~layer,
#       "public",  "ply_rgns",  "USA", "Regions",
#       "public", "ply_shlfs",  "USA",  "Shelfs")) |>
#   mutate(
#     schema_tbl = glue("{schema}.{tbl}") |> as.character()) |>
#   arrange(shelf, layer)

# lst_aois <- list()
# for (shelf in unique(d_aois$shelf)) { # shelf = unique(d$shelf)[1]
#   lst_aois[[shelf]] <- d_aois |>
#     filter(shelf == !!shelf) |>
#     select(layer, schema_tbl) |>
#     deframe()
#   
#   # d_env |> 
#   #   group_by(var) |> 
#   #   summarise(
#   #     vec = list(var_value)) |> 
#   #   deframe()
# }

# nspp_tile_url ----
# TODO: add method
# cog_url        = "https://file.marinesensitivity.org/tif/am_nspp.tif"
# cog_range      = c(1, 9405)
# cog_method     = "average"
# cog_palette    = "spectral_r"
# lgnd_palette   = "Spectral"
# lgnd_palette_r = T

# tile_opts <- glue(
#   "resampling_method={cog_method}&rescale={paste(cog_range, collapse=',')}&return_mask=true&colormap_name={cog_palette}")
# nspp_tile_url  <- glue(
#   "https://api.cogeo.xyz/cog/tiles/WebMercatorQuad/{{z}}/{{x}}/{{y}}@2x?url={cog_url}&{tile_opts}")

d_0 <- tbl(con, "ctd_casts") |> 
  select(cast_count, date) |>
  collect()

plcats <- dbGetQuery(con, "SELECT DISTINCT category FROM places ORDER BY category;") |> 
  pull(category)
