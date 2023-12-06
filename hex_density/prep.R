remotes::install_github("qfes/rdeck")

remotes::install_github("eddelbuettel/rcppsimdjson")

library(rdeck)
library(dplyr)
library(sf)
library(viridis)
# loading deck.gl example data
library(RcppSimdJson)


url <- file.path(
  "https://raw.githubusercontent.com/visgl/deck.gl-data/master",
  "examples/scatterplot/manhattan.json",
  fsep = "/"
)
manhattan_data <- fload(url) %>%
  as_tibble(.name_repair = ~ c("lon", "lat", "species")) %>%
  mutate(
    position = sfc_point(lon, lat),
    species = as.factor(species),
    species_name = if_else(species == 1, "dog", "cat")
  )

mb_token <- readLines("~/My Drive/private/mapbox_token_bdbest.txt")
options(rdeck.mapbox_access_token = mb_token)
rdeck::mapbox_access_token()

rdeck(
  map_style = mapbox_dark(),
  initial_bounds = st_bbox(
    c(xmin=-180, ymin=-90, xmax=180, ymax=90),
    crs = st_crs(4326))
  # add a 2 pixel buffer to each point, making it easier to hover
  #picking_radius = 2
) %>%
  add_mvt_layer(
    name = "hex",
    data = "https://tile.bbnj.app/public.hexagons/{z}/{x}/{y}.pbf?step=5",
    # get_fill_color = "#0000FF") # blue
    get_fill_color = scale_color_linear(
      col = "i",
      palette = viridis(21, alpha=0.5),
      limits = c(-10, 10)),
    auto_highlight = TRUE,
    pickable = TRUE,
    tooltip = c("i","j"))



rdeck() |>
  add_mvt_layer(
    data = mvt_url("mapbox.country-boundaries-v1"),
    get_fill_color = scale_color_linear(
      col = color_group,
      palette = viridis::viridis(6),
      limits = c(1, 6)
    ),
    auto_highlight = TRUE,
    pickable = TRUE,
    tooltip = TRUE)

rdeck(
  initial_view_state = view_state(center =  c(-73.58, 45.53), zoom = 9)) |> 
  add_mvt_layer(
    data = mvt_url("dwachsmuth.borough_5"),
    get_fill_color = scale_color_linear(
      col = "canale_ind_q5_2016",
      palette = c("#FF00FF", "#00FF00"),
      limits = c(0, 5)))


  add_scatterplot_layer(
    name = "manhattan_animals",
    data = manhattan_data,
    # the coloumn in manhattan_data which contains the location of each point
    get_position = position,
    # a categorical colour scale, using the species column and a cividis colour palette
    get_fill_color = scale_color_category(
      col = species,
      palette = cividis(2)
    ),
    # the radius of each point (default 1 metre) is scaled by 30
    radius_scale = 30,
    radius_min_pixels = 0.5,
    # highlight dot density
    blending_mode = "additive",
    # interactivity
    pickable = TRUE,
    auto_highlight = TRUE,
    # per-species highlight colour
    highlight_color = scale_color_category(
      col = species,
      palette = c("#0060e6", "#fff399"),
      legend = FALSE
    ),
    tooltip = c(species, species_name)
  )
