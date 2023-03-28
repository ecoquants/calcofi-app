# now trying to create raster table ----
# https://www.crunchydata.com/blog/waiting-for-postgis-3.2-st_interpolateraster

librarian::shelf(
  digest, jsonlite, rpostgis, sf)

get_contour <- function(
    variable, value, 
    aoi_pattern = c("standard", "extended"),
    aoi_shore   = c("nearshore", "offshore"),
    date_beg, date_end, date_qrtr=1, 
    depth_m_min, depth_m_max,
    n_bins = 7, return_raster = FALSE){
  
  # variable    = "ctd_bottles.t_degc" # input$sel_var
  # value       = "avg"                # input$sel_val
  # aoi_pattern = c("standard", "extended")
  # aoi_shore   = c("nearshore", "offshore")
  # date_beg    = as.Date(today() - dyears(5))  # input$sel_date_range[1]
  # date_end    = ctdcast_dates$max             # input$sel_date_range[2]
  # date_qrtr   = c(1, 2)
  # depth_m_min = 0                    # input$sel_depth_range[1]
  # depth_m_max = 515                  # input$sel_depth_range[2]
  # n_bins = 7
  # return_raster = FALSE
  
  args_in <- list(
    variable    = variable,
    value       = value,
    aoi_pattern = aoi_pattern,
    aoi_shore   = aoi_shore,
    date_beg    = date_beg, 
    date_end    = date_end,
    date_qrtr   = date_qrtr,
    depth_m_min = depth_m_min, 
    depth_m_max = depth_m_max,
    n_bins      = n_bins)
  args_json <- jsonlite::toJSON(args_in)
  hash <- digest(args_in, algo="crc32")
  
  # test values
  # d <- dbGetQuery(con, glue("
  #   SELECT
  #     ST_X(geom) AS x,
  #     ST_Y(geom) AS y,
  #     AVG(t_degc) AS z
  #     FROM ctd_casts
  #       JOIN ctd_bottles USING (cast_count)
  #     WHERE
  #       (depthm BETWEEN {depth_m_min} AND {depth_m_max}) AND
  #       (date BETWEEN '{date_beg}' AND '{date_end}')
  #     GROUP BY geom"))
  # hist(d$z)
  # range(d$z, na.rm=T) # 1.658276 30.353333
  
  # TODO: variable, value; use MATERIALIZED VIEW to combine all vars into single table in advance
  
  # do once
  # q <- dbSendStatement(con, "DROP TABLE z_idw"); dbClearResult(q)
  # q <- dbSendStatement(
  #   con,
  #   "CREATE TABLE z_idw (
  #     rid SERIAL PRIMARY KEY,
  #     args_hash TEXT,
  #     args_json JSON,
  #     rast RASTER)")
  # dbClearResult(q)
  # q <- dbSendStatement(con,  "SET postgis.gdal_enabled_drivers = 'ENABLE_ALL'")
  # dbClearResult(q)
  
  # OLD
  # rast_idw <- glue("z_idw_{hash}")
  # rast_idw_exists <- dbGetQuery(con, glue(
  #   "SELECT EXISTS (
  #     SELECT FROM pg_tables
  #     WHERE 
  #       schemaname = 'public' AND 
  #       tablename  = '{rast_idw}')"))$exists
  rast_idw_exists <- tbl(con, "z_idw") |> 
    filter(args_hash == hash) |> 
    collect() |> 
    nrow() > 0
  
  if (!rast_idw_exists){
    sql <- glue("
      INSERT INTO z_idw (
        args_hash, 
        args_json,
        rast)
      WITH 
      aoi AS (
        SELECT ST_Union(geom) AS geom
          FROM effort_zones 
          WHERE
            sta_pattern IN ('{paste(aoi_pattern, collapse = '\\',\\'')}') AND
            sta_shore   IN ('{paste(aoi_shore  , collapse = '\\',\\'')}') ),
      pts AS (
        SELECT 
          ST_SetSRID(
            ST_MakePoint(
              ST_X(c.geom),
              ST_Y(c.geom),
              AVG(t_degc) ),
            4326) AS geom
        FROM ctd_casts AS c 
          JOIN ctd_bottles AS cb USING (cast_count)
          JOIN aoi ON ST_Covers(aoi.geom, c.geom)
        WHERE 
          (depthm BETWEEN {depth_m_min} AND {depth_m_max}) AND
          (date BETWEEN '{date_beg}' AND '{date_end}') AND
          -- DATE_PART('year'   , date) = 2020  AND
          DATE_PART('quarter', date) IN ({paste(date_qrtr, collapse=',')})
        GROUP BY c.geom ),
      inputs AS (
        SELECT
          0.1::float8 AS pixelsize,
          -- https://gdal.org/programs/gdal_grid.html#interpolation-algorithms
          -- 'invdist:power:5.5:smoothing:2.0' AS algorithm,
          'invdist:power:2.0:smoothing:2.0' AS algorithm, -- default parameters
          ST_Collect(pts.geom) AS geom,
          ST_Expand(ST_Collect(aoi.geom), 0.5) AS ext
        FROM aoi, pts
      ),
      -- Calculate output raster geometry
      -- Use the expanded extent to take in areas beyond the limit of the
      -- temperature stations
      sizes AS (
        SELECT
          ceil((ST_XMax(ext) - ST_XMin(ext))/pixelsize)::integer AS width,
          ceil((ST_YMax(ext) - ST_YMin(ext))/pixelsize)::integer AS height,
          ST_XMin(ext) AS upperleftx,
          ST_YMax(ext) AS upperlefty
        FROM inputs
      )
      -- Feed it all into interpolation
      -- SELECT 1 AS rid,
      SELECT 
        '{hash}' AS args_hash,
        '{args_json}' AS args_json,
        ST_Clip(
          ST_SetBandNoDataValue(
            ST_InterpolateRaster(
              geom,
              algorithm,
              ST_SetSRID(
                ST_AddBand(
                  ST_MakeEmptyRaster(
                    width, height, upperleftx, upperlefty, pixelsize), 
                  '32BF'), 
                ST_SRID(geom))),
            -9999),
          (SELECT geom FROM aoi)) AS rast
      FROM sizes, inputs;")
    # cat(sql)
    q <- dbSendStatement(con,  sql)
    dbClearResult(q)
  }
  
  # pgListRast(con)
  if (return_raster){
    # r <- pgGetRast(con, name = rast_idw)
    r <- pgGetRast(
      con, name = "z_idw", clauses = glue("WHERE args_hash = '{hash}'"))
    
    return(r)
  }
  # mapview::mapView(r)
  
  # vals <- dbGetQuery(
  #   con, glue("
  #     SELECT UNNEST(ST_DumpValues(rast, 1)) AS vals FROM {rast_idw}"))
  
  # dbGetQuery(
  #   con, glue("
  #   SELECT rid, (foo.md).*
  #   FROM (
  #     SELECT rid, ST_BandMetaData(rast, 1) AS md
  #     FROM {rast_idw}
  #     WHERE rid=1 ) As foo"))
  # rid pixeltype nodatavalue isoutdb path outdbbandnum filesize
  # 1   1      32BF        -999   FALSE <NA>           NA     <NA>
  #   filetimestamp
  # 1          <NA>

  # stats <- dbGetQuery(con, glue("
  #   WITH
  #     s AS (
  #       SELECT ST_SummaryStats(rast, 1) AS stats FROM {rast_idw})
  #     SELECT (stats).* FROM s"))
  # stats
  # count           sum          mean       stddev           min      max
  # 1  8712 -1.610556e+42 -1.848664e+38 1.695028e+38 -3.402823e+38 15.18808
  # seq(stats$min, stats$max, length.out = n_bins)
  # bins <- pretty(c(stats$min, stats$max), n_bins)
  
  q <- seq(0, 1, length.out = n_bins)
  z <- dbGetQuery(con, glue("
    SELECT (pvq).*
      FROM (SELECT ST_Quantile(rast, ARRAY[{paste(q, collapse=',')}]) As pvq
            FROM z_idw WHERE args_hash='{hash}') As foo
    ORDER BY (pvq).quantile"))
  # round(z$value, 2)
  # z  
  # quantile     value
  # 1 0.0000000  4.142121
  # 2 0.1666667 10.743656
  # 3 0.3333333 11.099418
  # 4 0.5000000 11.250334
  # 5 0.6666667 11.398816
  # 6 0.8333333 11.651850
  # 7 1.0000000 27.722250
  
  p <- st_read(
    con,
    query = glue("
    WITH
      aoi AS (
        SELECT ST_Union(geom) AS geom
          FROM effort_zones 
          WHERE
            sta_pattern IN ('{paste(aoi_pattern, collapse = '\\',\\'')}') AND
            sta_shore   IN ('{paste(aoi_shore  , collapse = '\\',\\'')}') ),
      lns AS (
        SELECT (
          ST_Contour(
            rast, 1, 
            fixed_levels => ARRAY[{paste(z$value, collapse=',')}] )).*
          FROM z_idw WHERE args_hash='{hash}'),
      closed_lns AS (
        SELECT 
          ST_Union(geom) AS geom 
        FROM 
          (SELECT geom FROM lns 
           UNION ALL 
           SELECT ST_SetSRID(ST_Boundary(ST_Expand(ST_Extent(geom), -1e-10)), 4326) 
           FROM lns) sq),
      plys AS (
        SELECT
          poly_id, 
          min(polys.geom)::geometry AS geom, 
          MIN(value)  AS val_min, 
          AVG(value)  AS val_avg,
          MAX(value)  AS val_max
        FROM
          (SELECT row_number() OVER () AS poly_id, geom FROM
              (SELECT 
                 (ST_Dump(ST_Polygonize(geom))).geom
               FROM closed_lns) dump
          ) polys
        INNER JOIN lns ON ST_Intersects(polys.geom, lns.geom)
        GROUP BY poly_id)
      SELECT 
        poly_id, val_min, val_avg, val_max,
        ST_Intersection(plys.geom, aoi.geom) AS geom
        FROM plys INNER JOIN aoi ON ST_Intersects(plys.geom, aoi.geom)") )
  
  # p <- p |>
  #   mutate(
  #     val_avg = purrr::map2_dbl(val_min, val_max, mean))
  # mapview::mapView(r) +
  #   mapview::mapView(p, zcol = "val_avg")
  p
}

map_base <- function(){
  leaflet() |> 
    # add base: blue bathymetry and light brown/green topography
    addProviderTiles(
      "Esri.OceanBasemap",
      options = providerTileOptions(
        variant = "Ocean/World_Ocean_Base"),
      group = "OceanBase") |>
    # add reference: placename labels and borders
    addProviderTiles(
      "Esri.OceanBasemap",
      options = providerTileOptions(
        variant = "Ocean/World_Ocean_Reference"),
      group = "OceanLabels")
}

add_contours <- function(m, p, title){
  
  qbins <- sort(unique(c(p$val_min, p$val_max)))
  
  qpal <- colorBin("Spectral", bins = qbins, reverse = T)
  
  m |> 
    clearGroup("contours") |> 
    clearControls() |> 
    addPolygons(
      data = p, fillOpacity = 0.5, weight = 2, opacity = 0.7,
      color = ~qpal(val_avg), group = "contours") |> 
    addLegend(
      pal = qpal, values = p$val_avg, opacity = 1, 
      title = title, group = "contours") |> 
    addLayersControl(
      baseGroups = c("OceanBase", "OceanLabels"),
      overlayGroups = c("aoi", "contours"),
      options = layersControlOptions(collapsed = T))
  
}

# p <- get_contour(
#   variable = "ctd_bottles.t_degc",
#   value = "avg",
#   depth_m_min = 0,
#   depth_m_max = 5351,
#   date_beg = "1949-02-28",
#   date_end = "2020-01-26")
# mapview::mapView(p, zcol = "val_avg")
