# now trying to create raster table ----

librarian::shelf(
  digest, dplyr, jsonlite, labeling, rpostgis, sf, terra)

get_contour <- function(
    variable, value, 
    aoi_pattern = c("standard", "extended"),
    aoi_shore   = c("nearshore", "offshore"),
    date_beg, date_end, date_qrtr=1, 
    depth_m_min, depth_m_max,
    n_bins = 7, 
    return_type = c("polygons", "raster", "points", "aoi"),
    idw_algorithm = "invdist:power:2.0:smoothing:2.0",
    rast_redo = F){
  
  return_type = return_type[1]
    
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
    variable      = variable,
    value         = value,
    aoi_pattern   = aoi_pattern,
    aoi_shore     = aoi_shore,
    date_beg      = date_beg, 
    date_end      = date_end,
    date_qrtr     = date_qrtr,
    depth_m_min   = depth_m_min, 
    depth_m_max   = depth_m_max,
    n_bins        = n_bins,
    idw_algorithm = idw_algorithm)
  args_json <- jsonlite::toJSON(args_in)
  hash <- digest(args_in, algo="crc32")
  message(glue("hash: {hash}"))
  
  # TODO: variable, value; use MATERIALIZED VIEW to combine all vars into single table in advance
  aoi_sql <- glue("
    aoi AS (
      SELECT ST_Union(geom) AS geom
      FROM effort_zones 
      WHERE
      sta_pattern IN ('{paste(aoi_pattern, collapse = '\\',\\'')}') AND
      sta_shore   IN ('{paste(aoi_shore  , collapse = '\\',\\'')}') )")
  if (return_type == "aoi"){
    sql <- glue("
      WITH
        {aoi_sql}
      SELECT geom FROM aoi
      ")
    aoi <- st_read(con, query = sql)
    return(aoi)
  }
  aoi_pts_sql <- glue("
    {aoi_sql},
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
      GROUP BY c.geom )")
  
  if (return_type == "points"){
    sql <- glue("
      WITH
        {aoi_pts_sql}
      SELECT * FROM pts
      ")
    pts <- st_read(con, query = sql) |> 
      mutate(
        z = st_coordinates(geom)[,"Z"]) |> 
      st_zm()
    return(pts)
  }
  
  inputs_sql <- glue("
    inputs AS (
      SELECT
      0.1::float8 AS pixelsize,
      -- https://gdal.org/programs/gdal_grid.html#interpolation-algorithms
      -- 'invdist:power:5.5:smoothing:2.0' AS algorithm,
      '{idw_algorithm}' AS algorithm, -- default parameters
      ST_Collect(pts.geom) AS geom,
      ST_Expand(ST_Collect(aoi.geom), 0.5) AS ext
      FROM aoi, pts)")
  if (return_type == "inputs"){
    sql <- glue("
      WITH
        {aoi_pts_sql},
        {inputs_sql}
      SELECT * FROM inputs
      ")
    inputs <- st_read(con, query = sql)
    return(inputs)
  }
  
  sizes_sql <- glue("
    -- Calculate output raster geometry
    -- Use the expanded extent to take in areas beyond the limit of the
    -- temperature stations
    sizes AS (
      SELECT
      ceil((ST_XMax(ext) - ST_XMin(ext))/pixelsize)::integer AS width,
      ceil((ST_YMax(ext) - ST_YMin(ext))/pixelsize)::integer AS height,
      ST_XMin(ext) AS upperleftx,
      ST_YMax(ext) AS upperlefty
      FROM inputs)")
  if (return_type == "sizes"){
    sql <- glue("
      WITH
        {aoi_pts_sql},
        {inputs_sql},
        {sizes_sql}
      SELECT * FROM sizes
      ")
    sizes <- dbGetQuery(con, sql)
    return(sizes)
  }
  
  # do once
  # q <- dbSendStatement(con, "DROP TABLE z_idw"); dbClearResult(q)
  # q <- dbSendStatement(
  #   con,
  #   "CREATE TABLE z_idw (
  #     rid SERIAL PRIMARY KEY,
  #     args_hash TEXT,
  #     args_json JSON,
  #     rast RASTER)")
  # dbSendStatement(
  #   con,
  #   "DROP TABLE idw_plys")
  # dbSendStatement(
  #   con,
  #   "CREATE TABLE idw_plys (
  #     oid SERIAL PRIMARY KEY,
  #     hash_id TEXT,
  #     poly_id NUMERIC,
  #     k_min NUMERIC,
  #     k_max NUMERIC,
  #     k_avg NUMERIC,
  #     val_ctr NUMERIC,
  #     val_rndm NUMERIC,
  #     geom GEOMETRY(POLYGON, 4326) )")
  # dbClearResult(q)
  # q <- dbSendStatement(con,  "SET postgis.gdal_enabled_drivers = 'ENABLE_ALL'")
  # dbClearResult(q)

  rast_idw_exists <- tbl(con, "z_idw") |> 
    filter(args_hash == hash) |> 
    collect() |> 
    nrow() > 0
  
  if (rast_idw_exists & rast_redo){
    q <- dbSendStatement(con, glue("DELETE FROM z_idw WHERE args_hash = '{hash}'"))
    dbClearResult(q)
    rast_idw_exists <- F
  }
  
  sql <- glue("
      INSERT INTO z_idw (
        args_hash, 
        args_json,
        rast)
      WITH 
      {aoi_pts_sql},
      {inputs_sql},
      {sizes_sql}
      SELECT 
        '{hash}' AS args_hash,
        '{args_json}' AS args_json,
        -- ST_Clip(
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
            -9999)
          -- (SELECT geom FROM aoi)) 
        AS rast
      FROM sizes, inputs;")
  if (return_type == "sql"){
    return(sql)
  }
  
  if (!rast_idw_exists){
    # cat(sql)
    q <- dbSendStatement(con,  sql)
    dbClearResult(q)
  }
  
  # pgListRast(con)
  if (return_type == 'raster'){
    # r <- pgGetRast(con, name = rast_idw)
    r <- pgGetRast(
      con, name = "z_idw", clauses = glue("WHERE args_hash = '{hash}'")) |> 
      rast()
    
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
  
  # ORIGINAL
  # q <- seq(0, 1, length.out = n_bins+1)
  # sql <- glue("
  #   -- SELECT (pvq).*
  #   --   FROM (SELECT ST_Quantile(rast, ARRAY[{paste(q, collapse=',')}]) As pvq
  #   --         FROM z_idw WHERE args_hash='{hash}') As foo
  #   WITH
  #     {aoi_sql},
  #     r AS (
  #       SELECT rast FROM z_idw WHERE args_hash='{hash}'),
  #     rc AS (
  #       SELECT ST_Clip(rast, (SELECT geom FROM aoi)) AS rast from r)
  #   SELECT (pvq).*
  #     FROM (SELECT ST_Quantile(rast, ARRAY[{paste(q, collapse=',')}]) As pvq
  #           FROM rc) As foo
  #   ORDER BY (pvq).quantile")
  # message(sql)
  # z <- dbGetQuery(con, sql)
  # message(glue("q: {paste(q, collapse=', ')}"))
  
  # DEBUG
  # TODO: determine breaks as pretty equal interval
  # breaks = classInt::classIntervals(
  #   v, n=7, style = "pretty")$brks
  z <- tibble(
    value = seq(9,16))
  message(glue("z: {paste(z$value, collapse=', ')}"))
  
  
  # options(digits=12) # Part of the problem is that your default digits = 7,
  # #   so you won't see any digits beyond that.
  # #  If you put too many (e.g, 22), you'll get floating point imprecision
  # #
  # #  Note the behavior for rounding off a 5 under ?round:
  # #  "IEC 60559 standard is expected to be used, ‘go to the even digit’."
  # #
  # # digits needed to distinguish between two nonequal elements
  # diff.dec.places <- function(x, y){
  #   delta <- abs(x - y)
  #   dec.place <- -ceiling(log10(abs(delta))) # your idea here was correct
  #   # print(paste0("The elements (",x," & ",y,") differ in the ",
  #   #              10^-dec.place,"'s place."))
  #   if(round(x, dec.place)==round(y, dec.place)){
  #     #print("But we add another sig. figure so they do not round to the same number.")
  #     dec.place <- dec.place + 1
  #   } 
  #   #print(paste("The elements will round to",round(x, dec.place),'&',round(y, dec.place)))
  #   dec.place
  # }
  # #
  # discr.round <- function(x){
  #   #- Find minimum-magnitude difference and which elements possess it: 
  #   #-   Create upper triangle of difference matrix of elements of vector with itself
  #   d <- abs(outer(x,x,"-"))
  #   d[lower.tri(d, diag=T)] <- NA
  #   #-   Return the smallest-magnitude difference and indices of elements forming it
  #   m <- min(d, na.rm=T)     
  #   if(m != 0){
  #     #- Round to number of dec places required to distinguish the closest elements
  #     e <- x[which(d==m, arr.ind=T)]
  #     round(x, diff.dec.places(e[1],e[2]))
  #   }
  #   else{
  #     #print("Closest elements are equal.") 
  #     x
  #   }
  # }
  # message(glue("z, discr.round(z$value): {paste(discr.round(z$value), collapse=', ')}"))
  # z$value <- discr.round(z$value)
  
  # continuous range
  # sql <- glue("
  #   WITH
  #     {aoi_sql},
  #     r AS (
  #       SELECT rast FROM z_idw WHERE args_hash='{hash}'),
  #     rc AS (
  #       SELECT ST_Clip(rast, (SELECT geom FROM aoi)) AS rast from r)
  # 
  #     SELECT (stats).*
  #     FROM (SELECT ST_SummaryStats(rast, 1) As stats
  #         FROM rc) As foo")
  # message(sql)
  # z <- dbGetQuery(con, sql)
  # message(glue("q: {paste(q, collapse=', ')}"))
  # message(glue("z: {paste(z$value, collapse=', ')}"))
  #
  # message(glue("raster range: [{z$min}, {z$max}]"))
  # brks <- extended(z$min, z$max, n_bins, only.loose = T)
  # message(glue("raster brks: {paste(brks, collapse = ', ')}"))
  # z <- tibble(
  #   value = brks)
  
  if (return_type == "lines"){
    lns <- st_read(
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
              FROM z_idw WHERE args_hash='{hash}')
          SELECT * FROM lns"))
    # message(glue("l: {paste(sort(unique(l$value)), collapse=', ')}"))
    return(lns)
  }
  
  if (return_type == "closed_lines"){
    q <- glue("
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
               -- SELECT ST_SetSRID(ST_Boundary(ST_Expand(ST_Extent(geom), -1e-10)), 4326) 
               -- FROM lns) sq)
               -- SELECT ST_SetSRID(ST_Boundary(ST_Extent(geom)), 4326) 
               -- FROM aoi) sq)
               SELECT (ST_Dump(ST_Boundary(geom))).geom
               FROM aoi) aoi)
          SELECT * FROM closed_lns")
    message(q)
    cl_lns <- st_read(
      con,
      query = q)
    return(cl_lns)
  }
  
  if (return_type == "polygons"){
    sql <- glue("
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
             SELECT (ST_Dump(ST_Boundary(geom))).geom
             FROM aoi) aoi),
        plys AS (
          SELECT
            poly_id, 
            min(polys.geom)::geometry AS geom, 
            MIN(value)  AS k_min, 
            AVG(value)  AS k_avg,
            MAX(value)  AS k_max
          FROM
            (SELECT row_number() OVER () AS poly_id, geom FROM
                (SELECT 
                   (ST_Dump(ST_Polygonize(geom))).geom
                 FROM closed_lns) dump
            ) polys
          INNER JOIN lns ON ST_Intersects(polys.geom, lns.geom)
          GROUP BY poly_id),
        plys_clip AS (
          SELECT 
            poly_id, k_min, k_avg, k_max,
              ST_Intersection(plys.geom, aoi.geom) AS geom
            FROM plys 
            INNER JOIN aoi ON ST_Intersects(plys.geom, aoi.geom) ),
        plys_ctr AS (
          SELECT 
            *, 
            ST_PointOnSurface(geom) AS geom_ctr -- centroid always interior to polygon
          FROM plys_clip
          WHERE ST_GeometryType(geom) IN ('ST_Polygon','ST_MultiPolygon') ),
        plys_ctr_rast AS (
          SELECT
            poly_id, k_min, k_avg, k_max,
            ST_Value(rast, 1, p.geom_ctr) AS val_ctr, -- requires point geometry
            geom
          FROM plys_ctr p, z_idw z
          WHERE 
            z.args_hash = '{hash}' AND
            ST_Intersects(z.rast, p.geom_ctr) )
        SELECT
          * 
          -- ST_Multi(ST_Union(gd.geom) ) As geometry
        FROM plys_ctr_rast
        -- GROUP BY gd.gid_4

        -- OLD: slow and val_avg = NA for many polygons...
        --  plys_rast AS (
        --    SELECT  poly_id, (stats).*
        --    FROM (
        --      SELECT poly_id, ST_SummaryStats(ST_Clip(rast, 1, geom, TRUE)) As stats
        --      FROM z_idw z
        --      INNER JOIN plys_clip p ON ST_Intersects(p.geom, z.rast) 
        --      WHERE z.args_hash='{hash}') As foo ),
        --  plys_stats AS (
        --    SELECT 
        --      poly_id, 
        --      SUM(count) AS n_pixels, 
        --      MIN(min) AS val_min,
        --      MAX(max) AS val_max,
        --      SUM(mean*count)/SUM(count) AS val_avg
        --    FROM plys_rast
        --    WHERE count > 0
        --    GROUP BY poly_id
        --    ORDER BY poly_id)
        --  SELECT pc.*, ps.* 
        --  FROM
        --  plys_clip pc
        --  LEFT JOIN 
        --    plys_stats ps USING (poly_id)
        ")
    message(sql)
    p <- st_read(
      con,
      query = sql)
  }
  
  if (return_type == "polygons2"){
    sql <- glue("
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
             -- SELECT ST_SetSRID(ST_Boundary(ST_Expand(ST_Extent(geom), -1e-10)), 4326) 
             -- FROM lns) sq)
             --SELECT ST_SetSRID(ST_Boundary(ST_Extent(geom)), 4326) 
             -- SELECT ST_SetSRID(ST_Boundary(ST_Extent(geom)), 4326) 
             -- FROM aoi) sq),
             SELECT (ST_Dump(ST_Boundary(geom))).geom
             FROM aoi) aoi),
        plys AS (
          SELECT
            poly_id, 
            min(polys.geom)::geometry AS geom, 
            MIN(value)  AS k_min, 
            AVG(value)  AS k_avg,
            MAX(value)  AS k_max
          FROM
            (SELECT row_number() OVER () AS poly_id, geom FROM
                (SELECT 
                   (ST_Dump(ST_Polygonize(geom))).geom
                 FROM closed_lns) dump
            ) polys
          INNER JOIN lns ON ST_Intersects(polys.geom, lns.geom)
          GROUP BY poly_id),
        plys_clip AS (
          SELECT 
            poly_id, k_min, k_avg, k_max,
              ST_Intersection(plys.geom, aoi.geom) AS geom
            FROM plys 
            INNER JOIN aoi ON ST_Intersects(plys.geom, aoi.geom) ),
        plys_ctr AS (
          SELECT 
            *, 
            ST_PointOnSurface(geom) AS geom_ctr -- centroid always interior to polygon
          FROM plys_clip
          WHERE ST_GeometryType(geom) IN ('ST_Polygon','ST_MultiPolygon') ),
        plys_ctr_rast AS (
          SELECT
            poly_id, k_min, k_avg, k_max,
            ST_Value(rast, 1, p.geom_ctr) AS val_ctr,
            geom
          FROM plys_ctr p, z_idw z
          WHERE 
            z.args_hash = '{hash}' AND
            ST_Intersects(z.rast, p.geom_ctr)
        ),
        pts_rndm AS (
          SELECT 
            poly_id,
            ST_GeneratePoints(geom, npoints => 20, seed => 42) AS geom_pt
          FROM plys_ctr_rast),
        pts_rndm_rast AS (
          SELECT
            poly_id,
            AVG(ST_Value(rast, 1, geom_pt)) AS val_rndm
          FROM pts_rndm p, z_idw z
          WHERE 
            z.args_hash = '{hash}' AND
            ST_Intersects(z.rast, p.geom_pt)
          GROUP BY poly_id )
        SELECT
          poly_id, k_min, k_avg, k_max, val_ctr,
          val_rndm
        FROM plys_ctr_rast
          LEFT JOIN pts_rndm_rast USING (poly_id) 
          
        -- OLD: slow and val_avg = NA for many polygons...
        --  plys_rast AS (
        --    SELECT  poly_id, (stats).*
        --    FROM (
        --      SELECT poly_id, ST_SummaryStats(ST_Clip(rast, 1, geom, TRUE)) As stats
        --      FROM z_idw z
        --      INNER JOIN plys_clip p ON ST_Intersects(p.geom, z.rast) 
        --      WHERE z.args_hash='{hash}') As foo ),
        --  plys_stats AS (
        --    SELECT 
        --      poly_id, 
        --      SUM(count) AS n_pixels, 
        --      MIN(min) AS val_min,
        --      MAX(max) AS val_max,
        --      SUM(mean*count)/SUM(count) AS val_avg
        --    FROM plys_rast
        --    WHERE count > 0
        --    GROUP BY poly_id
        --    ORDER BY poly_id)
        --  SELECT pc.*, ps.* 
        --  FROM
        --  plys_clip pc
        --  LEFT JOIN 
        --    plys_stats ps USING (poly_id)
        ")
    message(sql)
    p <- st_read(
      con,
      query = sql)
  }
  
  if (return_type == "polygons3"){
    sql <- glue("
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
             SELECT (ST_Dump(ST_Boundary(geom))).geom
             FROM aoi) aoi),
        plys AS (
          SELECT
            poly_id, 
            min(polys.geom)::geometry AS geom, 
            MIN(value)  AS k_min, 
            AVG(value)  AS k_avg,
            MAX(value)  AS k_max
          FROM
            (SELECT row_number() OVER () AS poly_id, geom FROM
                (SELECT 
                   (ST_Dump(ST_Polygonize(geom))).geom
                 FROM closed_lns) dump
            ) polys
          INNER JOIN lns ON ST_Intersects(polys.geom, lns.geom)
          GROUP BY poly_id),
        plys_clip AS (
          SELECT 
            poly_id, k_min, k_avg, k_max,
              ST_Intersection(plys.geom, aoi.geom) AS geom
            FROM plys 
            INNER JOIN aoi ON ST_Intersects(plys.geom, aoi.geom) ),
        plys_ctr AS (
          SELECT 
            *, 
            ST_PointOnSurface(geom) AS geom_ctr -- centroid always interior to polygon
          FROM plys_clip
          WHERE ST_GeometryType(geom) IN ('ST_Polygon','ST_MultiPolygon') ),
        plys_ctr_rast AS (
          SELECT
            poly_id, k_min, k_avg, k_max,
            ST_Value(rast, 1, p.geom_ctr) AS val_ctr, -- requires point geometry
            geom
          FROM plys_ctr p, z_idw z
          WHERE 
            z.args_hash = '{hash}' AND
            ST_Intersects(z.rast, p.geom_ctr) ),
        -- OLD: slow and val_avg = NA for many polygons...
          plys_rast AS (
            SELECT  poly_id, (stats).*
            FROM (
              SELECT poly_id, ST_SummaryStats(ST_Clip(rast, 1, geom, TRUE)) As stats
              FROM z_idw z
              INNER JOIN plys_ctr_rast p ON ST_Intersects(p.geom, z.rast) 
              WHERE z.args_hash='{hash}') As foo ),
          plys_stats AS (
            SELECT 
              poly_id, 
              SUM(count) AS n_pixels, 
              MIN(min) AS val_min,
              MAX(max) AS val_max,
              SUM(mean*count)/SUM(count) AS val_avg
            FROM plys_rast
            WHERE count > 0
            GROUP BY poly_id
            ORDER BY poly_id)
          SELECT 
            pc.*,
            n_pixels, val_min, val_max, val_avg
          FROM
          plys_ctr_rast pc
          LEFT JOIN 
            plys_stats ps USING (poly_id)
        ")
    message(sql)
    p <- st_read(
      con,
      query = sql)
  }
  
  if (return_type == "insert_polygons"){
    sql <- glue("
      INSERT INTO idw_plys(
        hash_id,
        poly_id,
        k_min,
        k_max,
        k_avg,
        val_ctr,
        val_rndm,
        geom)
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
             -- SELECT ST_SetSRID(ST_Boundary(ST_Expand(ST_Extent(geom), -1e-10)), 4326) 
             -- FROM lns) sq)
             --SELECT ST_SetSRID(ST_Boundary(ST_Extent(geom)), 4326) 
             -- SELECT ST_SetSRID(ST_Boundary(ST_Extent(geom)), 4326) 
             -- FROM aoi) sq),
             SELECT (ST_Dump(ST_Boundary(geom))).geom
             FROM aoi) aoi),
        plys AS (
          SELECT
            poly_id, 
            min(polys.geom)::geometry AS geom, 
            MIN(value)  AS k_min, 
            AVG(value)  AS k_avg,
            MAX(value)  AS k_max
          FROM
            (SELECT row_number() OVER () AS poly_id, geom FROM
                (SELECT 
                   (ST_Dump(ST_Polygonize(geom))).geom
                 FROM closed_lns) dump
            ) polys
          INNER JOIN lns ON ST_Intersects(polys.geom, lns.geom)
          GROUP BY poly_id),
        plys_clip AS (
          SELECT 
            poly_id, k_min, k_avg, k_max,
              ST_Intersection(plys.geom, aoi.geom) AS geom
            FROM plys 
            INNER JOIN aoi ON ST_Intersects(plys.geom, aoi.geom) ),
        plys_ctr AS (
          SELECT 
            *, 
            ST_PointOnSurface(geom) AS geom_ctr -- centroid always interior to polygon
          FROM plys_clip
          WHERE ST_GeometryType(geom) IN ('ST_Polygon','ST_MultiPolygon') ),
        plys_ctr_rast AS (
          SELECT
            poly_id, k_min, k_avg, k_max,
            ST_Value(rast, 1, p.geom_ctr) AS val_ctr,
            geom
          FROM plys_ctr p, z_idw z
          WHERE 
            z.args_hash = '{hash}' AND
            ST_Intersects(z.rast, p.geom_ctr)
        )
        --pts_rndm AS (
        --  SELECT 
        --    poly_id,
        --    ST_GeneratePoints(geom, npoints => 20, seed => 42) AS geom_pt
        --  FROM plys_ctr_rast),
        --pts_rndm_rast AS (
        --  SELECT
        --    poly_id,
        --    AVG(ST_Value(rast, 1, geom_pt)) AS val_rndm
        --  FROM pts_rndm p, z_idw z
        --  WHERE 
        --    z.args_hash = '{hash}' AND
        --    ST_Intersects(z.rast, p.geom_pt)
        --  GROUP BY poly_id )
        SELECT
          '{hash}' AS hash_id,
          poly_id, k_min, k_max, k_avg, val_ctr,
          NULL AS val_rndm,
          geom
        FROM plys_ctr_rast 
          
        -- OLD: slow and val_avg = NA for many polygons...
        --  plys_rast AS (
        --    SELECT  poly_id, (stats).*
        --    FROM (
        --      SELECT poly_id, ST_SummaryStats(ST_Clip(rast, 1, geom, TRUE)) As stats
        --      FROM z_idw z
        --      INNER JOIN plys_clip p ON ST_Intersects(p.geom, z.rast) 
        --      WHERE z.args_hash='{hash}') As foo ),
        --  plys_stats AS (
        --    SELECT 
        --      poly_id, 
        --      SUM(count) AS n_pixels, 
        --      MIN(min) AS val_min,
        --      MAX(max) AS val_max,
        --      SUM(mean*count)/SUM(count) AS val_avg
        --    FROM plys_rast
        --    WHERE count > 0
        --    GROUP BY poly_id
        --    ORDER BY poly_id)
        --  SELECT pc.*, ps.* 
        --  FROM
        --  plys_clip pc
        --  LEFT JOIN 
        --    plys_stats ps USING (poly_id)
        ")
    message(sql)
    p <- dbSendStatement(con, sql)
  }
  
  if (return_type == "insert_polygons2"){
    sql <- glue("
      INSERT INTO idw_plys(
        hash_id,
        poly_id,
        k_min,
        k_max,
        k_avg,
        val_ctr,
        val_rndm,
        geom)
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
             -- SELECT ST_SetSRID(ST_Boundary(ST_Expand(ST_Extent(geom), -1e-10)), 4326) 
             -- FROM lns) sq)
             --SELECT ST_SetSRID(ST_Boundary(ST_Extent(geom)), 4326) 
             -- SELECT ST_SetSRID(ST_Boundary(ST_Extent(geom)), 4326) 
             -- FROM aoi) sq),
             SELECT (ST_Dump(ST_Boundary(geom))).geom
             FROM aoi) aoi),
        plys AS (
          SELECT
            poly_id, 
            min(polys.geom)::geometry AS geom, 
            MIN(value)  AS k_min, 
            AVG(value)  AS k_avg,
            MAX(value)  AS k_max
          FROM
            (SELECT row_number() OVER () AS poly_id, geom FROM
                (SELECT 
                   (ST_Dump(ST_Polygonize(geom))).geom
                 FROM closed_lns) dump
            ) polys
          INNER JOIN lns ON ST_Intersects(polys.geom, lns.geom)
          GROUP BY poly_id),
        plys_clip AS (
          SELECT 
            poly_id, k_min, k_avg, k_max,
              ST_Intersection(plys.geom, aoi.geom) AS geom
            FROM plys 
            INNER JOIN aoi ON ST_Intersects(plys.geom, aoi.geom) ),
        plys_ctr AS (
          SELECT 
            *, 
            ST_PointOnSurface(geom) AS geom_ctr -- centroid always interior to polygon
          FROM plys_clip
          WHERE ST_GeometryType(geom) IN ('ST_Polygon','ST_MultiPolygon') ),
        plys_ctr_rast AS (
          SELECT
            poly_id, k_min, k_avg, k_max,
            ST_Value(rast, 1, p.geom_ctr) AS val_ctr,
            geom
          FROM plys_ctr p, z_idw z
          WHERE 
            z.args_hash = '{hash}' AND
            ST_Intersects(z.rast, p.geom_ctr)
        ),
        pts_rndm AS (
          SELECT 
            poly_id,
            ST_GeneratePoints(geom, npoints => 20, seed => 42) AS geom_pt
          FROM plys_ctr_rast),
        pts_rndm_rast AS (
          SELECT
            poly_id,
            AVG(ST_Value(rast, 1, geom_pt)) AS val_rndm
          FROM pts_rndm p, z_idw z
          WHERE 
            z.args_hash = '{hash}' AND
            ST_Intersects(z.rast, p.geom_pt)
          GROUP BY poly_id )
        SELECT
          '{hash}' AS hash_id,
          poly_id, k_min, k_max, k_avg, val_ctr,
          val_rndm,
          geom
        FROM plys_ctr_rast
          LEFT JOIN pts_rndm_rast USING (poly_id) 
          
        -- OLD: slow and val_avg = NA for many polygons...
        --  plys_rast AS (
        --    SELECT  poly_id, (stats).*
        --    FROM (
        --      SELECT poly_id, ST_SummaryStats(ST_Clip(rast, 1, geom, TRUE)) As stats
        --      FROM z_idw z
        --      INNER JOIN plys_clip p ON ST_Intersects(p.geom, z.rast) 
        --      WHERE z.args_hash='{hash}') As foo ),
        --  plys_stats AS (
        --    SELECT 
        --      poly_id, 
        --      SUM(count) AS n_pixels, 
        --      MIN(min) AS val_min,
        --      MAX(max) AS val_max,
        --      SUM(mean*count)/SUM(count) AS val_avg
        --    FROM plys_rast
        --    WHERE count > 0
        --    GROUP BY poly_id
        --    ORDER BY poly_id)
        --  SELECT pc.*, ps.* 
        --  FROM
        --  plys_clip pc
        --  LEFT JOIN 
        --    plys_stats ps USING (poly_id)
        ")
    message(sql)
    p <- dbSendStatement(con, sql)
  }
  
  # p <- p |>
  #   mutate(
  #     val_avg = purrr::map2_dbl(val_min, val_max, mean))
  # mapview::mapView(r) +
  #   mapview::mapView(p, zcol = "val_avg")
  attr(p, "breaks") <- z
  p
}

map_base <- function(base_opacity=0.5){
  leaflet() |> 
    # add base: blue bathymetry and light brown/green topography
    addProviderTiles(
      "Esri.OceanBasemap",
      options = providerTileOptions(
        variant = "Ocean/World_Ocean_Base",
        opacity = base_opacity),
      group = "OceanBase") |>
    # add reference: placename labels and borders
    addProviderTiles(
      "Esri.OceanBasemap",
      options = providerTileOptions(
        variant = "Ocean/World_Ocean_Reference",
        opacity = base_opacity),
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

# DEBUG functions for scripts:explore_pg_contour2.qmd ----
interp_rast <- function(algorithm){
  r <- do.call(
    get_contour,
    c(contour_args, list(
      return_type   = "raster",
      idw_algorithm = algorithm)))
  map_rp(r, p)
}

map_rp <- function(r,p){
  rng_r <- global(r, "range", na.rm=T) |> as.numeric()
  rng_p <- range(p$z)
  rng <- range(c(rng_r, rng_p))
  
  pal <- colorNumeric("Spectral", rng, na.color = NA)
  
  map_base(base_opacity = 0.2) |> 
    addRasterImage(
      r, colors = pal, opacity=0.7, group = "raster") |>
    addLegend(
      pal = pal, values = rng) |> 
    addCircleMarkers(
      data   = p,
      radius = 5, fillOpacity = 0.2,
      stroke = T, weight = 1, opacity = 0.8,
      color  = ~pal(z), group = "points") |> 
    addLayersControl(
      # baseGroups    = c("OceanBase", "OceanLabels"),
      overlayGroups = c("raster", "points"),
      options       = layersControlOptions(collapsed = T),
      position      = "topleft") |> 
    hideGroup("points")
}

map_rpk <- function(r, p, k){
  rng_r <- global(r, "range", na.rm=T) |> as.numeric()
  rng_p <- range(p$z)
  rng_k <- range(c(k$val_min, k$val_max))
  rng <- range(c(rng_r, rng_p, rng_k))
  
  pal <- colorNumeric("Spectral", rng, na.color = NA)
  
  qbins <- sort(unique(c(k$val_min, k$val_max)))
  message(glue("k$val_avg: {paste(sort(unique(c(k$val_min, k$val_max))), collapse=', ')}"))
  message(glue("    qbins: {paste(qbins, collapse=', ')}", .trim = F))
  qpal <- colorBin("Spectral", bins = qbins, reverse = T)
  
  map_base(base_opacity = 0.2) |> 
    addRasterImage(
      r, colors = pal, opacity=0.7, group = "raster") |>
    addLegend(
      pal = pal, values = rng, title="raster") |> 
    addCircleMarkers(
      data   = p,
      radius = 5, fillOpacity = 0.2,
      stroke = T, weight = 1, opacity = 0.8,
      color  = ~pal(z), group = "points") |> 
    addPolygons(
      data = p, fillOpacity = 0.5, weight = 2, opacity = 0.7,
      color = ~qpal(val_avg), group = "contours") |> 
    addLegend(
      pal = qpal, values = p$val_avg, opacity = 1, 
      title = "contours", group = "contours") |> 
    addLayersControl(
      # baseGroups    = c("OceanBase", "OceanLabels"),
      overlayGroups = c("raster", "points","contours"),
      options       = layersControlOptions(collapsed = T),
      position      = "topleft") |> 
    hideGroup(c("points","raster"))
}


# p <- get_contour(
#   variable = "ctd_bottles.t_degc",
#   value = "avg",
#   depth_m_min = 0,
#   depth_m_max = 5351,
#   date_beg = "1949-02-28",
#   date_end = "2020-01-26")
# mapview::mapView(p, zcol = "val_avg")

options(digits=12) # Part of the problem is that your default digits = 7,
#   so you won't see any digits beyond that.
#  If you put too many (e.g, 22), you'll get floating point imprecision
#
#  Note the behavior for rounding off a 5 under ?round:
#  "IEC 60559 standard is expected to be used, ‘go to the even digit’."
#
# digits needed to distinguish between two nonequal elements
diff.dec.places <- function(x, y){
  delta <- abs(x - y)
  dec.place <- -ceiling(log10(abs(delta))) # your idea here was correct
  # print(paste0("The elements (",x," & ",y,") differ in the ",
  #              10^-dec.place,"'s place."))
  if(round(x, dec.place)==round(y, dec.place)){
    #print("But we add another sig. figure so they do not round to the same number.")
    dec.place <- dec.place + 1
  }
  #print(paste("The elements will round to",round(x, dec.place),'&',round(y, dec.place)))
  dec.place
}
#
discr.round <- function(x){
  #- Find minimum-magnitude difference and which elements possess it:
  #-   Create upper triangle of difference matrix of elements of vector with itself
  d <- abs(outer(x,x,"-"))
  d[lower.tri(d, diag=T)] <- NA
  #-   Return the smallest-magnitude difference and indices of elements forming it
  m <- min(d, na.rm=T)
  if(m != 0){
    #- Round to number of dec places required to distinguish the closest elements
    e <- x[which(d==m, arr.ind=T)]
    round(x, diff.dec.places(e[1],e[2]))
  }
  else{
    #print("Closest elements are equal.")
    x
  }
}
# message(glue("z, discr.round(z$value): {paste(discr.round(z$value), collapse=', ')}"))
# z$value <- discr.round(z$value)