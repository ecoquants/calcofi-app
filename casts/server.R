shinyServer(function(input, output, session) {


  output$map <- renderRdeck({

    # req(input$sel_aois)

    # get extent of features in db  # input = list(sel_aois = "public.ply_shlfs_s05")
    # schema_tbl <- input$sel_aois
    # schema_tbl <- "public.places"
    schema_tbl <- "public.ctd_casts"
    pts        <- str_split(schema_tbl, "\\.")[[1]]
    schema     <- pts[1]
    tbl        <- pts[2]
    # fld_geom   <- ifelse(schema == "public", "geometry", "geom")
    fld_geom   <- "geom"

    # * get extent of features in db ----
    # if (schema_tbl == "public.ply_rgns"){
    #   b <- c(-193.3, 14.8, -14.7, 74.9)
    # } else {
    b <- dbGetQuery(con, glue(
      # "SELECT ST_Extent({fld_geom}) AS ext FROM {schema_tbl}")) |>         # slow but exact
      "SELECT ST_EstimatedExtent('{schema}', '{tbl}', '{fld_geom}') AS ext;")) |>   # fast but inexact since VACUUM ANALYZE last run
      pull(ext) |>
      str_replace_all("BOX\\((.*)\\)", "\\1") |>
      str_split("[ ,]") %>%
      .[[1]] |>
      as.numeric()  # xmin, ymin, xmax, ymax
    # }
    # message(glue("using extent: {paste(b, collapse=', ')}"))

    # * get fields to display ----
    flds <<- dbGetQuery(con, glue(
      "SELECT column_name FROM information_schema.columns
      WHERE
        table_schema = '{schema}' AND
        table_name   = '{tbl}' AND
        column_name NOT IN ('geom','geometry');")) |>
      pull(column_name)
    # message(glue("with flds: {paste(flds, collapse=', ')}"))
    
    rdeck(
      map_style      = mapbox_dark(),
      theme          = "light",
      initial_bounds = st_bbox(
        c(xmin=b[1], ymin=b[2], xmax=b[3], ymax=b[4]),
        crs = st_crs(4326)),
      # editor         = T)  |>
      editor         = F,
      layer_selector = F)  |>
      # add_tile_layer(
      #   id                = "nspp",
      #   name              = "nspp",
      #   visible           = F,
      #   visibility_toggle = T,
      #   opacity           = 0.5,
      #   data              = nspp_tile_url) |>
      add_mvt_layer(
        id                = "ctd_casts",
        name              = "ctd_casts",
        # data              = glue("https://tile.marinesensitivity.org/{schema_tbl}/{{z}}/{{x}}/{{y}}.pbf"),
        data              = rx_url(),
        auto_highlight    = T,
        pickable          = T,
        tooltip           = all_of(!!flds), # c(mms_region, opd_name, prot_aprv, prot_numbe),
        # max_zoom          = 10,
        # visibility_toggle = T,
        opacity           = 0.5,
        # line_width_scale  = 1,
        # line_width_units  = "pixels",
        point_radius_scale = 4,
        point_radius_units = "pixels",
        point_antialiasing = TRUE,
        point_type         = "circle",
        # get_fill_color    = "#0000FF80",  # blue 0.5 opacity
        # get_line_color    = "#0000FFCC")  # blue 0.8 opacity
        get_fill_color     = "#FF0000",
        highlight_color    = "#FFFF00")

  })

  observe({
    d_c <- rdeck_proxy("map") |>
      get_clicked_object(session)  # default: NULL

    req(d_c)
    # message(str(d_c))
  })

  # observe({
  #   req(input$sel_aois)
  #
  #   # get extent of features in db  # input = list(sel_aois = "public.ply_shlfs_s05")
  #   schema_tbl <- input$sel_aois
  #   pts        <- str_split(schema_tbl, "\\.")[[1]]
  #   schema     <- pts[1]
  #   tbl        <- pts[2]
  #   fld_geom   <- ifelse(schema == "public", "geometry", "geom")
  #
  #   # * get fields to display ----
  #   flds <<- dbGetQuery(con, glue(
  #     "SELECT column_name FROM information_schema.columns
  #     WHERE
  #       table_schema = '{schema}' AND
  #       table_name   = '{tbl}';")) |>
  #     pull(column_name)
  #   message(glue("retry flds: {paste(flds, collapse=', ')}"))
  #
  #   rdeck_proxy("map") |>
  #     update_mvt_layer(
  #       id      = "aoi",
  #       tooltip = all_of(flds)) # c(mms_region, opd_name, prot_aprv, prot_numbe),
  #
  # })

  # observe({
  #   b <- rdeck_proxy("map") |>
  #     get_view_bounds(session)
  #   message(glue("extent: {paste(b, collapse=', ')}"))
  # })

  observe({
    # spatial data frame of edited (and uploaded) feature
    s_e <- rdeck_proxy("map") |>
      get_edited_features(session)  # default: Simple feature collection with 0 features and 0 fields
    # d_edited |> st_geometry() |> st_as_text()
  })

  
  # filters ----
  
  # * p_date ----
  output$p_date <- renderPlotly({
    # TODO: see [crossfilter_compare app](https://github.com/plotly/plotly.R/blob/master/inst/examples/shiny/crossfilter_compare/app.R)
    
    plot_ly(d_0, x = ~date, type = "histogram", source = "date") |> 
      layout(
        dragmode = "select",
        selectdirection = "h")
  })
  
  # filters ----
  
  # * f_date ----
  f_date <- reactive({
    b <- event_data("plotly_brushing", source = "date")
    if (is.null(b))
      return(TRUE)
    between(d_0$date, as.Date(b$x[1]), as.Date(b$x[2]))
  })
  
  # d_rx ----
  d_rx <- reactive({
    d_0 |> 
      filter(
        f_date())
        # f_depth(),
        # f_quarter())
  })
  
  # rx_url ----
  rx_url <- reactive({
    schema_tbl <- "public.ctd_casts"
    url        <- glue("https://tile.calcofi.io/{schema_tbl}/{{z}}/{{x}}/{{y}}.pbf")
    # https://github.com/CrunchyData/pg_tileserv/blob/master/hugo/content/usage/cql.md#between-predicate
    
    # date
    b <- event_data("plotly_brushing", source = "date")
    if (!is.null(b))
      url <- glue("{url}?filter=date BETWEEN '{as.Date(b$x[1])}' AND '{as.Date(b$x[2])}'")
    
    url
  })
  
  # print ----
  
  # * e_date ----
  output$e_date    <- renderPrint({
    b <- event_data("plotly_brushing", source = "date")
    b$x
  })
  
})
