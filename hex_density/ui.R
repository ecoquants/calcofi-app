shinyUI(fluidPage(
  windowTitle = "CalCOFI density",
  theme = bs_theme(
    version               = 5,
    bootswatch            = "darkly",
    "font-size-base"      = "0.8rem",
    "navbar-padding-y"    = "0", 
    "navbar-padding-x"    = "0",
    "container-padding-x" = "0"),
  
  tags$head(
    includeCSS("styles.css")),
  
  # area, spp, scapes, benthic, fishing, vgpm
  fluidRow(
    id = "params",
    column(
      12, 
      numericInput(
        "num_max", 
        label = h3("Max for color scale"), 
        value = 1000) 
      # switchInput(
      #   "tgl_hex",
      #   label = "Hexagons",
      #   value = T),
      # switchInput(
      #   "tgl_pts",
      #   label = "Points",
      #   value = T) ) 
    )),
  
  rdeckOutput("map", width = "100vw", height = "100vh")
))

