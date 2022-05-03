shinyUI(navbarPage(
  windowTitle = "CalCOFI app for oceanographic data",
  theme = bs_theme(
    version = 5,
    "navbar-padding-y" = "0", 
    "navbar-padding-x" = "0",
    "container-padding-x" = "0"),
  #h1("mapboxer"),
  mapboxerOutput("map", width = "100vw", height = "100vh") ))

