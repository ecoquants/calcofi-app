dashboardPage(
  dashboardHeader(
    title = "Old Faithful Geyser Data"),
  dashboardSidebar(
    sliderInput(
      "bins",
      "Number of bins:",
      min   = 1,
      max   = 50,
      value = 30),
    textInput(
      "txt_carnum",
      "Car #",
      init_carnum)),
  dashboardBody(
    plotOutput("distPlot"),
    em("Car #"), textOutput("carnum"))
)

