librarian::shelf(
  crosstalk, dplyr, ggplot2, here, lubridate, plotly, readr, shiny)

cc_bottle_rda <- here("../calcofi4r/data/cc_bottle.rda")

stopifnot(file.exists(cc_bottle_rda))
load(cc_bottle_rda)
set.seed(42)

d_0 <- cc_bottle %>% 
  filter(
    !is.na(t_degc),
    sta_dpos != 20) %>% 
  slice_sample(n=10000) %>% 
  mutate(
    quarter = factor(
      quarter, 
      levels = 1:4, labels = c("Win","Spr","Sum","Fal")))
  
# ui ----
ui <- fluidPage(
  titlePanel("CalCOFI slider filtering"),
  
  # * sidebar ----
  sidebarLayout(
    sidebarPanel(
      width = 3,
      plotlyOutput("p_quarter", height = "200px"),
      "Quarter:", verbatimTextOutput("e_quarter"),
      plotlyOutput("p_depth"),
      "Depth:",   verbatimTextOutput("e_depth")),
    
    # * mainPanel ----
    mainPanel(
      plotlyOutput("p_date", height = "200px"),
      "Date:",    verbatimTextOutput("e_date"),
      plotlyOutput("p_temp", height = "600px"))
  )
)

server <- function(input, output) {
  
  # plots ----
  
  # * p_quarter ----
  output$p_quarter <- renderPlotly({
    d_0 %>% 
      highlight_key(~quarter) %>% 
      plot_ly(x = ~quarter, type = "histogram", source = "quarter") %>% 
      highlight(
        ~quarter, 
        on = "plotly_click", off = "plotly_doubleclick") %>%  
        # persistent = T) %>% # not registered: please add `event_register(p, 'plotly_click')` to the plot (`p`)
        # dynamic    = F,
        # selectize  = F) %>% 
      layout(barmode = "overlay")
    # TODO: not persisting with previous selections in
    #   event_data("plotly_click", source = "quarter")
    #   See refs:
      # * [17 Server-side linking with shiny | Interactive web-based data visualization with R, plotly, and shiny](https://plotly-r.com/linking-views-with-shiny.html)
      # * [16 Client-side linking | Interactive web-based data visualization with R, plotly, and shiny](https://plotly-r.com/client-side-linking.html#)
      # * [Linking plotly graphs in shiny](https://talks.cpsievert.me/20191115/#27)
      # * [Advanced plotly](https://workshops.cpsievert.me/20171118/slides/day2/#35)
  })
  
  # * p_depth ----
  output$p_depth <- renderPlotly({
    plot_ly(d_0, y = ~depth_m, type = "histogram", source = "depth") %>%
      layout(
        dragmode = "select",
        selectdirection = "v",
        xaxis  = list(autorange = "reversed"),
        yaxis  = list(autorange = "reversed"))
  })
  
  # * p_date ----
  output$p_date <- renderPlotly({
    plot_ly(d_0, x = ~date, type = "histogram", source = "date") %>%
      layout(
        dragmode = "select",
        selectdirection = "h")
  })
  
  # * p_temp ----
  output$p_temp <- renderPlotly({
    
    plot_ly(
      d_rx(), y = ~t_degc, type = "violin", name = "Temperature",
      box = list(
        visible = T),
      color = I("#FF0000"),
      marker = list(
        line = list(color = "#FF0000")))
  })
  
  # filters ----
  # * f_date ----
  f_date <- reactive({
    b <- event_data("plotly_brushing", source = "date")
    if (is.null(b))
      return(TRUE)
    between(d_0$date, as.Date(b$x[1]), as.Date(b$x[2]))
  })
  
  # * f_depth ----
  f_depth <- reactive({
    b <- event_data("plotly_brushing", source = "depth")
    if (is.null(b))
      return(TRUE)
    between(d_0$depth_m, b$y[1], b$y[2])
  })
  
  # * f_quarter ----
  f_quarter <- reactive({
    k <- event_data("plotly_click", source = "quarter")
    if (is.null(k))
      return(TRUE)
    d_0$quarter %in% k$x
  })
  
  # d_rx ----
  d_rx <- reactive({
    d_0 %>% 
      filter(
        f_date(),
        f_depth(),
        f_quarter())
  })
  
  # print ----
  
  # * e_date ----
  output$e_date    <- renderPrint({
    b <- event_data("plotly_brushing", source = "date")
    b$x
  })
  
  # * e_depth ----
  output$e_depth   <- renderPrint({
    b <- event_data("plotly_brushing", source = "depth")
    b$y
  })
  
  # * e_quarter ----
  output$e_quarter <- renderPrint({
    k <- event_data("plotly_click", source = "quarter")
    k$x
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
