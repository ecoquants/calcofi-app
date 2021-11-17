shinyServer(function(input, output) {
  
  # tab_over ----
  # * map_over ----
  output$map_over <- renderLeaflet({
    req(input$sel_yr)
    map_survey_yr(input$sel_yr)
  })
  
  # * vbox_temp ----
  output$vbox_temp <- renderValueBox({
    valueBox(
      value    = 99,
      subtitle = "Temperature",
      icon     = icon("fas fa-thermometer-half"),
      color    = "red")
  })
  
  # * vbox_krill ----
  output$vbox_krill <- renderValueBox({
    valueBox(
      value    = 76,
      subtitle = "Krill",
      icon     = icon("fas fa-bug"),
      color    = "blue")
  })
  
  # * vbox_oxygen ----
  output$vbox_oxygen <- renderValueBox({
    valueBox(
      value    = 89,
      subtitle = "Oxygen",
      icon     = icon("fas fa-lungs"),
      color    = "blue")
  })
  
  # * vbox_forage ----
  output$vbox_forage <- renderValueBox({
    valueBox(
      value    = 30,
      subtitle = "Forage Fish",
      icon     = icon("fas fa-fish"),
      color    = "green")
  })
  
  # tab_co_temp ----
  
  # * map_temp ----
  output$map_sst <- renderLeaflet({
    req(input$sel_yrmo)
    map_sst_date(input$sel_yrmo)
  })
  
  # * plot_temp ----
  output$plot_temp <- renderLeaflet({
    plot_sst()
  })

})
