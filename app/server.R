shinyServer(function(input, output) {
  
  output$map_over <- renderLeaflet({
    req(input$sel_yr)
    cc_map_yr(input$sel_yr)
  })
  
  output$vbox_temp <- renderValueBox({
    valueBox(
      value    = 99,
      subtitle = "Temperature",
      icon     = icon("fas fa-thermometer-half"),
      color    = "red")
  })
  
  output$vbox_krill <- renderValueBox({
    valueBox(
      value    = 76,
      subtitle = "Krill",
      icon     = icon("fas fa-bug"),
      color    = "blue")
  })
  
  output$vbox_oxygen <- renderValueBox({
    valueBox(
      value    = 89,
      subtitle = "Oxygen",
      icon     = icon("fas fa-lungs"),
      color    = "blue")
  })
  
  output$vbox_forage <- renderValueBox({
    valueBox(
      value    = 30,
      subtitle = "Forage Fish",
      icon     = icon("fas fa-fish"),
      color    = "green")
  })
  
  

})
