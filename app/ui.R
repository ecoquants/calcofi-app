dashboardPage(
  
  dashboardHeader(
    title = "CalCOFI Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Overview", tabName = "tab_over"),
      menuItem(
        "Climate & Ocean Drivers", tabName = "tab_co",
        menuSubItem("Ocean Temperature",    tabName = "tab_co_temp"),
        menuSubItem("Regional Upwelling",   tabName = "tab_co_upwell"),
        menuSubItem("Hypoxia",              tabName = "tab_co_hypoxia"),
        menuSubItem("Ocean Acidification",  tabName = "tab_co_oa"),
        menuSubItem("Harmful algal blooms", tabName = "tab_co_hab"),
        menuSubItem("Picoplankton",         tabName = "tab_co_pico"),
        menuSubItem("Zooplankton",          tabName = "tab_co_zoo"),
        menuSubItem("Aquaculture",          tabName = "tab_co_aqua")),
      menuItem(
        "Ecological integrity", tabName = "tab_ei",
        menuSubItem("Forage Species", tabName = "tab_ei_forage"), 
        menuSubItem("Krill",          tabName = "tab_ei_krill"), 
        menuSubItem("Fish",           tabName = "tab_ei_fish"), 
        menuSubItem("Seabirds",       tabName = "tab_ei_seabird"), 
        menuSubItem("Groundfish",     tabName = "tab_ei_groundfish"), 
        menuSubItem("Marine mammals", tabName = "tab_ei_marmam"), 
        menuSubItem("Biodiversity",   tabName = "tab_ei_bio")))), 
  
  dashboardBody(
    tabItems(
      
      tabItem(
        "tab_over",
        fluidRow(
          box(
            title = "Survey", width = 6, height = "92vh",
            selectInput("sel_yr", "Year", yrs_lns),
            leafletOutput("map_over", height = "78vh")),
          box(
            title = "Indicators", width = 6, height = "92vh",
            fluidRow(  
              valueBoxOutput("vbox_temp", width = 6),
              valueBoxOutput("vbox_krill", width = 6)),
            fluidRow(  
              valueBoxOutput("vbox_oxygen", width = 6),
              valueBoxOutput("vbox_forage", width = 6)))))
      
      
      ))
)

