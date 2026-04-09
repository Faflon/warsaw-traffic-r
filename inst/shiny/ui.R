shiny::fluidPage(
  
  shiny::titlePanel("Warsaw Traffic Disruption Simulator"),
  
  shiny::sidebarLayout(
    
    shiny::sidebarPanel(
      width = 3,
      
      shiny::h4("Vehicle data"),
      shiny::actionButton(
        "fetch_btn",
        "Fetch vehicles",
        icon = shiny::icon("refresh"),
        width = "100%"
      ),
      
      shiny::hr(),
      
      shiny::h4("Disruption"),
      shiny::radioButtons(
        "disruption_type",
        label = "Disruption type",
        choices = c(
          "Road traffic (buses only)"     = "traffic",
          "Track blockage (trams only)"   = "track_blockage",
          "Both (affects all vehicles)"   = "both"
        ),
        selected = "traffic"
      ),
      shiny::helpText(
        "Click anywhere on the map to drop a disruption pin.",
        "Buses ignore track blockages. Trams ignore road traffic."
      ),
      shiny::sliderInput(
        "radius_m",
        label = "Disruption radius (metres)",
        min   = 5,
        max   = 500,
        value = 25,
        step  = 5,
        post  = " m"
      ),
      
      shiny::actionButton(
        "clear_btn",
        "Clear disruptions",
        icon = shiny::icon("times"),
        width = "100%"
      ),
      
      shiny::hr(),
      
      shiny::h4("Status"),
      shiny::textOutput("status_text"),
      
      shiny::br(),
      shiny::uiOutput("legend")
    ),
    
    shiny::mainPanel(
      width = 9,
      leaflet::leafletOutput("map", height = "85vh")
    )
  )
)