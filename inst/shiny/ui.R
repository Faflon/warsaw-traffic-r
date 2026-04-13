shiny::fluidPage(
  
  shiny::titlePanel("Warsaw traffic disruption simulator"),
  
  shiny::sidebarLayout(
    
    shiny::sidebarPanel(
      width = 3,
      
      shiny::h4("1. Load vehicles"),
      shiny::actionButton(
        "fetch_btn", "Fetch vehicles",
        icon = shiny::icon("rotate-right"),
        width = "100%",
        class = "btn-primary"
      ),
      
      shiny::hr(),
      
      shiny::h4("2. Place disruption"),
      shiny::radioButtons(
        "disruption_type",
        label = "Disruption type",
        choices = c(
          "Road traffic - buses only" = "traffic",
          "Track blockage - trams only" = "track_blockage",
          "Both - all vehicles" = "both"
        ),
        selected = "traffic"
      ),
      shiny::sliderInput(
        "radius_m",
        label = "Disruption radius (metres)",
        min = 5, max = 500, value = 25, step = 5, post = " m"
      ),
      shiny::helpText(
        shiny::icon("circle-info"),
        " Click on the map to drop a disruption pin."
      ),
      shiny::br(),
      shiny::actionButton(
        "clear_btn", "Clear disruptions",
        icon = shiny::icon("xmark"),
        width = "100%"
      ),
      
      shiny::hr(),
      
      shiny::h4("Status"),
      shiny::textOutput("status_text"),
      
      shiny::hr(),
      
      shiny::h4("Legend"),
      shiny::tags$div(
        style = "font-size: 13px; line-height: 2;",
        shiny::tags$div(
          shiny::tags$span("\u25CF", style = "color: #2ecc71; font-size: 16px;"),
          shiny::tags$strong(" Bus"), " - on time"
        ),
        shiny::tags$div(
          shiny::tags$span("\u25CF", style = "color: orange; font-size: 16px;"),
          shiny::tags$strong(" Bus"), " - delayed"
        ),
        shiny::tags$div(
          shiny::tags$span("\u25CB", style = "color: #2ecc71; font-size: 16px;"),
          shiny::tags$strong(" Tram"), " - on time"
        ),
        shiny::tags$div(
          shiny::tags$span("\u25CB", style = "color: red; font-size: 16px;"),
          shiny::tags$strong(" Tram"), " - blocked"
        )
      )
    ),
    
    shiny::mainPanel(
      width = 9,
      leaflet::leafletOutput("map", height = "85vh")
    )
  )
)
