fluidPage(
  
  titlePanel("Warsaw traffic disruption simulator"),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      h4("1. Load vehicles"),
      actionButton(
        "fetch_btn", "Fetch vehicles",
        icon = icon("rotate-right"),
        width = "100%",
        class = "btn-primary"
      ),
      
      hr(),
      
      h4("2. Place disruption"),
      radioButtons(
        "disruption_type",
        label = "Disruption type",
        choices = c(
          "Road traffic - buses only" = "traffic",
          "Track blockage - trams only" = "track_blockage",
          "Both - all vehicles" = "both"
        ),
        selected = "traffic"
      ),
      sliderInput(
        "radius_m",
        label = "Disruption radius (metres)",
        min = 5, max = 500, value = 25, step = 5, post = " m"
      ),
      helpText(
        icon("circle-info"),
        " Click on the map to drop a disruption pin."
      ),
      br(),
      actionButton(
        "clear_btn", "Clear disruptions",
        icon = icon("xmark"),
        width = "100%"
      ),
      
      hr(),
      
      h4("Status"),
      textOutput("status_text"),
      
      hr(),
      
      h4("Legend"),
      tags$div(
        style = "font-size: 13px; line-height: 2;",
        tags$div(
          tags$span("\u25CF", style = "color: #2ecc71; font-size: 16px;"),
          tags$strong(" Bus"), " - on time"
        ),
        tags$div(
          tags$span("\u25CF", style = "color: orange; font-size: 16px;"),
          tags$strong(" Bus"), " - delayed"
        ),
        tags$div(
          tags$span("\u25CB", style = "color: #2ecc71; font-size: 16px;"),
          tags$strong(" Tram"), " - on time"
        ),
        tags$div(
          tags$span("\u25CB", style = "color: red; font-size: 16px;"),
          tags$strong(" Tram"), " - blocked"
        ),
        # Text input to specify which line to draw
        textInput("debug_line", "Debug: Show Line Geometry (e.g., 15)"),
      )
    ),
    
    mainPanel(
      width = 9,
      leaflet::leafletOutput("map", height = "85vh")
    )
  )
)
