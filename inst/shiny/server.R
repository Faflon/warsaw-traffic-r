server <- function(input, output, session) {
  
  # --- Startup ---
  # Load pre-processed route shapes once when the app starts
  route_shapes <- tryCatch({
    readRDS(system.file("data", "warsaw_routes.rds", package = "WarsawTraffic"))
  }, error = function(e) {
    NULL
  })
  
  # Create the TransitNetwork fleet manager
  network <- TransitNetwork$new()
  
  # Reactive value holding current vehicle sf data (NULL until first fetch)
  vehicle_data <- shiny::reactiveVal(NULL)
  
  # Reactive value holding current disruption pin coordinates
  disruption_pin <- shiny::reactiveVal(NULL)
  
  # --- Initial map ---
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet() |>
      leaflet::addTiles() |>
      leaflet::setView(lng = 21.01, lat = 52.23, zoom = 12)
  })
  
  # --- Status text ---
  output$status_text <- shiny::renderText({
    "No vehicles loaded. Click 'Fetch vehicles' to begin."
  })
  
  # --- Legend ---
  output$legend <- shiny::renderUI({
    shiny::tags$div(
      shiny::tags$p(
        shiny::tags$span("\u25CF", style = "color: #2ecc71; font-size: 16px;"),
        " On time"
      ),
      shiny::tags$p(
        shiny::tags$span("\u25CF", style = "color: #e67e22; font-size: 16px;"),
        " Bus delayed"
      ),
      shiny::tags$p(
        shiny::tags$span("\u25CF", style = "color: #e74c3c; font-size: 16px;"),
        " Tram blocked"
      )
    )
  })
  
  # --- Fetch button handler ---
  shiny::observeEvent(input$fetch_btn, {
    
    api_key <- Sys.getenv("WARSAW_API_KEY")
    
    if (api_key == "") {
      output$status_text <- shiny::renderText({
        "Error: WARSAW_API_KEY not found in .Renviron. Please add it and restart R."
      })
      return()
    }
    
    output$status_text <- shiny::renderText({ "Fetching vehicles..." })
    
    result <- tryCatch({
      
      buses <- fetch_warsaw_transit(api_key, vehicle_type = 1)
      trams <- fetch_warsaw_transit(api_key, vehicle_type = 2)
      
      if (nrow(buses) > 0) network$update_fleet(buses, vehicle_type = 1)
      if (nrow(trams) > 0) network$update_fleet(trams, vehicle_type = 2)
      
      network$get_spatial_data()
      
    }, error = function(e) {
      output$status_text <- shiny::renderText({
        paste("Error fetching data:", conditionMessage(e))
      })
      return(NULL)
    })
    
    if (!is.null(result)) {
      vehicle_data(result)
      output$status_text <- shiny::renderText({
        n_delayed <- sum(result$is_delayed, na.rm = TRUE)
        paste0(
          network$fleet_size, " vehicles loaded. ",
          n_delayed, " delayed or blocked."
        )
      })
    }
  })
  
  # --- Map rendering observer (fires whenever vehicle_data changes) ---
  shiny::observe({
    shiny::req(vehicle_data())
    vd <- vehicle_data()
    coords <- sf::st_coordinates(vd)
    vd$lng <- coords[, 1]
    vd$lat_coord <- coords[, 2]
    
    # Assign color per vehicle status
    vd$color <- dplyr::case_when(
      vd$vehicle_type == "tram" & vd$is_blocked ~ "#e74c3c",
      vd$vehicle_type == "bus"  & vd$is_delayed ~ "#e67e22",
      TRUE ~ "#2ecc71"
    )
    
    # Build popup text
    vd$popup <- paste0(
      "<b>Line:</b> ", vd$line, "<br>",
      "<b>ID:</b> ", vd$id, "<br>",
      "<b>Type:</b> ", vd$vehicle_type, "<br>",
      "<b>Status:</b> ", dplyr::case_when(
        vd$vehicle_type == "tram" & vd$is_blocked ~ "Blocked",
        vd$vehicle_type == "bus"  & vd$is_delayed ~ "Delayed",
        TRUE ~ "On time"
      )
    )
    
    leaflet::leafletProxy("map") |>
      leaflet::clearGroup("vehicles") |>
      leaflet::addCircleMarkers(
        lng    = vd$lng,
        lat    = vd$lat_coord,
        color  = vd$color,
        fill   = TRUE,
        fillColor = vd$color,
        fillOpacity = 0.9,
        radius = 6,
        stroke = FALSE,
        popup  = vd$popup,
        group  = "vehicles"
      )
  })
  
  # --- Clear button handler ---
  shiny::observeEvent(input$clear_btn, {
    network$reset_disruptions()
    disruption_pin(NULL)
    vehicle_data(network$get_spatial_data())
    
    leaflet::leafletProxy("map") |>
      leaflet::clearGroup("disruption_pin")
    
    output$status_text <- shiny::renderText({
      paste0(network$fleet_size, " vehicles loaded. 0 delayed or blocked.")
    })
  })
}
