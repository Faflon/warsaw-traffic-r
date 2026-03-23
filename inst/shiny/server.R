server <- function(input, output, session) {
  
  # --- Startup ---
  # Load pre-processed route shapes once when the app starts
  route_shapes <- tryCatch({
    path <- file.path(system.file(package = "WarsawTraffic"), "..", "data", "warsaw_routes.rds")
    readRDS(normalizePath(path))
  }, error = function(e) {
    message("Failed to load route shapes: ", e$message)
    NULL
  })
  
  # Create the TransitNetwork fleet manager
  network <- TransitNetwork$new()
  
  # Reactive value holding current vehicle sf data (NULL until first fetch)
  vehicle_data <- shiny::reactiveVal(NULL)
  
  # Reactive value holding current disruption pin coordinates
  disruption_pin <- shiny::reactiveVal(NULL)
  
  status_msg <- shiny::reactiveVal("No vehicles loaded. Click 'Fetch vehicles' to begin.")
  
  # --- Initial map ---
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet() |>
      leaflet::addTiles() |>
      leaflet::setView(lng = 21.01, lat = 52.23, zoom = 12)
  })
  
  # --- Status text (single definition, driven by status_msg reactive) ---
  output$status_text <- shiny::renderText({ status_msg() })
  
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
      status_msg("Error: WARSAW_API_KEY not found in .Renviron. Please add it and restart R.")
      return()
    }
    
    status_msg("Fetching vehicles...")
    
    result <- tryCatch({
      
      buses <- fetch_warsaw_transit(api_key, vehicle_type = 1)
      trams <- fetch_warsaw_transit(api_key, vehicle_type = 2)
      
      if (nrow(buses) > 0) network$update_fleet(buses, vehicle_type = 1)
      if (nrow(trams) > 0) network$update_fleet(trams, vehicle_type = 2)
      
      network$get_spatial_data()
      
    }, error = function(e) {
      status_msg(paste("Error fetching data:", conditionMessage(e)))
      return(NULL)
    })
    
    if (!is.null(result)) {
      vehicle_data(result)
      n_delayed <- sum(result$is_delayed, na.rm = TRUE)
      status_msg(paste0(
        network$fleet_size, " vehicles loaded. ",
        n_delayed, " delayed or blocked."
      ))
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
    
    status_msg(paste0(network$fleet_size, " vehicles loaded. 0 delayed or blocked."))
  })
  
  # --- Map click handler (disruption pin) ---
  shiny::observeEvent(input$map_click, {
    
    # Do nothing if no route shapes loaded
    if (is.null(route_shapes)) {
      status_msg("Error: Route shapes not loaded. Run build_route_shapes() first.")
      return()
    }
    
    click <- input$map_click
    
    # Create the spatial buffer around the clicked point
    buffer <- tryCatch(
      create_disruption_buffer(lon = click$lng, lat = click$lat, radius_m = input$radius_m),
      error = function(e) {
        status_msg(paste("Error creating buffer:", conditionMessage(e)))
        return(NULL)
      }
    )
    
    if (is.null(buffer)) return()
    
    # Find which lines pass through the disruption zone
    affected <- find_affected_lines(buffer, route_shapes)
    
    # Draw the pin AFTER affected is known so popup can reference it
    leaflet::leafletProxy("map") |>
      leaflet::clearGroup("disruption_pin") |>
      leaflet::addCircles(
        lng         = click$lng,
        lat         = click$lat,
        radius      = input$radius_m,
        color       = "#2c3e50",
        fill        = TRUE,
        fillColor   = "#2c3e50",
        fillOpacity = 0.15,
        weight      = 2,
        opacity     = 0.8,
        group       = "disruption_pin",
        popup       = paste0(
          "<b>Disruption</b><br>",
          "Type: ", input$disruption_type, "<br>",
          "Radius: ", input$radius_m, "m<br>",
          if (length(affected) > 0)
            paste("Affected lines:", paste(sort(affected), collapse = ", "))
          else
            "No routes affected"
        )
      )
    
    if (length(affected) == 0) {
      status_msg("No transit routes pass through this area. Try clicking on a road or track.")
      return()
    }
    
    # Reset previous disruptions before applying new ones
    network$reset_disruptions()
    
    # Apply disruption to the fleet
    network$apply_disruption(affected, input$disruption_type)
    
    # Update reactive vehicle data
    vehicle_data(network$get_spatial_data())
    
    # Store pin location
    disruption_pin(list(lng = click$lng, lat = click$lat))
    
    # Update status
    n_delayed <- if (!is.null(vehicle_data())) sum(vehicle_data()$is_delayed, na.rm = TRUE) else 0
    status_msg(paste0(
      network$fleet_size, " vehicles loaded. ",
      n_delayed, " delayed or blocked. ",
      "Lines affected: ", paste(sort(affected), collapse = ", ")
    ))
  })
}
