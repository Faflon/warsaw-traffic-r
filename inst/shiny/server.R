server <- function(input, output, session) {
  
  # loading route shapes only once when the app starts, not on every click
  route_shapes <- tryCatch({
    path <- system.file("extdata", "warsaw_routes.rds", package = "WarsawTraffic")
    readRDS(path)
  }, error = function(e) {
    message("Could not load route shapes: ", e$message)
    NULL
  })
  
  network <- TransitNetwork$new()
  vehicle_data <- reactiveVal(NULL)
  status_msg <- reactiveVal("Ready. Fetch vehicles to begin.")
  
  # base map rendered once, all updates go through leafletProxy later
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(lng = 21.01, lat = 52.23, zoom = 12)
  })
  
  output$status_text <- renderText({ status_msg() })
  
  # function to assign color based on vehicle type and delay status
  get_color <- function(vehicle_type, is_blocked, is_delayed) {
    if (vehicle_type == "tram" && is_blocked) return("red")
    if (vehicle_type == "bus" && is_delayed) return("orange")
    return("#2ecc71")
  }
  
  # function to build popup text for a vehicle
  get_popup <- function(line, id, vehicle_type, is_blocked, is_delayed) {
    status <- if (vehicle_type == "tram" && is_blocked) "Blocked" else
      if (vehicle_type == "bus" && is_delayed) "Delayed" else "On time"
    paste0(
      "<b>Line:</b> ", line, "<br>",
      "<b>ID:</b> ", id, "<br>",
      "<b>Type:</b> ", vehicle_type, "<br>",
      "<b>Status:</b> ", status
    )
  }
  
  # fetch button
  observeEvent(input$fetch_btn, {
    api_key <- Sys.getenv("WARSAW_API_KEY")
    if (nchar(api_key) == 0) {
      status_msg("Error: WARSAW_API_KEY not set. Add it to .Renviron and restart R.")
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
      status_msg(paste("Fetch error:", conditionMessage(e)))
      NULL
    })
    
    if (!is.null(result)) {
      vehicle_data(result)
      status_msg(paste0(network$fleet_size, " vehicles loaded. No disruptions active."))
    }
  })
  
  # redraw markers when vehicle data changes
  observe({
    req(vehicle_data())
    vd <- vehicle_data()
    
    # coordinates in geometry, we pull them out into plain columns
    coords <- st_coordinates(vd)
    vd$lng <- coords[, 1]
    vd$lat_coord <- coords[, 2]
    
    vd$color <- mapply(get_color, vd$vehicle_type, vd$is_blocked, vd$is_delayed)
    vd$popup <- mapply(get_popup, vd$line, vd$id, vd$vehicle_type, vd$is_blocked, vd$is_delayed)
    
    buses <- vd[vd$vehicle_type == "bus", ]
    trams <- vd[vd$vehicle_type == "tram", ]
    
    proxy <- leafletProxy("map") |> clearGroup("vehicles")
    
    # buses drawn as solid filled circles
    if (nrow(buses) > 0) {
      proxy <- addCircleMarkers(proxy,
                                lng = buses$lng, lat = buses$lat_coord,
                                color = buses$color, fillColor = buses$color,
                                fillOpacity = 0.9, radius = 6, stroke = FALSE,
                                popup = buses$popup, group = "vehicles"
      )
    }
    
    # trams drawn as outlined rings
    if (nrow(trams) > 0) {
      proxy <- addCircleMarkers(proxy,
                                lng = trams$lng, lat = trams$lat_coord,
                                color = trams$color, fillColor = trams$color,
                                fillOpacity = 0.15, radius = 7, stroke = TRUE, weight = 2.5,
                                popup = trams$popup, group = "vehicles"
      )
    }
  })
  
  # clear button
  observeEvent(input$clear_btn, {
    network$reset_disruptions()
    vehicle_data(network$get_spatial_data())
    leafletProxy("map") |> clearGroup("disruption_pin")
    status_msg(paste0(network$fleet_size, " vehicles loaded. No disruptions active."))
  })
  
  # map click to place a disruption
  observeEvent(input$map_click, {
    if (is.null(route_shapes)) {
      status_msg("Route shapes not loaded. Run build_route_shapes() first.")
      return()
    }
    
    click <- input$map_click
    
    buffer <- tryCatch(
      create_disruption_buffer(click$lng, click$lat, input$radius_m),
      error = function(e) {
        status_msg(paste("Buffer error:", conditionMessage(e)))
        NULL
      }
    )
    if (is.null(buffer)) return()
    
    # finding all lines crossing the buffer
    geom_lines <- find_affected_lines(buffer, route_shapes)
    
    if (length(geom_lines) == 0) {
      leafletProxy("map") |>
        clearGroup("disruption_pin") |>
        addCircles(
          lng = click$lng, lat = click$lat, radius = input$radius_m,
          color = "#2c3e50", fillColor = "#2c3e50",
          fillOpacity = 0.15, weight = 2, opacity = 0.8,
          group = "disruption_pin", popup = "No routes in this area."
        )
      status_msg("No routes pass through here. Try clicking on a road or tram track.")
      return()
    }
    
    # reseting
    network$reset_disruptions()
    network$apply_disruption(geom_lines, input$disruption_type)
    vehicle_data(network$get_spatial_data())
    
    # getting lines that are actually disrupted, not just geometrically in the buffer
    vd <- vehicle_data()
    actually_affected <- sort(unique(vd$line[vd$is_delayed | vd$is_blocked]))
    affected_text <- if (length(actually_affected) > 0) paste(actually_affected, collapse = ", ") else "none"
    
    leafletProxy("map") |>
      clearGroup("disruption_pin") |>
      addCircles(
        lng = click$lng, lat = click$lat, radius = input$radius_m,
        color = "#2c3e50", fillColor = "#2c3e50",
        fillOpacity = 0.15, weight = 2, opacity = 0.8,
        group = "disruption_pin",
        popup = paste0(
          "<b>Disruption pin</b><br>",
          "Type: ", input$disruption_type, "<br>",
          "Radius: ", input$radius_m, " m<br>",
          "Affected lines: ", affected_text
        )
      )
    
    n_delayed <- sum(vd$is_delayed, na.rm = TRUE)
    status_msg(paste0(
      network$fleet_size, " vehicles loaded. ",
      n_delayed, " delayed/blocked. Lines: ", affected_text
    ))
  })
  
  # debug block: drawing specific route geometry on the map
  observe({
    req(route_shapes)
    
    line_to_show <- input$debug_line
    proxy <- leafletProxy("map") |> clearGroup("debug_route")
    
    # if input is not empty, search for the route and plot it
    if (!is.null(line_to_show) && nchar(trimws(line_to_show)) > 0) {
      target_shape <- route_shapes[route_shapes$route_short_name == trimws(line_to_show), ]
      
      if (nrow(target_shape) > 0) {
        
        # Transform the metric shape back to WGS84 (GPS degrees) just for Leaflet
        target_shape_wgs <- sf::st_transform(target_shape, crs = 4326)
        
        # plotting the geometry in red
        proxy |> addPolylines(
          data = target_shape_wgs,
          color = "red",
          weight = 5,
          opacity = 0.8,
          group = "debug_route",
          popup = paste("GTFS Shape for line:", target_shape_wgs$route_short_name)
        )
      } else {
        # if the shape doesn't exist in the data, notify via status message
        status_msg(paste("Debug: Shape for line", line_to_show, "not found in GTFS data."))
      }
    }
  })
}