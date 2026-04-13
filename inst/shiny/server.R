server <- function(input, output, session) {
  
  # loading route shapes only once when the app starts, not on every click
  route_shapes <- tryCatch({
    path <- file.path(system.file(package = "WarsawTraffic"), "..", "data", "warsaw_routes.rds")
    readRDS(normalizePath(path))
  }, error = function(e) {
    message("Could not load route shapes: ", e$message)
    NULL
  })
  
  network <- TransitNetwork$new()
  vehicle_data <- shiny::reactiveVal(NULL)
  status_msg <- shiny::reactiveVal("Ready. Fetch vehicles to begin.")
  
  # base map rendered once
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet() |>
      leaflet::addTiles() |>
      leaflet::setView(lng = 21.01, lat = 52.23, zoom = 12)
  })
  
  output$status_text <- shiny::renderText({ status_msg() })
  
  # fetch button pulls live bus and tram positions from the API
  shiny::observeEvent(input$fetch_btn, {
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
  
  # redraw markers when vehicle data changes after fetch and after each disruption click
  shiny::observe({
    shiny::req(vehicle_data())
    vd <- vehicle_data()
    
    # sf objects store coordinates in geometry, we pull them out into plain columns
    coords <- sf::st_coordinates(vd)
    vd$lng <- coords[, 1]
    vd$lat_coord <- coords[, 2]
    
    # color depends on vehicle type and current status
    vd$color <- dplyr::case_when(
      vd$vehicle_type == "tram" & vd$is_blocked ~ "red",
      vd$vehicle_type == "bus"  & vd$is_delayed ~ "orange",
      TRUE ~ "#2ecc71"
    )
    
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
    
    buses <- vd[vd$vehicle_type == "bus", ]
    trams <- vd[vd$vehicle_type == "tram", ]
    
    proxy <- leaflet::leafletProxy("map") |> leaflet::clearGroup("vehicles")
    
    # buses drawn as solid filled circles
    if (nrow(buses) > 0) {
      proxy <- leaflet::addCircleMarkers(proxy,
                                         lng = buses$lng, lat = buses$lat_coord,
                                         color = buses$color, fillColor = buses$color,
                                         fillOpacity = 0.9, radius = 6, stroke = FALSE,
                                         popup = buses$popup, group = "vehicles"
      )
    }
    
    # trams drawn as outlined rings
    if (nrow(trams) > 0) {
      proxy <- leaflet::addCircleMarkers(proxy,
                                         lng = trams$lng, lat = trams$lat_coord,
                                         color = trams$color, fillColor = trams$color,
                                         fillOpacity = 0.15, radius = 7, stroke = TRUE, weight = 2.5,
                                         popup = trams$popup, group = "vehicles"
      )
    }
  })
  
  # clear button resets all vehicle states and removes the disruption pin
  shiny::observeEvent(input$clear_btn, {
    network$reset_disruptions()
    vehicle_data(network$get_spatial_data())
    leaflet::leafletProxy("map") |> leaflet::clearGroup("disruption_pin")
    status_msg(paste0(network$fleet_size, " vehicles loaded. No disruptions active."))
  })
  
  # map click places a disruption pin and figures out which lines are affected
  shiny::observeEvent(input$map_click, {
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
    
    # find all lines crossing the buffer, filtering by disruption type happens inside the R6 classes
    geom_lines <- find_affected_lines(buffer, route_shapes)
    
    if (length(geom_lines) == 0) {
      leaflet::leafletProxy("map") |>
        leaflet::clearGroup("disruption_pin") |>
        leaflet::addCircles(
          lng = click$lng, lat = click$lat,
          radius = input$radius_m,
          color = "#2c3e50", fillColor = "#2c3e50",
          fillOpacity = 0.15, weight = 2, opacity = 0.8,
          group = "disruption_pin",
          popup = "No routes in this area."
        )
      status_msg("No routes pass through here. Try clicking on a road or tram track.")
      return()
    }
    
    # reset so that delays from the previous click don't carry over
    network$reset_disruptions()
    network$apply_disruption(geom_lines, input$disruption_type)
    vehicle_data(network$get_spatial_data())
    
    # get lines that are actually disrupted, not just geometrically in the buffer, differs for trams and buses
    vd <- vehicle_data()
    actually_affected <- sort(unique(vd$line[vd$is_delayed | vd$is_blocked]))
    
    leaflet::leafletProxy("map") |>
      leaflet::clearGroup("disruption_pin") |>
      leaflet::addCircles(
        lng = click$lng, lat = click$lat,
        radius = input$radius_m,
        color = "#2c3e50", fillColor = "#2c3e50",
        fillOpacity = 0.15, weight = 2, opacity = 0.8,
        group = "disruption_pin",
        popup = paste0(
          "<b>Disruption pin</b><br>",
          "Type: ", input$disruption_type, "<br>",
          "Radius: ", input$radius_m, " m<br>",
          if (length(actually_affected) > 0)
            paste("Affected lines:", paste(actually_affected, collapse = ", "))
          else
            "No vehicles affected."
        )
      )
    
    n_delayed <- sum(vd$is_delayed, na.rm = TRUE)
    status_msg(paste0(
      network$fleet_size, " vehicles loaded. ",
      n_delayed, " delayed/blocked. Lines: ",
      if (length(actually_affected) > 0) paste(actually_affected, collapse = ", ") else "none"
    ))
  })
}