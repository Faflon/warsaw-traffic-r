#' R6 Class Representing the Transit Network Manager
#'
#' @description Manages a fleet of Bus and Tram objects and handles global spatial updates.
#' @export
TransitNetwork <- R6::R6Class("TransitNetwork",
                          private = list(
                              .fleet = list() #all Bus and Tram objects
                            ),
                              
                          active = list(
                                fleet_size = function(value) {
                                  if (!missing(value)) stop("Error: fleet_size is read-only.")
                                  return(length(private$.fleet))
                                }
                              ),
                              
                          public = list(
                                initialize = function() {
                                  private$.fleet <- list()
                                },
                                
                                #' @description Update the fleet with fresh API data
                                #' @param api_data A data frame returned by your API fetcher
                                #' @param vechicle_type An integer: 1 for buses, 2 for trams.
                                update_fleet = function(api_data, vehicle_type) {
                                  if (!is.data.frame(api_data) || nrow(api_data) == 0) return(invisible(self))
                                  if (!vehicle_type %in% c(1, 2)) stop("vehicle_type must be 1 (buses) or 2 (trams).")
                                  
                                  for (i in seq_len(nrow(api_data))) {
                                    row <- api_data[i, ]
                                    v_id <- as.character(row$VehicleNumber)
                                    line_num <- as.character(row$Lines)
                                    
                                    if (v_id %in% names(private$.fleet)) {
                                      private$.fleet[[v_id]]$update_location(row$Lon, row$Lat, row$Time)
                                    } else {
                                      if (vehicle_type == 2) {
                                        private$.fleet[[v_id]] <- Tram$new(v_id, line_num, row$Lon, row$Lat, row$Time)
                                      } else {
                                        private$.fleet[[v_id]] <- Bus$new(v_id, line_num, row$Lon, row$Lat, row$Time)
                                      }
                                    }
                                  }
                                  invisible(self)
                                },
                                
                                #' @description Apply a disruption to all vehicles on affected lines
                                #' @param affected_lines A character vector of route_short_name values
                                #' @param disruption_type A string: "traffic" or "track_blockage"
                                apply_disruption = function(affected_lines, disruption_type) {
                                  if (!is.character(affected_lines)) {
                                    stop("affected_lines must be a character vector.")
                                  }
                                  if (!disruption_type %in% c("traffic", "track_blockage")) {
                                    stop("disruption_type must be 'traffic' or 'track_blockage'.")
                                  }
                                  
                                  lapply(private$.fleet, function(vehicle) {
                                    if (inherits(vehicle, "Tram")) {
                                      vehicle$check_disruption(affected_lines, disruption_type)
                                    } else {
                                      vehicle$check_disruption(affected_lines)
                                    }
                                  })
                                  invisible(self)
                                },
                                
                                #' @description Reset all vehicles in the fleet to non-delayed status
                                reset_disruptions = function() {
                                  lapply(private$.fleet, function(vehicle) {
                                    vehicle$clear_delay()
                                  })
                                  invisible(self)
                                },
                                
                                #' @description Export the current fleet as an sf spatial object for map rendering
                                #' @return An sf object with columns: id, line, is_delayed, is_blocked, vehicle_type, geometry.
                                #'   Returns NULL if the fleet is empty.
                                get_spatial_data = function() {
                                  if (length(private$.fleet) == 0) return(NULL)
                                  
                                  df <- do.call(rbind, lapply(private$.fleet, function(v) {
                                    data.frame(
                                      id         = v$id,
                                      line       = v$line,
                                      lon        = v$lon,
                                      lat        = v$lat,
                                      is_delayed = v$is_delayed,
                                      is_blocked = if (inherits(v, "Tram")) v$is_blocked else FALSE,
                                      vehicle_type = if (inherits(v, "Tram")) "tram" else "bus",
                                      stringsAsFactors = FALSE
                                    )
                                  }))
                                  
                                  # Drop rows where coordinates are NA (vehicles with no valid GPS fix)
                                  df <- df[!is.na(df$lon) & !is.na(df$lat), ]
                                  
                                  if (nrow(df) == 0) return(NULL)
                                  
                                  sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
                                }
                              )
)
