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
                                update_fleet = function(api_data, vechicle_type) {
                                  if (!is.data.frame(api_data) || nrow(api_data) == 0) return(invisible(self))
                                  if (!vechicle_type %in% c(1, 2)) stop("Type must be 1 (buses) or 2 (trams).")
                                  
                                  for (i in seq_len(nrow(api_data))) {
                                    row <- api_data[i, ]
                                    v_id <- as.character(row$VehicleNumber)
                                    line_num <- as.character(row$Lines)
                                    
                                    # Check if vehicle is already in the fleet
                                    if (v_id %in% names(private$.fleet)) {
                                      private$.fleet[[v_id]]$update_location(row$Lon, row$Lat, row$Time)
                                    } else {
                                      # Create the correct child class based on the type argument
                                      if (type == 2) {
                                        private$.fleet[[v_id]] <- Tram$new(v_id, line_num, row$Lon, row$Lat, row$Time)
                                      } else {
                                        private$.fleet[[v_id]] <- Bus$new(v_id, line_num, row$Lon, row$Lat, row$Time)
                                      }
                                    }
                                  }
                                  invisible(self)
                                },
                                
                              #' @description Send a disruption warning to all vehicles in the fleet
                              #' @param polygon An 'sf' polygon representing the blocked area
                              #' @param type A string: "traffic" or "track_blockage"
                              apply_disruption = function(polygon, type) {
                                  lapply(private$.fleet, function(vehicle) {
                                    if (inherits(vehicle, "Tram")) {
                                      vehicle$check_disruption(polygon, type)
                                    } else {
                                      vehicle$check_disruption(polygon) 
                                    }
                                  })
                                  invisible(self)
                                }
                              )
)