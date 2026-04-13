#' Base R6 class representing a transit vehicle
#'
#' @description A base class for buses and trams, storing common properties like ID, route line, spatial coordinates, and delay status.
#' @export
#' 
Vehicle <- R6::R6Class("Vehicle",
                       private = list(
                         .id = NULL,
                         .line = NULL,
                         .lon = NULL,
                         .lat = NULL,
                         .last_update = NULL,
                         .is_delayed = FALSE # FALSE by default, set to TRUE when a disruption is applied
                       ),
                       active = list(
                         id = function(value) {
                           if (missing(value)) return(private$.id)
                           stop("Error: 'id' is read-only.")
                         },
                         line = function(value) {
                           if (missing(value)) return(private$.line)
                           stop("Error: 'line' is read-only.")
                         },
                         lon = function(value) {
                           if (missing(value)) return(private$.lon) 
                           stop("Error: 'lon' is read-only.")
                         },
                         lat = function(value) {
                           if (missing(value)) return(private$.lat) 
                           stop("Error: 'lat' is read-only.")
                         },
                         last_update = function(value) {
                           if (missing(value)) return(private$.last_update) 
                           stop("Error: 'last_update' is read-only.")
                         },
                         is_delayed = function(value) {
                           if (missing(value)) return(private$.is_delayed)
                           stop("Error: 'is_delayed' is read-only. Status must be updated via internal methods.")
                         }
                       ),
                       public = list(
                         #' @description Initialize the vehicle
                         #' @param id A character string representing the vehicle ID.
                         #' @param line A character string representing the route line number.
                         #' @param lon A numeric value for the initial longitude.
                         #' @param lat A numeric value for the initial latitude.
                         #' @param last_update A string representing the timestamp of the data.
                         #' @return A new `Vehicle` object.
                         initialize = function(id, line, lon, lat, last_update) {
                           if (missing(id) || missing(line)) stop("Vehicle ID and line are required.")
                           private$.id <- as.character(id)
                           private$.line <- as.character(line)
                           self$update_location(lon, lat, last_update)
                         },
                         
                         #' @description Update the vehicle's GPS coordinates and timestamp
                         #' @param new_lon A numeric longitude.
                         #' @param new_lat A numeric latitude.
                         #' @param new_time A string representing the update time.
                         #' @return Invisibly returns the `Vehicle` object for chaining.
                         update_location = function(new_lon, new_lat, new_time) {
                           private$.lon <- as.numeric(new_lon)
                           private$.lat <- as.numeric(new_lat)
                           private$.last_update <- as.POSIXct(new_time, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Warsaw")
                           invisible(self)
                         },
                         
                         #' @description Placeholder for the disruption logic to be overridden by child classes
                         #' @param ... Arguments to be passed to specific child class implementations.
                         check_disruption = function(...) {
                           stop("This method should be implemented by the specific child classes.")
                         }
                       )
)


#' R6 Child Class Representing a Bus
#'
#' @description Inherits from Vehicle. Flags as delayed when its line is affected by a road traffic disruption.
#' @export
Bus <- R6::R6Class("Bus",
                   inherit = Vehicle,
                   public = list(
                     
                     #' @description Initialize a Bus object
                     #' @param id A character string representing the vehicle ID.
                     #' @param line A character string representing the route line number.
                     #' @param lon A numeric value for the initial longitude.
                     #' @param lat A numeric value for the initial latitude.
                     #' @param last_update A string representing the timestamp of the data.
                     #' @return A new `Bus` object.
                     initialize = function(id, line, lon, lat, last_update) {
                       super$initialize(id, line, lon, lat, last_update)
                     },
                     
                     #' @description Check if this bus is on an affected line and flag it as delayed
                     #' @param affected_lines A character vector of route_short_name values affected by the disruption
                     #' @param disruption_type A string: "traffic", "track_blockage", or "both"
                     #' @return Invisibly returns the `Bus` object for chaining.
                     check_disruption = function(affected_lines, disruption_type) {
                       if (!is.character(affected_lines)) {
                         stop("affected_lines must be a character vector of line names.")
                       }
                       if (!disruption_type %in% c("traffic", "track_blockage", "both")) {
                         stop("disruption_type must be 'traffic', 'track_blockage', or 'both'.")
                       }
                       
                       # buses run on roads so track blockages don't affect them
                       if (disruption_type == "track_blockage") {
                         return(invisible(self))
                       }
                       
                       if (self$line %in% affected_lines) {
                         private$.is_delayed <- TRUE
                       }
                       invisible(self)
                     },
                     
                     #' @description Reset this bus to non-delayed status
                     #' @return Invisibly returns the `Bus` object for chaining.
                     clear_delay = function() {
                       private$.is_delayed <- FALSE
                       invisible(self)
                     }
                   )
)


#' R6 Child Class Representing a Tram
#'
#' @description Inherits from Vehicle. Immune to road traffic. Flags as blocked when its track is affected by a track blockage disruption.
#' @export
Tram <- R6::R6Class("Tram",
                    inherit = Vehicle,
                    private = list(
                      # in trams being blocked is different from just being delayed
                      .is_blocked = FALSE
                    ),
                    active = list(
                      is_blocked = function(value) {
                        if (missing(value)) return(private$.is_blocked)
                        stop("Error: 'is_blocked' is read-only.")
                      }
                    ),
                    public = list(
                      
                      #' @description Initialize a Tram object
                      #' @param id A character string representing the vehicle ID.
                      #' @param line A character string representing the route line number.
                      #' @param lon A numeric value for the initial longitude.
                      #' @param lat A numeric value for the initial latitude.
                      #' @param last_update A string representing the timestamp of the data.
                      #' @return A new `Tram` object.
                      initialize = function(id, line, lon, lat, last_update) {
                        super$initialize(id, line, lon, lat, last_update)
                      },
                      
                      #' @description Check if this tram is affected by a disruption
                      #' @param affected_lines A character vector of route_short_name values affected
                      #' @param disruption_type A string: "traffic", "track_blockage", or "both"
                      #' @return Invisibly returns the `Tram` object for chaining.
                      check_disruption = function(affected_lines, disruption_type) {
                        if (!is.character(affected_lines)) {
                          stop("affected_lines must be a character vector of line names.")
                        }
                        if (!disruption_type %in% c("traffic", "track_blockage", "both")) {
                          stop("disruption_type must be 'traffic', 'track_blockage', or 'both'.")
                        }
                        
                        # trams run on dedicated rails so road traffic doesn't affect them
                        if (disruption_type == "traffic") {
                          return(invisible(self))
                        }
                        
                        if (self$line %in% affected_lines) {
                          private$.is_delayed <- TRUE
                          private$.is_blocked <- TRUE
                        }
                        invisible(self)
                      },
                      
                      #' @description Reset this tram to non-delayed, non-blocked status
                      #' @return Invisibly returns the `Tram` object for chaining.
                      clear_delay = function() {
                        private$.is_delayed <- FALSE
                        private$.is_blocked <- FALSE
                        invisible(self)
                      }
                    )
)