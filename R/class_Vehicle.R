#' R6 Parent Class Representing a Transit Vehicle
#'
#' @description A base class for buses and trams.
#' @export
#' 
Vehicle <- R6::R6Class("Vehicle",
                       private = list(
                         .id = NULL,
                         .line = NULL,
                         .lon = NULL,
                         .lat = NULL,
                         .last_update = NULL,
                         .is_delayed = FALSE
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
                         initialize = function(id, line, lon, lat, last_update) {
                           if (missing(id) || missing(line)) stop("Vehicle ID and Line are required.")
                           private$.id <- as.character(id)
                           private$.line <- as.character(line)
                           self$update_location(lon, lat, last_update)
                         },
                         
                         #' @description Update the vehicle's GPS coordinates and timestamp
                         update_location = function(new_lon, new_lat, new_time) {
                           private$.lon <- as.numeric(new_lon)
                           private$.lat <- as.numeric(new_lat)
                           private$.last_update <- as.POSIXct(new_time, format="%Y-%m-%d %H:%M:%S", tz="Europe/Warsaw")
                         },
                         
                         #' @description Placeholder for the disruption logic to be overridden by child classes
                         check_disruption = function(...) {
                           stop("This method should be implemented by the specific child classes.")
                         }
                       )
)



#' R6 Child Class Representing a Bus
#'
#' @description Inherits from Vehicle. Flags as delayed when its line
#'   is affected by a road traffic disruption.
#' @export
Bus <- R6::R6Class("Bus",
                   inherit = Vehicle,
                   public = list(
                     
                     #' @description Initialize a Bus object
                     initialize = function(id, line, lon, lat, last_update) {
                       super$initialize(id, line, lon, lat, last_update)
                     },
                     
                     #' @description Check if this bus is on an affected line and flag it as delayed
                     #' @param affected_lines A character vector of route_short_name values affected by the disruption
                     check_disruption = function(affected_lines) {
                       if (!is.character(affected_lines)) {
                         stop("affected_lines must be a character vector of line names.")
                       }
                       if (self$line %in% affected_lines) {
                         private$.is_delayed <- TRUE
                       }
                       invisible(self)
                     },
                     
                     #' @description Reset this bus to non-delayed status
                     clear_delay = function() {
                       private$.is_delayed <- FALSE
                       invisible(self)
                     }
                   )
)



#' R6 Child Class Representing a Tram
#'
#' @description Inherits from Vehicle. Immune to road traffic. Flags as
#'   blocked when its track is affected by a track blockage disruption.
#' @export
Tram <- R6::R6Class("Tram",
                    inherit = Vehicle,
                    private = list(
                      .is_blocked = FALSE
                    ),
                    active = list(
                      #' @field is_blocked Read-only. TRUE if tram is blocked by a track disruption.
                      is_blocked = function(value) {
                        if (missing(value)) return(private$.is_blocked)
                        stop("Error: 'is_blocked' is read-only.")
                      }
                    ),
                    public = list(
                      
                      #' @description Initialize a Tram object
                      initialize = function(id, line, lon, lat, last_update) {
                        super$initialize(id, line, lon, lat, last_update)
                      },
                      
                      #' @description Check if this tram is affected by a disruption
                      #' @param affected_lines A character vector of route_short_name values affected
                      #' @param disruption_type A string: "traffic" or "track_blockage"
                      check_disruption = function(affected_lines, disruption_type) {
                        if (!is.character(affected_lines)) {
                          stop("affected_lines must be a character vector of line names.")
                        }
                        if (!disruption_type %in% c("traffic", "track_blockage")) {
                          stop("disruption_type must be 'traffic' or 'track_blockage'.")
                        }
                        
                        # Trams run on dedicated rails — road traffic does not affect them
                        if (disruption_type == "traffic") {
                          return(invisible(self))
                        }
                        
                        # Track blockage: flag as both delayed and blocked if line is affected
                        if (self$line %in% affected_lines) {
                          private$.is_delayed <- TRUE
                          private$.is_blocked <- TRUE
                        }
                        invisible(self)
                      },
                      
                      #' @description Reset this tram to non-delayed, non-blocked status
                      clear_delay = function() {
                        private$.is_delayed <- FALSE
                        private$.is_blocked <- FALSE
                        invisible(self)
                      }
                    )
)