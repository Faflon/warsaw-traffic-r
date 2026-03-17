library(R6)

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
                           stop("Error: 'id' is read-only and cannot be modified.")
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