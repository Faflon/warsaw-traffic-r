#' Clean stale vehicle data based on timestamp
#'
#' @description Filters out vehicle locations older than a specified number of minutes
#'   by comparing the `Time` column in the API data to the current system time.
#'   This ensures the map only displays active vehicles.
#'
#' @param df A data frame returned by fetch_warsaw_transit(). Must contain a "Time" column.
#' @param max_age_mins Maximum allowed age of the data point in minutes. Default is 10.
#' @return A cleaned data frame with only recently updated vehicles. Returns the original 
#'   data frame if it is empty or missing the "Time" column.
#' @export
#' 
clean_stale_data <- function(df, max_age_mins = 10) {
  if (nrow(df) == 0 || !"Time" %in% colnames(df)) return(df)
  
  df$Time <- as.POSIXct(df$Time, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Warsaw")
  
  current_time <- Sys.time()
  
  df$age_mins <- as.numeric(difftime(current_time, df$Time, units = "mins"))
  
  cleaned_df <- df[df$age_mins <= max_age_mins & !is.na(df$age_mins), ]
  cleaned_df$age_mins <- NULL
  
  return(cleaned_df)
}

#' Create a spatial buffer for a disruption
#'
#' @description Takes a raw GPS coordinate (longitude and latitude), converts it 
#' into a spatial point, projects it to the metric Polish coordinate system (EPSG:2180), 
#' and draws a circular buffer around it.
#'
#' @param lon A numeric value representing longitude (must be between -180 and 180).
#' @param lat A numeric value representing latitude (must be between -90 and 90).
#' @param radius_m A positive numeric value representing the buffer radius in meters. Default is 25.
#' @return An `sfc` polygon object in EPSG:2180 projection representing the disruption zone.
#' @import sf
#' @export
#' 
create_disruption_buffer <- function(lon, lat, radius_m = 25) {
  
  if (!is.numeric(lon) || lon < -180 || lon > 180) {
    stop("Error: 'lon' must be a valid numeric longitude between -180 and 180.")
  }
  
  if (!is.numeric(lat) || lat < -90 || lat > 90) {
    stop("Error: 'lat' must be a valid numeric latitude between -90 and 90.")
  }
  
  if (!is.numeric(radius_m) || radius_m <= 0) {
    stop("Error: 'radius_m' must be a positive number.")
  }
  
  if (lon < 14.1 || lon > 24.1 || lat < 49.0 || lat > 54.8) {
    warning("Warning: The provided coordinates appear to be outside of Poland. Projection EPSG:2180 may yield distorted results.")
  }

  # projecting to metres first so the buffer distance is accurate, degrees won't work
  buffer_polygon <- st_point(c(lon, lat)) |>
    st_sfc(crs = 4326) |>
    st_transform(crs = 2180) |>
    st_buffer(dist = radius_m)
  
  return(buffer_polygon)
}

#' Find transit lines affected by a disruption buffer
#'
#' @description Takes a spatial polygon representing a disruption zone and checks
#' it against a spatial dataset of transit routes. Identifies which specific 
#' routes cross into the buffer.
#'
#' @param buffer_polygon An `sf` or `sfc` polygon object representing the danger zone.
#' @param route_shapes An `sf` object containing the transit polylines, which must include a `route_short_name` column.
#' @return A character vector of unique `route_short_name`s affected by the disruption. Returns `character(0)` if none.
#' @import sf
#' @export
#' 
find_affected_lines <- function(buffer_polygon, route_shapes) {
  
  if (!inherits(buffer_polygon, c("sf", "sfc"))) {
    stop("Error: 'buffer_polygon' must be an sf or sfc spatial object.")
  }
  
  if (!inherits(route_shapes, "sf")) {
    stop("Error: 'route_shapes' must be an sf spatial object.")
  }
  
  if (!"route_short_name" %in% colnames(route_shapes)) {
    stop("Error: 'route_shapes' must contain a 'route_short_name' column.")
  }
  
  # sparse = FALSE forces a dense matrix instead of a list of indices which is simpler to subset
  intersection_matrix <- st_intersects(route_shapes, buffer_polygon, sparse = FALSE)
  
  # rows are routes, the single column is our buffer, which() pulls out the TRUE affected indices
  affected_indices <- which(intersection_matrix[, 1])
  
  if (length(affected_indices) == 0) {
    return(character(0))
  }
  
  # extracting unique route names
  affected_routes <- unique(as.character(route_shapes$route_short_name[affected_indices]))
  
  return(affected_routes)
}