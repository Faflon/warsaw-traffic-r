#' @description Reads the GTFS calendar.txt file and finds the active service_id
#' for a specific date and weekday. Gracefully warns if the feed has expired or
#' the date is out of bounds.
#'
#' @param calendar_path A character string representing the file path to calendar.txt.
#' @param target_date A Date object specifying the date to check. Defaults to Sys.Date().
#' @return A character vector of active service_ids for the given date.
#' @importFrom utils read.csv
#' @export
#'
#' @examples
get_active_service_id_old <- function(calendar_path="inst/extdata/gtfs/calendar.txt", target_date = Sys.Date()) {
  
  if (!file.exists(calendar_path)) {
    stop("Error: calendar.txt not found at the specified path.")
  }
  
  if (!inherits(target_date, "Date")) {
    target_date <- tryCatch({
      as.Date(target_date)
    }, error = function(e) {
      stop("Error: target_date must be a valid Date object or a string coercible to a Date.")
    })
  }
  
  calendar_data <- tryCatch({
    read.csv(calendar_path)
  }, error = function(e) {
    stop("Error reading calendar.txt: ", e$message)
  })
  
  calendar_data$start_date_parsed <- as.Date(as.character(calendar_data$start_date), format = "%Y%m%d")
  calendar_data$end_date_parsed   <- as.Date(as.character(calendar_data$end_date), format = "%Y%m%d")
  
  min_date <- min(calendar_data$start_date_parsed, na.rm = TRUE)
  max_date <- max(calendar_data$end_date_parsed, na.rm = TRUE)
  
  if (target_date < min_date || target_date > max_date) {
    stop(sprintf(
      "Target date (%s) is outside the boundaries of this GTFS feed (%s to %s). No active services will be found.", 
      target_date, min_date, max_date))
  }
  
  # Determine the weekday of the target date
  day_index <- as.POSIXlt(target_date)$wday  # 0 = Sunday, 1 = Monday, ...
  days_map <- c("sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday")
  weekday_name <- days_map[day_index + 1]  # +1 because R vectors are 1-indexed
  
  # Filtering the data to find the active service_id
  active_rows <- calendar_data[
      calendar_data$start_date_parsed <= target_date &
      calendar_data$end_date_parsed >= target_date &
      calendar_data[[weekday_name]] == 1, 
  ]
  
  if (nrow(active_rows) == 0) {
    warning("No active service found for the specified date.")
    return(character(0))
  }
  
  return(active_rows$service_id)
}


#' @description Reads shapes.txt, trips.txt, and routes.txt. Filters by active 
#' service for today, joins to attach route_short_name to each shape, builds 
#' sf LINESTRING per shape, projects to EPSG:2180, and saves as an .rds file.
#'
#' @param gtfs_dir A character string with the path to the directory containing GTFS files.
#' @param output_path A character string with the path where the .rds file should be saved.
#' @return An sf object containing the spatial lines.
#' @import dplyr
#' @import readr
#' @import sf
#' @export
#'
build_route_shapes_old <- function(gtfs_dir = "inst/extdata/gtfs", output_path = "inst/extdata/warsaw_routes.rds") {
  
  calendar_path <- file.path(gtfs_dir, "calendar.txt")
  active_services <- get_active_service_id_old(calendar_path)
  
  if (length(active_services) == 0) {
    stop("Error: No active services found. Cannot build route shapes.")
  }
  
  shapes_path <- file.path(gtfs_dir, "shapes.txt")
  trips_path  <- file.path(gtfs_dir, "trips.txt")
  routes_path <- file.path(gtfs_dir, "routes.txt")
  
  if (!file.exists(shapes_path) || !file.exists(trips_path) || !file.exists(routes_path)) {
    stop("Error: Missing required GTFS files in the specified directory.")
  }
  
  routes <- read_csv(routes_path, show_col_types = FALSE)
  trips  <- read_csv(trips_path, show_col_types = FALSE)
  shapes <- read_csv(shapes_path, show_col_types = FALSE)

  active_trips <- trips |>
    filter(service_id %in% active_services) |>
    left_join(routes, by = "route_id") |>
    select(shape_id, route_short_name) |>
    distinct()
  
  # joining active shapes and ensuring the points are in the exact physical order
  active_shapes <- shapes |>
    inner_join(active_trips, by = "shape_id") |>
    arrange(shape_id, shape_pt_sequence)
  
  # converting to sf object, combining dots into lines, and project to metric Polish grid
  routes_sf <- active_shapes |>
    st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326) |>
    group_by(shape_id, route_short_name) |>
    summarise(geometry = st_combine(geometry), .groups = "drop") |>
    st_cast("LINESTRING") |>
    st_transform(crs = 2180)
  
  saveRDS(routes_sf, file = output_path)
  
  return(routes_sf)
}
