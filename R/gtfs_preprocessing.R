#' Get Active Service ID from GTFS Calendar
#'
#' @description Reads the GTFS calendar.txt file and finds the active service_id
#' for a specific date and weekday. Gracefully warns if the feed has expired or
#' the date is out of bounds.
#'
#' @param calendar_path A character string representing the file path to calendar.txt.
#' @param target_date A Date object specifying the date to check. Defaults to Sys.Date().
#' @return A character vector of active service_ids for the given date.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Check for today
#'   active_services <- get_active_service_id("data/gtfs/calendar.txt")
#'   
#'   # Check for a specific date
#'   active_services <- get_active_service_id("data/gtfs/calendar.txt", as.Date("2026-03-25"))
#' }
get_active_service_id <- function(calendar_path="data/gtfs/calendar.txt", target_date = Sys.Date()) {
  
  # Input Validation
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
  
  # Convert start_date and end_date from YYYYMMDD to Date objects
  calendar_data$start_date_parsed <- as.Date(as.character(calendar_data$start_date), format = "%Y%m%d")
  calendar_data$end_date_parsed   <- as.Date(as.character(calendar_data$end_date), format = "%Y%m%d")
  
  # Defensive Programming: Warn if the chosen date is outside the feed boundaries
  min_date <- min(calendar_data$start_date_parsed, na.rm = TRUE)
  max_date <- max(calendar_data$end_date_parsed, na.rm = TRUE)
  
  if (target_date < min_date || target_date > max_date) {
    stop(sprintf(
      "Target date (%s) is outside the boundaries of this GTFS feed (%s to %s). No active services will be found.", 
      target_date, min_date, max_date))
  }
  
  # Determine the weekday of the target date
  day_index <- as.POSIXlt(target_date)$wday  # 0 = Sunday, 1 = Monday, ..., 6 = Saturday
  days_map <- c("sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday")
  weekday_name <- days_map[day_index + 1]  # +1 because R vectors are 1-indexed
  
  # Filter the data to find the active service_id
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


#' Build Route Shapes from GTFS Data
#'
#' @description Reads shapes.txt, trips.txt, and routes.txt. Filters by active 
#' service for today, joins to attach route_short_name to each shape, builds 
#' sf LINESTRING per shape, projects to EPSG:2180, and saves as an .rds file.
#'
#' @param gtfs_dir A character string with the path to the directory containing GTFS files.
#' @param output_path A character string with the path where the .rds file should be saved.
#' @return An sf object containing the spatial lines.
#' @export
#'
#' @examples
#' \dontrun{
#'   routes_sf <- build_route_shapes("data/gtfs", "data/warsaw_routes.rds")
#' }
build_route_shapes <- function(gtfs_dir = "data/gtfs", output_path = "data/warsaw_routes.rds") {
  
  # Fetch active service IDs using the function we just created
  calendar_path <- file.path(gtfs_dir, "calendar.txt")
  active_services <- get_active_service_id(calendar_path)
  
  if (length(active_services) == 0) {
    stop("Error: No active services found. Cannot build route shapes.")
  }
  
  # Define file paths for the required text files
  shapes_path <- file.path(gtfs_dir, "shapes.txt")
  trips_path  <- file.path(gtfs_dir, "trips.txt")
  routes_path <- file.path(gtfs_dir, "routes.txt")
  
  if (!file.exists(shapes_path) || !file.exists(trips_path) || !file.exists(routes_path)) {
    stop("Error: Missing required GTFS files in the specified directory.")
  }
  
  # Read data using readr::read_csv for speed and memory efficiency
  routes <- readr::read_csv(routes_path, show_col_types = FALSE)
  trips  <- readr::read_csv(trips_path, show_col_types = FALSE)
  shapes <- readr::read_csv(shapes_path, show_col_types = FALSE)

  # Join and filter to get relevant shape IDs and their route names
  active_trips <- trips |>
    dplyr::filter(service_id %in% active_services) |>
    dplyr::left_join(routes, by = "route_id") |>
    dplyr::select(shape_id, route_short_name) |>
    dplyr::distinct()
  
  # Join active shapes and ensure the points are in the exact physical order
  active_shapes <- shapes |>
    dplyr::inner_join(active_trips, by = "shape_id") |>
    dplyr::arrange(shape_id, shape_pt_sequence)
  
  # Convert to sf object, combine dots into lines, and project to metric Polish grid
  routes_sf <- active_shapes |>
    sf::st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326) |>
    dplyr::group_by(shape_id, route_short_name) |>
    dplyr::summarise(geometry = sf::st_combine(geometry), .groups = "drop") |>
    sf::st_cast("LINESTRING") |>
    sf::st_transform(crs = 2180)
  
  # Save the final processed object to the specified output path
  saveRDS(routes_sf, file = output_path)
  
  return(routes_sf)
}
