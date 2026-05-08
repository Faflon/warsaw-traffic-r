#' Build Route Shapes from GTFS Data
#'
#' @description Reads shapes.txt, trips.txt, and routes.txt. Extracts the 
#' most frequently used shape for each route (based on trip count) to avoid 
#' depot runs and temporary detours. Builds sf LINESTRINGs, projects to 
#' EPSG:2180, and saves as an .rds file.
#'
#' @param gtfs_dir A character string with the path to the directory containing GTFS files.
#' @param output_path A character string with the path where the .rds file should be saved.
#' @return An sf object containing the spatial lines.
#' @import dplyr
#' @import readr
#' @import sf
#' @export
build_route_shapes <- function(gtfs_dir = "inst/extdata/gtfs", output_path = "inst/extdata/warsaw_routes.rds") {
  
  shapes_path <- file.path(gtfs_dir, "shapes.txt")
  trips_path  <- file.path(gtfs_dir, "trips.txt")
  routes_path <- file.path(gtfs_dir, "routes.txt")
  
  if (!file.exists(shapes_path) || !file.exists(trips_path) || !file.exists(routes_path)) {
    stop("Error: Missing required GTFS files in the specified directory.")
  }
  
  routes <- read_csv(routes_path, show_col_types = FALSE)
  trips  <- read_csv(trips_path, show_col_types = FALSE)
  shapes <- read_csv(shapes_path, show_col_types = FALSE)
  
  # 1. Count how many trips use each shape_id
  # Regular passenger routes will have dozens or hundreds of trips.
  # Depot runs will only have a few trips per day.
  # shape_frequencies <- trips |>
  #   left_join(routes, by = "route_id") |>
  #   group_by(route_short_name, shape_id) |>
  #   summarise(trip_count = n(), .groups = "drop")
  # 
  # # 2. Select the single most frequent shape_id for each route
  # representative_shapes <- shape_frequencies |>
  #   group_by(route_short_name) |>
  #   slice_max(order_by = trip_count, n = 1, with_ties = FALSE) |>
  #   select(shape_id, route_short_name)
  
  shape_frequencies <- trips |>
    left_join(routes, by = "route_id") |>
    group_by(route_short_name, direction_id, shape_id) |>
    summarise(trip_count = n(), .groups = "drop")
  
  representative_shapes <- shape_frequencies |>
    group_by(route_short_name, direction_id) |>
    slice_max(order_by = trip_count, n = 1, with_ties = FALSE) |>
    select(shape_id, route_short_name, direction_id)
  
  # 3. Filter the raw points to only keep the representative shapes
  clean_shapes <- shapes |>
    inner_join(representative_shapes, by = "shape_id") |>
    arrange(shape_id, shape_pt_sequence)
  
  # 4. Convert to sf LINESTRING objects
  routes_sf <- clean_shapes |>
    st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326) |>
    group_by(shape_id, route_short_name) |>
    summarise(geometry = st_combine(geometry), .groups = "drop") |>
    st_cast("LINESTRING") |>
    st_transform(crs = 2180)
  
  saveRDS(routes_sf, file = output_path)
  
  return(routes_sf)
}