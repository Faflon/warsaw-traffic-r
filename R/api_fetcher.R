#' @description Fetches live GPS coordinates and metadata for buses or trams in Warsaw using the official UM Warszawa API. Automatically cleans stale data and converts coordinates to numeric format for spatial processing.
#'
#' @param api_key A character string containing your personal API key from api.um.warszawa.pl.
#' @param vehicle_type An integer: 1 for buses, 2 for trams. Default is 1.
#' 
#' @return A data frame containing the live locations and details of the vehicles. Returns an empty data frame if the connection or parsing fails.
#' @export
#' @importFrom httr GET timeout content
#' @importFrom jsonlite fromJSON
#' 
fetch_warsaw_transit <- function(api_key, vehicle_type = 1) {
  
  if (missing(api_key) || !is.character(api_key) || nchar(api_key) == 0) {
    stop("Error: A valid API key must be provided as a string.")
  }
  
  if (!vehicle_type %in% c(1, 2)) {
    stop("Error: vehicle_type must be 1 (buses) or 2 (trams).")
  }
  
  base_url <- "https://api.um.warszawa.pl/api/action/busestrams_get/"
  resource_id <- "f2e5503e927d-4ad3-9500-4ab9e55deb59"
  
  response <- tryCatch({
    GET(
      url = base_url,
      query = list(
        resource_id = resource_id,
        apikey = api_key,
        type = vehicle_type
      ),
      timeout(10)
    )
  }, error = function(e) {
    message("Failed to connect to the Warsaw Transit API: ", e$message)
    return(NULL)
  })
  
  if (is.null(response)) {
    return(data.frame())
  }
  
  parsed_data <- tryCatch({
    raw_text <- content(response, as = "text", encoding = "UTF-8")
    fromJSON(raw_text, flatten = TRUE)
  }, error = function(e) {
    warning("Failed to parse API response: ", e$message)
    return(NULL)
  })
  
  if (is.null(parsed_data)) {
    return(data.frame())
  }
  
  # The Warsaw API returns errors as a string, rather than a standard HTTP error code
  if (!"result" %in% names(parsed_data)) {
    warning("Unexpected API response structure.")
    return(data.frame())
  }
  
  if (is.character(parsed_data$result)) {
    warning("API Error Message: ", parsed_data$result)
    return(data.frame())
  }
  
  df <- as.data.frame(parsed_data$result)
  
  if ("Lat" %in% colnames(df) && "Lon" %in% colnames(df)) {
    df$Lat <- as.numeric(df$Lat)
    df$Lon <- as.numeric(df$Lon)
  }
  
  return(clean_stale_data(df))
}