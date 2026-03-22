
#' Fetch Live Public Transit Data from Warsaw API
#'
#' @param api_key A string containing your personal API key from api.um.warszawa.pl
#' @param vehicle_type An integer: 1 for buses, 2 for trams. Default is 1.
#' @return A data frame containing the live locations and details of the vehicles.
#' @export
fetch_warsaw_transit <- function(api_key, vehicle_type = 1) {
  
  # Input Validation
  if (missing(api_key) || !is.character(api_key) || nchar(api_key) == 0) {
    stop("Error: A valid API key must be provided as a string.")
  }
  
  if (!vehicle_type %in% c(1, 2)) {
    stop("Error: vehicle_type must be 1 (buses) or 2 (trams).")
  }
  
  # API Endpoint and required resource ID for buses/trams
  base_url <- "https://api.um.warszawa.pl/api/action/busestrams_get/"
  resource_id <- "f2e5503e927d-4ad3-9500-4ab9e55deb59"
  
  # Safe API Call using tryCatch
  # We use a timeout so the app doesn't freeze if the server is offline
response <- tryCatch({
    httr::GET(
      url = base_url,
      query = list(
        resource_id = resource_id,
        apikey = api_key,
        type = vehicle_type
      ),
      httr::timeout(10)
    )
  }, error = function(e) {
    message("Failed to connect to the Warsaw Transit API: ", e$message)
    return(NULL)
  })

  if (is.null(response)) return(data.frame())

  if (httr::http_error(response)) {
    warning("API returned an error. HTTP Status: ", httr::status_code(response))
    return(data.frame())
  }

  parsed_data <- tryCatch({
    content_text <- httr::content(response, as = "text", encoding = "UTF-8")
    jsonlite::fromJSON(content_text, flatten = TRUE)
  }, error = function(e) {
    warning("Failed to parse JSON response: ", e$message)
    return(data.frame())
  })
  
  # If the connection completely failed, return an empty data frame
  if (is.null(response)) return(data.frame())
  
  # Check HTTP Status
  if (httr::http_error(response)) {
    warning("API returned an error. HTTP Status: ", httr::status_code(response))
    return(data.frame())
  }
  
  # Parse the JSON
  parsed_data <- tryCatch({
    content_text <- httr::content(response, as = "text", encoding = "UTF-8")
    jsonlite::fromJSON(content_text, flatten = TRUE)
  }, error = function(e) {
    warning("Failed to parse JSON response: ", e$message)
    return(data.frame())
  })
  
  # Handle the specific structure of the Warsaw API
  # The Warsaw API returns errors as a string inside the 'result' field, rather than a standard HTTP error.
  if (!"result" %in% names(parsed_data)) {
    warning("Unexpected API response structure.")
    return(data.frame())
  }
  
  if (is.character(parsed_data$result)) {
    warning("API Error Message: ", parsed_data$result)
    return(data.frame())
  }
  
  df <- as.data.frame(parsed_data$result)
  
  # Convert lat/lon from characters to numeric for future spatial calculations
  if("Lat" %in% colnames(df) && "Lon" %in% colnames(df)) {
    df$Lat <- as.numeric(df$Lat)
    df$Lon <- as.numeric(df$Lon)
  }
  
  return(clean_stale_data(df))
}


