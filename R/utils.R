#' Clean stale vehicle data based on timestamp
#'
#' @param df A data frame returned by fetch_warsaw_transit()
#' @param max_age_mins Maximum allowed age of the data point in minutes. Default is 10.
#' @return A cleaned data frame with only recently updated vehicles.
#' @export
clean_data <- function(df, max_age_mins = 10) {
  if (nrow(df) == 0 || !"Time" %in% colnames(df)) return(df)
  
  # The API returns time as a string (e.g., "2026-03-10 16:45:00")
  # We convert it to a POSIXct datetime object
  df$Time <- as.POSIXct(df$Time, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Warsaw")
  
  # Get the current system time
  current_time <- Sys.time()
  
  # Calculate the time difference in minutes
  df$age_mins <- as.numeric(difftime(current_time, df$Time, units = "mins"))
  
  # Filter out rows older than max_age_mins and drop the temporary column
  cleaned_df <- df[df$age_mins <= max_age_mins & !is.na(df$age_mins), ]
  cleaned_df$age_mins <- NULL
  
  return(cleaned_df)
}
