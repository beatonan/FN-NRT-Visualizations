#' shield_scraper
#' 
#' @author Ryan Whaley
#' @description Retrieves AFFES forecast data from OPS internal
#' weatherSHIELD web service. 
#' @param latitude Latitude of location you'd like forecasted values for.
#' (Character or number is fine) 
#' @param longitude Longitude of location you'd like forecasted values for.
#' (Character of number is fine) 
#' @return Data frame containing five days of forecast values for location 
#' specified with latitude and longitude. 

shield_scraper <- function(latitude, longitude){
  
  # Check for latitude and longitude
  if(missing(latitude) | missing(longitude)){
    stop("Must specify latitude and longitude.")
  }
  
  # Build URL
  query_url <- paste0(
    "http://10.56.71.19/wxshield/getEnsembles.php?",
    "lat=", latitude,
    "&long=", longitude, 
    "&dateOffset=0&numDays=30", 
    "&indices=APCP,TMP,RH,WS,WD"
  )
  
  # Read JSON
  raw <- jsonlite::fromJSON(query_url)
  
  # Get list of parameters
  params <- raw$Indices
  
  # Get list of forecast dates
  dates <- raw$ForDates

  # Extract data by parameter
  AFFES_data <- lapply(1:length(params), function(x){
    dat <- raw$Models$AFFES$Members[,,x]
  })
  
  # Rename list entires to parameter name
  names(AFFES_data) <- params
  
  # Add timestamps for number of forecasted values
  AFFES_data$Timestamp <- lubridate::ymd_hms(
    dates[1:length(AFFES_data[[1]])]
    )
  
  # Cast as data frame and rearrange columns
  AFFES_data <- data.frame(AFFES_data)
  AFFES_data <- dplyr::select(.data = AFFES_data,
                              Timestamp,
                              dplyr::everything())

  return(AFFES_data)
}