#' Retrieve activity stream data from Strava API
#'
#' \code{get_stream_data} returns a dataframe of ride stream data retrieved from the Strava API for a given activity id.
#'
#' @param activity_id The Strava activity id for which stream data should be retrieved.
#' @param stream_types Character vector of the stream types to retrieve. Defaults to \code{'c("time", "latlng", "distance", "altitude", "velocity_smooth", "heartrate", "cadence", "watts", "temp", "moving", "grade_smooth")'}.
#' @param strava_token Strava authentication token.
#' @param display_map Logical to indicate whether a leaflet map of the streams route be plotted. Default value is \code{'FALSE'}.
#' @return A dataframe of stream data
#' @import httr magrittr stringr dplyr tidyr jsonlite leaflet
#' @export
#' @examples
#' my_stream_df <- get_stream_data(activity_id = 7545455572,
#'                                 strava_token = my_auth_token,
#'                                 display_map = F)
#'

get_stream_data <- function(activity_id,
                            stream_types = c("time", "latlng", "distance", "altitude", "velocity_smooth", "heartrate", "cadence", "watts", "temp", "moving", "grade_smooth"),
                            strava_token,
                            display_map = F) {
  
  stringr::str_glue("Starting stream data retrieval for activity {activity_id}.") |> print()
  
  stream_types <- stringr::str_flatten(stream_types, collapse = ",")
  
  stream <- httr::GET(url = paste0("https://www.strava.com/api/v3/activities/",activity_id,"/streams/",stream_types),
                      config = strava_token)
  
  if(stream$status_code == 429) {
    stop("Rate limit exceeded")
  }
  
  stream_to_load <- stream$content |>
    rawToChar() |>
    jsonlite::fromJSON() |> 
    tidyr::unnest(data) |> 
    dplyr::group_by(type) |> 
    dplyr::mutate(record_id = dplyr::row_number()) |> 
    dplyr::ungroup()
  
  if(any(stream_to_load$type == "latlng")) {
    stream_to_load <- stream_to_load |> 
      dplyr::mutate(value = data[,1]) |> 
      dplyr::select(record_id, type, value) |> 
      tidyr::pivot_wider(names_from = "type") |> 
      dplyr::rename(lat = latlng) |> 
      dplyr::left_join(stream_to_load |>
                         dplyr::filter(type == "latlng") |> 
                         dplyr::transmute(record_id, lng = data[,2])) |> 
      dplyr::select(-record_id) |> 
      dplyr::mutate(strava_id = activity_id)
  } else {
    stream_to_load <- stream_to_load |> 
      dplyr::select(record_id, type, data) |> 
      tidyr::pivot_wider(names_from = "type",
                         values_from = "data") |> 
      dplyr::select(-record_id) |> 
      dplyr::mutate(strava_id = activity_id)
  }   
  
  if(display_map) {
    
    leaflet::leaflet() |> 
      leaflet::addTiles() |> 
      leaflet::addPolylines(stream_to_load$lng, stream_to_load$lat) |> 
      print()
    
  }
  
  return(stream_to_load)
  
}