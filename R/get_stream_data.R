#' Retrieve activity stream data from Strava API
#'
#' \code{get_stream_data} returns a dataframe of activity stream
#' data retrieved from the Strava API for a given activity ID.
#'
#' @param activity_id The Strava activity ID for which stream
#'   data should be retrieved.
#' @param stream_types Character vector of stream types to retrieve.
#' @param strava_token Strava authentication token.
#' @param display_map Logical indicating whether a leaflet map
#'   of the route should be plotted. Defaults to \code{FALSE}.
#'
#' @return A tibble of stream data.
#'
#' @import httr stringr dplyr tidyr jsonlite leaflet tibble
#' @export

get_stream_data <- function(
    activity_id,
    stream_types = c(
      "time",
      "latlng",
      "distance",
      "altitude",
      "velocity_smooth",
      "heartrate",
      "cadence",
      "watts",
      "temp",
      "moving",
      "grade_smooth"
    ),
    strava_token,
    display_map = FALSE
) {
  
  message(
    stringr::str_glue(
      "Starting stream data retrieval for activity {activity_id}."
    )
  )
  
  # ----- validation -----
  
  stopifnot(
    length(activity_id) == 1,
    is.numeric(activity_id),
    is.character(stream_types),
    is.logical(display_map)
  )
  
  # ----- build stream request -----
  
  stream_types_query <- stringr::str_flatten(
    stream_types,
    collapse = ","
  )
  
  stream_url <- paste0(
    "https://www.strava.com/api/v3/activities/",
    activity_id,
    "/streams/",
    stream_types_query
  )
  
  # ----- retrieve stream data -----
  
  stream <- strava_get_internal(
    url = stream_url,
    token = strava_token
  )
  
  # ----- parse stream data -----
  
  stream_to_load <- stream |>
    tidyr::unnest(data) |>
    dplyr::group_by(type) |>
    dplyr::mutate(
      record_id = dplyr::row_number()
    ) |>
    dplyr::ungroup()
  
  # ----- handle lat/lng streams -----
  
  if(any(stream_to_load$type == "latlng")) {
    
    stream_to_load <- stream_to_load |>
      dplyr::mutate(
        value = ifelse(
          type == "latlng",
          data[,1],
          data
        )
      ) |>
      dplyr::select(
        record_id,
        type,
        value
      ) |>
      tidyr::pivot_wider(
        names_from = "type",
        values_from = "value"
      ) |>
      dplyr::rename(
        lat = latlng
      ) |>
      dplyr::left_join(
        stream_to_load |>
          dplyr::filter(type == "latlng") |>
          dplyr::transmute(
            record_id,
            lng = data[,2]
          ),
        by = "record_id"
      ) |>
      dplyr::select(-record_id) |>
      dplyr::mutate(
        strava_id = activity_id
      )
    
  } else {
    
    stream_to_load <- stream_to_load |>
      dplyr::select(
        record_id,
        type,
        data
      ) |>
      tidyr::pivot_wider(
        names_from = "type",
        values_from = "data"
      ) |>
      dplyr::select(-record_id) |>
      dplyr::mutate(
        strava_id = activity_id
      )
    
  }
  
  # ----- optional map rendering -----
  
  if(display_map &&
     all(c("lat", "lng") %in% names(stream_to_load))) {
    
    leaflet::leaflet() |>
      leaflet::addTiles() |>
      leaflet::addPolylines(
        lng = stream_to_load$lng,
        lat = stream_to_load$lat
      ) |>
      print()
    
  }
  
  return(
    tibble::as_tibble(stream_to_load)
  )
  
}