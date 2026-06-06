#' Retrieve activity summary data from Strava API
#'
#' @param strava_token Strava authentication token.
#' @param N_to_return Number of activities to return.
#' @param activities_to_exclude Activity IDs to exclude.
#' @param sport_types_to_include Sport types to include.
#'
#' @return Tibble of activity summary data
#' @export

get_activity_data <- function(
    strava_token,
    N_to_return = 200,
    activities_to_exclude = integer(),
    sport_types_to_include = c("Ride", "VirtualRide")
) {
  
  # ----- validation -----
  
  stopifnot(
    is.numeric(N_to_return),
    length(N_to_return) == 1,
    N_to_return > 0
  )
  
  # ----- pagination -----
  
  per_page <- min(200, N_to_return)
  
  page <- 1
  total_retrieved <- 0
  
  activity_pages <- list()
  
  repeat {
    
    message(glue::glue(
      "Retrieving activity page {page}..."
    ))
    
    page_data <- .strava_get(
      url = "https://www.strava.com/api/v3/athlete/activities",
      token = strava_token,
      query = list(
        per_page = per_page,
        page = page
      )
    )
    
    # No more results
    if(length(page_data) == 0) {
      break
    }
    
    activity_pages[[page]] <- tibble::as_tibble(page_data)
    
    total_retrieved <- total_retrieved + nrow(activity_pages[[page]])
    
    if(total_retrieved >= N_to_return) {
      break
    }
    
    page <- page + 1
    
  }
  
  # ----- combine -----
  
  activities <- dplyr::bind_rows(activity_pages)
  
  # Limit exactly to requested count
  activities <- dplyr::slice_head(
    activities,
    n = N_to_return
  )
  
  # ----- filtering -----
  
  activities <- activities |>
    dplyr::filter(
      !id %in% activities_to_exclude,
      sport_type %in% sport_types_to_include
    )
  
  # ----- feature engineering -----
  
  activities <- activities |>
    dplyr::mutate(
      
      strava_id = id,
      
      start_lat = purrr::map_dbl(
        start_latlng,
        ~ .x[[1]] %||% NA_real_
      ),
      
      start_lng = purrr::map_dbl(
        start_latlng,
        ~ .x[[2]] %||% NA_real_
      ),
      
      end_lat = purrr::map_dbl(
        end_latlng,
        ~ .x[[1]] %||% NA_real_
      ),
      
      end_lng = purrr::map_dbl(
        end_latlng,
        ~ .x[[2]] %||% NA_real_
      ),
      
      strava_link = glue::glue(
        "https://www.strava.com/activities/{id}"
      ),
      
      ride_start = lubridate::ymd_hms(
        start_date_local,
        quiet = TRUE
      ),
      
      average_watts = dplyr::if_else(
        device_watts %in% TRUE,
        average_watts,
        NA_real_
      ),
      
      weighted_average_watts = dplyr::if_else(
        device_watts %in% TRUE,
        weighted_average_watts,
        NA_real_
      ),
      
      kilojoules = dplyr::if_else(
        device_watts %in% TRUE,
        kilojoules,
        NA_real_
      ),
      
      max_watts = dplyr::if_else(
        device_watts %in% TRUE,
        max_watts,
        NA_real_
      ),
      
      device_watts = as.integer(device_watts)
      
    ) |>
    
    dplyr::select(
      -dplyr::any_of(c(
        "id",
        "athlete",
        "map",
        "start_latlng",
        "end_latlng",
        "device_name"
      ))
    ) |>
    
    tibble::as_tibble()
  
  return(activities)
  
}