#' Retrieve activity summary data from Strava API
#'
#' \code{get_activity_data} returns a dataframe of activity data from the Strava API. The 200 most recent activities are returned by default.
#'
#' @param strava_token Strava authentication token.
#' @param N_to_return Number of activities to return. Defaults to \code{200}.
#' @param activities_to_exclude Any activity IDs to exclude. Defaults to \code{NA_character}.
#' @return A dataframe of stream data
#' @import httr jsonlite dplyr purrr stringr
#' @export
#' @examples
#' my_activity_df <- get_activity_data(strava_token = my_auth_token,
#'                                     N_to_return = 10)
#'

get_activity_data <- function(strava_token,
                             N_to_return = 200,
                             activities_to_exclude = NA_integer_) {
  
  activity_data <- httr::GET(url = "https://www.strava.com/api/v3/athlete/activities",
                             config = strava_token, 
                             query = list(per_page = N_to_return))
  
  # Find new activities and process
  activities <- activity_data$content %>%
    rawToChar() %>%
    jsonlite::fromJSON() %>% 
    dplyr::filter(!id %in% activities_to_exclude) %>% 
    dplyr::mutate(start_lat = purrr::map_dbl(start_latlng, 1),
                  start_lng = purrr::map_dbl(start_latlng, 2),
                  end_lat = purrr::map_dbl(end_latlng, 1),
                  end_lng = purrr::map_dbl(end_latlng, 2),
                  strava_link = stringr::str_glue("https://www.strava.com/activities/{id}"),
                  ride_start = stringr::str_replace_all(start_date_local, "T|Z", " ") %>% as.POSIXct(),
                  ride_start = format(ride_start, "%Y-%m-%d %H:%M:%S")) %>% 
    dplyr::select(-c(athlete, map, start_latlng, end_latlng)) %>% 
    as_tibble()
  
  return(activities)
  
}
