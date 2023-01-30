#' Retrieve activity summary data from Strava API
#'
#' \code{get_activity_data} returns an dataframe of activity data from the Strava API.
#'
#' @param strava_token Strava authentication token.
#' @param N_to_return Number of activities to return. Defaults to \code{200}.
#' @return A dataframe of stream data
#' @import httr jsonlite
#' @export
#' @examples
#' my_activity_df <- get_activity_data(strava_token = my_auth_token,
#'                                     N_to_return = 10)
#'

get_activity_data <- function(strava_token,
                             N_to_return = 200) {
  
  activity_data <- httr::GET(url = "https://www.strava.com/api/v3/athlete/activities",
                             config = strava_token, 
                             query = list(per_page = N_to_return))
  
  # Find new activities and process
  activities <- activity_data$content %>%
    rawToChar() %>%
    jsonlite::fromJSON()
  
  return(activities)
  
}
