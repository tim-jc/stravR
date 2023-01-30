#' Load / create Strava authentication token
#'
#' \code{get_strava_token} returns an exisiting OAuth token from the working directory. If a token isn't present, it is created before being returned.
#' See \code{https://developers.strava.com} for more information on create an application and getting the necessary keys to access the API.
#'
#' @param app_name The Strava application name, chosen by the user.
#' @param app_client_id An integer, assigned by Strava.
#' @param app_secret An alphanumeric key, assigned by Strava.
#' @return An OAuth token
#' @import httr
#' @export
#' @examples
#' stoken <- get_strava_token(app_name = "my_app_name",
#'                            app_client_id = 12345,
#'                            app_secret = my_key_value)
#'

get_strava_token <- function(app_name,
                             app_client_id,
                             app_secret) {
  
  # Check if strava authentication token exists; create if not
  if(file.exists(".httr-oauth")) {
    stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])
  } else {
    stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret, app_scope="activity:read_all", cache = T))
  }
  
  return(stoken)
  
}
