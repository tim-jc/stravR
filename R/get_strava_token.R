#' Load / create Strava authentication token
#'
#' \code{get_strava_token} returns an existing OAuth token from the working directory. If a token isn't present, it is created before being returned.
#' See \code{https://developers.strava.com} for more information on creating an application and getting the necessary keys to access the API.
#'
#' @param app_name The Strava application name, chosen by the user.
#' @param app_client_id An integer, assigned by Strava.
#' @param app_secret An alphanumeric key, assigned by Strava.
#' @param app_scope A character string of scope to request. Defaults to \code{"activity:read_all"}
#' @param cache A logical value, indicating whether the token should be chached. Defaults to \code{TRUE}.
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
                             app_secret,
                             app_scope = "activity:read_all",
                             cache = TRUE) {
  
  # check value supplied to app_scope
  allowed_scope_vals <- c("read","read_all","profile:read_all","profile:write","activity:read","activity:read_all","activity:write")
  if(!app_scope %in% allowed_scope_vals) {
    stop(stringr::str_glue("Invalid app_scope value supplied; allowed values are '{stringr::str_flatten(allowed_scope_vals, collapse = \"', '\")}'"))
  }
  
  # Check if strava authentication token exists; create if not
  if(file.exists(".httr-oauth")) {
    stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])
  } else {
    strava_app_details <- httr::oauth_app(appname = app_name, key = app_client_id, secret = app_secret)  
    
    strava_endpoint <- httr::oauth_endpoint(request = "https://www.strava.com/oauth/authorize?",
                                            authorize = "https://www.strava.com/oauth/authorize",
                                            access = "https://www.strava.com/oauth/token")
    
    strava_oauth_token <- httr::oauth2.0_token(endpoint = strava_endpoint, 
                                               app = strava_app_details, 
                                               scope = app_scope, 
                                               cache = cache)
    
    stoken <- httr::config(token = strava_oauth_token)
  }
  
  return(stoken)
  
}