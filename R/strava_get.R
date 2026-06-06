#' Internal helper for GET requests to the Strava API
#'
#' Executes authenticated GET requests against the Strava API,
#' validates the HTTP response, and parses JSON content into
#' an R object.
#'
#' @param url Character string. API endpoint URL.
#' @param token Strava authentication token created via
#'   \code{get_strava_token()}.
#' @param query Named list of query parameters passed to the API.
#'   Defaults to \code{NULL}.
#' @param verbose Logical indicating whether HTTP status
#'   information should be printed. Defaults to \code{FALSE}.
#'
#' @return Parsed JSON response as an R object.
#'
#' @import httr jsonlite
#'
#' @keywords internal
#' @noRd
strava_get_internal <- function(url, token, query = NULL) {
  
  resp <- httr::GET(
    url = url,
    config = token,
    query = query
  )
  
  httr::stop_for_status(resp)
  
  parsed <- httr::content(resp, as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON()
  
  return(parsed)
}