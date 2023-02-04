#' Add a track to a leaflet map from a dataframe containing latitude and longitude data
#'
#' \code{add_track} returns a leaflet object with a track added
#'
#' @param leaflet_obj Leaflet object.
#' @param position_tbl A dataframe containing separate columns corresponding to latitude and longitude position data
#' @param lat_lng_names A vector of the names of the latitude and longitude columns respectively. Defaults to \code{c("lat","lng")}
#' @param track_colour Colour of the track line drawn. Defaults to \code{"#0C2340"}.
#' @return A leaflet object
#' @import leaflet
#' @export
#' @examples
#' my_map <- get_activity_data(leaflet_obj = my_map,
#'                             position_tbl = my_tbl)
#'

add_track <- function(leaflet_obj, position_tbl, lat_lng_names = c("lat", "lng"), track_colour = "#0C2340") {
  
  latitude <- position_tbl[[lat_lng_names[1]]]
  longitude <- position_tbl[[lat_lng_names[2]]]
  
  leaflet_obj <- leaflet::addPolylines(map = leaflet_obj, lat = latitude, lng = longitude, opacity = 0.5, weight = 2, color = track_colour)
  
  return(leaflet_obj)
  
}