# Internal functions ------------------------------------------------------

#' @importFrom sf st_intersection st_cast st_nearest_points st_nearest_feature

return_fetch_vector = function(vector, origin, polygon) {
  fetch_on_land = suppressWarnings(st_intersection(vector, polygon))

  if (nrow(fetch_on_land) == 0) {
    fetch = vector
  } else {
    fetch_ends = suppressWarnings(st_cast(fetch_on_land, "POINT"))
    fetch_vector = st_nearest_points(origin, fetch_ends)
    which_closest = st_nearest_feature(origin, fetch_ends)
    fetch = fetch_vector[which_closest]
  }
  fetch
}
