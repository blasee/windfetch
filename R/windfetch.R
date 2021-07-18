#' Calculate Wind Fetch
#'
#' Wind fetch is the unobstructed length of water over which wind can blow, and
#' it is commonly used as a measure of exposure to wind and waves at coastal
#' sites. The \code{windfetch} function automatically calculates the wind fetch for
#' marine locations within the boundaries of the specified coastline layer.
#' This allows wind fetch to be calculated anywhere around the globe.
#'
#' The function takes an \code{\link[sf]{sf}} object
#' (\code{polygon_layer}) that represents the coastline, surrounding islands,
#' and any other obstructions, and calculates the wind fetch for every specified
#' direction. This is calculated for all the user-defined sites, that are
#' represented as the point geometries in an
#' \code{\link[sf]{sf}} object.
#'
#' The directions for which the wind fetch are calculated for each site are
#' determined by the number of directions per quadrant (\code{n_directions}).
#' The default value of 9 calculates 9 fetch vectors per quadrant (90 degrees),
#' or equivalently, one fetch vector every 10 degrees. The first fetch vector is
#' always calculated for the northerly direction (0/360 degrees).
#'
#' @param polygon_layer \code{\link[sf]{sf}} polygon object where the
#'                      polygon geometries represent any obstructions to fetch
#'                      calculations including the coastline, islands and/or
#'                      exposed reefs.
#' @param site_layer \code{\link[sf]{sf}}* points object where the point
#'                   geometries represent the site locations.
#' @param max_dist numeric. Maximum distance in kilometers (default 300). This
#'                 will need to be scaled manually if the units for the CRS are
#'                 not 'm'.
#' @param n_directions numeric. The number of fetch vectors to calculate per
#'                     quadrant (default 9).
#' @param site_names character vector of the site names. If missing, the site
#'                   names are taken from a column of the data associated with
#'                   \code{site_layer} matching the regular expression
#'                   \code{^[Nn]ames{0,1}}. If there is no such column, then
#'                   default names are created ('Site 1', 'Site 2', ...).
#' @param quiet logical. Suppress diagnostic messages? (Default \code{FALSE}).
#' @param progress_bar logical. Show a text progress bar? (Default \code{TRUE})
#'
#' @return Returns a \code{\link{WindFetch}} object.
#'
#' @note At least one of the inputs to the \code{polygon_layer} or
#'       \code{site_layer} arguments must be projected. If one of the inputs are
#'       not projected, then it will be transformed to have the same projection
#'       as the other. If both are projected, but do not have identical
#'       coordinate reference systems (CRS) then \code{site_layer} will be
#'       transformed to the same CRS as \code{polygon_layer}.
#'
#' @seealso \code{\link[sf]{st_transform}} for methods on transforming map
#'          projections and datum.
#' @seealso \code{\link{summary,WindFetch-method}} for summarising the fetch lengths.
#'
#' @importFrom utils head txtProgressBar setTxtProgressBar
#' @importFrom methods new is
#' @importFrom sf st_is st_is_longlat st_crs st_transform st_intersects
#'                st_buffer st_coordinates st_sf st_sfc st_linestring st_point
#'                st_length st_geometry<-
#' @importFrom dplyr left_join
#' @include windfetch_class.R
#' @examples
#'
#' # Create the polygon layer ----------------------------------------
#'
#' library(sf)
#'
#' # Read in North Carolina shape file
#' nc = st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#'
#' # convert to a geometry
#' ncg = st_geometry(nc)
#'
#' # Transform to NAD83 projection, which seems like a suitable projection for
#' # North Carolina.
#' nc_proj = st_transform(ncg, "epsg:32119")
#'
#' # Create the points layer ----------------------------------------
#'
#' # Create an sf MULTIPOINT object for the site locations. Note these are
#' # specified as lats and lons (epsg: 4326) and are not yet projected.
#' sites_df = data.frame(lon = c(-76, -76, -77.3),
#'                       lat = c(36, 35.3, 34.5))
#'
#' sites_points = st_as_sf(sites_df, coords = c("lon", "lat"), crs = "epsg:4326")
#'
#' # Project the site locations onto the same NAD83 projection
#' sites_points_proj = st_transform(sites_points, "epsg:32119")
#'
#' # Visualise the locations on a map --------------------------------
#'
#' plot(nc_proj, col = "lightgrey", border = "grey")
#' plot(sites_points_proj, add = TRUE, pch = "x")
#'
#' # Calculate wind fetch --------------------------------------------
#'
#' nc_fetch = windfetch(nc_proj, sites_points)
#'
#' # Visualise the fetch vectors on a map
#' plot(nc_fetch)
#' plot(nc_proj, col = "lightgrey", border = "grey", add = TRUE)
#'
#' # Return only the summary data frame
#' summary(nc_fetch)
#'
#' # Transform the fetch vectors back to the original CRS (lats and lons)
#' nc_fetch_latlon = crs_transform(nc_fetch, "epsg:4326")
#'
#' # Return the raw data as a data.frame in the original, lat/lon coordinates
#' nc_fetch_latlon_df = as(nc_fetch_latlon, "data.frame")
#'
#' # ...or as a tibble if you prefer...
#' nc_fetch_latlon_df = asTibble(nc_fetch_latlon)
#' nc_fetch_latlon
#'
#' # ... or as an sf object.
#' nc_fetch_sf = as_sf(nc_fetch_latlon)
#' nc_fetch_sf
#'
#' # Plot the wind fetch vectors ------------------------------------
#'
#' # Plot the fetch vectors in the projected space...
#' plot(nc_fetch)
#' plot(nc_proj, col = "lightgrey", border = "grey", add = TRUE)
#'
#' # ... or in the unprojected lat lon space
#' plot(nc_fetch_latlon)
#' plot(ncg, col = "lightgrey", border = "grey", add = TRUE)
#'
#' # Output to Shapefile --------------------------------------------
#' \dontrun{
#'
#' # Output to a shape file in the current working directory.
#' st_write(nc_fetch_sf, "nc_fetch.shp", driver = "ESRI Shapefile")
#' }
#' @export
windfetch = function(polygon_layer, site_layer, max_dist = 300, n_directions = 9,
                 site_names, quiet = FALSE, progress_bar = TRUE) {

  if (!any(is(polygon_layer, "sf"), is(polygon_layer, "sfc")))
    stop(paste("polygon_layer must be either an sf or sfc object.\nSee",
               "`?read_sf` for details on how to read in an",
               "sf object."), call. = FALSE)

  if (!any(is(site_layer, "sf"), is(site_layer, "sfc")))
    stop(paste("site_layer must be either an sf or sfc object.\nSee",
               "`?fetch` for an example on how to create this",
               "object."), call. = FALSE)

  if (!any(st_is(site_layer, "POINT"), st_is(site_layer, "MULTIPOINT")))
    stop(paste("site_layer must be a POINT or MULTIPOINT object.\nSee",
               "`?windfetch` for an example on how to create this",
               "object."), call. = FALSE)

  if (!is.numeric(max_dist) || length(max_dist) != 1)
    stop("max_dist must be a single number.", call. = FALSE)

  if (!is.numeric(n_directions) || length(n_directions) != 1)
    stop("n_directions must be a single integer.", call. = FALSE)
  n_directions = round(n_directions)

  if (n_directions < 1 || n_directions > 90)
    stop("n_directions must be between 1 and 90.", call. = FALSE)

  if (!missing(site_names))
    warning(paste("Argument site_names is deprecated; please incorporate the",
                  "site names into the site_layer dataframe instead.\nSee",
                  "`?fetch` for an example on how to do this."),
            call. = FALSE)

  quiet = as.logical(quiet[[1]])

  ## Check if the polygon and points layers are projected, and ensure they have
  ## the same CRS.

  which_proj = c(!st_is_longlat(polygon_layer), !st_is_longlat(site_layer))

  if (all(!which_proj))
    stop("The polygon_layer or site_layer argument must be projected to calculate fetch.",
         call. = FALSE)

  if (all(which_proj) && !(st_crs(site_layer) == st_crs(polygon_layer))){
    warning(paste("The CRS for the polygon_layer and site_layer arguments",
                  "differ; transforming site_layer CRS to match."),
            call. = FALSE)

    site_layer = st_transform(site_layer, st_crs(polygon_layer))
  }

  if (!which_proj[1]){
    if (!quiet)
      message("projecting polygon_layer onto the site_layer CRS")
    polygon_layer = st_transform(polygon_layer, st_crs(site_layer))
  }

  if (!which_proj[2]){
    if (!quiet)
      message("projecting site_layer onto the polygon_layer CRS")
    site_layer = st_transform(site_layer, st_crs(polygon_layer))
  }

  if (!quiet)
    message("checking site locations are not on land")

  sites_on_land = st_intersects(site_layer, polygon_layer, sparse = FALSE)[, 1]

  if (any(sites_on_land)) {
    warning(paste("Removing the", sum(sites_on_land), "points on land."),
            call. = FALSE)
    site_layer = site_layer[!sites_on_land, ]
  }

  if (any(grepl("^[Nn]ames{0,1}$", names(site_layer)))){
    name_col = grep("^[Nn]ames{0,1}$", names(site_layer))
    site_names = as.character(site_layer[[name_col]])
  } else {
    site_names = paste("Site", seq_len(dim(site_layer)[[1]]))
  }

  site_layer$site_names = site_names

  # Convert max_dist to appropriate units.
  # First of all convert max_dist to metres (default)
  max_dist = max_dist * 1000

  # Double check if metres are the correct units, and warn otherwise.
  proj_unit = st_crs(polygon_layer)$units

  if (proj_unit != "m")
    warning(paste("The CRS unit is not metres; ensure that the max_dist argument",
                  "has been scaled appropriately."), call. = FALSE)

  directions = head(seq(0, 360, by = 360 / (n_directions * 4)), -1)

  # Return the quadrant the directions belong to
  dirs = as.numeric(directions)
  dirs_bin = findInterval(dirs, seq(45, 315, by = 90))
  quadrant = rep("North", length(dirs))
  quadrant[dirs_bin == 1] = "East"
  quadrant[dirs_bin == 2] = "South"
  quadrant[dirs_bin == 3] = "West"

  # Rearrange sequence order to start at 90 degrees
  directions = unlist(split(directions, directions < 90), use.names = FALSE)

  fetch_ends = st_buffer(site_layer, max_dist, n_directions)
  fetch_ends_df = as.data.frame(st_coordinates(fetch_ends))

  fetch_ends_df$site_names = site_names[fetch_ends_df[, 4]]

  fetch_locs_df = as.data.frame(st_coordinates(site_layer))
  colnames(fetch_locs_df) = c("X0", "Y0")
  fetch_locs_df$site_names = site_layer[[2]]

  fetch_df = unique(left_join(fetch_ends_df, fetch_locs_df, by = "site_names"))
  fetch_df$directions = c(head(seq(90, 360, by = 360 / (n_directions * 4)), -1),
                          head(seq(0, 90, by = 360 / (n_directions * 4)), -1))
  fetch_df = fetch_df[with(fetch_df, order(site_names, directions)), ]


  fetch_df = st_sf(fetch_df[, c("site_names", "directions")],
                   geom = st_sfc(sapply(apply(fetch_df, 1, function(x) {
                     X0 = as.numeric(x['X0'])
                     Y0 = as.numeric(x['Y0'])
                     X = as.numeric(x['X'])
                     Y = as.numeric(x['Y'])
                     st_sfc(st_linestring(matrix(c(X0, X, Y0, Y), 2, 2), dim = "XY"))
                   }), st_sfc), crs = st_crs(polygon_layer)),
                   origin = st_sfc(sapply(apply(fetch_df, 1, function(x) {
                     st_sfc(st_point(c(as.numeric(x['X0']), as.numeric(x['Y0']))))
                   }), st_sfc), crs = st_crs(polygon_layer)))

  poly_subset = polygon_layer[lengths(st_intersects(polygon_layer, fetch_df)) > 0]

  # Create an empty vector to store the fetch linestrings
  fetch = NA

  if (progress_bar)
    pb = txtProgressBar(max = nrow(fetch_df), style = 3)

  for (i in 1:nrow(fetch_df)) {
    fetch_df$fetch[i] = as.data.frame(
      return_fetch_vector(fetch_df[i, "geom"],
                          fetch_df$origin[i],
                          poly_subset))
    if (progress_bar)
      setTxtProgressBar(pb, i)
  }

  fetch_df$fetch = st_sfc(lapply(fetch_df$fetch, `[[`, 1),
                          crs = st_crs(polygon_layer))

  st_geometry(fetch_df) = fetch_df$fetch
  fetch_df = fetch_df[, 1:2]
  fetch_df$quadrant = factor(quadrant,
                             levels = c("North", "East", "South", "West"))
  fetch_df$fetch = st_length(fetch_df$geom)

  new("WindFetch", fetch_df, names = site_names, max_dist = max_dist / 1000)
}

