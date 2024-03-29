% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/windfetch_class.R, R/windfetch.R
\docType{package}
\name{windfetch}
\alias{windfetch}
\title{Calculate Wind Exposure with the \pkg{windfetch} Package}
\usage{
windfetch(
  polygon_layer,
  site_layer,
  max_dist = 300,
  n_directions = 9,
  quiet = FALSE,
  progress_bar = TRUE
)
}
\arguments{
\item{polygon_layer}{\code{\link[sf]{sf}} polygon object where the
polygon geometries represent any obstructions to fetch
calculations including the coastline, islands and/or
exposed reefs.}

\item{site_layer}{\code{\link[sf]{sf}}* points object where the point
geometries represent the site locations.}

\item{max_dist}{numeric. Maximum distance in kilometers (default 300). This
will need to be scaled manually if the units for the CRS are
not 'm'.}

\item{n_directions}{numeric. The number of fetch vectors to calculate per
quadrant (default 9).}

\item{quiet}{logical. Suppress diagnostic messages? (Default \code{FALSE}).}

\item{progress_bar}{logical. Show a text progress bar? (Default \code{TRUE})}
}
\value{
Returns a \code{\link{WindFetch}} object.
}
\description{
The \pkg{windfetch} package allows for an objective calculation of wind fetch
and provides methods to visualise the wind exposure and export the fetch
vectors to a KML file.

Wind fetch is the unobstructed length of water over which wind can blow, and
it is commonly used as a measure of exposure to wind and waves at coastal
sites. The \code{windfetch} function automatically calculates the wind fetch for
marine locations within the boundaries of the specified coastline layer.
This allows wind fetch to be calculated anywhere around the globe.
}
\details{
Fetch is an important measurement in coastal applications. It
provides a measurement for the unobstructed length of water that wind from a
certain direction can blow over. The higher the wind fetch from a certain
direction, the more energy is imparted onto the surface of the
water resulting in a larger sea state. Therefore, the larger
the fetch, the larger the exposure to wind and the more likely the
site experiences larger sea states.

The fetch length from all directions (and from each quadrant) can
be averaged to provide an indication of the location's exposure to
wind. The \pkg{windfetch} package
calculates the lengths of wind fetch vectors from all directions, at any
given location(s) on Earth, and can provide summaries, visualisations and
shape files along with the raw data.

The function takes an \code{\link[sf]{sf}} object
(\code{polygon_layer}) that represents the coastline, surrounding islands,
and any other obstructions, and calculates the wind fetch for every specified
direction. This is calculated for all the user-defined sites, that are
represented as the point geometries in an
\code{\link[sf]{sf}} object.

The directions for which the wind fetch are calculated for each site are
determined by the number of directions per quadrant (\code{n_directions}).
The default value of 9 calculates 9 fetch vectors per quadrant (90 degrees),
or equivalently, one fetch vector every 10 degrees. The first fetch vector is
always calculated for the northerly direction (0/360 degrees).

The site names are taken from a column of the data associated with
\code{site_layer} matching the regular expression \code{^[Nn]ames{0,1}}. If
there is no such column, then default names are created ('Site 1', 'Site 2',
...).
}
\note{
At least one of the inputs to the \code{polygon_layer} or
      \code{site_layer} arguments must be projected. If one of the inputs are
      not projected, then it will be transformed to have the same projection
      as the other. If both are projected, but do not have identical
      coordinate reference systems (CRS) then \code{site_layer} will be
      transformed to the same CRS as \code{polygon_layer}.
}
\examples{

# Create the polygon layer ----------------------------------------

library(sf)

# Read in North Carolina shape file
nc = st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

# convert to a geometry
ncg = st_geometry(nc)

# Transform to NAD83 projection, which seems like a suitable projection for
# North Carolina.
nc_proj = st_transform(ncg, "epsg:32119")

# Create the points layer ----------------------------------------

# Create an sf MULTIPOINT object for the site locations. Note these are
# specified as lats and lons (epsg: 4326) and are not yet projected.
sites_df = data.frame(lon = c(-76, -76, -77.3),
                      lat = c(36, 35.3, 34.5))

sites_points = st_as_sf(sites_df, coords = c("lon", "lat"), crs = "epsg:4326")

# Project the site locations onto the same NAD83 projection
sites_points_proj = st_transform(sites_points, "epsg:32119")

# Visualise the locations on a map --------------------------------

plot(nc_proj, col = "lightgrey", border = "grey")
plot(sites_points_proj, add = TRUE, pch = "x")

# Calculate wind fetch --------------------------------------------

nc_fetch = windfetch(nc_proj, sites_points)

# Visualise the fetch vectors on a map
plot(nc_fetch)
plot(nc_proj, col = "lightgrey", border = "grey", add = TRUE)

# Return only the summary data frame
summary(nc_fetch)

# Transform the fetch vectors back to the original CRS (lats and lons)
nc_fetch_latlon = crs_transform(nc_fetch, "epsg:4326")

# Return the raw data as a data.frame in the original, lat/lon coordinates
nc_fetch_latlon_df = as(nc_fetch_latlon, "data.frame")

# ...or as a tibble if you prefer...
nc_fetch_latlon_df = asTibble(nc_fetch_latlon)
nc_fetch_latlon

# ... or as an sf object.
nc_fetch_sf = as_sf(nc_fetch_latlon)
nc_fetch_sf

# Plot the wind fetch vectors ------------------------------------

# Plot the fetch vectors in the projected space...
plot(nc_fetch)
plot(nc_proj, col = "lightgrey", border = "grey", add = TRUE)

# ... or in the unprojected lat lon space
plot(nc_fetch_latlon)
plot(ncg, col = "lightgrey", border = "grey", add = TRUE)

# Output to Shapefile --------------------------------------------
\dontrun{

# Output to a shape file in the current working directory.
st_write(nc_fetch_sf, "nc_fetch.shp", driver = "ESRI Shapefile")
}
}
\seealso{
\code{\link{windfetch}} for an extensive reproducible example.

\code{vignette("introduction-to-windfetch")} for a short introduction
to **windfetch**.

\code{\link[sf]{st_transform}} for methods on transforming map
         projections and datum.

\code{\link{summary,WindFetch-method}} for summarising the fetch lengths.
}
\keyword{package}
