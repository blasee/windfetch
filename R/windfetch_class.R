#' Calculate Wind Exposure with the \pkg{windfetch} Package
#'
#' The \pkg{windfetch} package allows for an objective calculation of wind fetch
#' and provides methods to visualise the wind exposure and export the fetch
#' vectors to a KML file.
#'
#' Fetch is an important measurement in coastal applications. It
#' provides a measurement for the unobstructed length of water that wind from a
#' certain direction can blow over. The higher the wind fetch from a certain
#' direction, the more energy is imparted onto the surface of the
#' water resulting in a larger sea state. Therefore, the larger
#' the fetch, the larger the exposure to wind and the more likely the
#' site experiences larger sea states.
#'
#' The fetch length from all directions (and from each quadrant) can
#' be averaged to provide an indication of the location's exposure to
#' wind. The \pkg{windfetch} package
#' calculates the lengths of wind fetch vectors from all directions, at any
#' given location(s) on Earth, and can provide summaries, visualisations and
#' shape files along with the raw data.
#'
#' @seealso \code{\link{windfetch}} for an extensive reproducible example.
#' @seealso \code{vignette("introduction-to-windfetch")} for a short introduction
#' to **windfetch**.
#' @name windfetch
#' @docType package
#' @keywords package
NULL

#' WindFetch Class
#'
#' Class to hold WindFetch objects.
#'
#' @note WindFetch objects should only be created using the \code{\link{windfetch}}
#' constructor function.
#'
#' @slot names character vector containing the names for each location.
#' @slot max_dist numeric vector of length 1 containing the maximum distance (km)
#'                of the fetch vectors.
#'
#' @name WindFetch
#' @rdname WindFetch-class
#' @importFrom methods setClass
setClass("WindFetch", slots = c(names = "character", max_dist = "numeric"),
         contains = "data.frame")

# Fetch Class Validation
valid_windfetch = function(object){
  errors = character()

  if (length(errors) == 0){
    TRUE
  } else {
    errors
  }
}

#' @importFrom methods setValidity
setValidity("WindFetch", valid_windfetch)

#' Summarise a WindFetch Object
#'
#' The \code{summary} function calculates the average wind fetch for the separate
#' northerly, easterly, southerly and westerly quadrants. For example, the mean
#' fetch for the northerly component averages over the fetch vectors with
#' directions between 315 (inclusive) and 45 (exclusive) degrees, i.e. the fetch
#' vectors within the interval [315, 45).
#'
#' @return The \code{summary} function returns a \code{\link[dplyr]{tibble}}.
#'
#' @param object a \code{WindFetch} object that has been returned by the
#'               \code{\link{windfetch}} function.
#'
#' @importFrom methods setMethod
#' @importFrom magrittr %>%
#' @importFrom dplyr tibble group_by summarise select
#' @importFrom rlang .data
#' @export
setMethod("summary", "WindFetch", function(object){

  tibble(site_name = object@.Data[[1]],
         direction = object@.Data[[2]],
         quadrant = object@.Data[[4]],
         fetch = object@.Data[[5]]) %>%
    group_by(.data$site_name, .data$quadrant) %>%
    summarise(avg_fetch = round(mean(.data$fetch))) %>%
    select(.data$site_name, .data$quadrant, .data$avg_fetch)
})

#' @rdname summary-WindFetch-method
#' @importFrom methods setMethod show
#' @importFrom sf st_is_longlat
setMethod("show", "WindFetch", function(object){

  cat("Is projected\t: ", !st_is_longlat(object[[3]]), "\n",
      "Max distance\t: ", object@max_dist, " km\n",
      "Directions\t: ", length(object[[1]]), "\n",
      "Sites\t\t: ", length(unique(object[[1]])), "\n\n",
      sep = "")
  print(summary(object))
})

#' @importFrom methods setAs validObject S3Part<-
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble group_by summarise select tibble bind_cols
#' @importFrom sf st_coordinates
#' @importFrom rlang .data
setAs("WindFetch", "data.frame", function(from){
  validObject(from)
  xy_ends_df = from@.Data[[3]] %>%
    st_coordinates() %>%
    as_tibble() %>%
    group_by(.data$L1) %>%
    summarise(x = .data$X[1],
              y = .data$Y[1],
              x_end = .data$X[2],
              y_end = .data$Y[2]) %>%
    select(.data$x, .data$y, .data$x_end, .data$y_end)

  tibble(site_names = from@.Data[[1]],
         directions = from@.Data[[2]],
         quadrant = from@.Data[[4]],
         fetch = from@.Data[[5]]) %>%
    bind_cols(xy_ends_df) %>%
    as.data.frame()
})

#' Transform the CRS of a WindFetch object
#'
#' Transform the CRS of a WindFetch object.
#'
#' @param x a WindFetch object created using the \link{windfetch} function.
#' @param y character string denoting the EPSG code in the format "epsg:xxxx".
#'          This parameter is passed into the \link[sf]{st_transform} function to
#'          do the transformation.
#' @importFrom methods setGeneric
setGeneric("crs_transform", function(x, y) standardGeneric("crs_transform"))

#' Transform the CRS of a WindFetch object
#'
#' Transform the CRS of a WindFetch object.
#'
#' @param x a WindFetch object created using the \link{windfetch} function.
#' @param y character string denoting the EPSG code in the format "epsg:xxxx".
#'          This parameter is passed into the \link[sf]{st_transform} function to
#'          do the transformation.
#' @importFrom methods setMethod validObject
#' @importFrom sf st_transform
#' @export
setMethod("crs_transform",
          signature(x = "WindFetch", y = "character"),
          function (x, y) {
            validObject(x)

            x@.Data[[3]] = st_transform(x@.Data[[3]], y)
            return(x)
          }
)

#' Coerce a WindFetch object to an sf Object
#'
#' Coerce a WindFetch object to an sf Object.
#'
#' @param x a WindFetch object created using the \link{windfetch} function.
#' @importFrom methods setGeneric
setGeneric("as_sf", function(x) standardGeneric("as_sf"))

#' Coerce a WindFetch object to an sf Object
#'
#' Coerce a WindFetch object to an sf Object.
#'
#' @param x a WindFetch object created using the \link{windfetch} function.
#' @importFrom methods setMethod validObject
#' @importFrom sf st_transform
#' @export
setMethod("as_sf",
          signature(x = "WindFetch"),
          function (x) {
            validObject(x)

            x = st_sf(as.data.frame(x@.Data,
                                col.names = c("site_name", "directions",
                                              "geom", "quadrant", "fetch")))
            return(x)
          }
)

#' Coerce a WindFetch Object to a Tibble
#'
#' Coerce a WindFetch object to a tibble.
#'
#' @param x a WindFetch object created using the \link{windfetch} function.
#' @importFrom methods setGeneric
setGeneric("asTibble", function(x) standardGeneric("asTibble"))

#' Coerce a WindFetch Object to a Tibble
#'
#' Coerce a WindFetch object to a tibble.
#'
#' @param x a WindFetch object created using the \link{windfetch} function.
#' @importFrom methods setMethod validObject
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble group_by summarise select tibble bind_cols
#' @importFrom sf st_coordinates
#' @importFrom rlang .data
#' @export
setMethod("asTibble",
          signature(x = "WindFetch"),
          function (x) {
            validObject(x)

            xy_ends_df = x@.Data[[3]] %>%
              st_coordinates() %>%
              as_tibble() %>%
              group_by(.data$L1) %>%
              summarise(x = .data$X[1],
                        y = .data$Y[1],
                        x_end = .data$X[2],
                        y_end = .data$Y[2]) %>%
              select(.data$x, .data$y, .data$x_end, .data$y_end)

            tibble(site_names = x@.Data[[1]],
                   directions = x@.Data[[2]],
                   quadrant = x@.Data[[4]],
                   fetch = x@.Data[[5]]) %>%
              bind_cols(xy_ends_df)
          }
)

#' Plot a WindFetch Object
#'
#' Plot method for \code{\link{WindFetch}} objects.
#'
#' @param x a \code{\link{WindFetch}} object as returned by
#'          \code{\link{windfetch}}.
#' @param y missing.
#' @param ... further arguments passed to
#'            \code{\link{plot}}.
#'
#' @importFrom methods setMethod as
#' @export
setMethod("plot",
          signature(x = "WindFetch", y = "missing"),
          definition = function(x, y, ...){
            validObject(x)
            plot(x@.Data[[3]], ...)
          })
