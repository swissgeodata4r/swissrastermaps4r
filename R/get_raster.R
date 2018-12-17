#' Get corresponding raster maps to feature
#'
#' Specify \pkg{sf} Object an some additional parameters to get corresponding raster maps
#'
#'  The function takes an \{} object as an input. In addition, scale level
#'  (a scale 1:1'000'000 is defined as \code{scale = 1000}) and method with which the
#'  extent is defined (\code{method = }) need to be specified. Currently, two methods
#'  are implemented: `bbox` takes a bounding box while `centroid` calculates the
#'  centroid of the \pkg{sf}  object. Both methods accept \code{x_add}/\code{y_add} with which the
#'  extent window is enlarged. Method \code{centroid} _requires_ \code{x_add}/\code{y_add}, since
#'  the extent windows would have \eqn{\Delta x,\ \Delta y} of Zero.
#'  Todo
#' \enumerate{
#'   \item warning if nlayers != 3 and turn greyscale = T
#'   \item Add colortable options
#'   \item "x/y_add" should have a nicer name.. something with distance maybe?
#'   \item add failsafes: e.g check if "features" is really an \pkg{sf}  object
#'   \item make this a lazy function: at the moment, all raster files are downloaded into memory.
#' }
#' @param features The \pkg{sf}  object to derive the raster data from
#' @param scale_level The scale at which to get raster data (x in 1:1'000x). Usually one of the following values (depending on the available rasters): 10,25,100,500,1000
#' @param x_add,y_add Depending on method, x and y will be added to the centeroid or to the bounding box
#' @param per_feature A TRUE/FALSE value specifying if one raster map should be returend for the entire \pkg{sf}  object or if one map should be returned per feature
#' @param method A character string specifying the method with which the extent should be calculated. \code{centroid} calculates the centroid of the object(s), \code{bbox} calculates the bounding box of the object.
#' @param turn_greyscale Should the output rastermaps be turned into greyscale?
#' @param name If muliple, different maps are available with overlapping extents, \code{name} can be used to differentiate between different maptypes. Default is an empty sting.
#' @param fdir By default, the \code{fdir} is retrieved from the package Environment (named \code{swissrastermapEnv}). Override this with \code{fdir = }
#' @param limit For testing puposes only: Limits the number of rasters returned per object. Defaults to \code{Inf}
#'
get_raster <- function(features,
                       scale_level = NULL,
                       x_add = 0,
                       y_add = 0,
                       method = "bbox",
                       turn_greyscale = F,
                       name = "",
                       fdir = NULL,
                       limit = Inf,
                       asp = NULL,
                       year = NULL,
                       scale_factor = 1
){
  stopifnot("sf" %in% class(features))

  if(is.null(fdir)){
    if(exists("fdir", envir = swissrastermapEnv)){
      fdir <- get("fdir",envir = swissrastermapEnv)
    } else{
      stop("Please run init_fdir() first.")
    }
  }

  ex <- get_extent(features = features,
                   x_add = x_add,
                   y_add = y_add,
                   method = method,
                   per_feature = F,
                   asp = asp
  )

  scale_level <- guess_scale(extent = ex,available_scales = unique(fdir$scale),factor = scale_factor)

  fdir_filtered <- fdir_filter(fdir,
                               epsg = ex$epsg,
                               scale_level = scale_level,
                               xmin = ex$xmin,
                               xmax = ex$xmax,
                               ymin = ex$ymin,
                               ymax = ex$ymax,
                               year = year,
                               name = name
  )

  if(nrow(fdir_filtered) == 0){
    stop("No raster files found with matching criteria.")
  }


  rast <- raster_harmonize(fdir_filtered = fdir_filtered,
                           extent = ex$extent[[1]])

  # Todo: modularize and clean the following code
  if(turn_greyscale){
    if(raster::nlayers(rast) == 3){
      rast <- raster_greyscale(rast)
    } else if(raster::nlayers(rast) == 1){
      coltab <- raster::colortable(raster::raster(fdir_filtered)) # colortable_greyscle can probabbly be subsituted to something more generic
      raster::colortable(rast) <- colortable_greyscale(coltab)
    } else(warning("Unexpected number of Layers"))

  }else{ # if raster should not be turned greyscale
    if(raster::nlayers(rast) == 3){
      # maybe turn into singleband raster with rgb_raster2singleband??
    } else if(raster::nlayers(rast) == 1){
      raster::colortable(rast) <- raster::colortable(raster::brick(fdir_filtered$file[1]))
    } else(warning("Unexpectend number of Layers"))

  }
  rast
}
