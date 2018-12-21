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
#' @param features The \pkg{sf}  object to derive the raster data from
#' @param scale_level The scale at which to get raster data (x in 1:1'000x). Usually one of the following values (depending on the available rasters): 10,25,100,500,1000
#' @param add Depending on method, add will be added to the centeroid or to the bounding box. A numeric value of length 2 (A single value will be recycled)
#' @param method A character string specifying the method with which the extent should be calculated. \code{centroid} calculates the centroid of the object(s), \code{bbox} calculates the bounding box of the object.
#' @param turn_greyscale Should the output rastermaps be turned into greyscale?
#' @param name If muliple, different maps are available with overlapping extents, \code{name} can be used to differentiate between different maptypes. Default is an empty sting.
#' @param limit For testing puposes only: Limits the number of rasters returned per object. Defaults to \code{Inf}
#'
get_raster <- function(features,
                       scale_level = NULL,
                       extent_add = c(0,0),
                       method = "bbox",
                       turn_greyscale = F,
                       name = "",
                       limit = Inf,
                       asp = NULL,
                       year = NULL,
                       scale_factor = 1
){
  stopifnot("sf" %in% class(features))

  if(exists("fdir", envir = swissrastermapEnv)){
    fdir <- get("fdir",envir = swissrastermapEnv)
  } else{
    stop("Please run init_fdir() first.")
  }

  x_add <- extent_add[1]
  y_add <- ifelse(length(extent_add) > 1,extent_add[2],extent_add[1])


  ex <- get_extent(features = features,
                   x_add = x_add,
                   y_add = y_add,
                   method = method,
                   per_feature = F,
                   asp = asp
  )

  if(is.null(scale_level)){
    scale_level <- guess_scale(extent = ex,available_scales = unique(fdir$scale),factor = scale_factor)
  }


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
