#' Threeband RGB to singleband GREYSCALE raster
#'
#' \code{rgb_raster2singleband} turns a threeband RGB Raster (\code{\link{brick}})
#' into a singleband GREYSCALE \code{\link{raster}} with a \code{\link{colortable}}
#'
#' @param raster_in An object of class RasterBrick.
#' @param method The method to use when turning RGB to greyscale.
raster_greyscale <- function(raster_in, method = "ysrgb"){

  stopifnot(raster::nlayers(raster_in) == 3)

  raster_in <- list(raster_in[[1]],raster_in[[2]],raster_in[[3]])

  if(method == "ysrgb"){
    converted <- rgb_grey_ysrgb(raster_in)
  } else if(method == "mean"){
    converted <- rgb_grey_mean(raster_in)
  } else if(method == "weighted"){
    converted <- rgb_grey_weighted(raster_in)
  } else{
    stop(paste0("This method is not defined: ",method))
  }

  vals <- 0:255
  coltab <- rgb(vals,vals,vals,maxColorValue = 255)
  raster::colortable(converted) <- coltab
  converted
}


#' Threeband RGB to singleband raster
#'
#' \code{rgb_raster2singleband} turns a threeband RGB Raster (\code{\link{brick}})
#' into a singleband \code{\link{raster}} with a \code{\link{colortable}}
#'
#' This function turns a three band RGB Raster Brick (from the Package \code{\link{raster}})
#' into a singleband raster with an included \code{\link{colortable}}. The function
#' requires the packages:  \code{\link{raster}}, \code{\link{rgdal}}
#'
#' @param rgb_raster A character string pointing to a raster brick object or
#' an object of class RasterBrick. RBG values are assumed to range from 0-255
rgb_raster2singleband <- function(rgb_raster){
  if(class(rgb_raster) == "character"){
    rgb_raster <- raster::brick(rgb_raster)
  }
  stopifnot(class(rgb_raster) == "RasterBrick")
  stopifnot(raster::nbands(rgb_raster) == 3)

  pct <- rgdal::SGDF2PCT(as(rgb_raster, "SpatialGridDataFrame"))

  r <- setValues(raster::raster(rgb_raster), pct$idx-1)

  colortable(r) <- pct$ct
  r

}


#' Colortable of singleband Raster into Greyscale
#'
#' \code{rgb_raster2singleband} turns a threeband RGB Raster (\code{\link{brick}})
#' into a singleband GREYSCALE \code{\link{raster}} with a \code{\link{colortable}}
#'
#' @param raster_in An object of class RasterBrick.
#' @param method The method to use when turning RGB to greyscale.
colortable_greyscale <- function(colortable, method = "ysrgb"){

  colortable <- col2rgb(colortable)
  toconvert <- list(colortable[1,],colortable[2,],colortable[2,])

  if(method == "ysrgb"){
    converted <- rgb_grey_ysrgb(toconvert)
  } else if(method == "mean"){
    converted <- rgb_grey_mean(toconvert)
  } else if(method == "weighted"){
    converted <- rgb_grey_weighted(toconvert)
  } else{
    stop(paste0("This method is not defined: ",method))
  }
  rgb(converted,converted,converted,maxColorValue =255)

}



#' Y_linear to Y_srgb
#'
#' Helperfunction to turn Y_linear values to Y_srgb as described in
#' \link{https://en.wikipedia.org/wiki/Grayscale}
#'
#'@param val The Y_linear Value to be turned into Y_srgb
#'
ysrgb <- function(val){ifelse(val <= 0.0031308,val,1.055*val^(1/2.4)-0.055)}



#' Name
#'
#' Short Desc
#'
#' Long Desc
#'
#' @param param desc
rgb_grey_ysrgb <- function(raster_in,weights = c(0.2126,0.7152,0.0722)){
  raster_in %>%
    purrr::map2(.,weights,~(.x/255)*.y) %>%
    purrr::reduce(`+`) %>%
    purrr::when(
      class(.) == "RasterLayer" ~raster::calc(.,ysrgb)*255,
      class(.) == "numeric"~ysrgb(.)*255
    )
}

#' Name
#'
#' Short Desc
#'
#' Long Desc
#'
#' @param param desc
rgb_grey_weighted <- function(raster_in,weights = c(0.30,0.59,0.11)){
  raster_in %>%
    as.list() %>%
    map2(.,weights,~(.x/255)*.y) %>%
    reduce(`+`) %>%
    (function(x){x*255})

}

#' Name
#'
#' Short Desc
#'
#' Long Desc
#'
#' @param param desc
rgb_grey_mean <- function(raster_in){
  raster_in %>%
    as.list() %>%
    map(~.x/255) %>%
    reduce(`+`)/3 %>%
    (function(x){x*255})
}

