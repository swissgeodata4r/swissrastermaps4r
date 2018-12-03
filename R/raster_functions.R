
import::from(magrittr, "%>%")

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


#' Y_linear to Y_srgb
#'
#' Helperfunction to turn Y_linear values to Y_srgb as described in
#' \link{https://en.wikipedia.org/wiki/Grayscale}
#'
#'@param val The Y_linear Value to be turned into Y_srgb
#'
ysrgb <- function(val){ifelse(val <= 0.0031308,val,1.055*val^(1/2.4)-0.055)}



rgb_grey_ysrgb <- function(raster_in,weights = c(0.2126,0.7152,0.0722)){
  raster_in %>%
    purrr::map2(.,weights,~(.x/255)*.y) %>%
    purrr::reduce(`+`) %>%
    purrr::when(
      class(.) == "RasterLayer" ~raster::calc(.,ysrgb)*255,
      class(.) == "numeric"~ysrgb(.)*255
      )
}

rgb_grey_weighted <- function(raster_in,weights = c(0.30,0.59,0.11)){
  raster_in %>%
    as.list() %>%
    map2(.,weights,~(.x/255)*.y) %>%
    reduce(`+`) %>%
    (function(x){x*255})

}

rgb_grey_mean <- function(raster_in){
  raster_in %>%
    as.list() %>%
    map(~.x/255) %>%
    reduce(`+`)/3 %>%
    (function(x){x*255})
}


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








rgb_brick_count <- function(brick,maxColorValue = 255){
  # takes a three band raster, turns all cells into a vector for every band,
  # counts the number of unique combinations and returns a hex colour code
  # for each combination

  brick %>%
    as.list() %>%
    map(as.vector) %>%
    bind_cols() %>%
    magrittr::set_colnames(c("r","g","b")) %>%
    group_by_all() %>%
    count() %>%
    dplyr::mutate(hex = rgb(r,g,b,maxColorValue = maxColorValue))
}



get_extent <- function(features,x_add,y_add,method = "centroid",per_feature = T){
  # gets the centeroid of feature(s), adds the x, and y distances and returns a
  # matrix with x/y min/max plus and extent-object.

  # features: an object of type sf
  # per feature: should raster be returned PER FEATURE part or for the whole sf object?
  # x_add, yadd: the distances from the centeroid, with which the extent window should be drawn

  if(!per_feature){
    features <- features %>%
      group_by(1) %>%
      summarise()
  }

  ext <- if(method == "centroid"){
    features %>%
      sf::st_geometry() %>%
      sf::st_centroid() %>%
      sf::st_coordinates() %>%
      as.data.frame() %>%
      dplyr::mutate(
        xmin = X-x_add,
        xmax = X+x_add,
        ymin = Y-y_add,
        ymax = Y+y_add
      ) %>%
      dplyr::select(-c(X,Y))
  } else if(method == "bbox"){
    features %>%
      sf::st_geometry() %>%
      purrr::map_dfr(~sf::st_bbox(.x) %>%
                   as.matrix() %>%
                   t() %>%
                   as.data.frame()) %>%
      dplyr::select(xmin,xmax,ymin,ymax) %>%
      dplyr::mutate(
        xmin = xmin-x_add,
        xmax = xmax+x_add,
        ymin = ymin-y_add,
        ymax = ymax+y_add
      )
    } else(
      stop(paste("This method is not defined:",method))
    )
  ext %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      extent = list(raster::extent(matrix(c(xmin,xmax,ymin,ymax),nrow = 2,byrow = T)))
    ) %>%
    dplyr::ungroup() %>%
    geom_from_boundary(add = T,2056)
}


get_raster <- function(features,
                       scale_level,
                       x_add = 0,      # add some default values. either 0 or 100..
                       y_add = 0,        # in case of 0, strange things will happen with method centroid
                       per_feature = F,
                       method = "centroid",
                       epsg = NULL,
                       limit = Inf,
                       turn_greyscale = F,
                       fdir = NULL
){

  # input features to get raster data of
  # scale to get raster data (x in 1:1'000x) from 10,25,100,500,1000
  # file_directory: fdir from init_fdir() (todo: get packageenv fdir if fdir is not supplied)
  # resolution: resolution of the map. this is different per scale, 25 typically has 1.25, 100 typicically has 10
  # per feature: will probabbly be removed once this function has a per_feature wrapper
  # method: "fixed_size_centroid" takes the centeroid of the feature and drawas a fixed window around it with x, and y.
  #         in the future, I'd like to implement bounding_box with a buffer (x,y)
  # x_add, y_add: depending on method, x and y will be added to the centeroid (fixed_size_centroid) or to the bounding box
  # epsg: null if CRS need not be set. Otherwise this is an epsg code wich is turned into a CRS() object using rgdal::CRS
  # limit: in order to test the function, a limit on the number of rows can be set


  # Todo:
  #   - warning if nlayers != 3 and turn greyscale = T
  #   - Add colortable options
  #   - "x/y_add" should have a nicer name.. something with distance maybe?
  #   - implement other was to get the extent (bounding box w/ or w/o buffer)
  #   - add failsafes: e.g check if "features" is really an sf object
  #   - make this a lazy function: at the moment, all raster files are downloaded into memory.
                #Can this be avoided? see which function requires memory loading (probabbly crop or merge)


  # if(method == "centroid" & (x_add == 0 | y_add = 0)){stop("Please add x_add AND y_add values when using method 'centroid'")}


  if(is.null(fdir)){
    if(exists("fdir", envir = packageEnv)){
      fdir <- get("fdir",envir = packageEnv)
    } else{
      stop("Please run init_fdir() first.")
    }
  }


  ex <- get_extent(features = features,
                   x_add = x_add,
                   y_add = y_add,
                   method = method,
                   per_feature = per_feature
                   )

  if(!all(((ex$xmax - ex$xmin) != 0 )& ((ex$ymax - ex$ymin) != 0 ))){stop("All extents must be >0")}


  ex %>%
    sf::st_set_geometry(NULL) %>%
    head(limit) %>%
    purrr::pmap(function(xmin_i,xmax_i,ymin_i,ymax_i,extent_i){
      rast_file <- fdir %>%
        data.frame(stringsAsFactors = F) %>%
        dplyr::filter(scale == scale_level) %>%
        dplyr::filter(xmin <= xmax_i & xmax >= xmin_i) %>%
        dplyr::filter(ymin <= ymax_i & ymax >= ymin_i) %>%
        dplyr::select(file,res1,res2) #%>% dplyr::pull()

      res_min <- c(min(rast_file$res1),min(rast_file$res2))



      rast <- rast_file %>%
        purrr::pmap(function(file,res1,res2){
          raster <- raster::brick(file)
          if(!is.null(epsg)){raster::crs(raster) <- sp::CRS(paste0("+init=EPSG:",epsg))}
          # raster
          raster <- raster::crop(raster,extent_i)
          res_rast <- raster::res(raster)
          if(res_rast[1] > res_min[1] | res_rast[2] > res_min[2]){
            warning("Rasters in Extent do not have matching resolutions. Using disaggregate in order to enable merging")
            fac1 <- res_rast[1]/res_min[1]
            fac2 <- res_rast[2]/res_min[2]
            raster <- raster::disaggregate(raster,fact = c(fac1,fac2))

          }
          raster
        }) %>%
        purrr::map(function(x){raster::crop(x,extent_i)}) %>%
        purrr::accumulate(function(x,y){raster::merge(x,y)}) %>%
        tail(1) %>%
        magrittr::extract2(1)

      if(turn_greyscale){
        if(raster::nlayers(rast) == 3){
          rast <- raster_greyscale(rast)
        } else if(raster::nlayers(rast) == 1){
          coltab <- colortable(raster::raster(rast_file)) # colortable_greyscle can probabbly be subsituted to something more generic
          raster::colortable(rast) <- colortable_greyscale(coltab)
        } else(warning("Unexpectend number of Layers"))

      }else{ # if raster should not be turned greyscale
        if(raster::nlayers(rast) == 3){
          # maybe turn into singleband raster with rgb_raster2singleband??
        } else if(raster::nlayers(rast) == 1){
          raster::colortable(rast) <- raster::colortable(raster::brick(rast_file$file[1]))
        } else(warning("Unexpectend number of Layers"))

      }
      rast
    })%>%
    purrr::when(
      length(.) == 1~magrittr::extract2(.,1),
      TRUE~.
    )
}
