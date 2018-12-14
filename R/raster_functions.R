# import::from(magrittr, "%>%")

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







#' Count the number of RBG Combinations
#'
#' Count the number of \strong{used} RGB Combinations in a given raster brick object.
#'
#' Given 255 possible values for R, B and G the number of possiblities is \eqn{255^3 = 16581375}
#' But how many are actually used? This function calculates the number of used
#' combinations of R, B and G in a given Raster Brick object.
#' It takes a three band raster, turns all cells into a vector for every band,
#' counts the number of unique combinations and returns a hex colour code
#' for each combination
#' @param param desc
rgb_brick_count <- function(brick,maxColorValue = 255){


  brick %>%
    as.list() %>%
    map(as.vector) %>%
    bind_cols() %>%
    magrittr::set_colnames(c("r","g","b")) %>%
    dplyr::group_by_all() %>%
    count() %>%
    dplyr::mutate(hex = rgb(r,g,b,maxColorValue = maxColorValue))
}


#' Get extent of \pkg{sf}  object
#'
#' Calculate the extent of an \code\{sf} object.
#'
#' gets the centeroid of feature(s), adds the x, and y distances and returns a
#' matrix with x/y min/max plus and extent-object.
#'
#' @param features an object of type sf
#' @param per_feature per feature: should raster be returned PER FEATURE part or for the whole sf object?
#' @param x_add,yadd: the distances from the centeroid, with which the extent window should be drawn
get_extent <- function(features,x_add = 0,y_add = 0,method = "centroid",per_feature = T,asp = NULL){

  epsg_i <- sf::st_crs(features)$epsg

  if(!per_feature){
    features <- features %>%
      dplyr::group_by(1) %>%
      dplyr::summarise()
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


  if(!all(((ext$xmax - ext$xmin) != 0 )& ((ext$ymax - ext$ymin) != 0 ))){
    stop("All extents must be >0")
  }

  if(!is.null(asp)){ext <- asp2extent(ext$xmin,ext$xmax,ext$ymin,ext$ymax,asp = asp)}


  ext %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      extent = list(raster::extent(matrix(c(xmin,xmax,ymin,ymax),nrow = 2,byrow = T))),
      epsg = epsg_i
    ) %>%
    dplyr::ungroup() %>%
    geom_from_boundary(epsg = epsg_i, add = T)

}


#' Filter fdir to find the required raster files
#'
#' Filters an \code{fdir} object to find the required raster files
#' Only used in \code{get_raster()}. Outsorced it to a function in order to
#' modularize \code{get_raster()} and make it easier to debug.
#'
#' @param fdir A dataframe or \pkg{sf} object from \code{fdir_init}
#' @param scale level integer specyfing the scale level of the raster
#' @param xmin,xmax,ymin,ymax Integers specyfing the extent of the desired raster
#' @param year Optional integer specyfing the year from which data is needed
fdir_filter <- function(fdir,epsg,scale_level,xmin,xmax,ymin,ymax,year = NULL){

  fdir <- fdir[fdir$epsg == epsg &
                 fdir$scale == scale_level &
                 fdir$xmin <= xmax &
                 fdir$xmax >= xmin &
                 fdir$ymin <= ymax &
                 fdir$ymax >= ymin,]

  if(!is.null(year)){
    fdir <- fdir[fdir$year_start <= year &
                   fdir$year_end >= year,]
  }
  return(fdir)
}



#' Check Raster Overlaps
#'
#' Takes an \code{fdir} object \strong{with} geometries and checks if any of the
#' features overlap. This function is only in \code{get_raster()} after filtering
#' and before merging.
#'
#' @param fdir_filtered An \code{fdir} dataframe / sf object
#' @param epsg Only necessary if \code{fdir} is not an sf object
check_raster_overlaps <- function(fdir_filtered,epsg = NULL){
  if(!"sf" %in% class(fdir_filtered)){
    fdir_filtered <- geom_from_boundary(fdir_filtered,epsg = epsg,add = T)
  }

  do_intersect <- sf::st_intersects(fdir_filtered,sparse = F)
  do_touch <- sf::st_touches(fdir_filtered,sparse = F)

  # since intersections includes instances where geometries just touch,
  # I'm trying to remove these by applying !st_touches(). Since I dont know
  # the operaions that well, I'm not sure if this can break at some point
  pure_intersection <- do_intersect & !do_touch

  pure_intersection[lower.tri(pure_intersection,diag = T)] <- NA

  if(any(pure_intersection,na.rm = T)){
    # This part just runs if some of the geometries intersect
    pure_intersection %>%
      which(arr.ind = T) %>%
      as.data.frame() %>%
      purrr::pmap_dfr(function(row,col){
        row <- as.integer(row)
        col <- as.integer(col)
        data.frame(
          file1 = fdir_filtered$filename[row],
          file2 = fdir_filtered$filename[col],
          stringsAsFactors = F
        )
      }) %>%
      assign(x = "self_overlaps",value = .,envir = swissrastermapEnv)

    fdir_filtered %>%
      dplyr::group_by(maptype,scale,epsg,sheet) %>%
      dplyr::summarise(years = paste(year,collapse = ",")) %>%
      assign(x = "self_overlaps2",value = .,envir = swissrastermapEnv)

    message(paste(
      "Some of the selected rasters overlap. Run get_srm4r('self_overlaps')",
      "to get a dataframe with all the overlapping objects.",
      "Run get_srm4r('self_overlaps2') to get an overview of all rasters in the extent"
    ))
    stop()
  }
}

#' Get swissmapraster4r Environment data
#'
#' Gets data stored in the package's own environment
#'
#' Simply a wrapper around \code{get()} where \code{envir = } is set to the package's
#' default environment, \code{swissrastermapEnv}.

get_srm4r <- function(x){
  get(x = x, envir = swissrastermapEnv)
}


#' Harmonize Rasters
#'
#' Harmonizes all the raster files from \{code{fdir_filtered}} to one single raster
#' This process includes
#' \enumerate{
#'   \item Crop rasters to right size
#'   \item Checking all resolutions
#'   \item Resample (disaggregate) all rasters low res -> high res (warning issued)
#'   \item Merge rasters into one raster
#' }
raster_harmonize <- function(fdir_filtered,extent){
  res_min <- c(min(fdir_filtered$res1),min(fdir_filtered$res2)) %>%
    round() %>%
    as.integer()

  rast <- fdir_filtered %>%
    dplyr::select(file,res1,res2,epsg,nlayers) %>%
    purrr::pmap(function(file,res1,res2,name,epsg,nlayers){
      raster <- raster::brick(file)
      raster::crs(raster) <- sp::CRS(paste0("+init=EPSG:",epsg))
      raster <- raster::crop(raster,extent)
      res_rast <- as.integer(round(raster::res(raster)))
      if(res_rast[1] > res_min[1] | res_rast[2] > res_min[2]){
        warning(
          paste(
            "Rasters in extent do not have matching resolutions.",
            "Applying disaggregate on the following raster to enable merging:",
            file))
        fac1 <- res_rast[1]/res_min[1]
        fac2 <- res_rast[2]/res_min[2]
        raster <- raster::disaggregate(raster,fact = c(fac1,fac2))
      }
      raster
    }) %>%
    purrr::map(function(x){raster::crop(x,extent)}) %>%
    purrr::accumulate(function(x,y){raster::merge(x,y)}) %>%
    tail(1) %>%
    magrittr::extract2(1)
}






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
                       scale_level,
                       x_add = 0,
                       y_add = 0,
                       method = "centroid",
                       turn_greyscale = F,
                       name = "",
                       fdir = NULL,
                       limit = Inf,
                       asp = NULL,
                       year = NULL
                       ){
  # If needed, features could also be extents or objects with x/y coordinates
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


  fdir_filtered <- fdir_filter(fdir,
                               epsg = ex$epsg,
                               scale_level = scale_level,
                               xmin = ex$xmin,
                               xmax = ex$xmax,
                               ymin = ex$ymin,
                               ymax = ex$ymax,
                               year = year
                               )

  if(nrow(fdir_filtered) == 0){
    stop("No raster files found with matching criteria.")
  }


  if(nrow(fdir_filtered)>1){
    check_raster_overlaps(fdir_filtered,epsg = ex$epsg)
  }

  rast <- raster_harmonize(fdir_filtered = fdir_filtered,
                           extent = ex$extent[[1]])

  if(turn_greyscale){
    if(raster::nlayers(rast) == 3){
      rast <- raster_greyscale(rast)
    } else if(raster::nlayers(rast) == 1){
      coltab <- raster::colortable(raster::raster(fdir_filtered)) # colortable_greyscle can probabbly be subsituted to something more generic
      raster::colortable(rast) <- colortable_greyscale(coltab)
    } else(warning("Unexpectend number of Layers"))

  }else{ # if raster should not be turned greyscale
    if(raster::nlayers(rast) == 3){
      # maybe turn into singleband raster with rgb_raster2singleband??
    } else if(raster::nlayers(rast) == 1){
      raster::colortable(rast) <- raster::colortable(raster::brick(fdir_filtered$file[1]))
    } else(warning("Unexpectend number of Layers"))

  }
  rast
}
