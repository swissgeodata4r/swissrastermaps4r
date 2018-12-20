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
      sf_remove_geom() %>% # swiched from "as.data.frame()"
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
fdir_filter <- function(fdir,epsg,scale_level,xmin,xmax,ymin,ymax,year = NULL,name = ""){

  fdir_filtered <- fdir[fdir$epsg == epsg &
         fdir$scale == scale_level,]

  if(!is.null(year)){
    fdir_filtered <- fdir_filtered[fdir_filtered$year_start <= year &
                                     fdir_filtered$year_end >= year,]
  }

  if(nrow(fdir_filtered) < 1){
    stop("No dataset matching this criteria")
  } else if(nrow(fdir_filtered) > 1){
    warning("More than one dataset matching this criteria. Using the newest one")
  }

  fdir_filtered <- fdir_filtered %>%
    mutate(
      data = map(data,function(x){x %>%   # this is only neccessary because
          mutate(epsg = epsg,             # unnest does not work with sf()
                 maptype = maptype,
                 scale = scale,
                 nlayers = nlayers
          )})
    ) %>%
    arrange(year_end) %>%
    pull(data) %>%
    magrittr::extract2(1)


  fdir_filtered <- fdir_filtered[fdir_filtered$xmin <= xmax &
                                   fdir_filtered$xmax >= xmin &
                                   fdir_filtered$ymin <= ymax &
                                   fdir_filtered$ymax >= ymin, ]

  if(!is.null(year)){
    fdir_filtered <- fdir_filtered[fdir_filtered$year_start <= year &
                                     fdir_filtered$year_end >= year,]
  }

  if(name != ""){
    fdir_filtered <- fdir_filtered[fdir_filtered$name == name,]
  }

  if(nrow(fdir_filtered) == 0){
    stop("No raster files found with matching criteria.")
  }

  pure_overlaps <- check_raster_overlaps(fdir_filtered,epsg = epsg)

  if(any(isTRUE(pure_overlaps))){
    stop("Some rasters overlapping. Check data integrity")
  }

  # if(any(isTRUE(pure_overlaps))){
  #   message("Selected rasters overlap at least partially. Attempting filter further:")
  #   years <- unique(fdir$year)
  #   if(length(years)>1){
  #     year = max(years,na.rm = T)
  #     years <- paste(years,collapse = ",")
  #     message(paste0("-- Multiple years found. Only using year: ",year, " (Available years: ",years,")"))
  #     fdir <- fdir[fdir$year_start <= year &
  #                    fdir$year_end >= year,]
  #   }
  #   names <- unique(fdir$name)
  #   if(length(names)>1){
  #     name <- names[1]
  #     names <- paste(names,collapse = ",")
  #     message(paste("-- Multiple names found. Only using name: ",name, " (Available names: ",names,")"))
  #
  #     fdir <- fdir[fdir$name == name,]
  #   }
  # }

  # pure_overlaps <- check_raster_overlaps(fdir,epsg = epsg)


  return(fdir_filtered)
}



#' Check Raster Overlaps
#'
#' Takes an \code{fdir} object \strong{with} geometries and checks if any of the
#' features overlap. This function is only in \code{get_raster()} after filtering
#' and before merging.
#'
#' @param fdir_filtered An \code{fdir} dataframe / sf object
#' @param epsg Only necessary if \code{fdir} is not an sf object
check_raster_overlaps <- function(fdir_filtered,epsg = NULL,action = "stop"){
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

  pure_intersection
}

#' Get swissmapraster4r Environment data
#'
#' Gets data stored in the package's own environment
#'
#' Simply a wrapper around \code{get()} where \code{envir = } is set to the package's
#' default environment, \code{swissrastermapEnv}.
#'  \describe{
#'   \item{search_pattern_dict}{See \{metainfo_from_filename()}}
#'   \item{fdir}{See \code{fdir_init()}}
#' }
#'
#' @param A character string specifying the object to retrieve from the environment.

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

  size_sum <- sum(fdir_filtered$size_mb)

  message("Collective Size of Rasters to harmonize: ",size_sum," (mb)")

  threshold <- 500
  if(size_sum>threshold){
    stop("Size is larger than Limit. Aborting.") # todo: make this an option (like tm_option())
  }

  rast <- fdir_filtered %>%
    sf_remove_geom() %>%
    dplyr::select(file,res1,res2,epsg,nlayers) %>%
    purrr::pmap(function(file,res1,res2,name,epsg,nlayers){
      raster <- raster::brick(file)
      raster::crs(raster) <- sp::CRS(paste0("+init=EPSG:",epsg))
      raster <- raster::crop(raster,extent)
      res_rast <- as.integer(round(raster::res(raster)))
      if(res_rast[1] > res_min[1] | res_rast[2] > res_min[2]){
        warning(
            "Rasters in extent do not have matching resolutions.",
            "Applying disaggregate on the following raster to enable merging: ",
            file)
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



#' Guess scale
#'
#' Guess map scale based on the extent and the output window
#'
#' This function takes an extent object (with \code{xmin}, \code{xmax},
#' \code{ymin} and \code{ymax} values)
#' as well as a set of available scales to determin the best suited scale.
#' The extent object (\code{xmin}, \code{xmax}, \code{ymin} and \code{ymax} values)
#' are expected in Meters while the available scales are expected to be the the
#' x in 1:1'000x (e.g. 25 in 1:25'000).
#'
#' @param extent A dataframe containing \code{xmin}, \code{xmax}, \code{ymin} and
#' \code{ymax} values
#' @param available_cales An integer vector containing the available scales in the
#' format x in 1:1'000x (e.g. 25 in 1:25'000).
#' @param factor Usually, the scale is overestimated and smaller scales are desired.
#' This factor enables correction by dividing the calculated scale by the factor.
#' (e.g. a \code{factor = 2} returns a 1:25'000 scale when 1:50'000 has been calculated)
#' @param outsize An integer vector of the dimensions of the output window size in cm.
#' If left at \code{NULL} (default), the output window size is determined by
#' \code{dev.size("cm")}
guess_scale <- function(extent,available_scales,factor = 1,outsize = NULL){

  if(is.null(outsize)){outsize <- dev.size("cm")}
  outsize_real <- c(extent$xmax-extent$xmin,extent$ymax-extent$ymin)*100
  scale_real <- (mean((outsize_real/outsize)/1000))/factor
  scale_closest <- available_scales[which.min(abs(scale_real-available_scales))]
  message(paste("Using scale level: ",scale_closest))
  scale_closest
}


