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
#'   \item implement other was to get the extent (bounding box w/ or w/o buffer)
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
#' @param fdir By default, the \code{fdir} is retrieved from the package Environment (named \code{swissmaprasterEnv}). Override this with \code{fdir = }
#' @param limit For testing puposes only: Limits the number of rasters returned per object. Defaults to \code{Inf}
#'
get_raster <- function(features,
                       scale_level,
                       x_add = 0,
                       y_add = 0,
                       per_feature = F,
                       method = "centroid",
                       turn_greyscale = F,
                       name = "",
                       fdir = NULL,
                       limit = Inf,
                       asp = NULL,
                       year = NULL
){







  if(is.null(fdir)){
    if(exists("fdir", envir = swissmaprasterEnv)){
      fdir <- get("fdir",envir = swissmaprasterEnv)
    } else{
      stop("Please run init_fdir() first.")
    }
  }



  ex <- get_extent(features = features,
                   x_add = x_add,
                   y_add = y_add,
                   method = method,
                   per_feature = per_feature,
                   asp = asp
  )



  name_i <- name # to avoid conflicts with columns of the same name
  year_i <- year # to avoid confusion with the column name

  # get_raster_from_ex <- function(extent,fdir,epsg,scale,year = NULL){
  #
  # }

  # library(zeallot)

  ex %>%
    sf::st_set_geometry(NULL) %>%
    # dplyr::slice(1) %->% c(xmin_i,xmax_i,ymin_i,ymax_i,extent_i,epsg_i)
    head(limit)  %>%
    purrr::pmap(function(xmin_i,xmax_i,ymin_i,ymax_i,extent_i,epsg_i){



      rast_file <- fdir %>%
        data.frame(stringsAsFactors = F) %>%
        dplyr::filter(epsg == epsg_i) %>%
        dplyr::filter(scale == scale_level) %>%
        dplyr::filter(xmin <= xmax_i & xmax >= xmin_i) %>%
        dplyr::filter(ymin <= ymax_i & ymax >= ymin_i) %>%
        # dplyr::filter(name == name_i) %>%
        purrr::when(
          !is.null(year_i)~dplyr::filter(.,fn_year_start <= year_i,fn_year_end >= year_i),
          TRUE~.
        )

      if(nrow(rast_file) == 0){
        stop("No raster files found with matching criteria.")}


      if(nrow(rast_file)>1){
        geoms <- geom_from_boundary(rast_file,epsg = epsg_i,add = T)

        # cover <- sf::st_covers(geoms,sparse = F)
        # overl <- sf::st_overlaps(geoms,sparse = F)

        inters <- sf::st_intersects(geoms,sparse = F)
        touch <- sf::st_touches(geoms,sparse = F)

        # since intersections includes instances where geometries just touch,
        # I'm trying to remove these by applying !st_touches(). Since I dont know
        # the operaions that well, I'm not sure if this can break at some point
        pure_intersection <- inters & !touch

        pure_intersection[lower.tri(pure_intersection,diag = T)] <- NA

        # rast_file_intersect <- sf::st_intersects(geoms,sparse = F)


        if(any(pure_intersection,na.rm = T)){
          rast_file_intersect_message <- pure_intersection %>%
            which(arr.ind = T) %>%
            head(1) %>%
            as.data.frame() %>%
            purrr::pmap_chr(function(row,col){
              row <- as.integer(row)
              col <- as.integer(col)
              paste(rast_file$filename[row],"INTERSECTS",rast_file$filename[col])
            }) %>%
            paste(collapse = "\n")

          rast_file_years <- rast_file %>%
            dplyr::group_by(maptype,scale,epsg,fn_sheet) %>%
            dplyr::summarise(years = paste(fn_year,collapse = ","))

          # rast_file_years <- paste(sort(unique(rast_file$fn_year)),collapse = ",")


          message(paste(
            "Some rasters overlapping (e.g. :",
            rast_file_intersect_message,") \n",
            "Maybe multiple years? Printing sheet-years dataframe\n"
          ))
          message(paste0(capture.output(rast_file_years), collapse = "\n"))
          stop()



        }

      }


      # check if some geometries are self overlapping. Stop the function if TRUE

      res_min <- c(min(rast_file$res1),min(rast_file$res2))
      res_min <- as.integer(round(res_min))



      rast <- rast_file %>%
        dplyr::select(file,res1,res2,epsg,nlayers) %>%
        purrr::pmap(function(file,res1,res2,name,epsg,nlayers){
          raster <- raster::brick(file)
          raster::crs(raster) <- sp::CRS(paste0("+init=EPSG:",epsg))
          # raster
          raster <- raster::crop(raster,extent_i)
          res_rast <- raster::res(raster)
          res_rast <- as.integer(round(res_rast))
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
