
swissrastermapEnv <- new.env()

# move this to data?
data.frame(
  placeholder = c("A","B","C","D","E","F","G","H","Z"),
  name = paste(c("maptype_fn","scale_fn","CRS","format","sheet", "year","index","name","Ignore"),sep = "_"),
  description = c("Name of the map (LK,PK, SMR, TA)",
                  "Scale, usually in two digits",
                  "Projection, usually either LV95 or LV03",
                  "Format of the data (usually KREL or KOMB)",
                  "A character or number specifying the sheet name/number",
                  "An integer, specifying the year of publication (4 digits)",
                  "An index usually at the end of the filename, specifying some sort of map index",
                  "A name to distinguish between different representations of the same map",
                  "Parts of the name that can be ignored"
  ),
  stringsAsFactors = F
) %>%
  assign("search_pattern_dict",.,envir = swissrastermapEnv)

#' Get Metadata from filename
#'
#' Retrieves metadata from filename with the help of a predefined 'pattern'
#'
#' Takes a filename (character string) and retrieves metadata from the filename
#' by consulting a predefined pattern. The whole process is somewhat similar to
#' specifying a format of a datetime object (see \code{strftime}). The pattern is
#' specified by placeholders (in the argument \code{pattern} ), which are in turn
#' specified in a dataframe (in the argument \code{search_pattern_dict}). Run
#' \code{get_srm4r("search_pattern_dict")} to see what these argument currently are.
#'
metainfo_from_filename <- function(filename,pattern){

  this_year <- as.integer(strftime(Sys.Date(),"%Y"))

  filename <- strsplit(filename,"\\.") %>% purrr::map_chr(~.x[1])

  pattern <- filename %>% purrr::map_chr(function(x){
    pattern[nchar(x) == nchar(pattern)]
    })


  search_pattern_dict <- get("search_pattern_dict",envir = swissrastermapEnv) %>%
    dplyr::filter(placeholder != "Z")

  search_pattern_dict %>%
    dplyr::select(placeholder,name) %>%
    # slice(1) %->% c(placeholder,name)
    purrr::pmap_dfr(function(placeholder,name){
      # ints <- gregexpr(placeholder,pattern)[[1]]
      ints <- gregexpr(placeholder,pattern) %>% purrr::map(~c(.x[1],.x[length(.x)]))
      su <- purrr::pmap_chr(list(ints,filename),function(ints,filename){substr(filename,min(ints),max(ints))})
      # su <- substr(filename,min(ints),max(ints))
      data.frame(name = name,
                 val = su,
                 filename = filename,
                 stringsAsFactors = F)
    }) %>%
    tidyr::spread(name,val) %>%
    dplyr::select(-filename) %>%
    dplyr::mutate(
      year = as.integer(year),
      year = ifelse(is.na(year),NA,year),
      sheet = ifelse(sheet == "","0",sheet)
    )
}


#' Make Geometry from boundary
#'
#' Takes a dataframe at returns an \pkg{sf} object.
#'
#' This function takes a dataframe and returns an \pkg{sf} object. The dataframe
#' must contain the columns, \code{xmin}, \code{xmax}, \code{ymin}, \code{max},
#' which will be turned into the edges of a rectangular \pkg{sf} polygon.
#'
#' @param df A dataframe including the columns \code{xmin}, \code{xmax},
#' \code{ymin}, \code{max}
#' @param add A boolean determining whether the geometries should be added to the
#' input dataframe (\code{add = TRUE}) or if only the geometries should be returned
#' (\code{add = FALSE}).
#' @param epsg Intger value of the CRS the data is set to.

geom_from_boundary <- function(df, epsg, add = T){
  # This functions creates a "bounding box"-polygon from 4
  # numeric values. The colnames storing the values must be
  # named xmin, xmax, ymin, ymax.
  # df is a dataframe (or something similar) containing the values
  # add: if set to true, the geometries and the input data will be cbinded
  #      if false, only the geometries will be returned
  # epsg: if not set to null, the epsg code will be set as
  #       the new geom's CRS.

  if(!all(c("xmin","xmax","ymin","xmax") %in% names(df))){
    stop("df must contain all of the following columns:
         xmin, xmax, ymin, ymax")
  }
  if(length(epsg) != 1){stop("Can only add Gemetry if only
                             one EPSG is provided")}

  geo <- df %>%
    dplyr::select(xmin,ymin,xmax,ymax) %>%
    purrr::pmap(function(xmin,ymin,xmax,ymax){
      c(xmin,ymin,xmax,ymin,xmax,ymax,xmin,ymax,xmin,ymin) %>%
        matrix(ncol = 2,byrow = T) %>%
        list() %>%
        sf::st_polygon()
    }) %>%
    do.call(sf::st_sfc,.)

  if(!is.null(epsg)){geo <- sf::st_set_crs(geo,epsg)}

  if(add == T){
    dplyr::mutate(df, geometry = geo) %>%
      sf::st_as_sf()
  }else(geo)

}



raster_metadata <- function(rasterpath){
  purrr::map_dfr(rasterpath,function(rasterpath_i){
    rast <- raster::brick(rasterpath_i)
    reso <- raster::res(rast)
    ex <- matrix(raster::extent(rast))
    data.frame(
      nlayers = raster::nlayers(rast),
      res1 = reso[1],
      res2 = reso[2],
      xmin = ex[1],
      xmax = ex[2],
      ymin = ex[3],
      ymax = ex[4],
      stringsAsFactors = F
    )
  })
}







#' Show extents of available raster Files
#'
#' This function visualizes the extents of all available rasters. The plots are
#' faceted by scale and the area is colorized by resolution. The extents have an
#' alpha value so that overlapping extents can be detected.
#'
#' This function requires ggplot2 and / or tmap, depending on the output type defined
#' in "method".
#'
#' @param method The method with which to visualize the data: \code{ggplot2} or \code{tmap}
#' Wrap the output in \code{ggplotly()} to get an interactive ggplot output or set \code{tmap_mode}
#' to "view" go get an interactive tmap.
#' @param filedirectory The file directory aquired by \code{\link{fdir_init()}}. If left as
#' \code{NULL}, the \code{fdir} from the package Environment is taken.

show_extents <- function(method = "ggplot2",fdir = NULL){

  if(is.null(fdir)){
    fdir <- get("fdir",envir = swissrastermapEnv)

    fdir$res1 <- as.factor(fdir$res1)
  }

  if(!"sf" %in% class(fdir)){
    epsgs <- unique(fdir$epsg)
    epsgs <- epsgs[!is.na(epsgs)]
    fdir <- geom_from_boundary(fdir,epsg = epsgs,add = T)
  }



  if(method == "ggplot2"){
    plotoutput <- ggplot2::ggplot(fdir) +
      ggplot2::geom_sf(mapping = ggplot2::aes(fill = factor(res1)),alpha = 0.4) +
      ggplot2::facet_wrap(~scale) +
      ggplot2::coord_sf(datum = 2056) +
      ggplot2::labs(fill = "Resolution") +
      ggplot2::scale_x_continuous(breaks = seq(25,29,2)*10^5) +
      ggplot2::scale_y_continuous(breaks = seq(11,13,1)*10^5) +
      ggplot2::theme(legend.position = "top",legend.direction = "horizontal")
  }
  if(method == "tmap"){
    plotoutput <- tmap::tm_shape(fdir) +
      tmap::tm_fill(title = "Resolution", col = "res1",alpha = 0.4,group = "extents",palette = "Accent") +
      tmap::tm_borders() +
      tmap::tm_facets(by = "scale")
  }
  return(plotoutput)
}






#' Recalculate extent values from a given aspect ratio
#'
#' Takes four vectors (xmin, xmax, ymin and ymax) and returns the new values based on a given aspect ratio
#' Aspect ratio is calculated as \eqn{\frac{\Delta y\ }{\Delta x\ }}. Values > 1 therefore means portrait,
#' values < 1 means landscape.
#'
#' At the moment, this function can only enlargen the extent in order to match the aspect ratio. At one point,
#' it might be neccessary to add the option to shrink the extent accordingly.
#'
#' @param xmin,xmax,ymin,ymax Numeric Vectors of the same length specifying the x/y extents
#' @param aps Aspect ratio calculated as specified in the description. A single numeric value or a vector in the
#' same length as xmin etc.
#' @param return Returns a dataframe with four columns named \code{xmin}, \code{xmax}, \code{ymin}, \code{ymax}
asp2extent <- function(xmin,xmax,ymin,ymax,asp = 1){
  width = xmax-xmin
  height = ymax-ymin
  asp_calc = height/width
  width_new = ifelse(asp_calc>asp,asp*height,width)
  height_new = ifelse(asp_calc<asp,asp*width,height)
  width_diff = width_new - width
  height_diff = height_new-height
  xmin = xmin-width_diff/2
  xmax = xmax+width_diff/2
  ymin = ymin-height_diff/2
  ymax = ymax+height_diff/2
  data.frame(xmin = xmin,
             xmax = xmax,
             ymin = ymin,
             ymax = ymax,
             stringsAsFactors = F)
}


#' Return String with credits
#'
#' Perticularly swisstopo wants to be credited with the special Copyright character (\u00A9)
#' Since I need to credit the everytime I make a map, i've prepared a function returing
#' that exact text.
#'
#' @param who Character string specifying whom to credit. Currently only "swisstopo" is implemented.
credits <- function(who = "swisstopo"){
  if(who == "swisstopo"){
    "Geodata \u00A9 Swisstopo"
  } else{
    stop("Dont know ",who, "(not implemented)")
  }
}

#' Remove geometry from sf objects
#'
#' Funtion to remove geometry, if the object \emph{is} in fact an sf object. Simply
#' Returns the input object if it isn't. This funtion is only necessary, because
#' \code{st_set_geometry(x,NULL)} returns an error if the input is not sf, and
#' \code{data.frame(x)} keep the geometry as a column.
#'
#' @param sf Anything.
#' @return The input, if it was \emph{not} of class \code{sf}. A \code{data.frame},
#' if it \emph{was}
sf_remove_geom <- function(sf){
  if("sf" %in% class(sf)){
    sf <- sf::st_set_geometry(sf,NULL)
  }
  sf
}
