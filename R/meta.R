
swissrastermapEnv <- new.env()


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


#' Prints the search pattern nomenklature
#'
#' Prints the search pattern nomenklature
#'
#' Retrieves the \code{search_pattern_dict} which is used in the
#' function \code{metainfo_from_filename}. Any characters not specified in
#' \code{search_pattern_dict} will be ignored in the '.pattern' file.
#'

search_pattern <- function(){
  search_pattern_dict <- get("search_pattern_dict",envir = swissrastermapEnv)
  print(search_pattern_dict)
}

#' Get Metadata from filename
#'
#' Retrieves metadata from filename with the help of a predefined 'pattern'
#'
#' Takes a filename (character string) and retrieves metadata from the filename
#' by consulting a predefined pattern. The whole process is somewhat similar to
#' specifying a format of a datetime object (see \code{strftime}). The pattern is
#' specified by placeholders (in the argument \code{pattern} ), which are in turn
#' specified in a dataframe (in the argument \code{search_pattern_dict}). Run \code{search_pattern}
#' to see what these argument currently are.
#'
metainfo_from_filename <- function(filename,pattern){

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
      year = ifelse(is.na(year),Inf,year),
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


#' Initialize File Directory
#'
#' Scans all folders in the root dirctory corresponding to the folders given by
#' \code{folders = }. OR: Makes an existing fdir object (either a variable or
#' an .Rda File) the current fdir file by writing it to the Environment
#' 'swissrastermapEnv'. If stored as an Rda File, the associated variable name
#' must be \code{fdir}
#'
#' This command creates a "File Directory" in the package environment by scanning
#' all folders specified by \code{folders} within the \code{rootdir} and analyzing the content.
#' All files ending with "tif" are checked for extent, number of layers and resolution.
#' All the mentioned attributes of each raster file, along with the file path and the extent as a
#' geometry, are stoerd in the variable \code{fdir} of the package environment.
#' Parent folders containing the rasterdata require a specific structure:
#' \code{TYPE_Scale_EPSG_index}
#' \describe{
#'   \item{\code{TYPE}}{corresponds to the maptype. IN Switzerland this is typically \code{SMR}, \code{PK}, \code{LK} or \code{TA}. \strong{Must} be followed by a \code{_}}
#'   \item{\code{Scale}}{defines the map scale as x in 1:1'000x. \strong{Must} be followed by a \code{_}}
#'   \item{\code{EPSG}}{specifies the CRS of the containing raster data \strong{Must} be followed by a \code{_} \strong{only} if \code{name} is specified}
#'   \item{\code{index}}{Needed when multiple folders with same maptype, scale and epsg code exist, but different naming patterns.}
#' }
#' @param rootdir Character string specifying the directory where the folders are stored OR
#' character string pointing to an .Rda file from a previous fdir_init() run OR an fdir variable.
#' @param maxfiles Integer limiting the number of files to be scanned. For testing purposes only.
#' @param add_geometry Should the bounding box of each file be added as a geometry to \code{fdir}?
#' @param filter Names of the folders to look for. Only folders containing the character strings
#' specified here are included in the search.

fdir_init <- function(rootdir,
                      maxfiles = Inf,
                      add_geometry = T,
                      filter = c("PK","SMR","LK","TA")
){

  start <- Sys.time()

  if(maxfiles != Inf){warning("maxfiles currently not implemented")}


  dirs <- list.dirs(rootdir,recursive = F,full.names = F)
  dirs <- purrr::map(filter,~dirs[grepl(.x,dirs)]) %>% unlist()

  folders_df <- strsplit(dirs,"_") %>%
    purrr::map_dfr(function(x){
      data.frame(
        maptype = x[1],
        scale = as.integer(x[2]),
        epsg = as.integer(x[3]),
        stringsAsFactors = F
      )
    }) %>%
    dplyr::mutate(
      folder = dirs
    )
  # library(zeallot)
  fdir <- folders_df %>%
    dplyr::mutate(folderpath = file.path(rootdir,folder)) %>%
    # slice(1) %->% c(maptype,scale,epsg,folder,folderpath) # use only to debugg pmap_dfr()
    purrr::pmap_dfr(function(maptype,scale,epsg,folder,folderpath){
      pattern <- list.files(folderpath,".pattern",full.names = F)
      pattern <- strsplit(pattern,"\\.") %>% purrr::map_chr(~.x[1])
      out <- data.frame(
        file = list.files(folderpath,".tif$",full.names = T),
        stringsAsFactors = F)
      out$size_mb <- file.info(out$file)$size/1e+6
      out$epsg <- epsg
      out$maptype <- maptype
      out$scale <- scale

      out <- cbind(out,raster_metadata(out$file))

      filename <- list.files(folderpath,".tif$",full.names = F)
      filename <- gsub(".tif","",filename)

      out <- cbind(out,metainfo_from_filename(filename,pattern))
      out
    })

  fdir <- fdir %>%
    dplyr::group_by(epsg,sheet) %>%
    dplyr::arrange(epsg,sheet,year) %>%
    dplyr::mutate(
      year_start = year - floor((year-dplyr::lag(year))/2),
      year_start = ifelse(is.na(year_start),-Inf,year_start),
      year_end = (year + ceiling((dplyr::lead(year)-year)/2))-1,
      year_end = ifelse(is.na(year_end),Inf,year_end),
    ) %>%
    dplyr::ungroup()


  epsgs <- unique(fdir$epsg)
  epsgs <- epsgs[!is.na(epsgs)]

  if(add_geometry){
    fdir <- geom_from_boundary(fdir, epsgs, add = T)
  }


  assign("fdir",fdir,envir = swissrastermapEnv)
  mb <- format(sum(fdir$size_mb),big.mark = "'")
  duration <- difftime(Sys.time(),start)
  duration_units <- attr(duration,"units")
  duration <- duration%>%
    as.numeric() %>%
    round(2) %>%
    format(nsmall = 2)

  print(paste0("Done. Scanned ",nrow(fdir), " Files", " (",mb," MB) in ",duration," (",duration_units,"). -> All metadata stored in fdir."))
}





#' Show extents of Available Raste Files
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

#' Import an existing fdir
#'
#' Avoid rescanning all your files (\code{fdir_init}) by
#' exporting an existing \{fdir} (with \code{fdir_export})
#' after initializing and importing it again in the next session
#'
#' The function enables importing an existing "File Directory"
#' (\code{fdir}) from an ".Rda" File after running \code{fdir_init}
#' and \code{fdir_export}.
#' This avoids having to rescan all the files with \code{fdir_init}.
#' Handle with care, changes in the source files are not registered with
#' this method. Use \code{fdir_init} if unsure.

#' @param name Path to an .Rda File
fdir_import <- function(path){
  fdir <- get(load(path))
  assign("fdir",fdir,envir = swissrastermapEnv)
}

#' Export an existing fdir to an Rda-File
#'
#' Avoid rescanning all your files (\code{fdir_init}) by
#' exporting an existing \{fdir} (with \code{fdir_export})
#' after initializing and importing it again (with \code{fdir_import})
#' in the next session
#'
#' The function enables exporting an existing "File Directory"
#' (\code{fdir}) to an ".Rda" File after running \code{fdir_init}.
#' This avoids having to rescan all the files with \code{fdir_init}.
#' Handle with care, changes in the source files are not registered with
#' this method. Use \code{fdir_init} if unsure.

fdir_export <- function(path){
  if(exists("fdir", envir = swissrastermapEnv)){
    fdir <- get("fdir",envir = swissrastermapEnv)
    save(fdir,file = path)
  } else{
    stop("Please run fdir_init() first.")
  }
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


credits <- function(who){
  if(who == "swisstopo"){
    "Geodata \u00A9 Swisstopo"
  }
}
