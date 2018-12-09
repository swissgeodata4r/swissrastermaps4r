
swissmaprasterEnv <- new.env()


pattern_keywords <- data.frame(
  keyword = c("A","B","C","D","E","F"),
  name = paste("fn",c("maptype","scale","CRS","format","sheet", "year"),sep = "_"),
  stringsAsFactors = F
)

#' Get Metadata from filename
#'
#' Retrieves metadata from filename through a defined pattern
#'
#' Takes a filename (character string) and retrieves metadata from the filename
#' by consulting a defined pattern. The whole process is somewhat similar to
#' specifying a format of a datetime object (see \code{strftime}). The pattern is
#' specified by placeholders, which are in turn specified in a dataframe.
#'
metainfo_from_filename <- function(filename,pattern){
  filename %>% purrr::map_dfr(function(filename_i){
    pattern_keywords %>% purrr::pmap_dfr(function(keyword,name){
      ints <- gregexpr(keyword,pattern)[[1]]
      star <- min(ints)
      sto <- max(ints)
      su <- substr(filename_i,star,sto)
      data.frame(name = name,
                 val = su,
                 filename = filename_i,
                 stringsAsFactors = F)
    }) %>%
      tidyr::spread(name,val)
  })
}

geom_from_boundary <- function(df, add = T, epsg = NULL){
  # This functions creates a "bounding box"-polygon from 4
  # numeric values. The colnames storing the values must be
  # named xmin, xmax, ymin, ymax.
  # df is a dataframe (or something similar) containing the values
  # add: if set to true, the geometries and the input data will be cbinded
  #      if false, only the geometries will be returned
  # epsg: if not set to null, the epsg code will be set as
  #       the new geom's CRS.

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


#' Initialize File Directory
#'
#' Scans all folders in the root dirctory corresponding to the folders given by
#' \code{folders = }.
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
#' @param rootdir Character string specifying the directory where the folders are stored
#' @param maxfiles Integer limiting the number of files to be scanned. For testing purposes only.
#' @param add_geometry Should the bounding box of each file be added as a geometry to \code{fdir}?
#' @param maptypes Names of the maptypes to look for. Only folders containing at least one of the
#' the character strings noted here are included in the search.

init_fdir <- function(rootdir,
                      maxfiles = Inf,
                      # add_geometry = T,
                      maptypes = c("PK","SMR","LK","TA")
                      ){

  dirs <- list.dirs(rootdir,recursive = F,full.names = F)
  dirs <- purrr::map(maptypes,~dirs[grepl(.x,dirs)]) %>% unlist()


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

  # Creates a data_frame by going through all the maptypes and their
  # corresponding folders, reading in all raster files (only "tifs" at the )
  fdir <- folders_df %>%
    # head(1) %->% c(maptype,scale,epsg,folder) # use only to debugg pmap_dfr()
    purrr::pmap_dfr(function(maptype,scale,epsg,name,folder){
      folderpath <- file.path(rootdir,folder)
      pattern <- list.files(folderpath,".pattern",full.names = F)
      if(length(pattern)>1){
        warning("Fround more than one file with the ending '.pattern'. Using only first one")
        } else if(length(pattern) == 1){
          pattern <- strsplit(pattern,"\\.")[[1]][1]
        } else(
          pattern <- ""
        )
      list.files(folderpath,".tif$",full.names = F) %>%
        # head(1) -> x
        head(maxfiles) %>% # this can be used to test and debug the function
        purrr::map_dfr(function(x){
          folderfile <- file.path(folderpath,x)
          raster_i <- raster::brick(folderfile)
          size_mb <- file.info(folderfile)$size/1e+6
          n_layers <- raster::nlayers(raster_i)
          reso <- raster::res(raster_i)
          name_dissection <- metainfo_from_filename(x,pattern)

          raster_i %>%
            raster::extent() %>%
            matrix(nrow = 1) %>%
            as.data.frame() %>%
            magrittr::set_colnames(c("xmin","xmax","ymin","ymax")) %>%
            dplyr::mutate(
              maptype = maptype,
              scale = scale,
              epsg = epsg,
              file = folderfile,
              nlayers = n_layers,
              res1 = reso[1],
              res2 = reso[2],
              size_mb = size_mb
            ) %>%
            cbind(name_dissection)
        })
    }) #%>% dplyr::filter(epsg == 2056) #. in this stage, only epsg 2056 is supported. A way to handle other EPSG should at one point be implemented

  # if(add_geometry){
  #   fdir <- geom_from_boundary(fdir, add = T,2056)
  # }
  assign("fdir",fdir,envir = swissmaprasterEnv)
  mb <- format(sum(fdir$size_mb),big.mark = "'")
  print(paste0("Done. Scanned ",nrow(fdir), " Files", " (",mb," MB)."," All metadata stored in fdir."))
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
#' @param filedirectory The file directory aquired by \code{\link{init_fdir()}}. If left as
#' \code{NULL}, the \code{fdir} from the package Environment is taken.

show_extents <- function(method = "ggplot2",fdir = NULL){
  stop("currently not working since I'm implementing a way to handle different CRS'")

  if(is.null(fdir)){
    fdir <- get("fdir",envir = swissmaprasterEnv)

    fdir$res1 <- as.factor(fdir$res1)
  }



  if(method == "ggplot2"){
    plotoutput <- ggplot2::ggplot(x) +
      ggplot2::geom_sf(mapping = aes(fill = factor(res1)),alpha = 0.4) +
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


credits <- function(who){
  if(who == "swisstopo"){
  "Geodata \u00A9 Swisstopo"
  }
}
