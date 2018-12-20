#' Initialize a "File Directory" by scanning all raster files
#'
#' This function creates a "file directory" by scanning all available raster files
#' and extracting relevant metadata.
#'
#' This function scans all folders in the root dirctory corresponding to the folders given by
#' \code{folders = }. It saves this directory in the package environment, and and
#' can later use this info to determin which rasters to import when running the
#' command \code{get_raster()}.
#'
#' Currently, only files ending with "tif" are checked for extent, number of layers
#' and resolution and other informatin.
#'
#' Parent folders (subfolders of \code{rootdir =}) are scanned non recursively for
#' raster data with the extention ".tif". These folders must adhere a specific structure:
#' \code{TYPE_Scale_EPSG_index}
#' \describe{
#'   \item{\code{TYPE}}{corresponds to the maptype. IN Switzerland this is typically \code{SMR}, \code{PK}, \code{LK} or \code{TA}. \strong{Must} be followed by a \code{_}}
#'   \item{\code{Scale}}{defines the map scale as x in 1:1'000x. \strong{Must} be followed by a \code{_}}
#'   \item{\code{EPSG}}{specifies the CRS of the containing raster data \strong{Must} be followed by a \code{_} \strong{only} if \code{name} is specified}
#'   \item{\code{index}}{Optional}
#' }
#' @param rootdir Character string specifying the directory where the folders are stored
#' @param add_geometry Should the bounding box of each file be added as a geometry to \code{fdir}?
#' @param filter Names of the folders to look for. Only folders containing the character strings
#' specified here are included in the search.

fdir_init <- function(rootdir,
                      add_geometry = T,
                      filter = c("PK","SMR","LK","TA")
){

  start <- Sys.time()

  if(is.null(year)){year <- as.integer(strftime(Sys.Date(),"%Y"))}



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

  fdir <-  fdir %>%
    group_by(epsg,maptype,scale,nlayers) %>%
    nest() %>%
    group_by(epsg,maptype,scale,nlayers) %>%
    mutate(
      data = map(data,~geom_from_boundary(.x,epsg)),
      year_start = map_int(data,~min(.x$year)),
      year_end = map_int(data,~max(.x$year))
    )


  fdir <- fdir %>%
    mutate(
      data = map(data,function(x){
        x %>%
          dplyr::arrange(sheet,year) %>%
          dplyr::mutate(
            year_start = year - floor((year-dplyr::lag(year))/2),
            year_start = ifelse(is.na(year_start),-Inf,year_start),
            year_end = (year + ceiling((dplyr::lead(year)-year)/2))-1,
            year_end = ifelse(is.na(year_end),Inf,year_end),
          )
      })
    )






  # epsgs <- unique(fdir$epsg)
  # epsgs <- epsgs[!is.na(epsgs)]

  # if(add_geometry){
  #   if(length(epsgs) == 1){
  #     fdir <- geom_from_boundary(fdir, epsgs, add = T)
  #   } else if(length(epsgs) > 1){
  #     warning("Multiple EPSG Codes found (",paste(epsgs,collapse = ","),").
  #             Can only add geometry if fdir contains rasterfiles of a single CRS")
  #   } else if(length(epsgs) == 0){
  #     warning("No EPSG Codes found in fdir. Cant add geometry")
  #   }
  # }


  assign("fdir",fdir,envir = swissrastermapEnv)
  mb <-map_dbl(fdir$data,~.x$size_mb %>% sum()) %>% sum() %>% format(big.mark = "'")
  n_files <- map_int(fdir$data,nrow) %>% sum()

  duration <- difftime(Sys.time(),start)
  duration_units <- attr(duration,"units")
  duration <- duration%>%
    as.numeric() %>%
    round(2) %>%
    format(nsmall = 2)

  message("Done. Scanned ",n_files, " Files", " (",mb," MB) in ",duration," (",duration_units,"). -> All metadata stored in fdir (@swissrastermapEnv)")
}


