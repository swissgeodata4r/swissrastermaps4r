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
#' @param filter Names of the folders to look for. Only folders containing the character strings
#' specified here are included in the search.

fdir_init <- function(rootdir,
                      filter = c("PK","SMR","LK","TA")
){

  start <- Sys.time()

  year <- as.integer(strftime(Sys.Date(),"%Y"))



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
    dplyr::group_by(epsg,maptype,scale,nlayers) %>%
    tidyr::nest() %>%
    dplyr::group_by(epsg,maptype,scale,nlayers) %>%
    dplyr::mutate(
      data = purrr::map(data,~geom_from_boundary(.x,epsg)),
      year_median = purrr::map_dbl(data,~median(.x$year))
    )


  fdir <- fdir %>%
    dplyr::mutate(
      data = purrr::map(data,function(x){
        x %>%
          dplyr::group_by(sheet) %>%
          dplyr::arrange(sheet,year) %>%
          dplyr::mutate(
            year_start = year - floor((year-dplyr::lag(year))/2),
            year_start = ifelse(is.na(year_start),-Inf,year_start),
            year_end = (year + ceiling((dplyr::lead(year)-year)/2))-1,
            year_end = ifelse(is.na(year_end),Inf,year_end),
          ) #%>% dplyr::ungroup()
      })
    )


  assign("fdir",fdir,envir = swissrastermapEnv)
  mb <- purrr::map_dbl(fdir$data,~.x$size_mb %>% sum()) %>% sum() %>% format(big.mark = "'")
  n_files <- purrr::map_int(fdir$data,nrow) %>% sum()

  duration <- difftime(Sys.time(),start)
  duration_units <- attr(duration,"units")
  duration <- duration%>%
    as.numeric() %>%
    round(2) %>%
    format(nsmall = 2)

  message("Done. Scanned ",n_files, " Files", " (",mb," MB) in ",duration," (",duration_units,"). -> All metadata stored in fdir (@swissrastermapEnv)")
}


