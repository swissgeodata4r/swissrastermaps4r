
packageEnv <- new.env()
import::from(magrittr, "%>%")



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
#' Folders as specified by \code{folders} require a specific structure:
#' \code{TYPE-Scale_EPSG_name}
#' \describe{
#'   \item{\code{TYPE}}{corresponds to the maptype. IN Switzerland this is typically \code{SMR}, \code{PK}, \code{LK} or \code{TA}. \strong{Must} be followed by a \code{-}}
#'   \item{\code{Scale}}{defines the map scale as x in 1:1'000x. \strong{Must} be followed by a \code{_}}
#'   \item{\code{EPSG}}{specifies the CRS of the containing raster data \strong{Must} be followed by a \code{_} \strong{only} if \code{name} is specified}
#'   \item{\code{name}}{is only needed when multiple maps with overlapping extents as well as same scale, level and projection exist (e.g. \href{SMR1000 https://shop.swisstopo.admin.ch/en/products/maps/national/digital/srm1000}{SMR1000})}
#' }
#' @param rootdir Character string specifying the directory where the folders are stored
#' @param maxfiles Integer limiting the number of files to be scanned. For testing purposes only.
#' @param add_geometry Should the bounding box of each file be added as a geometry to \code{fdir?
#' @param folders Names of the folders containing the raster files. These folders names
#' require a very specific nomenclature for this whole thing to work (see description)

init_fdir <- function(rootdir,
                      maxfiles = Inf,
                      add_geometry = T,
                      folders = c("SMR-25_2056",
                                  "SMR-25_21781",
                                  "SMR-25_21781",
                                  "SMR-50_2056",
                                  "PK-100_2056",
                                  "PK-200_2056",
                                  "PK-500_2056",
                                  "PK-1000_2056_orte",
                                  "PK-1000_2056_relief",
                                  "PK-1000_2056_reliefcol",
                                  "PK-1000_2056_strassen")
                      ){

  # needs: dplyr, raster and purrr


  # maxfiles: maximum number of files per maptype
  # scales: what scales should be looked for?

  # create a data_frame with all maptypes and scale.
  # (note: "name" is the name of the map on one hand, and the keyword which
  # will be queried on the other)
  # Needs to be rewritten if other maptypes are included (LK, PK..)
  # folder_poss <- purrr::map_dfr(maptypes,function(maptype){
  #   purrr::map_dfr(scales,function(scale){
  #     purrr::map_dfr(epsg_codes,function(epsg){
  #       data.frame(
  #         maptype = maptype,
  #         scale = scale,
  #         epsg = epsg,
  #         folder = paste0(maptype,scale,"_",epsg),
  #         stringsAsFactors = F)
  #     })
  #   })
  # })

    # Get all directories in rootdir
  # dirs <- list.dirs(rootdir,recursive = T,full.names = F)

  folders_df <- strsplit(folders,"-") %>%
    purrr::map_dfr(function(x){
      strsplit(x[2],"_") %>%
        purrr::map_dfr(function(y){
          data.frame(
            maptype = x[1],
            scale = y[1],
            epsg = y[2],
            name = ifelse(length(y) == 3,y[3],""),
            stringsAsFactors = F
          )
        })
      }) %>%
    dplyr::mutate(
      folder = folders
    )





  # Assign Folders to each maptype. This part is very fragile and will break
  # if more than one folder meets the query, and if rasterfiles are organized
  # in subfolders of each maptype-folder. Must be rewritten!
  # folderpaths <- maptypes %>%
  #   purrr::pmap_dfr(function(name,scale){
  #     folder <-  dirs[grepl(paste0(name,"_"),dirs,ignore.case = T)]
  #     epsg <-  strsplit(folder,split = "_")[[1]]
  #     epsg <- epsg[length(epsg)]
  #     data.frame(
  #       name = name,
  #       folder = folder,
  #       scale = scale,
  #       epsg = epsg,
  #       stringsAsFactors = F)
  #   })

  # Creates a data_frame by going through all the maptypes and their
  # corresponding folders, reading in all raster files (only "tifs" at the )
  fdir <- folders_df %>%
    purrr::pmap_dfr(function(maptype,scale,epsg,name,folder){
      folderpath <- file.path(rootdir,folder)
      list.files(folderpath,".tif$",full.names = T) %>%
        head(maxfiles) %>% # this can be used to test and debug the function
        purrr::map_dfr(function(x){
          raster_i <- raster::brick(x)
          n_layers <- raster::nlayers(raster_i)
          reso <- raster::res(raster_i)
          raster_i %>%
            raster::extent() %>%
            matrix(nrow = 1) %>%
            as.data.frame() %>%
            magrittr::set_colnames(c("xmin","xmax","ymin","ymax")) %>%
            dplyr::mutate(
              maptype = maptype,
              scale = scale,
              epsg = epsg,
              name = name,
              file = x,
              nlayers = n_layers,
              res1 = reso[1],
              res2 = reso[2]
            )
        })
    })
  if(add_geometry){
    fdir <- geom_from_boundary(fdir, add = T,2056)
  }
  assign("fdir",fdir,envir = packageEnv)
  fdir
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

  if(is.null(fdir)){
    fdir <- get("fdir",envir = packageEnv)

    fdir$res1 <- as.factor(fdir$res1)
  }



  if(method == "ggplot2"){
    plotoutput <- ggplot2::ggplot(data = fdir) +
      ggplot2::geom_sf(mapping = aes(fill = factor(res1)),alpha = 0.4) +
      ggplot2::facet_wrap(~scale) +
      ggplot2::coord_sf(datum = 2056) +
      ggplot2::labs(fill = "Resolution") +
      ggplot2::scale_x_continuous(breaks = seq(25,29,2)*10^5) +
      ggplot2::scale_y_continuous(breaks = seq(11,13,1)*10^5) +
      ggplot2::theme(legend.position = "top",legend.direction = "horizontal")
  }
  if(method == "tmap"){   # I don't think this is good practice
    plotoutput <- tmap::tm_shape(fdir) +
      tmap::tm_fill(title = "Resolution", col = "res1",alpha = 0.4,group = "extents",palette = "Accent") +
      tmap::tm_borders() +
      tmap::tm_facets(by = "scale")
  }
  return(plotoutput)
}

