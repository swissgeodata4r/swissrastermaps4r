
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


init_fdir <- function(rootdir,maxfiles = Inf,scales = c(10,25,50,200,100,500,1000),add_geometry = T){

  # needs: dplyr, raster and purrr


  # maxfiles: maximum number of files per maptype
  # scales: what scales should be looked for?

  # create a data_frame with all maptypes and scale.
  # (note: "name" is the name of the map on one hand, and the keyword which
  # will be queried on the other)
  # Needs to be rewritten if other maptypes are included (LK, PK..)
  maptypes <- dplyr::data_frame(
    name = paste0("smr",scales),
    scale = scales
  )

  # Get all directories in rootdir
  dirs <- list.dirs(rootdir,recursive = T,full.names = F)

  # Assign Folders to each maptype. This part is very fragile and will break
  # if more than one folder meets the query, and if rasterfiles are organized
  # in subfolders of each maptype-folder. Must be rewritten!
  folderpaths <- maptypes %>%
    purrr::pmap_dfr(function(name,scale){
      folder = dirs[grepl(paste0(name,"$"),dirs,ignore.case = T)]
      dplyr::data_frame(name = name, folder = folder,scale = scale)
    })

  # Creates a data_frame by going through all the maptypes and their
  # corresponding folders, reading in all raster files (only "tifs" at the )
  fdir <- folderpaths %>%
    purrr::pmap_dfr(function(name,folder,scale){
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
            dplyr::mutate(scale = scale,
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
#'#' @param method The method with which to visualize the data. "ggplot2" or "tmap"
#' Wrap the output in ggplotly() to get an interactive ggplot output or set tmap_mode
#' to "view" go get an interactive tmap.
#' @param as_layers Only applicable if method = "ggplot2". If TRUE, the function
#' will only return ggplot2 layers (as a list) and the initial ggplot() object
#' can be self defined
#' @param filedirectory The file directory aquired by \code{init_fdir()}. If left as
#' NULL, the fdir from the package Environment is taken.

show_extents <- function(method = "ggplot2",fdir = NULL){

  if(is.null(fdir)){
    fdir <- get("fdir",envir = packageEnv)

    fdir <- fdir %>%
      dplyr::mutate(res1 = as.factor(res1))
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
      tmap::tm_polygons(col = "res1",alpha = 0.4,group = "extents",palette = "Accent") +
      tmap::tm_facets(by = "scale")
  }
  return(plotoutput)
}

