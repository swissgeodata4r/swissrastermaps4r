
packageEnv <- new.env()


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
  fdir
}

