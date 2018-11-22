

# rootdir <- "C:/Users/rata/Geodata/01_Switzerland/"

init_fdir <- function(rootdir,maxfiles = Inf,scales = c(10,25,50,100,500,1000)){
  require(dplyr)
  require(raster)
  require(purrr)

  # maxfiles: maximum number of files per maptype
  # scales: what scales should be looked for?

  # create a data_frame with all maptypes and scale.
  # (note: "name" is the name of the map on one hand, and the keyword which
  # will be queried on the other)
  # Needs to be rewritten if other maptypes are included (LK, PK..)
  maptypes <- data_frame(
    name = paste0("smr",scales),
    scale = scales
  )

  # Get all directories in rootdir
  dirs <- list.dirs(rootdir,recursive = T,full.names = F)

  # Assign Folders to each maptype. This part is very fragile and will break
  # if more than one folder meets the query, and if rasterfiles are organized
  # in subfolders of each maptype-folder. Must be rewritten!
  folderpaths <- maptypes %>%
    pmap_dfr(function(name,scale){
      folder = dirs[grepl(paste0(name,"$"),dirs,ignore.case = T)]
      data_frame(name = name, folder = folder,scale = scale)
    })

  # Creates a data_frame by going through all the maptypes and their
  # corresponding folders, reading in all raster files (only "tifs" at the )
  fdir <<- folderpaths %>%
    pmap_dfr(function(name,folder,scale){
      folderpath <- file.path(rootdir,folder)
      list.files(folderpath,".tif$",full.names = T) %>%
        head(maxfiles) %>% # this can be used to test and debug the function
        map_dfr(function(x){
          raster::brick(x) %>%
            extent() %>%
            matrix(nrow = 1) %>%
            as.data.frame() %>%
            magrittr::set_colnames(c("xmin","xmax","ymin","ymax")) %>%
            mutate(scale = scale,file = x)
        })
    })
}








# pmap(function(filepath){
#   raster <- brick(filepath)
#   crs(raster) <- CRS('+init=EPSG:2056')
#   raster
# }) %>%
#   map_dfr(function(x){
#     fname <- raster::filename(x)
#     x %>%
#       extent() %>%
#       matrix(nrow = 1) %>%
#       as.data.frame() %>%
#       magrittr::set_colnames(c("xmin","xmax","ymin","ymax")) %>%
#       mutate(file = fname)
#   })
