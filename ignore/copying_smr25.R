library(tidyverse)
library(sf)
library(raster)

select <- dplyr::select

folder_from <- "D:/SMR25/"

fdir <- init_fdir("C:/Users/rata/Geodata/01_Switzerland/")

library(sf)
library(tidyverse)

# fdir %>%
#   st_set_geometry(NULL) %>%
#   # head(1) %>%
#   select(file, res1,scale) %>%
#   pmap(function(file,res1,scale){
#       file_split <- strsplit(file,"/")[[1]]
#       filename_tif <- file_split[length(file_split)]
#       filename_tfw <- paste0(strsplit(filename_tif,"\\.")[[1]][1],".tfw")
#       filename_xml <- paste0(filename_tif,".aux.xml")
#
#       parent_folder <- file_split[length(file_split)-1]
#       parent_folder_new <- paste(parent_folder,res1,sep = "_")
#       file_split_new <- file_split
#
#
#
#       file_split_new[length(file_split_new)-1] <- parent_folder_new
#
#       newfile_tif <- paste(file_split_new,collapse = "/")
#
#       file_split_new[length(file_split_new)] <- filename_tfw
#       file_split[length(file_split)] <- filename_tfw
#
#       newfile_tfw <- paste(file_split_new, collapse = "/")
#       oldfile_tfw <- paste(file_split, collapse = "/")
#
#       file_split_new[length(file_split_new)] <- filename_xml
#       file_split[length(file_split)] <- filename_xml
#
#       newfile_xml <- paste(file_split_new, collapse = "/")
#       oldfile_xml <- paste(file_split, collapse = "/")
#
#       newfolder <- paste(file_split_new[1:(length(file_split_new)-1)],collapse = "/")
#
#       dir.create(newfolder, showWarnings = FALSE)
#
#       file.rename(file,newfile_tif)
#       file.rename(oldfile_tfw,newfile_tfw)
#       file.rename(oldfile_xml,newfile_xml)
#   })

library(ggplot2)

fdir <- fdir %>%
  st_centroid() %>%
  st_coordinates() %>%
  cbind(.,fdir)


fdir %>%
  # mutate(fileshort = substr(file,70,77)) %>%
  # filter(scale == 100) %>%
  ggplot() +
  geom_sf(fill = "lightblue",alpha = 0.5) +
  # ggrepel::geom_label_repel(aes(X,Y,label = fileshort))+
  facet_wrap(~scale)

test <- "C:/Users/rata/Geodata/01_Switzerland//01_Maps/SMR25/Ov_i02_L01_R000006BE_C00000617.tif"

st <- strsplit(test,"/")[[1]]
st[length(st)-1]


# folder_to <- "X:/Archiv/02_Schweiz/03_Karten/Swissmap/Swissmap25"
folder_to <- "C:/Users/rata/Geodata/01_Switzerland/01_Maps/SMR25"

list.files(folder_from,".tif$",full.names = T) %>%
  str_split_fixed(.,"/",3) %>%
  as_data_frame() %>%
  magrittr::set_colnames(c("path1","path2","file")) %>%
  separate(file,c("maptype","crs","col","blatt","year","ext"),"_",remove = F) %>%
  group_by(blatt) %>%
  filter(year == max(year)) %>%
  dplyr::mutate(filepath = file.path(path1,path2,file)) %>%
  ungroup() %>%
  dplyr::select(file,filepath) %>%
  purrr::pmap(function(file,filepath){
    file_tfw <- stringi::str_replace(file,".tif",".tfw")
    file_xml <- paste0(file,".xml")
    file_tfw_folder <- file.path(c(folder_from,folder_to),file_tfw)
    file_xml_folder <- file.path(c(folder_from,folder_to),file_xml)
    file_tif_folder <- file.path(c(folder_from,folder_to),file)

    if(file.exists(file_tfw_folder[1])){file.copy(file_tfw_folder[1],file_tfw_folder[2],overwrite = T)}
    if(file.exists(file_xml_folder[1])){file.copy(file_xml_folder[1],file_xml_folder[2],overwrite = T)}
    if(file.exists(file_tif_folder[1])){file.copy(file_tif_folder[1],file_tif_folder[2],overwrite = T)}
  })
