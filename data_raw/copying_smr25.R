library(tidyverse)
library(sf)
library(raster)

select <- dplyr::select

folder_from <- "D:/SMR25/"
# folder_to <- "X:/Archiv/02_Schweiz/03_Karten/Swissmap/Swissmap25"
folder_to <- "C:/Users/rata/Geodata/01_Switzerland/01_Maps/SMR25"

list.files(folder_from,".tif$",full.names = T) %>%
  str_split_fixed(.,"/",3) %>%
  as_data_frame() %>%
  magrittr::set_colnames(c("path1","path2","file")) %>%
  separate(file,c("maptype","crs","col","blatt","year","ext"),"_",remove = F) %>%
  group_by(blatt) %>%
  filter(year == max(year)) %>%
  mutate(filepath = file.path(path1,path2,file)) %>%
  ungroup() %>%
  select(file,filepath) %>%
  pmap(function(file,filepath){
    file_tfw <- str_replace(file,".tif",".tfw")
    file_xml <- paste0(file,".xml")
    file_tfw_folder <- file.path(c(folder_from,folder_to),file_tfw)
    file_xml_folder <- file.path(c(folder_from,folder_to),file_xml)
    file_tif_folder <- file.path(c(folder_from,folder_to),file)

    if(file.exists(file_tfw_folder[1])){file.copy(file_tfw_folder[1],file_tfw_folder[2],overwrite = T)}
    if(file.exists(file_xml_folder[1])){file.copy(file_xml_folder[1],file_xml_folder[2],overwrite = T)}
    if(file.exists(file_tif_folder[1])){file.copy(file_tif_folder[1],file_tif_folder[2],overwrite = T)}
  })
