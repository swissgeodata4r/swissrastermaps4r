ov <- fdir %>%
  # filter(fn_year == 2011) %>%
  split(.$fn_year) %>%
  map_dfr(function(x){
    mat <- st_intersects(x,sparse = F) & !st_touches(x,sparse = F)
    mat[lower.tri(mat,diag = T)] <- NA
    df <- which(mat,arr.ind = T) %>%
      as.data.frame()

    pmap_dfr(df,function(row,col){
      data_frame(
        file1 = x$file[row],
        file2 = x$file[col],
        filename1 = x$filename[row],
        filename2 = x$filename[col],
        maptype1 = x$maptype[row],
        maptype2 = x$maptype[col],
        year1 = x$fn_year[row],
        year2 = x$fn_year[col],
        scale1 = x$scale[row],
        scale2 = x$scale[col],
      )
    })
  }) %>%
  filter(scale1 == scale2)

ov %>%
  # head(1) %>%
  select(file1,file2,filename1,filename2, maptype1,maptype2,scale1,scale2) %>%
  pmap(function(file1,file2,filename1,filename2, maptype1,maptype2,scale1,scale2){
    base <- file1
    base <- strsplit(base,"\\.")[[1]][1]
    file_tif_from <- file1
    file_xml_from <- paste0(file1,".xml")
    file_tfw_from <- paste0(base,".tfw")

    maptype_new <- paste(str_split(maptype1,"")[[1]], collapse = "_")

    newpath <- file.path("D:/",paste0(maptype_new,"_",scale1,"_duplicated"))
    dir.create(newpath, showWarnings = FALSE)
    file_tif_to <- strsplit(file_tif_from,"/")[[1]]
    file_tif_to <- file_tif_to[length(file_tif_to)]
    file_tif_to <- file.path(newpath,file_tif_to)
    # paste(base)
    file_xml_to <- strsplit(file_xml_from,"/")[[1]]
    file_xml_to <- file_xml_to[length(file_xml_to)]
    file_xml_to <- file.path(newpath,file_xml_to)

    file_tfw_to <- strsplit(file_tfw_from,"/")[[1]]
    file_tfw_to <- file_tfw_to[length(file_tfw_to)]
    file_tfw_to <- file.path(newpath,file_tfw_to)

    if(file.exists(file_tif_from)){file.copy(file_tif_from,file_tif_to,overwrite = T)}else(paste(file_tif_from,"not found"))
    if(file.exists(file_xml_from)){file.copy(file_xml_from,file_xml_to,overwrite = T)}else(paste(file_xml_from,"not found"))
    if(file.exists(file_tfw_from)){file.copy(file_tfw_from,file_tfw_to,overwrite = T)}else(paste(file_tfw_from,"not found"))

  })




fdir %>%
  select(-c(fn_year_start,fn_year_end)) %>%
  dplyr::group_by(maptype,fn_sheet) %>%
  dplyr::arrange(maptype,fn_sheet,fn_year) %>%
  dplyr::mutate(
    fn_year_start = as.integer(fn_year - floor((fn_year-dplyr::lag(fn_year))/2)),
    fn_year_start = dplyr::if_else(is.na(fn_year_start),as.integer(fn_year-5),as.integer(fn_year_start)),
    fn_year_end = as.integer(fn_year + ceiling((dplyr::lead(fn_year)-fn_year)/2))-1,
    fn_year_end = dplyr::if_else(is.na(fn_year_end),as.integer(fn_year+5),as.integer(fn_year_end)),
  ) %>%
  filter(fn_sheet == "1106") %>%
  select(fn_year_start,fn_year,fn_year_end) %>%
  filter(fn_year_end >= lead(fn_year_start))




