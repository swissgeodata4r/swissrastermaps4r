
library(sf)
library(tidyverse)
library(tmap)

# init_fdir(rootdir = "D:/",add_geometry = T)
# fdir <- get("fdir",swissmaprasterEnv)
# save(fdir,file = "D:/fdir.Rda")

init_fdir("D:/fdir.Rda")

data("gemeinden_top_centroid")

load("C:/Users/rata/switchdrive/Projekte/2018_07_histoGWN/5_Analysis/HistoRproj/temp_data/polygon_list.Rda")

polygon_sf <- polygon_list %>%
  map(function(x){
    x %>%
      group_by(map,year) %>%
      summarise()
  }) %>%
  do.call(rbind,.)


polygon_sf <- polygon_sf %>%
  ungroup() %>%
  mutate(area_ha = as.integer(st_area(polygon_sf)/10000))

tmap_mode("view")
tm_shape(polygon_sf) + tm_polygons(col = "year")

polygon_df <- polygon_sf %>%
  st_set_geometry(NULL) %>%
  ungroup()


library(grid)

point1 <- st_sf(st_sfc(st_point(c(583278, 231431)),crs = 21781))
point2 <- st_sf(st_sfc(st_point(c(584843, 231781 )),crs = 21781))


years <- sort(as.integer(polygon_sf$year))

for(i in 13:length(years)){
  tmap_mode("plot")
  map <- point2 %>%
    get_extent(750,750) %>%
    get_raster(scale_level = 25,method = "bbox",year = years[i])%>%
    tm_shape() +
    tm_rgb() +
    tm_shape(polygon_sf[polygon_sf$year == years[i],]) + tm_fill(col = "red")

  gg <- polygon_df %>%
    arrange(year) %>%
    mutate(before = year<years[i]) %>%
    ggplot(aes(x = year,y = area_ha,alpha = before)) +
    geom_point() +
    geom_line(group = 1) +
    scale_alpha_discrete() +
    labs(x = "year",y = "Area (ha)") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1))

  path <- "C:/Users/rata/switchdrive/Projekte/2018_07_histoGWN/5_Analysis/HistoRproj/Plots/filmchen2/"
  filename <- paste0(path,years[i],".png")

  png(filename,2000,1000,"px")
  grid.newpage()
  print(gg, vp=viewport(0.5,0,0.5,1,just = c("left","bottom")))
  print(map, vp=viewport(0,0,0.5,1,just = c("left","bottom")))
  dev.off()
}


fdir %>%
  filter(fn_sheet == "1106") %>%
  select(fn_year_start,fn_year,fn_year_end)

