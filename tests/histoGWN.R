################################################################################
## Initiate Environment ########################################################
################################################################################


library(sf)
library(tidyverse)
library(tmap)
library(grid)
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
  mutate(
    area_ha = as.integer(st_area(polygon_sf)/10000),
    year = as.integer(year)
    )

tmap_mode("view")
tm_shape(polygon_sf) + tm_polygons(col = "year")

polygon_df <- polygon_sf %>%
  st_set_geometry(NULL) %>%
  ungroup()


tmap_mode("view")
tm_shape(point1) + tm_bubbles() + tm_shape(polygon_sf) + tm_fill(col = "year",palette = "viridis")



################################################################################
## Comparing Swissimage data with raster maps ##################################
################################################################################


point1 <- st_sf(st_sfc(st_point(c(583278,231431)),crs = 21781))
point2 <- st_sf(st_sfc(st_point(c(584843,231781)),crs = 21781))
point2 <- st_sf(st_sfc(st_point(c(584843,231781)),crs = 21781))
point3 <- st_sf(st_sfc(st_point(c(582761,230909)),crs = 21781))
point4 <- st_sf(st_sfc(st_point(c(584859,231786 )),crs = 21781))



years <- sort(as.integer(polygon_sf$year))

for(i in 1:length(years)){
  tmap_mode("plot")
  map <- point2 %>%
    get_extent(500,250) %>%
    get_raster(scale_level = 25,method = "bbox",year = years[i])%>%
    tm_shape() +
    tm_rgb() +
    tm_credits(credits("swisstopo"),bg.color = "white",bg.alpha = 0.4) +
    tm_shape(polygon_sf[polygon_sf$year == years[i],]) +
    tm_fill(col = "red")

  gg <- polygon_df %>%
    arrange(year) %>%
    mutate(before = year<years[i]) %>%
    ggplot() +
    geom_point(aes(x = year,y = area_ha),colour = "grey") +
    geom_line(aes(x = year,y = area_ha),colour = "grey",lwd = 2) +
    geom_point(aes(x = year,y = area_ha,colour = before)) +
    geom_line(aes(x = year,y = area_ha,colour = before),lwd = 2) +
    scale_color_manual(values = c("grey","black")) +
    labs(x = "year",y = "Area (ha)") +
    scale_x_continuous(breaks = years) +
    expand_limits(y = 0) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1),
      text = element_text(size=20)
    )

  path <- "C:/Users/rata/switchdrive/Projekte/2018_07_histoGWN/5_Analysis/HistoRproj/Plots/filmchen3/"
  dir.create(path, showWarnings = FALSE)
  filename <- paste0(path,years[i],".png")

  png(filename,1000,1000,"px")
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(2, 1)))
  print(gg, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(map, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  dev.off()
}


for(i in 1:length(years)){
  tmap_mode("plot")
  map <- point3 %>%
    get_extent(500,750) %>%
    get_raster(scale_level = 25,method = "bbox",year = years[i])%>%
    tm_shape() +
    tm_rgb() +
    tm_credits(credits("swisstopo"),bg.color = "white",bg.alpha = 0.4) +
    tm_shape(polygon_sf[polygon_sf$year == years[i],]) +
    tm_fill(col = "red")

  gg <- polygon_df %>%
    arrange(year) %>%
    mutate(before = year<years[i]) %>%
    ggplot() +
    geom_point(aes(x = year,y = area_ha),colour = "grey") +
    geom_line(aes(x = year,y = area_ha),colour = "grey",lwd = 2) +
    geom_point(aes(x = year,y = area_ha,colour = before)) +
    geom_line(aes(x = year,y = area_ha,colour = before),lwd = 2) +
    scale_color_manual(values = c("grey","black")) +
    labs(x = "year",y = "Area (ha)") +
    scale_x_continuous(breaks = years) +
    expand_limits(y = 0) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1),
      text = element_text(size=20)
    )

  path <- "C:/Users/rata/switchdrive/Projekte/2018_07_histoGWN/5_Analysis/HistoRproj/Plots/filmchen4/"
  dir.create(path, showWarnings = FALSE)
  filename <- paste0(path,years[i],".png")

  png(filename,1000,750,"px")
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 2)))
  print(gg, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(map, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
  dev.off()
}


for(i in 1:length(years)){
  tmap_mode("plot")
  map <- point4 %>%
    get_extent(500,250) %>%
    get_raster(scale_level = 25,method = "bbox",year = years[i])%>%
    tm_shape() +
    tm_rgb() +
    tm_credits(credits("swisstopo"),bg.color = "white",bg.alpha = 0.4) +
    tm_shape(polygon_sf[polygon_sf$year == years[i],]) +
    tm_fill(col = "red")

  gg <- polygon_df %>%
    arrange(year) %>%
    mutate(before = year<years[i]) %>%
    ggplot() +
    geom_point(aes(x = year,y = area_ha),colour = "grey") +
    geom_line(aes(x = year,y = area_ha),colour = "grey",lwd = 2) +
    geom_point(aes(x = year,y = area_ha,colour = before)) +
    geom_line(aes(x = year,y = area_ha,colour = before),lwd = 2) +
    scale_color_manual(values = c("grey","black")) +
    labs(x = "year",y = "Area (ha)") +
    scale_x_continuous(breaks = years) +
    expand_limits(y = 0) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1),
      text = element_text(size=20)
    )

  path <- "C:/Users/rata/switchdrive/Projekte/2018_07_histoGWN/5_Analysis/HistoRproj/Plots/filmchen4/"
  dir.create(path, showWarnings = FALSE)
  filename <- paste0(path,years[i],".png")

  png(filename,1000,750,"px")
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 2)))
  print(gg, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(map, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
  dev.off()
}



################################################################################
## Comparing Swissimage data with raster maps ##################################
################################################################################



fgdb <- "C:/Users/rata/switchdrive/Projekte/2018_07_histoGWN/4_Data/digitized_3km_smoothed_2010.gdb"

library(rgdal)
ogrListLayers(fgdb)

sense_smr <- sf::read_sf(dsn=fgdb,layer="Sense_smr")
sense_img <- sf::read_sf(dsn=fgdb,layer="Sense_swissimage")

map_sense <- sense_smr %>%
  get_raster(scale_level = 25,x_add = 750,y_add = 1500,method = "centroid",year = 2010)%>%
  tm_shape() +
  tm_rgb() +
  tm_credits(credits("swisstopo"),bg.color = "white",bg.alpha = 0.4,size = 10) +
  tm_compass(position = c("right","top"),size = 4) +
  tm_scale_bar(position = c("left","bottom"),size = 20) +
  tm_shape(sense_smr) +
  tm_fill(col = "red") +
  tm_shape(sense_img) +
  tm_fill(col = "blue")


saane_smr <- sf::read_sf(dsn=fgdb,layer="Saane_smr")
saane_img <- sf::read_sf(dsn=fgdb,layer="Saane_swissimage")


map_saane <- saane_smr %>%
  get_raster(scale_level = 25,x_add = 750,y_add = 1500,method = "centroid",year = 2010)%>%
  tm_shape() +
  tm_rgb() +
  tm_credits(credits("swisstopo"),bg.color = "white",bg.alpha = 0.4,size = 10) +
  tm_compass(position = c("right","top"),size = 4) +
  tm_scale_bar(position = c("left","bottom"),size = 10) +
  tm_shape(saane_smr) +
  tm_fill(col = "red") +
  tm_shape(saane_img) +
  tm_fill(col = "blue")

png("C:/Users/rata/switchdrive/Projekte/2018_07_histoGWN/5_Analysis/HistoRproj/Plots/vergleich_swissimage/both.png",
    1000,
    1000,"px")
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
print(map_saane, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(map_sense, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()













library(raster)

r1 <- point4 %>%
  get_extent(100,100) %>%
  get_raster(scale_level = 25,method = "bbox",year = years[1])



hsv <- raster::overlay(r1, fun = rgb2hsv)

r <- r1[[1]]
g <- r1[[2]]
b <- r1[[3]]

r2 <- calc(r,function(x){ifelse(x<220,1,0)}) #%>% tm_shape() + tm_raster(n = 2)
g2 <- calc(g,function(x){ifelse(x<220,1,0)}) #%>% tm_shape() + tm_raster(n = 2)
b2 <- calc(b,function(x){ifelse(x<220,1,0)}) #%>% tm_shape() + tm_raster(n = 2)

rgb3 <- raster::overlay(r2,g2,b2,fun = function(r,g,b){r*b*g})
tm_shape(rgb3) + tm_raster(n = 2)

th_sat <- calc(hsv[[3]],function(x){1-((0.8*x)/1)})

th_sat_thresh <- calc(th_sat,function(x){ifelse(x>0.2,0,1)})

tm_shape(th_sat_thresh) + tm_raster()
tm_shape(hsv) + tm_rgb()

rgb2hsv()
