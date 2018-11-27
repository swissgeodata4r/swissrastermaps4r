library(testthat)
library(swissmapraster)

test_check("swissmapraster")


fdir <- init_fdir("C:/Users/rata/Geodata/01_Switzerland/")
#
# fdir <- filter(fdir,res1 != 1)
#

#
#
# fdir %>%
#   filter(scale == 25) %>%
#   filter(res1 == 3.75) %>%
#   sf::write_sf("C:/Users/rata/Temp/res375.shp")



# If I  want to do some tests with a constructed brick:
# Here is how to construct a brick

# d <- brick(nrows=100, ncols=100, xmn=-180, xmx=180, ymn=-90, ymx=90, nl=3)
#
# d[[1]] <- matrix((1:100*100),ncol = 100,nrow = 100)
# d[[2]] <- matrix((100:1*100),ncol = 100,nrow = 100)
# d[[3]] <- matrix((1:100*100),ncol = 100,nrow = 100)
#
#
# library(ggplot2)
#
# library(swissboundriesR)
#
# fdir <- init_fdir("C:/Users/rata/Geodata/01_Switzerland/")
#
# data("kantonsgebiet")
#
#
# kantone <- kantonsgebiet %>%
#   group_by(NAME) %>%
#   summarise()
#
#
# rasts <- get_raster(kantone,fdir,500,25,500,500,T,"bbox",epsg = 2056,turn_greyscale = F)
#
#
# for(i in 1:nrow(kantone)){
#   kanton <- kantone$NAME[i]
#   map <- tm_shape(rasts[[i]]) + tm_raster(legend.show = F) +
#     tm_shape(kantone[i,]) + tm_polygons(fill = "blue",alpha = 0.8)+
#     tm_layout(title = kanton,title.bg.color = "white",title.bg.alpha = 0.5)
#   tmap_save(map,paste0("C:/Users/rata/Temp/maps/",i,".png"),width = 15,units = "cm")
# }
#
# sf::st_bbox(kantonsgebiet)
# .
#
# tm_shape(r[[1]]) + tm_raster(legend.show = F) +
#   tm_shape(features) + tm_polygons(alpha = 0.5, colour = "lightblue")
#
# fdir %>%
#   dplyr::filter(res1 != 1) %>%
#   ggplot(data = .) +
#   geom_sf(fill = "lightblue", alpha = 0.3) +
#   facet_wrap(~res1+scale)

