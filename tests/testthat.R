library(testthat)
library(swissmapraster)

test_check("swissmapraster")


# fdir <- init_fdir("C:/Users/rata/Geodata/01_Switzerland/")
#
# fdir <- filter(fdir,res1 != 1)
#
# fdir %>%
#   filter(scale == 25) %>%
#   sf::st_set_crs(2056) %>%
#   ggplot(data = .) +
#   geom_sf(fill = "lightblue", alpha = 0.3) +
#   facet_wrap(~res1)
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
