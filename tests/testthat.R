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
