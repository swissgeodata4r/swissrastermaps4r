library(sf)
library(tidyverse)
library(swissboundriesR)
data("hoheitsgebiet")

sample_polygons <- hoheitsgebiet %>%
  filter(OBJEKTART == "Gemeindegebiet") %>%
  select(NAME,EINWOHNERZ) %>%
  arrange(desc(EINWOHNERZ)) %>%
  head(10)


usethis::use_data(sample_polygons,overwrite = TRUE)


sample_points <- sample_polygons %>%
  st_centroid()


usethis::use_data(sample_points, overwrite = TRUE,compress = "xz")


data("landesgebiet")

switzerland <- landesgebiet %>%
  select(NAME,EINWOHNERZ) %>%
  st_zm()

usethis::use_data(switzerland, overwrite = TRUE,compress = "xz")

