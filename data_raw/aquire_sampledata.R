library(sf)
library(tidyverse)
library(swissboundriesR)
data("hoheitsgebiet")

gemeinden_top_poly <- hoheitsgebiet %>%
  filter(OBJEKTART == "Gemeindegebiet") %>%
  select(NAME,EINWOHNERZ) %>%
  arrange(desc(EINWOHNERZ)) %>%
  head(10)


usethis::use_data(gemeinden_top_poly,overwrite = TRUE)


gemeinden_top_cenroid <- gemeinden_top_poly %>%
  st_centroid()


usethis::use_data(gemeinden_top_cenroid, overwrite = TRUE,compress = "xz")


data("landesgebiet")


usethis::use_data(landesgebiet, overwrite = TRUE,compress = "xz")

