

sizes = c(5,10,20,40)
scales = c(10,25,50,100,200,500,1000)*1000

sc <- 1:1000 %>%
  map_dfr(function(realwidth_km){
    map_dfr(sizes,function(sizes){
      data_frame(
        realwidth_km = realwidth_km,
        sizes = sizes,
        scale = (realwidth_km*1000*100)/sizes,
        scale_near = scales[which.min(abs(scales-scale))]
      )
    })
  })

library(ggplot2)
library(tidyr)
sc %>%
  gather(key,val,c(scale,scale_near)) %>%
  ggplot(aes(realwidth_km ,val, color = as.factor(sizes))) +
  geom_line() +
  facet_grid(.~key) +
  scale_x_log10() +
  scale_y_log10()
