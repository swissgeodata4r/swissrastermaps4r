
raster_greyscale <- function(raster_in, method = "CIELAB"){
  # needs raster
  stopifnot(nlayers(raster_in) == 3)

  # I'm not really sure if CIELAB is the correct name for this method
  if(method == "CIELAB"){
    raster_out <- 0.2126*(raster_in[[1]]/255) + 0.7152*(raster_in[[2]]/255) + 0.0722*(raster_in[[3]]/255)
    raster_out <- calc(raster_out,function(val){ifelse(val <= 0.0031308,val,1.055*val^(1/2.4))})
  } else if(method == "avarage"){
    raster_out <- raster_in[[1]]+raster_in[[2]]+raster_in[[3]]/3
  } else if(method == "weighted"){
    raster_out <- raster_in[[1]]*0.30+raster_in[[2]]*0.59+raster_in[[3]]*0.11
  } else{
    stop(paste0("This method is not defined: ",method))
  }
  brick(raster_out)
}

