
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



colourtable_to_grey <- function(colourtable){
  # turns a hex colourtable
  # requires grdevices
  # code copied from Desctools
  rgb <- col2rgb(colortable)
  g <- rbind( c(0.3, 0.59, 0.11) ) %*% rgb
  rgb(g, g, g, maxColorValue=255)
}

scales::show_col(rgb(100,100,50,maxColorValue = 255))





rgb_brick_count <- function(brick,maxColorValue = 255){
  # takes a three band raster, turns all cells into a vector for every band,
  # counts the number of unique combinations and returns a hex colour code
  # for each combination

  brick %>%
    as.list() %>%
    map(as.vector) %>%
    bind_cols() %>%
    magrittr::set_colnames(c("r","g","b")) %>%
    group_by_all() %>%
    count() %>%
    mutate(hex = rgb(r,g,b,maxColorValue = maxColorValue))
}



get_extent_centroid <- function(features,per_feature = T,x_add,y_add){
  # gets the centeroid of feature(s), adds the x, and y distances and returns a
  # matrix with x/y min/max plus and extent-object.

  # features: an object of type sf
  # per feature: should raster be returned PER FEATURE part or for the whole sf object?
  # x_add, yadd: the distances from the centeroid, with which the extent window should be drawn

  if(!per_feature){
    features <- features %>%
      group_by(1) %>%
      summarise()
  }

  features %>%
    st_geometry() %>%
    st_centroid() %>%
    st_coordinates() %>%
    as.data.frame() %>%
    mutate(
      xmin = X-x_add,
      xmax = X+x_add,
      ymin = Y-y_add,
      ymax = Y+y_add
    ) %>%
    select(-c(X,Y)) %>%
    rowwise() %>%
    mutate(
      extent = list(extent(matrix(c(xmin,xmax,ymin,ymax),nrow = 2,byrow = T)))
    )
}


get_raster <- function(features,
                       file_direcory,
                       scale,
                       resolution,
                       x_add,
                       y_add,
                       per_feature = T,
                       method = "fixed_size_centroid",
                       epsg = NULL,
                       limit = Inf,
                       turn_greyscale = F
){

  # input features to get raster data of
  # scale to get raster data (x in 1:1'000x) from 10,25,100,500,1000
  # file_directory: fdir from init_fdir() (todo: get packageenv fdir if fdir is not supplied)
  # resolution: resolution of the map. this is different per scale, 25 typically has 1.25, 100 typicically has 10
  # per feature: will probabbly be removed once this function has a per_feature wrapper
  # method: "fixed_size_centroid" takes the centeroid of the feature and drawas a fixed window around it with x, and y.
  #         in the future, I'd like to implement bounding_box with a buffer (x,y)
  # x_add, y_add: depending on method, x and y will be added to the centeroid (fixed_size_centroid) or to the bounding box
  # epsg: null if CRS need not be set. Otherwise this is an epsg code wich is turned into a CRS() object using rgdal::CRS
  # limit: in order to test the function, a limit on the number of rows can be set


  # Todo:
  #   - warning if nlayers != 3 and turn greyscale = T
  #   - Add colortable options
  #   - "x/y_add" should have a nicer name.. something with distance maybe?
  #   - implement other was to get the extent (bounding box w/ or w/o buffer)
  #   - add failsafes: e.g check if "features" is really an sf object

  if(method == "fixed_size_centroid"){
    ex <- get_extent_centroid(features = features,x_add = x_add, y_add = y_add,per_feature = per_feature)
  } else{
    stop(paste0("This method has not yet been implemented: ",method))
  }

  ex %>%
    head(limit) %>%
    pmap(function(xmin_i,xmax_i,ymin_i,ymax_i,extent_i){
      rast <- file_direcory %>%
        data.frame(stringsAsFactors = F) %>%
        filter(scale == scale) %>%
        filter(res1 == resolution) %>%
        filter(xmin <= xmax_i & xmax >= xmin_i) %>%
        filter(ymin <= ymax_i & ymax >= ymin_i) %>%
        select(file) %>%
        pull() %>%
        map(function(file){
          raster <- brick(file)
          if(!is.null(epsg)){crs(raster) <- CRS(paste0("+init=EPSG:",epsg))}
          raster
        }) %>%
        map(function(x){crop(x,extent_i)}) %>%
        accumulate(function(x,y){raster::merge(x,y)}) %>%
        tail(1) %>%
        magrittr::extract2(1)

      if(nlayers(rast == 3) & turn_greyscale){
        rast <- raster_greyscale(rast)
        rast
        }else(
          rast
        )

    })
}
