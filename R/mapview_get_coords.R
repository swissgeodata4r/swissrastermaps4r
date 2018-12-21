
#' Transfrom coordinate values stroed as integer vectors
#'
#' I often have some lat/long coordinates stored as integer vectors that I need
#' to transform. Instead of manually creating an sf object ever time, I've created
#' this function to facilitate this process. I might throw this function out at some
#' point.
transform_integers <- function(lat,long,crs_input = 4326,crs_output = crs){
    points_input <- sf::st_as_sf(data.frame(Long = long,Lat = lat), coords = c("Long", "Lat"), crs = crs_input)
    points_output <- sf::st_transform(points_input,crs_output) %>% sf::st_coordinates() %>% as.data.frame()
    return(points_output)
}

#' Get coordinates from mapview
#'
#' I frequently need the coordinates of points I visually identify. I wrapped
#' a shinyapp around mapview() to get these coordinates. Code from
#' \href{https://stackoverflow.com/a/48763280/4139249}{this SO answer by Tim Salabim}
#' I might throw this function out at some point, it doesn't really fit into the package
mapview_get_coords <- function(input_sf,crs = 2056){
    # requires shiny,mapview ,leaflet,clipr,sf,magrittr

    ui <- fluidPage(
        mapviewOutput("inputmap")
    )

    server <- function(input,output,session){
        output$inputmap <- renderMapview({
            mapview(input_sf)
        })
        ## Observe mouse clicks and add circles
        observeEvent(input$inputmap_click, {
            click <- input$inputmap_click
            clat <- click$lat
            clng <- click$lng
            xy <- transform_integers(clat,clng)

            showNotification(paste("Message", xy$X,xy$Y), duration = NULL)
            clipr::write_clip(paste(xy$X,xy$Y),breaks = ",")


            leafletProxy('inputmap') %>%
                addCircles(lng=clng, lat=clat, group='circles',
                           weight=1, radius=10, color='black', fillColor='green',
                           fillOpacity=0.2, opacity=1)
        })
    }

    shinyApp(ui,server)

}






