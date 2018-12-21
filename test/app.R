
mapview_mouse <- function(input_sf,crs = 2056){
    require(shiny)
    require(mapview)
    require(leaflet)
    require(clipr)
    require(sf)
    require(magrittr)
    transform_integers <- function(lat,long,crs_input = 4326,crs_output = crs){
        points_input <- sf::st_as_sf(data.frame(Long = long,Lat = lat), coords = c("Long", "Lat"), crs = crs_input)
        points_output <- sf::st_transform(points_input,crs_output) %>% sf::st_coordinates() %>% as.data.frame()
        return(points_output)
    }



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






