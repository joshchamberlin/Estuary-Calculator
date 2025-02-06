output$map <- renderLeaflet({
  map %>% 
    clearPopups()
})

leafletProxy("map", data=data)%>%
  clearShapes()%>%
  addPolygons( 
    fillColor = pal(as.numeric(data[[hour_column]])),  
    weight = 0.0,
    opacity = 1,
    color = "white",
    label = county17$NAME,
    layerId = county17$NAME,
    highlight = highlightOptions(weight = 2, 
                                 color = "red",
                                 fillOpacity = 0.7,
                                 bringToFront = F),
    dashArray = "3",
    fillOpacity = 0.7 
  )   %>% clearControls() %>%
  addLegend(pal = pal, 
            values = as.numeric(data[[hour_column]]), 
            opacity = 0.7, 
            #baseSize = 10,
            title = react_leg, #paste(selecter3, " Average"), 
            position = "bottomright",
            labFormat = function(type, cuts, p){
              paste0(labels) 
            } 
            
  )