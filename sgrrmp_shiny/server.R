library(shiny)
library(ggmap)
library(tidyverse)
library(leaflet)

load('data/nhd.sgr.unfort.Rdata')
load('data/sites.RData')

# color domain
dmn <- nhd.sgr@data %>% 
  select(matches('^full0_')) %>% 
  gather('var', 'val') %>% 
  .$val

# color palette
pal <- colorNumeric(
  palette = c('#d7191c', '#abd9e9', '#2c7bb6'),  
  domain = c(sites$csci, dmn))

# custom label format function
myLabelFormat = function(..., reverse_order = FALSE){ 
  if(reverse_order){ 
    function(type = 'numeric', cuts){ 
      cuts <- sort(cuts, decreasing = T)
    } 
  }else{
    labelFormat(...)
  }
}

# server logic
server <- function(input, output) {
  
  # data to plot
  dat <- reactive({
    
    # get polylines to plot
    inp <- gsub('\\.', '_', input$ptile)
    names(nhd.sgr@data)[names(nhd.sgr@data) %in% inp] <- 'lns'
    
    nhd.sgr
    
  })
  
  output$map <- renderLeaflet(
    
    leaflet(sites) %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>% 
      addProviderTiles(providers$CartoDB.Positron)
    
  )
  
  observe({
    
    # other inputs
    ptsz <- input$pt_sz
    lnsz <- input$ln_sz
    prox <- leafletProxy("map", data = dat()) %>%
      clearMarkers() %>%
      clearShapes() %>% 
      clearControls()
    
    prox %>% 
      addPolylines(opacity = 1, weight = lnsz, color = ~pal(lns), 
                   label = ~paste('Likely score:', as.character(round(lns, 2)))
      ) %>% 
      addCircleMarkers(data = sites, lng = ~long, lat = ~lat, radius = ptsz, weight = 0, fillOpacity = 0.8, 
                       label = ~paste('CSCI:', as.character(round(csci, 2))),
                       fillColor = ~pal(csci)
      ) %>% 
      addLegend("bottomright", pal = pal, values = ~lns,
                title = "Likely score",
                opacity = 1#, 
                # labFormat = myLabelFormat(reverse_order = T)
      )
    
  })
   
}
