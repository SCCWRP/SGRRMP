library(shiny)
library(ggmap)
library(tidyverse)
library(leaflet)

load('data/nhd.sgr.unfort.Rdata')
load('data/sites.RData')

# Define server logic
# color domain
dmn <- nhd.sgr@data %>% 
  select(matches('^full0_')) %>% 
  gather('var', 'val') %>% 
  .$val

# color palette
pal <- colorNumeric(
  palette = c('#d7191c', '#abd9e9', '#2c7bb6'),  
  domain = dmn)

# server logic
server <- function(input, output) {
  
  output$map <- renderLeaflet({

    # get polylines to plot
    inp <- gsub('\\.', '_', input$ptile)
    names(nhd.sgr@data)[names(nhd.sgr@data) %in% inp] <- 'lns'

    # other inputs
    ptsz <- input$pt_sz
    lnsz <- input$ln_sz
    
    # base map for stations
    leaflet(nhd.sgr) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolylines(opacity = 1, weight = lnsz, color = ~pal(lns), 
                   label = ~paste('Likely score:', as.character(round(lns, 2)))
                   ) %>% 
      addCircleMarkers(data = sites, lng = ~long, lat = ~lat, radius = ptsz, weight = 0, fillOpacity = 0.8, 
                       label = ~paste('CSCI:', as.character(round(csci, 2))),
                       fillColor = ~pal(csci)
                       )

   })
   
}
