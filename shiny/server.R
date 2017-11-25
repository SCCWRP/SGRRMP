library(shiny)
library(ggmap)
library(tidyverse)

load('data/nhd.sgr.fort.Rdata')

# Make SG map
bbx <- make_bbox(lon = nhd.sgr.fort$long, lat = nhd.sgr.fort$lat, f = 0.25)
sg.map <- get_map(location = sg.box, zoom = 10, source = 'google', maptype = 'satellite', color = 'bw')
base <- ggmap(sg.map) + 
  geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = 'white', alpha = 0.2) +
  coord_map(xlim = c(min(nhd.sgr.fort$long, na.rm = T)-.02,max(nhd.sgr.fort$long, na.rm = T) + .02),
            ylim = c(min(nhd.sgr.fort$lat, na.rm = T)-.02,max(nhd.sgr.fort$lat, na.rm = T) + .02)) + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot  <-  renderPlot({

      # generate bins based on input$bins from ui.R
      nhd.sgr.fort$pref.score <- nhd.sgr.fort[, input$ptile]
      
      # draw the histogram with the specified number of bins
      # hist(x, breaks  =  bins, col  =  'darkgray', border  =  'white')
      base +
        geom_path(data = nhd.sgr.fort, aes(x = long, y = lat, group = COMID, color = pref.score)) +
        scale_color_gradient2(name = 'Likely score', high = '#2c7bb6', mid = '#abd9e9', low = '#d7191c', midpoint  =  0.79,
                              na.value = '#ffff33', limits = c(0,1.4), breaks = c(.63,.79,.92,1)) +
        ggtitle(input$ptile)
   })
   
}
