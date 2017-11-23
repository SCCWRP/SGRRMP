library(shiny)
library(ggmap)

load('data/nhd.sgr.fort.Rdata')

#Make SG map
bbx <- make_bbox(lon = nhd.sgr.fort$long, lat = nhd.sgr.fort$lat, f = 0.25)
sg.map <- get_map(location = sg.box, zoom = 10, source = 'google', maptype = 'satellite', color = 'bw')
SGbasemap2.basemap <- ggmap(sg.map)
# SGbasemap2.basemap <- SGbasemap2 + 
#   geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = '#f0f0f020') +
#   coord_map(xlim = c(min(nhd.sgr.fort$long, na.rm = T)-.025,max(nhd.sgr.fort$long, na.rm = T)+.025),
#             ylim = c(min(nhd.sgr.fort$lat, na.rm = T)-.025,max(nhd.sgr.fort$lat, na.rm = T)+.025))

xwalk <- data.frame(ptile = seq(from = .05, to  = 0.95, by = 0.05))
xwalk$field <- paste0('full', sprintf('%.2f', xwalk$ptile))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel('Stream classification in the San Gabriel River Watershed'),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId  =  'ptile',
                     label = 'Likely score:',
                     choices = xwalk$field)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput('distPlot')
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot  <-  renderPlot({
      # generate bins based on input$bins from ui.R
     nhd.sgr.fort$pref.score <- nhd.sgr.fort[,input$ptile]
      
      # draw the histogram with the specified number of bins
      # hist(x, breaks  =  bins, col  =  'darkgray', border  =  'white')
      SGbasemap2.basemap +
        geom_path(data = nhd.sgr.fort, aes(x = long, y = lat, group = COMID, color = pref.score)) +
        scale_color_gradient2(name = 'Likely score', high = '#2c7bb6', mid = '#abd9e9', low = '#d7191c', midpoint  =  0.79,
                              na.value = '#ffff33', limits = c(0,1.4), breaks = c(.63,.79,.92,1)) +
        ggtitle(input$ptile)
   })
}

# Run the application 
shinyApp(ui  =  ui, server  =  server)

