library(leaflet)

# percentiles to select
ptile <- seq(0.05, 0.95, by = 0.05) %>% 
  format(nsmall = 2) %>% 
  paste0('full', .)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel('Stream classification in the San Gabriel River Watershed'),
  
  mainPanel(width = 12,
  
    # select percentile        
    column(width = 2, 
      selectInput(inputId  =  'ptile',
                  label = h4('Likely score:'),
                  choices = ptile, selected = 'full0.50')
    ),
    
    # select point radius
    column(width = 3,
      sliderInput("pt_sz", 
                  label = h4("Point size:"), 
                  min = 0, 
                  max = 15,
                  value = 4, 
                  step = 1
      )
    ),
    
    # select line size
    column(width = 3,
           sliderInput("ln_sz", 
                       label = h4("Line size:"), 
                       min = 0, 
                       max = 5,
                       value = 1, 
                       step = 0.1
           )
    ),
    
    # map output
    column(width = 12,
      
      leafletOutput('map', width = '100%', height = 550)
           
    )
    
  )
            
)
