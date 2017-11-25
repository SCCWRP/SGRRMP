# percentiles to select
ptile <- seq(0.05, 0.95, by = 0.05) %>% 
  format(nsmall = 2) %>% 
  paste0('full', .)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel('Stream classification in the San Gabriel River Watershed'),
  
  mainPanel(width = 12,
  
    column(width = 2, 
    fluidRow(
      selectInput(inputId  =  'ptile',
                  label = 'Likely score:',
                  choices = ptile)
      )
    ),
    
    column(width = 12,
           
      plotOutput('distPlot')    
           
    )
    
  )
            
)
