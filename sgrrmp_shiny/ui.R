library(leaflet)

# percentiles to select
ptile <- seq(0.05, 0.95, by = 0.05) %>% 
  format(nsmall = 2)

# tails to cut by likelihood expectation
likes <- ptile[1:9]

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel('Stream classification in the San Gabriel River Watershed'),
  
  fluidRow(
    
    column(width = 12, 
           
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
      )
      
    )
    
  ),
    
  tabsetPanel(
    
    tabPanel('Score distributions',
             
      # select percentile        
      column(width = 4, 
        selectInput(inputId  =  'ptile',
                    label = h4('Percentile estimated score:'),
                    choices = ptile, selected = '0.50')
      ),
    
      # map output
      column(width = 12,
        
        leafletOutput('map', width = '100%', height = 550)
             
      )
    
    ), 
    
    tabPanel('Estimated constraints',
    
      column(width = 12,
            
        # select CSCI threshold       
        column(width = 4, 
               sliderInput('thrsh', 
                           label = h4("CSCI threshold:"), 
                           min = 0, 
                           max = 1.5,
                           value = 0.79, 
                           step = 0.01
               )
        ),
        
        # selected tails
        column(width = 4, 
               selectInput(inputId  =  'likes',
                           label = h4('Expectation ranges:'),
                           choices = likes, selected = '0.05')
        )
            
      ),       
             
      tabsetPanel(
        
        tabPanel('Map',
       
          # map output
          column(width = 12,
                
            leafletOutput('map_exp', width = '100%', height = 550)
                
          ) 
          
        ),
          
        tabPanel('Plot', 
               
          # plot output
          column(width = 12,
                
            plotOutput('plo_exp', width = '100%', height = 550)
                
          ) 
                 
        )
        
      )
                
    )
    
  )
            
)
