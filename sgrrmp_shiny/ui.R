library(leaflet)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel('Stream classification in the San Gabriel River Watershed'),
  
  fluidRow(
    
    column(width = 12, 
      
      # prediction model
      column(width = 3, 
            selectInput(inputId  =  'modls',
                        label = h4('Prediction model:'),
                        choices = c('core', 'full'), selected = 'full')
      ),    
      
      # which site classification
      column(width = 3, 
             selectInput(inputId  =  'typs',
                         label = h4('Site classifications:'),
                         choices = c('perf', 'type'), selected = 'perf')
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
      )
      
    )
    
  ),
    
  tabsetPanel(
    
    tabPanel('Score distributions',
             
      # select percentile        
      column(width = 4, 
             
        sliderInput('ptile',
                    label = h4("Percentile estimated score:"),
                    min = 0.05,
                    max = 0.95,
                    value = 0.5,
                    step = 0.05
        )
        
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
               sliderInput('tails', 
                           label = h4("Expectation tails:"), 
                           min = 0.05, 
                           max = 0.45,
                           value = 0.05, 
                           step = 0.05
               )
        )
            
      ),       
             
      tabsetPanel(type = 'pills', 
        
        tabPanel('Map',
       
          # map output
          column(width = 12,
                
            leafletOutput('map_exp', width = '100%', height = 550)
                
          ) 
          
        ),
          
        tabPanel('Plot', 
               
          # plot output
          column(width = 12,
                
            plotOutput('plo_exp', width = '90%', height = 850)
                
          ) 
                 
        ), 
        
        tabPanel('Table', 
                 
          # table output
          column(width = 12, 
          
            DT::dataTableOutput('tab_sum'), 
            HTML('<p></p>')
                       
          )
          
        )
                  
      )
      
    )
    
  )
            
)
