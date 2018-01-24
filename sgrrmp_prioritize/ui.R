library(leaflet)
library(shinyjs)
library(shinyBS)
library(shinyCustom)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(rvest)

# last commit date
dt <- read_html('https://github.com/SCCWRP/SGRRMP/commits/master') %>% 
  html_nodes(".commit-group-title") %>% 
  html_text %>% 
  .[1] %>% 
  gsub('^.*Commits on (.*)\\n.*$', '\\1', .)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  theme = 'styles.css',
  useShinyjs(),
  useShinyCustom(slider_delay = '1500'),
  
  # Application title
  h1('Stream prioritization in the San Gabriel River Watershed', 
     style = "font-family: 'Volkhov';
        font-weight: 500; line-height: 1.1"),
  
  fluidRow(
  
    column(width = 3, img(src = "logo.jpg", width = '200px'), align = 'center', style = "margin-top: 0px;"),
    
    column(width = 9, 
      h5('This application can be used to explore stream and site classifications for the San Gabriel River Watershed.  Classications are based on the relationship of field CSCI scores at a site to biological expectations for the stream reach.  Expectations are based on user-defined parameters for CSCI thresholds and confidence in the biological expectation. Site classifications for CSCI scores are defined as over-performing, expected, and under-performing.  Stream reach expectations are defined as likely constrained, undetermined, or likely unconstrained. Last updated:', dt)
    ),

    column(width = 12, 
      h4('Created by Marcus W. Beck,', a('marcusb@sccwrp.org', href = 'mailto:marcusb@sccwrp.org'), ", Raphael D. Mazor,", a('raphaelm@sccwrp.org', href = 'mailto:raphaelm@sccwrp.org'), ", Scott Johnson,", a('scott@aquaticbioassay.com', href = 'mailto:scott@aquaticbioassay.com'), ", Peter R. Ode,", a('Peter.Ode@wildlife.ca.gov', href = 'mailto:Peter.Ode@wildlife.ca.gov'))
      ),
    
    column(width = 4, 
           
      # select CSCI threshold, master
      sliderTextInput(
        inputId = "thrsh",
        label = h6("CSCI reference threshold:"),
        grid = FALSE,
        force_edges = TRUE,
        selected = '10% (0.79)',
        choices = c('1% (0.63)', '10% (0.79)', '30% (0.89)'),
        width = '600px'
      )
      
    ),
    
    column(width = 4, 
           
      # selected tails, master
      sliderTextInput(
        inputId = "tails", 
        label = h6("Confidence range (+/-):"),  
        grid = FALSE, 
        force_edges = TRUE,
        choices = c('More certain (0.45)', '0.40', '0.35', '0.30', '0.25', '0.20', '0.15', '0.10', 'Less certain (0.05)'), 
        width = '600px'
      )   
      
    )

  ),
    
  tabsetPanel(
    
    tabPanel('Prioritize',
             
      # plot output
      column(width = 12,
            
        plotOutput('plo_exp', width = '100%', height = 450)
            
      ), 
        
      column(width = 1,
        radioButtons('Site 1', 'Site 1', choices = c('Protect', 'Monitor', 'Restore'), selected = 'Protect',
                    inline = FALSE, width = NULL)
      ), 
      
      column(width = 1,
        radioButtons('Site 2', 'Site 2', choices = c('Protect', 'Monitor', 'Restore'), selected = 'Monitor',
                    inline = FALSE, width = NULL)
      ),
      
      column(width = 1,
        radioButtons('Site 3', 'Site 3', choices = c('Protect', 'Monitor', 'Restore'), selected = 'Monitor',
                    inline = FALSE, width = NULL)
      ), 
    
      column(width = 1,
        radioButtons('Site 4', 'Site 4', choices = c('Protect', 'Monitor', 'Restore'), selected = 'Restore',
                    inline = FALSE, width = NULL)
      ), 
    
      column(width = 1,
        radioButtons('Site 5', 'Site 5', choices = c('Protect', 'Monitor', 'Restore'), selected = 'Protect',
                    inline = FALSE, width = NULL)
      ), 
      
      column(width = 1,
        radioButtons('Site 6', 'Site 6', choices = c('Protect', 'Monitor', 'Restore'), selected = 'Monitor',
                    inline = FALSE, width = NULL)
      ), 
      
      column(width = 1,
        radioButtons('Site 7', 'Site 7', choices = c('Protect', 'Monitor', 'Restore'), selected = 'Restore',
                    inline = FALSE, width = NULL)
      ), 
      
      column(width = 1,
        radioButtons('Site 8', 'Site 8', choices = c('Protect', 'Monitor', 'Restore'), selected = 'Restore',
                    inline = FALSE, width = NULL)
      ), 
      
      column(width = 1,
        radioButtons('Site 9', 'Site 9', choices = c('Protect', 'Monitor', 'Restore'), selected = 'Protect',
                    inline = FALSE, width = NULL)
      ), 
      
      column(width = 1,
        radioButtons('Site 10', 'Site 10', choices = c('Protect', 'Monitor', 'Restore'), selected = 'Monitor',
                    inline = FALSE, width = NULL)
      ), 
      
      column(width = 1,
        radioButtons('Site 11', 'Site 11', choices = c('Protect', 'Monitor', 'Restore'), selected = 'Monitor',
                    inline = FALSE, width = NULL)
      ), 
      
      column(width = 1,
        radioButtons('Site 12', 'Site 12', choices = c('Protect', 'Monitor', 'Restore'), selected = 'Monitor',
                    inline = FALSE, width = NULL)
      
      )
        
    ), 
    
    tabPanel('Maps', 
      
      h5("Move a slider to initialize maps..."),
      
      # select point radius
      column(width = 4,
            customSliderInput("pt_sz", 
                              label = h6("Point size:"), 
                              min = 0, 
                              max = 15,
                              value = 6, 
                              step = 1, 
                              width = '400px', 
                              ticks = FALSE
            )
      ),
      
      # select line size
      column(width = 4,
            customSliderInput("ln_sz", 
                              label = h6("Line size:"), 
                              min = 0, 
                              max = 5,
                              value = 1, 
                              step = 0.1, 
                              width = '400px', 
                              ticks = FALSE
            )
      
      ),
      
      column(width = 4,
             customSliderInput('jitr', 
                               label = h6("Jitter overlaps:"), 
                               min = 0, 
                               max = 500,
                               value = 0, 
                               step = 25, 
                               width = '400px', 
                               ticks = FALSE
             )
             
      ),
      
      # map_exp output
      column(width = 6,

         leafletOutput('map_exp', width = '100%', height = 550),
         h3()

      ) ,

      # map_pri output
      column(width = 6,

         leafletOutput('map_pri', width = '100%', height = 550),
         h3()

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

))
            

