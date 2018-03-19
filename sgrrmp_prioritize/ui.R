library(leaflet)
library(shinyjs)
library(shinyBS)
library(shinyCustom)
library(shinydashboard)
library(shinyWidgets)
library(ShinyDash)
library(tidyverse)
library(mapview)
library(rvest)

# column padding
pad <- 'padding:0px;'

# last commit date
dt <- read_html('https://github.com/SCCWRP/SGRRMP/commits/master') %>% 
  html_nodes(".commit-group-title") %>% 
  html_text %>% 
  .[1] %>% 
  gsub('^.*Commits on (.*)\\n.*$', '\\1', .)

# html text for type counts
typi <- paste0('Type', sprintf('%02d', seq(1, 16)))
typtxt <- NULL
for(i in typi){
  ctxt <- paste0('<b><span id="', i, '"></span></b> ', i)
  typtxt <- c(typtxt, ctxt)
}
typtxt <- paste(typtxt, collapse = ', ') %>% 
  paste('<h4>Site type counts:', ., '</h4>')

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
           h5('This application can be used to explore site prioritization for the San Gabriel River Watershed.  Site priorities are based on user selections for the twelve types of possible site classifications. The priority selections are plotted on the maps for all sites in the watershed.  Classifications are based on the relationship of field CSCI scores at a site to biological expectations for the stream reach.  Expectations are based on user-defined parameters for CSCI thresholds and confidence in the biological expectation. Site classifications for CSCI scores are defined as over-performing, expected, and under-performing.  Stream reach expectations are defined as likely constrained, undetermined, or likely unconstrained. Last updated:', dt)
    ),

    column(width = 12, 
      h4('Created by Marcus W. Beck,', a('marcusb@sccwrp.org', href = 'mailto:marcusb@sccwrp.org'), ", Raphael D. Mazor,", a('raphaelm@sccwrp.org', href = 'mailto:raphaelm@sccwrp.org'), ", Scott Johnson,", a('scott@aquaticbioassay.com', href = 'mailto:scott@aquaticbioassay.com'), ", Peter R. Ode,", a('Peter.Ode@wildlife.ca.gov', href = 'mailto:Peter.Ode@wildlife.ca.gov'))
      )

  ),
    
  tabsetPanel(
    
    tabPanel('Prioritize',
      
      # plot output legend
      column(width = 12,
            
        plotOutput('plo_leg', width = '100%', height = 100)
            
      ),

      # site priority selectors             
      column(width = 2, 

        div(style = 'padding:11px;'),
        
        div(style = pad,
               pickerInput(inputId = "Site 1", label = NULL, choices = c('Monitor', 'Protect', 'Restore'),
                           options = list(`actions-box` = TRUE, size = 20), selected = c('Monitor', 'Protect'),  
                           multiple = TRUE
               )
        ), 
                    
        div(style = pad,
               pickerInput(inputId = "Site 2", label = NULL, choices = c('Monitor', 'Protect', 'Restore'), 
                          options = list(`actions-box` = TRUE, size = 20), selected = c('Protect'),  
                          multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site 3", label = NULL, choices = c('Monitor', 'Protect', 'Restore'), 
                           options = list(`actions-box` = TRUE, size = 20), selected = c('Monitor'), 
                           multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site 4", label = NULL, choices = c('Monitor', 'Protect', 'Restore'), 
                           options = list(`actions-box` = TRUE, size = 20), selected = c('Monitor', 'Restore'),  
                           multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site 5", label = NULL, choices = c('Monitor', 'Protect', 'Restore'), 
                           options = list(`actions-box` = TRUE, size = 20), selected = c('Protect'),  
                           multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site 6", label = NULL, choices = c('Protect', 'Monitor', 'Restore'), 
                           options = list(`actions-box` = TRUE, size = 20), selected = c('Protect'), 
                           multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site 7", label = NULL, choices = c('Monitor', 'Protect', 'Restore'), 
                           options = list(`actions-box` = TRUE, size = 20), selected = c('Restore'), 
                           multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site 8", label = NULL, choices = c('Monitor', 'Protect', 'Restore'), 
                           options = list(`actions-box` = TRUE, size = 20), selected = c('Monitor', 'Restore'),  
                           multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site 9", label = NULL, choices = c('Monitor', 'Protect', 'Restore'), 
                           options = list(`actions-box` = TRUE, size = 20), selected = c('Protect'),  
                           multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site 10", label = NULL, choices = c('Monitor', 'Protect', 'Restore'), 
                           options = list(`actions-box` = TRUE, size = 20), selected = c('Protect'),  
                           multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site 11", label = NULL, choices = c('Monitor', 'Protect', 'Restore'), 
                           options = list(`actions-box` = TRUE, size = 20), selected = c('Restore'),  
                           multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site 12", label = NULL, choices = c('Monitor', 'Protect', 'Restore'), 
                           options = list(`actions-box` = TRUE, size = 20), selected = c('Monitor', 'Restore'),  
                           multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site 13", label = NULL, choices = c('Monitor', 'Protect', 'Restore'), 
                        options = list(`actions-box` = TRUE, size = 20), selected = c('Protect'),  
                        multiple = TRUE
            )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site 14", label = NULL, choices = c('Monitor', 'Protect', 'Restore'), 
                        options = list(`actions-box` = TRUE, size = 20), selected = c('Monitor', 'Restore'),  
                        multiple = TRUE
            )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site 15", label = NULL, choices = c('Monitor', 'Protect', 'Restore'), 
                        options = list(`actions-box` = TRUE, size = 20), selected = c('Restore'),  
                        multiple = TRUE
            )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site 16", label = NULL, choices = c('Monitor', 'Protect', 'Restore'), 
                        options = list(`actions-box` = TRUE, size = 20), selected = c('Restore'),  
                        multiple = TRUE
            )
        )
        
      ),
      # plot output
      column(width = 10,
             
        plotOutput('plo_exp', width = '100%', height = 900)
             
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
        
      ),
      
      # site priority counts
      column(width = 12,
             htmlWidgetOutput(
               outputId = 'cnts',
               HTML('<h3>Site priority counts: <b><span id="Protect"></span></b> protect, <b><span id="Monitor"></span></b> monitor, <b><span id="Restore"></span></b> restore</h3>')
             )),
             
      # site type counts
      column(width = 12,
             htmlWidgetOutput(
               outputId = 'typs',
               HTML(typtxt)
             )),
      
      # monitor map
      column(width = 4,
             
             h3('Monitor'), 
             leafletOutput('bs_mon', width = '100%', height = 550),
             h3()
             
      ),
      
      # protect map
      column(width = 4,

        h3('Protect'), 
        leafletOutput('bs_pro', width = '100%', height = 550),
        h3()

      ),
      
      # restore map
      column(width = 4,
          
        h3('Restore'),
        leafletOutput('bs_res', width = '100%', height = 550),
        h3()
             
      )

    )
                  
  )

))
            

