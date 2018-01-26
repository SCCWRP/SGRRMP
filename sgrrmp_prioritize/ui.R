library(leaflet)
library(shinyjs)
library(shinyBS)
library(shinyCustom)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(rvest)

# column padding
pad <- 'padding:4.2px;'

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

        div(style = 'padding:12px;'),
        
        div(style = pad,
               pickerInput(inputId = "Site1", label = NULL, choices = c('Protect', 'Monitor', 'Restore'), 
                           options = list(`actions-box` = TRUE, size = 20, `selected-text-format` = "count > 3"), 
                           multiple = TRUE
               )
        ), 
                    
        div(style = pad,
               pickerInput(inputId = "Site2", label = NULL, choices = c('Protect', 'Monitor', 'Restore'), 
                 options = list(`actions-box` = TRUE, size = 20, `selected-text-format` = "count > 3"), 
                 multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site3", label = NULL, choices = c('Protect', 'Monitor', 'Restore'), 
                           options = list(`actions-box` = TRUE, size = 20, `selected-text-format` = "count > 3"), 
                           multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site4", label = NULL, choices = c('Protect', 'Monitor', 'Restore'), 
                           options = list(`actions-box` = TRUE, size = 20, `selected-text-format` = "count > 3"), 
                           multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site5", label = NULL, choices = c('Protect', 'Monitor', 'Restore'), 
                           options = list(`actions-box` = TRUE, size = 20, `selected-text-format` = "count > 3"), 
                           multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site6", label = NULL, choices = c('Protect', 'Monitor', 'Restore'), 
                           options = list(`actions-box` = TRUE, size = 20, `selected-text-format` = "count > 3"), 
                           multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site7", label = NULL, choices = c('Protect', 'Monitor', 'Restore'), 
                           options = list(`actions-box` = TRUE, size = 20, `selected-text-format` = "count > 3"), 
                           multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site8", label = NULL, choices = c('Protect', 'Monitor', 'Restore'), 
                           options = list(`actions-box` = TRUE, size = 20, `selected-text-format` = "count > 3"), 
                           multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site9", label = NULL, choices = c('Protect', 'Monitor', 'Restore'), 
                           options = list(`actions-box` = TRUE, size = 20, `selected-text-format` = "count > 3"), 
                           multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site10", label = NULL, choices = c('Protect', 'Monitor', 'Restore'), 
                           options = list(`actions-box` = TRUE, size = 20, `selected-text-format` = "count > 3"), 
                           multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site11", label = NULL, choices = c('Protect', 'Monitor', 'Restore'), 
                           options = list(`actions-box` = TRUE, size = 20, `selected-text-format` = "count > 3"), 
                           multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site12", label = NULL, choices = c('Protect', 'Monitor', 'Restore'), 
                           options = list(`actions-box` = TRUE, size = 20, `selected-text-format` = "count > 3"), 
                           multiple = TRUE
               )
        )
        
      ),
      # plot output
      column(width = 10,
             
             plotOutput('plo_exp', width = '100%', height = 800)
             
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

    )#,

    # tabPanel('Table', 
    #          
    #    # table output
    #    column(width = 12, 
    #           
    #       DT::dataTableOutput('tab_sum'), 
    #       HTML('<p></p>')
    #       
    #    )
    #          
    # )
                  
  )

))
            

