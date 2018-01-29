library(leaflet)
library(shinyjs)
library(shinyBS)
library(shinyCustom)
library(shinydashboard)
library(shinyWidgets)
library(ShinyDash)
library(tidyverse)
library(rvest)
library(leaflet.extras)

# column padding
pad <- 'padding:4.1px;'

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
               pickerInput(inputId = "Site 1", label = NULL, choices = c('Protect', 'Monitor', 'Restore', 'Do nothing'),
                           selected = 'Do nothing',
                           options = list(`actions-box` = TRUE, size = 20, `selected-text-format` = "count > 3"), 
                           multiple = TRUE
               )
        ), 
                    
        div(style = pad,
               pickerInput(inputId = "Site 2", label = NULL, choices = c('Protect', 'Monitor', 'Restore', 'Do nothing'), 
                          selected = 'Do nothing',
                          options = list(`actions-box` = TRUE, size = 20, `selected-text-format` = "count > 3"), 
                          multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site 3", label = NULL, choices = c('Protect', 'Monitor', 'Restore', 'Do nothing'), 
                           selected = 'Do nothing',
                           options = list(`actions-box` = TRUE, size = 20, `selected-text-format` = "count > 3"), 
                           multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site 4", label = NULL, choices = c('Protect', 'Monitor', 'Restore', 'Do nothing'), 
                           selected = 'Do nothing',
                           options = list(`actions-box` = TRUE, size = 20, `selected-text-format` = "count > 3"), 
                           multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site 5", label = NULL, choices = c('Protect', 'Monitor', 'Restore', 'Do nothing'), 
                           selected = 'Do nothing',
                           options = list(`actions-box` = TRUE, size = 20, `selected-text-format` = "count > 3"), 
                           multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site 6", label = NULL, choices = c('Protect', 'Monitor', 'Restore', 'Do nothing'), 
                           selected = 'Do nothing',
                           options = list(`actions-box` = TRUE, size = 20, `selected-text-format` = "count > 3"), 
                           multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site 7", label = NULL, choices = c('Protect', 'Monitor', 'Restore', 'Do nothing'), 
                           selected = 'Do nothing',
                           options = list(`actions-box` = TRUE, size = 20, `selected-text-format` = "count > 3"), 
                           multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site 8", label = NULL, choices = c('Protect', 'Monitor', 'Restore', 'Do nothing'), 
                           selected = 'Do nothing',
                           options = list(`actions-box` = TRUE, size = 20, `selected-text-format` = "count > 3"), 
                           multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site 9", label = NULL, choices = c('Protect', 'Monitor', 'Restore', 'Do nothing'), 
                           selected = 'Do nothing',
                           options = list(`actions-box` = TRUE, size = 20, `selected-text-format` = "count > 3"), 
                           multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site 10", label = NULL, choices = c('Protect', 'Monitor', 'Restore', 'Do nothing'), 
                           selected = 'Do nothing',
                           options = list(`actions-box` = TRUE, size = 20, `selected-text-format` = "count > 3"), 
                           multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site 11", label = NULL, choices = c('Protect', 'Monitor', 'Restore', 'Do nothing'), 
                           selected = 'Do nothing',
                           options = list(`actions-box` = TRUE, size = 20, `selected-text-format` = "count > 3"), 
                           multiple = TRUE
               )
        ), 
        
        div(style = pad,
               pickerInput(inputId = "Site 12", label = NULL, choices = c('Protect', 'Monitor', 'Restore', 'Do nothing'), 
                           selected = 'Do nothing',
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

      # site counts
      column(width = 12,
             htmlWidgetOutput(
               outputId = 'cnts',
               HTML(paste('<h4>Total site counts: <b><span id="Protect"></span></b> protect, <b><span id="Monitor"></span></b> monitor, <b><span id="Restore"></span></b> restore, <b><span id="donothing"></span></b> do nothing</h4>'))
             )),
             
      # protect map
      column(width = 6,

        h3('Protect'),
        leafletOutput('bs_pro', width = '100%', height = 550),
        h3()

      ),
      
      # monitor map
      column(width = 6,
        
        h3('Monitor'), 
        leafletOutput('bs_mon', width = '100%', height = 550),
        h3()

      ),
      
      # restore map
      column(width = 6,
          
        h3('Restore'),
        leafletOutput('bs_res', width = '100%', height = 550),
        h3()
             
      ),
      
      # do nothing map
      column(width = 6,
             
        h3('Do nothing'),
        leafletOutput('bs_don', width = '100%', height = 550),
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
            

