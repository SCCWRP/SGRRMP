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

# # watersheds to select from data folder
# shds <- list.files('data') %>% 
#   gsub('\\.RData$|^scrs_|^spat_', '', .) %>% 
#   unique

# column padding global
pad <- 'padding:0px;'

# Define UI for application
shinyUI(fluidPage(
  
  theme = 'styles.css',
  tags$head(includeScript('google-analytics.js')),
  useShinyjs(),
  useShinyCustom(slider_delay = '1500'),

  HTML('<br></br>'),
  
  # master widgets    
  column(width = 12, 
         
         # select CSCI threshold, master
         column(width = 4,    
                sliderTextInput(
                  inputId = "thrsh",
                  label = h6("CSCI reference threshold:"),
                  grid = FALSE,
                  force_edges = TRUE,
                  selected = '10% (0.79)',
                  choices = c('1% (0.63)', '10% (0.79)', '30% (0.92)'),
                  width = '400px'
                )
         ),
         
         # selected tails, master
         column(width = 4,
                sliderTextInput(
                  inputId = "tails", 
                  label = h6("Confidence range (+/-):"),  
                  grid = FALSE, 
                  force_edges = TRUE,
                  choices = c('More certain (0.45)', '0.40', '0.35', '0.30', '0.25', '0.20', '0.15', '0.10', 'Less certain (0.05)'), 
                  width = '400px'
                )
         ), 
         
         # apply jitter 
         column(width = 4, 
                # jitr switch   
                materialSwitch('jitr', 
                               label = h6(HTML('Show individual samples at each site:<br/><br/></br>')), 
                               status = 'primary',
                               right = F, 
                               width = '400px'
                )
                
         )
         
  ),
  
  # map output
  column(width = 12,
    
    leafletOutput('map_exp', width = '100%', height = 550), 
    h3()
        
  ) 
                       
))



