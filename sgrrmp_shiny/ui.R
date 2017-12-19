library(leaflet)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  theme = 'styles.css',
  useShinyjs(),
  
  # Application title
  titlePanel('Stream classification in the San Gabriel River Watershed'),
  
  fluidRow(
  
    column(width = 2, img(src = "sccwrp_logo.jpg", width = 160)),
    
    column(width = 10, 
      h5('This application can be used to explore stream and site classifications for the San Gabriel River Watershed.  Classications are based on the relationship of field CSCI scores at a site to biological expectations for the stream reach.  Expectations are based on ranges of predicted CSCI scores for a stream reach and user-defined parameters for CSCI tresholds and range cutoffs (tails).  The user may also choose the model used for predicting CSCI scores as the full (all predictors) or core (selected predictors) model.  Site classifications for CSCI scores are defined as over-performing, expected, and under-performing or as one of twelve types within each stream reach expectation.  Stream reach expectations are defined as likely constrained, undetermined, or likely unconstrained.')
    ),

    column(width = 12, 
      h4('Created by Marcus W. Beck,', a('marcusb@sccwrp.org', href = 'mailto:marcusb@sccwrp.org'), ", Raphael D. Mazor,", a('raphaelm@sccwrp.org', href = 'mailto:raphaelm@sccwrp.org'))
      ),
    
    column(width = 12, 
      
      # prediction model
      column(width = 6,
            
        h5('Pick the prediction model for expected CSCI scores at a stream reach.  The full model includes all predictors and the core model includes only selected predictors.')
             
      ),    
      
      # which site classification
      column(width = 6, 
             
        h5('Pick the site classifications to display.  The "perf" classification shows sites as over-performing, expected, or under-performing.  The "type" classification shows sites as one of twelve types based on the stream reach expectation, tails on the expectation, and the CSCI threshold.')
      
      ),
      
      column(width = 6, 
             
      selectInput(inputId  =  'modls',
                  label = h4('Prediction model:'),
                  choices = c('core', 'full'), selected = 'full')
      ),
      
      column(width = 6, 
        selectInput(inputId  =  'typs',
                   label = h4('Site classifications:'),
                   choices = c('perf', 'type'), selected = 'perf')

      )
      
    )
    
  ),
    
  tabsetPanel(
    
    tabPanel('Maps',

      h5('These two maps show stream reach classifications by COMID and CSCI scores at monitoring stations.  The', strong('left map'), 'shows the predicted CSCI scores for a COMID and measured CSCI score from field samples.  The', strong('right map'), 'shows the CSCI score expectation and the relative performance of a monitoring station.'),       
      # select point radius
      column(width = 12, 
             
        h5('These sliders control the map aesthetics. Use them to change the point/line sizes and apply a jitter for overlapping stations on the maps.'), 
        column(width = 4,
              sliderInput("pt_sz", 
                          label = h6("Point size:"), 
                          min = 0, 
                          max = 15,
                          value = 4, 
                          step = 1
              )
        ),
        
        # select line size
        column(width = 4,
              sliderInput("ln_sz", 
                          label = h6("Line size:"), 
                          min = 0, 
                          max = 5,
                          value = 1, 
                          step = 0.1
              )
        ),          
        
        column(width = 4,
               
               sliderInput('jitr', 
                           label = h6("Jitter overlaps:"), 
                           min = 0, 
                           max = 500,
                           value = 0, 
                           step = 25
               )
               
        )
        
      ),
         
      column(width = 6, 
        
        h5('These controls change the attributes in the left map.  The first slider controls which percentile of predicted CSCI scores is shown for the stream hydrography lines.  The toggle switch controls the CSCI scores shown at each sampling station.  The scores from field samples are shown when the switch is off and the differences of the scores with the predictions are shown when the switch is on.'),      
         
        # which csci percentile to show
        sliderInput('ptile',
                    label = h6("Percentile estimated score:"),
                    min = 0.05,
                    max = 0.95,
                    value = 0.5,
                    step = 0.05
        ),

        # show csci differences   
        materialSwitch('difr', 
                      label = h6('CSCI as difference:'), 
                      status = 'primary',
                      right = F
        )
        
      ), 
      
      column(width = 6,
          
        h5('These controls change the attributes in the right map.  The first slider controls the CSCI threshold and the second slider controls the tails of the predicted CSCI scores at each stream reach. Overlap of the tails with the CSCI threshold determines the performance classification of a reach and expected performance of the CSCI score at a sampling station.'),      
             
        # select CSCI threshold, master       
        sliderInput('thrsh', 
                   label = h6("CSCI threshold:"), 
                   min = 0, 
                   max = 1.5,
                   value = 0.79, 
                   step = 0.01

        ),
      
        # selected tails, master
        sliderInput('tails', 
                   label = h6("Expectation tails:"), 
                   min = 0.05, 
                   max = 0.45,
                   value = 0.05, 
                   step = 0.05
        )

      ),
    
      # map output
      column(width = 6,
        
        leafletOutput('map', width = '100%', height = 550), 
        h3()
             
      ),
     
      # map output
      column(width = 6,
             
             leafletOutput('map_exp', width = '100%', height = 550), 
             h3()

      ) 

    ),
    
    tabPanel('Plot',
      
      h5('This plot shows the CSCI score expectations for every stream reach with CSCI sampling stations.  The CSCI threshold and expectation tails define the reach expectation and the CSCI performance for the sampling stations.  Toggle the sliders to see how these change on the plot, including the maps and table in the other tabs.  The model and type selectors on the top will also change the plot.'),
             
      column(width = 3,
             
        # select CSCI threshold       
        sliderInput('thrsh2', 
                   label = h6("CSCI threshold:"), 
                   min = 0, 
                   max = 1.5,
                   value = 0.79, 
                   step = 0.01
          )
        
      ),   
      
      column(width = 3, 
             
        # selected tails
        sliderInput('tails2', 
                   label = h6("Expectation tails:"), 
                   min = 0.05, 
                   max = 0.45,
                   value = 0.05, 
                   step = 0.05
          )
        
      ),
    
      # plot output
      column(width = 12,
            
        plotOutput('plo_exp', width = '90%', height = 850)
            
      ) 
             
    ), 
        
    tabPanel('Table', 

      h5('This table summarizes the sampling station performance for CSCI scores shown in the map and plot in the other tabs. The "type" categories can be identified from the table. The model and type selectors on the top will also change the table.'),
             
      column(width = 3,
            
            # select CSCI threshold       
            sliderInput('thrsh3', 
                        label = h6("CSCI threshold:"), 
                        min = 0, 
                        max = 1.5,
                        value = 0.79, 
                        step = 0.01
            )
            
      ),   
      
      column(width = 3, 
            
            # selected tails
            sliderInput('tails3', 
                        label = h6("Expectation tails:"), 
                        min = 0.05, 
                        max = 0.45,
                        value = 0.05, 
                        step = 0.05
            )
            
      ),

      # table output
      column(width = 12, 
      
        DT::dataTableOutput('tab_sum'), 
        HTML('<p></p>')
                   
      )
    
    )
                  
  )
      
))
            

