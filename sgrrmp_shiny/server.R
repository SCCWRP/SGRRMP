library(shiny)
library(sf)
library(tidyverse)
library(leaflet)
library(stringr)
source('R/funcs.R')

load('data/nhd.sgr.unfort.Rdata')
load('data/scrs.RData')

# data as nhd.sgr
nhd.sgr <- st_as_sf(nhd.sgr)

# color domain
dmn <- nhd.sgr %>% 
  select(matches('^full0_')) %>% 
  data.frame %>% 
  select(-geometry) %>% 
  gather('var', 'val') %>% 
  .$val %>% 
  c(., scrs$csci)

# color palette
pal <- colorNumeric(
  palette = c('#d7191c', '#abd9e9', '#2c7bb6'),
  na.color = 'yellow',
  domain = dmn)

# color palette for stream expectations
pal_exp <- colorFactor(
  palette = RColorBrewer::brewer.pal(9, 'Set1')[c(1, 2, 3)],
  na.color = 'yellow',
  domain = c('likely constrained', 'undetermined', 'likely unconstrained'))

# color palette for CSCI performance
pal_prf <- colorFactor(
  palette = c('white', 'blue', 'red'),
  na.color = 'yellow',
  domain = c('expected', 'over performing', 'under performing'))

# custom label format function
myLabelFormat = function(..., reverse_order = FALSE){ 
  if(reverse_order){ 
    function(type = 'numeric', cuts){ 
      cuts <- sort(cuts, decreasing = T)
    } 
  }else{
    labelFormat(...)
  }
}

# server logic
server <- function(input, output) {
  
  # data to plot, polylines with score expections
  dat <- reactive({

    # get polylines to plot
    ptile <- input$ptile %>% 
      paste0('full', .) %>% 
      gsub('\\.', '_', .)
    names(nhd.sgr)[names(nhd.sgr) %in% ptile] <- 'lns'
    
    # set zero values to NA
    out <- nhd.sgr %>% 
      mutate(
        lns = ifelse(lns == 0, NA, lns)
      )

    out
    
  })
  
  # data to plot, polylines with condition expectations
  dat_exp <- reactive({
    
    # inputs
    likes <- input$likes %>% as.numeric
    thrsh <- input$thrsh
    
    # get biological condition expectations
    cls <- getcls2(nhd.sgr, thrsh = thrsh, likes = likes)
  
    # join with spatial data
    out <- nhd.sgr %>% 
      left_join(cls, by = 'COMID')

    out
    
  })
  
  # CSCI scores and stream condition expectations
  scr_exp <- reactive({
    
    thrsh <- input$thrsh
    likes <- input$likes %>% as.numeric
    
    # process
    incl <- site_exp(nhd.sgr, scrs, thrsh, likes)
    
    return(incl)
    
  })

  # non-reactive base map
  output$map <- renderLeaflet(
    
    leaflet(scrs) %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>% 
      addProviderTiles(providers$CartoDB.Positron)
    
  )
  
  # non-reactive base map, condition expectations
  output$map_exp <- renderLeaflet(
    
    leaflet(scrs) %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>% 
      addProviderTiles(providers$CartoDB.Positron)
    
  )
  
  # reactive maps
  observe({
    
    # other inputs
    ptsz <- input$pt_sz
    lnsz <- input$ln_sz
    
    # score expectations
    leafletProxy("map", data = dat()) %>%
      clearMarkers() %>%
      clearShapes() %>% 
      clearControls() %>% 
      addPolylines(opacity = 1, weight = lnsz, color = ~pal(lns), 
                   label = ~paste('Likely score:', as.character(round(lns, 2)))
      ) %>% 
      addCircleMarkers(data = scrs, lng = ~long, lat = ~lat, radius = ptsz, weight = 0, fillOpacity = 0.8, 
                       label = ~paste('CSCI:', as.character(round(csci, 2))),
                       fillColor = ~pal(csci)
      ) %>% 
      addLegend("topright", pal = pal, values = ~lns,
                title = "Likely score",
                opacity = 1#, 
                # labFormat = myLabelFormat(reverse_order = T)
      )
    
    # condition expectations
    leafletProxy("map_exp", data = dat_exp()) %>%
      clearMarkers() %>%
      clearShapes() %>% 
      clearControls()%>% 
      addPolylines(opacity = 1, weight = lnsz, color = ~pal_exp(strcls), 
                   label = ~paste('Stream class:', strcls)
      ) %>% 
      addCircleMarkers(data = scrs, lng = ~long, lat = ~lat, radius = ptsz, weight = 0, fillOpacity = 0.8, 
                       label = ~paste('CSCI:', as.character(round(csci, 2))),
                       fillColor = ~pal(csci)
      ) %>% 
      addLegend("topright", pal = pal_exp, values = ~strcls,
                title = "Expected classification",
                opacity = 1
      )
    
  })
  
  # plot of csci scores and expectations by station code
  output$plo_exp <- renderPlot({

    thrsh <- input$thrsh
    
    # CSCI scores and expectations
    toplo1 <- scr_exp() %>% 
      select(COMID, StationCode, datcut, strcls, csci, perf) %>% 
      unnest %>% 
      rename(
        `Stream Class` = strcls,
        `Relative\nperformance` = perf
        )
    
    # total expected range
    toplo2 <- scr_exp() %>% 
      select(COMID, StationCode, data, strcls) %>% 
      unnest %>% 
      rename(`Stream Class` = strcls)
    
    # plot
    p <- ggplot(toplo1, aes(y = StationCode, x = val)) + 
      geom_line(data = toplo2, aes(x = val, colour = `Stream Class`), alpha = 0.1, size = 2) +
      geom_line(aes(colour = `Stream Class`), alpha = 0.6, size = 2) + 
      geom_point(aes(x = csci, fill = `Relative\nperformance`), shape = 21, size = 4, alpha = 0.4) +
      geom_vline(xintercept = thrsh, linetype = 'dashed', size = 1) +
      theme_bw(base_family = 'serif', base_size = 18) +
      theme(
        axis.text.y = element_text(size = 10)
      ) +
      scale_x_continuous('CSCI') +
      scale_colour_manual(values = pal_exp(levels(toplo1$`Stream Class`))) +
      scale_fill_manual(values = pal_prf(levels(toplo1$`Relative\nperformance`)), na.value = 'yellow')
    
    print(p)
    
  })
  
   
}
