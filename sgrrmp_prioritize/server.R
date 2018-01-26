library(shiny)
library(sf)
library(tidyverse)
library(leaflet)
library(stringr)
library(scales)
library(leaflet.minicharts)
library(manipulateWidget)
library(gridExtra)
source('R/funcs.R')

# spatial comid data
load('data/spat.RData')

# csci scores at sites
load('data/scrs.RData')

# globals
thrsh <- 0.79
tails <- 0.05

sts <- paste('Site', seq(1:12))

# example data, csci scores
scrs_ex <- data.frame(
  Site = factor(sts, levels = sts),
  csci = c(1.25, 1, 0.81, 0.7, 1.1, 0.9, 0.75, 0.5, 0.84, 0.75, 0.55, 0.4)
)

# example data, stream predictions
exps_ex <- data.frame(
  Site = factor(sts, levels = sts),
  minv = rep(c(0.84, 0.68, 0.43), each = 4),
  maxv = rep(c(1.14, 0.98, 0.73), each = 4), 
  stringsAsFactors = F
)

# color palette for stream expectations
pal_exp <- colorFactor(
  palette = RColorBrewer::brewer.pal(9, 'Set1')[c(2, 3, 1)],
  na.color = 'yellow',
  levels = c('likely unconstrained', 'undetermined', 'likely constrained'))

# color palette for CSCI performance
pal_prf <- colorFactor(
  palette = c(
    RColorBrewer::brewer.pal(9, 'Blues')[c(9, 6, 3)],
    RColorBrewer::brewer.pal(9, 'Greens')[c(9, 6, 3)],
    RColorBrewer::brewer.pal(9, 'Reds')[c(9, 6, 3)]
    ),
  na.color = 'yellow',
  levels = c(
    'over performing (lu)', 'expected (lu)', 'under performing (lu)',
    'over performing (u)', 'expected (u)','under performing (u)',  
    'over performing (lc)', 'expected (lc)', 'under performing (lc)')
  )


# color palette for stream expectations
pal_pri <- colorFactor(
  palette = RColorBrewer::brewer.pal(9, 'Greys')[c(8, 5, 2)],
  na.color = 'yellow',
  levels = c('Protect', 'Monitor', 'Restore'))

# server logic
server <- function(input, output, session) {
  
  # data to plot, polylines with score expections
  plot_ex <- reactive({
  
    # output
    out <- proc_all(exps_ex, scrs_ex, thrsh = 0.79, tails = 0.05)

    out
    
  })
  
  # data to plot, polylines with condition expectations
  dat_exp <- reactive({
    
    # get biological condition expectations
    cls <- getcls2(spat, thrsh = thrsh, tails = tails, modls = 'full')
    
    # join with spatial data
    out <- spat %>% 
      left_join(cls, by = 'COMID')
    
    out
    
  })
  
  # site priorities
  scr_pri <- reactive({
  
    # get plot example contstraints
    ex_jn <- plot_ex() %>% 
      select(Site, typelv)
    
    # site classications
    scr_exp_map <- scr_exp_map() %>%
      mutate(typelv = as.character(typelv))
    
    # format site priorities from input
    site_pri <- isolate(reactiveValuesToList(input)) %>%
      enframe %>%
      filter(grepl('^Site', name)) %>%
      unnest %>%
      rename(Site = name) %>%
      mutate(Site = factor(Site, levels = levels(ex_jn$Site))) %>%
      left_join(ex_jn, by = 'Site') %>%
      mutate(typelv = as.character(typelv)) %>%
      select(-Site) %>%
      left_join(scr_exp_map, ., by = 'typelv') %>%
      rename(Priority = value)
    
    # factor levels for prioritites
    lbs <- c('Protect', 'Monitor', 'Restore')
    lbs <- lbs[lbs %in% unique(site_pri$Priority)]
    site_pri <- site_pri %>% 
      mutate(Priority = factor(Priority, levels = lbs))

    return(site_pri)

  })
  
  # the selection plot
  siteplo <- reactive({
    
    mythm <- theme_minimal(base_family = 'serif', base_size = 18) +
      theme(
        axis.title.y = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line.x = element_line(), 
        axis.line.y = element_line(), 
        axis.ticks = element_line(), 
        legend.position = 'top'
      )
    
    p <- ggplot(plot_ex(), aes(x = Site)) + 
      geom_errorbar(aes(ymin = minv, ymax = maxv, colour = `Stream class`), width = 0, size = 2, alpha = 0.2) + 
      geom_errorbar(aes(ymin = minv_qt, ymax = maxv_qt, colour = `Stream class`), width = 0, size = 2, alpha = 0.7) + 
      geom_point(aes(y  = `CSCI score`, fill = `Relative\nperformance`), shape = 21, size = 7, alpha = 0.8) +
      geom_text(y = 0.35, aes(label = typelv)) +
      scale_y_continuous(limits = c(0.35, max(plot_ex()$`CSCI score`))) +
      geom_hline(yintercept = thrsh, linetype = 'dashed') +
      scale_colour_manual(values = pal_exp(levels(plot_ex()$`Stream class`)), 
                          guide = guide_legend(direction = 'vertical', title.position = 'left')) +
      scale_fill_manual(values = pal_prf(levels(plot_ex()$`Relative\nperformance`)), 
                        guide = guide_legend(ncol = 3, direction = 'vertical', title.position = 'left')) +
      scale_x_discrete(limits = rev(levels(plot_ex()$Site))) +
      mythm +
      coord_flip()
    
    # get legend
    pleg <- g_legend(p)
    p <- p +
      mythm %+replace%
      theme(legend.position = 'none')
      
    # output as list
    plo_ls <- list(pleg, p)
    return(plo_ls)
 
  })
  
  # expectation plot legend
  output$plo_leg <- renderPlot({
    grid.arrange(siteplo()[[1]])
  })
  
  # expectation plot
  output$plo_exp <- renderPlot({
    siteplo()[[2]]
  })
  
  # # non-reactive base map, condition expectations
  # output$bs_pro <- renderLeaflet(
  # 
  #   leaflet(scrs) %>%
  #     fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
  #     addProviderTiles(providers$CartoDB.Positron) %>%
  #     syncWith('maps')
  # 
  # )
  # 
  # # non-reactive base map
  # output$bs_mon <- renderLeaflet(
  # 
  #   leaflet(scrs) %>%
  #     fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
  #     addProviderTiles(providers$CartoDB.Positron) %>%
  #     syncWith('maps')
  # 
  # )
  # 
  # ##
  # # reactive maps
  # observe({
  # 
  #   # other inputs
  #   ptsz <- input$pt_sz
  #   lnsz <- input$ln_sz
  # 
  #   # reactives
  #   dat_exp <- dat_exp()
  #   scr_pri <- scr_pri()
  # 
  #   # condition expectations
  #   pri_pro <- leafletProxy("bs_pro", data = dat_exp) %>%
  #     clearMarkers() %>%
  #     clearShapes() %>%
  #     clearControls() %>%
  #     addPolylines(
  #       opacity = 1, weight = lnsz, label = ~ COMID, color = 'grey'
  #     ) %>%
  #     addCircleMarkers(data = scr_pri, lng = ~long, lat = ~lat, radius = ptsz, weight = 0.9, fillOpacity = 0.9,
  #                      label = ~paste0(StationCode),
  #                      fillColor = ~pal_pri(Priority), color = 'black'
  #     ) %>%
  #     addLegend("topright", pal = pal_pri, values = scr_pri$Priority,
  #               title = "Priority",
  #               opacity = 1
  #     )    
  # 
  #   # condition expectations
  #   pri_mon <- leafletProxy("bs_mon", data = dat_exp) %>%
  #     clearMarkers() %>%
  #     clearShapes() %>%
  #     clearControls() %>%
  #     addPolylines(
  #       opacity = 1, weight = lnsz, label = ~ COMID, color = 'grey'
  #     ) %>%
  #     addCircleMarkers(data = scr_pri, lng = ~long, lat = ~lat, radius = ptsz, weight = 0.9, fillOpacity = 0.9,
  #                      label = ~paste0(StationCode),
  #                      fillColor = ~pal_pri(Priority), color = 'black'
  #     ) %>%
  #     addLegend("topright", pal = pal_pri, values = scr_pri$Priority,
  #               title = "Priority",
  #               opacity = 1
  #     )
  # 
  #   # sync the maps
  #   combineWidgets(pri_pro, pri_mon)
  # 
  # })
  # 
  # # summary tables
  # output$tab_sum <- DT::renderDataTable({
  # 
  #   # summary table by csci type          
  #   totab <- get_tab(scr_exp_map(), scr_pri(), thrsh = thrsh, tails = tails)
  # 
  #   return(totab)
  #   
  # }, rownames = F, options = list(dom = 't', pageLength = 12))
  
   
}
