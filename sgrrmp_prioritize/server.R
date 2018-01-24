library(shiny)
library(sf)
library(tidyverse)
library(leaflet)
library(stringr)
library(scales)
library(leaflet.minicharts)
library(manipulateWidget)
source('R/funcs.R')

# spatial comid data
load('data/spat.RData')

# csci scores at sites
load('data/scrs.RData')

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
  
  # CSCI thresold reactive input
  thrsh <- reactive({
    
    input$thrsh %>%
      gsub('^.*\\(|\\)$', '', .) %>% 
      as.numeric
    
  })
  
  # tails input as reactive, passed to multiple
  tails <- reactive({
    
    tails <- input$tails %>% 
      gsub('More certain|Less certain|\\(|\\)|\\s+', '', .) %>% 
      as.numeric
    tails <- 0.5 - tails
    return(tails)
    
  })
  
  # data to plot, polylines with score expections
  plot_ex <- reactive({
  
    # output
    out <- proc_all(exps_ex, scrs_ex, thrsh = thrsh(), tails = tails())

    out
    
  })
  
  # data to plot, polylines with condition expectations
  dat_exp <- reactive({
    
    # get biological condition expectations
    cls <- getcls2(spat, thrsh = thrsh(), tails = tails(), modls = 'full')
    
    # join with spatial data
    out <- spat %>% 
      left_join(cls, by = 'COMID')
    
    out
    
  })
  
  # CSCI scores and stream condition expectations, maps only 
  scr_exp_map <- reactive({
    
    jitr <- input$jitr
    
    # get csci spatially
    csci_dat <- spat %>% 
      select(COMID) %>% 
      mutate(COMID = as.character(COMID)) %>% 
      left_join(scrs, ., by = 'COMID') 
    
    # jitter scores with overlapping lat/lon
    if(jitr != 0){
      
      csci_dat <- csci_dat %>% 
        mutate(
          lat = ifelse(duplicated(lat), jitter(lat, factor = jitr), lat),
          long = ifelse(duplicated(long), jitter(long, factor = jitr), long)
        )
      
      # take average csci is jitter is zero
    } else {
      
      csci_dat <- csci_dat %>% 
        group_by(COMID, StationCode, lat, long) %>% 
        summarise(
          csci = mean(csci, na.rm = TRUE)
        ) %>% 
        ungroup
      
    }
    
    # process
    incl <- site_exp(spat, csci_dat, thrsh = thrsh(), tails = tails(), modls = 'full') %>% 
      select(-lat, -long) %>% 
      group_by(StationCode) %>% 
      nest
    
    # assign csci station locations for jittr
    out <- csci_dat %>% 
      select(StationCode, lat, long) %>% 
      group_by(StationCode) %>% 
      nest %>% 
      mutate(StationCode = factor(StationCode, levels = levels(incl$StationCode))) %>% 
      left_join(incl, ., by = 'StationCode') %>% 
      unnest
    
    # add additional perf column for multicolor by strcls (pal_prf)
    out <- get_perf_mlt(out) %>% 
      select(lat, long, perf_mlt, csci, StationCode, typelv, strcls, perf, typeoc)
    
    return(out)
    
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
  
  # expectation plot
  output$plo_exp <- renderPlot({

    ggplot(plot_ex(), aes(x = Site)) + 
      geom_errorbar(aes(ymin = minv, ymax = maxv, colour = `Stream class`), width = 0, size = 2, alpha = 0.2) + 
      geom_errorbar(aes(ymin = minv_qt, ymax = maxv_qt, colour = `Stream class`), width = 0, size = 2, alpha = 0.7) + 
      geom_point(aes(y  = `CSCI score`, fill = `Relative\nperformance`), shape = 21, size = 7, alpha = 0.8) +
      geom_text(y = 0.35, aes(label = typelv)) +
      scale_y_continuous(limits = c(0.35, max(plot_ex()$`CSCI score`))) +
      geom_hline(yintercept = thrsh(), linetype = 'dashed') +
      scale_colour_manual(values = pal_exp(levels(plot_ex()$`Stream class`)), 
                          guide = guide_legend(direction = 'vertical', title.position = 'left')) +
      scale_fill_manual(values = pal_prf(levels(plot_ex()$`Relative\nperformance`)), 
                        guide = guide_legend(ncol = 3, direction = 'vertical', title.position = 'left')) +
      theme_minimal(base_family = 'serif', base_size = 18) +
      theme(
        axis.title.x = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line.x = element_line(), 
        axis.line.y = element_line(), 
        axis.ticks = element_line(), 
        legend.position = 'top'
      )
    
  })
  
  # non-reactive base map, condition expectations
  output$map_exp <- renderLeaflet(

    leaflet(scrs) %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      syncWith('maps')

  )

  # non-reactive base map
  output$map_pri <- renderLeaflet(

    leaflet(scrs) %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      syncWith('maps')

  )

  ##
  # reactive maps
  observe({

    # other inputs
    ptsz <- input$pt_sz
    lnsz <- input$ln_sz

    # reactives
    dat_exp <- dat_exp()
    scr_exp_map <- scr_exp_map()
    scr_pri <- scr_pri()

    # condition expectations
    exp_bs <- leafletProxy("map_exp", data = dat_exp) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolylines(opacity = 1, weight = lnsz, color = ~pal_exp(strcls),
                   label = ~paste0(COMID, ', Stream class:', strcls)
      ) %>%
      addCircleMarkers(data = scr_exp_map, lng = ~long, lat = ~lat, radius = ptsz, weight = 0.9, fillOpacity = 0.9,
                       label = ~paste0(StationCode, ', CSCI: ', as.character(round(csci, 2)), ', ', perf_mlt, ', ', typelv),
                       fillColor = ~pal_prf(perf_mlt), color = 'black'
      ) %>%
      addLegend("topright", pal = pal_exp, values = ~strcls,
                title = "Expected classification (lines)",
                opacity = 1
      ) %>%
      addLegend("topright", pal = pal_prf, values = scr_exp_map$perf_mlt,
                title = "CSCI performance (points)",
                opacity = 1
      )

    # condition expectations
    pri_bs <- leafletProxy("map_pri", data = dat_exp) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolylines(
        opacity = 1, weight = lnsz, label = ~ COMID, color = 'grey'
      ) %>%
      addCircleMarkers(data = scr_pri, lng = ~long, lat = ~lat, radius = ptsz, weight = 0.9, fillOpacity = 0.9,
                       label = ~paste0(StationCode),
                       fillColor = ~pal_pri(Priority), color = 'black'
      ) %>%
      addLegend("topright", pal = pal_pri, values = scr_pri$Priority,
                title = "Priority",
                opacity = 1
      )

    # sync the maps
    combineWidgets(exp_bs, pri_bs)

  })
  
  # summary tables
  output$tab_sum <- DT::renderDataTable({

    # summary table by csci type          
    totab <- get_tab(scr_exp_map(), scr_pri(), thrsh = thrsh(), tails = tails())
 
    return(totab)
    
  }, rownames = F, options = list(dom = 't', pageLength = 12))
  
   
}
