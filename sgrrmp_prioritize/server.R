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

# color palette for CSCI type
pal_typ <- colorFactor(
  palette = RColorBrewer::brewer.pal(11, 'Spectral'),#hue_pal()(100), 
  na.color = 'yellow',
  domain = paste0('Type', sprintf('%02d', seq(1:12)))
)

# server logic
server <- function(input, output, session) {
  
  # data to plot, polylines with score expections
  plot_ex <- reactive({
  
    # output
    out <- proc_all(exps_ex, scrs_ex, thrsh = 0.79, tails = 0.05)

    out
    
  })
  
  # data to plot, polylines with score expections
  dat <- reactive({
    
    # change percentile column name
    names(spat)[names(spat) %in% 'full0.50'] <- 'lns'
    
    # output
    out <- spat 
    out
    
  })
  
  # tails input as reactive, passed to multiple
  tails <- reactive({
    
    tails <- input$tails %>% 
      gsub('More certain|Less certain|\\(|\\)|\\s+', '', .) %>% 
      as.numeric
    tails <- 0.5 - tails
    return(tails)
    
  })
  
  # CSCI thresold reactive input
  thrsh <- reactive({
    
    input$thrsh %>%
      gsub('^.*\\(|\\)$', '', .) %>% 
      as.numeric
    
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
  
  # CSCI scores, take difference from expectation if difr is true
  csci <- reactive({
    
    jitr <- input$jitr
    
    # get csci difference
    out <- dat() %>% 
      select(COMID, lns) %>% 
      mutate(COMID = as.character(COMID)) %>% 
      left_join(scrs, ., by = 'COMID') %>% 
      mutate(csci_difr = csci - lns)
    
    # jitter scores with overlapping lat/lon
    if(jitr != 0){
      
      out <- out %>% 
        mutate(
          lat = ifelse(duplicated(lat), jitter(lat, factor = jitr), lat),
          long = ifelse(duplicated(long), jitter(long, factor = jitr), long)
        )
      
      # take average csci is jitter is zero
    } else {
      
      out <- out %>% 
        group_by(COMID, StationCode, lat, long) %>% 
        summarise(
          csci = mean(csci, na.rm = TRUE), 
          csci_difr = mean(csci_difr, na.rm = TRUE)
        ) %>% 
        ungroup
      
    }
    
    return(out)
    
  })
  
  # CSCI scores and stream condition expectations, maps only 
  scr_exp_map <- reactive({
    
    # process
    incl <- site_exp(spat, csci(), thrsh = thrsh(), tails = tails(), modls = 'full') %>% 
      select(-lat, -long) %>% 
      group_by(StationCode) %>% 
      nest
    
    # assign csci station locations for jittr
    out <- csci() %>% 
      select(StationCode, lat, long) %>% 
      group_by(StationCode) %>% 
      nest %>% 
      mutate(StationCode = factor(StationCode, levels = levels(incl$StationCode))) %>% 
      left_join(incl, ., by = 'StationCode') %>% 
      unnest
    
    # add additional perf column for multicolor by strcls (pal_prf)
    out <- get_perf_mlt(out)
    
    return(out)
    
  })
  
  # CSCI scores and stream condition expectations, not on maps
  # these data are never averaged by station averaged for CSCI
  scr_exp <- reactive({
    
    # process
    incl <- site_exp(spat, scrs, thrsh = thrsh(), tails = tails(), modls = 'full')
    
    # add additional perf column for multicolor by strcls (pal_prf)
    out <- get_perf_mlt(incl)
    
    return(out)
    
  })
  
  # site priorities from user selections
  scr_pri <- reactive({

    out <- get_pri_inp(input, plot_ex(), scr_exp_map()) 
    return(out)
    
  })
  
  # all priority, type counts
  allcnts <- reactive({
    
    # to join to get all priority categories (for those with zero)
    allpri <- data.frame(Priority = c('Protect', 'Monitor', 'Restore', 'Do nothing'), stringsAsFactors = F)
    
    # to join to get all types (for those with zero)
    alltyp <- data.frame(Type = paste0('Type', sprintf('%02d', seq(1, 12))), stringsAsFactors = F)
    
    # get priority counts, join with allpri for all priority categories
    out <- scr_pri() %>% 
      unnest %>% 
      rename(Type = typelv) %>% 
      group_by(Priority, Type) %>% 
      nest %>% 
      mutate(n = map(data, nrow)) %>%
      select(-data) %>% 
      left_join(allpri, ., by = 'Priority') %>% 
      group_by(Priority) %>% 
      nest %>% 
      mutate(data = map(data, ~ left_join(alltyp, .x, by = 'Type'))) %>% 
      unnest %>% 
      mutate(
        n = map(n, ~ ifelse(is.null(.x), 0, .x))
      )
    
    return(out)
    
  })
  
  # priority only counts, across typs
  output$cnts <- reactive({

    out <- allcnts() %>% 
      select(Priority, n) %>% 
      unnest %>% 
      group_by(Priority) %>% 
      summarise(n = sum(n)) %>% 
      mutate(n = as.character(n)) %>% 
      deframe %>% 
      as.list
    names(out)[names(out) == 'Do nothing'] <- 'Donothing'
    
    return(out)
    
  })
  
  # type only counts, across priorities
  output$typs <- reactive({

    out <- allcnts() %>% 
      select(Type, n) %>% 
      unnest %>% 
      group_by(Type) %>% 
      summarise(n = sum(n)) %>% 
      mutate(n = as.character(n)) %>% 
      deframe %>% 
      as.list
    
    return(out)
    
  })
  
  # # type only counts, across priorities
  # output$cnts_pro <- reactive({
  #   browser()
  #   out <- allcnts() %>% 
  #     filter(Priority %in% 'Protect') %>% 
  #     unnest %>% 
  #     group_by(Type)
  #   
  #   return(out)
  #   
  # })
  
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
    
    p <- ggplot(plot_ex(), aes(x = typelv)) + 
      geom_errorbar(aes(ymin = minv, ymax = maxv, colour = `Stream class`), width = 0, size = 2, alpha = 0.2) + 
      geom_errorbar(aes(ymin = minv_qt, ymax = maxv_qt, colour = `Stream class`), width = 0, size = 2, alpha = 0.7) + 
      geom_point(aes(y  = `CSCI score`, fill = `Relative\nperformance`), shape = 21, size = 7, alpha = 0.8) +
      geom_hline(yintercept = 0.79, linetype = 'dashed') +
      scale_colour_manual(values = pal_exp(levels(plot_ex()$`Stream class`)), 
                          guide = guide_legend(direction = 'vertical', title.position = 'left')) +
      scale_fill_manual(values = pal_prf(levels(plot_ex()$`Relative\nperformance`)), 
                        guide = guide_legend(ncol = 3, direction = 'vertical', title.position = 'left')) +
      scale_x_discrete(limits = rev(levels(plot_ex()$typelv))) +
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
  
  # non-reactive base map, do nothing priority
  output$bs_don <- renderLeaflet(

    leaflet(scrs) %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      syncWith('maps')

  )

  # non-reactive base map, protect priority
  output$bs_pro <- renderLeaflet(

    leaflet(scrs) %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      syncWith('maps')

  )

  # non-reactive base map, monitor priority
  output$bs_mon <- renderLeaflet(
    
    leaflet(scrs) %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      syncWith('maps')
    
  )
  
  # non-reactive base map, restore priority
  output$bs_res <- renderLeaflet(
    
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
    scr_pri <- scr_pri()

    # get seperate priorities from scr_pri_map
    dat_pro <- filter(scr_pri, Priority %in% 'Protect')$value
    dat_mon <- filter(scr_pri, Priority %in% 'Monitor')$value
    dat_res <- filter(scr_pri, Priority %in% 'Restore')$value
    dat_don <- filter(scr_pri, Priority %in% 'Do nothing')$value
    
    # base protect map
    pri_pro <- leafletProxy("bs_pro", data = dat_exp) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>% 
      addLegend("topright", pal = pal_exp, values = ~strcls,
                title = "Expected classification (lines)",
                opacity = 1
      ) %>% 
      addPolylines(opacity = 1, weight = lnsz, color = ~pal_exp(strcls), 
                   label = ~paste0(COMID, ', Stream class:', strcls)
      )
    
    # base monitor map
    pri_mon <- leafletProxy("bs_mon", data = dat_exp) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>% 
      addPolylines(opacity = 1, weight = lnsz, color = ~pal_exp(strcls), 
                   label = ~paste0(COMID, ', Stream class:', strcls)
      ) 
    
    # base restore map
    pri_res <- leafletProxy("bs_res", data = dat_exp) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>% 
      addPolylines(opacity = 1, weight = lnsz, color = ~pal_exp(strcls), 
                   label = ~paste0(COMID, ', Stream class:', strcls)
      )
    
    # base do nothing map
    pri_don <- leafletProxy("bs_don", data = dat_exp) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>% 
      addPolylines(opacity = 1, weight = lnsz, color = ~pal_exp(strcls), 
                   label = ~paste0(COMID, ', Stream class:', strcls)
      )
    
    
    # add protect points if not empty
    if(length(dat_pro) > 0){
      
      pri_pro <- pri_pro %>% 
        addCircleMarkers(data = dat_pro[[1]], lng = ~long, lat = ~lat, radius = ptsz, weight = 0.9, fillOpacity = 0.9,
                         label = ~paste0(StationCode, ', CSCI: ', round(csci, 2), ', ', perf_mlt, ', ', typelv),
                         fillColor = ~pal_typ(typelv), color = 'black'
        ) %>% 
        addLegend("topright", pal = pal_typ, values = dat_pro[[1]]$typelv,
                  title = "Site type (points)",
                  opacity = 1
        )
      
    }
    
    # add monitor points if not empty
    if(length(dat_mon) > 0){
      
      pri_mon <- pri_mon %>% 
        addCircleMarkers(data = dat_mon[[1]], lng = ~long, lat = ~lat, radius = ptsz, weight = 0.9, fillOpacity = 0.9,
                         label = ~paste0(StationCode, ', CSCI: ', round(csci, 2), ', ', perf_mlt, ', ', typelv),
                         fillColor = ~pal_typ(typelv), color = 'black'
        ) %>% 
        addLegend("topright", pal = pal_typ, values = dat_mon[[1]]$typelv,
                  title = "Site type (points)",
                  opacity = 1
        )
      
    }
    
    # add restore points if not empty
    if(length(dat_res) > 0){
      
      pri_res <- pri_res %>% 
        addCircleMarkers(data = dat_res[[1]], lng = ~long, lat = ~lat, radius = ptsz, weight = 0.9, fillOpacity = 0.9,
                         label = ~paste0(StationCode, ', CSCI: ', round(csci, 2), ', ', perf_mlt, ', ', typelv),
                         fillColor = ~pal_typ(typelv), color = 'black'
        ) %>% 
        addLegend("topright", pal = pal_typ, values = dat_res[[1]]$typelv,
                  title = "Site type (points)",
                  opacity = 1
        )
    }
    
    # add do nothing points if not empty
    if(length(dat_don) > 0){
      
      pri_don <- pri_don %>% 
        addCircleMarkers(data = dat_don[[1]], lng = ~long, lat = ~lat, radius = ptsz, weight = 0.9, fillOpacity = 0.9,
                         label = ~paste0(StationCode, ', CSCI: ', round(csci, 2), ', ', perf_mlt, ', ', typelv),
                         fillColor = ~pal_typ(typelv), color = 'black'
        ) %>% 
        addLegend("topright", pal = pal_typ, values = dat_don[[1]]$typelv,
                            title = "Site type (points)",
                            opacity = 1
                  )

    }
    
    # sync the maps
    combineWidgets(pri_pro, pri_mon, pri_res, pri_don)

  })
   
}
