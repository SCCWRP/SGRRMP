library(shiny)
library(sf)
library(tidyverse)
library(leaflet)
library(mapview)
library(stringr)
library(scales)
library(leaflet.minicharts)
library(manipulateWidget)
library(RColorBrewer)
source('R/funcs.R')

prj <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
lnsz <- 1.25
ptsz <- 4.5

# color palette for stream expectations
pal_exp <- colorFactor(
  palette = RColorBrewer::brewer.pal(9, 'Paired')[c(2, 1, 5, 6)],
  na.color = 'yellow',
  levels = c('likely unconstrained', 'possibly unconstrained', 'possibly constrained', 'likely constrained'))

# icons for map, created externally
crsz <- 11
trsz <- 15
mapicons <- iconList(
  `over scoring (lu)` = makeIcon("www/overlu.png", trsz, trsz),
  `expected (lu)` = makeIcon("www/explu.png", crsz, crsz),
  `under scoring (lu)`= makeIcon("www/underlu.png", trsz, trsz),
  `over scoring (pu)` = makeIcon("www/overpu.png", trsz, trsz),
  `expected (pu)` = makeIcon("www/exppu.png", crsz, crsz),
  `under scoring (pu)`= makeIcon("www/underpu.png", trsz, trsz),
  `over scoring (pc)` = makeIcon("www/overpc.png", trsz, trsz),
  `expected (pc)` = makeIcon("www/exppc.png", crsz, crsz),
  `under scoring (pc)`= makeIcon("www/underpc.png", trsz, trsz),
  `over scoring (lc)` = makeIcon("www/overlc.png", trsz, trsz),
  `expected (lc)` = makeIcon("www/explc.png", crsz, crsz),
  `under scoring (lc)`= makeIcon("www/underlc.png", trsz, trsz)
)

# have to reassign sgr data for reactives
data(scrs)
scrs_tmp <- scrs
data(spat)
spat_tmp <- spat

# server logic
server <- function(input, output, session) {
  
  # spatial polylines from watershed selection
  spat <- reactive({
    
    na.omit(spat_tmp)
    
  })
  
  # csci scores from watershed selection
  scrs <- reactive({
  
    na.omit(scrs_tmp)
    
  })
  
  # base mapview
  scrs_mv1 <- reactive({
    
    scrs() %>% 
      st_as_sf(coords = c('long', 'lat')) %>% 
      st_set_crs(prj) %>% 
      mapview(layer.name = 'reset') %>% 
      .@map
    
  })

  # data to plot, polylines with score expections
  dat <- reactive({
    
    out <- spat()
    # change percentile column name
    names(out)[names(out) %in% 'full0.50'] <- 'lns'
    
    # output
    out
    
  })
  
  # tails input as reactive, passed to multiple
  tlinp <- reactive({
    
    tails <- input$tails %>% 
      gsub('More certain|Less certain|\\(|\\)|\\s+', '', .) %>% 
      as.numeric
    tails <- 0.5 - tails
    return(tails)
    
  })
  
  # data to plot, polylines with condition expectations
  dat_exp <- reactive({
    
    # get biological condition expectations
    cls <- getcls2(spat(), thrsh = thrsh(), tails = tlinp(), modls = 'full')
    
    # join with spatial data
    out <- spat() %>% 
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
      left_join(scrs(), ., by = 'COMID') %>% 
      mutate(csci_difr = csci - lns)
    
    # jitter scores with overlapping lat/lon
    if(jitr){
      
      out <- out %>% 
        mutate(
          lat = ifelse(duplicated(lat), jitter(lat, factor = 300), lat),
          long = ifelse(duplicated(long), jitter(long, factor = 300), long)
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
    incl <- site_exp(spat(), csci(), thrsh = thrsh(), tails = tlinp(), modls = 'full') %>% 
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

  # CSCI thresold reactive input
  thrsh <- reactive({
    
    input$thrsh %>%
      gsub('^.*\\(|\\)$', '', .) %>% 
      as.numeric
    
  })
  
  ######
  # maps
  
  # non-reactive base map, condition expectations
  output$map_exp <- renderLeaflet(scrs_mv1())
  
  ##
  # reactive maps, all steps
  observe({
    
    # reactives
    dat_exp <- dat_exp()
    scr_exp_map <- scr_exp_map()
    
    # condition expectations
    exp_cls <- leafletProxy("map_exp", data = dat_exp) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls()%>%
      addLegend("topright", pal = pal_exp, values = ~strcls,
                title = "Reach classification",
                opacity = 1, na.label = "not in StreamCat"
      ) %>%
      addPolylines(opacity = 1, weight = lnsz, color = ~pal_exp(strcls),
                   label = ~paste0(COMID, ', Stream class:', strcls)
      ) %>%
      addMarkers(data = scr_exp_map, lng = ~long, lat = ~lat,
                 label = ~paste0(StationCode, ', CSCI: ', as.character(round(csci, 2)), ', ', perf_mlt),
                 icon = ~mapicons[perf_mlt]
                 
      )
    
  })
  
}