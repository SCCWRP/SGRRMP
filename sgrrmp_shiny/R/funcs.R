library(tidyverse)
library(sf)
library(stringr)

load(file = 'data/nhd.sgr.unfort.Rdata')

thrsh <- 0.79
tails <- 0.05

lns_sf <- nhd.sgr %>% 
  st_as_sf

dat <- lns_sf
st_geometry(dat) <- NULL
dat <- dat %>% 
  select(matches('^COMID$|^full0')) %>% 
  gather('var', 'val', -COMID) %>% 
  arrange(COMID, var) %>% 
  group_by(COMID) %>% 
  nest %>% 
  mutate(strcls = map(data, function(x){
    
    # return NA if any zero values in predictions
    if(any(x$val == 0)){
      
      cls <- NA
      return(cls)
      
    } 
    
    # get quantile labels to filter
    lovl <- 100 * tails
    hivl <- 100 * (1 -  tails) 
    vls <- c(lovl, hivl) %>% 
      str_pad(2, pad = '0') %>% 
      paste0('full0_', .)
    
    # filter by quantile labels, get values
    rngs <- x %>% 
      filter(var %in% vls) %>% 
      .$val
    
    # find if above/below or covering thrsh
    cls <- findInterval(thrsh, rngs)
    
    return(cls)
   
  }))
