library(sf)
library(tidyverse)

##
# update spatial comid data with model predictions

load('data/spatial_data/nhd.sgr.unfort.Rdata')

# data as nhd.sgr
nhd.sgr <- st_as_sf(nhd.sgr)

library(tidyverse)

# all_cid <- read.csv('P:/RaphaelMazor/SGRRMP/Revision_120117/comid.pred.modeled_120117.csv', 
#                     stringsAsFactors = F)
# save(all_cid, file = 'C:/Users/Marcus.SCCWRP2K/Desktop/all_cid.RData', compress = 'xz')

load(file = 'P:/RaphaelMazor/SGRRMP/Revision_120117/all_cid.RData')

# comids not in all_cid
spat <- nhd.sgr %>% 
  select(COMID, LENGTHKM) %>% 
  left_join(all_cid, by = 'COMID')
save(spat, file = 'data/spat.RData', compress = 'xz')
save(spat, file = 'sgrrmp_shiny/data/spat.RData', compress = 'xz')


  
