library(sf)
library(tidyverse)

##
# update spatial comid data with model predictions

load('data/spatial_data/nhd.sgr.unfort.Rdata')
data(comid_prd)

# data as nhd.sgr
nhd.sgr <- st_as_sf(nhd.sgr)

# all_cid <- read.csv('P:/RaphaelMazor/SGRRMP/Revision_120117/comid.pred.modeled_120117.csv', 
#                     stringsAsFactors = F)
# save(all_cid, file = 'C:/Users/Marcus.SCCWRP2K/Desktop/all_cid.RData', compress = 'xz')
#
# load(file = 'Z:/RaphaelMazor/SGRRMP/Revision_120117/all_cid.RData')

# comids not in all_cid
spat <- nhd.sgr %>% 
  select(COMID, LENGTHKM) %>% 
  left_join(comid_prd, by = 'COMID')
save(spat, file = 'data/spat.RData', compress = 'xz')
save(spat, file = 'sgrrmp_classify/data/spat.RData', compress = 'xz')


  
