library(tidyverse)

# calvin's scores
scrs <- read.csv('ignore/csci_toread.csv') %>% 
  mutate(StationCode = as.character(StationCode))

# rafis comid, station link
lnks <- read.csv('ignore/csci_061917.csv') %>% 
  select(StationCode, COMID) %>% 
  mutate(
    StationCode = as.character(StationCode),
    COMID = as.character(COMID)
    ) %>% 
  unique

# statewide comid by reach expectation
load(file = 'data/comid_statewide.RData')
exps <- comid %>% 
  select(COMID, core0.50) %>% 
  mutate(COMID = as.character(COMID))

# combine 
out <- scrs %>% 
  left_join(lnks, by = 'StationCode') %>% 
  left_join(exps, by = 'COMID') %>% 
  mutate(CSCIdiff = CSCI - core0.50)

# save
write.csv(out, file = 'ignore/CSCI_diff.csv', row.names = F)
