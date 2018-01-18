library(tidyverse)
library(sf)

prstr <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

load(file = 'data/comid_statewide.RData')

# flow lines
nhd_flo <- st_read('S:/Spatial_Data/NHDPlus/NHDPlus18/Hydrography/nhdflowline.shp')

# station locations as sf
stat <- read.csv('C:/Users/Marcus.SCCWRP2K/Desktop/csci_toread.csv') %>% 
  select(LATITUDE, LONGITUDE, StationCode) %>% 
  st_as_sf(coords = c('LONGITUDE', 'LATITUDE'), crs = st_crs(nhd_flo))


flo_crds <- nhd_flo[1:5, ]
sta_crds <- stat[1:2, ]

dist <- geosphere::dist2Line(p = st_coordinates(sta_crds), line = st_coordinates(flo_crds)[, 1:2])

nrs <- st_as_sf(data.frame(dist), coords = c('lon', 'lat'), crs = st_crs(stat))

plot(st_geometry(flo_crds))
points(st_coordinates(nrs))
points(st_coordinates(sta_crds))

###
library(sp)
library(spdep)
example(columbus)
plot(columbus)

pts = locator(4,type="p")

spts = SpatialPoints(pts)
library(rgeos)
apply(gDistance(spts, columbus,byid=TRUE),2,min)
# library(sp)
# library(spdep)
# example(columbus)
# plot(columbus)
# 
# pts = locator(4,type="p")
# 
# spts = SpatialPoints(pts)
# library(rgeos)
# apply(gDistance(spts, columbus,byid=TRUE),2,min)
