library(tidyverse)
library(ggridges)
library(leaflet)
library(broom)
library(scales)

set.seeed(1232)
vals <- rnorm(1e5, 0.79, 0.5) %>% 
  scales::rescale(c(0.3, 1))
toplo <- data.frame(
  sites = 'a', scrs = vals
)

ggplot(toplo, aes(scrs, sites)) + 
  geom_density_ridges2() + 
  scale_y_discrete('Likelihood') + 
  scale_x_continuous('CSCI score') + 
  theme_minimal() + 
  theme(axis.text.y = element_blank())
  
toplo <- list(
  a = rescale(vals, c(0.1, 0.7)),
  b = rescale(vals, c(0.3, 0.9)), 
  c = rescale(vals, c(0.6, 1.1))
  ) %>% 
  enframe('sites', 'scrs') %>% 
  unnest %>% 
  mutate(sites = paste('Stream', sites))
  
ggplot(toplo, aes(scrs, sites, group = sites)) + 
  geom_density_ridges2() + 
  scale_y_discrete('Likelihood') + 
  scale_x_continuous('CSCI score') + 
  theme_minimal()

ggplot(toplo, aes(scrs, sites, group = sites)) + 
  geom_density_ridges2() + 
  scale_y_discrete('Likelihood') + 
  scale_x_continuous('CSCI score') + 
  theme_void()

data(spat)
pal <- colorNumeric(
  palette = c('#d7191c', '#abd9e9', '#2c7bb6'),
  na.color = 'yellow',
  domain = c(0.15, 1.3)
  )
toplo <- select(spat, COMID, full0.05, full0.50, full0.95) %>% 
  st_sf %>%
  as('Spatial')
tojn <- toplo@data %>% 
  mutate(id = row.names(.))
toplo <- toplo %>% 
  tidy() %>% 
  left_join(tojn, by = 'id') %>% 
  gather('var', 'CSCI predictions', matches('^full')) %>% 
  mutate(var = factor(var, 
                      levels = c('full0.05', 'full0.50', 'full0.95'), 
                      labels = c('Low', 'Median', 'High')
  ))


ggplot(toplo, aes(x = long, y = lat, group = group, colour = `CSCI predictions`)) + 
  geom_path() + 
  facet_wrap(~var) + 
  coord_equal() + 
  theme_void() + 
  theme(legend.position = 'top') +
  scale_colour_gradientn(colors = c('#d7191c', '#abd9e9', '#2c7bb6'))
  

# from shiny

ggplot(toplo1, aes(y = StationCode, x = val)) + 
  geom_line(data = toplo2, aes(x = val, colour = `Stream Class`), alpha = 0.1, size = 2) +
  geom_line(aes(colour = `Stream Class`), alpha = 0.6, size = 2) + 
  theme_bw(base_family = 'serif', base_size = 18) +
  theme(
    axis.text.y = element_text(size = 6), 
    legend.position = 'none'
  ) +
  scale_x_continuous('CSCI') +
  scale_y_discrete('Stream reach') +
  scale_colour_manual(values = pal_exp(levels(toplo1$`Stream Class`))) +
  # geom_point(aes(x = csci, fill = `Relative\nperformance`), shape = 21, size = 4, alpha = 0.8) +
  geom_vline(xintercept = thrsh(), linetype = 'dashed', size = 1) +
  scale_fill_manual(values = pal_prf(levels(toplo1$`Relative\nperformance`)), na.value = 'yellow')

