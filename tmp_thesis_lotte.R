# Libraries

library(etn)
library(tidyverse)
library(sf)
library(leaflet)
library(htmlwidgets)
library(webshot)


# Spatial data layers

bpns_boundaries <- st_read('~/spatial_data/bpns_boundaries/bpns_boundaries.shp', layer = 'bpns_boundaries')
sandbanks <- st_read('~/spatial_data/sea_floor_geomorphology/sea_floor_geomorphology.shp', layer = 'sea_floor_geomorphology')
owf_boundaries <- st_read('~/spatial_data/OWFs_shapefiles/RD20140320_art8_1_20140328.shp', layer = 'RD20140320_art8_1_20140328')


# Basic map

basic_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.VoyagerNoLabels) %>% 
  
  # Add BPNS boundaries
  addPolygons(data = bpns_boundaries, color = "#989898", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = F)) %>% 
  
  # Add sandbanks
  addPolygons(data = sandbanks, color = "#FFF2B3", weight = 0.5, smoothFactor = 1,
              opacity = 0.8, fillOpacity = 0.4,
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = F)) %>% 
  
  # Add OWF boundaries
  addPolygons(data = owf_boundaries, color = "#989898", weight = 1, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.4,
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = F))


# DB connection

con <- connect_to_etn(Sys.getenv("userid"), Sys.getenv("pwd"))


# DEPLOYMENTS -------------------------------------------------------------

# Deployments during years 2018 and 2019 (latest deployment)

deployments <- get_acoustic_deployments(con, open_only = FALSE)

deployments$deploy_year <- format(deployments$deploy_date_time, '%Y')

deployments_20182019 <- deployments %>%
  filter(deploy_year %in% c(2018, 2019)) %>%
  group_by(station_name) %>%
  arrange(desc(deploy_date_time)) %>%
  slice_head()


# Map of deployments in 2018 - 2019

map_receivers <- basic_map %>%
  setView(3.8, 51.4, zoom = 9) %>% 
  addCircleMarkers(data = deployments_20182019,
                   lng = ~deploy_longitude,
                   lat = ~deploy_latitude,
                   label = ~station_name,
                   popup = ~station_name,
                   radius = 3,
                   stroke = F,
                   fillOpacity = .6)

map_receivers


# Save figure as html and export

# saveWidget(map_plot, "tmp_map.html", selfcontained = FALSE)
# webshot("tmp_map.html", file = "Deployments_2018-2019.pdf", vwidth = 1200, vheight = 700, cliprect = NULL)


# ANIMAL DATA -----------------------------------------------------------

animal_db <- get_animals(con, scientific_name = 'Mustelus asterias')


# Map of tagging locations

map_shark_tagging <- basic_map %>%
  setView(3.8, 51.4, zoom = 10) %>% 
  addCircleMarkers(data = animal_db,
                   lng = ~release_longitude,
                   lat = ~release_latitude,
                   radius = 3,
                   stroke = F,
                   fillOpacity = .6)

map_shark_tagging


# ACOUSTIC DETECTIONS -----------------------------------------------------

detections_shark <- get_acoustic_detections(con, animal_project_code = "ADST-Shark")

# Receivers with detections
basic_map %>%
  setView(3.8, 51.4, zoom = 10) %>% 
  addCircleMarkers(data = detections_shark,
                   lng = ~deploy_longitude,
                   lat = ~deploy_latitude,
                   radius = 3,
                   stroke = F,
                   fillOpacity = .3)

detections_per_site <- detections_shark %>% group_by(deploy_latitude, deploy_longitude) %>% count() %>% arrange(desc(n))

basic_map %>%
  setView(3.8, 51.4, zoom = 10) %>% 
  addCircleMarkers(data = detections_per_site,
                   lng = ~deploy_longitude,
                   lat = ~deploy_latitude,
                   radius = ~sqrt(n),
                   label = paste0(detections_per_site$n, ' detections'),
                   stroke = F,
                   fillOpacity = .3)
