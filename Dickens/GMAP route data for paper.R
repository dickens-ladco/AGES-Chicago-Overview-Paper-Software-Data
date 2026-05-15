## GMAP routes for AGES Chicago paper


require(tidyverse)
require(sf)
require(ggspatial)
require(prettymapr)
# require(ggrepel)


setwd("...")


#Load GMAP data
GMAP.data <- read.csv("AGES23_08_1_12_MA01-update-dates.csv", header=TRUE)

GMAP.for.map <- st_as_sf(GMAP.data, coords = c("GPS.Longitude", "GPS.Latitude"), crs = 4326, remove = FALSE) #convert to sf object for mapping
GMAP.reproject <- st_transform(GMAP.for.map, 3857) #reproject data

GMAP.reproject$Date <- factor(GMAP.reproject$Date, levels = c("8/1","8/2","8/8","8/12"))

#Add locations of target points
targets <- data.frame(Site = c("O'Hare", "Global Two", "Bedford Park", "Corwith", "UIC"),
                      Longitude = c(-87.902, -87.908, -87.770, -87.715, -87.649),
                      Latitude = c(41.979, 41.899, 41.767, 41.819, 41.869))
targets.for.map <- st_as_sf(targets, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
targets.reproject <- st_transform(targets.for.map, 3857)

#specify x/y for labels:
labels <- targets.reproject %>% 
  dplyr::mutate(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = Site)
#Adjust to avoid overplotting: 
labels <- labels %>%
  mutate(
    x_nudge = case_when(
      Site == "Global Two" ~ x - 3000,
      Site == "Corwith" ~ x - 4500,
      # Site == "Bedford Park" ~ x - 1000,
      TRUE ~ x + 1500
    ),
    y_nudge = case_when(
      Site == "O'Hare" ~ y + 2200,
      Site == "UIC" ~ y + 2000,
      Site == "Bedford Park" ~ y + 2700,
      Site == "Global Two" ~ y + 2500,
      TRUE ~ y + 0
    )
  )


    GMAP.map <- ggplot() + annotation_map_tile(type = "osm", zoomin = 0) + facet_wrap(~Date) +
      geom_sf(data = GMAP.reproject, size = 0.5) + 
      geom_sf(data = targets.reproject, shape = 24, fill = "blue", size = 2.5) +
      geom_text(data = labels, aes(x = x_nudge, y = y_nudge, label = Site), size = 3) +
      # geom_text_repel(data = labels, aes(x = x_nudge, y = y_nudge, label = Site), size = 3, min.segment.length = 0, seed = 123) +
      coord_sf(xlim = c(-9795000, -9750000), ylim = c(5115000, 5165000), expand = FALSE) +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            strip.text = element_text(size = 12))
    ggsave("GMAP routes facet by day.png", GMAP.map, width = 7, height = 8.5)
    
    
    
    
    
    
    
    
    
    