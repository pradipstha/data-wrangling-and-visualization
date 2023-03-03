# import libraries
library(tidyverse)
library(osmdata)
library(ggplot2)
library(tidyverse)
library(ggrepel)
library(sf)
library(showtext)

## automatically use showtext for new devices
showtext_auto() 

# https://fonts.google.com/featured/Superfamilies
font_add_google("Montserrat", "Montserrat")

# check for features in osm dataset
available_features()
available_tags("highway")
available_tags("water")

# get extent of area
getbb("Kathmandu Nepal")

##get street data from osm data
streets<- getbb("Kathmandu Nepal") %>%
  opq() %>%
add_osm_feature(key = "highway", 
        value = c("motorway", "primary", "secondary")) %>%
osmdata_sf()
streets

# get secondary layer od street data  
streets_small<- getbb("Kathmandu Nepal") %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "unclassified", "service")) %>%
  osmdata_sf()
streets_small

# Create the plot object, using the osm_lines element of streets
street_plot <- ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = 0.7)+ geom_sf(data = streets_small$osm_lines,
                              inherit.aes = FALSE,
                              color = "#666666",  # medium gray
                              size = 0.1)
street_plot

site<- getbb("Kathmandu Nepal") %>%
  opq() %>%
  add_osm_feature(key = "amenity", 
                  value = c("place_of_worship")) %>%
  osmdata_sf()
site

site_plot <- ggplot() +
  geom_sf(data = site$osm_points,
          inherit.aes = FALSE,
          color = "firebrick4",
          size = 0.2)
site_plot

## plot the data 
p<- ggplot() +  geom_sf(data=site$osm_points,
                        inherit.aes = FALSE,
                        size = 2,
                        alpha = .7, fill="firebrick4", color="white", pch=21) +
  geom_sf(data=streets$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size =.7,
          alpha = .8) +
  geom_sf(data=streets_small$osm_lines,
          inherit.aes = FALSE,
          color = "#ffbe7f",
          size =.5,
          alpha = .6) +
  coord_sf(xlim = c(85.26, 85.38),
           ylim = c(27.66, 27.76),
           expand = FALSE,
           datum = sf::st_crs(4326)) + 
  theme_void() +
  #theme(plot.background = element_rect(fill = "#282828")) + 
  ggspatial::annotation_scale(location = "bl") +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
  style = ggspatial::north_arrow_fancy_orienteering) + 
  theme(plot.title = element_text(family = "Montserrat", size = 23, face="bold", hjust=.5),
        plot.subtitle = element_text(family = "Montserrat", size = 10, hjust=.5, margin=margin(2, 0, 5, 0)),
        plot.caption = element_text(family = "Montserrat", size = 8, hjust = 1)) +
  labs(title = "Kathmandu", 
       subtitle = "27.72°N / 85.32°E",
       caption = paste0("\u00A9 Pradip Shrestha, 2022"))

p

# Save map
ggsave("map.tiff", width = 6, height = 6, dpi = 150)
dev.off()
          

