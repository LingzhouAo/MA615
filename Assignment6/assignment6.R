library(ggmap)
suppressMessages(library("tidyverse"))
library(magick)
#Road map of Bude 
map_road <- get_googlemap(center =c(-4.543678,50.82664), zoom = 14, maptype = "roadmap")
ggmap(map_road)

#Watercolor map of Bude 
map_watercolor <- get_map("Bude",source="stamen", zoom = 14, maptype = "watercolor")
ggmap(map_watercolor)

## Road map
#Red: Summerleaze Beach 
#Blue: Bude North Cornwall Cricket Club
#Green: Crooklets Beach
#Yellow: Bude tidal swimming pool

ggmap(map_road) +
  geom_point(
    aes(x = -4.551312 , y = 50.83074),
    color = "red", size = 2) +
  geom_point(
    aes(x =  -4.552314 , y = 50.835289),
    color = "blue", size = 2) +
  geom_point(
    aes(x = -4.553962 , y = 50.83587),
    color = "green", size = 2) +
  geom_point(
    aes(x = -4.5540, y = 50.8326),
    color = "yellow", size = 2)

##Watercolor map
ggmap(map_watercolor) +
  geom_point(
    aes(x = -4.551312 , y = 50.83074),
    color = "red", size = 2) +
  geom_point(
    aes(x =  -4.552314 , y = 50.835289),
    color = "blue", size = 2) +
  geom_point(
    aes(x = -4.553962 , y = 50.83587),
    color = "green", size = 2) +
  geom_point(
    aes(x = -4.5540, y = 50.8326),
    color = "yellow", size = 2)

# Maps with routes
from <- "Bude North Cornwall Cricket Club"
to <- "Crooklets Inn"
route_df <- route(from, to, structure = "route")
ggmap(map_road) + 
  geom_point(aes(x =  -4.552314 , y = 50.835289), color = "red", size = 2) +
  geom_point(aes(x = -4.5510, y = 50.8362), color = "brown", size = 2) +
  geom_path(aes(x = lon, y = lat), colour = "blue", size = 1, data = route_df, lineend = "round")

# Watercolor map wiht routes
ggmap(map_watercolor) + 
  geom_point(aes(x =  -4.552314 , y = 50.835289), color = "red", size = 2) +
  geom_point(aes(x = -4.5510, y = 50.8362), color = "brown", size = 2) +
  geom_path(aes(x = lon, y = lat), colour = "blue", size = 1, data = route_df, lineend = "round")
