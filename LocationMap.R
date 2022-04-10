library(tidyverse)
library(ggplot2)
library(ggmap)
library(sf)
taxizones = read_sf("/Users/kevinhutchins/Desktop/Stat 479/Project3WorkAround/taxiZone.geojson")
taxis = read_csv("/Users/kevinhutchins/Desktop/Stat 479/Project3WorkAround/taxis.csv")

#Modifying subway
subwayV2 = read_csv("/Users/kevinhutchins/Desktop/Stat 479/Project3WorkAround/subwayV2.csv")
subwayV2 = subwayV2 %>% distinct(the_geom) %>% mutate(coords = strsplit(substr(the_geom, 8, 100), " "))
subwayV2 = subwayV2 %>% mutate(Longitude = 0.0, Latitude = 0.0)
for(i in 1:length(subwayV2$the_geom)){
  subwayV2$Longitude[i] = as.numeric(subwayV2$coords[[i]][1])
  subwayV2$Latitude[i] = as.numeric(substr(subwayV2$coords[[i]][2],1, str_length(subwayV2$coords[[i]][2]) - 1))
}
subwayV2 = subwayV2 %>% dplyr::select(Longitude, Latitude) %>% mutate(type = "Subway Station")

#modifying Bike Stations  
bikes = read_csv("/Users/kevinhutchins/Desktop/Stat 479/Project3WorkAround/bikes.csv")
stations = bikes %>% 
  distinct(startStationID, startLongitude, startLatitude) %>%
  dplyr::select(startLongitude, startLatitude) %>% 
  transmute(Longitude = startLongitude, Latitude = startLatitude, type = "Bike Station")

data = bind_rows(stations, subwayV2)

map = ggplot(data) +
  geom_sf(data = taxizones, fill = "grey", color = "black") +
#  theme_minimal() +
  geom_point(aes(x = Longitude, y = Latitude, color = type),size = 0.4) +
  scale_color_manual(values = c("Bike Station" = "dodgerblue4", "Subway Station" = "red")) + 
  coord_sf(xlim = c(-74.3, -73.7), ylim = c(40.5, 40.9)) +
  labs(title = "Map of NYC Transportation in 2019",
       color = "Type of Station:",
       caption = "New York City is also divided into taxi districts, which are shown by the many separations on the map") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top",
        plot.caption = element_text(hjust = 0, face = "italic"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())
map
ggsave("NYCmap.png", plot = map)
