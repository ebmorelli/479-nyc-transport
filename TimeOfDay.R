library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggridges)
library(scales)

taxis = read_csv("/Users/kevinhutchins/Desktop/Stat 479/Project3WorkAround/taxis.csv")
bikes = read_csv("/Users/kevinhutchins/Desktop/Stat 479/Project3WorkAround/bikes.csv")

#Duration:
bikeLen = bikes %>% 
  mutate(duration = as.numeric(stopTime - startTime)) %>% 
  select(duration)
taxiLen = taxis %>% 
  mutate(duration = as.numeric((dropoffTime - pickupTime) / 60)) %>% 
  select(duration)


#Time of Day (Hours)
bikeSum = 577703
bikeHours = bikes %>% 
  mutate(starthour = hour(startTime)) %>%
  group_by(starthour) %>%
  summarize(number = n() / bikeSum)

taxiSum = 84395
taxiHours = taxis %>% 
  mutate(starthour = hour(pickupTime)) %>%
  group_by(starthour) %>%
  summarize(number = n() / taxiSum)

timeData = bind_cols(bikeHours, taxiHours) %>%
  rename(hour = starthour...1, Bikes = number...2, Taxis = number...4) %>%
  select(hour, Bikes, Taxis) %>% pivot_longer(cols = c("Bikes", "Taxis"), names_to = "type", values_to = "num")


ggplot(timeData, aes(x = hour, y = num)) +  
  geom_density() +
  facet_grid(type ~ .)
  geom_density_ridges(data = timeData, aes(x = num, y = type, fill = type))

p1 = ggplot(timeData) + 
  geom_ridgeline(aes(x = hour, y = num,height = num, fill = type), show.legend = FALSE) +
  facet_grid(type ~ .) + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top",
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank()) +
        labs(x = "Hours since Midnight", 
             title = "Density of NYC Transportation over a 24 Hour Period", 
             y = "Percentage of Total Transportation Active") + 
        scale_y_continuous(labels = percent)
ggsave("TimeofDay.png", plot = p1)


