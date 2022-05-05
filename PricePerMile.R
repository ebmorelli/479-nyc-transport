library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggridges)
library(scales)

taxis = read_csv("/Users/kevinhutchins/Desktop/Stat 479/Project3WorkAround/taxis.csv")

taxiHours = taxis %>% 
  filter(totalDistance > 0) %>%
  drop_na(totalDistance) %>%
  drop_na(totalAmount) %>%
  mutate(starthour = hour(pickupTime), ppm = totalAmount / totalDistance) %>%
  group_by(starthour) %>%
  summarize(PricePerMile =  mean(ppm)) %>%
  rename(hour = starthour, price = PricePerMile) %>%
  mutate(type = "Taxi")

#hour price
#2.75
fakeSubway = seq(0,23)
fakeSubway = as_tibble(fakeSubway) %>% mutate(price = 2.75) %>%
  rename(hour = value) %>% 
  mutate(type = "Subway")
#3.99
fakeBikes = seq(0,23)
fakeBikes = as_tibble(fakeBikes) %>% 
  mutate(price = 3.99, type = "Bike") %>%
  rename(hour = value)

super = rbind(fakeSubway, fakeBikes)
super = rbind(super, taxiHours)

p1 = ggplot(super, aes(x = hour, y = price, group = type, color = type)) + 
  geom_point() +
  geom_smooth(se=FALSE) +
  labs(title = "New York City Prices Per Mile",
       color = "Transportation:",
       x = "Hours past Midnight",
       y = "Price Per Mile in Dollars") + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top",
        plot.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major=element_line(color="gray"),
        panel.background = element_rect(fill = "white", color = "gray"),
        panel.border = element_blank()) +
  scale_color_manual(values=c("firebrick1","palegreen1", "cyan3"))

ggsave("PricePerMile.png", plot = p1)
