library(tidyverse)
library(lubridate)

taxis = read.csv("https://github.com/ebmorelli/479-nyc-transport/raw/main/taxis.csv")

taxiHours = taxis %>% 
  drop_na(passengerCount) %>%
  mutate(starthour = hour(pickupTime)) %>%
  group_by(starthour) %>%
  summarize(amount = mean(totalAmount), apm = mean(totalAmount) / mean(totalDistance)) %>%
  filter(apm < 1000000, apm > 0)

ggplot(taxiHours) +
  geom_bar(aes(x = starthour, y = apm),stat="identity", fill = "#00BFC4", color = "black") +
  labs(title = "New York City Taxi Prices per Mile") +
  xlab("Time (24hr)")+
  ylab("Price per Mile")