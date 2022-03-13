library(tidyverse)
library("sf")
stringOne = "https://s3.amazonaws.com/nyc-tlc/trip+data/yellow_tripdata_2019-0"
stringTwo = "https://s3.amazonaws.com/nyc-tlc/trip+data/yellow_tripdata_2019-"
data = read_csv("https://s3.amazonaws.com/nyc-tlc/trip+data/yellow_tripdata_2019-01.csv")
output = data[1,]

for(num in 1:12){
  
  for(i in 1:length(data$VendorID)){
    if(i %% 1000 == 0){
      output = rbind(output, data[i,])
    }
  }
  
  if(num < 10){
    line = paste(stringOne, num + 1, ".csv", sep = "", collapse=NULL)
  }else{
    line = paste(stringTwo, num + 1, ".csv", sep = "", collapse=NULL)
  }
  
  if(num != 12){
    data = read_csv(line)
  }
}
  





output = output %>% select(tpep_pickup_datetime,
                           tpep_dropoff_datetime,
                           passenger_count,
                           trip_distance,
                           PULocationID,
                           DOLocationID,
                           total_amount,
                           congestion_surcharge)
output = output %>% rename(pickupTime = tpep_pickup_datetime,
                           dropoffTime = tpep_dropoff_datetime,
                           passengerCount = passenger_count,
                           totalDistance = trip_distance,
                           pickupLocation = PULocationID,
                           dropoffLocation = DOLocationID,
                           totalAmount = total_amount,
                           congestionCharge = congestion_surcharge)

#change file to current directory
write.table(output , file = "taxis.csv", sep=",", row.names=FALSE)

taxizones <- read_sf("https://data.cityofnewyork.us/api/geospatial/d3c5-ddgc?method=export&format=GeoJSON")
#can now combine taxi locations with geospacial data to make maps of New York
