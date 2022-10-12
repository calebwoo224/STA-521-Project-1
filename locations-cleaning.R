locations <- read.table("data/mote-location-data.txt")
names(locations) <- locations[1,]
names(locations)[1] <- "nodeid"
locations <- data.frame(locations[-1,])
row.names(locations) <- NULL

write.csv(locations, "data/locations.csv")