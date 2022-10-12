library(tidyverse)
dates <- read.table("data/sonoma-dates", sep = " ")
dates <- data.frame(t(dates)[-2,], row.names = NULL)
names(dates) <- dates[1,]
dates <- dates[-1,]
dates <- dates[-13001,]
dates[1,] <- c("1",	"Tue Apr 27 17:10:00 2004",	"12536.0069444444")
row.names(dates) <- NULL

dates <- dates %>%
  mutate(epochNums = as.integer(epochNums),
         epochDates = as.POSIXct(epochDates, format = "%a %b %d %H:%M:%S %Y"),
         epochDays = as.numeric(epochDays))
names(dates)[1] <- "epoch"

write.csv(dates, "data/dates.csv")