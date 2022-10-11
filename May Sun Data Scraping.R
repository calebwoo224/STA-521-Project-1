library(tidyverse)
library(rvest)
library(lubridate)

sonoma_may_sun <- read_html("https://www.timeanddate.com/sun/usa/sonoma?month=5&year=2004") %>%
  html_element(xpath="//*[@id='as-monthsun']") %>%
  html_table()
names(sonoma_may_sun)[1] <- "Day"
names(sonoma_may_sun)[2] <- "Sunrise"
names(sonoma_may_sun)[3] <- "Sunset"
names(sonoma_may_sun)[13] <- "Solar Noon Direction"
names(sonoma_may_sun)[10] <- "Begin Sunrise"
names(sonoma_may_sun)[11] <- "End Sunset"

sonoma_may_sun <- sonoma_may_sun %>%
  select(Day, Sunrise, Sunset, `Solar Noon`, `Begin Sunrise`, `End Sunset`) %>%
  mutate(Sunrise = str_extract(Sunrise, "[:digit:]:[:digit:][:digit:]"),
         Sunset = str_extract(Sunset, "[:digit:]:[:digit:][:digit:]"),
         `Solar Noon` = str_extract(`Solar Noon`, "[:digit:]:[:digit:][:digit:]"),
         `Begin Sunrise` = str_extract(`Begin Sunrise`, "[:digit:]:[:digit:][:digit:]"),
         `End Sunset` = str_extract(`End Sunset`, "[:digit:]:[:digit:][:digit:]"))
sonoma_may_sun <- sonoma_may_sun[2:32,]
sonoma_may_sun[1, 1] <- "1"
sonoma_may_sun[1, 2] <- "6:12"
sonoma_may_sun[1, 3] <- "8:01"
sonoma_may_sun[1, 4] <- "1:06"
sonoma_may_sun[1, 5] <- "5:44"
sonoma_may_sun[1, 6] <- "8:30"
sonoma_may_sun <- sonoma_may_sun %>%
  mutate(Day = as.integer(Day),
         Sunrise = as.POSIXct(paste0("2004-05-", Day, " ", Sunrise, ":00"), tz = "UTC"),
         Sunset = as.POSIXct(paste0("2004-05-", Day, " ", Sunset, ":00"), tz = "UTC") + hours(12),
         `Solar Noon` = as.POSIXct(paste0("2004-05-", Day, " ", `Solar Noon`, ":00"), tz = "UTC") + hours(12),
         `Begin Sunrise` = as.POSIXct(paste0("2004-05-", Day, " ", `Begin Sunrise`, ":00"), tz = "UTC"),
         `End Sunset` = as.POSIXct(paste0("2004-05-", Day, " ", `End Sunset`, ":00"), tz = "UTC") + hours(12)) %>%
  mutate(`End Sunrise` = Sunrise + (Sunrise-`Begin Sunrise`),
         `Begin Sunset` = Sunset - (`End Sunset`-Sunset))

data_all <- read_csv("data/data-all-dates-locations.csv")
data_all <- data_all %>%
  filter(as.Date(epochDates) >= as.Date("2004-05-01") & as.Date(epochDates) <= as.Date("2004-05-31")) %>%
  mutate(Day = day(epochDates))
data_all_sun <- merge(data_all, sonoma_may_sun, by="Day")
data_all_sun <- data_all_sun %>%
  mutate(Time = case_when(
    epochDates %within% interval(`Begin Sunrise`, `End Sunrise`) ~ "Sunrise",
    epochDates %within% interval(`Begin Sunset`, `End Sunset`) ~ "Sunset",
    epochDates %within% interval(`End Sunrise`, as.POSIXct(paste0("2004-05-", Day, " 10:00:00"), tz = "UTC")) ~ "Early Morning",
    epochDates %within% interval(as.POSIXct(paste0("2004-05-", Day, " 10:00:00"), tz = "UTC"), `Solar Noon`) ~ "Late Morning",
    epochDates %within% interval(`Solar Noon`, as.POSIXct(paste0("2004-05-", Day, " 16:30:00"), tz = "UTC")) ~ "Early Noon",
    epochDates %within% interval(as.POSIXct(paste0("2004-05-", Day, " 16:30:00"), tz = "UTC"), `Begin Sunset`) ~ "Late Noon",
    epochDates %within% interval(`End Sunset`, as.POSIXct(paste0("2004-05-", Day, " 23:59:59"), tz = "UTC")) ~ "Early Night",
    epochDates %within% interval(as.POSIXct(paste0("2004-05-", Day, " 00:00:00"), tz = "UTC"),
                                 as.POSIXct(paste0("2004-05-", Day, " 01:30:00"), tz = "UTC")) ~ "Early Night",
    epochDates %within% interval(as.POSIXct(paste0("2004-05-", Day, " 01:30:00"), tz = "UTC"), `Begin Sunrise`) ~ "Late Night"
  ))
data_all_sun <- data_all_sun %>%
  mutate(Hour = hour(epochDates))
data_all_sun$Time <- factor(data_all_sun$Time, levels = c("Early Night", "Late Night", "Sunrise", "Early Morning", "Late Morning", "Early Noon", "Late Noon", "Sunset"))

data_all_sun <- data_all_sun %>%
  select(-c("...1", "...2", "Sunrise", "Sunset", "Solar Noon", "Begin Sunrise",
            "End Sunset", "End Sunrise", "Begin Sunset"))
#write.csv(data_all_sun, "data/may-data-sun.csv")



