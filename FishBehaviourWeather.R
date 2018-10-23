# Investigation of lumpfish behaviour and weather patterns
# Adam Brooker 18th October 2018


library(tidyverse)


setwd('G:/Data/2018 Lumpfish Husbandry/Data processing/6a. Coded Day CSV')

load.all('12')

wdata <- read.csv('G:/Data/WeatherData/Data tables/Rahoy/Rahoy_56_636N_5_841W_Weather_Jun-Aug2018.csv')
wdata$X1 <- NULL
wdata <- as.tibble(wdata)
wdata$time <- as.POSIXct(wdata$time)

# calculate cumulative rainfall
wdata$runTot_precip <- cumsum(wdata$precipRate_mm/12) # rate is in mm/h and 12 is no. of observations in 1 hr

dayfile <- as.tibble(dayfile)



  
dayfile <- full_join(dayfile, wdata, by = c('EchoTime' = 'time')) %>%
  arrange(EchoTime) %>%
  fill(temp_C) %>%
  fill(dewpoint_C) %>%  
  fill(humidity_percent) %>%
  fill(windDir_deg) %>%
  fill(windSpeed_kph) %>%
  fill(windGust_kph) %>%
  fill(pressure_hPa) %>%
  fill(precipRate_mm) %>%
  fill(precipAccum_mm) %>%
  fill(uvIndex) %>%
  fill(solarIrradiance_wperm2) %>%
  fill(runTot_precip) %>%
  filter(!is.na(PosX)) %>%
  arrange(Period, EchoTime)

#colsToUse <- intersect(colnames(all), colnames(dayfile))
#matchRows <- match(do.call('paste', all[,colsToUse]), do.call('paste', dayfile[,colsToUse]))


group_by(dayfile, solarIrradiance_wperm2) %>%
  summarize(depth_mean = mean(PosZ))





