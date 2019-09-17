# Investigation of lumpfish behaviour and weather patterns
# Adam Brooker 18th October 2018


library(tidyverse)
library(dplyr)
library(ggplot2)


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
  filter(SUN == 'D') %>%
  summarize(depth_mean = mean(PosZ)) %>%
  ggplot() + geom_point(aes(solarIrradiance_wperm2, depth_mean))

# behaviour/weather correlations

library(devtools)
devtools::source_gist("524eade46135f6348140", filename = "ggplot_smooth_func.R")

#dayfile[sample(nrow(dayfile), 10000),] %>%
wplot <- sample_frac(dayfile, 0.01) %>% # samples random fraction of dataset
  filter(SUN == 'D') %>%
  ggplot(aes(precipRate_mm, PosZ)) + 
  geom_point() +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE, colour = 'red') +
  geom_smooth(method="lm",se=T)
  
wplot + facet_wrap(vars(Period))


# Pincipal components analysis (PCA)--------------------------------------------
library(devtools)
library(scales)
install_github("vqv/ggbiplot")
library(ggbiplot)

wpca <- prcomp(sample_frac(na.omit(RahoyWeather[,c('temp_C', "precipRate_mm", "solarIrradiance_wperm2", "humidity_percent", "windSpeed_kph")]), 0.1), center = T, scale. = T)
dsamp <- sample_frac(dayfile, 0.001) %>% 
  filter(Period != '15919' & Period != '15863' & Period != '15695' & Period != '15471') %>%
  filter(Period != '7351', Period != '7099') %>%
  filter(precipRate_mm < 3) %>%
  filter(BLSEC <2)
wpca <- prcomp(na.omit(dsamp[,c('PosZ', "pressure_hPa", 'temp_C')]), center = T, scale. = T)


ggbiplot(wpca, choices = c(1, 2), alpha = 0.1, groups = dsamp$Period, ellipse = T, varname.size = 5) + theme_minimal()



