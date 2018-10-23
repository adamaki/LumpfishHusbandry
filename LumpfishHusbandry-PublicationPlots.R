# Plots for publication
# WLumpfish Husbandry paper
# Adam Brooker 15th October 2018

# Plot list
# 1a. temperature
# 1b. salinity
# 1c. DO


# 1. Environmental probe plots

library(rJava)
library(xlsxjars)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(extrafont)
library(extrafontdb)
library(zoo)
library(grid)
library(reshape2)
library(cowplot)
library(data.table)

setwd('G:/Data/2018 Lumpfish Husbandry')

# LOAD ENVIRONMENTAL PROBE READINGS

masterfileloc = "G:/Data/2018 Lumpfish Husbandry/AcousticTagFile_2018v6.xlsx" # 2015 wild vs. farmed wrasse

load.DO <- function(probename, colnums) {
  pn <- probename
  probename <- read.xlsx(masterfileloc, sheet = 12, startRow = 3, cols = colnums)
  colnames(probename) <- c('Time', 'DO', 'Temp')
  probename$Time <- as.POSIXct(strptime(probename$Time, "%Y-%m-%d %H:%M:%S", tz = 'UTC'))
  probename$Time <- probename$Time - as.difftime(1, unit = 'hours') 
  probename <- probename %>% mutate_at(.vars = vars(DO, Temp), .funs = funs(round(.,2)))
  assign(pn, probename, envir = globalenv())
}

load.sal <- function(probename, colnums) {
  pn <- probename
  probename <- read.xlsx(masterfileloc, sheet = 12, startRow = 3, cols = colnums)
  colnames(probename) <- c('Time', 'Sal')
  probename$Time <- as.POSIXct(strptime(probename$Time, "%Y-%m-%d %H:%M:%S", tz = 'UTC'))
  probename$Time <- probename$Time - as.difftime(1, unit = 'hours')
  probename <- probename %>% mutate(Sal = round(Sal, 2))
  assign(pn, probename, envir = globalenv())
}


# load probe data

load.DO('probe.DOT2', colnums = c(1, 2, 3))
load.DO('probe.DOT4', colnums = c(6, 7, 8))
load.DO('probe.DOT7', colnums = c(11, 12, 13))
load.DO('probe.DOT10', colnums = c(16, 17, 18))
load.sal('probe.sal2', colnums = c(4, 5))
load.sal('probe.sal4', colnums = c(9, 10))
load.sal('probe.sal7', colnums = c(14, 15))
load.sal('probe.sal10', colnums = c(19, 20))

# combine all data into one data frame
probes <- cbind(probe.DOT2, probe.sal2, probe.DOT4, probe.sal4, probe.DOT7, probe.sal7, probe.DOT10, probe.sal10)
probes <- probes[,c(1, 2, 3, 5, 7, 8, 10, 12, 13, 15, 17, 18, 20)]
colnames(probes) <- c('Time', 'do.2m', 'temp.2m', 'sal.2m', 'do.4m', 'temp.4m', 'sal.4m', 'do.7m', 'temp.7m', 'sal.7m', 'do.10m', 'temp.10m', 'sal.10m')

# calculate rolling 6h-means for data
probes$rolldo2m <- c(rep(NA,11), rollapply(probes$do.2m, width = 12, FUN = mean, na.rm = T, align = 'right'))
probes$rolldo4m <- c(rep(NA,11), rollapply(probes$do.4m, width = 12, FUN = mean, na.rm = T, align = 'right'))
probes$rolldo7m <- c(rep(NA,11), rollapply(probes$do.7m, width = 12, FUN = mean, na.rm = T, align = 'right'))
probes$rolldo10m <- c(rep(NA,11), rollapply(probes$do.10m, width = 12, FUN = mean, na.rm = T, align = 'right'))

probes$rollt2m <- c(rep(NA,11), rollapply(probes$temp.2m, width = 12, FUN = mean, na.rm = T, align = 'right'))
probes$rollt4m <- c(rep(NA,11), rollapply(probes$temp.4m, width = 12, FUN = mean, na.rm = T, align = 'right'))
probes$rollt7m <- c(rep(NA,11), rollapply(probes$temp.7m, width = 12, FUN = mean, na.rm = T, align = 'right'))
probes$rollt10m <- c(rep(NA,11), rollapply(probes$temp.10m, width = 12, FUN = mean, na.rm = T, align = 'right'))

probes$rolls2m <- c(rep(NA,11), rollapply(probes$sal.2m, width = 12, FUN = mean, na.rm = T, align = 'right'))
probes$rolls4m <- c(rep(NA,11), rollapply(probes$sal.4m, width = 12, FUN = mean, na.rm = T, align = 'right'))
probes$rolls7m <- c(rep(NA,11), rollapply(probes$sal.7m, width = 12, FUN = mean, na.rm = T, align = 'right'))
probes$rolls10m <- c(rep(NA,11), rollapply(probes$sal.10m, width = 12, FUN = mean, na.rm = T, align = 'right'))

probes$meando <- rowMeans(probes[,c(14, 15, 16, 17)], na.rm = T)
probes$meantemp <- rowMeans(probes[,c(18, 19, 20, 21)], na.rm = T)
probes$meansal <- rowMeans(probes[,c(22, 23, 24, 25)], na.rm = T)

# subset for study start and end dates
probes <- subset(probes, Time > '2018-06-01 00:59:00' & Time < '2018-08-21 00:59:00')


# code to add experiment day number to probe data

exp.dates <- unique(as.Date(probes$Time))
exp.start <- 1
exp.length <- 64
exp.days <- seq(exp.start, exp.start+exp.length-1, 1)
names(exp.days) <- exp.dates
probes$day <- as.numeric(exp.days[as.character(as.Date(probes$Time))])


# 1a. temperature

ggplot(probes) +  
  scale_x_datetime('Date', limits = as.POSIXct(range(probes$Time)), expand = c(0,0)) + 
  scale_y_continuous(expression(paste('Temperature (', ~degree,'C)', sep='')), limits = c(10,18), expand = c(0,0)) +
  theme_classic() + theme(text = element_text(family = 'Times New Roman', size = 14), legend.position = c(0.90, 0.2)) +
  #geom_line(aes(as.POSIXct(probes$do.time.1m), probes$do.1m), linetype = 'dashed') +
  geom_line(aes(as.POSIXct(probes$Time), probes$rollt2m, colour = ' 2m', linetype = ' 2m')) + #, size = 0.7, color = 'gray', linetype = 'longdash') +
  geom_line(aes(as.POSIXct(probes$Time), probes$rollt4m, colour = ' 4m', linetype = ' 4m')) + #, size = 0.7, color = 'gray', linetype = 'solid') + 
  geom_line(aes(as.POSIXct(probes$Time), probes$rollt7m, colour = ' 7m', linetype = ' 7m')) + #, size = 0.7, color = 'black', linetype = 'longdash') +
  geom_line(aes(as.POSIXct(probes$Time), probes$rollt10m, colour = '10m', linetype = '10m')) + #, size = 0.7, color = 'black', linetype = 'solid') +
  scale_colour_manual(name = '', values = c(' 2m' = 'gray', ' 4m' = 'gray', ' 7m' = 'black', '10m' = 'black')) +
  scale_linetype_manual(name = '', values = c(' 2m' = 'longdash', ' 4m' = 'solid', ' 7m' = 'longdash', '10m' = 'solid')) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), legend.text = element_text(size = 14)) #+
  #annotation_custom(grobTree(textGrob('(a)', x = 0.05, y = 0.95, gp = gpar(fontsize = 18, fontfamily = 'Times New Roman'))))

# 1b. salinity

ggplot(probes) +  
  scale_x_datetime('Date', limits = as.POSIXct(range(probes$do.time.1m))) + 
  scale_y_continuous('Salinity (PSU)', limits = c(0,35)) +
  theme_classic() + theme(text = element_text(family = 'Times New Roman', size = 18), legend.position = c(0.90, 0.2)) +
  #geom_line(aes(as.POSIXct(probes$do.time.1m), probes$do.1m), linetype = 'dashed') +
  geom_line(aes(as.POSIXct(probes$do.time.1m), probes$rolls1m, colour = ' 1m', linetype = ' 1m')) + #, size = 0.7, color = 'gray', linetype = 'longdash') +
  geom_line(aes(as.POSIXct(probes$do.time.4m), probes$rolls4m, colour = ' 4m', linetype = ' 4m')) + #, size = 0.7, color = 'gray', linetype = 'solid') + 
  geom_line(aes(as.POSIXct(probes$do.time.8m), probes$rolls8m, colour = ' 8m', linetype = ' 8m')) + #, size = 0.7, color = 'black', linetype = 'longdash') +
  geom_line(aes(as.POSIXct(probes$do.time.12m), probes$rolls12m, colour = '12m', linetype = '12m')) + #, size = 0.7, color = 'black', linetype = 'solid') +
  scale_colour_manual(name = '', values = c(' 1m' = 'gray', ' 4m' = 'gray', ' 8m' = 'black', '12m' = 'black')) +
  scale_linetype_manual(name = '', values = c(' 1m' = 'longdash', ' 4m' = 'solid', ' 8m' = 'longdash', '12m' = 'solid')) +
  annotation_custom(grobTree(textGrob('(b)', x = 0.05, y = 0.95, gp = gpar(fontsize = 18, fontfamily = 'Times New Roman'))))



# 1c. DO

ggplot(probes) +  
  scale_x_datetime('Date', limits = as.POSIXct(range(probes$Time)), expand = c(0,0)) + 
  scale_y_continuous('Dissolved oxygen (mg/L)', limits = c(5,15), expand = c(0,0)) +
  theme_classic() + theme(text = element_text(family = 'Times New Roman', size = 14), legend.position = c(0.90, 0.2)) +
  #geom_line(aes(as.POSIXct(probes$do.time.1m), probes$do.1m), linetype = 'dashed') +
  geom_line(aes(as.POSIXct(probes$Time), probes$rolldo2m, colour = ' 2m', linetype = ' 2m')) + #, size = 0.7, color = 'gray', linetype = 'longdash') +
  geom_line(aes(as.POSIXct(probes$Time), probes$rolldo4m, colour = ' 4m', linetype = ' 4m')) + #, size = 0.7, color = 'gray', linetype = 'solid') + 
  geom_line(aes(as.POSIXct(probes$Time), probes$rolldo7m, colour = ' 7m', linetype = ' 7m')) + #, size = 0.7, color = 'black', linetype = 'longdash') +
  geom_line(aes(as.POSIXct(probes$Time), probes$rolldo10m, colour = '10m', linetype = '10m')) + #, size = 0.7, color = 'black', linetype = 'solid') +
  scale_colour_manual(name = '', values = c(' 2m' = 'gray', ' 4m' = 'gray', ' 7m' = 'black', '10m' = 'black')) +
  scale_linetype_manual(name = '', values = c(' 2m' = 'longdash', ' 4m' = 'solid', ' 7m' = 'longdash', '10m' = 'solid')) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), legend.text = element_text(size = 14)) #+
  #annotation_custom(grobTree(textGrob('(c)', x = 0.05, y = 0.95, gp = gpar(fontsize = 18, fontfamily = 'Times New Roman'))))




