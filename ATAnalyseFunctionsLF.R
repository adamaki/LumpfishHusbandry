##------Functions for ATAnalyseLumphus

#run as: 
#source("ATAnalyseFunctionsLF.R")
# FUNCTIONS----------------------------------------------------------------------------------------------------------------------------------


# 1. FUNCTION TO CALCULATE SUMMARY OF FISH LOCATIONS
locations <- function()
{
  # pen 12 location summary
  dayfile.bot <- subset(dayfile, BOT == 'B' & PEN == '12' & SEC >= 0)
  dayfile.top <- subset(dayfile, BOT == 'Z' & PEN == '12' & SEC >= 0)
  dayfile.out <- subset(dayfile, OUT == '12OE' & PEN == '12' & SEC >= 0 | OUT == '12OS' & PEN == '12' & SEC >= 0 | OUT == '12ON' & PEN == '12' & SEC >= 0 | OUT == '12OW' & PEN == '12' & SEC >= 0)
  dayfile.edg <- subset(dayfile, EDG == '12EN' & PEN == '12' & SEC >= 0 | EDG == '12EW' & PEN == '12' & SEC >= 0 | EDG == '12ES' & PEN == '12' & SEC >= 0 | EDG == '12EE' & PEN == '12' & SEC >= 0)
  dayfile.NWCor <- subset(dayfile, BIGC == '12CNW' & PEN == '12' & SEC >= 0)
  dayfile.NECor <- subset(dayfile, BIGC == '12CNE' & PEN == '12' & SEC >= 0)
  dayfile.SWCor <- subset(dayfile, BIGC == '12CSW' & PEN == '12' & SEC >= 0)
  dayfile.SECor <- subset(dayfile, BIGC == '12CSE' & PEN == '12' & SEC >= 0)
  dayfile.cen <- subset(dayfile, CEN == '12MH' & PEN == '12' & SEC >= 0 | CEN == '12MM' & PEN == '12' & SEC >= 0 | CEN == '12ML' & PEN == '12' & SEC >= 0)
  dayfile.hid <- subset(dayfile, HID == 'P12HE' & PEN == '12' & SEC >= 0 | HID == 'P12HW' & PEN == '12' & SEC >= 0)
  dayfile.fs <- subset(dayfile, FS == 'FS12'  & PEN == '12' & SEC >= 0)
  #location.sum <- data.frame(c(nrow(dayfile.bot), nrow(dayfile.top), nrow(dayfile.out), nrow(dayfile.edg), nrow(dayfile.bigc), nrow(dayfile.cen), nrow(dayfile.hid)))
  location.sum <- data.frame(c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600))
  rownames(location.sum) <- c('<15m', '>15m', 'outer', 'edge', 'NW_corner', 'NE_corner', 'SW_corner', 'SE_corner', 'centre', 'hides', 'feed station')
  colnames(location.sum) <- 'Pen12'
  
  # pen 14 location summary
  dayfile.bot <- subset(dayfile, BOT == 'B' & SEC >= 0 & PEN == '14' & SEC >= 0)
  dayfile.top <- subset(dayfile, BOT == 'Z' & PEN == '14' & SEC >= 0)
  dayfile.out <- subset(dayfile, OUT == '14OE' & PEN == '14' & SEC >= 0| OUT == '14OS' & PEN == '14' & SEC >= 0 | OUT == '14ON' & PEN == '14' & SEC >= 0 | OUT == '14OW' & PEN == '14' & SEC >= 0)
  dayfile.edg <- subset(dayfile, EDG == '14EN' & PEN == '14' & SEC >= 0 | EDG == '14EW' & PEN == '14' & SEC >= 0 | EDG == '14ES' & PEN == '14' & SEC >= 0 | EDG == '14EE' & PEN == '14' & SEC >= 0)
  dayfile.NWCor <- subset(dayfile, BIGC == '14CNW' & PEN == '14' & SEC >= 0)
  dayfile.NECor <- subset(dayfile, BIGC == '14CNE' & PEN == '14' & SEC >= 0)
  dayfile.SWCor <- subset(dayfile, BIGC == '14CSW' & PEN == '14' & SEC >= 0)
  dayfile.SECor <- subset(dayfile, BIGC == '14CSE' & PEN == '14' & SEC >= 0)
  dayfile.cen <- subset(dayfile, CEN == '14MH' & PEN == '14' & SEC >= 0 | CEN == '14MM' & PEN == '14' & SEC >= 0 | CEN == '14ML' & PEN == '14' & SEC >= 0)
  dayfile.hid <- subset(dayfile, HID == 'P14HW' & PEN == '14' & SEC >= 0 | HID == 'P14HE' & PEN == '14' & SEC >= 0)
  dayfile.fs <- subset(dayfile, FS == 'FS14' & PEN == '14' & SEC >= 0)
  #location.sum$Pen14 <- c(nrow(dayfile.bot), nrow(dayfile.top), nrow(dayfile.out), nrow(dayfile.edg), nrow(dayfile.bigc), nrow(dayfile.cen), nrow(dayfile.hid))
  location.sum$Pen14 <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
  
  # pen 15 location summary
  dayfile.bot <- subset(dayfile, BOT == 'B' & SEC >= 0 & PEN == '15' & SEC >= 0)
  dayfile.top <- subset(dayfile, BOT == 'Z' & PEN == '15' & SEC >= 0)
  dayfile.out <- subset(dayfile, OUT == '15OE' & PEN == '15' & SEC >= 0| OUT == '15OS' & PEN == '15' & SEC >= 0 | OUT == '15ON' & PEN == '15' & SEC >= 0 | OUT == '15OW' & PEN == '15' & SEC >= 0)
  dayfile.edg <- subset(dayfile, EDG == '15EN' & PEN == '15' & SEC >= 0 | EDG == '15EW' & PEN == '15' & SEC >= 0 | EDG == '15ES' & PEN == '15' & SEC >= 0 | EDG == '15EE' & PEN == '15' & SEC >= 0)
  dayfile.NWCor <- subset(dayfile, BIGC == '15CNW' & PEN == '15' & SEC >= 0)
  dayfile.NECor <- subset(dayfile, BIGC == '15CNE' & PEN == '15' & SEC >= 0)
  dayfile.SWCor <- subset(dayfile, BIGC == '15CSW' & PEN == '15' & SEC >= 0)
  dayfile.SECor <- subset(dayfile, BIGC == '15CSE' & PEN == '15' & SEC >= 0)
  dayfile.cen <- subset(dayfile, CEN == '15MH' & PEN == '15' & SEC >= 0 | CEN == '15MM' & PEN == '15' & SEC >= 0 | CEN == '15ML' & PEN == '15' & SEC >= 0)
  dayfile.hid <- subset(dayfile, HID == 'P15HW' & PEN == '15' & SEC >= 0 | HID == 'P15HE' & PEN == '15' & SEC >= 0)
  dayfile.fs <- subset(dayfile, FS == 'FS15' & PEN == '15' & SEC >= 0)
  #location.sum$Pen15 <- c(nrow(dayfile.bot), nrow(dayfile.top), nrow(dayfile.out), nrow(dayfile.edg), nrow(dayfile.bigc), nrow(dayfile.cen), nrow(dayfile.hid))
  location.sum$Pen15 <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
  location.sum
}
  

# 2. location summary for multiple day files
batch.locations <- function()
{
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  locations.P12 <- data.frame(c('0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'))
  colnames(locations.P12) <- 'ID'
  rownames(locations.P12) <- c('P12_<15m', 'P12_>15m', 'P12_outer', 'P12_edge', 'P12_NWcorner', 'P12_NEcorner', 'P12_SWcorner', 'P12_SEcorner', 'P12_centre', 'P12_hides', 'P12_feedstation')
  locations.P14 <- data.frame(c('0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'))
  colnames(locations.P14) <- 'ID'
  rownames(locations.P14) <- c('P14_<15m', 'P14_>15m', 'P14_outer', 'P14_edge', 'P14_NWcorner', 'P14_NEcorner', 'P14_SWcorner', 'P14_SEcorner', 'P14_centre', 'P14_hides', 'P14_feedstation')
  locations.P15 <- data.frame(c('0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'))
  colnames(locations.P15) <- 'ID'
  rownames(locations.P15) <- c('P15_<15m', 'P15_>15m', 'P15_outer', 'P15_edge', 'P15_NWcorner', 'P15_NEcorner', 'P15_SWcorner', 'P15_SEcorner', 'P15_centre', 'P15_hides', 'P15_feedstation')
  
  for (i in 1:length(files))
  {
    dayfile.loc <- files[[i]]
    dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = dayfile.classes)
    
    
    #SORT BY TIME AND TAG
    dayfile <- dayfile[order(dayfile$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
    dayfile <- dayfile[order(dayfile$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
    
    
    # pen 12 location summary
    dayfile.bot <- subset(dayfile, BOT == 'B' & PEN == '12' & SEC >= 0)
    dayfile.top <- subset(dayfile, BOT == 'Z' & PEN == '12' & SEC >= 0)
    dayfile.out <- subset(dayfile, OUT == '12OE' & PEN == '12' & SEC >= 0 | OUT == '12OS' & PEN == '12' & SEC >= 0 | OUT == '12ON' & PEN == '12' & SEC >= 0 | OUT == '12OW' & PEN == '12' & SEC >= 0)
    dayfile.edg <- subset(dayfile, EDG == '12EN' & PEN == '12' & SEC >= 0 | EDG == '12EW' & PEN == '12' & SEC >= 0 | EDG == '12ES' & PEN == '12' & SEC >= 0 | EDG == '12EE' & PEN == '12' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, BIGC == '12CNW' & PEN == '12' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, BIGC == '12CNE' & PEN == '12' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, BIGC == '12CSW' & PEN == '12' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, BIGC == '12CSE' & PEN == '12' & SEC >= 0)
    dayfile.cen <- subset(dayfile, CEN == '12MH' & PEN == '12' & SEC >= 0 | CEN == '12MM' & PEN == '12' & SEC >= 0 | CEN == '12ML' & PEN == '12' & SEC >= 0)
    dayfile.hid <- subset(dayfile, HID == 'P12HW' & PEN == '12' & SEC >= 0 | HID == 'P12HE' & PEN == '12' & SEC >= 0)
    dayfile.fs <- subset(dayfile, FS == 'FS12'  & PEN == '12' & SEC >= 0)
    locations.P12[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 14 location summary
    dayfile.bot <- subset(dayfile, BOT == 'B' & SEC >= 0 & PEN == '14' & SEC >= 0)
    dayfile.top <- subset(dayfile, BOT == 'Z' & PEN == '14' & SEC >= 0)
    dayfile.out <- subset(dayfile, OUT == '14OE' & PEN == '14' & SEC >= 0| OUT == '14OS' & PEN == '14' & SEC >= 0 | OUT == '14ON' & PEN == '14' & SEC >= 0 | OUT == '14OW' & PEN == '14' & SEC >= 0)
    dayfile.edg <- subset(dayfile, EDG == '14EN' & PEN == '14' & SEC >= 0 | EDG == '14EW' & PEN == '14' & SEC >= 0 | EDG == '14ES' & PEN == '14' & SEC >= 0 | EDG == '14EE' & PEN == '14' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, BIGC == '14CNW' & PEN == '14' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, BIGC == '14CNE' & PEN == '14' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, BIGC == '14CSW' & PEN == '14' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, BIGC == '14CSE' & PEN == '14' & SEC >= 0)
    dayfile.cen <- subset(dayfile, CEN == '14MH' & PEN == '14' & SEC >= 0 | CEN == '14MM' & PEN == '14' & SEC >= 0 | CEN == '14ML' & PEN == '14' & SEC >= 0)
    dayfile.hid <- subset(dayfile, HID == 'P14HW' & PEN == '14' & SEC >= 0 | HID == 'P14HE' & PEN == '14' & SEC >= 0)
    dayfile.fs <- subset(dayfile, FS == 'FS14' & PEN == '14' & SEC >= 0)
    locations.P14[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 15 location summary
    dayfile.bot <- subset(dayfile, BOT == 'B' & SEC >= 0 & PEN == '15' & SEC >= 0)
    dayfile.top <- subset(dayfile, BOT == 'Z' & PEN == '15' & SEC >= 0)
    dayfile.out <- subset(dayfile, OUT == '15OE' & PEN == '15' & SEC >= 0| OUT == '15OS' & PEN == '15' & SEC >= 0 | OUT == '15ON' & PEN == '15' & SEC >= 0 | OUT == '15OW' & PEN == '15' & SEC >= 0)
    dayfile.edg <- subset(dayfile, EDG == '15EN' & PEN == '15' & SEC >= 0 | EDG == '15EW' & PEN == '15' & SEC >= 0 | EDG == '15ES' & PEN == '15' & SEC >= 0 | EDG == '15EE' & PEN == '15' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, BIGC == '15CNW' & PEN == '15' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, BIGC == '15CNE' & PEN == '15' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, BIGC == '15CSW' & PEN == '15' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, BIGC == '15CSE' & PEN == '15' & SEC >= 0)
    dayfile.cen <- subset(dayfile, CEN == '15MH' & PEN == '15' & SEC >= 0 | CEN == '15MM' & PEN == '15' & SEC >= 0 | CEN == '15ML' & PEN == '15' & SEC >= 0)
    dayfile.hid <- subset(dayfile, HID == 'P15HW' & PEN == '15' & SEC >= 0 | HID == 'P15HE' & PEN == '15' & SEC >= 0)
    dayfile.fs <- subset(dayfile, FS == 'FS15' & PEN == '15' & SEC >= 0)
    locations.P15[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
  }
  location.sum <- rbind(locations.P12, locations.P14, locations.P15)  
  location.sum$ID <- NULL
  location.sum  
  
  #loadWorkbook('LocationsOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('LocationsOutput.xlsx', location.sum, 'Sheet 1')
  
  write.xlsx(location.sum, 'LocationsOutput.xlsx')
}



# 3a. depth and activity summary
depact <- function()
{
  day <- subset(dayfile, SUN == 'D' & PEN == '12')
  night <- subset(dayfile, SUN == 'N' & PEN == '12')
  depact.sum <- data.frame(c(format(mean(day$PosZ), digits = 4), format(mean(night$PosZ), digits = 4), format(mean(day$MSEC), digits = 4), format(mean(night$MSEC), digits = 4)))
  rownames(depact.sum) <- c('mean depth day (m)', 'mean depth night (m)', 'mean activity day (BL/sec)', 'mean activity night (BL/sec)')
  colnames(depact.sum) <- 'mean.P12'
  depact.sum$sd.P12 <-c(format(sd(day$PosZ), digits = 4), format(sd(night$PosZ), digits = 4), format(sd(day$MSEC), digits = 4), format(sd(night$MSEC), digits = 4))
  
  
  day <- subset(dayfile, SUN == 'D' & PEN == '14')
  night <- subset(dayfile, SUN == 'N' & PEN == '14')
  depact.sum$mean.P14 <-c(format(mean(day$PosZ), digits = 4), format(mean(night$PosZ), digits = 4), format(mean(day$MSEC), digits = 4), format(mean(night$MSEC), digits = 4))
  depact.sum$sd.P14 <-c(format(sd(day$PosZ), digits = 4), format(sd(night$PosZ), digits = 4), format(sd(day$MSEC), digits = 4), format(sd(night$MSEC), digits = 4))
  
  
  day <- subset(dayfile, SUN == 'D' & PEN == '15')
  night <- subset(dayfile, SUN == 'N' & PEN == '15')
  depact.sum$mean.P15 <-c(format(mean(day$PosZ), digits = 4), format(mean(night$PosZ), digits = 4), format(mean(day$MSEC), digits = 4), format(mean(night$MSEC), digits = 4))
  depact.sum$sd.P15 <-c(format(sd(day$PosZ), digits = 4), format(sd(night$PosZ), digits = 4), format(sd(day$MSEC), digits = 4), format(sd(night$MSEC), digits = 4))
  depact.sum
}


# 3b. depth and activity summary
depact.se <- function()
{
  day <- subset(dayfile, SUN == 'D' & PEN == '12')
  night <- subset(dayfile, SUN == 'N' & PEN == '12')
  depact.sum <- data.frame(c(format(mean(day$PosZ), digits = 4), format(mean(night$PosZ), digits = 4), format(mean(day$MSEC), digits = 4), format(mean(night$MSEC), digits = 4)))
  rownames(depact.sum) <- c('mean depth day (m)', 'mean depth night (m)', 'mean activity day (BL/sec)', 'mean activity night (BL/sec)')
  colnames(depact.sum) <- 'mean.P12'
  depact.sum$sd.P12 <-c(format(sd(day$PosZ)/sqrt(length(day$PosZ)), digits = 4), format(sd(night$PosZ)/sqrt(length(night$PosZ)), digits = 4), format(sd(day$MSEC)/sqrt(length(day$MSEC)), digits = 4), format(sd(night$MSEC)/sqrt(length(night$MSEC)), digits = 4))
  
  
  day <- subset(dayfile, SUN == 'D' & PEN == '14')
  night <- subset(dayfile, SUN == 'N' & PEN == '14')
  depact.sum$mean.UnconP14 <-c(format(mean(day$PosZ), digits = 4), format(mean(night$PosZ), digits = 4), format(mean(day$MSEC), digits = 4), format(mean(night$MSEC), digits = 4))
  depact.sum$sd.P14 <-c(format(sd(day$PosZ)/sqrt(length(day$PosZ)), digits = 4), format(sd(night$PosZ)/sqrt(length(night$PosZ)), digits = 4), format(sd(day$MSEC)/sqrt(length(day$MSEC)), digits = 4), format(sd(night$MSEC)/sqrt(length(night$MSEC)), digits = 4))
  
  
  day <- subset(dayfile, SUN == 'D' & PEN == '15')
  night <- subset(dayfile, SUN == 'N' & PEN == '15')
  depact.sum$mean.UnconP15 <-c(format(mean(day$PosZ), digits = 4), format(mean(night$PosZ), digits = 4), format(mean(day$MSEC), digits = 4), format(mean(night$MSEC), digits = 4))
  depact.sum$sd.P15 <-c(format(sd(day$PosZ), digits = 4), format(sd(night$PosZ), digits = 4), format(sd(day$MSEC), digits = 4), format(sd(night$MSEC), digits = 4))
  depact.sum
}

# 4. function to return depth summary for each fish

depth.sum <- function(){
  sumfunc <- function(x){ c(min = min(x), max = max(x), range = max(x)-min(x), mean = mean(x), median = median(x), std = sd(x)) }
  depth.sum.tab <- cbind(Period = unique(dayfile$Period), do.call(rbind, tapply(dayfile$PosZ, dayfile$Period, sumfunc)))
  print(depth.sum.tab)
}


# 5. batch function to return matrix of mean and standard deviation depths for individual fish over multiple days

batch.depth <- function(){
  
  sumfunc <- function(x){ c(min = min(x), max = max(x), range = max(x)-min(x), mean = mean(x), median = median(x), std = sd(x)) }
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  depths.P12 <- data.frame(c('P12_dawn_mean', 'P12_dawn_stdev', 'P12_day_mean', 'P12_day_stdev', 'P12_dusk_mean', 'P12_dusk_stdev', 'P12_night_mean', 'P12_night_stdev'))
  colnames(depths.P12) <- 'ID'
  rownames(depths.P12) <- c('P12_dawn_mean', 'P12_dawn_stdev', 'P12_day_mean', 'P12_day_stdev', 'P12_dusk_mean', 'P12_dusk_stdev', 'P12_night_mean', 'P12_night_stdev')
  depths.P14 <- data.frame(c('P14_dawn_mean', 'P14_dawn_stdev', 'P14_day_mean', 'P14_day_stdev', 'P14_dusk_mean', 'P14_dusk_stdev', 'P14_night_mean', 'P14_night_stdev'))
  colnames(depths.P14) <- 'ID'
  rownames(depths.P14) <- c('P14_dawn_mean', 'P14_dawn_stdev', 'P14_day_mean', 'P14_day_stdev', 'P14_dusk_mean', 'P14_dusk_stdev', 'P14_night_mean', 'P14_night_stdev')
  depths.P15 <- data.frame(c('P15_dawn_mean', 'P15_dawn_stdev', 'P15_day_mean', 'P15_day_stdev', 'P15_dusk_mean', 'P15_dusk_stdev', 'P15_night_mean', 'P15_night_stdev'))
  colnames(depths.P15) <- 'ID'
  rownames(depths.P15) <- c('P15_dawn_mean', 'P15_dawn_stdev', 'P15_day_mean', 'P15_day_stdev', 'P15_dusk_mean', 'P15_dusk_stdev', 'P15_night_mean', 'P15_night_stdev')
  
  for (i in 1:length(files))
  {
    dayfile.loc <- files[[i]]
    dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = dayfile.classes) #read data into table
    
    #SORT BY TIME AND TAG
    dayfile <- dayfile[order(dayfile$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
    dayfile <- dayfile[order(dayfile$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
    
    
    depths.dawn <- subset(dayfile, SUN == 'W' & PEN == '12')
    depths.day <- subset(dayfile, SUN == 'D' & PEN == '12')
    depths.dusk <- subset(dayfile, SUN == 'K' & PEN == '12')
    depths.night <- subset(dayfile, SUN == 'N' & PEN == '12')
    dawn.sum <- cbind(Period = unique(depths.dawn$Period), do.call(rbind, tapply(depths.dawn$PosZ, depths.dawn$Period, sumfunc)))
    day.sum <- cbind(Period = unique(depths.day$Period), do.call(rbind, tapply(depths.day$PosZ, depths.day$Period, sumfunc)))
    dusk.sum <- cbind(Period = unique(depths.dusk$Period), do.call(rbind, tapply(depths.dusk$PosZ, depths.dusk$Period, sumfunc)))
    night.sum <- cbind(Period = unique(depths.night$Period), do.call(rbind, tapply(depths.night$PosZ, depths.night$Period, sumfunc)))
    dawn.sum[is.na(dawn.sum)] <- 0
    day.sum[is.na(day.sum)] <- 0
    dusk.sum[is.na(dusk.sum)] <- 0
    night.sum[is.na(night.sum)] <- 0
    depths.P12[,as.character(i)] <- c(mean(dawn.sum[,'mean']), mean(dawn.sum[,'std']), mean(day.sum[,'mean']), mean(day.sum[,'std']), mean(dusk.sum[,'mean']), mean(dusk.sum[,'std']), mean(night.sum[,'mean']), mean(night.sum[,'std']))
    
    depths.dawn <- subset(dayfile, SUN == 'W' & PEN == '14')
    depths.day <- subset(dayfile, SUN == 'D' & PEN == '14')
    depths.dusk <- subset(dayfile, SUN == 'K' & PEN == '14')
    depths.night <- subset(dayfile, SUN == 'N' & PEN == '14')
    dawn.sum <- cbind(Period = unique(depths.dawn$Period), do.call(rbind, tapply(depths.dawn$PosZ, depths.dawn$Period, sumfunc)))
    day.sum <- cbind(Period = unique(depths.day$Period), do.call(rbind, tapply(depths.day$PosZ, depths.day$Period, sumfunc)))
    dusk.sum <- cbind(Period = unique(depths.dusk$Period), do.call(rbind, tapply(depths.dusk$PosZ, depths.dusk$Period, sumfunc)))
    night.sum <- cbind(Period = unique(depths.night$Period), do.call(rbind, tapply(depths.night$PosZ, depths.night$Period, sumfunc)))
    dawn.sum[is.na(dawn.sum)] <- 0
    day.sum[is.na(day.sum)] <- 0
    dusk.sum[is.na(dusk.sum)] <- 0
    night.sum[is.na(night.sum)] <- 0
    depths.P14[,as.character(i)] <- c(mean(dawn.sum[,'mean']), mean(dawn.sum[,'std']), mean(day.sum[,'mean']), mean(day.sum[,'std']), mean(dusk.sum[,'mean']), mean(dusk.sum[,'std']), mean(night.sum[,'mean']), mean(night.sum[,'std']))
    
    depths.dawn <- subset(dayfile, SUN == 'W' & PEN == '15')
    depths.day <- subset(dayfile, SUN == 'D' & PEN == '15')
    depths.dusk <- subset(dayfile, SUN == 'K' & PEN == '15')
    depths.night <- subset(dayfile, SUN == 'N' & PEN == '15')
    dawn.sum <- cbind(Period = unique(depths.dawn$Period), do.call(rbind, tapply(depths.dawn$PosZ, depths.dawn$Period, sumfunc)))
    day.sum <- cbind(Period = unique(depths.day$Period), do.call(rbind, tapply(depths.day$PosZ, depths.day$Period, sumfunc)))
    dusk.sum <- cbind(Period = unique(depths.dusk$Period), do.call(rbind, tapply(depths.dusk$PosZ, depths.dusk$Period, sumfunc)))
    night.sum <- cbind(Period = unique(depths.night$Period), do.call(rbind, tapply(depths.night$PosZ, depths.night$Period, sumfunc)))
    dawn.sum[is.na(dawn.sum)] <- 0
    day.sum[is.na(day.sum)] <- 0
    dusk.sum[is.na(dusk.sum)] <- 0
    night.sum[is.na(night.sum)] <- 0
    depths.P15[,as.character(i)] <- c(mean(dawn.sum[,'mean']), mean(dawn.sum[,'std']), mean(day.sum[,'mean']), mean(day.sum[,'std']), mean(dusk.sum[,'mean']), mean(dusk.sum[,'std']), mean(night.sum[,'mean']), mean(night.sum[,'std']))
  }
  
  depths.sum <- rbind(depths.P12, depths.P14, depths.P15)  
  #depths.sum$ID <- NULL
  depths.sum    
  loadWorkbook('DepthsOutput.xlsx', create = TRUE)
  writeWorksheetToFile('DepthsOutput.xlsx', depths.sum, 'Sheet 1')
}


# 6. batch function to return matrix of mean and standard error depths for all fish combined over multiple days

batch.totdepth <- function(){
  
  sumfunc <- function(x){ c(min = min(x), max = max(x), range = max(x)-min(x), mean = mean(x), median = median(x), std = sd(x)) }
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  depth.P12 <- data.frame(c('P12_dawn_mean', 'P12_dawn_se', 'P12_day_mean', 'P12_day_se', 'P12_dusk_mean', 'P12_dusk_se', 'P12_night_mean', 'P12_night_se'))
  colnames(depth.P12) <- 'ID'
  rownames(depth.P12) <- c('P12_dawn_mean', 'P12_dawn_se', 'P12_day_mean', 'P12_day_se', 'P12_dusk_mean', 'P12_dusk_se', 'P12_night_mean', 'P12_night_se')
  depth.P14 <- data.frame(c('P14_dawn_mean', 'P14_dawn_se', 'P14_day_mean', 'P14_day_se', 'P14_dusk_mean', 'P14_dusk_se', 'P14_night_mean', 'P14_night_se'))
  colnames(depth.P14) <- 'ID'
  rownames(depth.P14) <- c('P14_dawn_mean', 'P14_dawn_se', 'P14_day_mean', 'P14_day_se', 'P14_dusk_mean', 'P14_dusk_se', 'P14_night_mean', 'P14_night_se')
  depth.P15 <- data.frame(c('P15_dawn_mean', 'P15_dawn_se', 'P15_day_mean', 'P15_day_se', 'P15_dusk_mean', 'P15_dusk_se', 'P15_night_mean', 'P15_night_se'))
  colnames(depth.P15) <- 'ID'
  rownames(depth.P15) <- c('P15_dawn_mean', 'P15_dawn_se', 'P15_day_mean', 'P15_day_se', 'P15_dusk_mean', 'P15_dusk_se', 'P15_night_mean', 'P15_night_se')
  
  for (i in 1:length(files))
  {
    dayfile.loc <- files[[i]]
    dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = dayfile.classes) #read data into table
    
    
    depth.dawn <- subset(dayfile, SUN == 'W' & PEN == '12')
    depth.day <- subset(dayfile, SUN == 'D' & PEN == '12')
    depth.dusk <- subset(dayfile, SUN == 'K' & PEN == '12')
    depth.night <- subset(dayfile, SUN == 'N' & PEN == '12')
    depth.P12[,as.character(i)] <- c(mean(depth.dawn$PosZ), sd(depth.dawn$PosZ)/sqrt(length(depth.dawn)), mean(depth.day$PosZ), sd(depth.day$PosZ)/sqrt(length(depth.day)), mean(depth.dusk$PosZ), sd(depth.dusk$PosZ)/sqrt(length(depth.dusk)), mean(depth.night$PosZ), sd(depth.night$PosZ)/sqrt(length(depth.night)))
    
    depth.dawn <- subset(dayfile, SUN == 'W' & PEN == '14')
    depth.day <- subset(dayfile, SUN == 'D' & PEN == '14')
    depth.dusk <- subset(dayfile, SUN == 'K' & PEN == '14')
    depth.night <- subset(dayfile, SUN == 'N' & PEN == '14')
    depth.P14[,as.character(i)] <- c(mean(depth.dawn$PosZ), sd(depth.dawn$PosZ)/sqrt(length(depth.dawn)), mean(depth.day$PosZ), sd(depth.day$PosZ)/sqrt(length(depth.day)), mean(depth.dusk$PosZ), sd(depth.dusk$PosZ)/sqrt(length(depth.dusk)), mean(depth.night$PosZ), sd(depth.night$PosZ)/sqrt(length(depth.night)))
    
    depth.dawn <- subset(dayfile, SUN == 'W' & PEN == '15')
    depth.day <- subset(dayfile, SUN == 'D' & PEN == '15')
    depth.dusk <- subset(dayfile, SUN == 'K' & PEN == '15')
    depth.night <- subset(dayfile, SUN == 'N' & PEN == '15')
    depth.P15[,as.character(i)] <- c(mean(depth.dawn$PosZ), sd(depth.dawn$PosZ)/sqrt(length(depth.dawn)), mean(depth.day$PosZ), sd(depth.day$PosZ)/sqrt(length(depth.day)), mean(depth.dusk$PosZ), sd(depth.dusk$PosZ)/sqrt(length(depth.dusk)), mean(depth.night$PosZ), sd(depth.night$PosZ)/sqrt(length(depth.night)))
    
  }
  
  depths.sum <- rbind(depth.P12, depth.P14, depth.P15)  
  #depths.sum$ID <- NULL
  depths.sum    
  #loadWorkbook('DepthTotOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('DepthTotOutput.xlsx', depths.sum, 'Sheet 1')
  
  write.xlsx(depths.sum, 'DepthTotOutput.xlsx')
}


# 7. batch function to return matrix of mean and standard deviation activity for individual fish over multiple days

batch.activity <- function(){
  
  sumfunc <- function(x){ c(min = min(x), max = max(x), range = max(x)-min(x), mean = mean(x), median = median(x), std = sd(x)) }
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  activity.P12 <- data.frame(c('P12_dawn_mean', 'P12_dawn_stdev', 'P12_day_mean', 'P12_day_stdev', 'P12_dusk_mean', 'P12_dusk_stdev', 'P12_night_mean', 'P12_night_stdev'))
  colnames(activity.P12) <- 'ID'
  rownames(activity.P12) <- c('P12_dawn_mean', 'P12_dawn_stdev', 'P12_day_mean', 'P12_day_stdev', 'P12_dusk_mean', 'P12_dusk_stdev', 'P12_night_mean', 'P12_night_stdev')
  activity.P14 <- data.frame(c('P14_dawn_mean', 'P14_dawn_stdev', 'P14_day_mean', 'P14_day_stdev', 'P14_dusk_mean', 'P14_dusk_stdev', 'P14_night_mean', 'P14_night_stdev'))
  colnames(activity.P14) <- 'ID'
  rownames(activity.P14) <- c('P14_dawn_mean', 'P14_dawn_stdev', 'P14_day_mean', 'P14_day_stdev', 'P14_dusk_mean', 'P14_dusk_stdev', 'P14_night_mean', 'P14_night_stdev')
  activity.P15 <- data.frame(c('P15_dawn_mean', 'P15_dawn_stdev', 'P15_day_mean', 'P15_day_stdev', 'P15_dusk_mean', 'P15_dusk_stdev', 'P15_night_mean', 'P15_night_stdev'))
  colnames(activity.P15) <- 'ID'
  rownames(activity.P15) <- c('P15_dawn_mean', 'P15_dawn_stdev', 'P15_day_mean', 'P15_day_stdev', 'P15_dusk_mean', 'P15_dusk_stdev', 'P15_night_mean', 'P15_night_stdev')
  
  
  for (i in 1:length(files))
  {
    dayfile.loc <- files[[i]]
    dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = c
                        (
                          'NULL', 'factor', 'factor', 'factor', 'POSIXct', 'double', 'double', 
                          'double', 'double', 'double', 'double', 'double', 'double', 'factor',
                          'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                          'double', 'double', 'double', 'double', 'double', 'double', 'double',
                          'double', 'double', 'double', 'double', 'double', 'double', 'double',
                          'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                          'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                          'factor', 'factor', 'double', 'double', 'double', 'double', 'double', 
                          'double', 'double', 'double', 'double', 'double', 'double', 'double' 
                        )) #read data into table
    
    
    activity.dawn <- subset(dayfile, SUN == 'W' & PEN == '12')
    activity.day <- subset(dayfile, SUN == 'D' & PEN == '12')
    activity.dusk <- subset(dayfile, SUN == 'K' & PEN == '12')
    activity.night <- subset(dayfile, SUN == 'N' & PEN == '12')
    dawn.sum <- cbind(Period = unique(activity.dawn$Period), do.call(rbind, tapply(activity.dawn$BLSEC, activity.dawn$Period, sumfunc)))
    day.sum <- cbind(Period = unique(activity.day$Period), do.call(rbind, tapply(activity.day$BLSEC, activity.day$Period, sumfunc)))
    dusk.sum <- cbind(Period = unique(activity.dusk$Period), do.call(rbind, tapply(activity.dusk$BLSEC, activity.dusk$Period, sumfunc)))
    night.sum <- cbind(Period = unique(activity.night$Period), do.call(rbind, tapply(activity.night$BLSEC, activity.night$Period, sumfunc)))
    dawn.sum[is.na(dawn.sum)] <- 0
    day.sum[is.na(day.sum)] <- 0
    dusk.sum[is.na(dusk.sum)] <- 0
    night.sum[is.na(night.sum)] <- 0
    activity.P12[,as.character(i)] <- c(mean(dawn.sum[,'mean']), mean(dawn.sum[,'std']), mean(day.sum[,'mean']), mean(day.sum[,'std']), mean(dusk.sum[,'mean']), mean(dusk.sum[,'std']), mean(night.sum[,'mean']), mean(night.sum[,'std']))
    
    activity.dawn <- subset(dayfile, SUN == 'W' & PEN == '14')
    activity.day <- subset(dayfile, SUN == 'D' & PEN == '14')
    activity.dusk <- subset(dayfile, SUN == 'K' & PEN == '14')
    activity.night <- subset(dayfile, SUN == 'N' & PEN == '14')
    dawn.sum <- cbind(Period = unique(activity.dawn$Period), do.call(rbind, tapply(activity.dawn$BLSEC, activity.dawn$Period, sumfunc)))
    day.sum <- cbind(Period = unique(activity.day$Period), do.call(rbind, tapply(activity.day$BLSEC, activity.day$Period, sumfunc)))
    dusk.sum <- cbind(Period = unique(activity.dusk$Period), do.call(rbind, tapply(activity.dusk$BLSEC, activity.dusk$Period, sumfunc)))
    night.sum <- cbind(Period = unique(activity.night$Period), do.call(rbind, tapply(activity.night$BLSEC, activity.night$Period, sumfunc)))
    dawn.sum[is.na(dawn.sum)] <- 0
    day.sum[is.na(day.sum)] <- 0
    dusk.sum[is.na(dusk.sum)] <- 0
    night.sum[is.na(night.sum)] <- 0
    activity.P14[,as.character(i)] <- c(mean(dawn.sum[,'mean']), mean(dawn.sum[,'std']), mean(day.sum[,'mean']), mean(day.sum[,'std']), mean(dusk.sum[,'mean']), mean(dusk.sum[,'std']), mean(night.sum[,'mean']), mean(night.sum[,'std']))
    
    activity.dawn <- subset(dayfile, SUN == 'W' & PEN == '15')
    activity.day <- subset(dayfile, SUN == 'D' & PEN == '15')
    activity.dusk <- subset(dayfile, SUN == 'K' & PEN == '15')
    activity.night <- subset(dayfile, SUN == 'N' & PEN == '15')
    dawn.sum <- cbind(Period = unique(activity.dawn$Period), do.call(rbind, tapply(activity.dawn$BLSEC, activity.dawn$Period, sumfunc)))
    day.sum <- cbind(Period = unique(activity.day$Period), do.call(rbind, tapply(activity.day$BLSEC, activity.day$Period, sumfunc)))
    dusk.sum <- cbind(Period = unique(activity.dusk$Period), do.call(rbind, tapply(activity.dusk$BLSEC, activity.dusk$Period, sumfunc)))
    night.sum <- cbind(Period = unique(activity.night$Period), do.call(rbind, tapply(activity.night$BLSEC, activity.night$Period, sumfunc)))
    dawn.sum[is.na(dawn.sum)] <- 0
    day.sum[is.na(day.sum)] <- 0
    dusk.sum[is.na(dusk.sum)] <- 0
    night.sum[is.na(night.sum)] <- 0
    activity.P15[,as.character(i)] <- c(mean(dawn.sum[,'mean']), mean(dawn.sum[,'std']), mean(day.sum[,'mean']), mean(day.sum[,'std']), mean(dusk.sum[,'mean']), mean(dusk.sum[,'std']), mean(night.sum[,'mean']), mean(night.sum[,'std']))
  }
  
  activity.sum <- rbind(activity.P12, activity.P14, activity.P15)  
  #depths.sum$ID <- NULL
  activity.sum    
  #loadWorkbook('ActivityOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('ActivityOutput.xlsx', activity.sum, 'Sheet 1')
  
  write.xlsx(activity.sum, 'ActivityOutput.xlsx')
}


# 8. batch function to return matrix of mean and standard error activity for all fish combined over multiple days

batch.totactivity <- function(){
  
  sumfunc <- function(x){ c(min = min(x), max = max(x), range = max(x)-min(x), mean = mean(x), median = median(x), std = sd(x)) }
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  activity.P12 <- data.frame(c('P12_dawn_mean', 'P12_dawn_se', 'P12_day_mean', 'P12_day_se', 'P12_dusk_mean', 'P12_dusk_se', 'P12_night_mean', 'P12_night_se'))
  colnames(activity.P12) <- 'ID'
  rownames(activity.P12) <- c('P12_dawn_mean', 'P12_dawn_se', 'P12_day_mean', 'P12_day_se', 'P12_dusk_mean', 'P12_dusk_se', 'P12_night_mean', 'P12_night_se')
  activity.P14 <- data.frame(c('P14_dawn_mean', 'P14_dawn_se', 'P14_day_mean', 'P14_day_se', 'P14_dusk_mean', 'P14_dusk_se', 'P14_night_mean', 'P14_night_se'))
  colnames(activity.P14) <- 'ID'
  rownames(activity.P14) <- c('P14_dawn_mean', 'P14_dawn_se', 'P14_day_mean', 'P14_day_se', 'P14_dusk_mean', 'P14_dusk_se', 'P14_night_mean', 'P14_night_se')
  activity.P15 <- data.frame(c('P15_dawn_mean', 'P15_dawn_se', 'P15_day_mean', 'P15_day_se', 'P15_dusk_mean', 'P15_dusk_se', 'P15_night_mean', 'P15_night_se'))
  colnames(activity.P15) <- 'ID'
  rownames(activity.P15) <- c('P15_dawn_mean', 'P15_dawn_se', 'P15_day_mean', 'P15_day_se', 'P15_dusk_mean', 'P15_dusk_se', 'P15_night_mean', 'P15_night_se')
  
  for (i in 1:length(files))
  {
    dayfile.loc <- files[[i]]
    dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = dayfile.classes) 
    
    activity.dawn <- subset(dayfile, SUN == 'W' & PEN == '12')
    activity.day <- subset(dayfile, SUN == 'D' & PEN == '12')
    activity.dusk <- subset(dayfile, SUN == 'K' & PEN == '12')
    activity.night <- subset(dayfile, SUN == 'N' & PEN == '12')
    activity.P12[,as.character(i)] <- c(mean(activity.dawn$BLSEC, na.rm = T), sd(activity.dawn$BLSEC, na.rm = T)/sqrt(length(activity.dawn)), mean(activity.day$BLSEC, na.rm = T), sd(activity.day$BLSEC, na.rm = T)/sqrt(length(activity.day)), mean(activity.dusk$BLSEC, na.rm = T), sd(activity.dusk$BLSEC, na.rm = T)/sqrt(length(activity.dusk)), mean(activity.night$BLSEC, na.rm = T), sd(activity.night$BLSEC, na.rm = T)/sqrt(length(activity.night)))
    
    activity.dawn <- subset(dayfile, SUN == 'W' & PEN == '14')
    activity.day <- subset(dayfile, SUN == 'D' & PEN == '14')
    activity.dusk <- subset(dayfile, SUN == 'K' & PEN == '14')
    activity.night <- subset(dayfile, SUN == 'N' & PEN == '14')
    activity.P14[,as.character(i)] <- c(mean(activity.dawn$BLSEC, na.rm = T), sd(activity.dawn$BLSEC, na.rm = T)/sqrt(length(activity.dawn)), mean(activity.day$BLSEC, na.rm = T), sd(activity.day$BLSEC, na.rm = T)/sqrt(length(activity.day)), mean(activity.dusk$BLSEC, na.rm = T), sd(activity.dusk$BLSEC, na.rm = T)/sqrt(length(activity.dusk)), mean(activity.night$BLSEC, na.rm = T), sd(activity.night$BLSEC, na.rm = T)/sqrt(length(activity.night)))
    
    activity.dawn <- subset(dayfile, SUN == 'W' & PEN == '15')
    activity.day <- subset(dayfile, SUN == 'D' & PEN == '15')
    activity.dusk <- subset(dayfile, SUN == 'K' & PEN == '15')
    activity.night <- subset(dayfile, SUN == 'N' & PEN == '15')
    activity.P15[,as.character(i)] <- c(mean(activity.dawn$BLSEC, na.rm = T), sd(activity.dawn$BLSEC, na.rm = T)/sqrt(length(activity.dawn)), mean(activity.day$BLSEC, na.rm = T), sd(activity.day$BLSEC, na.rm = T)/sqrt(length(activity.day)), mean(activity.dusk$BLSEC, na.rm = T), sd(activity.dusk$BLSEC, na.rm = T)/sqrt(length(activity.dusk)), mean(activity.night$BLSEC, na.rm = T), sd(activity.night$BLSEC, na.rm = T)/sqrt(length(activity.night)))
  }
  
  activity.sum <- rbind(activity.P12, activity.P14, activity.P15)  
  #depths.sum$ID <- NULL
  activity.sum    
  #loadWorkbook('ActivityTotOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('ActivityTotOutput.xlsx', activity.sum, 'Sheet 1')
  
  write.xlsx(activity.sum, 'ActivityTotOutput.xlsx')
}

# 9a. proportion coverage

prop.coverage <- function(xmin12 = 40.5, xmax12 = 64.5, ymin12 = 40.5, ymax12 = 64.5, xmin14 = 15, xmax14 = 39, ymin14 = 40.5, ymax14 = 64.5, xmin15 = 15, xmax15 = 39, ymin15 = 15, ymax15 = 39, boxsize = 0.3) {
  fish.id <- subset(dayfile, PEN == '12')
  x.grid <- floor((fish.id$PosX - xmin12) / boxsize) + 1
  y.grid <- floor((fish.id$PosY - ymin12) / boxsize) + 1
  x.grid.max <- floor((xmax12 - xmin12) / boxsize) + 1
  y.grid.max <- floor((ymax12 - ymin12) / boxsize) + 1
  t.x <- sort(unique(x.grid))
  t.y <- sort(unique(y.grid))
  tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
  ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
  t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
  grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
  t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
  t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
  eg <- expand.grid(t.y,t.x)
  grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
  coverage.P12 <- matrix(c(length(which(grid.cov > 0)), length(grid.cov), length(which(grid.cov > 0))/length(grid.cov)), ncol = 3)
  colnames(coverage.P12) <- c('occupied', 'total', 'proportion')
  
  fish.id <- subset(dayfile, PEN == '14')
  x.grid <- floor((fish.id$PosX - xmin14) / boxsize) + 1
  y.grid <- floor((fish.id$PosY - ymin14) / boxsize) + 1
  x.grid.max <- floor((xmax14 - xmin14) / boxsize) + 1
  y.grid.max <- floor((ymax14 - ymin14) / boxsize) + 1
  t.x <- sort(unique(x.grid))
  t.y <- sort(unique(y.grid))
  tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
  ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
  t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
  grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
  t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
  t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
  eg <- expand.grid(t.y,t.x)
  grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
  coverage.P14 <- matrix(c(length(which(grid.cov > 0)), length(grid.cov), length(which(grid.cov > 0))/length(grid.cov)), ncol = 3)
  colnames(coverage.P14) <- c('occupied', 'total', 'proportion')
  
  fish.id <- subset(dayfile, PEN == '15')
  x.grid <- floor((fish.id$PosX - xmin15) / boxsize) + 1
  y.grid <- floor((fish.id$PosY - ymin15) / boxsize) + 1
  x.grid.max <- floor((xmax15 - xmin15) / boxsize) + 1
  y.grid.max <- floor((ymax15 - ymin15) / boxsize) + 1
  t.x <- sort(unique(x.grid))
  t.y <- sort(unique(y.grid))
  tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
  ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
  t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
  grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
  t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
  t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
  eg <- expand.grid(t.y,t.x)
  grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
  coverage.P15 <- matrix(c(length(which(grid.cov > 0)), length(grid.cov), length(which(grid.cov > 0))/length(grid.cov)), ncol = 3)
  colnames(coverage.P15) <- c('occupied', 'total', 'proportion')
  
  coverage <- rbind(coverage.P12, coverage.P14, coverage.P15) 
  rownames(coverage) <- c('P12', 'P14', 'P15')
  coverage
}


# 9b. mean proportion coverage per hour

hmean.prop.coverage <- function(xmin12 = 40.5, xmax12 = 64.5, ymin12 = 40.5, ymax12 = 64.5, xmin14 = 15, xmax14 = 39, ymin14 = 40.5, ymax14 = 64.5, xmin15 = 15, xmax15 = 39, ymin15 = 15, ymax15 = 39, boxsize = 0.3) {
  
  fish.id <- subset(dayfile, PEN == '12')
  
  fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
  starttime <- fish.id[1,'EchoTime']-seconds(1)
  nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
  fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
  
  occupied <- numeric()
  total <- numeric()
  proportion <- numeric()
  
  for (i in 1:nhours){
    
    hoursub <- fish.id[fish.id$EchoTime > starttime & fish.id$EchoTime < starttime+hours(1),]   
    
    x.grid <- floor((hoursub$PosX - xmin12) / boxsize) + 1
    y.grid <- floor((hoursub$PosY - ymin12) / boxsize) + 1
    x.grid.max <- floor((xmax12 - xmin12) / boxsize) + 1
    y.grid.max <- floor((ymax12 - ymin12) / boxsize) + 1
    t.x <- sort(unique(x.grid))
    t.y <- sort(unique(y.grid))
    tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
    ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
    t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
    grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
    t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
    t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
    eg <- expand.grid(t.y,t.x)
    grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
    occupied <- c(occupied, length(which(grid.cov > 0)))
    total <- c(total, length(grid.cov))
    proportion <- c(proportion, length(which(grid.cov > 0))/length(grid.cov))
    
    starttime <- starttime+hours(1)
    
  }
  
  coverage.P12 <- matrix(c(mean(occupied), mean(total), mean(proportion)), ncol = 3)
  colnames(coverage.P12) <- c('occupied', 'total', 'proportion')
  
  
  fish.id <- subset(dayfile, PEN == '14')
  
  fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
  starttime <- fish.id[1,'EchoTime']-seconds(1)
  nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
  fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
  
  occupied <- numeric()
  total <- numeric()
  proportion <- numeric()
  
  for (i in 1:nhours){
    
    hoursub <- fish.id[fish.id$EchoTime >starttime & fish.id$EchoTime <starttime+hours(1),]   
    
    x.grid <- floor((hoursub$PosX - xmin14) / boxsize) + 1
    y.grid <- floor((hoursub$PosY - ymin14) / boxsize) + 1
    x.grid.max <- floor((xmax14 - xmin14) / boxsize) + 1
    y.grid.max <- floor((ymax14 - ymin14) / boxsize) + 1
    t.x <- sort(unique(x.grid))
    t.y <- sort(unique(y.grid))
    tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
    ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
    t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
    grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
    t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
    t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
    eg <- expand.grid(t.y,t.x)
    grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
    
    occupied <- c(occupied, length(which(grid.cov > 0)))
    total <- c(total, length(grid.cov))
    proportion <- c(proportion, length(which(grid.cov > 0))/length(grid.cov))
    
    starttime <- starttime+hours(1)
    
  }
  
  coverage.P14 <- matrix(c(mean(occupied), mean(total), mean(proportion)), ncol = 3)
  colnames(coverage.P14) <- c('occupied', 'total', 'proportion')
  
  
  fish.id <- subset(dayfile, PEN == '15')
  
  fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
  starttime <- fish.id[1,'EchoTime']-seconds(1)
  nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
  fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
  
  occupied <- numeric()
  total <- numeric()
  proportion <- numeric()
  
  for (i in 1:nhours){
    
    hoursub <- fish.id[fish.id$EchoTime >starttime & fish.id$EchoTime <starttime+hours(1),]   
    
    x.grid <- floor((hoursub$PosX - xmin15) / boxsize) + 1
    y.grid <- floor((hoursub$PosY - ymin15) / boxsize) + 1
    x.grid.max <- floor((xmax15 - xmin15) / boxsize) + 1
    y.grid.max <- floor((ymax15 - ymin15) / boxsize) + 1
    t.x <- sort(unique(x.grid))
    t.y <- sort(unique(y.grid))
    tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
    ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
    t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
    grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
    t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
    t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
    eg <- expand.grid(t.y,t.x)
    grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
    
    occupied <- c(occupied, length(which(grid.cov > 0)))
    total <- c(total, length(grid.cov))
    proportion <- c(proportion, length(which(grid.cov > 0))/length(grid.cov))
    
    starttime <- starttime+hours(1)
    
  }
  
  coverage.P15 <- matrix(c(mean(occupied), mean(total), mean(proportion)), ncol = 3)
  colnames(coverage.P15) <- c('occupied', 'total', 'proportion')
  
  coverage <- rbind(coverage.P12, coverage.P14, coverage.P15) 
  rownames(coverage) <- c('P12', 'P14', 'P15')
  coverage
}



# 10a. batch proportion coverage

batch.coverage <- function(xmin12 = 40.5, xmax12 = 64.5, ymin12 = 40.5, ymax12 = 64.5, xmin14 = 15, xmax14 = 39, ymin14 = 40.5, ymax14 = 64.5, xmin15 = 15, xmax15 = 39, ymin15 = 15, ymax15 = 39, boxsize = 0.3) {
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  coverage.P12 <- data.frame(c('P12'))
  colnames(coverage.P12) <- 'ID'
  rownames(coverage.P12) <- c('P12')
  coverage.P14 <- data.frame(c('P14'))
  colnames(coverage.P14) <- 'ID'
  rownames(coverage.P14) <- c('P14')
  coverage.P15 <- data.frame(c('P15'))
  colnames(coverage.P15) <- 'ID'
  rownames(coverage.P15) <- c('P15')
  
  for (i in 1:length(files))
  {
    dayfile.loc <- files[[i]]
    dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = dayfile.classes)
    
    
    if(length(unique(dayfile$Period)) == 1) {
      
      if(unique(dayfile$PEN) == '12'){
        
        fish.id <- subset(dayfile, PEN == '12')
        x.grid <- floor((fish.id$PosX - xmin12) / boxsize) + 1
        y.grid <- floor((fish.id$PosY - ymin12) / boxsize) + 1
        x.grid.max <- floor((xmax12 - xmin12) / boxsize) + 1
        y.grid.max <- floor((ymax12 - ymin12) / boxsize) + 1
        t.x <- sort(unique(x.grid))
        t.y <- sort(unique(y.grid))
        tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
        ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
        t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
        grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
        t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
        t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
        eg <- expand.grid(t.y,t.x)
        grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
        coverage.P12[,as.character(i)] <-  length(which(grid.cov > 0))/length(grid.cov)
        coverage.P14[,as.character(i)] <- 'NA'
        coverage.P15[,as.character(i)] <- 'NA'
        
      }else{ if(unique(dayfile$PEN) == '14'){
        
        fish.id <- subset(dayfile, PEN == '14')
        x.grid <- floor((fish.id$PosX - xmin14) / boxsize) + 1
        y.grid <- floor((fish.id$PosY - ymin14) / boxsize) + 1
        x.grid.max <- floor((xmax14 - xmin14) / boxsize) + 1
        y.grid.max <- floor((ymax14 - ymin14) / boxsize) + 1
        t.x <- sort(unique(x.grid))
        t.y <- sort(unique(y.grid))
        tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
        ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
        t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
        grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
        t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
        t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
        eg <- expand.grid(t.y,t.x)
        grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
        coverage.P14[,as.character(i)] <- length(which(grid.cov > 0))/length(grid.cov)
        coverage.P12[,as.character(i)] <- 'NA'
        coverage.P15[,as.character(i)] <- 'NA'
        
      }    
      }else{ if(unique(dayfile$PEN) == '15'){
        
        fish.id <- subset(dayfile, PEN == '15')
        x.grid <- floor((fish.id$PosX - xmin15) / boxsize) + 1
        y.grid <- floor((fish.id$PosY - ymin15) / boxsize) + 1
        x.grid.max <- floor((xmax15 - xmin15) / boxsize) + 1
        y.grid.max <- floor((ymax15 - ymin15) / boxsize) + 1
        t.x <- sort(unique(x.grid))
        t.y <- sort(unique(y.grid))
        tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
        ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
        t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
        grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
        t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
        t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
        eg <- expand.grid(t.y,t.x)
        grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
        coverage.P15[,as.character(i)] <- length(which(grid.cov > 0))/length(grid.cov)
        coverage.P12[,as.character(i)] <- 'NA'
        coverage.P14[,as.character(i)] <- 'NA'
      }
      }
      
      
      else {
        
        fish.id <- subset(dayfile, PEN == '12')
        x.grid <- floor((fish.id$PosX - xmin12) / boxsize) + 1
        y.grid <- floor((fish.id$PosY - ymin12) / boxsize) + 1
        x.grid.max <- floor((xmax12 - xmin12) / boxsize) + 1
        y.grid.max <- floor((ymax12 - ymin12) / boxsize) + 1
        t.x <- sort(unique(x.grid))
        t.y <- sort(unique(y.grid))
        tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
        ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
        t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
        grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
        t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
        t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
        eg <- expand.grid(t.y,t.x)
        grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
        coverage.P12[,as.character(i)] <-  length(which(grid.cov > 0))/length(grid.cov)
        
        fish.id <- subset(dayfile, PEN == '14')
        x.grid <- floor((fish.id$PosX - xmin14) / boxsize) + 1
        y.grid <- floor((fish.id$PosY - ymin14) / boxsize) + 1
        x.grid.max <- floor((xmax14 - xmin14) / boxsize) + 1
        y.grid.max <- floor((ymax14 - ymin14) / boxsize) + 1
        t.x <- sort(unique(x.grid))
        t.y <- sort(unique(y.grid))
        tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
        ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
        t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
        grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
        t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
        t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
        eg <- expand.grid(t.y,t.x)
        grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
        coverage.P14[,as.character(i)] <- length(which(grid.cov > 0))/length(grid.cov)
        
        fish.id <- subset(dayfile, PEN == '15')
        x.grid <- floor((fish.id$PosX - xmin15) / boxsize) + 1
        y.grid <- floor((fish.id$PosY - ymin15) / boxsize) + 1
        x.grid.max <- floor((xmax15 - xmin15) / boxsize) + 1
        y.grid.max <- floor((ymax15 - ymin15) / boxsize) + 1
        t.x <- sort(unique(x.grid))
        t.y <- sort(unique(y.grid))
        tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
        ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
        t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
        grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
        t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
        t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
        eg <- expand.grid(t.y,t.x)
        grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
        coverage.P15[,as.character(i)] <- length(which(grid.cov > 0))/length(grid.cov)
        
      }  
    }
  }
  
  coverage <- rbind(coverage.P12, coverage.P14, coverage.P15)
  print(coverage)
  #loadWorkbook('CoverageOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('CoverageOutput.xlsx', coverage, 'Sheet 1')
  
  write.xlsx(coverage, 'CoverageOutput.xlsx')
}


#coverage old dimensions: xmin12 = 15, xmax12 = 39, ymin12 = 15, ymax12 = 39, xmin14 = 41, xmax14 = 65, ymin14 = 15, ymax14 = 39, xmin15 = 41, xmax15 = 65, ymin15 = 15, ymax15 = 39
# 10b. batch mean proportion coverage per hour

hmean.batch.coverage <- function(xmin12 = 41, xmax12 = 65, ymin12 = 41, ymax12 = 65, xmin14 = 15, xmax14 = 39, ymin14 = 41, ymax14 = 65, xmin15 = 41, xmax15 = 65, ymin15 = 41, ymax15 = 65, boxsize = 0.3) {
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  coverage.P12 <- data.frame(c('P12_mean_coverage', 'P12_sd'))
  colnames(coverage.P12) <- 'ID'
  rownames(coverage.P12) <- c('P12_mean_coverage', 'P12_sd')
  coverage.P14 <- data.frame(c('P14_mean_coverage', 'P14_sd'))
  colnames(coverage.P14) <- 'ID'
  rownames(coverage.P14) <- c('P14_mean_coverage', 'P14_sd')
  coverage.P15 <- data.frame(c('P15_mean_coverage', 'P15_sd'))
  colnames(coverage.P15) <- 'ID'
  rownames(coverage.P15) <- c('P15_mean_coverage', 'P15_sd')
  
  anova.list <- data.frame('P value')
  colnames(anova.list) <- 'ID'
  rownames(anova.list) <- 'P value'
  
  for (i in 1:length(files))
  {
    dayfile.loc <- files[[i]]
    dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = dayfile.classes) 
    #(
    #'NULL', 'factor', 'factor', 'factor', 'POSIXct', 'double', 'double', 
    #'double', 'double', 'double', 'double', 'double', 'double', 'factor',
    #'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
    #'double', 'double', 'double', 'double', 'double', 'double', 'double',
    #'double', 'double', 'double', 'double', 'double', 'double', 'double',
    #'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
    #'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
    #'factor', 'factor', 'double', 'double', 'double', 'double', 'double', 
    #'double', 'double', 'double', 'double', 'double', 'double', 'double' 
    #)) #read data into table
    
    if(length(unique(dayfile$Period)) == 1) {
      
      if(unique(dayfile$PEN) == '12'){
        
        fish.id <- subset(dayfile, PEN == '12')
        
        
        fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
        starttime <- fish.id[1,'EchoTime']-seconds(1)
        nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
        fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
        
        proportion.P12 <- numeric()
        
        for (j in 1:nhours){
          
          hoursub <- fish.id[fish.id$EchoTime > starttime & fish.id$EchoTime < starttime+hours(1),]  
          
          if (nrow(hoursub) > 1){
            
            
            x.grid <- floor((hoursub$PosX - xmin12) / boxsize) + 1
            y.grid <- floor((hoursub$PosY - ymin12) / boxsize) + 1
            x.grid.max <- floor((xmax12 - xmin12) / boxsize) + 1
            y.grid.max <- floor((ymax12 - ymin12) / boxsize) + 1
            t.x <- sort(unique(x.grid))
            t.y <- sort(unique(y.grid))
            tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
            ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
            t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
            grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
            t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
            t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
            eg <- expand.grid(t.y,t.x)
            grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
            
            proportion.P12 <- c(proportion.P12, length(which(grid.cov > 0))/length(grid.cov))
            
          } else {
            
            proportion.P12 <- c(proportion.P12, 0)  
            
          }
          
          starttime <- starttime+hours(1)
          
        }
        
        proportion.P12[proportion.P12 == 0] <- NA
        #coverage.P7[,as.character(i)] <-  mean(proportion, na.rm = T)
        coverage.P12[,as.character(i)] <-  c(mean(proportion.P12, na.rm = T), sd(proportion.P12, na.rm = T))
        coverage.P14[,as.character(i)] <- c('NA', 'NA')
        coverage.P15[,as.character(i)] <- c('NA', 'NA')
        
      }else{if(unique(dayfile$PEN) == '14'){
        
        fish.id <- subset(dayfile, PEN == '14')
        
        fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
        starttime <- fish.id[1,'EchoTime']-seconds(1)
        nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
        fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
        
        proportion.P14 <- numeric()
        
        for (j in 1:nhours){
          
          hoursub <- fish.id[fish.id$EchoTime >starttime & fish.id$EchoTime <starttime+hours(1),]   
          
          if (nrow(hoursub) > 1){
            
            x.grid <- floor((hoursub$PosX - xmin14) / boxsize) + 1
            y.grid <- floor((hoursub$PosY - ymin14) / boxsize) + 1
            x.grid.max <- floor((xmax14 - xmin14) / boxsize) + 1
            y.grid.max <- floor((ymax14 - ymin14) / boxsize) + 1
            t.x <- sort(unique(x.grid))
            t.y <- sort(unique(y.grid))
            tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
            ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
            t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
            grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
            t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
            t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
            eg <- expand.grid(t.y,t.x)
            grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
            
            proportion.P14 <- c(proportion.P14, length(which(grid.cov > 0))/length(grid.cov))
            
          } else {
            
            proportion.P14 <- c(proportion.P14, 0)  
            
          }
          
          
          starttime <- starttime+hours(1)
          
        }
        
        proportion.P14[proportion.P14 == 0] <- NA
        #coverage.P8[,as.character(i)] <-  mean(proportion, na.rm = T)
        coverage.P14[,as.character(i)] <- c(mean(proportion.P14, na.rm = T), sd(proportion.P14, na.rm = T))
        coverage.P12[,as.character(i)] <- c('NA', 'NA')
        coverage.P15[,as.character(i)] <- c('NA', 'NA')
        
      }
        else{if(unique(dayfile$PEN) == '15'){
          
          fish.id <- subset(dayfile, PEN == '15')
          
          fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
          starttime <- fish.id[1,'EchoTime']-seconds(1)
          nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
          fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
          
          proportion.P15 <- numeric()
          
          for (j in 1:nhours){
            
            hoursub <- fish.id[fish.id$EchoTime >starttime & fish.id$EchoTime <starttime+hours(1),]   
            
            if (nrow(hoursub) > 1){
              
              x.grid <- floor((hoursub$PosX - xmin15) / boxsize) + 1
              y.grid <- floor((hoursub$PosY - ymin15) / boxsize) + 1
              x.grid.max <- floor((xmax15 - xmin15) / boxsize) + 1
              y.grid.max <- floor((ymax15 - ymin15) / boxsize) + 1
              t.x <- sort(unique(x.grid))
              t.y <- sort(unique(y.grid))
              tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
              ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
              t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
              grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
              t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
              t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
              eg <- expand.grid(t.y,t.x)
              grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
              
              proportion.P15 <- c(proportion.P15, length(which(grid.cov > 0))/length(grid.cov))
              
            } else {
              
              proportion.P15 <- c(proportion.P15, 0)  
              
            }
            
            
            starttime <- starttime+hours(1)
            
          }
          
          proportion.P15[proportion.P15 == 0] <- NA
          #coverage.P8[,as.character(i)] <-  mean(proportion, na.rm = T)
          coverage.P15[,as.character(i)] <- c(mean(proportion.P15, na.rm = T), sd(proportion.P15, na.rm = T))
          coverage.P12[,as.character(i)] <- c('NA', 'NA')
          coverage.P14[,as.character(i)] <- c('NA', 'NA')
          
        }
        }
      }
      else {
        
        fish.id <- subset(dayfile, PEN == '12')
        
        
        fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
        starttime <- fish.id[1,'EchoTime']-seconds(1)
        nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
        fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
        
        proportion.P12 <- numeric()
        
        for (j in 1:nhours){
          
          hoursub <- fish.id[fish.id$EchoTime > starttime & fish.id$EchoTime < starttime+hours(1),]   
          
          if (nrow(hoursub) > 1){
            
            x.grid <- floor((hoursub$PosX - xmin12) / boxsize) + 1
            y.grid <- floor((hoursub$PosY - ymin12) / boxsize) + 1
            x.grid.max <- floor((xmax12 - xmin12) / boxsize) + 1
            y.grid.max <- floor((ymax12 - ymin12) / boxsize) + 1
            t.x <- sort(unique(x.grid))
            t.y <- sort(unique(y.grid))
            tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
            ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
            t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
            grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
            t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
            t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
            eg <- expand.grid(t.y,t.x)
            grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
            
            proportion.P12 <- c(proportion.P12, length(which(grid.cov > 0))/length(grid.cov))
            
          } else {
            
            proportion.P12 <- c(proportion.P12, 0)  
            
          }
          
          starttime <- starttime+hours(1)
          
        }
        
        proportion.P12[proportion.P12 == 0] <- NA
        #coverage.P12[,as.character(i)] <-  mean(proportion, na.rm = T)
        coverage.P12[,as.character(i)] <-  c(mean(proportion.P12, na.rm = T), sd(proportion.P12, na.rm = T))
        
        
        proportion.P12 <- as.data.frame(proportion.P12)
        proportion.P12$pen <- 12
        names(proportion.P12) <- c('proportion', 'pen')
        
        
        fish.id <- subset(dayfile, PEN == '14')
        
        fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
        starttime <- fish.id[1,'EchoTime']-seconds(1)
        nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
        fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
        
        proportion.P14 <- numeric()
        
        
        for (j in 1:nhours){
          
          hoursub <- fish.id[fish.id$EchoTime >starttime & fish.id$EchoTime <starttime+hours(1),]   
          
          if (nrow(hoursub) > 1){
            
            x.grid <- floor((hoursub$PosX - xmin14) / boxsize) + 1
            y.grid <- floor((hoursub$PosY - ymin14) / boxsize) + 1
            x.grid.max <- floor((xmax14 - xmin14) / boxsize) + 1
            y.grid.max <- floor((ymax14 - ymin14) / boxsize) + 1
            t.x <- sort(unique(x.grid))
            t.y <- sort(unique(y.grid))
            tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
            ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
            t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
            grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
            t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
            t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
            eg <- expand.grid(t.y,t.x)
            grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
            
            proportion.P14 <- c(proportion.P14, length(which(grid.cov > 0))/length(grid.cov))
            
          } else {
            
            proportion.P14 <- c(proportion.P14, 0)  
            
          }
          
          starttime <- starttime+hours(1)
          
        }
        
        proportion.P14[proportion.P14 == 0] <- NA
        #coverage.P14[,as.character(i)] <- mean(proportion, na.rm = T)
        coverage.P14[,as.character(i)] <- c(mean(proportion.P14, na.rm = T), sd(proportion.P14, na.rm = T))
        
        proportion.P14 <- as.data.frame(proportion.P14)
        proportion.P14$pen <- 14
        names(proportion.P14) <- c('proportion', 'pen')
        
        
        else{if(unique(dayfile$PEN) == '15'){
          
          fish.id <- subset(dayfile, PEN == '15')
          
          fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
          starttime <- fish.id[1,'EchoTime']-seconds(1)
          nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
          fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
          
          proportion.P15 <- numeric()
          
          for (j in 1:nhours){
            
            hoursub <- fish.id[fish.id$EchoTime >starttime & fish.id$EchoTime <starttime+hours(1),]   
            
            if (nrow(hoursub) > 1){
              
              x.grid <- floor((hoursub$PosX - xmin15) / boxsize) + 1
              y.grid <- floor((hoursub$PosY - ymin15) / boxsize) + 1
              x.grid.max <- floor((xmax15 - xmin15) / boxsize) + 1
              y.grid.max <- floor((ymax15 - ymin15) / boxsize) + 1
              t.x <- sort(unique(x.grid))
              t.y <- sort(unique(y.grid))
              tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
              ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
              t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
              grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
              t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
              t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
              eg <- expand.grid(t.y,t.x)
              grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
              
              proportion.P15 <- c(proportion.P15, length(which(grid.cov > 0))/length(grid.cov))
              
            } else {
              
              proportion.P15 <- c(proportion.P15, 0)  
              
            }
            
            
            starttime <- starttime+hours(1)
            
          }
          
          proportion.P15[proportion.P15 == 0] <- NA
          #coverage.P15[,as.character(i)] <- mean(proportion, na.rm = T)
          coverage.P15[,as.character(i)] <- c(mean(proportion.P15, na.rm = T), sd(proportion.P15, na.rm = T))
          
          proportion.P15 <- as.data.frame(proportion.P15)
          proportion.P15$pen <- 15
          names(proportion.P15) <- c('proportion', 'pen')
          
        }
        }
        
        prop.perhr <- rbind(proportion.P12, proportion.P14, proportion.P15)
        cov.anova <- aov(proportion~pen, data = prop.perhr)
        anova.sum <- unlist(summary(cov.anova))
        anova.list[,as.character(i)] <- anova.sum[9]
        
        
      }  
    }  
  }  
  
  coverage <- rbind(coverage.P12, coverage.P14, coverage.P15, anova.list)
  print(coverage)
  
  write.xlsx(coverage, 'CoverageOutput_hmean.xlsx')
}



# 11a. draws a plot of fish depth for the fish id specified

fish.depth <- function(period)
{
  fish.id <- subset(dayfile, Period == period)
  plot(fish.id$EchoTime, fish.id$PosZ, xlab = 'Time', ylab = 'Depth (m)', ylim = c(35, 0), type = 'l', col = '#26b426')
  segments(fish.id[1,4], 15, fish.id[nrow(fish.id), 4], 15, lty = 2)
  legend('bottomleft', as.character(period), col = '#26b426', pch = 20, bty = 'n', pt.cex = 1.5, horiz = TRUE, y.intersp = 0)
  
}



# 11b. draws a plot of fish activity for the fish id specified

fish.act <- function(period)
{
  fish.id <- subset(dayfile, Period == period)
  plot(fish.id$EchoTime, fish.id$BLSEC, xlab = 'Time', ylab = 'Activity (BL/SEC)', ylim = c(0, 5), type = 'l', col = '#26b426')
  legend('bottomleft', as.character(period), col = '#26b426', pch = 20, bty = 'n', pt.cex = 1.5, horiz = TRUE, y.intersp = 0)
  
}

# 12. draws a plot of depths for three fish

fish.3depth <- function(period1, period2, period3)
{
  fish.id <- subset(dayfile, Period == period1)
  plot(fish.id$EchoTime, fish.id$PosZ, xlab = 'Time', ylab = 'Depth (m)', ylim = c(35,0), type = 'l', col = '#26b426')
  
  fish.id <- subset(dayfile, Period == period2)
  lines(fish.id$EchoTime, fish.id$PosZ, col = '#d80000')
  
  fish.id <- subset(dayfile, Period == period3)
  lines(fish.id$EchoTime, fish.id$PosZ, col = '#038ef0')
  segments(fish.id[1,4], 15, fish.id[nrow(fish.id), 4], 15, lty = 2)
  legend('bottom', as.character(c(period1, period2, period3)), col = c('#26b426', '#d80000', '#038ef0'), pch = 20, bty = 'n', pt.cex = 1.5, horiz = TRUE, y.intersp = 0)
}

# 13. draws a plot of fish location

fish.plot <- function(period)
{
  fishpal <- rainbow_hcl(20, c=100, l=63, start=-360, end=-32, alpha = 0.2)
  fish.id <- subset(dayfile, Period == period)
  par(mfrow=c(1,1))
  
  if(fish.id[1,3] == '12')
  {
    
    # plot(fish.id$PosX, fish.id$PosY, xlab = 'X', ylab = 'Y', pch = 20, cex = 0.8, xlim = c(0, 40), ylim = c(0, 45), type = 'p', col = rgb(0, 0.6, 0, 0.2)) # wider plot
    plot(fish.id$PosX, fish.id$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(35, 70), ylim = c(35, 70), type = 'l', col = fishpal[20]) # tight plot
    rect(locations.lookup['12EW', 'xmin'], locations.lookup['12EW', 'ymin'], locations.lookup['12EW', 'xmax'], locations.lookup['12EW', 'ymax'], lty = 2) # 12EW edge
    rect(locations.lookup['12ES', 'xmin'], locations.lookup['12ES', 'ymin'], locations.lookup['12ES', 'xmax'], locations.lookup['12ES', 'ymax'], lty = 2) # 12ES edge
    rect(locations.lookup['12EE', 'xmin'], locations.lookup['12EE', 'ymin'], locations.lookup['12EE', 'xmax'], locations.lookup['12EE', 'ymax'], lty = 2) # 12EE edge
    rect(locations.lookup['12EN', 'xmin'], locations.lookup['12EN', 'ymin'], locations.lookup['12EN', 'xmax'], locations.lookup['12EN', 'ymax'], lty = 2) # 12EN edge
    rect(locations.lookup['12HET', 'xmin'], locations.lookup['12HET', 'ymin'], locations.lookup['12HET', 'xmax'], locations.lookup['12HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HET
    rect(locations.lookup['12HEB', 'xmin'], locations.lookup['12HEB', 'ymin'], locations.lookup['12HEB', 'xmax'], locations.lookup['12HEB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HEB
    rect(locations.lookup['12HWT', 'xmin'], locations.lookup['12HWT', 'ymin'], locations.lookup['12HWT', 'xmax'], locations.lookup['12HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HWT
    rect(locations.lookup['12HWB', 'xmin'], locations.lookup['12HWB', 'ymin'], locations.lookup['12HWB', 'xmax'], locations.lookup['12HWB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HWB
    rect(locations.lookup['FS12', 'xmin'], locations.lookup['FS12', 'ymin'], locations.lookup['FS12', 'xmax'], locations.lookup['FS12', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 12FS
    rect(locations.lookup['12EW', 'xmin'], locations.lookup['12ES', 'ymin'], locations.lookup['12EE', 'xmax'], locations.lookup['12EN', 'ymax'], lwd = 2) # cage limits
    #legend(1, 10, as.character(period), col = '#26b426', pch = 20, bty = 'n', pt.cex = 1.5, horiz = TRUE)
    
  }else{if(fish.id[1,3] == '14'){
    
    #plot(fish.id$PosX, fish.id$PosY, xlab = 'X', ylab = 'Y', pch = 20, cex = 0.8, xlim = c(25, 70), ylim = c(0, 45), type = 'p', col = rgb(0, 0.6, 0, 0.2)) # wider plot
    plot(fish.id$PosX, fish.id$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(10, 45), ylim = c(35, 70), type = 'l', col = fishpal[20]) # tight plot
    rect(locations.lookup['14EW', 'xmin'], locations.lookup['14EW', 'ymin'], locations.lookup['14EW', 'xmax'], locations.lookup['14EW', 'ymax'], lty = 2) # 14EW edge
    rect(locations.lookup['14ES', 'xmin'], locations.lookup['14ES', 'ymin'], locations.lookup['14ES', 'xmax'], locations.lookup['14ES', 'ymax'], lty = 2) # 14ES edge
    rect(locations.lookup['14EE', 'xmin'], locations.lookup['14EE', 'ymin'], locations.lookup['14EE', 'xmax'], locations.lookup['14EE', 'ymax'], lty = 2) # 14EE edge
    rect(locations.lookup['14EN', 'xmin'], locations.lookup['14EN', 'ymin'], locations.lookup['14EN', 'xmax'], locations.lookup['14EN', 'ymax'], lty = 2) # 14EN edge
    rect(locations.lookup['14HET', 'xmin'], locations.lookup['14HET', 'ymin'], locations.lookup['14HET', 'xmax'], locations.lookup['14HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HET
    rect(locations.lookup['14HEB', 'xmin'], locations.lookup['14HEB', 'ymin'], locations.lookup['14HEB', 'xmax'], locations.lookup['14HEB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HEB
    rect(locations.lookup['14HWT', 'xmin'], locations.lookup['14HWT', 'ymin'], locations.lookup['14HWT', 'xmax'], locations.lookup['14HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HWT
    rect(locations.lookup['14HWB', 'xmin'], locations.lookup['14HWB', 'ymin'], locations.lookup['14HWB', 'xmax'], locations.lookup['14HWB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HWB
    rect(locations.lookup['FS14', 'xmin'], locations.lookup['FS14', 'ymin'], locations.lookup['FS14', 'xmax'], locations.lookup['FS14', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 14FS
    rect(locations.lookup['14EW', 'xmin'], locations.lookup['14ES', 'ymin'], locations.lookup['14EE', 'xmax'], locations.lookup['14EN', 'ymax'], lwd = 2) # cage limits
    #legend(25, 10, as.character(period), col = '#26b426', pch = 20, bty = 'n', pt.cex = 1.5, horiz = TRUE)
    
  }else{if(fish.id[1,3] == '15'){
    #plot(fish.id$PosX, fish.id$PosY, xlab = 'X', ylab = 'Y', pch = 20, cex = 0.8, xlim = c(25, 70), ylim = c(0, 45), type = 'p', col = rgb(0, 0.6, 0, 0.2)) # wider plot
    plot(fish.id$PosX, fish.id$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(10, 45), ylim = c(10, 45), type = 'l', col = fishpal[20]) # tight plot
    rect(locations.lookup['15EW', 'xmin'], locations.lookup['15EW', 'ymin'], locations.lookup['15EW', 'xmax'], locations.lookup['15EW', 'ymax'], lty = 2) # 15EW edge
    rect(locations.lookup['15ES', 'xmin'], locations.lookup['15ES', 'ymin'], locations.lookup['15ES', 'xmax'], locations.lookup['15ES', 'ymax'], lty = 2) # 15ES edge
    rect(locations.lookup['15EE', 'xmin'], locations.lookup['15EE', 'ymin'], locations.lookup['15EE', 'xmax'], locations.lookup['15EE', 'ymax'], lty = 2) # 15EE edge
    rect(locations.lookup['15EN', 'xmin'], locations.lookup['15EN', 'ymin'], locations.lookup['15EN', 'xmax'], locations.lookup['15EN', 'ymax'], lty = 2) # 15EN edge
    rect(locations.lookup['15HET', 'xmin'], locations.lookup['15HET', 'ymin'], locations.lookup['15HET', 'xmax'], locations.lookup['15HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HET
    rect(locations.lookup['15HEB', 'xmin'], locations.lookup['15HEB', 'ymin'], locations.lookup['15HEB', 'xmax'], locations.lookup['15HEB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HEB
    rect(locations.lookup['15HWT', 'xmin'], locations.lookup['15HWT', 'ymin'], locations.lookup['15HWT', 'xmax'], locations.lookup['15HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HWT
    rect(locations.lookup['15HWB', 'xmin'], locations.lookup['15HWB', 'ymin'], locations.lookup['15HWB', 'xmax'], locations.lookup['15HWB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HWB
    rect(locations.lookup['FS15', 'xmin'], locations.lookup['FS15', 'ymin'], locations.lookup['FS15', 'xmax'], locations.lookup['FS15', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 15FS
    rect(locations.lookup['15EW', 'xmin'], locations.lookup['15ES', 'ymin'], locations.lookup['15EE', 'xmax'], locations.lookup['15EN', 'ymax'], lwd = 2) # cage limits
    #legend(25, 10, as.character(period), col = '#26b426', pch = 20, bty = 'n', pt.cex = 1.5, horiz = TRUE)
  } 
  }
  }
}

# 14. Draws a plot of fish locations for 3 fish

fish.3plot <- function(period1, period2, period3)
{
  fish.id <- subset(dayfile, Period == period1)
  if(fish.id[1,3] == '12')
  {
    
    plot(fish.id$PosX, fish.id$PosY, xlab = 'X', ylab = 'Y', pch = 20, xlim = c(35, 70), ylim = c(35, 70), type = 'l', col = '#26b426')
    fish.id <- subset(dayfile, Period == period2)
    lines(fish.id$PosX, fish.id$PosY, pch = 20, col = '#d80000')
    fish.id <- subset(dayfile, Period == period3)
    lines(fish.id$PosX, fish.id$PosY, pch = 20, col = '#038ef0')
    rect(locations.lookup['12EW', 'xmin'], locations.lookup['12EW', 'ymin'], locations.lookup['12EW', 'xmax'], locations.lookup['12EW', 'ymax'], lty = 2) # 12EW edge
    rect(locations.lookup['12ES', 'xmin'], locations.lookup['12ES', 'ymin'], locations.lookup['12ES', 'xmax'], locations.lookup['12ES', 'ymax'], lty = 2) # 12ES edge
    rect(locations.lookup['12EE', 'xmin'], locations.lookup['12EE', 'ymin'], locations.lookup['12EE', 'xmax'], locations.lookup['12EE', 'ymax'], lty = 2) # 12EE edge
    rect(locations.lookup['12EN', 'xmin'], locations.lookup['12EN', 'ymin'], locations.lookup['12EN', 'xmax'], locations.lookup['12EN', 'ymax'], lty = 2) # 12EN edge
    rect(locations.lookup['12HET', 'xmin'], locations.lookup['12HET', 'ymin'], locations.lookup['12HET', 'xmax'], locations.lookup['12HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HET
    rect(locations.lookup['12HEB', 'xmin'], locations.lookup['12HEB', 'ymin'], locations.lookup['12HEB', 'xmax'], locations.lookup['12HEB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HEB
    rect(locations.lookup['12HWT', 'xmin'], locations.lookup['12HWT', 'ymin'], locations.lookup['12HWT', 'xmax'], locations.lookup['12HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HWT
    rect(locations.lookup['12HWB', 'xmin'], locations.lookup['12HWB', 'ymin'], locations.lookup['12HWB', 'xmax'], locations.lookup['12HWB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HWB
    rect(locations.lookup['FS12', 'xmin'], locations.lookup['FS12', 'ymin'], locations.lookup['FS12', 'xmax'], locations.lookup['FS12', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 12FS
    rect(locations.lookup['12EW', 'xmin'], locations.lookup['12ES', 'ymin'], locations.lookup['12EE', 'xmax'], locations.lookup['12EN', 'ymax'], lwd = 2) # cage limits
    legend(1, 10, as.character(c(period1, period2, period3)), col = c('#26b426', '#d80000', '#038ef0'), pch = 20, bty = 'n', pt.cex = 1.5, horiz = TRUE)
    
  }else{if(fish.id[1,3] == '14'){
    
    plot(fish.id$PosX, fish.id$PosY, xlab = 'X', ylab = 'Y', pch = 20, xlim = c(10, 45), ylim = c(35, 70), type = 'l', col = '#26b426')
    fish.id <- subset(dayfile, Period == period2)
    lines(fish.id$PosX, fish.id$PosY, pch = 20, col = '#d80000')
    fish.id <- subset(dayfile, Period == period3)
    lines(fish.id$PosX, fish.id$PosY, pch = 20, col = '#038ef0')
    rect(locations.lookup['14EW', 'xmin'], locations.lookup['14EW', 'ymin'], locations.lookup['14EW', 'xmax'], locations.lookup['14EW', 'ymax'], lty = 2) # 14EW edge
    rect(locations.lookup['14ES', 'xmin'], locations.lookup['14ES', 'ymin'], locations.lookup['14ES', 'xmax'], locations.lookup['14ES', 'ymax'], lty = 2) # 14ES edge
    rect(locations.lookup['14EE', 'xmin'], locations.lookup['14EE', 'ymin'], locations.lookup['14EE', 'xmax'], locations.lookup['14EE', 'ymax'], lty = 2) # 14EE edge
    rect(locations.lookup['14EN', 'xmin'], locations.lookup['14EN', 'ymin'], locations.lookup['14EN', 'xmax'], locations.lookup['14EN', 'ymax'], lty = 2) # 14EN edge
    rect(locations.lookup['14HET', 'xmin'], locations.lookup['14HET', 'ymin'], locations.lookup['14HET', 'xmax'], locations.lookup['14HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HET
    rect(locations.lookup['14HEB', 'xmin'], locations.lookup['14HEB', 'ymin'], locations.lookup['14HEB', 'xmax'], locations.lookup['14HEB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HEB
    rect(locations.lookup['14HWT', 'xmin'], locations.lookup['14HWT', 'ymin'], locations.lookup['14HWT', 'xmax'], locations.lookup['14HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HWT
    rect(locations.lookup['14HWB', 'xmin'], locations.lookup['14HWB', 'ymin'], locations.lookup['14HWB', 'xmax'], locations.lookup['14HWB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HWB
    rect(locations.lookup['FS14', 'xmin'], locations.lookup['FS14', 'ymin'], locations.lookup['FS14', 'xmax'], locations.lookup['FS14', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 14FS
    rect(locations.lookup['14EW', 'xmin'], locations.lookup['14ES', 'ymin'], locations.lookup['14EE', 'xmax'], locations.lookup['14EN', 'ymax'], lwd = 2) # cage limits
    legend(25, 10, as.character(c(period1, period2, period3)), col = c('#26b426', '#d80000', '#038ef0'), pch = 20, bty = 'n', pt.cex = 1.5, horiz = TRUE)
    
  }else{if(fish.id[1,3] == '15'){
    
    plot(fish.id$PosX, fish.id$PosY, xlab = 'X', ylab = 'Y', pch = 20, xlim = c(10, 45), ylim = c(10, 45), type = 'l', col = '#26b426')
    fish.id <- subset(dayfile, Period == period2)
    lines(fish.id$PosX, fish.id$PosY, pch = 20, col = '#d80000')
    fish.id <- subset(dayfile, Period == period3)
    lines(fish.id$PosX, fish.id$PosY, pch = 20, col = '#038ef0')
    rect(locations.lookup['15EW', 'xmin'], locations.lookup['15EW', 'ymin'], locations.lookup['15EW', 'xmax'], locations.lookup['15EW', 'ymax'], lty = 2) # 15EW edge
    rect(locations.lookup['15ES', 'xmin'], locations.lookup['15ES', 'ymin'], locations.lookup['15ES', 'xmax'], locations.lookup['15ES', 'ymax'], lty = 2) # 15ES edge
    rect(locations.lookup['15EE', 'xmin'], locations.lookup['15EE', 'ymin'], locations.lookup['15EE', 'xmax'], locations.lookup['15EE', 'ymax'], lty = 2) # 15EE edge
    rect(locations.lookup['15EN', 'xmin'], locations.lookup['15EN', 'ymin'], locations.lookup['15EN', 'xmax'], locations.lookup['15EN', 'ymax'], lty = 2) # 15EN edge
    rect(locations.lookup['15HET', 'xmin'], locations.lookup['15HET', 'ymin'], locations.lookup['15HET', 'xmax'], locations.lookup['15HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HET
    rect(locations.lookup['15HEB', 'xmin'], locations.lookup['15HEB', 'ymin'], locations.lookup['15HEB', 'xmax'], locations.lookup['15HEB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HEB
    rect(locations.lookup['15HWT', 'xmin'], locations.lookup['15HWT', 'ymin'], locations.lookup['15HWT', 'xmax'], locations.lookup['15HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HWT
    rect(locations.lookup['15HWB', 'xmin'], locations.lookup['15HWB', 'ymin'], locations.lookup['15HWB', 'xmax'], locations.lookup['15HWB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HWB
    rect(locations.lookup['FS15', 'xmin'], locations.lookup['FS15', 'ymin'], locations.lookup['FS15', 'xmax'], locations.lookup['FS15', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 15FS
    rect(locations.lookup['15EW', 'xmin'], locations.lookup['15ES', 'ymin'], locations.lookup['15EE', 'xmax'], locations.lookup['15EN', 'ymax'], lwd = 2) # cage limits
    legend(25, 10, as.character(c(period1, period2, period3)), col = c('#26b426', '#d80000', '#038ef0'), pch = 20, bty = 'n', pt.cex = 1.5, horiz = TRUE)
  } 
  }
  }
}


# 15. Add a fish to the current plot

add.fish <- function(period, fishcol)
{
  fish.id <- subset(dayfile, Period == period)
  points(fish.id$PosX, fish.id$PosY, pch = 20, cex = 1, col = fishcol)
}

#16a. draws a plot of fish location density for the fish id specified 

fish.hexplot <- function(period)
  
{
  
  pen.col <- 'black'
  pen.size <- 1.4
  #plot.col <- rev(heat.colors(2, alpha = 1))
  plot.col <- matlab.like(1000)  
  
  fish.id <- subset(dayfile, Period == period)
  
  #pingmax <- as.integer((as.double(max(dayfile$EchoTime))-as.double(min(dayfile$EchoTime)))/500)
  pingmax <- 100
  
  if(fish.id[1, 'PEN'] == 12){  
    
    ggplot(fish.id, aes(fish.id$PosX, fish.id$PosY)) +
      geom_hex(bins = 55, alpha = 0.6) + scale_fill_gradientn(colours=plot.col, space = 'Lab', limits = c(0, pingmax), na.value = plot.col[length(plot.col)], name = 'No. pings') +
      annotate('segment', x = locations.lookup['12CSW', 'xmin'], xend = locations.lookup['12CSE', 'xmax'], y = locations.lookup['12CSW', 'ymin'], yend = locations.lookup['12CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['12CSW', 'xmin'], xend = locations.lookup['12CNW', 'xmin'], y = locations.lookup['12CSW', 'ymin'], yend = locations.lookup['12CNW', 'ymax'], colour = pen.col, size = pen.size) +  # pen boundary
      annotate('segment', x = locations.lookup['12CNW', 'xmin'], xend = locations.lookup['12CNE', 'xmax'], y = locations.lookup['12CNW', 'ymax'], yend = locations.lookup['12CNE', 'ymax'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['12CNE', 'xmax'], xend = locations.lookup['12CSE', 'xmax'], y = locations.lookup['12CNE', 'ymax'], yend = locations.lookup['12CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['12CSW', 'xmin'], xend = locations.lookup['12CSE', 'xmax'], y = locations.lookup['12CSW', 'ymax'], yend = locations.lookup['12CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['12CSW', 'xmax'], xend = locations.lookup['12CNW', 'xmax'], y = locations.lookup['12CSW', 'ymin'], yend = locations.lookup['12CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['12CNW', 'xmin'], xend = locations.lookup['12CNE', 'xmax'], y = locations.lookup['12CNW', 'ymin'], yend = locations.lookup['12CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['12CNE', 'xmin'], xend = locations.lookup['12CSE', 'xmin'], y = locations.lookup['12CNE', 'ymax'], yend = locations.lookup['12CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('rect', xmin = locations.lookup['12HET', 'xmin'], xmax = locations.lookup['12HET', 'xmax'], ymin = locations.lookup['12HET', 'ymin'], ymax = locations.lookup['12HET', 'ymax'], colour = pen.col, linetype = 'longdash', alpha = 0.4, fill = 'gray65') + # hide boundary
      annotate('rect', xmin = locations.lookup['12HEB', 'xmin'], xmax = locations.lookup['12HEB', 'xmax'], ymin = locations.lookup['12HEB', 'ymin'], ymax = locations.lookup['12HEB', 'ymax'], colour = pen.col, linetype = 'longdash', alpha = 0.4, fill = 'gray65') + # hide boundary
      annotate('rect', xmin = locations.lookup['12HWT', 'xmin'], xmax = locations.lookup['12HWT', 'xmax'], ymin = locations.lookup['12HWT', 'ymin'], ymax = locations.lookup['12HWT', 'ymax'], colour = pen.col, linetype = 'longdash', alpha = 0.4, fill = 'gray65') + # hide boundary
      annotate('rect', xmin = locations.lookup['12HWB', 'xmin'], xmax = locations.lookup['12HWB', 'xmax'], ymin = locations.lookup['12HWB', 'ymin'], ymax = locations.lookup['12HWB', 'ymax'], colour = pen.col, linetype = 'longdash', alpha = 0.4, fill = 'gray65') + # hide boundary
      annotate('rect', xmin = locations.lookup['FS12', 'xmin'], xmax = locations.lookup['FS12', 'xmax'], ymin = locations.lookup['FS12', 'ymin'], ymax = locations.lookup['FS12', 'ymax'], colour = pen.col, linetype = 'dotted', size = 1, alpha = 0.4, fill = 'gray50') + # FS Rectangle
      #annotate('segment', x = locations.lookup['FS12', 'xmin']+1, xend = locations.lookup['FS12', 'xmax']-1, y = locations.lookup['FS12', 'ymin']+1, yend = locations.lookup['FS12', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # FS Curve
      #annotate('segment', x = locations.lookup['FS12', 'xmin'], xend = locations.lookup['FS12', 'xmax'], y = locations.lookup['FS12', 'ymin'], yend = locations.lookup['FS12', 'ymax'], colour = pen.col, linetype = 'dashed', size = pen.size) + # Feed Station
      theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
      scale_x_continuous('x (m)', limits = c(35, 70)) + scale_y_continuous('y (m)', limits = c(35,70))
    
  } else {
    
    
    if(fish.id[1, 'PEN'] == 14) {
      
      ggplot(fish.id, aes(fish.id$PosX, fish.id$PosY)) +
        geom_hex(bins = 55, alpha = 0.6) + scale_fill_gradientn(colours=plot.col, space = 'Lab', limits = c(0, pingmax), na.value = plot.col[length(plot.col)], name = 'No. pings') +
        annotate('segment', x = locations.lookup['14CSW', 'xmin'], xend = locations.lookup['14CSE', 'xmax'], y = locations.lookup['14CSW', 'ymin'], yend = locations.lookup['14CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
        annotate('segment', x = locations.lookup['14CSW', 'xmin'], xend = locations.lookup['14CNW', 'xmin'], y = locations.lookup['14CSW', 'ymin'], yend = locations.lookup['14CNW', 'ymax'], colour = pen.col, size = pen.size) +  # pen boundary
        annotate('segment', x = locations.lookup['14CNW', 'xmin'], xend = locations.lookup['14CNE', 'xmax'], y = locations.lookup['14CNW', 'ymax'], yend = locations.lookup['14CNE', 'ymax'], colour = pen.col, size = pen.size) + # pen boundary
        annotate('segment', x = locations.lookup['14CNE', 'xmax'], xend = locations.lookup['14CSE', 'xmax'], y = locations.lookup['14CNE', 'ymax'], yend = locations.lookup['14CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
        annotate('segment', x = locations.lookup['14CSW', 'xmin'], xend = locations.lookup['14CSE', 'xmax'], y = locations.lookup['14CSW', 'ymax'], yend = locations.lookup['14CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
        annotate('segment', x = locations.lookup['14CSW', 'xmax'], xend = locations.lookup['14CNW', 'xmax'], y = locations.lookup['14CSW', 'ymin'], yend = locations.lookup['14CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
        annotate('segment', x = locations.lookup['14CNW', 'xmin'], xend = locations.lookup['14CNE', 'xmax'], y = locations.lookup['14CNW', 'ymin'], yend = locations.lookup['14CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
        annotate('segment', x = locations.lookup['14CNE', 'xmin'], xend = locations.lookup['14CSE', 'xmin'], y = locations.lookup['14CNE', 'ymax'], yend = locations.lookup['14CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
        annotate('rect', xmin = locations.lookup['14HET', 'xmin'], xmax = locations.lookup['14HET', 'xmax'], ymin = locations.lookup['14HET', 'ymin'], ymax = locations.lookup['14HET', 'ymax'], colour = pen.col, linetype = 'longdash', alpha = 0.4, fill = 'gray65') + # hide boundary
        annotate('rect', xmin = locations.lookup['14HEB', 'xmin'], xmax = locations.lookup['14HEB', 'xmax'], ymin = locations.lookup['14HEB', 'ymin'], ymax = locations.lookup['14HEB', 'ymax'], colour = pen.col, linetype = 'longdash', alpha = 0.4, fill = 'gray65') + # hide boundary
        annotate('rect', xmin = locations.lookup['14HWT', 'xmin'], xmax = locations.lookup['14HWT', 'xmax'], ymin = locations.lookup['14HWT', 'ymin'], ymax = locations.lookup['14HWT', 'ymax'], colour = pen.col, linetype = 'longdash', alpha = 0.4, fill = 'gray65') + # hide boundary
        annotate('rect', xmin = locations.lookup['14HWB', 'xmin'], xmax = locations.lookup['14HWB', 'xmax'], ymin = locations.lookup['14HWB', 'ymin'], ymax = locations.lookup['14HWB', 'ymax'], colour = pen.col, linetype = 'longdash', alpha = 0.4, fill = 'gray65') + # hide boundary
        annotate('rect', xmin = locations.lookup['FS14', 'xmin'], xmax = locations.lookup['FS14', 'xmax'], ymin = locations.lookup['FS14', 'ymin'], ymax = locations.lookup['FS14', 'ymax'], colour = pen.col, linetype = 'dotted', size = 1, alpha = 0.4, fill = 'gray50') + # FS Rectangle
        #annotate('segment', x = locations.lookup['14HET', 'xmin'], xend = locations.lookup['14HET', 'xmax'], y = locations.lookup['14HET', 'ymin'], yend = locations.lookup['14HET', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
        #annotate('segment', x = locations.lookup['14HEB', 'xmin'], xend = locations.lookup['14HEB', 'xmax'], y = locations.lookup['14HEB', 'ymin'], yend = locations.lookup['14HEB', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
        #annotate('segment', x = locations.lookup['14HWT', 'xmin'], xend = locations.lookup['14HWT', 'xmax'], y = locations.lookup['14HWT', 'ymin'], yend = locations.lookup['14HWT', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
        #annotate('segment', x = locations.lookup['14HWB', 'xmin'], xend = locations.lookup['14HWB', 'xmax'], y = locations.lookup['14HWB', 'ymin'], yend = locations.lookup['14HWB', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
        #annotate('curve', x = locations.lookup['FS14', 'xmin']+1, xend = locations.lookup['FS14', 'xmax']-1, y = locations.lookup['FS14', 'ymin']+1, yend = locations.lookup['FS14', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # Round Feed station
        #annotate('segment', x = locations.lookup['FS14', 'xmin'], xend = locations.lookup['FS14', 'xmax'], y = locations.lookup['FS14', 'ymin'], yend = locations.lookup['FS14', 'ymax'], colour = pen.col, linetype = 'dashed', size = pen.size) + # Feed Station
        theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
        scale_x_continuous('x (m)', limits = c(10,45)) + scale_y_continuous('y (m)', limits = c(35,70))  
      
    } else {
      
      if(fish.id[1, 'PEN'] == 15) {
        
        ggplot(fish.id, aes(fish.id$PosX, fish.id$PosY)) +
          geom_hex(bins = 55, alpha = 0.6) + scale_fill_gradientn(colours=plot.col, space = 'Lab', limits = c(0, pingmax), na.value = plot.col[length(plot.col)], name = 'No. pings') +
          annotate('segment', x = locations.lookup['15CSW', 'xmin'], xend = locations.lookup['15CSE', 'xmax'], y = locations.lookup['15CSW', 'ymin'], yend = locations.lookup['15CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
          annotate('segment', x = locations.lookup['15CSW', 'xmin'], xend = locations.lookup['15CNW', 'xmin'], y = locations.lookup['15CSW', 'ymin'], yend = locations.lookup['15CNW', 'ymax'], colour = pen.col, size = pen.size) +  # pen boundary
          annotate('segment', x = locations.lookup['15CNW', 'xmin'], xend = locations.lookup['15CNE', 'xmax'], y = locations.lookup['15CNW', 'ymax'], yend = locations.lookup['15CNE', 'ymax'], colour = pen.col, size = pen.size) + # pen boundary
          annotate('segment', x = locations.lookup['15CNE', 'xmax'], xend = locations.lookup['15CSE', 'xmax'], y = locations.lookup['15CNE', 'ymax'], yend = locations.lookup['15CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
          annotate('segment', x = locations.lookup['15CSW', 'xmin'], xend = locations.lookup['15CSE', 'xmax'], y = locations.lookup['15CSW', 'ymax'], yend = locations.lookup['15CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
          annotate('segment', x = locations.lookup['15CSW', 'xmax'], xend = locations.lookup['15CNW', 'xmax'], y = locations.lookup['15CSW', 'ymin'], yend = locations.lookup['15CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
          annotate('segment', x = locations.lookup['15CNW', 'xmin'], xend = locations.lookup['15CNE', 'xmax'], y = locations.lookup['15CNW', 'ymin'], yend = locations.lookup['15CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
          annotate('segment', x = locations.lookup['15CNE', 'xmin'], xend = locations.lookup['15CSE', 'xmin'], y = locations.lookup['15CNE', 'ymax'], yend = locations.lookup['15CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
          annotate('rect', xmin = locations.lookup['15HET', 'xmin'], xmax = locations.lookup['15HET', 'xmax'], ymin = locations.lookup['15HET', 'ymin'], ymax = locations.lookup['15HET', 'ymax'], colour = pen.col, linetype = 'longdash', alpha = 0.4, fill = 'gray65') + # hide boundary
          annotate('rect', xmin = locations.lookup['15HEB', 'xmin'], xmax = locations.lookup['15HEB', 'xmax'], ymin = locations.lookup['15HEB', 'ymin'], ymax = locations.lookup['15HEB', 'ymax'], colour = pen.col, linetype = 'longdash', alpha = 0.4, fill = 'gray65') + # hide boundary
          annotate('rect', xmin = locations.lookup['15HWT', 'xmin'], xmax = locations.lookup['15HWT', 'xmax'], ymin = locations.lookup['15HWT', 'ymin'], ymax = locations.lookup['15HWT', 'ymax'], colour = pen.col, linetype = 'longdash', alpha = 0.4, fill = 'gray65') + # hide boundary
          annotate('rect', xmin = locations.lookup['15HWB', 'xmin'], xmax = locations.lookup['15HWB', 'xmax'], ymin = locations.lookup['15HWB', 'ymin'], ymax = locations.lookup['15HWB', 'ymax'], colour = pen.col, linetype = 'longdash', alpha = 0.4, fill = 'gray65') + # hide boundary
          annotate('rect', xmin = locations.lookup['FS15', 'xmin'], xmax = locations.lookup['FS15', 'xmax'], ymin = locations.lookup['FS15', 'ymin'], ymax = locations.lookup['FS15', 'ymax'], colour = pen.col, linetype = 'dotted', size = 1, alpha = 0.4, fill = 'gray50') + # FS Rectangle
          #annotate('segment', x = locations.lookup['15HET', 'xmin'], xend = locations.lookup['15HET', 'xmax'], y = locations.lookup['15HET', 'ymin'], yend = locations.lookup['15HET', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
          #annotate('segment', x = locations.lookup['15HEB', 'xmin'], xend = locations.lookup['15HEB', 'xmax'], y = locations.lookup['15HEB', 'ymin'], yend = locations.lookup['15HEB', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
          #annotate('segment', x = locations.lookup['15HWT', 'xmin'], xend = locations.lookup['15HWT', 'xmax'], y = locations.lookup['15HWT', 'ymin'], yend = locations.lookup['15HWT', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
          #annotate('segment', x = locations.lookup['15HWB', 'xmin'], xend = locations.lookup['15HWB', 'xmax'], y = locations.lookup['15HWB', 'ymin'], yend = locations.lookup['15HWB', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
          #annotate('curve', x = locations.lookup['FS15', 'xmin']+1, xend = locations.lookup['FS15', 'xmax']-1, y = locations.lookup['FS15', 'ymin']+1, yend = locations.lookup['FS15', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # Round feed station
          #annotate('segment', x = locations.lookup['FS15', 'xmin'], xend = locations.lookup['FS15', 'xmax'], y = locations.lookup['FS15', 'ymin'], yend = locations.lookup['FS15', 'ymax'], colour = pen.col, linetype = 'dashed', size = pen.size) + # Feed Station
          theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
          scale_x_continuous('x (m)', limits = c(10,45)) + scale_y_continuous('y (m)', limits = c(10,45))  
        
        
      }
    }
  }
}




#16b. draws a plot of fish location density for all fish in the specified pen (7 or 8)


hexplot.all <- function(pen)
{
  
  pen.col <- 'black'
  pen.size <- 1.4
  #plot.col <- rev(heat.colors(2, alpha = 1))
  plot.col <- matlab.like(1000)  
  
  if(pen == 12){  
    
    fish.id <- subset(dayfile, PEN == 12)  
    
    hexplot <- ggplot(fish.id, aes(fish.id$PosX, fish.id$PosY))
    hexplot <- hexplot + geom_hex(bins = 55, alpha = 0.6) + scale_fill_gradientn(colours=plot.col, space = 'Lab', limits = c(0, 1000), na.value = plot.col[length(plot.col)], name = 'No. pings')
    hexplot + annotate('segment', x = locations.lookup['12CSW', 'xmin'], xend = locations.lookup['12CSE', 'xmax'], y = locations.lookup['12CSW', 'ymin'], yend = locations.lookup['12CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['12CSW', 'xmin'], xend = locations.lookup['12CSE', 'xmax'], y = locations.lookup['12CSW', 'ymin'], yend = locations.lookup['12CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['12CSW', 'xmin'], xend = locations.lookup['12CNW', 'xmin'], y = locations.lookup['12CSW', 'ymin'], yend = locations.lookup['12CNW', 'ymax'], colour = pen.col, size = pen.size) +  # pen boundary
      annotate('segment', x = locations.lookup['12CNW', 'xmin'], xend = locations.lookup['12CNE', 'xmax'], y = locations.lookup['12CNW', 'ymax'], yend = locations.lookup['12CNE', 'ymax'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['12CNE', 'xmax'], xend = locations.lookup['12CSE', 'xmax'], y = locations.lookup['12CNE', 'ymax'], yend = locations.lookup['12CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['12CSW', 'xmin'], xend = locations.lookup['12CSE', 'xmax'], y = locations.lookup['12CSW', 'ymax'], yend = locations.lookup['12CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['12CSW', 'xmax'], xend = locations.lookup['12CNW', 'xmax'], y = locations.lookup['12CSW', 'ymin'], yend = locations.lookup['12CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['12CNW', 'xmin'], xend = locations.lookup['12CNE', 'xmax'], y = locations.lookup['12CNW', 'ymin'], yend = locations.lookup['12CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['12CNE', 'xmin'], xend = locations.lookup['12CSE', 'xmin'], y = locations.lookup['12CNE', 'ymax'], yend = locations.lookup['12CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['12HET', 'xmin'], xend = locations.lookup['12HET', 'xmax'], y = locations.lookup['12HET', 'ymin'], yend = locations.lookup['12HET', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
      annotate('segment', x = locations.lookup['12HEB', 'xmin'], xend = locations.lookup['12HEB', 'xmax'], y = locations.lookup['12HEB', 'ymin'], yend = locations.lookup['12HEB', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
      annotate('segment', x = locations.lookup['12HWT', 'xmin'], xend = locations.lookup['12HWT', 'xmax'], y = locations.lookup['12HWT', 'ymin'], yend = locations.lookup['12HWT', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
      annotate('segment', x = locations.lookup['12HWB', 'xmin'], xend = locations.lookup['12HWB', 'xmax'], y = locations.lookup['12HWB', 'ymin'], yend = locations.lookup['12HWB', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
      annotate('segment', x = locations.lookup['FS12', 'xmin'], xend = locations.lookup['FS12', 'xmax'], y = locations.lookup['FS12', 'ymin'], yend = locations.lookup['FS12', 'ymax'], colour = pen.col, linetype = 'dotdash', size = pen.size) + # Feed Station
      theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
      scale_x_continuous('x (m)', limits = c(10, 45)) + scale_y_continuous('y (m)', limits = c(10,45))
    
  } else {
    if(pen == 14) {
      
      fish.id <- subset(dayfile, PEN == 14)  
      
      hexplot <- ggplot(fish.id, aes(fish.id$PosX, fish.id$PosY))
      hexplot <- hexplot + geom_hex(bins = 55, alpha = 0.6) + scale_fill_gradientn(colours=plot.col, space = 'Lab', limits = c(0, 1000), na.value = plot.col[length(plot.col)], name = 'No. pings') 
      hexplot + annotate('segment', x = locations.lookup['14CSW', 'xmin'], xend = locations.lookup['14CSE', 'xmax'], y = locations.lookup['14CSW', 'ymin'], yend = locations.lookup['14CSE', 'ymin'], colour = pen.col, size = pen.size) +
        annotate('segment', x = locations.lookup['14CSW', 'xmin'], xend = locations.lookup['14CSE', 'xmax'], y = locations.lookup['14CSW', 'ymin'], yend = locations.lookup['14CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
        annotate('segment', x = locations.lookup['14CSW', 'xmin'], xend = locations.lookup['14CNW', 'xmin'], y = locations.lookup['14CSW', 'ymin'], yend = locations.lookup['14CNW', 'ymax'], colour = pen.col, size = pen.size) +  # pen boundary
        annotate('segment', x = locations.lookup['14CNW', 'xmin'], xend = locations.lookup['14CNE', 'xmax'], y = locations.lookup['14CNW', 'ymax'], yend = locations.lookup['14CNE', 'ymax'], colour = pen.col, size = pen.size) + # pen boundary
        annotate('segment', x = locations.lookup['14CNE', 'xmax'], xend = locations.lookup['14CSE', 'xmax'], y = locations.lookup['14CNE', 'ymax'], yend = locations.lookup['14CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
        annotate('segment', x = locations.lookup['14CSW', 'xmin'], xend = locations.lookup['14CSE', 'xmax'], y = locations.lookup['14CSW', 'ymax'], yend = locations.lookup['14CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
        annotate('segment', x = locations.lookup['14CSW', 'xmax'], xend = locations.lookup['14CNW', 'xmax'], y = locations.lookup['14CSW', 'ymin'], yend = locations.lookup['14CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
        annotate('segment', x = locations.lookup['14CNW', 'xmin'], xend = locations.lookup['14CNE', 'xmax'], y = locations.lookup['14CNW', 'ymin'], yend = locations.lookup['14CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
        annotate('segment', x = locations.lookup['14CNE', 'xmin'], xend = locations.lookup['14CSE', 'xmin'], y = locations.lookup['14CNE', 'ymax'], yend = locations.lookup['14CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
        annotate('segment', x = locations.lookup['14HET', 'xmin'], xend = locations.lookup['14HET', 'xmax'], y = locations.lookup['14HET', 'ymin'], yend = locations.lookup['14HET', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
        annotate('segment', x = locations.lookup['14HEB', 'xmin'], xend = locations.lookup['14HEB', 'xmax'], y = locations.lookup['14HEB', 'ymin'], yend = locations.lookup['14HEB', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
        annotate('segment', x = locations.lookup['14HWT', 'xmin'], xend = locations.lookup['14HWT', 'xmax'], y = locations.lookup['14HWT', 'ymin'], yend = locations.lookup['14HWT', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
        annotate('segment', x = locations.lookup['14HWB', 'xmin'], xend = locations.lookup['14HWB', 'xmax'], y = locations.lookup['14HWB', 'ymin'], yend = locations.lookup['14HWB', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
        annotate('segment', x = locations.lookup['FS14', 'xmin'], xend = locations.lookup['FS14', 'xmax'], y = locations.lookup['FS14', 'ymin'], yend = locations.lookup['FS14', 'ymax'], colour = pen.col, linetype = 'dotdash', size = pen.size) + # Feed Station
        theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
        scale_x_continuous('x (m)', limits = c(35,70)) + scale_y_continuous('y (m)', limits = c(10,45))
    } else{
      if(pen == 15) {
        
        fish.id <- subset(dayfile, PEN == 15)  
        
        hexplot <- ggplot(fish.id, aes(fish.id$PosX, fish.id$PosY))
        hexplot <- hexplot + geom_hex(bins = 55, alpha = 0.6) + scale_fill_gradientn(colours=plot.col, space = 'Lab', limits = c(0, 1000), na.value = plot.col[length(plot.col)], name = 'No. pings') 
        hexplot + annotate('segment', x = locations.lookup['15CSW', 'xmin'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, size = pen.size) +
          annotate('segment', x = locations.lookup['15CSW', 'xmin'], xend = locations.lookup['15CSE', 'xmax'], y = locations.lookup['15CSW', 'ymin'], yend = locations.lookup['15CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
          annotate('segment', x = locations.lookup['15CSW', 'xmin'], xend = locations.lookup['15CNW', 'xmin'], y = locations.lookup['15CSW', 'ymin'], yend = locations.lookup['15CNW', 'ymax'], colour = pen.col, size = pen.size) +  # pen boundary
          annotate('segment', x = locations.lookup['15CNW', 'xmin'], xend = locations.lookup['15CNE', 'xmax'], y = locations.lookup['15CNW', 'ymax'], yend = locations.lookup['15CNE', 'ymax'], colour = pen.col, size = pen.size) + # pen boundary
          annotate('segment', x = locations.lookup['15CNE', 'xmax'], xend = locations.lookup['15CSE', 'xmax'], y = locations.lookup['15CNE', 'ymax'], yend = locations.lookup['15CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
          annotate('segment', x = locations.lookup['15CSW', 'xmin'], xend = locations.lookup['15CSE', 'xmax'], y = locations.lookup['15CSW', 'ymax'], yend = locations.lookup['15CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
          annotate('segment', x = locations.lookup['15CSW', 'xmax'], xend = locations.lookup['15CNW', 'xmax'], y = locations.lookup['15CSW', 'ymin'], yend = locations.lookup['15CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
          annotate('segment', x = locations.lookup['15CNW', 'xmin'], xend = locations.lookup['15CNE', 'xmax'], y = locations.lookup['15CNW', 'ymin'], yend = locations.lookup['15CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
          annotate('segment', x = locations.lookup['15CNE', 'xmin'], xend = locations.lookup['15CSE', 'xmin'], y = locations.lookup['15CNE', 'ymax'], yend = locations.lookup['15CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
          annotate('segment', x = locations.lookup['15HET', 'xmin'], xend = locations.lookup['15HET', 'xmax'], y = locations.lookup['15HET', 'ymin'], yend = locations.lookup['15HET', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
          annotate('segment', x = locations.lookup['15HEB', 'xmin'], xend = locations.lookup['15HEB', 'xmax'], y = locations.lookup['15HEB', 'ymin'], yend = locations.lookup['15HEB', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
          annotate('segment', x = locations.lookup['15HWT', 'xmin'], xend = locations.lookup['15HWT', 'xmax'], y = locations.lookup['15HWT', 'ymin'], yend = locations.lookup['15HWT', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
          annotate('segment', x = locations.lookup['15HWB', 'xmin'], xend = locations.lookup['15HWB', 'xmax'], y = locations.lookup['15HWB', 'ymin'], yend = locations.lookup['15HWB', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
          annotate('segment', x = locations.lookup['FS15', 'xmin'], xend = locations.lookup['FS15', 'xmax'], y = locations.lookup['FS15', 'ymin'], yend = locations.lookup['FS15', 'ymax'], colour = pen.col, linetype = 'dotdash', size = pen.size) + # Feed Station
          theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
          scale_x_continuous('x (m)', limits = c(35,70)) + scale_y_continuous('y (m)', limits = c(10,45))
    }
    }
    }
  
}



# 17. draws a 3d plot of fish location and depth

fish.3dplot <- function(period)
{
  fish.id <- subset(dayfile, Period == period)
  scatterplot3d(fish.id$PosX, fish.id$PosY, fish.id$PosZ, pch = 20, xlim =  c(10, 45), ylim = c(10, 45), zlim = c(26, 0))
}


# 18. draws a 3d interactive plot of fish location and depth

fish.3dmove <- function(period)
{
  fish.id <- subset(dayfile, Period == period)
  plot3d(fish.id$PosX, fish.id$PosY, fish.id$PosZ, cex = 1, xlim =  c(10, 45), ylim = c(10, 45), zlim = c(0, 35), xlab = 'X', ylab = 'Y', zlab = 'Z', type = 'l', col = '#26b426', lwd = 2)
}



# 19a. draws a plot of fish location by depth

plot.bydepth <- function(period)
{
  depthpal <- diverge_hcl(30, h = c(11,266), c = 100, l = c(21,85), power = 0.6)
  fish.id <- subset(dayfile, Period == period)
  
  if(fish.id[1,3] == '12')
  {
    
    # plot(fish.id$PosX, fish.id$PosY, xlab = 'X', ylab = 'Y', pch = 20, cex = 0.8, xlim = c(0, 40), ylim = c(0, 45), type = 'p', col = rgb(0, 0.6, 0, 0.2)) # wider plot
    plot(fish.id$PosX, fish.id$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(35, 70), ylim = c(35, 70), type = 'p', col = depthpal[round(fish.id$PosZ)]) # tight plot
    rect(locations.lookup['12EW', 'xmin'], locations.lookup['12EW', 'ymin'], locations.lookup['12EW', 'xmax'], locations.lookup['12EW', 'ymax'], lty = 2) # 12EW edge
    rect(locations.lookup['12ES', 'xmin'], locations.lookup['12ES', 'ymin'], locations.lookup['12ES', 'xmax'], locations.lookup['12ES', 'ymax'], lty = 2) # 12ES edge
    rect(locations.lookup['12EE', 'xmin'], locations.lookup['12EE', 'ymin'], locations.lookup['12EE', 'xmax'], locations.lookup['12EE', 'ymax'], lty = 2) # 12EE edge
    rect(locations.lookup['12EN', 'xmin'], locations.lookup['12EN', 'ymin'], locations.lookup['12EN', 'xmax'], locations.lookup['12EN', 'ymax'], lty = 2) # 12EN edge
    rect(locations.lookup['12HET', 'xmin'], locations.lookup['12HET', 'ymin'], locations.lookup['12HET', 'xmax'], locations.lookup['12HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HET
    rect(locations.lookup['12HEB', 'xmin'], locations.lookup['12HEB', 'ymin'], locations.lookup['12HEB', 'xmax'], locations.lookup['12HEB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HEB
    rect(locations.lookup['12HWT', 'xmin'], locations.lookup['12HWT', 'ymin'], locations.lookup['12HWT', 'xmax'], locations.lookup['12HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HWT
    rect(locations.lookup['12HWB', 'xmin'], locations.lookup['12HWB', 'ymin'], locations.lookup['12HWB', 'xmax'], locations.lookup['12HWB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HWB
    rect(locations.lookup['FS12', 'xmin'], locations.lookup['FS12', 'ymin'], locations.lookup['FS12', 'xmax'], locations.lookup['FS12', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 12FS
    rect(locations.lookup['12EW', 'xmin'], locations.lookup['12ES', 'ymin'], locations.lookup['12EE', 'xmax'], locations.lookup['12EN', 'ymax'], lwd = 2) # cage limits
    legend(67, 67, as.character(1:30), col = depthpal, pch = 15, bty = 'n', cex = 1, pt.cex = 2.6, horiz = FALSE, y.intersp = 0.5, title = 'depth (m)', text.width = 0.2, yjust = 1)
    
    
  }else{
    if(fish.id[1,3] == '14'){
      
      #plot(fish.id$PosX, fish.id$PosY, xlab = 'X', ylab = 'Y', pch = 20, cex = 0.8, xlim = c(25, 70), ylim = c(0, 45), type = 'p', col = rgb(0, 0.6, 0, 0.2)) # wider plot
      plot(fish.id$PosX, fish.id$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(10, 45), ylim = c(35, 70), type = 'p', col = depthpal[round(fish.id$PosZ)]) # tight plot
      rect(locations.lookup['14EW', 'xmin'], locations.lookup['14EW', 'ymin'], locations.lookup['14EW', 'xmax'], locations.lookup['14EW', 'ymax'], lty = 2) # 14EW edge
      rect(locations.lookup['14ES', 'xmin'], locations.lookup['14ES', 'ymin'], locations.lookup['14ES', 'xmax'], locations.lookup['14ES', 'ymax'], lty = 2) # 14ES edge
      rect(locations.lookup['14EE', 'xmin'], locations.lookup['14EE', 'ymin'], locations.lookup['14EE', 'xmax'], locations.lookup['14EE', 'ymax'], lty = 2) # 14EE edge
      rect(locations.lookup['14EN', 'xmin'], locations.lookup['14EN', 'ymin'], locations.lookup['14EN', 'xmax'], locations.lookup['14EN', 'ymax'], lty = 2) # 14EN edge
      rect(locations.lookup['14HET', 'xmin'], locations.lookup['14HET', 'ymin'], locations.lookup['14HET', 'xmax'], locations.lookup['14HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HET
      rect(locations.lookup['14HEB', 'xmin'], locations.lookup['14HEB', 'ymin'], locations.lookup['14HEB', 'xmax'], locations.lookup['14HEB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HEB
      rect(locations.lookup['14HWT', 'xmin'], locations.lookup['14HWT', 'ymin'], locations.lookup['14HWT', 'xmax'], locations.lookup['14HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HWT
      rect(locations.lookup['14HWB', 'xmin'], locations.lookup['14HWB', 'ymin'], locations.lookup['14HWB', 'xmax'], locations.lookup['14HWB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HWB
      rect(locations.lookup['FS14', 'xmin'], locations.lookup['FS14', 'ymin'], locations.lookup['FS14', 'xmax'], locations.lookup['FS14', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 14FS
      rect(locations.lookup['14EW', 'xmin'], locations.lookup['14ES', 'ymin'], locations.lookup['14EE', 'xmax'], locations.lookup['14EN', 'ymax'], lwd = 2) # cage limits
      legend(42, 67, as.character(1:30), col = depthpal, pch = 15, bty = 'n', cex = 1, pt.cex = 2.6, horiz = FALSE, y.intersp = 0.5, title = 'depth (m)', text.width = 0.2)
      
    }else{
      if(fish.id[1,3] == '15'){
        
        plot(fish.id$PosX, fish.id$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(10, 45), ylim = c(10, 45), type = 'p', col = depthpal[round(fish.id$PosZ)]) # tight plot
        rect(locations.lookup['15EW', 'xmin'], locations.lookup['15EW', 'ymin'], locations.lookup['15EW', 'xmax'], locations.lookup['15EW', 'ymax'], lty = 2) # 15EW edge
        rect(locations.lookup['15ES', 'xmin'], locations.lookup['15ES', 'ymin'], locations.lookup['15ES', 'xmax'], locations.lookup['15ES', 'ymax'], lty = 2) # 15ES edge
        rect(locations.lookup['15EE', 'xmin'], locations.lookup['15EE', 'ymin'], locations.lookup['15EE', 'xmax'], locations.lookup['15EE', 'ymax'], lty = 2) # 15EE edge
        rect(locations.lookup['15EN', 'xmin'], locations.lookup['15EN', 'ymin'], locations.lookup['15EN', 'xmax'], locations.lookup['15EN', 'ymax'], lty = 2) # 15EN edge
        rect(locations.lookup['15HET', 'xmin'], locations.lookup['15HET', 'ymin'], locations.lookup['15HET', 'xmax'], locations.lookup['15HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HET
        rect(locations.lookup['15HEB', 'xmin'], locations.lookup['15HEB', 'ymin'], locations.lookup['15HEB', 'xmax'], locations.lookup['15HEB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HEB
        rect(locations.lookup['15HWT', 'xmin'], locations.lookup['15HWT', 'ymin'], locations.lookup['15HWT', 'xmax'], locations.lookup['15HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HWT
        rect(locations.lookup['15HWB', 'xmin'], locations.lookup['15HWB', 'ymin'], locations.lookup['15HWB', 'xmax'], locations.lookup['15HWB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HWB
        rect(locations.lookup['FS15', 'xmin'], locations.lookup['FS15', 'ymin'], locations.lookup['FS15', 'xmax'], locations.lookup['FS15', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 15FS
        rect(locations.lookup['15EW', 'xmin'], locations.lookup['15ES', 'ymin'], locations.lookup['15EE', 'xmax'], locations.lookup['15EN', 'ymax'], lwd = 2) # cage limits
        legend(42, 42, as.character(1:30), col = depthpal, pch = 15, bty = 'n', cex = 1, pt.cex = 2.6, horiz = FALSE, y.intersp = 0.5, title = 'depth (m)', text.width = 0.2)    
      }  
    } 
  }
}



# 19b. draws a plot of fish location by activity behaviour state (NOT for Laga Bay)

plot.byactivity <- function(period, static = 0.1, burst = 1)
{
  #activitypal <- heat_hcl(3, h = c(0,-100), c = c(40, 80), l = c(75,40), power = 1)
  activitypal <- brewer.pal(3, 'Set1')
  pen.col <- 'black'
  pen.size <- 0.8
  
  fish.id <- subset(dayfile, Period == period)
  fish.id$BS <- as.factor(ifelse(fish.id$BLSEC < 0.1, 'static', ifelse(fish.id$BLSEC >=0.1 & fish.id$BLSEC <1, 'cruise', 'burst')))
  fish.id$BS <- factor(fish.id$BS, levels = c('cruise', 'static', 'burst'))
  fish.id <- fish.id[order(fish.id$BS, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by behaviour state
  
  
  if (unique(fish.id$PEN == '7')) {
    
    fish.plot <- ggplot(fish.id, aes(PosX, PosY)) +
      scale_x_continuous('x (m)', limits = c(10, 45)) + scale_y_continuous('y (m)', limits = c(10,45)) + # set scale limits      
      theme(panel.background = element_rect(fill = 'white', colour = 'black')) + # white background, black lines
      geom_point(aes(colour = cut(BLSEC, c(-Inf, static, burst, Inf))), size = 3)  + scale_color_manual(name = 'activity (BL/sec)', values = c("(-Inf,0.1]" = activitypal[[3]], "(0.1,1]" = activitypal[[2]], "(1, Inf]" = activitypal[[1]]), labels = c('static (< 0.1)', 'cruise (0.1 - 1)', 'burst (>1)')) +
      annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CNW', 'xmin'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CNW', 'ymax'], colour = pen.col, size = pen.size) +  # pen boundary
      annotate('segment', x = locations.lookup['7CNW', 'xmin'], xend = locations.lookup['7CNE', 'xmax'], y = locations.lookup['7CNW', 'ymax'], yend = locations.lookup['7CNE', 'ymax'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['7CNE', 'xmax'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CNE', 'ymax'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CSW', 'ymax'], yend = locations.lookup['7CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['7CSW', 'xmax'], xend = locations.lookup['7CNW', 'xmax'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['7CNW', 'xmin'], xend = locations.lookup['7CNE', 'xmax'], y = locations.lookup['7CNW', 'ymin'], yend = locations.lookup['7CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['7CNE', 'xmin'], xend = locations.lookup['7CSE', 'xmin'], y = locations.lookup['7CNE', 'ymax'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('curve', x = locations.lookup['7WHNW', 'xmin']+1, xend = locations.lookup['7WHNW', 'xmax']-1, y = locations.lookup['7WHNW', 'ymin']+1, yend = locations.lookup['7WHNW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
      annotate('curve', x = locations.lookup['7WHNW', 'xmin']+1, xend = locations.lookup['7WHNW', 'xmax']-1, y = locations.lookup['7WHNW', 'ymin']+1, yend = locations.lookup['7WHNW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
      annotate('curve', x = locations.lookup['7WHSE', 'xmin']+1, xend = locations.lookup['7WHSE', 'xmax']-1, y = locations.lookup['7WHSE', 'ymin']+1, yend = locations.lookup['7WHSE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
      annotate('curve', x = locations.lookup['7WHSE', 'xmin']+1, xend = locations.lookup['7WHSE', 'xmax']-1, y = locations.lookup['7WHSE', 'ymin']+1, yend = locations.lookup['7WHSE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) # hide boundary
    fish.plot
    
  } else {
    
    fish.plot <- ggplot(fish.id, aes(PosX, PosY)) +
      scale_x_continuous('x (m)', limits = c(30,65)) + scale_y_continuous('y (m)', limits = c(8,43)) + 
      theme(panel.background = element_rect(fill = 'white', colour = 'black')) + # white background, black lines
      geom_point(aes(colour = cut(BLSEC, c(-Inf, static, burst, Inf))), size = 3)  + scale_color_manual(name = 'activity (BL/sec)', values = c("(-Inf,0.1]" = activitypal[[3]], "(0.1,1]" = activitypal[[2]], "(1, Inf]" = activitypal[[1]]), labels = c('static (< 0.1)', 'cruise (0.1 - 1)', 'burst (>1)')) +
      annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CNW', 'xmin'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CNW', 'ymax'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['8CNW', 'xmin'], xend = locations.lookup['8CNE', 'xmax'], y = locations.lookup['8CNW', 'ymax'], yend = locations.lookup['8CNE', 'ymax'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['8CNE', 'xmax'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CNE', 'ymax'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CSW', 'ymax'], yend = locations.lookup['8CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('segment', x = locations.lookup['8CSW', 'xmax'], xend = locations.lookup['8CNW', 'xmax'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('segment', x = locations.lookup['8CNW', 'xmin'], xend = locations.lookup['8CNE', 'xmax'], y = locations.lookup['8CNW', 'ymin'], yend = locations.lookup['8CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('segment', x = locations.lookup['8CNE', 'xmin'], xend = locations.lookup['8CSE', 'xmin'], y = locations.lookup['8CNE', 'ymax'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('curve', x = locations.lookup['8WHSW', 'xmin']+1, xend = locations.lookup['8WHSW', 'xmax']-1, y = locations.lookup['8WHSW', 'ymin']+1, yend = locations.lookup['8WHSW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
      annotate('curve', x = locations.lookup['8WHSW', 'xmin']+1, xend = locations.lookup['8WHSW', 'xmax']-1, y = locations.lookup['8WHSW', 'ymin']+1, yend = locations.lookup['8WHSW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
      annotate('curve', x = locations.lookup['8WHNE', 'xmin']+1, xend = locations.lookup['8WHNE', 'xmax']-1, y = locations.lookup['8WHNE', 'ymin']+1, yend = locations.lookup['8WHNE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
      annotate('curve', x = locations.lookup['8WHNE', 'xmin']+1, xend = locations.lookup['8WHNE', 'xmax']-1, y = locations.lookup['8WHNE', 'ymin']+1, yend = locations.lookup['8WHNE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1)# + # hide boundary
    fish.plot
    
  }
  
}



# 19c. draws a plot of fish location by time of day

plot.bylight <- function(period)
{
  
  lightpal <- brewer.pal(11, 'Spectral')
  lightpal <- c(lightpal[[4]], lightpal[[5]], lightpal[[3]], lightpal[[11]])
  pen.col <- 'black'
  pen.size <- 0.8
  
  fish.id <- subset(dayfile, Period == period)
  fish.id <- subset(fish.id, SUN == 'N' | SUN == 'W' | SUN == 'D' | SUN == 'K')
  #fish.id$BS <- as.factor(ifelse(fish.id$BLSEC < 0.1, 'static', ifelse(fish.id$BLSEC >=0.1 & fish.id$BLSEC <1, 'cruise', 'burst')))
  fish.id$SUN <- factor(fish.id$SUN, levels = c('D', 'W', 'K', 'N'))
  fish.id <- fish.id[order(fish.id$SUN, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by behaviour state
  fish.id$SUN <- factor(fish.id$SUN, levels = c('W', 'D', 'K', 'N'))
  
  if (unique(fish.id$PEN == '12')) {
    
    fish.plot <- ggplot(fish.id, aes(PosX, PosY)) +
      scale_x_continuous('x (m)', limits = c(35, 70)) + scale_y_continuous('y (m)', limits = c(35, 70)) + # set scale limits      
      theme(panel.background = element_rect(fill = 'white', colour = 'black')) + # white background, black lines
      geom_point(aes(colour = SUN), size = 3)  + scale_color_manual(name = 'Time of day', values = lightpal, labels = c('Dawn', 'Day', 'Dusk', 'Night')) +
      annotate('segment', x = locations.lookup['12CSW', 'xmin'], xend = locations.lookup['12CSE', 'xmax'], y = locations.lookup['12CSW', 'ymin'], yend = locations.lookup['12CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['12CSW', 'xmin'], xend = locations.lookup['12CNW', 'xmin'], y = locations.lookup['12CSW', 'ymin'], yend = locations.lookup['12CNW', 'ymax'], colour = pen.col, size = pen.size) +  # pen boundary
      annotate('segment', x = locations.lookup['12CNW', 'xmin'], xend = locations.lookup['12CNE', 'xmax'], y = locations.lookup['12CNW', 'ymax'], yend = locations.lookup['12CNE', 'ymax'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['12CNE', 'xmax'], xend = locations.lookup['12CSE', 'xmax'], y = locations.lookup['12CNE', 'ymax'], yend = locations.lookup['12CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['12CSW', 'xmin'], xend = locations.lookup['12CSE', 'xmax'], y = locations.lookup['12CSW', 'ymax'], yend = locations.lookup['12CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['12CSW', 'xmax'], xend = locations.lookup['12CNW', 'xmax'], y = locations.lookup['12CSW', 'ymin'], yend = locations.lookup['12CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['12CNW', 'xmin'], xend = locations.lookup['12CNE', 'xmax'], y = locations.lookup['12CNW', 'ymin'], yend = locations.lookup['12CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['12CNE', 'xmin'], xend = locations.lookup['12CSE', 'xmin'], y = locations.lookup['12CNE', 'ymax'], yend = locations.lookup['12CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['12HET', 'xmin'], xend = locations.lookup['12HET', 'xmax'], y = locations.lookup['12HET', 'ymin'], yend = locations.lookup['12HET', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
      annotate('segment', x = locations.lookup['12HEB', 'xmin'], xend = locations.lookup['12HEB', 'xmax'], y = locations.lookup['12HEB', 'ymin'], yend = locations.lookup['12HEB', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
      annotate('segment', x = locations.lookup['12HWT', 'xmin'], xend = locations.lookup['12HWT', 'xmax'], y = locations.lookup['12HWT', 'ymin'], yend = locations.lookup['12HWT', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
      annotate('segment', x = locations.lookup['12HWB', 'xmin'], xend = locations.lookup['12HWB', 'xmax'], y = locations.lookup['12HWB', 'ymin'], yend = locations.lookup['12HWB', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
      annotate('segment', x = locations.lookup['FS12', 'xmin'], xend = locations.lookup['FS12', 'xmax'], y = locations.lookup['FS12', 'ymin'], yend = locations.lookup['FS12', 'ymax'], colour = pen.col, linetype = 'dotdash', size = pen.size) # + # Feed Station
    fish.plot
    
  } else {
    
    if (unique(fish.id$PEN == '14')) {
      
      fish.plot <- ggplot(fish.id, aes(PosX, PosY)) +
        scale_x_continuous('x (m)', limits = c(10, 45)) + scale_y_continuous('y (m)', limits = c(35, 70)) + 
        theme(panel.background = element_rect(fill = 'white', colour = 'black')) + # white background, black lines
        geom_point(aes(colour = SUN), size = 3)  + scale_color_manual(name = 'Time of day', values = lightpal, labels = c('Dawn', 'Day', 'Dusk', 'Night')) +
        annotate('segment', x = locations.lookup['14CSW', 'xmin'], xend = locations.lookup['14CSE', 'xmax'], y = locations.lookup['14CSW', 'ymin'], yend = locations.lookup['14CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
        annotate('segment', x = locations.lookup['14CSW', 'xmin'], xend = locations.lookup['14CNW', 'xmin'], y = locations.lookup['14CSW', 'ymin'], yend = locations.lookup['14CNW', 'ymax'], colour = pen.col, size = pen.size) +  # pen boundary
        annotate('segment', x = locations.lookup['14CNW', 'xmin'], xend = locations.lookup['14CNE', 'xmax'], y = locations.lookup['14CNW', 'ymax'], yend = locations.lookup['14CNE', 'ymax'], colour = pen.col, size = pen.size) + # pen boundary
        annotate('segment', x = locations.lookup['14CNE', 'xmax'], xend = locations.lookup['14CSE', 'xmax'], y = locations.lookup['14CNE', 'ymax'], yend = locations.lookup['14CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
        annotate('segment', x = locations.lookup['14CSW', 'xmin'], xend = locations.lookup['14CSE', 'xmax'], y = locations.lookup['14CSW', 'ymax'], yend = locations.lookup['14CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
        annotate('segment', x = locations.lookup['14CSW', 'xmax'], xend = locations.lookup['14CNW', 'xmax'], y = locations.lookup['14CSW', 'ymin'], yend = locations.lookup['14CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
        annotate('segment', x = locations.lookup['14CNW', 'xmin'], xend = locations.lookup['14CNE', 'xmax'], y = locations.lookup['14CNW', 'ymin'], yend = locations.lookup['14CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
        annotate('segment', x = locations.lookup['14CNE', 'xmin'], xend = locations.lookup['14CSE', 'xmin'], y = locations.lookup['14CNE', 'ymax'], yend = locations.lookup['14CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
        annotate('segment', x = locations.lookup['14HET', 'xmin'], xend = locations.lookup['14HET', 'xmax'], y = locations.lookup['14HET', 'ymin'], yend = locations.lookup['14HET', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
        annotate('segment', x = locations.lookup['14HEB', 'xmin'], xend = locations.lookup['14HEB', 'xmax'], y = locations.lookup['14HEB', 'ymin'], yend = locations.lookup['14HEB', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
        annotate('segment', x = locations.lookup['14HWT', 'xmin'], xend = locations.lookup['14HWT', 'xmax'], y = locations.lookup['14HWT', 'ymin'], yend = locations.lookup['14HWT', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
        annotate('segment', x = locations.lookup['14HWB', 'xmin'], xend = locations.lookup['14HWB', 'xmax'], y = locations.lookup['14HWB', 'ymin'], yend = locations.lookup['14HWB', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
        annotate('segment', x = locations.lookup['FS14', 'xmin'], xend = locations.lookup['FS14', 'xmax'], y = locations.lookup['FS14', 'ymin'], yend = locations.lookup['FS14', 'ymax'], colour = pen.col, linetype = 'dotdash', size = pen.size) # + # Feed Station
      fish.plot
      
    } else {
      
      if (unique(fish.id$PEN == '15')) {
        
        fish.plot <- ggplot(fish.id, aes(PosX, PosY)) +
          scale_x_continuous('x (m)', limits = c(10, 45)) + scale_y_continuous('y (m)', limits = c(10, 45)) + 
          theme(panel.background = element_rect(fill = 'white', colour = 'black')) + # white background, black lines
          geom_point(aes(colour = SUN), size = 3)  + scale_color_manual(name = 'Time of day', values = lightpal, labels = c('Dawn', 'Day', 'Dusk', 'Night')) +
          annotate('segment', x = locations.lookup['15CSW', 'xmin'], xend = locations.lookup['15CSE', 'xmax'], y = locations.lookup['15CSW', 'ymin'], yend = locations.lookup['15CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
          annotate('segment', x = locations.lookup['15CSW', 'xmin'], xend = locations.lookup['15CNW', 'xmin'], y = locations.lookup['15CSW', 'ymin'], yend = locations.lookup['15CNW', 'ymax'], colour = pen.col, size = pen.size) +  # pen boundary
          annotate('segment', x = locations.lookup['15CNW', 'xmin'], xend = locations.lookup['15CNE', 'xmax'], y = locations.lookup['15CNW', 'ymax'], yend = locations.lookup['15CNE', 'ymax'], colour = pen.col, size = pen.size) + # pen boundary
          annotate('segment', x = locations.lookup['15CNE', 'xmax'], xend = locations.lookup['15CSE', 'xmax'], y = locations.lookup['15CNE', 'ymax'], yend = locations.lookup['15CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
          annotate('segment', x = locations.lookup['15CSW', 'xmin'], xend = locations.lookup['15CSE', 'xmax'], y = locations.lookup['15CSW', 'ymax'], yend = locations.lookup['15CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
          annotate('segment', x = locations.lookup['15CSW', 'xmax'], xend = locations.lookup['15CNW', 'xmax'], y = locations.lookup['15CSW', 'ymin'], yend = locations.lookup['15CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
          annotate('segment', x = locations.lookup['15CNW', 'xmin'], xend = locations.lookup['15CNE', 'xmax'], y = locations.lookup['15CNW', 'ymin'], yend = locations.lookup['15CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
          annotate('segment', x = locations.lookup['15CNE', 'xmin'], xend = locations.lookup['15CSE', 'xmin'], y = locations.lookup['15CNE', 'ymax'], yend = locations.lookup['15CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
          annotate('segment', x = locations.lookup['15HET', 'xmin'], xend = locations.lookup['15HET', 'xmax'], y = locations.lookup['15HET', 'ymin'], yend = locations.lookup['15HET', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
          annotate('segment', x = locations.lookup['15HEB', 'xmin'], xend = locations.lookup['15HEB', 'xmax'], y = locations.lookup['15HEB', 'ymin'], yend = locations.lookup['15HEB', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
          annotate('segment', x = locations.lookup['15HWT', 'xmin'], xend = locations.lookup['15HWT', 'xmax'], y = locations.lookup['15HWT', 'ymin'], yend = locations.lookup['15HWT', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
          annotate('segment', x = locations.lookup['15HWB', 'xmin'], xend = locations.lookup['15HWB', 'xmax'], y = locations.lookup['15HWB', 'ymin'], yend = locations.lookup['15HWB', 'ymax'], colour = pen.col, linetype = 'longdash', size = pen.size) + # hide boundary
          annotate('segment', x = locations.lookup['FS15', 'xmin'], xend = locations.lookup['FS15', 'xmax'], y = locations.lookup['FS15', 'ymin'], yend = locations.lookup['FS15', 'ymax'], colour = pen.col, linetype = 'dotdash', size = pen.size) # + # Feed Station
        fish.plot
      }
    }
  }
  
}

# 20. Add a fish to the current plot

add.depthfish <- function(period)
{
  depthpal <- diverge_hcl(30, h = c(11,266), c = 100, l = c(21,85), power = 0.6, alpha = 0.2)
  fish.id <- subset(dayfile, Period == period)
  points(fish.id$PosX, fish.id$PosY, pch = 20, cex = 1, col = depthpal[round(fish.id$PosZ)])
}




# 21. Fractal dimension [Laga data?: NEED to ask about x and y min/max: where to infer? Doesn't match masterloc]

fractal <- function(xmin7 = 5, xmax7 = 45, ymin7 = 5, ymax7 = 45, xmin8 = 35, xmax8 = 75, ymin8 = 5, ymax8 = 45, boxsize = 0.1) {
  
  fd.P7 <- data.frame(x = numeric, y = integer)
  fd.P8 <- data.frame(x = numeric, y = integer)
  bs <- boxsize
  
  pen.id <- subset(dayfile, PEN == '7')
  
  repeat {
    
    
    x.grid <- floor((pen.id$PosX - xmin7) / bs) + 1
    y.grid <- floor((pen.id$PosY - ymin7) / bs) + 1
    x.grid.max <- floor((xmax7 - xmin7) / bs) + 1
    y.grid.max <- floor((ymax7 - ymin7) / bs) + 1
    t.x <- sort(unique(x.grid))
    t.y <- sort(unique(y.grid))
    tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
    ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
    t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
    grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
    t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
    t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
    eg <- expand.grid(t.y,t.x)
    grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
    fd.P7 <- rbind(fd.P7, c(bs, length(which(grid.cov > 0))))
    bs <- bs*2
    
    if (bs > xmax7-xmin7 | bs > ymax7-ymin7)
    {break}
  }
  colnames(fd.P7) <- c('P7.boxsize', 'P7.count')
  bs <- boxsize
  
  
  fl <- lm(log(P7.count) ~ log(P7.boxsize), data=fd.P7)
  scatterplot(fd.P7$P7.boxsize, fd.P7$P7.count, log = 'xy', boxplots = FALSE, smoother = FALSE, grid = FALSE)
  text(1, 100, paste0('fd = ', as.character(round(fl$coefficients[[2]], 3)), '\nR2 = ', round(summary(fl)$r.squared, 4)))
  
  #scatterplot(fd.P7$P7.boxsize, fd.P7$P7.count, log = 'xy', boxplots = FALSE, smoother = FALSE, grid = FALSE)
  
  cat('Press [enter] to continue')
  line <- readline()
  
  pen.id <- subset(dayfile, PEN == '8')
  
  repeat{
    
    x.grid <- floor((pen.id$PosX - xmin8) / bs) + 1
    y.grid <- floor((pen.id$PosY - ymin8) / bs) + 1
    x.grid.max <- floor((xmax8 - xmin8) / bs) + 1
    y.grid.max <- floor((ymax8 - ymin8) / bs) + 1
    t.x <- sort(unique(x.grid))
    t.y <- sort(unique(y.grid))
    tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
    ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
    t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
    grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
    t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
    t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
    eg <- expand.grid(t.y,t.x)
    grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
    fd.P8 <- rbind(fd.P8, c(bs, length(which(grid.cov > 0))))
    bs <- bs*2
    
    if (bs > xmax8-xmin8 | bs > ymax8-ymin8)
    {break}
  }
  colnames(fd.P8) <- c('P8.boxsize', 'P8.count')
  
  fl <- lm(log(P8.count) ~ log(P8.boxsize), data=fd.P8)
  scatterplot(fd.P8$P8.boxsize, fd.P8$P8.count, log = 'xy', boxplots = FALSE, smoother = FALSE, grid = FALSE)
  text(1, 100, paste0('fd = ', as.character(round(fl$coefficients[[2]], 3)), '\nR2 = ', round(summary(fl)$r.squared, 4)))
  
  fd <- cbind(fd.P7, fd.P8) 
  fd
  
  
}


# 22. batch Fractal dimension

batch.fractals <- function(xmin7 = 5, xmax7 = 45, ymin7 = 5, ymax7 = 45, xmin8 = 35, xmax8 = 75, ymin8 = 5, ymax8 = 45, boxsize = 0.1) {
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  #fcount.P7 <- data.frame(x = numeric, y = integer)
  #fcount.P8 <- data.frame(x = numeric, y = integer)
  bs <- boxsize
  
  dayfile.loc <- files[[1]]
  dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = 'character')
  # dayfile[,1] <- NULL
  
  pen.id <- subset(dayfile, dayfile$PEN == '7')
  fish.ids7 <- unique(pen.id$Period)
  fd.P7 <- data.frame(fish.ids7)
  rownames(fd.P7) <- fd.P7[,1]
  colnames(fd.P7) <- 'Period'
  pen.id <- subset(dayfile, dayfile$PEN == '8')
  fish.ids8 <- unique(pen.id$Period)
  fd.P8 <- data.frame(fish.ids8)
  rownames(fd.P8) <- fd.P8[,1]
  colnames(fd.P8) <- 'Period'
  
  for (n in 1:length(files))
  {
    dayfile.loc <- files[[n]]
    dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = c
                        (
                          'NULL', 'factor', 'factor', 'factor', 'POSIXct', 'double', 'double', 
                          'double', 'double', 'double', 'double', 'double', 'double', 'factor',
                          'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                          'double', 'double', 'double', 'double', 'double', 'double', 'double',
                          'double', 'double', 'double', 'double', 'double', 'double', 'double',
                          'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                          'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                          'factor', 'factor', 'double', 'double', 'double', 'double', 'double', 
                          'double', 'double', 'double', 'double', 'double', 'double', 'double' 
                        )) #read data into table
    
    fcount.P7 <- data.frame(x = numeric, y = integer)
    fcount.P8 <- data.frame(x = numeric, y = integer)  
    
    pen.id <- subset(dayfile, PEN == '7')
    
    for (i in 1:length(fish.ids7)){
      
      fish.id <- subset(pen.id, Period == fish.ids7[[i]])  
      
      if(nrow(fish.id) == 0){
        fd.P7[i,paste0(n, '.fractal')] <- NA
        fd.P7[i,paste0(n, '.R2')] <- NA
      }
      else{
        
        repeat {
          
          x.grid <- floor((fish.id$PosX - xmin7) / bs) + 1
          y.grid <- floor((fish.id$PosY - ymin7) / bs) + 1
          x.grid.max <- floor((xmax7 - xmin7) / bs) + 1
          y.grid.max <- floor((ymax7 - ymin7) / bs) + 1
          t.x <- sort(unique(x.grid))
          t.y <- sort(unique(y.grid))
          tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
          ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
          t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
          grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
          t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
          t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
          eg <- expand.grid(t.y,t.x)
          grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
          fcount.P7 <- rbind(fcount.P7, c(bs, length(which(grid.cov > 0))))
          bs <- bs*2
          
          if (bs > xmax7-xmin7 | bs > ymax7-ymin7)
          {break}
        }
        colnames(fcount.P7) <- c('P7.boxsize', 'P7.count')
        bs <- boxsize
        
        
        fl <- lm(log(P7.count) ~ log(P7.boxsize), data=fcount.P7)
        fd.P7[i,paste0(n, '.fractal')] <- round(fl$coefficients[[2]], 3)
        fd.P7[i,paste0(n, '.R2')] <- round(summary(fl)$r.squared, 4)
        #print(fcount.P7)
        
      }
      
    }
    
    pen.id <- subset(dayfile, PEN == '8')
    
    
    for (i in 1:length(fish.ids8)){
      
      fish.id <- subset(pen.id, Period == fish.ids8[[i]])
      
      if(nrow(fish.id) == 0){
        fd.P8[i,paste0(n, '.fractal')] <- NA
        fd.P8[i,paste0(n, '.R2')] <- NA
      }
      else{
        
        repeat{
          
          x.grid <- floor((fish.id$PosX - xmin8) / bs) + 1
          y.grid <- floor((fish.id$PosY - ymin8) / bs) + 1
          x.grid.max <- floor((xmax8 - xmin8) / bs) + 1
          y.grid.max <- floor((ymax8 - ymin8) / bs) + 1
          t.x <- sort(unique(x.grid))
          t.y <- sort(unique(y.grid))
          tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
          ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
          t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
          grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
          t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
          t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
          eg <- expand.grid(t.y,t.x)
          grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
          fcount.P8 <- rbind(fcount.P8, c(bs, length(which(grid.cov > 0))))
          bs <- bs*2
          
          if (bs > xmax8-xmin8 | bs > ymax8-ymin8)
          {break}
        }
        colnames(fcount.P8) <- c('P8.boxsize', 'P8.count')
        bs <- boxsize
        
        
        fl <- lm(log(P8.count) ~ log(P8.boxsize), data=fcount.P8)
        fd.P8[i,paste0(n, '.fractal')] <- round(fl$coefficients[[2]], 3)
        fd.P8[i,paste0(n, '.R2')] <- round(summary(fl)$r.squared, 4)
        #print(fcount.P8)
        
      }
      
    }
    
    remove(fcount.P7)
    remove(fcount.P8)
    
  }
  
  #fd.P7$fish.ids7 <- NULL
  #fd.P8$fish.ids8 <- NULL
  fd <- rbind(fd.P7, fd.P8) 
  fd
  #loadWorkbook('FractalOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('FractalOutput.xlsx', fd, 'Sheet 1')
  
  write.xlsx(fd, 'FractalOutput.xlsx')
}




# 23. Invidual fish Fractal dimension

id.fractals <- function(xmin7 = 5, xmax7 = 45, ymin7 = 5, ymax7 = 45, xmin8 = 35, xmax8 = 75, ymin8 = 5, ymax8 = 45, boxsize = 0.1) {
  
  #files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  #fcount.P7 <- data.frame(x = numeric, y = integer)
  #fcount.P8 <- data.frame(x = numeric, y = integer)
  bs <- boxsize
  
  #dayfile.loc <- files[[1]]
  dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = c
                      (
                        'NULL', 'factor', 'factor', 'factor', 'POSIXct', 'double', 'double', 
                        'double', 'double', 'double', 'double', 'double', 'double', 'factor',
                        'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                        'double', 'double', 'double', 'double', 'double', 'double', 'double',
                        'double', 'double', 'double', 'double', 'double', 'double', 'double',
                        'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                        'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                        'factor', 'factor', 'double', 'double', 'double', 'double', 'double', 
                        'double', 'double', 'double', 'double', 'double', 'double', 'double' 
                      )) #read data into table
  # dayfile[,1] <- NULL
  
  pen.id <- subset(dayfile, dayfile$PEN == '7')
  fish.ids7 <- unique(pen.id$Period)
  fd.P7 <- data.frame(fish.ids7)
  rownames(fd.P7) <- fd.P7[,1]
  colnames(fd.P7) <- 'Period'
  pen.id <- subset(dayfile, dayfile$PEN == '8')
  fish.ids8 <- unique(pen.id$Period)
  fd.P8 <- data.frame(fish.ids8)
  rownames(fd.P8) <- fd.P8[,1]
  colnames(fd.P8) <- 'Period'
  
  
  fcount.P7 <- data.frame(x = numeric, y = integer)
  fcount.P8 <- data.frame(x = numeric, y = integer)  
  
  pen.id <- subset(dayfile, PEN == '7')
  
  for (i in 1:length(fish.ids7)){
    
    fish.id <- subset(pen.id, Period == fish.ids7[[i]])  
    
    if(nrow(fish.id) == 0){
      fd.P7[i,'fractal'] <- NA
      fd.P7[i,'R2'] <- NA
    }
    else{
      
      repeat {
        
        x.grid <- floor((fish.id$PosX - xmin7) / bs) + 1
        y.grid <- floor((fish.id$PosY - ymin7) / bs) + 1
        x.grid.max <- floor((xmax7 - xmin7) / bs) + 1
        y.grid.max <- floor((ymax7 - ymin7) / bs) + 1
        t.x <- sort(unique(x.grid))
        t.y <- sort(unique(y.grid))
        tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
        ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
        t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
        grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
        t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
        t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
        eg <- expand.grid(t.y,t.x)
        grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
        fcount.P7 <- rbind(fcount.P7, c(bs, length(which(grid.cov > 0))))
        bs <- bs*2
        
        if (bs > xmax7-xmin7 | bs > ymax7-ymin7)
        {break}
      }
      colnames(fcount.P7) <- c('P7.boxsize', 'P7.count')
      bs <- boxsize
      
      
      fl <- lm(log(P7.count) ~ log(P7.boxsize), data=fcount.P7)
      fd.P7[i,'fractal'] <- round(fl$coefficients[[2]], 3)
      fd.P7[i,'R2'] <- round(summary(fl)$r.squared, 4)
      #print(fcount.P7)
      
    }
    
  }
  
  pen.id <- subset(dayfile, PEN == '8')
  
  
  for (i in 1:length(fish.ids8)){
    
    fish.id <- subset(pen.id, Period == fish.ids8[[i]])
    
    if(nrow(fish.id) == 0){
      fd.P8[i,'fractal'] <- NA
      fd.P8[i,'R2'] <- NA
    }
    else{
      
      repeat{
        
        x.grid <- floor((fish.id$PosX - xmin8) / bs) + 1
        y.grid <- floor((fish.id$PosY - ymin8) / bs) + 1
        x.grid.max <- floor((xmax8 - xmin8) / bs) + 1
        y.grid.max <- floor((ymax8 - ymin8) / bs) + 1
        t.x <- sort(unique(x.grid))
        t.y <- sort(unique(y.grid))
        tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
        ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
        t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
        grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
        t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
        t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
        eg <- expand.grid(t.y,t.x)
        grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
        fcount.P8 <- rbind(fcount.P8, c(bs, length(which(grid.cov > 0))))
        bs <- bs*2
        
        if (bs > xmax8-xmin8 | bs > ymax8-ymin8)
        {break}
      }
      colnames(fcount.P8) <- c('P8.boxsize', 'P8.count')
      bs <- boxsize
      
      
      fl <- lm(log(P8.count) ~ log(P8.boxsize), data=fcount.P8)
      fd.P8[i,'fractal'] <- round(fl$coefficients[[2]], 3)
      fd.P8[i,'R2'] <- round(summary(fl)$r.squared, 4)
      #print(fcount.P8)
      
    }
    
  }
  
  #print(fcount.P7)
  #print(fcount.P8)
  
  
  
  #fd.P7$fish.ids7 <- NULL
  #fd.P8$fish.ids8 <- NULL
  fd <- rbind(fd.P7, fd.P8) 
  print(fd)
  loadWorkbook('FractalOutput.xlsx', create = TRUE)
  writeWorksheetToFile('FractalOutput.xlsx', fd, 'Sheet 1')
}


# 24. draws a plot of fish location coloured by time

plot.bytime <- function(period, units = 'd')
{
  fish.id <- subset(dayfile, Period == period)
  ifelse(units == 'd', timepoints <- unique(format(as.Date(dayfile$EchoTime, format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d')), ifelse(units == 'h', timepoints <- unique(trunc(dayfile$EchoTime, "hour")), print('Error: specify days (d) or hours (h)'))) 
  bins <- length(timepoints)
  timepal <- rainbow(bins, alpha = 0.2)
  par(mfrow=c(1,1))
  
  if(fish.id[1,3] == '12')
  {
    if(units == 'd'){
      plot(fish.id[which(format(as.Date(dayfile$EchoTime, format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d') == timepoints[[1]]),'PosX'], fish.id[which(format(as.Date(dayfile$EchoTime, format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d') == timepoints[[1]]),'PosY'], xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(35, 70), ylim = c(35, 70), type = 'p', col = timepal[1])
    }else{
      plot(fish.id[which(trunc(dayfile$EchoTime, "hour") == timepoints[1]),'PosX'], fish.id[which(trunc(dayfile$EchoTime, "hour") == timepoints[1]),'PosY'], xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(37, 72), ylim = c(37, 72), type = 'p', col = timepal[1])
    }
    
    legend(10, 47, as.character(1:bins), col = rainbow(bins, alpha = 1) , pch = 15, bty = 'n', pt.cex = 1.5, horiz = FALSE, y.intersp = 1, cex = (100-bins)/100)
    
    
    if(units == 'd'){
      for (i in 2:bins){
        points(fish.id[which(format(as.Date(dayfile$EchoTime, format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d') == timepoints[[i]]),'PosX'], fish.id[which(format(as.Date(dayfile$EchoTime, format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d') == timepoints[[i]]),'PosY'], pch = 20, cex = 1, col = timepal[i])
      }
    }else{
      for (i in 2:bins){   
        points(fish.id[which(trunc(dayfile$EchoTime, "hour") == timepoints[i]),'PosX'], fish.id[which(trunc(dayfile$EchoTime, "hour") == timepoints[i]),'PosY'], pch = 20, cex = 1, col = timepal[i])
      }
    }
    
    rect(locations.lookup['12EW', 'xmin'], locations.lookup['12EW', 'ymin'], locations.lookup['12EW', 'xmax'], locations.lookup['12EW', 'ymax'], lty = 2) # 12EW edge
    rect(locations.lookup['12ES', 'xmin'], locations.lookup['12ES', 'ymin'], locations.lookup['12ES', 'xmax'], locations.lookup['12ES', 'ymax'], lty = 2) # 12ES edge
    rect(locations.lookup['12EE', 'xmin'], locations.lookup['12EE', 'ymin'], locations.lookup['12EE', 'xmax'], locations.lookup['12EE', 'ymax'], lty = 2) # 12EE edge
    rect(locations.lookup['12EN', 'xmin'], locations.lookup['12EN', 'ymin'], locations.lookup['12EN', 'xmax'], locations.lookup['12EN', 'ymax'], lty = 2) # 12EN edge
    rect(locations.lookup['12HET', 'xmin'], locations.lookup['12HET', 'ymin'], locations.lookup['12HET', 'xmax'], locations.lookup['12HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HET
    rect(locations.lookup['12HEB', 'xmin'], locations.lookup['12HEB', 'ymin'], locations.lookup['12HEB', 'xmax'], locations.lookup['12HEB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HEB
    rect(locations.lookup['12HWT', 'xmin'], locations.lookup['12HWT', 'ymin'], locations.lookup['12HWT', 'xmax'], locations.lookup['12HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HWT
    rect(locations.lookup['12HWB', 'xmin'], locations.lookup['12HWB', 'ymin'], locations.lookup['12HWB', 'xmax'], locations.lookup['12HWB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HWB
    rect(locations.lookup['FS12', 'xmin'], locations.lookup['FS12', 'ymin'], locations.lookup['FS12', 'xmax'], locations.lookup['FS12', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 12FS
    rect(locations.lookup['12EW', 'xmin'], locations.lookup['12ES', 'ymin'], locations.lookup['12EE', 'xmax'], locations.lookup['12EN', 'ymax'], lwd = 2) # cage limits
    
  }else{ if(fish.id[1,3] == '14'){
    
    
    if(units == 'd'){
      plot(fish.id[which(format(as.Date(dayfile$EchoTime, format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d') == timepoints[[1]]),'PosX'], fish.id[which(format(as.Date(dayfile$EchoTime, format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d') == timepoints[[1]]),'PosY'], xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(25, 70), ylim = c(0, 45), type = 'p', col = timepal[1])
    }else{
      plot(fish.id[which(trunc(dayfile$EchoTime, "hour") == timepoints[1]),'PosX'], fish.id[which(trunc(dayfile$EchoTime, "hour") == timepoints[1]),'PosY'], xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(37, 72), ylim = c(10, 45), type = 'p', col = timepal[1])
    }
    
    legend(32, 47, as.character(1:bins), col = rainbow(bins, alpha = 1) , pch = 15, bty = 'n', pt.cex = 1.5, horiz = FALSE, y.intersp = 1, cex = (100-bins)/100)
    
    if(units == 'd'){ 
      for (i in 2:bins){
        points(fish.id[which(format(as.Date(dayfile$EchoTime, format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d') == timepoints[[i]]),'PosX'], fish.id[which(format(as.Date(dayfile$EchoTime, format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d') == timepoints[[i]]),'PosY'], pch = 20, cex = 1, col = timepal[i])
      }
    }else{
      for (i in 2:bins){    
        points(fish.id[which(trunc(dayfile$EchoTime, "hour") == timepoints[i]),'PosX'], fish.id[which(trunc(dayfile$EchoTime, "hour") == timepoints[i]),'PosY'], pch = 20, cex = 1, col = timepal[i])
      }
    }
    
    rect(locations.lookup['14EW', 'xmin'], locations.lookup['14EW', 'ymin'], locations.lookup['14EW', 'xmax'], locations.lookup['14EW', 'ymax'], lty = 2) # 14EW edge
    rect(locations.lookup['14ES', 'xmin'], locations.lookup['14ES', 'ymin'], locations.lookup['14ES', 'xmax'], locations.lookup['14ES', 'ymax'], lty = 2) # 14ES edge
    rect(locations.lookup['14EE', 'xmin'], locations.lookup['14EE', 'ymin'], locations.lookup['14EE', 'xmax'], locations.lookup['14EE', 'ymax'], lty = 2) # 14EE edge
    rect(locations.lookup['14EN', 'xmin'], locations.lookup['14EN', 'ymin'], locations.lookup['14EN', 'xmax'], locations.lookup['14EN', 'ymax'], lty = 2) # 14EN edge
    rect(locations.lookup['14HET', 'xmin'], locations.lookup['14HET', 'ymin'], locations.lookup['14HET', 'xmax'], locations.lookup['14HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HET
    rect(locations.lookup['14HEB', 'xmin'], locations.lookup['14HEB', 'ymin'], locations.lookup['14HEB', 'xmax'], locations.lookup['14HEB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HEB
    rect(locations.lookup['14HWT', 'xmin'], locations.lookup['14HWT', 'ymin'], locations.lookup['14HWT', 'xmax'], locations.lookup['14HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HWT
    rect(locations.lookup['14HWB', 'xmin'], locations.lookup['14HWB', 'ymin'], locations.lookup['14HWB', 'xmax'], locations.lookup['14HWB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HWB
    rect(locations.lookup['FS14', 'xmin'], locations.lookup['FS14', 'ymin'], locations.lookup['FS14', 'xmax'], locations.lookup['FS14', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 14FS
    rect(locations.lookup['14EW', 'xmin'], locations.lookup['14ES', 'ymin'], locations.lookup['14EE', 'xmax'], locations.lookup['14EN', 'ymax'], lwd = 2) # cage limits
    
  }
    else{ if(fish.id[1,3] == '15'){
      if(units == 'd'){
        plot(fish.id[which(format(as.Date(dayfile$EchoTime, format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d') == timepoints[[1]]),'PosX'], fish.id[which(format(as.Date(dayfile$EchoTime, format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d') == timepoints[[1]]),'PosY'], xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(25, 70), ylim = c(0, 45), type = 'p', col = timepal[1])
      }else{
        plot(fish.id[which(trunc(dayfile$EchoTime, "hour") == timepoints[1]),'PosX'], fish.id[which(trunc(dayfile$EchoTime, "hour") == timepoints[1]),'PosY'], xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(10, 45), ylim = c(10, 45), type = 'p', col = timepal[1])
      }
      
      legend(32, 47, as.character(1:bins), col = rainbow(bins, alpha = 1) , pch = 15, bty = 'n', pt.cex = 1.5, horiz = FALSE, y.intersp = 1, cex = (100-bins)/100)
      
      if(units == 'd'){ 
        for (i in 2:bins){
          points(fish.id[which(format(as.Date(dayfile$EchoTime, format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d') == timepoints[[i]]),'PosX'], fish.id[which(format(as.Date(dayfile$EchoTime, format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d') == timepoints[[i]]),'PosY'], pch = 20, cex = 1, col = timepal[i])
        }
      }else{
        for (i in 2:bins){    
          points(fish.id[which(trunc(dayfile$EchoTime, "hour") == timepoints[i]),'PosX'], fish.id[which(trunc(dayfile$EchoTime, "hour") == timepoints[i]),'PosY'], pch = 20, cex = 1, col = timepal[i])
        }
      }
      
      rect(locations.lookup['15EW', 'xmin'], locations.lookup['15EW', 'ymin'], locations.lookup['15EW', 'xmax'], locations.lookup['15EW', 'ymax'], lty = 2) # 15EW edge
      rect(locations.lookup['15ES', 'xmin'], locations.lookup['15ES', 'ymin'], locations.lookup['15ES', 'xmax'], locations.lookup['15ES', 'ymax'], lty = 2) # 15ES edge
      rect(locations.lookup['15EE', 'xmin'], locations.lookup['15EE', 'ymin'], locations.lookup['15EE', 'xmax'], locations.lookup['15EE', 'ymax'], lty = 2) # 15EE edge
      rect(locations.lookup['15EN', 'xmin'], locations.lookup['15EN', 'ymin'], locations.lookup['15EN', 'xmax'], locations.lookup['15EN', 'ymax'], lty = 2) # 15EN edge
      rect(locations.lookup['15HET', 'xmin'], locations.lookup['15HET', 'ymin'], locations.lookup['15HET', 'xmax'], locations.lookup['15HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HET
      rect(locations.lookup['15HEB', 'xmin'], locations.lookup['15HEB', 'ymin'], locations.lookup['15HEB', 'xmax'], locations.lookup['15HEB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HEB
      rect(locations.lookup['15HWT', 'xmin'], locations.lookup['15HWT', 'ymin'], locations.lookup['15HWT', 'xmax'], locations.lookup['15HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HWT
      rect(locations.lookup['15HWB', 'xmin'], locations.lookup['15HWB', 'ymin'], locations.lookup['15HWB', 'xmax'], locations.lookup['15HWB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HWB
      rect(locations.lookup['FS15', 'xmin'], locations.lookup['FS15', 'ymin'], locations.lookup['FS15', 'xmax'], locations.lookup['FS15', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 15FS
      rect(locations.lookup['15EW', 'xmin'], locations.lookup['15ES', 'ymin'], locations.lookup['15EE', 'xmax'], locations.lookup['15EN', 'ymax'], lwd = 2) # cage limits
      
    }
    }
    
  }
  
  
  remove(timepoints)
}

# 25. Removes single fish id from specified day files

batch.remove <- function(period, start.day, no.days){
  
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  day1 <- grep(paste0('^..............', start.day, '_day_coded.csv'), files)
  end.day <- day1+(no.days-1)
  # dayfile.loc <- files[[grep(paste0('^..............', start.day, '_day_coded.csv'), files)]]
  
  for (i in day1:end.day) {
    dayfile <- read.csv(files[[i]], header = TRUE, sep = ",", colClasses = c('NULL', 'numeric', 'factor', 'factor', 'POSIXct', 'double', 'double',
                                                                             'double', 'double', 'double', 'double', 'double', 'double', 'double', 'double', 'double', 'factor',
                                                                             'factor', 'factor', 'factor', 'factor', 'double', 'double', 'double',
                                                                             'double', 'double', 'double', 'double', 'double', 'double', 'double',
                                                                             'double', 'double', 'double', 'double', 'double', 'double', 'double',
                                                                             'double', 'double', 'double', 'double', 'factor', 'factor', 'factor',
                                                                             'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                                                                             'factor', 'factor', 'factor', 'double', 'double', 'double', 'double',
                                                                             'double', 'double', 'double', 'double', 'double', 'double', 'double', 'double'
    )) #read data into table
    
    dayfile <- dayfile[!(dayfile$Period == period),] # remove dead fish
    write.csv(dayfile, file = files[[i]]) #write output to file
    
  } 
  
}



# 26. proportion coverage 3D (not sure this is working properly!)

prop.coverage.3d <- function(xmin7 = 15, xmax7 = 40, ymin7 = 15, ymax7 = 40, xmin8 = 42, xmax8 = 67, ymin8 = 15, ymax8 = 40, zmin7 = 0, zmax7 = 15, zmin8 = 0, zmax8 = 15, boxsize = 0.3) {
  fish.id <- subset(dayfile, PEN == '7')
  x.grid <- floor((fish.id$PosX - xmin7) / boxsize) + 1
  y.grid <- floor((fish.id$PosY - ymin7) / boxsize) + 1
  z.grid <- floor((fish.id$PosZ - zmin7) / boxsize) + 1
  x.grid.max <- floor((xmax7 - xmin7) / boxsize) + 1
  y.grid.max <- floor((ymax7 - ymin7) / boxsize) + 1
  z.grid.max <- floor((zmax7 - zmin7) / boxsize) + 1
  t.x <- sort(unique(x.grid))
  t.y <- sort(unique(y.grid))
  t.z <- sort(unique(z.grid))
  tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
  ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
  tz.range <- c(min(which(t.z > 0)), max(which(t.z <= z.grid.max)))
  t.xy <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
  t.yz <- table(y.grid, z.grid)[ty.range[1]:ty.range[2],tz.range[1]:tz.range[2]]
  grid.cov.xy <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
  grid.cov.yz <- matrix(0,nrow=y.grid.max,ncol=z.grid.max)
  t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
  t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
  t.z <- t.z[(t.z > 0) & (t.z <= z.grid.max)]
  eg.xy <- expand.grid(t.y,t.x)
  eg.yz <- expand.grid(t.y,t.z)
  grid.cov.xy[cbind(eg.xy$Var1,eg.xy$Var2)] <- as.vector(t.xy)  
  grid.cov.yz[cbind(eg.yz$Var1,eg.yz$Var2)] <- as.vector(t.yz) 
  coverage.P7 <- matrix(c(round(length(which(grid.cov.xy > 0))+length(which(grid.cov.yz > 0)), digits = 3), round(length(grid.cov.xy)*((zmax7-zmin7)/boxsize), digits = 3), signif((length(which(grid.cov.xy > 0))+length(which(grid.cov.yz > 0)))/(length(grid.cov.xy)*((zmax7-zmin7)/boxsize)), digits = 3)), ncol = 3)
  coverage.P7
  colnames(coverage.P7) <- c('occupied', 'total', 'proportion')
  
  
  #density.pal <- heat_hcl(length(as.vector(t)))
  #eg$col <- as.vector(t)
  #plot(eg$Var1, eg$Var2, col = density.pal[eg$col], pch = 15, cex = 2.5)
  
  
  fish.id <- subset(dayfile, PEN == '8')
  x.grid <- floor((fish.id$PosX - xmin8) / boxsize) + 1
  y.grid <- floor((fish.id$PosY - ymin8) / boxsize) + 1
  z.grid <- floor((fish.id$PosZ - zmin8) / boxsize) + 1
  x.grid.max <- floor((xmax8 - xmin8) / boxsize) + 1
  y.grid.max <- floor((ymax8 - ymin8) / boxsize) + 1
  z.grid.max <- floor((zmax8 - zmin8) / boxsize) + 1
  t.x <- sort(unique(x.grid))
  t.y <- sort(unique(y.grid))
  t.z <- sort(unique(z.grid))
  tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
  ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
  tz.range <- c(min(which(t.z > 0)), max(which(t.z <= z.grid.max)))
  t.xy <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
  t.yz <- table(y.grid, z.grid)[ty.range[1]:ty.range[2],tz.range[1]:tz.range[2]]
  grid.cov.xy <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
  grid.cov.yz <- matrix(0,nrow=y.grid.max,ncol=z.grid.max)
  t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
  t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
  t.z <- t.z[(t.z > 0) & (t.z <= z.grid.max)]
  eg.xy <- expand.grid(t.y,t.x)
  eg.yz <- expand.grid(t.y,t.z)
  grid.cov.xy[cbind(eg.xy$Var1,eg.xy$Var2)] <- as.vector(t.xy)  
  grid.cov.yz[cbind(eg.yz$Var1,eg.yz$Var2)] <- as.vector(t.yz) 
  coverage.P8 <- matrix(c(round(length(which(grid.cov.xy > 0))+length(which(grid.cov.yz > 0)), digits = 3), round(length(grid.cov.xy)*((zmax8-zmin8)/boxsize), digits = 3), signif((length(which(grid.cov.xy > 0))+length(which(grid.cov.yz > 0)))/(length(grid.cov.xy)*((zmax8-zmin8)/boxsize)), digits = 3)), ncol = 3)
  coverage.P8
  colnames(coverage.P8) <- c('occupied', 'total', 'proportion')
  
  coverage <- rbind(coverage.P7, coverage.P8) 
  rownames(coverage) <- c('P7', 'P8')
  coverage
}



# 27. moving average filter function


ma.filter <- function(period, smooth = 20, thresh = 5){
  
  fish.id <- subset(dayfile, dayfile$Period == period)
  par(mfrow=c(2,2))
  #fish.id <- subset(fish.id, fish.id$SEC >5 | is.na(fish.id$SEC) == TRUE) # remove entries where time delay too low or too high
  plot(fish.id$PosX, fish.id$PosY, xlab = 'Original', ylab = '')
  axes <- par('usr')
  filt <- rep(1/smooth, smooth)
  rem.tot <- data.frame(numeric(0))
  iteration <- 0
  
  repeat{
    
    fish.id$PosX.ma <- filter(fish.id$PosX, filt, sides = 1)
    fish.id$PosY.ma <- filter(fish.id$PosY, filt, sides = 1)
    fish.id$PosZ.ma <- filter(fish.id$PosZ, filt, sides = 1)
    fish.id$PosX.ma <- as.numeric(fish.id$PosX.ma)
    fish.id$PosY.ma <- as.numeric(fish.id$PosY.ma)
    fish.id$PosZ.ma <- as.numeric(fish.id$PosZ.ma)
    
    rem <- subset(fish.id, !(fish.id$PosX < (fish.id$PosX.ma+thresh) & fish.id$PosX > (fish.id$PosX.ma-thresh) & fish.id$PosY < (fish.id$PosY.ma+thresh) & fish.id$PosY > (fish.id$PosY.ma-thresh) & fish.id$PosZ < (fish.id$PosZ.ma+thresh) & fish.id$PosZ > (fish.id$PosZ.ma-thresh) | is.na(fish.id$PosX.ma) == TRUE))
    fish.id <- subset(fish.id, fish.id$PosX < (fish.id$PosX.ma+thresh) & fish.id$PosX > (fish.id$PosX.ma-thresh) & fish.id$PosY < (fish.id$PosY.ma+thresh) & fish.id$PosY > (fish.id$PosY.ma-thresh) & fish.id$PosZ < (fish.id$PosZ.ma+thresh) & fish.id$PosZ > (fish.id$PosZ.ma-thresh) | is.na(fish.id$PosX.ma) == TRUE)
    
    rem.tot <- rbind(rem.tot, rem)
    iteration <- iteration+1
    
    if (nrow(rem) == 0){break}
    rem <- data.frame(numeric(0))
  }
  
  cat(paste('Iterations =', iteration, '\n', sep = ' '))
  cat(paste('obervations removed =', nrow(rem.tot), '\n', sep = ' '))
  cat(paste('observations remaining =', nrow(fish.id), '\n', sep = ' '))
  plot(rem.tot$PosX, rem.tot$PosY, xlim = c(axes[[1]], axes[[2]]), ylim = c(axes[[3]], axes[[4]]), xlab = 'Observations removed', ylab = '')
  plot(fish.id$PosX, fish.id$PosY, xlim = c(axes[[1]], axes[[2]]), ylim = c(axes[[3]], axes[[4]]), xlab = 'Observations remaining', ylab = '')
  plot(fish.id$EchoTime, fish.id$PosZ, xlab = 'Time series', type = 'l')
  
  fish.id$PosX.ma <- NULL
  fish.id$PosY.ma <- NULL
  fish.id$PosZ.ma <- NULL
  
  fish.id <<- fish.id
  
}

# 28. add single fish to dayfile after cleaning data using ma.filter

add <- function(period){
  
  dayfile <- subset(dayfile, !(dayfile$Period == period))
  dayfile <- rbind(dayfile, fish.id)
  #dayfile <- dayfile[order(dayfile$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
  #dayfile <- dayfile[order(dayfile$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
  
  dayfile <<- dayfile
  
}

# 29. function to recode fish speeds and save to dayfile after cleaning data

recode <- function(masterfileloc = "H:/Data processing/AcousticTagFile_2016.xlsx"){
  
  fishid_tbl <- readWorksheetFromFile(masterfileloc, sheet = 5, startRow = 18, endCol = 16) # read in code from Fish ID lookup table
  
  periods <- unique(dayfile$Period)
  SEC <- numeric(0)
  for(i in 1:length(periods)){
    SEC <- c(SEC, as.integer(c(NA, diff(subset(dayfile$EchoTime, dayfile$Period == periods[i]), lag = 1, differences = 1)))) # calculate time delay between pings
  }
  dayfile$SEC <- SEC
  rm(SEC)  
  
  dayfile$M <- round(c(0, sqrt(diff(dayfile$PosX)^2+diff(dayfile$PosY)^2+diff(dayfile$PosZ)^2)), digits = 3) # calculate distance between pings
  dayfile$MSEC <- round(dayfile$M/dayfile$SEC, digits = 3) # calculate swimming speed in m/sec
  dayfile$MSEC <- as.numeric(sub("Inf", "0", dayfile$MSEC)) # replace "Inf" entries
  dayfile <- subset(dayfile, !dayfile$SEC <0 | is.na(dayfile$SEC) == T) # remove negative time differences
  
  fishid.bl.lookup <- fishid_tbl$L_m # create fish ID lookup table
  names(fishid.bl.lookup) <- fishid_tbl$Period
  dayfile$BL <- as.numeric(fishid.bl.lookup[as.character(dayfile$Period)]) # add fish lengths to day file
  dayfile$BLSEC <- round(dayfile$MSEC/dayfile$BL, 3) # calculate BL per sec
  
  write.csv(dayfile, file = sub("coded.csv", "recoded.csv", dayfile.loc, ignore.case = FALSE, fixed = T)) #write output to file
  
}


# 30. batch function to subset and save data according to specified variable and factors

batch.subset <- function(variable = 'SUN', factors = c('N', 'W', 'D', 'K')) {
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  
  for (i in 1:length(files))
  {
    dayfile.loc <- files[[i]]
    dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = dayfile.classes) #c('NULL', 'numeric', 'factor', 'factor', 'POSIXct', 'double', 'double', 
    #'double', 'double', 'double', 'double', 'double', 'double', 'factor',
    #'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
    #'double', 'double', 'double', 'double', 'double', 'double', 'double',
    #'double', 'double', 'double', 'double', 'double', 'double', 'double',
    #'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
    #'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
    #'factor', 'factor', 'double', 'double', 'double', 'double', 'double', 
    #'double', 'double', 'double', 'double', 'double', 'double', 'double'
    #)) #read data into table
    
    #SORT BY TIME AND TAG
    dayfile <- dayfile[order(dayfile$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
    dayfile <- dayfile[order(dayfile$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
    
    for (j in 1:length(factors))
    {
      assign(factors[[j]], subset(dayfile, dayfile[,variable] == factors[[j]]))  
      write.csv(get(factors[[j]]), file = sub('.csv', paste0('_', factors[[j]], '.csv'), files[[i]]))
      remove(list = ls(pattern = factors[[j]])) 
    }
    
  }
  
}



# 31a. Create series of heatplots for animation

heatplot.anim <- function(pen, frames){
  
  system.time({ 
    dir.create(paste0(workingdir, '/animate'))
    setwd(paste0(workingdir, '/animate'))
    
    #frames = 24
    #pen = 7
    
    #dayfile <- dayfile[order(dayfile$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
    
    pen.col <- 'black'
    pen.size <- 0.8
    plot.col <- matlab.like(1000)  
    
    pingmax <- as.integer((as.double(max(dayfile$EchoTime))-as.double(min(dayfile$EchoTime)))/(500*5))
    
    if(pen == 7){
      pen.group <- subset(dayfile, PEN == 7)
    } else {
      pen.group <- subset(dayfile, PEN == 8)
    }
    
    minseg <- pen.group[1,'EchoTime']-seconds(1)
    
    for(i in 1:frames){
      
      # creating a name for each plot file with leading zeros
      if (i < 10) {name = paste('000',i,'plot.png',sep='')}
      
      if (i < 100 && i >= 10) {name = paste('00',i,'plot.png', sep='')}
      if (i >= 100) {name = paste('0', i,'plot.png', sep='')}
      
      # code to prepare dataset for each frame
      maxseg <- pen.group[1, 'EchoTime']+hours(i)
      
      fish.id <- subset(pen.group, EchoTime > minseg & EchoTime < maxseg)
      
      #saves the plot as a .png file in the working directory
      #png(name)
      sun <- ifelse(fish.id[1, 'SUN'] == 'N', 'Night', ifelse(fish.id[1, 'SUN'] == 'W', 'Dawn', ifelse(fish.id[1, 'SUN'] == 'K', 'Dusk', ifelse(fish.id[1,'SUN'] == 'D', 'Day', sun))))
      
      if(fish.id[1, 'PEN'] == 7){  
        
        ggplot(fish.id, aes(fish.id$PosX, fish.id$PosY)) +
          geom_hex(bins = 55, alpha = 0.6) + scale_fill_gradientn(colours=plot.col, space = 'Lab', limits = c(0, pingmax), na.value = plot.col[length(plot.col)], name = 'No. pings') +
          annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
          annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CNW', 'xmin'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CNW', 'ymax'], colour = pen.col, size = pen.size) +  # pen boundary
          annotate('segment', x = locations.lookup['7CNW', 'xmin'], xend = locations.lookup['7CNE', 'xmax'], y = locations.lookup['7CNW', 'ymax'], yend = locations.lookup['7CNE', 'ymax'], colour = pen.col, size = pen.size) + # pen boundary
          annotate('segment', x = locations.lookup['7CNE', 'xmax'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CNE', 'ymax'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
          #annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CSW', 'ymax'], yend = locations.lookup['7CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
          #annotate('segment', x = locations.lookup['7CSW', 'xmax'], xend = locations.lookup['7CNW', 'xmax'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
          #annotate('segment', x = locations.lookup['7CNW', 'xmin'], xend = locations.lookup['7CNE', 'xmax'], y = locations.lookup['7CNW', 'ymin'], yend = locations.lookup['7CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
          #annotate('segment', x = locations.lookup['7CNE', 'xmin'], xend = locations.lookup['7CSE', 'xmin'], y = locations.lookup['7CNE', 'ymax'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
          annotate('curve', x = locations.lookup['7WHNW', 'xmin']+1, xend = locations.lookup['7WHNW', 'xmax']-1, y = locations.lookup['7WHNW', 'ymin']+1, yend = locations.lookup['7WHNW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
          annotate('curve', x = locations.lookup['7WHNW', 'xmin']+1, xend = locations.lookup['7WHNW', 'xmax']-1, y = locations.lookup['7WHNW', 'ymin']+1, yend = locations.lookup['7WHNW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
          annotate('curve', x = locations.lookup['7WHSE', 'xmin']+1, xend = locations.lookup['7WHSE', 'xmax']-1, y = locations.lookup['7WHSE', 'ymin']+1, yend = locations.lookup['7WHSE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
          annotate('curve', x = locations.lookup['7WHSE', 'xmin']+1, xend = locations.lookup['7WHSE', 'xmax']-1, y = locations.lookup['7WHSE', 'ymin']+1, yend = locations.lookup['7WHSE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
          annotate('text', x = 42, y = 42, label = paste(as.character(i), 'h', sep = ' ')) + # hour count
          annotate('text', x = 42, y = 40, label = sun) + # Time of day
          theme(panel.background = element_rect(fill = 'white', colour = 'black')) + # white background, black lines
          scale_x_continuous('x (m)', limits = c(10, 45)) + scale_y_continuous('y (m)', limits = c(10,45)) # set scale limits
        
      } else {
        
        ggplot(fish.id, aes(fish.id$PosX, fish.id$PosY)) +
          geom_hex(bins = 55, alpha = 0.6) + scale_fill_gradientn(colours=plot.col, space = 'Lab', limits = c(0, pingmax), na.value = plot.col[length(plot.col)], name = 'No. pings') +
          annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, size = pen.size) +
          annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CNW', 'xmin'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CNW', 'ymax'], colour = pen.col, size = pen.size) +
          annotate('segment', x = locations.lookup['8CNW', 'xmin'], xend = locations.lookup['8CNE', 'xmax'], y = locations.lookup['8CNW', 'ymax'], yend = locations.lookup['8CNE', 'ymax'], colour = pen.col, size = pen.size) +
          annotate('segment', x = locations.lookup['8CNE', 'xmax'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CNE', 'ymax'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, size = pen.size) +
          annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CSW', 'ymax'], yend = locations.lookup['8CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) +
          annotate('segment', x = locations.lookup['8CSW', 'xmax'], xend = locations.lookup['8CNW', 'xmax'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) +
          annotate('segment', x = locations.lookup['8CNW', 'xmin'], xend = locations.lookup['8CNE', 'xmax'], y = locations.lookup['8CNW', 'ymin'], yend = locations.lookup['8CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) +
          annotate('segment', x = locations.lookup['8CNE', 'xmin'], xend = locations.lookup['8CSE', 'xmin'], y = locations.lookup['8CNE', 'ymax'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) +
          annotate('curve', x = locations.lookup['8WHSW', 'xmin']+1, xend = locations.lookup['8WHSW', 'xmax']-1, y = locations.lookup['8WHSW', 'ymin']+1, yend = locations.lookup['8WHSW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
          annotate('curve', x = locations.lookup['8WHSW', 'xmin']+1, xend = locations.lookup['8WHSW', 'xmax']-1, y = locations.lookup['8WHSW', 'ymin']+1, yend = locations.lookup['8WHSW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
          annotate('curve', x = locations.lookup['8WHNE', 'xmin']+1, xend = locations.lookup['8WHNE', 'xmax']-1, y = locations.lookup['8WHNE', 'ymin']+1, yend = locations.lookup['8WHNE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
          annotate('curve', x = locations.lookup['8WHNE', 'xmin']+1, xend = locations.lookup['8WHNE', 'xmax']-1, y = locations.lookup['8WHNE', 'ymin']+1, yend = locations.lookup['8WHNE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
          annotate('text', x = 69, y = 42, label = paste(as.character(i), 'h', sep = ' ')) + # hour count
          annotate('text', x = 69, y = 40, label = sun) + # Time of day
          theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
          scale_x_continuous('x (m)', limits = c(35,70)) + scale_y_continuous('y (m)', limits = c(10,45))  
        
      }
      
      ggsave(name)
      #write.csv(fish.id, paste0(as.character(i), '.csv'))
      
      #dev.off()
      minseg <- maxseg
    }
    
    
    setwd(workingdir)
  })
}



# 31b. Create series of individual fish plots for animation

fishplot.anim <- function(pen, frames, framedur, animdur){
  
  system.time({ 
    dir.create(paste0(workingdir, '/animate'))
    setwd(paste0(workingdir, '/animate'))
    
    dayfile <- dayfile[order(dayfile$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
    
    pen.col <- 'black'
    pen.size <- 0.8
    #fish.cols <- brewer.pal(8, 'Dark2')  
    
    #pingmax <- as.integer((as.double(max(dayfile$EchoTime))-as.double(min(dayfile$EchoTime)))/(500*5))
    
    if(pen == 7){
      pen.group <- subset(dayfile, PEN == 7)
    } else {
      pen.group <- subset(dayfile, PEN == 8)
    }
    
    fish.codes <- unique(pen.group$Period)
    
    if(length(fish.codes) < 9){
      colours <- brewer.pal(length(fish.codes), 'Dark2')  
    } else {
      colours <- c(brewer.pal(8, 'Dark2'), brewer.pal(length(fish.codes)-8, 'Set1'))  
    }
    
    colours <- sort(colours)
    
    
    minseg <- pen.group[1,'EchoTime']#-seconds(1)
    
    fish.id <- data.frame(Period = double(), PEN = factor(), EchoTime = as.POSIXct(character()), PosX = double(), PosY = double(), PosZ = double(), BLSEC = double())
    
    if(pen.group[1, 'PEN'] == 7){
      
      fish.plot <- ggplot() + #fish.id, aes(fish.id$PosX, fish.id$PosY)) +
        scale_x_continuous('x (m)', limits = c(10, 45)) + scale_y_continuous('y (m)', limits = c(10,45)) + # set scale limits      
        theme(panel.background = element_rect(fill = 'white', colour = 'black')) + # white background, black lines
        #geom_point(fish.id, aes(fish.id$PosX, fish.id$PosY)) + #scale_fill_gradientn(colours=plot.col, space = 'Lab', limits = c(0, pingmax), na.value = plot.col[length(plot.col)], name = 'No. pings') +
        annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
        annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CNW', 'xmin'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CNW', 'ymax'], colour = pen.col, size = pen.size) +  # pen boundary
        annotate('segment', x = locations.lookup['7CNW', 'xmin'], xend = locations.lookup['7CNE', 'xmax'], y = locations.lookup['7CNW', 'ymax'], yend = locations.lookup['7CNE', 'ymax'], colour = pen.col, size = pen.size) + # pen boundary
        annotate('segment', x = locations.lookup['7CNE', 'xmax'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CNE', 'ymax'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
        annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CSW', 'ymax'], yend = locations.lookup['7CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
        annotate('segment', x = locations.lookup['7CSW', 'xmax'], xend = locations.lookup['7CNW', 'xmax'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
        annotate('segment', x = locations.lookup['7CNW', 'xmin'], xend = locations.lookup['7CNE', 'xmax'], y = locations.lookup['7CNW', 'ymin'], yend = locations.lookup['7CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
        annotate('segment', x = locations.lookup['7CNE', 'xmin'], xend = locations.lookup['7CSE', 'xmin'], y = locations.lookup['7CNE', 'ymax'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
        annotate('curve', x = locations.lookup['7WHNW', 'xmin']+1, xend = locations.lookup['7WHNW', 'xmax']-1, y = locations.lookup['7WHNW', 'ymin']+1, yend = locations.lookup['7WHNW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
        annotate('curve', x = locations.lookup['7WHNW', 'xmin']+1, xend = locations.lookup['7WHNW', 'xmax']-1, y = locations.lookup['7WHNW', 'ymin']+1, yend = locations.lookup['7WHNW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
        annotate('curve', x = locations.lookup['7WHSE', 'xmin']+1, xend = locations.lookup['7WHSE', 'xmax']-1, y = locations.lookup['7WHSE', 'ymin']+1, yend = locations.lookup['7WHSE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
        annotate('curve', x = locations.lookup['7WHSE', 'xmin']+1, xend = locations.lookup['7WHSE', 'xmax']-1, y = locations.lookup['7WHSE', 'ymin']+1, yend = locations.lookup['7WHSE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
        theme(legend.position = 'none')
      #annotate('text', x = 42, y = 42, label = paste(as.character(i), 'h', sep = ' ')) + # hour count
      #annotate('text', x = 42, y = 40, label = sun) + # Time of day
      
    } else {
      
      fish.plot <- ggplot() + #fish.id, aes(fish.id$PosX, fish.id$PosY)) +
        scale_x_continuous('x (m)', limits = c(35,70)) + scale_y_continuous('y (m)', limits = c(10,45)) +      
        theme(panel.background = element_rect(fill = 'white', colour = 'black')) + # white background, black lines
        annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, size = pen.size) +
        annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CNW', 'xmin'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CNW', 'ymax'], colour = pen.col, size = pen.size) +
        annotate('segment', x = locations.lookup['8CNW', 'xmin'], xend = locations.lookup['8CNE', 'xmax'], y = locations.lookup['8CNW', 'ymax'], yend = locations.lookup['8CNE', 'ymax'], colour = pen.col, size = pen.size) +
        annotate('segment', x = locations.lookup['8CNE', 'xmax'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CNE', 'ymax'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, size = pen.size) +
        annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CSW', 'ymax'], yend = locations.lookup['8CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) +
        annotate('segment', x = locations.lookup['8CSW', 'xmax'], xend = locations.lookup['8CNW', 'xmax'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) +
        annotate('segment', x = locations.lookup['8CNW', 'xmin'], xend = locations.lookup['8CNE', 'xmax'], y = locations.lookup['8CNW', 'ymin'], yend = locations.lookup['8CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) +
        annotate('segment', x = locations.lookup['8CNE', 'xmin'], xend = locations.lookup['8CSE', 'xmin'], y = locations.lookup['8CNE', 'ymax'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) +
        annotate('curve', x = locations.lookup['8WHSW', 'xmin']+1, xend = locations.lookup['8WHSW', 'xmax']-1, y = locations.lookup['8WHSW', 'ymin']+1, yend = locations.lookup['8WHSW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
        annotate('curve', x = locations.lookup['8WHSW', 'xmin']+1, xend = locations.lookup['8WHSW', 'xmax']-1, y = locations.lookup['8WHSW', 'ymin']+1, yend = locations.lookup['8WHSW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
        annotate('curve', x = locations.lookup['8WHNE', 'xmin']+1, xend = locations.lookup['8WHNE', 'xmax']-1, y = locations.lookup['8WHNE', 'ymin']+1, yend = locations.lookup['8WHNE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
        annotate('curve', x = locations.lookup['8WHNE', 'xmin']+1, xend = locations.lookup['8WHNE', 'xmax']-1, y = locations.lookup['8WHNE', 'ymin']+1, yend = locations.lookup['8WHNE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
        theme(legend.position = 'none')
      #  annotate('text', x = 69, y = 42, label = paste(as.character(i), 'h', sep = ' ')) + # hour count
      #  annotate('text', x = 69, y = 40, label = sun) + # Time of day
      
    }
    
    
    #for(j in 1:length(fish.codes)){
    #  assign(as.character(paste0('fish_', fish.codes[[j]])), data.frame(Period = double(), PEN = factor(), EchoTime = as.POSIXct(character()), PosX = double(), PosY = double(), PosZ = double(), BLSEC = double())) 
    #}  
    
    
    for(i in 1:frames){
      
      # creating a name for each plot file with leading zeros
      if (i < 10) {name = paste('000',i,'plot.png',sep='')}
      
      if (i < 100 && i >= 10) {name = paste('00',i,'plot.png', sep='')}
      if (i >= 100) {name = paste('0', i,'plot.png', sep='')}
      
      # code to prepare dataset for each frame
      maxseg <- pen.group[1, 'EchoTime']+seconds(i*framedur)
      
      #for(k in 1:length(fish.codes)){
      
      #assign(as.character(paste0('fish_', fish.codes[[k]])), rbind(get(as.character(paste0('fish_', fish.codes[[k]]))), subset(pen.group, EchoTime >= minseg & EchoTime < maxseg & Period == as.character(fish.codes[[k]]), select=c(Period, PEN, EchoTime, PosX, PosY, PosZ, BLSEC))))
      
      
      if(animdur == 0){
        
        fish.id <- rbind(fish.id, subset(pen.group, EchoTime >= minseg & EchoTime < maxseg, select=c(Period, PEN, EchoTime, PosX, PosY, PosZ, BLSEC, SUN)))
        
      } else{
        
        fish.id <- rbind(fish.id, subset(pen.group, EchoTime >= minseg & EchoTime < maxseg, select=c(Period, PEN, EchoTime, PosX, PosY, PosZ, BLSEC, SUN)))  
        fish.id <- subset(fish.id, EchoTime >= minseg-seconds(framedur*animdur))
        
      }
      
      
      #saves the plot as a .png file in the working directory
      sun <- ifelse(fish.id[1, 'SUN'] == 'N', 'Night', ifelse(fish.id[1, 'SUN'] == 'W', 'Dawn', ifelse(fish.id[1, 'SUN'] == 'K', 'Dusk', ifelse(fish.id[1,'SUN'] == 'D', 'Day', sun))))
      
      #xinput <- paste0('fish_', as.character(fish.codes[[k]]), '$PosX')
      #yinput <- paste0('fish_', as.character(fish.codes[[k]]), '$PosY')
      #fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = TRUE, method = c("shell")),] # reverse chronological order
      #chronord <- as.factor(fish.id$EchoTime)
      
      
      if(pen.group[1, 'PEN'] == 7){
        fish.plot + geom_point(data = fish.id, aes(x = PosX, y = PosY, colour = as.factor(Period), alpha = as.factor(EchoTime)), size = 2) + scale_fill_manual(values = fish.cols) + scale_alpha_manual(values = seq(0.1, 1, length.out = nrow(fish.id))) +
          annotate('text', x = 41, y = 45, label = max(fish.id$EchoTime)) + # time stamp
          # annotate('text', x = 41, y = 43, label = '100x')    
          annotate('text', x = 41, y = 43, label = sun) # day period
      }
      
      if(pen.group[1, 'PEN'] == 8){
        fish.plot + geom_point(data = fish.id, aes(x = PosX, y = PosY, colour = as.factor(Period), alpha = as.factor(EchoTime)), size = 2) + scale_fill_manual(values = fish.cols) + scale_alpha_manual(values = seq(0.1, 1, length.out = nrow(fish.id))) +
          annotate('text', x = 66, y = 45, label = max(fish.id$EchoTime)) + # time stamp
          annotate('text', x = 66, y = 43, label = sun) # day period  
        
      }
      
      
      
      #fish.plot + geom_point(aes(x = fish.id$PosX, y = fish.id$PosY, colour = factor(fish.id$Period)))  + scale_alpha_discrete(range = c(1, 0.2))
      #fish.plot <- fish.plot + geom_point(aes(x = eval(parse(text = xinput)), y = eval(parse(text = yinput)), colour = fish.cols[[1]]))
      
      
      
      #}
      
      #print(fish.plot)
      
      ggsave(name)
      
      minseg <- maxseg
    }
    
    
    setwd(workingdir)
  })
}



# 32. draw histogram of fish depth or activity from fish files

fish.hist <- function(pt){
  
  if(pt == 'depth'){plot.type <- 'PosZ'}
  if(pt == 'activity'){plot.type <- 'BLSEC'}  
  
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE) 
  
  if(length(files) < 13){
    colours <- brewer.pal(length(files), 'Set3')  
  } else {
    colours <- c(brewer.pal(12, 'Set3'), brewer.pal(length(files)-12, 'Set1'))  
  }
  
  colours <- sort(colours)
  
  fish.codes <- substr(files, 15, 18)
  
  for(i in 1: length(files)) {
    
    assign(paste0('dayfile', as.character(i)), read.csv(files[[i]], header = TRUE, sep = ",", colClasses = c('NULL', 'numeric', 'factor', 'factor', 'POSIXct', 'double', 'double',
                                                                                                             'double', 'double', 'double', 'double', 'double', 'double', 'double', 'double', 'double', 'factor',
                                                                                                             'factor', 'factor', 'factor', 'factor', 'double', 'double', 'double',
                                                                                                             'double', 'double', 'double', 'double', 'double', 'double', 'double',
                                                                                                             'double', 'double', 'double', 'double', 'double', 'double', 'double',
                                                                                                             'double', 'double', 'double', 'double', 'factor', 'factor', 'factor',
                                                                                                             'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                                                                                                             'factor', 'factor', 'factor', 'double', 'double', 'double', 'double',
                                                                                                             'double', 'double', 'double', 'double', 'double', 'double', 'double', 'double'
    ))) #read data into table
    
    if(pt == 'activity'){
      assign(paste0('dayfile', as.character(i)), subset(get(paste0('dayfile', (i))), BLSEC < 5 & BLSEC >= 0 ))
      
    }
    
    #assign('dayfile1', subset(dayfile1, BLSEC < 10))  
    
  }
  
  
  
  hdep <- ggplot()
  
  for(j in 1: length(files)){
    
    # hdep <- print(hdep + geom_freqpoly(data = get(paste0('dayfile', as.character(j))), binwidth = 0.3, aes(get(paste0('dayfile', as.character(j)))[,'PosZ'])))
    loop_input = paste0('geom_freqpoly(data = dayfile', as.character(j), ', binwidth = 0.3, size = 1, aes(dayfile', as.character(j), '$', plot.type, ', color = colours[[', (j), ']]))')
    hdep <- hdep + eval(parse(text = loop_input))
    
  }
  
  hdep <- hdep + theme(panel.background = element_rect(fill = 'white', colour = 'black'))
  hdep <- hdep + scale_colour_manual('Fish ID', labels = fish.codes, values = colours)
  if(pt == 'depth'){
    hdep <- hdep + labs(x = 'Depth (m)', colour = 'fish ID') + scale_y_continuous(limits = c(0, 40000))
    hdep <- hdep + coord_flip() + scale_x_reverse()
  }
  if(pt == 'activity'){
    hdep <- hdep + labs(x = 'Activity (BL/s)', colour = 'fish ID') + scale_y_continuous(limits = c(0, 120000))
  }
  
  print(hdep)
  hdep <<- hdep
  
  
}


# 33. Load all data into single data frame

load.all <- function(){
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  
  dayfile <- data.frame()
  
  for(i in 1:length(files)){
    
    daytemp <- read.csv(files[[i]], header = TRUE, sep = ",", colClasses = dayfile.classes)#c('NULL', 'numeric', 'factor', 'factor', 'POSIXct', 'double', 'double', 
    #'double', 'double', 'double', 'double', 'double', 'double', 'factor',
    #'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
    #'double', 'double', 'double', 'double', 'double', 'double', 'double',
    #'double', 'double', 'double', 'double', 'double', 'double', 'double',
    #'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
    #'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
    #'factor', 'factor', 'double', 'double', 'double', 'double', 'double', 
    #'double', 'double', 'double', 'double', 'double', 'double', 'double'
    
    #)) #read data into table
    
    dayfile <- rbind(dayfile, daytemp)
    
  }
  
  
  #SORT BY TIME AND TAG
  dayfile <- dayfile[order(dayfile$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
  dayfile <- dayfile[order(dayfile$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
  
  dayfile <<- dayfile
  
}


#34. Crop edges of dataset to remove multipath

crop <- function(xmin = 30, xmax = 64, ymin = 7, ymax = 42){
  
  dayfile <- subset(dayfile, dayfile$PosY > ymin & dayfile$PosY < ymax & dayfile$PosX > xmin & dayfile$PosX < xmax)
  
  dayfile <<- dayfile
  
}



#35. Save loaded dayfile to .csv file of original name

save <- function(){
  
  write.csv(dayfile, file = dayfile.loc) #write output to file
  
}


#36. calculate distance travelled for each fish in dayfile

distance <- function(){
  
  fish.codes <- unique(dayfile$Period) 
  
  total.dist <- as.data.frame(setNames(replicate(2, numeric(0), simplify = F), c('Fish_ID', 'distance_m')))
  
  for (i in 1:length(fish.codes)){
    
    total.dist[i,] <- c(fish.codes[i], round(sum(dayfile[dayfile$Period == fish.codes[[i]],]$M), 1))
    
  }
  total.dist$distance_m <- as.double(total.dist$distance_m)
  ggplot(total.dist, aes(ID, distance_m)) + geom_bar(stat = 'identity') + scale_x_discrete('fish ID') + scale_y_continuous('distance (m)')
  total.dist
  
}

#37. calculate distance travelled in multiple fish files

batch.dist <- function(){
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  total.dist <- as.data.frame(setNames(replicate(2, numeric(0), simplify = F), c('Fish_ID', 'distance_km')))
  
  for(i in 1:length(files)){
    
    dayfile <- read.csv(files[[i]], header = TRUE, sep = ",", colClasses = c('NULL', 'numeric', 'factor', 'factor', 'POSIXct', 'double', 'double',
                                                                             'double', 'double', 'double', 'double', 'double', 'double', 'double', 'double', 'double', 'factor',
                                                                             'factor', 'factor', 'factor', 'factor', 'double', 'double', 'double',
                                                                             'double', 'double', 'double', 'double', 'double', 'double', 'double',
                                                                             'double', 'double', 'double', 'double', 'double', 'double', 'double',
                                                                             'double', 'double', 'double', 'double', 'factor', 'factor', 'factor',
                                                                             'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                                                                             'factor', 'factor', 'factor', 'double', 'double', 'double', 'double',
                                                                             'double', 'double', 'double', 'double', 'double', 'double', 'double', 'double'
    )) #read data into table
    
    fish.codes <- unique(dayfile$Period) 
    total.dist[i,] <- c(fish.codes[1], (round(sum(dayfile[dayfile$Period == fish.codes[[1]],]$M), 2)/1000))
    
    
  }
  
  total.dist$distance_km <- as.double(total.dist$distance_km)
  total.dist$Fish_ID <- as.character(total.dist$Fish_ID)
  distplot <- ggplot(total.dist, aes(Fish_ID, distance_km)) + geom_bar(stat = 'identity') + scale_x_discrete('fish ID') + scale_y_continuous('distance (km)', expand = c(0, 0))
  total.dist  <<- total.dist
  print(distplot)
  return(distplot)
  #distplot <<- distplot
  
  
}


# 38. Load dayfile

load.dayfile <- function(filename){
  
  setwd(workingdir)  
  dayfile <- read.csv(filename, header = TRUE, sep = ",", colClasses = dayfile.classes)  
  
  dayfile <<- dayfile
  
  
}  


# 39. Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# 40. Polar plots of headings

headplot <- function(threshold = 0.1){
  
  p12 <- subset(dayfile, PEN == 12 & MSEC >= threshold)
  p14 <- subset(dayfile, PEN == 14 & MSEC >= threshold)
  p15 <- subset(dayfile, PEN == 15 & MSEC >= threshold)
  
  pplot12 <- ggplot(p12, aes(HEAD))
  pplot12 <- pplot12 + geom_histogram(breaks = seq(0, 360, 10), color = 'black', alpha = 0, size = 0.75, closed = 'left') + 
    theme_minimal() + theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
    scale_x_continuous('', limits = c(0, 360), expand = c(0, 0), breaks = c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330)) +
    #scale_y_continuous(limits = c(0, 1500)) +
    coord_polar(theta = 'x', start = 0) +
    ggtitle('P12 Lumpfish') + theme(plot.title = element_text(hjust = 0.5))
  
  pplot14 <- ggplot(p14, aes(HEAD))
  pplot14 <- pplot14 + geom_histogram(breaks = seq(0, 360, 10), color = 'black', alpha = 0, size = 0.75) + 
    theme_minimal() + theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
    scale_x_continuous('', limits = c(0, 360), breaks = c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330)) +
    # scale_y_continuous(limits = c(0, 1500)) +
    coord_polar(theta = 'x', start = 0) +
    ggtitle('P14 Lumpfish') + theme(plot.title = element_text(hjust = 0.5))
  
  pplot15 <- ggplot(p15, aes(HEAD))
  pplot15 <- pplot15 + geom_histogram(breaks = seq(0, 360, 10), color = 'black', alpha = 0, size = 0.75) + 
    theme_minimal() + theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
    scale_x_continuous('', limits = c(0, 360), breaks = c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330)) +
    # scale_y_continuous(limits = c(0, 1500)) +
    coord_polar(theta = 'x', start = 0) +
    ggtitle('P15 Lumpfish') + theme(plot.title = element_text(hjust = 0.5))
  
  multiplot(pplot12, pplot14, pplot15, cols = 3) #col is number of columns to plot in a row (e.g. if 4 graphs, cols=2 plots 2x2; if 2 graphs, cols=2 plots 2x1 (colxrow))
  
}


# 41. Polar plots of turn rates

turnplot <- function(){
  
  p12 <- subset(dayfile, PEN == 12)
  p12$TURNRATE <- p12$TURN/p12$SEC
  p14 <- subset(dayfile, PEN == 14)
  p14$TURNRATE <- p14$TURN/p14$SEC
  p15 <- subset(dayfile, PEN == 15)
  p15$TURNRATE <- p14$TURN/p14$SEC
  
  pplot12 <- ggplot(p12, aes(TURNRATE))
  pplot12 <- pplot12 + geom_histogram(breaks = seq(0, 30, 1), color = 'black', alpha = 0, size = 0.75, closed = 'left') + 
    theme_minimal() + theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
    scale_x_continuous('', limits = c(0, 30), expand = c(0, 0), breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 330)) +
    #scale_y_continuous(limits = c(0, 1500)) +
    coord_polar(theta = 'x', start = 0) +
    ggtitle('Pen 12') + theme(plot.title = element_text(hjust = 0.5))
  
  pplot14 <- ggplot(p14, aes(TURNRATE))
  pplot14 <- pplot14 + geom_histogram(breaks = seq(0, 30, 1), color = 'black', alpha = 0, size = 0.75) + 
    theme_minimal() + theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
    scale_x_continuous('', limits = c(0, 30), expand = c(0, 0), breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 330)) +
    # scale_y_continuous(limits = c(0, 1500)) +
    coord_polar(theta = 'x', start = 0) +
    ggtitle('Pen 14') + theme(plot.title = element_text(hjust = 0.5))
  
  pplot15 <- ggplot(p15, aes(TURNRATE))
  pplot15 <- pplot15 + geom_histogram(breaks = seq(0, 30, 1), color = 'black', alpha = 0, size = 0.75) + 
    theme_minimal() + theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
    scale_x_continuous('', limits = c(0, 30), expand = c(0, 0), breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 330)) +
    # scale_y_continuous(limits = c(0, 1500)) +
    coord_polar(theta = 'x', start = 0) +
    ggtitle('Pen 15') + theme(plot.title = element_text(hjust = 0.5))
  
  multiplot(pplot12, pplot14, pplot15, cols = 3)
  
}



# 42.  draw turn / velocity plots for every step specified

bplot <- function(period, step = 100){
  
  daytemp <- subset(dayfile, Period == period)
  start <- step-step
  end <- step
  
  f5 <- rep(1/5, 5) # 5 step moving average filter
  f10 <- rep(1/10, 10) # 5 step moving average filter
  
  for (i in 1:floor(nrow(daytemp)/step)){
    
    sect <- daytemp[start:end,] # subset dayfile
    
    #par(mfrow=c(2,2))
    layout(matrix(c(1,2,3,3), 2, 2,byrow = T))
    par(new=F)
    par(mar = c(4, 4, 4, 4))# + 0.1)
    fishpal <- rainbow_hcl(24, c=100, l=63, start=-360, end=-32, alpha = 0.2)
    fish.id <- subset(dayfile, Period == period)
    
    # position plot
    
    if(fish.id[1,3] == '12')
    {
      
      plot(sect$PosX, sect$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(37, 72), ylim = c(37, 72), type = 'l', col = '#26b426') # tight plot
      rect(locations.lookup['12EW', 'xmin'], locations.lookup['12EW', 'ymin'], locations.lookup['12EW', 'xmax'], locations.lookup['12EW', 'ymax'], lty = 2) # 12EW edge
      rect(locations.lookup['12ES', 'xmin'], locations.lookup['12ES', 'ymin'], locations.lookup['12ES', 'xmax'], locations.lookup['12ES', 'ymax'], lty = 2) # 12ES edge
      rect(locations.lookup['12EE', 'xmin'], locations.lookup['12EE', 'ymin'], locations.lookup['12EE', 'xmax'], locations.lookup['12EE', 'ymax'], lty = 2) # 12EE edge
      rect(locations.lookup['12EN', 'xmin'], locations.lookup['12EN', 'ymin'], locations.lookup['12EN', 'xmax'], locations.lookup['12EN', 'ymax'], lty = 2) # 12EN edge
      rect(locations.lookup['12HET', 'xmin'], locations.lookup['12HET', 'ymin'], locations.lookup['12HET', 'xmax'], locations.lookup['12HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HET
      rect(locations.lookup['12HEB', 'xmin'], locations.lookup['12HEB', 'ymin'], locations.lookup['12HEB', 'xmax'], locations.lookup['12HEB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HEB
      rect(locations.lookup['12HWT', 'xmin'], locations.lookup['12HWT', 'ymin'], locations.lookup['12HWT', 'xmax'], locations.lookup['12HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HWT
      rect(locations.lookup['12HWB', 'xmin'], locations.lookup['12HWB', 'ymin'], locations.lookup['12HWB', 'xmax'], locations.lookup['12HWB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HWB
      rect(locations.lookup['FS12', 'xmin'], locations.lookup['FS12', 'ymin'], locations.lookup['FS12', 'xmax'], locations.lookup['FS12', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 12FS
      rect(locations.lookup['12EW', 'xmin'], locations.lookup['12ES', 'ymin'], locations.lookup['12EE', 'xmax'], locations.lookup['12EN', 'ymax'], lwd = 2) # cage limits
      
      text(37, 45, adj = c(0, 1), label = paste0('Salmon feeding: ', sect[1,'SMEAL12'], '\nBiofouling: ', sect[1, 'BIOF12'], '\nSun: ', sect[1,'SUN'], '\nTide: ', sect[1, 'TID']), cex = 1) 
      text(73, 45, adj = c(1, 1), label = paste0(sect[1, 'EchoTime'], ' to ', sect[nrow(sect), 'EchoTime'], '\n', start, ' - ', end))
      
    }else{
      if(fish.id[1,3] == '14'){
        
        plot(sect$PosX, sect$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(10, 45), ylim = c(37, 72), type = 'l', col = '#26b426') # tight plot
        rect(locations.lookup['14EW', 'xmin'], locations.lookup['14EW', 'ymin'], locations.lookup['14EW', 'xmax'], locations.lookup['14EW', 'ymax'], lty = 2) # 14EW edge
        rect(locations.lookup['14ES', 'xmin'], locations.lookup['14ES', 'ymin'], locations.lookup['14ES', 'xmax'], locations.lookup['14ES', 'ymax'], lty = 2) # 14ES edge
        rect(locations.lookup['14EE', 'xmin'], locations.lookup['14EE', 'ymin'], locations.lookup['14EE', 'xmax'], locations.lookup['14EE', 'ymax'], lty = 2) # 14EE edge
        rect(locations.lookup['14EN', 'xmin'], locations.lookup['14EN', 'ymin'], locations.lookup['14EN', 'xmax'], locations.lookup['14EN', 'ymax'], lty = 2) # 14EN edge
        rect(locations.lookup['14HET', 'xmin'], locations.lookup['14HET', 'ymin'], locations.lookup['14HET', 'xmax'], locations.lookup['14HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HET
        rect(locations.lookup['14HEB', 'xmin'], locations.lookup['14HEB', 'ymin'], locations.lookup['14HEB', 'xmax'], locations.lookup['14HEB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HEB
        rect(locations.lookup['14HWT', 'xmin'], locations.lookup['14HWT', 'ymin'], locations.lookup['14HWT', 'xmax'], locations.lookup['14HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HWT
        rect(locations.lookup['14HWB', 'xmin'], locations.lookup['14HWB', 'ymin'], locations.lookup['14HWB', 'xmax'], locations.lookup['14HWB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HWB
        rect(locations.lookup['FS14', 'xmin'], locations.lookup['FS14', 'ymin'], locations.lookup['FS14', 'xmax'], locations.lookup['FS14', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 14FS
        rect(locations.lookup['14EW', 'xmin'], locations.lookup['14ES', 'ymin'], locations.lookup['14EE', 'xmax'], locations.lookup['14EN', 'ymax'], lwd = 2) # cage limits
        
        text(37, 45, adj = c(0, 1), label = paste0('Salmon feeding: ', sect[1,'SMEAL14'], '\nBiofouling: ', sect[1, 'BIOF14'], '\nSun: ', sect[1,'SUN'], '\nTide: ', sect[1, 'TID']), cex = 1) 
        text(73, 45, adj = c(1, 1), label = paste0(sect[1, 'EchoTime'], ' to ', sect[nrow(sect), 'EchoTime'], '\n', start, ' - ', end))
        
      }else{
        if(fish.id[1,3] == '15'){
          
          plot(sect$PosX, sect$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(10, 45), ylim = c(10, 45), type = 'l', col = '#26b426') # tight plot
          rect(locations.lookup['15EW', 'xmin'], locations.lookup['15EW', 'ymin'], locations.lookup['15EW', 'xmax'], locations.lookup['15EW', 'ymax'], lty = 2) # 15EW edge
          rect(locations.lookup['15ES', 'xmin'], locations.lookup['15ES', 'ymin'], locations.lookup['15ES', 'xmax'], locations.lookup['15ES', 'ymax'], lty = 2) # 15ES edge
          rect(locations.lookup['15EE', 'xmin'], locations.lookup['15EE', 'ymin'], locations.lookup['15EE', 'xmax'], locations.lookup['15EE', 'ymax'], lty = 2) # 15EE edge
          rect(locations.lookup['15EN', 'xmin'], locations.lookup['15EN', 'ymin'], locations.lookup['15EN', 'xmax'], locations.lookup['15EN', 'ymax'], lty = 2) # 15EN edge
          rect(locations.lookup['15HET', 'xmin'], locations.lookup['15HET', 'ymin'], locations.lookup['15HET', 'xmax'], locations.lookup['15HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HET
          rect(locations.lookup['15HEB', 'xmin'], locations.lookup['15HEB', 'ymin'], locations.lookup['15HEB', 'xmax'], locations.lookup['15HEB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HEB
          rect(locations.lookup['15HWT', 'xmin'], locations.lookup['15HWT', 'ymin'], locations.lookup['15HWT', 'xmax'], locations.lookup['15HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HWT
          rect(locations.lookup['15HWB', 'xmin'], locations.lookup['15HWB', 'ymin'], locations.lookup['15HWB', 'xmax'], locations.lookup['15HWB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HWB
          rect(locations.lookup['FS15', 'xmin'], locations.lookup['FS15', 'ymin'], locations.lookup['FS15', 'xmax'], locations.lookup['FS15', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 15FS
          rect(locations.lookup['15EW', 'xmin'], locations.lookup['15ES', 'ymin'], locations.lookup['15EE', 'xmax'], locations.lookup['15EN', 'ymax'], lwd = 2) # cage limits
          
          text(10, 45, adj = c(0, 1), label = paste0('Salmon feeding: ', sect[1,'SMEAL15'], '\nBiofouling: ', sect[1, 'BIOF15'], '\nSun: ', sect[1,'SUN'], '\nTide: ', sect[1, 'TID']), cex = 1) 
          text(45, 45, adj = c(1, 1), label = paste0(sect[1, 'EchoTime'], ' to ', sect[nrow(sect), 'EchoTime'], '\n', start, ' - ', end))
          
        }
      }
      
    }    
    
    
    #depth plot
    
    plot(sect$EchoTime, sect$PosZ, xlab = 'Time', ylab = 'Depth (m)', ylim = c(35, 0), type = 'l', col = '#26b426')
    segments(sect[1,4], 15, sect[nrow(fish.id), 4], 15, lty = 2)
    legend('bottomleft', as.character(period), col = '#26b426', pch = 20, bty = 'n', pt.cex = 1.5, horiz = TRUE, y.intersp = 0)
    
    #turn/velocity plots
    
    par(mar = c(4, 6, 2, 4))# + 0.1)
    plot(sect$EchoTime, sect$TURN, xlab = 'Time', type = 'l', lwd = 2, col = 'lightgreen', ylab = '', yaxt = 'n', ylim = c(0, 180)) # plot turn
    axis(2, ylim = c(0, 180), at = c(0, 30, 60, 90, 120, 150, 180), labels = c('0', '30', '60', '90', '120', '150', '180'))
    #turnlag <- filter(sect$TURN, f5, sides=1) # filter turn
    #lines(sect$EchoTime, turnlag, col = 'darkgreen') # add moving average to plot
    
    #par(new = T)
    #plot(sect$EchoTime, sect$rollturn, xlab = 'Time', type = 'l', lwd = 2, col = 'lightblue', ylab = '', yaxt = 'n', ylim = c(0, 1500)) # plot turn
    #axis(4, ylim = c(0, 1500), at = c(0, 500, 1000, 1500), labels = c('0', '500', '1000', '1500'))
    #mtext(text = 'x10 rolling average of turn', side = 4, line = 2.5)
    
    par(new = T)
    #plot(sect$EchoTime, sect$HEAD, xlab = 'Time', type = 'l', lwd = 2, col = 'lightblue', ylab = '', yaxt = 'n', ylim = c(0, 360)) # plot turn
    axis(2, ylim = c(0, 360), line = 2, at = c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330, 360), labels = c('0', '30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330', '360'))
    mtext(2, text = 'Turn/heading (degrees)', line = 4.5)
    
    par(new = T)
    plot(sect$EchoTime, sect$MSEC, col = 'green', axes = F, xlab = '', ylab = '', type = 'l', lwd = 2, ylim = c(0, 0.8))
    axis(4, ylim = c(0, 1), line = 2, at = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8), labels = c('0', '0.1', '0.2', '0.3', '0.4', '0.5', '0.6', '0.7', '0.8'))
    #mtext(text = 'velocity (m/sec)', side = 4, line = 3.5)
    legend('topleft', legend = c('Turn', 'Velocity', 'Heading'), lty = 1, lwd = 2, col = c('lightgreen', 'red', 'lightblue'), horiz = T)
    vellag <- filter(sect$MSEC, f5, sides=1) # filter turn
    lines(sect$EchoTime, vellag, col = 'darkgreen') # add moving average to plot
    
    par(new = T)
    plot(sect$EchoTime, sect$BLSEC, col = 'red', axes = F, xlab = '', ylab = '', type = 'l', lwd = 2, ylim = c(0, 6))
    axis(4, ylim = c(0, 6), at = c(0, 1, 2, 3, 4, 5, 6), labels = c('0', '1', '2', '3', '4', '5', '6'))
    #mtext(text = 'velocity (BL/sec)', side = 4, line = 2.5)
    vellag <- filter(sect$BLSEC, f5, sides=1) # filter turn
    lines(sect$EchoTime, vellag, col = 'pink') # add moving average to plot
    
    legend('topleft', legend = c('Turn', 'Velocity', 'Heading'), lty = 1, lwd = 2, col = c('lightgreen', 'red', 'lightblue'), horiz = T)
    
    par(new = F)
    
    start <- start+step
    end <- end+step
    
    readline(prompt = 'Press [enter] to continue')
    
  }
  par(mfrow=c(1,1))
}


# 43. Perform behaviour calculations for loaded dayfile and add to dayfile

bcalc <- function(){
  
  #calculate difference in turn and 10 width rolling sum of turn
  dayfile$turndiff <- c(NA, abs(diff(dayfile$TURN, lag = 1)))
  dayfile$rollturnsumpersec <- c(rep(NA,4), rollapply(dayfile$turndiff, width = 10, FUN = sum, na.rm = T, align = 'center')/rollapply(dayfile$SEC, width = 10, FUN = sum, na.rm = T, align = 'center'), rep(NA, 5))
  
  # Displacement code
  
  # calculate rolling mean of x,y,z coords over 20 points
  dayfile$rollx <- c(rep(NA,19), rollapply(dayfile$PosX, width = 20, FUN = mean, na.rm = T, align = 'right'))#, rep(NA, 10))
  dayfile$rolly <- c(rep(NA,19), rollapply(dayfile$PosY, width = 20, FUN = mean, na.rm = T, align = 'right'))#, rep(NA, 10))
  dayfile$rollz <- c(rep(NA,19), rollapply(dayfile$PosZ, width = 20, FUN = mean, na.rm = T, align = 'right'))#, rep(NA, 10))
  
  # calculate rolling sum of time between pings over 20 points
  dayfile$rollsec <- c(rep(NA,19), rollapply(dayfile$SEC, width = 20, FUN = sum, na.rm = T, align = 'right'))#, rep(NA, 10))
  
  #calculate displacement
  dayfile$displace <- round(sqrt(abs(dayfile$PosX-dayfile$rollx)^2+abs(dayfile$PosY-dayfile$rolly)^2+abs(dayfile$PosZ-dayfile$rollz)^2)/dayfile$rollsec, digits = 3)
  
  # calculate rolling mean of velocity/sec
  dayfile$rollvel <- c(rep(NA,9), rollapply(dayfile$M, width = 10, FUN = sum, na.rm = T, align = 'right')/rollapply(dayfile$SEC, width = 10, FUN = sum, na.rm = T, align = 'center'))
  
  # calculate instantanous acceleration
  dayfile$acc <- c(NA, abs(diff(dayfile$MSEC, lag = 1)))
  
  #calculate acceleration mean over 10 points
  dayfile$accmean <- c(rep(NA,4), rollapply(dayfile$acc, width = 10, FUN = mean, na.rm = T, align = 'center'), rep(NA, 5)) # acceleration mean over 10 points
  
  dayfile <<- dayfile
  
}


# 44. calculate behaviour state for all dayfiles in working directory and save to dayfiles


batch.bscalc <- function(){
  
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  
  for(i in 1:length(files)){
    
    #day <- substr(files[[i]], 15, 17)
    dayfile <- read.csv(files[[i]], header = TRUE, sep = ",", colClasses = dayfile.classes)  
    
    #calculate difference in turn and 10 width rolling sum of turn
    dayfile$turndiff <- c(NA, abs(diff(dayfile$TURN, lag = 1)))
    dayfile$rollturnsumpersec <- c(rep(NA,4), rollapply(dayfile$turndiff, width = 10, FUN = sum, na.rm = T, align = 'center')/rollapply(dayfile$SEC, width = 10, FUN = sum, na.rm = T, align = 'center'), rep(NA, 5))
    
    # calculate rolling mean of x,y,z coords over 20 points
    dayfile$rollx <- c(rep(NA,19), rollapply(dayfile$PosX, width = 20, FUN = mean, na.rm = T, align = 'right'))#, rep(NA, 10))
    dayfile$rolly <- c(rep(NA,19), rollapply(dayfile$PosY, width = 20, FUN = mean, na.rm = T, align = 'right'))#, rep(NA, 10))
    dayfile$rollz <- c(rep(NA,19), rollapply(dayfile$PosZ, width = 20, FUN = mean, na.rm = T, align = 'right'))#, rep(NA, 10))
    
    # calculate rolling sum of time between pings over 20 points
    dayfile$rollsec <- c(rep(NA,19), rollapply(dayfile$SEC, width = 20, FUN = sum, na.rm = T, align = 'right'))#, rep(NA, 10))
    
    #calculate displacement
    dayfile$displace <- round(sqrt(abs(dayfile$PosX-dayfile$rollx)^2+abs(dayfile$PosY-dayfile$rolly)^2+abs(dayfile$PosZ-dayfile$rollz)^2)/dayfile$rollsec, digits = 3)
    
    # calculate rolling mean of velocity/sec
    #dayfile$rollvel <- c(rep(NA,9), rollapply(dayfile$M, width = 10, FUN = mean, na.rm = T, align = 'right')/rollapply(dayfile$SEC, width = 10, FUN = mean, na.rm = T, align = 'right'))
    
    # calculate rolling mean of BL/sec
    dayfile$rollvel <- c(rep(NA,9), (rollapply(dayfile$M, width = 10, FUN = mean, na.rm = T, align = 'right')/rollapply(dayfile$SEC, width = 10, FUN = mean, na.rm = T, align = 'right'))/rollapply(dayfile$BL, width = 10, FUN = mean, na.rm = T, align = 'right'))
    
    
    # calculate instantanous acceleration
    #dayfile$acc <- c(NA, abs(diff(dayfile$MSEC, lag = 1)))
    
    #calculate acceleration mean over 10 points
    #dayfile$accmean <- c(rep(NA,4), rollapply(dayfile$acc, width = 10, FUN = mean, na.rm = T, align = 'center'), rep(NA, 5)) # acceleration mean over 10 points
    
    # code behaviour state for each position    
    #dayfile$BS <- ifelse(dayfile$displace <= 0.015, ifelse(dayfile$rollvel <= 0.02, 'Rr', 'Ra'), ifelse(dayfile$accmean <= 0.05, 'C', ifelse(dayfile$rollvel <= 0.1, 'F', 'A')))
    
    #alternative behaviour state coding
    #dayfile$BS <- ifelse(dayfile$displace <= 0.015, ifelse(dayfile$rollvel <= 0.02, 'Rr', 'Ra'), ifelse(dayfile$rollturnsumpersec <= 4, 'C', ifelse(dayfile$rollvel <= 0.1, 'F', 'A')))
    
    # another alternative behaviour state coding
    #dayfile$BS <- ifelse(dayfile$displace <= 0.015, ifelse(dayfile$rollvel <= 0.02, 'Rr', ifelse(dayfile$rollvel >0.02 & dayfile$rollvel <=0.1, 'Rf', 'Ra')), ifelse(dayfile$rollturnsumpersec <= 4, 'Ep', ifelse(dayfile$rollvel <= 0.1, 'Ef', 'Ea')))
    
    # alternative behaviour state coding based on BL/SEC
    dayfile$BS <- ifelse(dayfile$displace <= 0.015, ifelse(dayfile$rollvel <= 0.15, 'Rr', ifelse(dayfile$rollvel >0.15 & dayfile$rollvel <=0.8, 'Rf', 'Ra')), ifelse(dayfile$rollturnsumpersec <= 4, 'Ep', ifelse(dayfile$rollvel <= 0.8, 'Ef', 'Ea')))
    
    
    dayfile$turndiff <- NULL
    dayfile$rollturnsumpersec <- NULL
    dayfile$rollx <- NULL
    dayfile$rolly <- NULL
    dayfile$rollz <- NULL
    dayfile$rollsec <- NULL
    dayfile$displace <- NULL
    dayfile$rollvel <- NULL
    dayfile$acc <- NULL
    dayfile$accmean <- NULL
    
    write.csv(dayfile, file = files[[i]]) #write output to file
    
  }
  
}



# 45. batch.bsprop() = calculate proportions of behaviour states for each dayfile in working directory

batch.bsprop <- function(){
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  bsproptab <- data.frame(c('Ea', 'Ef', 'Ep', 'Ra', 'Rf', 'Rr'))
  colnames(bsproptab) <- 'ID'
  
  for(i in 1:length(files)){
    
    dayfile <- read.csv(files[[i]], header = TRUE, sep = ",", colClasses = dayfile.classes)    
    
    #bstab <- table(dayfile$BS)
    daysub <- subset(dayfile, SEC < 600) # keep only records where gap between signals is less than 10 mins (600s)
    bstab <- aggregate(x = daysub$SEC, by = list(daysub$BS), FUN = 'sum', na.rm = T)
    
    bsproptab[,as.character(i)] <- as.vector(bstab$x)
    
  } 
  
  write.xlsx(bsproptab, 'bsproportions.xlsx')
  
}


# 46. Calculate kernel distribution utilisation for single fish file

kudcalc <- function(){
  
  #kudcols <- c(brewer.pal(4, 'Accent')[[1]], brewer.pal(4, 'Accent')[[2]]) # create colour palette for KUDs
  kudcols <- terrain.colors(6, alpha = 0.6)
  
  if(unique(dayfile$PEN == 12)){
    
    x <- seq(25, 70, by = 0.5)
    y <- seq(25, 70, by = 0.5)
    xy <- expand.grid(x=x, y=y)
    coordinates(xy) <- ~x+y
    gridded(xy) <- TRUE
    class(xy)
    
  } else {
    if(unique(dayfile$PEN == 14)){
      
      x <- seq(0, 50, by = 0.5)
      y <- seq(25, 70, by = 0.5)
      xy <- expand.grid(x=x, y=y)
      coordinates(xy) <- ~x+y
      gridded(xy) <- TRUE
      class(xy)  
      
    }else{
      
      if(unique(dayfile$PEN == 15)){
        
        x <- seq(0, 50, by = 0.5)
        y <- seq(0, 50, by = 0.5)
        xy <- expand.grid(x=x, y=y)
        coordinates(xy) <- ~x+y
        gridded(xy) <- TRUE
        class(xy)    
      }
    }
    
  }
  
  coords <- dayfile[,c(1, 5, 6)] # extract x,y coords and fish id from dayfile
  coordinates(coords) <- c('PosX', 'PosY') # convert to spatial points data frame object
  ud <- kernelUD(coords, h = 'href', grid = xy, kern = 'bivnorm') # KUD calculation for adehabitatHR package
  
  #mcp100 <- mcp(coords, percent = 100)
  ver50 <- getverticeshr(ud, 50) # extract 50% vertex for plotting
  ver95 <- getverticeshr(ud, 95) # extract 95% vertex for plotting
  #plot(mcp100, col = NULL, axes = T, xlim = c(10, 45), ylim = c(0, 45)) # plot MCP100
  ka <- kernel.area(ud, percent = c(50, 95), unin = 'm', unout = 'm2') # calculates area of KUD50, KUD95 and MCP100
  
  if (unique(dayfile$PEN == 12)){
    
    plot(ver95, col = kudcols[[1]], axes = T, xlim = c(35, 70), ylim = c(35, 70), xlab = 'x (m)', ylab = 'y (m)') # plot KUD95
    plot(ver50, col = kudcols[[4]], axes = F, xlim = c(35, 70), ylim = c(35, 70), add=T) # plot KUD50
    
    rect(locations.lookup['12EW', 'xmin'], locations.lookup['12EW', 'ymin'], locations.lookup['12EW', 'xmax'], locations.lookup['12EW', 'ymax'], lty = 2) # 12EW edge
    rect(locations.lookup['12ES', 'xmin'], locations.lookup['12ES', 'ymin'], locations.lookup['12ES', 'xmax'], locations.lookup['12ES', 'ymax'], lty = 2) # 12ES edge
    rect(locations.lookup['12EE', 'xmin'], locations.lookup['12EE', 'ymin'], locations.lookup['12EE', 'xmax'], locations.lookup['12EE', 'ymax'], lty = 2) # 12EE edge
    rect(locations.lookup['12EN', 'xmin'], locations.lookup['12EN', 'ymin'], locations.lookup['12EN', 'xmax'], locations.lookup['12EN', 'ymax'], lty = 2) # 12EN edge
    rect(locations.lookup['12HET', 'xmin'], locations.lookup['12HET', 'ymin'], locations.lookup['12HET', 'xmax'], locations.lookup['12HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HET
    rect(locations.lookup['12HEB', 'xmin'], locations.lookup['12HEB', 'ymin'], locations.lookup['12HEB', 'xmax'], locations.lookup['12HEB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HEB
    rect(locations.lookup['12HWT', 'xmin'], locations.lookup['12HWT', 'ymin'], locations.lookup['12HWT', 'xmax'], locations.lookup['12HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HWT
    rect(locations.lookup['12HWB', 'xmin'], locations.lookup['12HWB', 'ymin'], locations.lookup['12HWB', 'xmax'], locations.lookup['12HWB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HWB
    rect(locations.lookup['FS12', 'xmin'], locations.lookup['FS12', 'ymin'], locations.lookup['FS12', 'xmax'], locations.lookup['FS12', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 12FS
    rect(locations.lookup['12EW', 'xmin'], locations.lookup['12ES', 'ymin'], locations.lookup['12EE', 'xmax'], locations.lookup['12EN', 'ymax'], lwd = 2) # cage limits
    text(31, 44, labels = bquote(paste(KUD[50], ' = ', .(ka[1,1]), m^2)), adj = c(0,0))
    text(31, 42.5, labels = bquote(paste(KUD[95], ' = ', .(ka[2,1]), m^2)), adj = c(0,0))
    
    
  } else{if (unique(dayfile$PEN == 14)){
    
    plot(ver95, col = kudcols[[1]], axes = T, xlim = c(10, 45), ylim = c(35, 70), xlab = 'x (m)', ylab = 'y (m)') # plot KUD95
    plot(ver50, col = kudcols[[4]], axes = F, xlim = c(10, 45), ylim = c(35, 70), add=T) # plot KUD50
    
    rect(locations.lookup['14EW', 'xmin'], locations.lookup['14EW', 'ymin'], locations.lookup['14EW', 'xmax'], locations.lookup['14EW', 'ymax'], lty = 2) # 14EW edge
    rect(locations.lookup['14ES', 'xmin'], locations.lookup['14ES', 'ymin'], locations.lookup['14ES', 'xmax'], locations.lookup['14ES', 'ymax'], lty = 2) # 14ES edge
    rect(locations.lookup['14EE', 'xmin'], locations.lookup['14EE', 'ymin'], locations.lookup['14EE', 'xmax'], locations.lookup['14EE', 'ymax'], lty = 2) # 14EE edge
    rect(locations.lookup['14EN', 'xmin'], locations.lookup['14EN', 'ymin'], locations.lookup['14EN', 'xmax'], locations.lookup['14EN', 'ymax'], lty = 2) # 14EN edge
    rect(locations.lookup['14HET', 'xmin'], locations.lookup['14HET', 'ymin'], locations.lookup['14HET', 'xmax'], locations.lookup['14HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HET
    rect(locations.lookup['14HEB', 'xmin'], locations.lookup['14HEB', 'ymin'], locations.lookup['14HEB', 'xmax'], locations.lookup['14HEB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HEB
    rect(locations.lookup['14HWT', 'xmin'], locations.lookup['14HWT', 'ymin'], locations.lookup['14HWT', 'xmax'], locations.lookup['14HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HWT
    rect(locations.lookup['14HWB', 'xmin'], locations.lookup['14HWB', 'ymin'], locations.lookup['14HWB', 'xmax'], locations.lookup['14HWB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HWB
    rect(locations.lookup['FS14', 'xmin'], locations.lookup['FS14', 'ymin'], locations.lookup['FS14', 'xmax'], locations.lookup['FS14', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 14FS
    rect(locations.lookup['14EW', 'xmin'], locations.lookup['14ES', 'ymin'], locations.lookup['14EE', 'xmax'], locations.lookup['14EN', 'ymax'], lwd = 2) # cage limits
    text(6, 44, labels = bquote(paste(KUD[50], ' = ', .(ka[1,1]), m^2)), adj = c(0,0))
    text(6, 42.5, labels = bquote(paste(KUD[95], ' = ', .(ka[2,1]), m^2)), adj = c(0,0))
    
  }else{if (unique(dayfile$PEN == 15)){
    
    plot(ver95, col = kudcols[[1]], axes = T, xlim = c(10, 45), ylim = c(10, 45), xlab = 'x (m)', ylab = 'y (m)') # plot KUD95
    plot(ver50, col = kudcols[[4]], axes = F, xlim = c(10, 45), ylim = c(10, 45), add=T) # plot KUD50
    
    rect(locations.lookup['15EW', 'xmin'], locations.lookup['15EW', 'ymin'], locations.lookup['15EW', 'xmax'], locations.lookup['15EW', 'ymax'], lty = 2) # 15EW edge
    rect(locations.lookup['15ES', 'xmin'], locations.lookup['15ES', 'ymin'], locations.lookup['15ES', 'xmax'], locations.lookup['15ES', 'ymax'], lty = 2) # 15ES edge
    rect(locations.lookup['15EE', 'xmin'], locations.lookup['15EE', 'ymin'], locations.lookup['15EE', 'xmax'], locations.lookup['15EE', 'ymax'], lty = 2) # 15EE edge
    rect(locations.lookup['15EN', 'xmin'], locations.lookup['15EN', 'ymin'], locations.lookup['15EN', 'xmax'], locations.lookup['15EN', 'ymax'], lty = 2) # 15EN edge
    rect(locations.lookup['15HET', 'xmin'], locations.lookup['15HET', 'ymin'], locations.lookup['15HET', 'xmax'], locations.lookup['15HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HET
    rect(locations.lookup['15HEB', 'xmin'], locations.lookup['15HEB', 'ymin'], locations.lookup['15HEB', 'xmax'], locations.lookup['15HEB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HEB
    rect(locations.lookup['15HWT', 'xmin'], locations.lookup['15HWT', 'ymin'], locations.lookup['15HWT', 'xmax'], locations.lookup['15HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HWT
    rect(locations.lookup['15HWB', 'xmin'], locations.lookup['15HWB', 'ymin'], locations.lookup['15HWB', 'xmax'], locations.lookup['15HWB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HWB
    rect(locations.lookup['FS15', 'xmin'], locations.lookup['FS15', 'ymin'], locations.lookup['FS15', 'xmax'], locations.lookup['FS15', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 15FS
    rect(locations.lookup['15EW', 'xmin'], locations.lookup['15ES', 'ymin'], locations.lookup['15EE', 'xmax'], locations.lookup['15EN', 'ymax'], lwd = 2) # cage limits
    text(6, 44, labels = bquote(paste(KUD[50], ' = ', .(ka[1,1]), m^2)), adj = c(0,0))
    text(6, 42.5, labels = bquote(paste(KUD[95], ' = ', .(ka[2,1]), m^2)), adj = c(0,0))}
  }
  }
  
}


# 47. Calculate 3d kernel distribution utilisation for single fish file as 1m z-stack and save plots to file

kudcalc3d <- function(save = T){
  
  kudcols <- terrain.colors(4, alpha = 0.6)
  
  if(unique(dayfile$PEN == 7)){
    
    x <- seq(0, 50, by = 0.5)
    y <- seq(0, 50, by = 0.5)
    xy <- expand.grid(x=x, y=y)
    coordinates(xy) <- ~x+y
    gridded(xy) <- TRUE
    class(xy)
    
  } else {
    
    x <- seq(20, 75, by = 0.5)
    y <- seq(0, 55, by = 0.5)
    xy <- expand.grid(x=x, y=y)
    coordinates(xy) <- ~x+y
    gridded(xy) <- TRUE
    class(xy)  
    
  }
  
  max.depth <- ceiling(max(dayfile$PosZ))
  #k50 <- 0
  #k95 <- 0
  
  for (j in 1:max.depth){
    
    depthtemp <- subset(dayfile, PosZ > j-1 & PosZ < j)
    
    if (nrow(depthtemp) >50){
      
      coords <- depthtemp[,c(1, 5, 6)] # extract x,y coords and fish id from dayfile
      coordinates(coords) <- c('PosX', 'PosY') # convert to spatial points data frame object
      ud <- kernelUD(coords, h = 'href', grid = xy, kern = 'bivnorm') # KUD calculation for adehabitatHR package
      
      #kcont50 <- ceiling(((nrow(depthtemp)/nrow(dayfile))*0.5)*100)
      #kcont95 <- ifelse(((nrow(depthtemp)/nrow(dayfile))*0.95)*100 >3, ceiling(((nrow(depthtemp)/nrow(dayfile))*0.95)*100), 3)
      
      #if (kcont50 > 3){
      
      ver50 <- getverticeshr(ud, 50) # extract 50% vertex for plotting
      ver95 <- getverticeshr(ud, 95) # extract 95% vertex for plotting
      
      ka <- kernel.area(ud, percent = c(50, 95), unin = 'm', unout = 'm2') # calculates area of KUD50, KUD95
      
      #} else {
      
      #ver95 <- getverticeshr(ud, kcont95) # extract 95% vertex for plotting   
      #ka <- kernel.area(ud, percent = 95, unin = 'm', unout = 'm2') # calculates area of KUD95
      
      #}
      
      if (unique(dayfile$PEN == 7)){
        
        jpeg(file = paste0('kudplot_', unique(dayfile$Period), '_', j, '.jpg'))
        plot(ver95, col = kudcols[[1]], axes = T, xlim = c(10, 45), ylim = c(10, 45), xlab = 'x (m)', ylab = 'y (m)') # plot KUD95
        if (kcont50 >3) { plot(ver50, col = kudcols[[4]], axes = F, xlim = c(10, 45), ylim = c(10, 45), add=T) } # plot KUD50
        plot.pen(7)
        text(7, 44, labels = bquote(paste(KUD[50], ' = ', .(ka[1,1]), m^2)), adj = c(0,0))
        text(7, 42.5, labels = bquote(paste(KUD[95], ' = ', .(ka[2,1]), m^2)), adj = c(0,0))
        text(7, 41, labels = paste0('Depth = ', j), adj = c(0,0))
        dev.off()
        
      } else {
        
        jpeg(file = paste0('kudplot_', unique(dayfile$Period), '_', j, '.jpg'))
        plot(ver95, col = kudcols[[1]], axes = T, xlim = c(40, 65), ylim = c(10, 45), xlab = 'x (m)', ylab = 'y (m)') # plot KUD95
        if (kcont50> 3){ plot(ver50, col = kudcols[[4]], axes = F, xlim = c(40, 65), ylim = c(10, 45), add=T)} # plot KUD50
        plot.pen(8)
        text(32, 44, labels = bquote(paste(KUD[50], ' = ', .(ka[1,1]), m^2)), adj = c(0,0))
        text(32, 42.5, labels = bquote(paste(KUD[95], ' = ', .(ka[2,1]), m^2)), adj = c(0,0))
        text(32, 41, labels = paste0('Depth = ', j), adj = c(0,0))
        dev.off()
        
      }
      
    } else {
      
      #k50 <- k50 + 0
      #k95 <- k95 + 0
      if (unique(dayfile$PEN == 7)){
        jpeg(file = paste0('kudplot_', unique(dayfile$Period), '_', j, '.jpg'))
        plot(ver95, col = 'white', border = 'white', axes = T, xlim = c(10, 45), ylim = c(10, 45), xlab = 'x (m)', ylab = 'y (m)') # plot KUD95
        #plot(x = 1, y = 1, col = 'white', axes = F, xlim = c(10, 45), ylim = c(10, 45), add=T) # plot KUD50
        plot.pen(7)
        text(7, 44, labels = bquote(paste(KUD[50], ' = 0', m^2)), adj = c(0,0))
        text(7, 42.5, labels = bquote(paste(KUD[95], ' = 0', m^2)), adj = c(0,0))
        text(7, 41, labels = paste0('Depth = ', j), adj = c(0,0))
        dev.off()
        
      } else{
        jpeg(file = paste0('kudplot_', unique(dayfile$Period), '_', j, '.jpg'))
        plot(ver95, col = 'white', border = 'white', axes = T, xlim = c(40, 65), ylim = c(10, 45), xlab = 'x (m)', ylab = 'y (m)') # plot KUD95
        #plot(x = 1, y = 1, col = 'white', axes = F, xlim = c(40, 65), ylim = c(10, 45), add=T) # plot KUD50
        plot.pen(8)
        text(32, 44, labels = bquote(paste(KUD[50], ' = 0', m^2)), adj = c(0,0))
        text(32, 42.5, labels = bquote(paste(KUD[95], ' = 0', m^2)), adj = c(0,0))
        text(32, 41, labels = paste0('Depth = ', j), adj = c(0,0))
        dev.off()
        
      }
      
    }
    
  }
  
}


# 48. Plot outline of pen 12, 14 or 15

plot.pen <- function(pen){
  
  if (pen == 12){
    
    rect(locations.lookup['12EW', 'xmin'], locations.lookup['12EW', 'ymin'], locations.lookup['12EW', 'xmax'], locations.lookup['12EW', 'ymax'], lty = 2) # 12EW edge
    rect(locations.lookup['12ES', 'xmin'], locations.lookup['12ES', 'ymin'], locations.lookup['12ES', 'xmax'], locations.lookup['12ES', 'ymax'], lty = 2) # 12ES edge
    rect(locations.lookup['12EE', 'xmin'], locations.lookup['12EE', 'ymin'], locations.lookup['12EE', 'xmax'], locations.lookup['12EE', 'ymax'], lty = 2) # 12EE edge
    rect(locations.lookup['12EN', 'xmin'], locations.lookup['12EN', 'ymin'], locations.lookup['12EN', 'xmax'], locations.lookup['12EN', 'ymax'], lty = 2) # 12EN edge
    rect(locations.lookup['12HET', 'xmin'], locations.lookup['12HET', 'ymin'], locations.lookup['12HET', 'xmax'], locations.lookup['12HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HET
    rect(locations.lookup['12HEB', 'xmin'], locations.lookup['12HEB', 'ymin'], locations.lookup['12HEB', 'xmax'], locations.lookup['12HEB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HEB
    rect(locations.lookup['12HWT', 'xmin'], locations.lookup['12HWT', 'ymin'], locations.lookup['12HWT', 'xmax'], locations.lookup['12HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HWT
    rect(locations.lookup['12HWB', 'xmin'], locations.lookup['12HWB', 'ymin'], locations.lookup['12HWB', 'xmax'], locations.lookup['12HWB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HWB
    rect(locations.lookup['FS12', 'xmin'], locations.lookup['FS12', 'ymin'], locations.lookup['FS12', 'xmax'], locations.lookup['FS12', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 12FS
    rect(locations.lookup['12EW', 'xmin'], locations.lookup['12ES', 'ymin'], locations.lookup['12EE', 'xmax'], locations.lookup['12EN', 'ymax'], lwd = 2) # cage limits
    
  } else {if (pen == 14){
    
    rect(locations.lookup['14EW', 'xmin'], locations.lookup['14EW', 'ymin'], locations.lookup['14EW', 'xmax'], locations.lookup['14EW', 'ymax'], lty = 2) # 14EW edge
    rect(locations.lookup['14ES', 'xmin'], locations.lookup['14ES', 'ymin'], locations.lookup['14ES', 'xmax'], locations.lookup['14ES', 'ymax'], lty = 2) # 14ES edge
    rect(locations.lookup['14EE', 'xmin'], locations.lookup['14EE', 'ymin'], locations.lookup['14EE', 'xmax'], locations.lookup['14EE', 'ymax'], lty = 2) # 14EE edge
    rect(locations.lookup['14EN', 'xmin'], locations.lookup['14EN', 'ymin'], locations.lookup['14EN', 'xmax'], locations.lookup['14EN', 'ymax'], lty = 2) # 14EN edge
    rect(locations.lookup['14HET', 'xmin'], locations.lookup['14HET', 'ymin'], locations.lookup['14HET', 'xmax'], locations.lookup['14HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HET
    rect(locations.lookup['14HEB', 'xmin'], locations.lookup['14HEB', 'ymin'], locations.lookup['14HEB', 'xmax'], locations.lookup['14HEB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HEB
    rect(locations.lookup['14HWT', 'xmin'], locations.lookup['14HWT', 'ymin'], locations.lookup['14HWT', 'xmax'], locations.lookup['14HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HWT
    rect(locations.lookup['14HWB', 'xmin'], locations.lookup['14HWB', 'ymin'], locations.lookup['14HWB', 'xmax'], locations.lookup['14HWB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HWB
    rect(locations.lookup['FS14', 'xmin'], locations.lookup['FS14', 'ymin'], locations.lookup['FS14', 'xmax'], locations.lookup['FS14', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 14FS
    rect(locations.lookup['14EW', 'xmin'], locations.lookup['14ES', 'ymin'], locations.lookup['14EE', 'xmax'], locations.lookup['14EN', 'ymax'], lwd = 2) # cage limits
    
  }else{if (pen == 15){
    
    rect(locations.lookup['15EW', 'xmin'], locations.lookup['15EW', 'ymin'], locations.lookup['15EW', 'xmax'], locations.lookup['15EW', 'ymax'], lty = 2) # 15EW edge
    rect(locations.lookup['15ES', 'xmin'], locations.lookup['15ES', 'ymin'], locations.lookup['15ES', 'xmax'], locations.lookup['15ES', 'ymax'], lty = 2) # 15ES edge
    rect(locations.lookup['15EE', 'xmin'], locations.lookup['15EE', 'ymin'], locations.lookup['15EE', 'xmax'], locations.lookup['15EE', 'ymax'], lty = 2) # 15EE edge
    rect(locations.lookup['15EN', 'xmin'], locations.lookup['15EN', 'ymin'], locations.lookup['15EN', 'xmax'], locations.lookup['15EN', 'ymax'], lty = 2) # 15EN edge
    rect(locations.lookup['15HET', 'xmin'], locations.lookup['15HET', 'ymin'], locations.lookup['15HET', 'xmax'], locations.lookup['15HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HET
    rect(locations.lookup['15HEB', 'xmin'], locations.lookup['15HEB', 'ymin'], locations.lookup['15HEB', 'xmax'], locations.lookup['15HEB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HEB
    rect(locations.lookup['15HWT', 'xmin'], locations.lookup['15HWT', 'ymin'], locations.lookup['15HWT', 'xmax'], locations.lookup['15HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HWT
    rect(locations.lookup['15HWB', 'xmin'], locations.lookup['15HWB', 'ymin'], locations.lookup['15HWB', 'xmax'], locations.lookup['15HWB', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HWB
    rect(locations.lookup['FS15', 'xmin'], locations.lookup['FS15', 'ymin'], locations.lookup['FS15', 'xmax'], locations.lookup['FS15', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 15FS
    rect(locations.lookup['15EW', 'xmin'], locations.lookup['15ES', 'ymin'], locations.lookup['15EE', 'xmax'], locations.lookup['15EN', 'ymax'], lwd = 2) # cage limits 
  }  
  }
  }
  
}


# 50a. calculate behaviour state frequencies

bsf <- function(static = 0.15, cruise = 1.1, save = T){
  
  bsffile <- dayfile[,c('Period', 'PEN', 'SEC', 'BLSEC')]
  bsffile$BSF <- ifelse(bsffile$BLSEC <= static, 'static', ifelse(bsffile$BLSEC > static & bsffile$BLSEC <= cruise, 'cruise', 'burst'))
  bsffile$BSFcount <- sequence(rle(bsffile$BSF)$lengths)
  bsffile$CountTF <- c(ifelse(diff(bsffile$BSFcount, 1, 1) < 1, T, F), F)
  
  
  library(data.table)
  
  setDT(bsffile)
  bsffile[,BSFdur:=ifelse(CountTF == T, sum(SEC),0), by =.(rleid(BSF))] # sums secs for each behaviour bout
  
  detach("package:data.table")
  
  bsffile <- subset(bsffile, BSFdur > 0)
  #bsffile$round <- as.numeric(as.character(cut(bsffile$BSFdur, breaks = c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000), labels = c('1', '2', '5', '10', '20', '50', '100', '200', '500', '1000'))))
  bsffile$round <- as.numeric(as.character(cut(bsffile$BSFdur, breaks = c(0, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024), labels = c('1', '2', '4', '8', '16', '32', '64', '128', '256', '512', '1024'))))
  
  # generates table of BSF frequencies and draws plot
  
  bsffile$BSF <- as.factor(bsffile$BSF)
  
  bsftab <- as.data.frame(table(bsffile$round, bsffile$BSF, bsffile$PEN)) # tabulate frequencies of each duration and BSF
  names(bsftab) <- c('dur', 'BSF', 'pen', 'count')
  bsftab$dur <- as.numeric(as.character(bsftab$dur))
  bsftab$count <- as.numeric(bsftab$count)
  
  bsfsum <- tapply(bsftab$count, list(bsftab$BSF, bsftab$pen), sum)
  bsftab$freq <- ifelse(bsftab$BSF == 'static' & bsftab$pen == '7', bsftab$count / bsfsum[3,1], ifelse(bsftab$BSF == 'cruise' & bsftab$pen == '7', bsftab$count / bsfsum[2,1], ifelse(bsftab$BSF == 'burst' & bsftab$pen == '7', bsftab$count / bsfsum[1,1], ifelse(bsftab$BSF == 'static' & bsftab$pen == '8', bsftab$count / bsfsum[3,2], ifelse(bsftab$BSF == 'cruise' & bsftab$pen == '8', bsftab$count / bsfsum[2,2], ifelse(bsftab$BSF == 'burst' & bsftab$pen == '8', bsftab$count / bsfsum[1,2], NA))))))
  
  bsftab <- subset(bsftab, bsftab$freq > 0)
  
  power_eqn = function(df, start = list(a = 50, b = 1)){
    m = nls(freq ~ a*dur^b, start = start, data = df);
    #eq <- substitute(italic(y) == a  ~italic(x)^b, list(a = format(coef(m)[1], digits = 2), b = format(coef(m)[2], digits = 2)))
    eq <- substitute(italic(y) == a  ~italic(x)^b, list(a = format(coef(m)[1], digits = 2), b = format(coef(m)[2], digits = 2)))
    as.character(as.expression(eq));                 
  }
  
  grouppal <- c(brewer.pal(3, 'Set1')[[1]], brewer.pal(3, 'Set1')[[2]], brewer.pal(3, 'Set1')[[1]], brewer.pal(3, 'Set1')[[2]])
  
  sp = ggplot(subset(bsftab, BSF == 'static'), aes(x=dur, y=freq, colour = pen)) + theme(panel.background = element_rect(fill = 'white', colour = 'black'))
  sp = sp + scale_x_log10(limits = c(10, 1000), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000))
  #sp = sp + scale_y_log10(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000)) 
  sp = sp + scale_y_log10(limits = c(0.001, 1), breaks = c(0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008, 0.009, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.8, 0.09, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), labels = c(bquote(10^-3), '', '', '', '', '', '', '', '', bquote(10^-2), '', '', '', '', '', '', '', '', bquote(10^-1), '', '', '', '', '', '', '', '', bquote(10^0))) 
  sp = sp + geom_path(size = 1) + labs(title = 'Static', x = 'duration', y = 'frequency') + guides(colour = F) + geom_smooth(linetype = 'dashed',  method = 'nls', formula = y~a*x^b, se = F) + geom_text(size = 4.5, hjust = 0, aes(x = 100, y = 1, colour = grouppal[[2]], label = power_eqn(subset(bsftab, pen == '7' & BSF == 'static'))), parse = TRUE) + geom_text(size = 4.5, hjust = 0, aes(x = 100, y = 0.6, colour = grouppal[[1]], label = power_eqn(subset(bsftab, pen == '8' & BSF == 'static'))), parse = TRUE) + scale_colour_manual(values = grouppal)
  
  #+ geom_text(aes(x = 100, y = 1, label = lm_eqn(lm(log(freq) ~ log(dur), subset(bsftab, pen == '7')))), parse = TRUE) + geom_text(aes(x = 100, y = 0.7, label = lm_eqn(lm(log(freq) ~ log(dur), subset(bsftab, pen == '8')))), parse = TRUE)
  
  cp = ggplot(subset(bsftab, BSF == 'cruise'), aes(x=dur, y=freq, colour = pen)) + theme(panel.background = element_rect(fill = 'white', colour = 'black'))
  cp = cp + scale_x_log10(limits = c(10, 1000), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000))
  #cp = cp + scale_y_log10(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000)) 
  cp = cp + scale_y_log10(limits = c(0.001, 1), breaks = c(0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008, 0.009, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.8, 0.09, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), labels = c(bquote(10^-3), '', '', '', '', '', '', '', '', bquote(10^-2), '', '', '', '', '', '', '', '', bquote(10^-1), '', '', '', '', '', '', '', '', bquote(10^0))) 
  #cp = cp + geom_path(size = 1) + labs(title = 'Cruise', x = 'duration', y = 'frequency') + guides(colour = F)
  cp = cp + geom_path(size = 1) + labs(title = 'Cruise', x = 'duration', y = 'frequency') + guides(colour = F) + geom_smooth(linetype = 'dashed',  method = 'nls', formula = y~a*x^b, se = F) + geom_text(size = 4.5, hjust = 0, aes(x = 100, y = 1, colour = grouppal[[2]], label = power_eqn(subset(bsftab, pen == '7' & BSF == 'cruise'))), parse = TRUE) + geom_text(size = 4.5, hjust = 0, aes(x = 100, y = 0.6, colour = grouppal[[1]], label = power_eqn(subset(bsftab, pen == '8' & BSF == 'cruise'))), parse = TRUE) + scale_colour_manual(values = grouppal)
  
  bp = ggplot(subset(bsftab, BSF == 'burst'), aes(x=dur, y=freq, colour = factor(pen, labels = c('conditioned', 'unconditioned')))) + theme(panel.background = element_rect(fill = 'white', colour = 'black'), legend.title = element_text(size = 16, face = 'bold'), legend.title.align = 0.5, legend.background = element_rect(colour = 'black', size = 1, linetype = 'solid'), legend.key.size = unit(1, 'cm'))
  bp = bp + scale_x_log10(limits = c(10, 1000), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000))
  #bp = bp + scale_y_log10(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000)) 
  bp = bp + scale_y_log10(limits = c(0.001, 1), breaks = c(0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008, 0.009, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.8, 0.09, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), labels = c(bquote(10^-3), '', '', '', '', '', '', '', '', bquote(10^-2), '', '', '', '', '', '', '', '', bquote(10^-1), '', '', '', '', '', '', '', '', bquote(10^0))) 
  #bp = bp + geom_path(size = 1) + labs(title = 'Burst', x = 'duration', y = 'frequency', colour = 'Group')
  bp = bp + geom_path(size = 1) + labs(title = 'Burst', x = 'duration', y = 'frequency', colour = 'Group') + geom_smooth(linetype = 'dashed',  method = 'nls', formula = y~a*x^b, se = F) + geom_text(size = 4.5, hjust = 0, show.legend = F, aes(x = 100, y = 1, colour = grouppal[[2]], label = power_eqn(subset(bsftab, pen == '7' & BSF == 'burst'))), parse = TRUE) + geom_text(size = 4.5, hjust = 0, show.legend = F, aes(x = 100, y = 0.6, colour = grouppal[[1]], label = power_eqn(subset(bsftab, pen == '8' & BSF == 'burst'))), parse = TRUE) + scale_colour_manual(breaks = c('conditioned', 'unconditioned'), values = grouppal)
  
  legend <- get_legend(bp)
  bp = bp  + guides(colour = F)
  
  bsfplot <- plot_grid(sp, cp, bp, legend, nrow = 2, ncol = 2)
  daytext = paste('Day', substr(dayfile.loc, 15, 17), sep = ' ')
  bsfplot <- bsfplot + draw_text(daytext, size = 16, x = 0.71, y = 0.33, hjust = 0)
  print(bsfplot) 
  
  if(save == T){
    #ggsave(filename = sub('day_coded.csv', '_bsfplot.png', dayfile.loc), plot = bsfplot) 
    save_plot(sub('day_coded.csv', '_bsfplot.png', dayfile.loc), bsfplot, ncol = 2.5, nrow = 2.5, base_aspect_ratio = 1.1, base_height = 4)  
    write.csv(bsftab, file = sub("day_coded.csv", "_bsftable.csv", dayfile.loc))  
  }
  
}


# 50b. calculate behaviour state frequencies (Rr, Rf, Ra, Ep, Ef, Ea) for pens 7 and 8. save = save plot and data file(T/F)

bsf2 <- function(save = T){
  
  
  bsffile <- dayfile[,c('Period', 'PEN', 'SEC', 'BLSEC', 'BS')]
  #bsffile$BSF <- ifelse(bsffile$BLSEC <= static, 'static', ifelse(bsffile$BLSEC > static & bsffile$BLSEC <= cruise, 'cruise', 'burst'))
  bsffile$BS <- as.character(bsffile$BS)
  bsffile$BScount <- sequence(rle(bsffile$BS)$lengths)
  bsffile$CountTF <- c(ifelse(diff(bsffile$BScount, 1, 1) < 1, T, F), F)
  
  
  library(data.table)
  
  setDT(bsffile)
  bsffile[,BSdur:=ifelse(CountTF == T, sum(SEC),0), by =.(rleid(BS))] # sums secs for each behaviour bout
  
  detach("package:data.table")
  
  #bsffile$BSdur <- with(bsffile, ave(SEC, cumsum(c(TRUE, BS[-1]!= BS[-nrow(bsffile)])), FUN = sum)*CountTF)
  
  bsffile <- subset(bsffile, BSdur > 0)
  #bsffile$round <- as.numeric(as.character(cut(bsffile$BSFdur, breaks = c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000), labels = c('1', '2', '5', '10', '20', '50', '100', '200', '500', '1000'))))
  bsffile$round <- as.numeric(as.character(cut(bsffile$BSdur, breaks = c(0, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024), labels = c('1', '2', '4', '8', '16', '32', '64', '128', '256', '512', '1024'))))
  
  # generates table of BSF frequencies and draws plot
  
  bsffile$BS <- as.factor(bsffile$BS)
  
  bstab <- as.data.frame(table(bsffile$round, bsffile$BS, bsffile$PEN)) # tabulate frequencies of each duration and BSF
  names(bstab) <- c('dur', 'BS', 'pen', 'count')
  bstab$dur <- as.numeric(as.character(bstab$dur))
  bstab$count <- as.numeric(bstab$count)
  
  bssum <- tapply(bstab$count, list(bstab$BS, bstab$pen), sum)
  
  bstab$freq <- ifelse(bstab$BS == 'Ea', bstab$count / bssum[1,1], ifelse(bstab$BS == 'Ef', bstab$count / bssum[2,1], ifelse(bstab$BS == 'Ep', bstab$count / bssum[3,1], ifelse(bstab$BS == 'Ra', bstab$count / bssum[4,1], ifelse(bstab$BS == 'Rf', bstab$count / bssum[5,1], ifelse(bstab$BS == 'Rr', bstab$count / bssum[6,1], NA))))))
  
  
  bstab <- subset(bstab, bstab$freq > 0)
  
  power_eqn = function(df, start = list(a = 50, b = 1)){
    m = nls(freq ~ a*dur^b, start = start, data = df);
    #eq <- substitute(italic(y) == a  ~italic(x)^b, list(a = format(coef(m)[1], digits = 2), b = format(coef(m)[2], digits = 2)))
    eq <- substitute(italic(y) == a  ~italic(x)^b, list(a = format(coef(m)[1], digits = 2), b = format(coef(m)[2], digits = 2)))
    as.character(as.expression(eq));                 
  }
  
  #grouppal <- c(brewer.pal(3, 'Set1')[[1]], brewer.pal(3, 'Set1')[[2]], brewer.pal(3, 'Set1')[[1]], brewer.pal(3, 'Set1')[[2]])
  grouppal <- c(brewer.pal(11, 'Spectral')[[2]], brewer.pal(11, 'Spectral')[[3]], brewer.pal(11, 'Spectral')[[4]], brewer.pal(11, 'Spectral')[[8]], brewer.pal(11, 'Spectral')[[9]], brewer.pal(11, 'Spectral')[[10]])
  
  sp = ggplot(bstab, aes(x=dur, y=freq, colour = BS, group = BS)) + theme(panel.background = element_rect(fill = 'white', colour = 'black'))
  sp = sp + scale_x_log10(limits = c(10, 1000), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000))
  #sp = sp + scale_y_log10(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000)) 
  sp = sp + scale_y_log10(limits = c(0.001, 1), breaks = c(0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008, 0.009, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.8, 0.09, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), labels = c(bquote(10^-3), '', '', '', '', '', '', '', '', bquote(10^-2), '', '', '', '', '', '', '', '', bquote(10^-1), '', '', '', '', '', '', '', '', bquote(10^0))) 
  sp = sp + geom_path(size = 1) + labs(title = unique(bsffile$Period), x = 'duration', y = 'frequency') + scale_colour_manual(values = grouppal)# + guides(colour = F)# + geom_smooth(linetype = 'dashed',  method = 'nls', formula = y~a*x^b, se = F) + geom_text(size = 4.5, hjust = 0, aes(x = 100, y = 1, colour = grouppal[[2]], label = power_eqn(subset(bstab, pen == '7' & BS == 'Ea'))), parse = TRUE) + geom_text(size = 4.5, hjust = 0, aes(x = 100, y = 0.6, colour = grouppal[[1]], label = power_eqn(subset(bstab, pen == '8' & BS == 'Ea'))), parse = TRUE)
  
  #+ geom_text(aes(x = 100, y = 1, label = lm_eqn(lm(log(freq) ~ log(dur), subset(bsftab, pen == '7')))), parse = TRUE) + geom_text(aes(x = 100, y = 0.7, label = lm_eqn(lm(log(freq) ~ log(dur), subset(bsftab, pen == '8')))), parse = TRUE)
  
  #cp = ggplot(subset(bstab, BS == 'cruise'), aes(x=dur, y=freq, colour = pen)) + theme(panel.background = element_rect(fill = 'white', colour = 'black'))
  #cp = cp + scale_x_log10(limits = c(10, 1000), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000))
  #cp = cp + scale_y_log10(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000)) 
  #cp = cp + scale_y_log10(limits = c(0.001, 1), breaks = c(0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008, 0.009, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.8, 0.09, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), labels = c(bquote(10^-3), '', '', '', '', '', '', '', '', bquote(10^-2), '', '', '', '', '', '', '', '', bquote(10^-1), '', '', '', '', '', '', '', '', bquote(10^0))) 
  #cp = cp + geom_path(size = 1) + labs(title = 'Cruise', x = 'duration', y = 'frequency') + guides(colour = F)
  #cp = cp + geom_path(size = 1) + labs(title = 'Cruise', x = 'duration', y = 'frequency') + guides(colour = F) + geom_smooth(linetype = 'dashed',  method = 'nls', formula = y~a*x^b, se = F) + geom_text(size = 4.5, hjust = 0, aes(x = 100, y = 1, colour = grouppal[[2]], label = power_eqn(subset(bstab, pen == '7' & BS == 'cruise'))), parse = TRUE) + geom_text(size = 4.5, hjust = 0, aes(x = 100, y = 0.6, colour = grouppal[[1]], label = power_eqn(subset(bstab, pen == '8' & BS == 'cruise'))), parse = TRUE) + scale_colour_manual(values = grouppal)
  
  #bp = ggplot(subset(bstab, BSF == 'burst'), aes(x=dur, y=freq, colour = factor(pen, labels = c('farmed wrasse', 'wild wrasse')))) + theme(panel.background = element_rect(fill = 'white', colour = 'black'), legend.title = element_text(size = 16, face = 'bold'), legend.title.align = 0.5, legend.background = element_rect(colour = 'black', size = 1, linetype = 'solid'), legend.key.size = unit(1, 'cm'))
  #bp = bp + scale_x_log10(limits = c(10, 1000), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000))
  #bp = bp + scale_y_log10(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000)) 
  #bp = bp + scale_y_log10(limits = c(0.001, 1), breaks = c(0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008, 0.009, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.8, 0.09, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), labels = c(bquote(10^-3), '', '', '', '', '', '', '', '', bquote(10^-2), '', '', '', '', '', '', '', '', bquote(10^-1), '', '', '', '', '', '', '', '', bquote(10^0))) 
  #bp = bp + geom_path(size = 1) + labs(title = 'Burst', x = 'duration', y = 'frequency', colour = 'Group')
  #bp = bp + geom_path(size = 1) + labs(title = 'Burst', x = 'duration', y = 'frequency', colour = 'Group') + geom_smooth(linetype = 'dashed',  method = 'nls', formula = y~a*x^b, se = F) + geom_text(size = 4.5, hjust = 0, show.legend = F, aes(x = 100, y = 1, colour = grouppal[[2]], label = power_eqn(subset(bstab, pen == '7' & BS == 'burst'))), parse = TRUE) + geom_text(size = 4.5, hjust = 0, show.legend = F, aes(x = 100, y = 0.6, colour = grouppal[[1]], label = power_eqn(subset(bstab, pen == '8' & BS == 'burst'))), parse = TRUE) + scale_colour_manual(breaks = c('farmed wrasse', 'wild wrasse'), values = grouppal)
  
  #legend <- get_legend(bp)
  #bp = bp  + guides(colour = F)
  
  #bsfplot <- plot_grid(sp, cp, bp, legend, nrow = 2, ncol = 2)
  #daytext = paste('Day', substr(dayfile.loc, 15, 17), sep = ' ')
  #bsfplot <- bsfplot + draw_text(daytext, size = 16, x = 0.71, y = 0.33, hjust = 0)
  #print(bsfplot) 
  print(sp)
  
  #if(save == T){
  #ggsave(filename = sub('day_coded.csv', '_bsfplot.png', dayfile.loc), plot = bsfplot) 
  #  save_plot(sub('day_coded.csv', '_bsfplot.png', dayfile.loc), bsfplot, ncol = 2.5, nrow = 2.5, base_aspect_ratio = 1.1, base_height = 4)  
  #  write.csv(bstab, file = sub("day_coded.csv", "_bsftable.csv", dayfile.loc))  
  #}
  
}

