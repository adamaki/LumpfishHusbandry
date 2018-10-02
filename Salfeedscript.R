##Locations functions edited for Salmon feeding times and day/night times
##For each fish, (see if can do for each dayfile over multiple days)
library(xlsx) #make sure it's attached or won't create output file
#salfeedon12 <- subset(dayfile, SMEAL12 == 'Y')
#salfeedon14 <- subset(dayfile, SMEAL14 == 'Y')
#salfeedon15 <- subset(dayfile, SMEAL15 == 'Y')

#salfeedoff12 <- subset(dayfile, SMEAL12 == 'N')
#salfeedoff14 <- subset(dayfile, SMEAL14 == 'N')
#salfeedoff15 <- subset(dayfile, SMEAL15 == 'N')

#(can do hexplots too, but locations more important)
##Repeat for Day/Night

#########This script uses & SMEAL_ == 'Y' or 'N' in function; instead of first subsetting dayfile by SMEAL.
 
# LOAD FILES-------------------------------------------------------------------------------------------------------------------
workingdir = "H:/Data processing/2018 Lumpfish Husbandry/6b. Coded Fish CSV/T1 2wks coded fish" ## Reload after doing hides workingdir
setwd(workingdir)

masterfileloc = "H:/Data processing/AcousticTagFile_2018v6.xlsx" # change to location of AcousticTagFile.xlsx

#Lumpfish Laga 2018 Coded Dayfile classes
dayfile.classes = c('NULL', 'numeric', 'factor', 'factor', 'POSIXct', 'double', 'double',
                    'double', 'double', 'double', 'double', 'double', 'double', 'double', 'double', 'double', 'factor',
                    'factor', 'factor', 'factor', 'factor', 'double', 'double', 'double',
                    'double', 'double', 'double', 'double', 'double', 'double', 'double',
                    'double', 'double', 'double', 'double', 'double', 'double', 'double',
                    'double', 'double', 'double', 'double', 'factor', 'factor', 'factor',
                    'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                    'factor', 'factor', 'factor', 'double', 'double', 'double', 'double',
                    'double', 'double', 'double', 'double', 'double', 'double', 'double', 'double'
)

#LOAD LOCATIONS CODING DATA ##Need library(xlsx), make sure not detached
locations.lookup <- read.xlsx(masterfileloc, sheetIndex = 11, startRow = 1, endRow = 61, colIndex = seq(1, 7)) # read in codes from Locations Coding spreadsheet

#locations.lookup <- readWorksheetFromFile(masterfileloc, sheet = 12, startRow = 1, endCol = 7) # read in codes from Locations Coding spreadsheet
rownames(locations.lookup) <- locations.lookup$Code



#----- 1. FUNCTION TO CALCULATE SUMMARY OF FISH LOCATIONS (Pen total) for Sal Feed on (for One loaded dayfile)----
locationssalfeedon <- function()
{
  # pen 12 location summary
  dayfile.bot <- subset(dayfile, SMEAL12 == 'Y' & BOT == 'B' & PEN == '12' & SEC >= 0)
  dayfile.top <- subset(dayfile, SMEAL12 == 'Y' & BOT == 'Z' & PEN == '12' & SEC >= 0)
  dayfile.out <- subset(dayfile, SMEAL12 == 'Y' & OUT == '12OE' & PEN == '12' & SEC >= 0 | SMEAL12 == 'Y' & OUT == '12OS' & PEN == '12' & SEC >= 0 | SMEAL12 == 'Y' & OUT == '12ON' & PEN == '12' & SEC >= 0 | SMEAL12 == 'Y' & OUT == '12OW' & PEN == '12' & SEC >= 0)
  dayfile.edg <- subset(dayfile, SMEAL12 == 'Y' & EDG == '12EN' & PEN == '12' & SEC >= 0 | SMEAL12 == 'Y' & EDG == '12EW' & PEN == '12' & SEC >= 0 | SMEAL12 == 'Y' & EDG == '12ES' & PEN == '12' & SEC >= 0 | SMEAL12 == 'Y' & EDG == '12EE' & PEN == '12' & SEC >= 0)
  dayfile.NWCor <- subset(dayfile, SMEAL12 == 'Y' & BIGC == '12CNW' & PEN == '12' & SEC >= 0)
  dayfile.NECor <- subset(dayfile, SMEAL12 == 'Y' & BIGC == '12CNE' & PEN == '12' & SEC >= 0)
  dayfile.SWCor <- subset(dayfile, SMEAL12 == 'Y' & BIGC == '12CSW' & PEN == '12' & SEC >= 0)
  dayfile.SECor <- subset(dayfile, SMEAL12 == 'Y' & BIGC == '12CSE' & PEN == '12' & SEC >= 0)
  dayfile.cen <- subset(dayfile, SMEAL12 == 'Y' & CEN == '12MH' & PEN == '12' & SEC >= 0 | SMEAL12 == 'Y' & CEN == '12MM' & PEN == '12' & SEC >= 0 | SMEAL12 == 'Y' & CEN == '12ML' & PEN == '12' & SEC >= 0)
  dayfile.hid <- subset(dayfile, SMEAL12 == 'Y' & HID == 'P12HE' & PEN == '12' & SEC >= 0 | SMEAL12 == 'Y' & HID == 'P12HW' & PEN == '12' & SEC >= 0)
  dayfile.fs <- subset(dayfile, SMEAL12 == 'Y' & FS == 'FS12'  & PEN == '12' & SEC >= 0)
  #location.sum <- data.frame(c(nrow(dayfile.bot), nrow(dayfile.top), nrow(dayfile.out), nrow(dayfile.edg), nrow(dayfile.bigc), nrow(dayfile.cen), nrow(dayfile.hid)))
  location.sum <- data.frame(c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC), sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600))
  rownames(location.sum) <- c('<15m', '>15m', 'outer', 'edge', 'NW_corner', 'NE_corner', 'SW_corner', 'SE_corner', 'centre', 'hides', 'feed station')
  colnames(location.sum) <- 'Pen12'
  
  # pen 14 location summary
  dayfile.bot <- subset(dayfile, SMEAL14 == 'Y' & BOT == 'B' & SEC >= 0 & PEN == '14' & SEC >= 0)
  dayfile.top <- subset(dayfile, SMEAL14 == 'Y' & BOT == 'Z' & PEN == '14' & SEC >= 0)
  dayfile.out <- subset(dayfile, SMEAL14 == 'Y' & OUT == '14OE' & PEN == '14' & SEC >= 0| SMEAL14 == 'Y' & OUT == '14OS' & PEN == '14' & SEC >= 0 | SMEAL14 == 'Y' & OUT == '14ON' & PEN == '14' & SEC >= 0 | SMEAL14 == 'Y' & OUT == '14OW' & PEN == '14' & SEC >= 0)
  dayfile.edg <- subset(dayfile, SMEAL14 == 'Y' & EDG == '14EN' & PEN == '14' & SEC >= 0 | SMEAL14 == 'Y' & EDG == '14EW' & PEN == '14' & SEC >= 0 | SMEAL14 == 'Y' & EDG == '14ES' & PEN == '14' & SEC >= 0 | SMEAL14 == 'Y' & EDG == '14EE' & PEN == '14' & SEC >= 0)
  dayfile.NWCor <- subset(dayfile, SMEAL14 == 'Y' & BIGC == '14CNW' & PEN == '14' & SEC >= 0)
  dayfile.NECor <- subset(dayfile, SMEAL14 == 'Y' & BIGC == '14CNE' & PEN == '14' & SEC >= 0)
  dayfile.SWCor <- subset(dayfile, SMEAL14 == 'Y' & BIGC == '14CSW' & PEN == '14' & SEC >= 0)
  dayfile.SECor <- subset(dayfile, SMEAL14 == 'Y' & BIGC == '14CSE' & PEN == '14' & SEC >= 0)
  dayfile.cen <- subset(dayfile, SMEAL14 == 'Y' & CEN == '14MH' & PEN == '14' & SEC >= 0 | SMEAL14 == 'Y' & CEN == '14MM' & PEN == '14' & SEC >= 0 | SMEAL14 == 'Y' & CEN == '14ML' & PEN == '14' & SEC >= 0)
  dayfile.hid <- subset(dayfile, SMEAL14 == 'Y' & HID == 'P14HW' & PEN == '14' & SEC >= 0 | SMEAL14 == 'Y' & HID == 'P14HE' & PEN == '14' & SEC >= 0)
  dayfile.fs <- subset(dayfile, SMEAL14 == 'Y' & FS == 'FS14' & PEN == '14' & SEC >= 0)
  #location.sum$Pen14 <- c(nrow(dayfile.bot), nrow(dayfile.top), nrow(dayfile.out), nrow(dayfile.edg), nrow(dayfile.bigc), nrow(dayfile.cen), nrow(dayfile.hid))
  location.sum$Pen14 <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600))
  
  # pen 15 location summary
  dayfile.bot <- subset(dayfile, SMEAL15 == 'Y' & BOT == 'B' & SEC >= 0 & PEN == '15' & SEC >= 0)
  dayfile.top <- subset(dayfile, SMEAL15 == 'Y' & BOT == 'Z' & PEN == '15' & SEC >= 0)
  dayfile.out <- subset(dayfile, SMEAL15 == 'Y' & OUT == '15OE' & PEN == '15' & SEC >= 0| SMEAL15 == 'Y' & OUT == '15OS' & PEN == '15' & SEC >= 0 | SMEAL15 == 'Y' & OUT == '15ON' & PEN == '15' & SEC >= 0 | SMEAL15 == 'Y' & OUT == '15OW' & PEN == '15' & SEC >= 0)
  dayfile.edg <- subset(dayfile, SMEAL15 == 'Y' & EDG == '15EN' & PEN == '15' & SEC >= 0 | SMEAL15 == 'Y' & EDG == '15EW' & PEN == '15' & SEC >= 0 | SMEAL15 == 'Y' & EDG == '15ES' & PEN == '15' & SEC >= 0 | SMEAL15 == 'Y' & EDG == '15EE' & PEN == '15' & SEC >= 0)
  dayfile.NWCor <- subset(dayfile, SMEAL15 == 'Y' & BIGC == '15CNW' & PEN == '15' & SEC >= 0)
  dayfile.NECor <- subset(dayfile, SMEAL15 == 'Y' & BIGC == '15CNE' & PEN == '15' & SEC >= 0)
  dayfile.SWCor <- subset(dayfile, SMEAL15 == 'Y' & BIGC == '15CSW' & PEN == '15' & SEC >= 0)
  dayfile.SECor <- subset(dayfile, SMEAL15 == 'Y' & BIGC == '15CSE' & PEN == '15' & SEC >= 0)
  dayfile.cen <- subset(dayfile, SMEAL15 == 'Y' & CEN == '15MH' & PEN == '15' & SEC >= 0 | SMEAL15 == 'Y' & CEN == '15MM' & PEN == '15' & SEC >= 0 | SMEAL15 == 'Y' & CEN == '15ML' & PEN == '15' & SEC >= 0)
  dayfile.hid <- subset(dayfile, SMEAL15 == 'Y' & HID == 'P15HW' & PEN == '15' & SEC >= 0 | SMEAL15 == 'Y' & HID == 'P15HE' & PEN == '15' & SEC >= 0)
  dayfile.fs <- subset(dayfile, SMEAL15 == 'Y' & FS == 'FS15' & PEN == '15' & SEC >= 0)
  #location.sum$Pen15 <- c(nrow(dayfile.bot), nrow(dayfile.top), nrow(dayfile.out), nrow(dayfile.edg), nrow(dayfile.bigc), nrow(dayfile.cen), nrow(dayfile.hid))
  location.sum$Pen15 <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
  location.sum
}

#--- 2. Batch location summary for multiple day files (files in same folder) SMEALON------
batch.locationssalfeedon <- function()
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
    dayfile.bot <- subset(dayfile, SMEAL12 == 'Y' & BOT == 'B' & PEN == '12' & SEC >= 0)
    dayfile.top <- subset(dayfile, SMEAL12 == 'Y' & BOT == 'Z' & PEN == '12' & SEC >= 0)
    dayfile.out <- subset(dayfile, SMEAL12 == 'Y' & OUT == '12OE' & PEN == '12' & SEC >= 0 | SMEAL12 == 'Y' & OUT == '12OS' & PEN == '12' & SEC >= 0 | SMEAL12 == 'Y' & OUT == '12ON' & PEN == '12' & SEC >= 0 | SMEAL12 == 'Y' & OUT == '12OW' & PEN == '12' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SMEAL12 == 'Y' & EDG == '12EN' & PEN == '12' & SEC >= 0 | SMEAL12 == 'Y' & EDG == '12EW' & PEN == '12' & SEC >= 0 | SMEAL12 == 'Y' & EDG == '12ES' & PEN == '12' & SEC >= 0 | SMEAL12 == 'Y' & EDG == '12EE' & PEN == '12' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SMEAL12 == 'Y' & BIGC == '12CNW' & PEN == '12' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SMEAL12 == 'Y' & BIGC == '12CNE' & PEN == '12' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SMEAL12 == 'Y' & BIGC == '12CSW' & PEN == '12' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SMEAL12 == 'Y' & BIGC == '12CSE' & PEN == '12' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SMEAL12 == 'Y' & CEN == '12MH' & PEN == '12' & SEC >= 0 | SMEAL12 == 'Y' & CEN == '12MM' & PEN == '12' & SEC >= 0 | SMEAL12 == 'Y' & CEN == '12ML' & PEN == '12' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SMEAL12 == 'Y' & HID == 'P12HW' & PEN == '12' & SEC >= 0 | SMEAL12 == 'Y' & HID == 'P12HE' & PEN == '12' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SMEAL12 == 'Y' & FS == 'FS12'  & PEN == '12' & SEC >= 0)
    locations.P12[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 14 location summary
    dayfile.bot <- subset(dayfile, SMEAL14 == 'Y' & BOT == 'B' & SEC >= 0 & PEN == '14' & SEC >= 0)
    dayfile.top <- subset(dayfile, SMEAL14 == 'Y' & BOT == 'Z' & PEN == '14' & SEC >= 0)
    dayfile.out <- subset(dayfile, SMEAL14 == 'Y' & OUT == '14OE' & PEN == '14' & SEC >= 0| SMEAL14 == 'Y' & OUT == '14OS' & PEN == '14' & SEC >= 0 | SMEAL14 == 'Y' & OUT == '14ON' & PEN == '14' & SEC >= 0 | SMEAL14 == 'Y' & OUT == '14OW' & PEN == '14' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SMEAL14 == 'Y' & EDG == '14EN' & PEN == '14' & SEC >= 0 | SMEAL14 == 'Y' & EDG == '14EW' & PEN == '14' & SEC >= 0 | SMEAL14 == 'Y' & EDG == '14ES' & PEN == '14' & SEC >= 0 | SMEAL14 == 'Y' & EDG == '14EE' & PEN == '14' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SMEAL14 == 'Y' & BIGC == '14CNW' & PEN == '14' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SMEAL14 == 'Y' & BIGC == '14CNE' & PEN == '14' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SMEAL14 == 'Y' & BIGC == '14CSW' & PEN == '14' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SMEAL14 == 'Y' & BIGC == '14CSE' & PEN == '14' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SMEAL14 == 'Y' & CEN == '14MH' & PEN == '14' & SEC >= 0 | SMEAL14 == 'Y' & CEN == '14MM' & PEN == '14' & SEC >= 0 | SMEAL14 == 'Y' & CEN == '14ML' & PEN == '14' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SMEAL14 == 'Y' & HID == 'P14HW' & PEN == '14' & SEC >= 0 | SMEAL14 == 'Y' & HID == 'P14HE' & PEN == '14' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SMEAL14 == 'Y' & FS == 'FS14' & PEN == '14' & SEC >= 0)
    locations.P14[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 15 location summary
    dayfile.bot <- subset(dayfile, SMEAL15 == 'Y' & BOT == 'B' & SEC >= 0 & PEN == '15' & SEC >= 0)
    dayfile.top <- subset(dayfile, SMEAL15 == 'Y' & BOT == 'Z' & PEN == '15' & SEC >= 0)
    dayfile.out <- subset(dayfile, SMEAL15 == 'Y' & OUT == '15OE' & PEN == '15' & SEC >= 0| SMEAL15 == 'Y' & OUT == '15OS' & PEN == '15' & SEC >= 0 | SMEAL15 == 'Y' & OUT == '15ON' & PEN == '15' & SEC >= 0 | SMEAL15 == 'Y' & OUT == '15OW' & PEN == '15' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SMEAL15 == 'Y' & EDG == '15EN' & PEN == '15' & SEC >= 0 | SMEAL15 == 'Y' & EDG == '15EW' & PEN == '15' & SEC >= 0 | SMEAL15 == 'Y' & EDG == '15ES' & PEN == '15' & SEC >= 0 | SMEAL15 == 'Y' & EDG == '15EE' & PEN == '15' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SMEAL15 == 'Y' & BIGC == '15CNW' & PEN == '15' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SMEAL15 == 'Y' & BIGC == '15CNE' & PEN == '15' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SMEAL15 == 'Y' & BIGC == '15CSW' & PEN == '15' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SMEAL15 == 'Y' & BIGC == '15CSE' & PEN == '15' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SMEAL15 == 'Y' & CEN == '15MH' & PEN == '15' & SEC >= 0 | SMEAL15 == 'Y' & CEN == '15MM' & PEN == '15' & SEC >= 0 | SMEAL15 == 'Y' & CEN == '15ML' & PEN == '15' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SMEAL15 == 'Y' & HID == 'P15HW' & PEN == '15' & SEC >= 0 | SMEAL15 == 'Y' & HID == 'P15HE' & PEN == '15' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SMEAL15 == 'Y' & FS == 'FS15' & PEN == '15' & SEC >= 0)
    locations.P15[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
  }
  location.sum <- rbind(locations.P12, locations.P14, locations.P15)  
  location.sum$ID <- NULL
  location.sum  
  
  #loadWorkbook('LocationsOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('LocationsOutput.xlsx', location.sum, 'Sheet 1')
  
  write.xlsx(location.sum, 'LocationsOutputSMEALon.xlsx')
}

#----- 1b. FUNCTION TO CALCULATE SUMMARY OF (Pen total) FISH LOCATIONS for Sal Feed OFF (One loaded dayfile)-----
locationssalfeedoff <- function()
{
  # pen 12 location summary
  dayfile.bot <- subset(dayfile, SMEAL12 == 'N' & BOT == 'B' & PEN == '12' & SEC >= 0)
  dayfile.top <- subset(dayfile, SMEAL12 == 'N' & BOT == 'Z' & PEN == '12' & SEC >= 0)
  dayfile.out <- subset(dayfile, SMEAL12 == 'N' & OUT == '12OE' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & OUT == '12OS' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & OUT == '12ON' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & OUT == '12OW' & PEN == '12' & SEC >= 0)
  dayfile.edg <- subset(dayfile, SMEAL12 == 'N' & EDG == '12EN' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & EDG == '12EW' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & EDG == '12ES' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & EDG == '12EE' & PEN == '12' & SEC >= 0)
  dayfile.NWCor <- subset(dayfile, SMEAL12 == 'N' & BIGC == '12CNW' & PEN == '12' & SEC >= 0)
  dayfile.NECor <- subset(dayfile, SMEAL12 == 'N' & BIGC == '12CNE' & PEN == '12' & SEC >= 0)
  dayfile.SWCor <- subset(dayfile, SMEAL12 == 'N' & BIGC == '12CSW' & PEN == '12' & SEC >= 0)
  dayfile.SECor <- subset(dayfile, SMEAL12 == 'N' & BIGC == '12CSE' & PEN == '12' & SEC >= 0)
  dayfile.cen <- subset(dayfile, SMEAL12 == 'N' & CEN == '12MH' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & CEN == '12MM' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & CEN == '12ML' & PEN == '12' & SEC >= 0)
  dayfile.hid <- subset(dayfile, SMEAL12 == 'N' & HID == 'P12HE' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & HID == 'P12HW' & PEN == '12' & SEC >= 0)
  dayfile.fs <- subset(dayfile, SMEAL12 == 'N' & FS == 'FS12'  & PEN == '12' & SEC >= 0)
  #location.sum <- data.frame(c(nrow(dayfile.bot), nrow(dayfile.top), nrow(dayfile.out), nrow(dayfile.edg), nrow(dayfile.bigc), nrow(dayfile.cen), nrow(dayfile.hid)))
  location.sum <- data.frame(c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC), sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600))
  rownames(location.sum) <- c('<15m', '>15m', 'outer', 'edge', 'NW_corner', 'NE_corner', 'SW_corner', 'SE_corner', 'centre', 'hides', 'feed station')
  colnames(location.sum) <- 'Pen12'
  
  # pen 14 location summary
  dayfile.bot <- subset(dayfile, SMEAL14 == 'N' & BOT == 'B' & SEC >= 0 & PEN == '14' & SEC >= 0)
  dayfile.top <- subset(dayfile, SMEAL14 == 'N' & BOT == 'Z' & PEN == '14' & SEC >= 0)
  dayfile.out <- subset(dayfile, SMEAL14 == 'N' & OUT == '14OE' & PEN == '14' & SEC >= 0| SMEAL14 == 'N' & OUT == '14OS' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & OUT == '14ON' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & OUT == '14OW' & PEN == '14' & SEC >= 0)
  dayfile.edg <- subset(dayfile, SMEAL14 == 'N' & EDG == '14EN' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & EDG == '14EW' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & EDG == '14ES' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & EDG == '14EE' & PEN == '14' & SEC >= 0)
  dayfile.NWCor <- subset(dayfile, SMEAL14 == 'N' & BIGC == '14CNW' & PEN == '14' & SEC >= 0)
  dayfile.NECor <- subset(dayfile, SMEAL14 == 'N' & BIGC == '14CNE' & PEN == '14' & SEC >= 0)
  dayfile.SWCor <- subset(dayfile, SMEAL14 == 'N' & BIGC == '14CSW' & PEN == '14' & SEC >= 0)
  dayfile.SECor <- subset(dayfile, SMEAL14 == 'N' & BIGC == '14CSE' & PEN == '14' & SEC >= 0)
  dayfile.cen <- subset(dayfile, SMEAL14 == 'N' & CEN == '14MH' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & CEN == '14MM' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & CEN == '14ML' & PEN == '14' & SEC >= 0)
  dayfile.hid <- subset(dayfile, SMEAL14 == 'N' & HID == 'P14HW' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & HID == 'P14HE' & PEN == '14' & SEC >= 0)
  dayfile.fs <- subset(dayfile, SMEAL14 == 'N' & FS == 'FS14' & PEN == '14' & SEC >= 0)
  #location.sum$Pen14 <- c(nrow(dayfile.bot), nrow(dayfile.top), nrow(dayfile.out), nrow(dayfile.edg), nrow(dayfile.bigc), nrow(dayfile.cen), nrow(dayfile.hid))
  location.sum$Pen14 <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600))
  
  # pen 15 location summary
  dayfile.bot <- subset(dayfile, SMEAL15 == 'N' & BOT == 'B' & SEC >= 0 & PEN == '15' & SEC >= 0)
  dayfile.top <- subset(dayfile, SMEAL15 == 'N' & BOT == 'Z' & PEN == '15' & SEC >= 0)
  dayfile.out <- subset(dayfile, SMEAL15 == 'N' & OUT == '15OE' & PEN == '15' & SEC >= 0| SMEAL15 == 'N' & OUT == '15OS' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & OUT == '15ON' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & OUT == '15OW' & PEN == '15' & SEC >= 0)
  dayfile.edg <- subset(dayfile, SMEAL15 == 'N' & EDG == '15EN' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & EDG == '15EW' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & EDG == '15ES' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & EDG == '15EE' & PEN == '15' & SEC >= 0)
  dayfile.NWCor <- subset(dayfile, SMEAL15 == 'N' & BIGC == '15CNW' & PEN == '15' & SEC >= 0)
  dayfile.NECor <- subset(dayfile, SMEAL15 == 'N' & BIGC == '15CNE' & PEN == '15' & SEC >= 0)
  dayfile.SWCor <- subset(dayfile, SMEAL15 == 'N' & BIGC == '15CSW' & PEN == '15' & SEC >= 0)
  dayfile.SECor <- subset(dayfile, SMEAL15 == 'N' & BIGC == '15CSE' & PEN == '15' & SEC >= 0)
  dayfile.cen <- subset(dayfile, SMEAL15 == 'N' & CEN == '15MH' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & CEN == '15MM' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & CEN == '15ML' & PEN == '15' & SEC >= 0)
  dayfile.hid <- subset(dayfile, SMEAL15 == 'N' & HID == 'P15HW' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & HID == 'P15HE' & PEN == '15' & SEC >= 0)
  dayfile.fs <- subset(dayfile, SMEAL15 == 'N' & FS == 'FS15' & PEN == '15' & SEC >= 0)
  #location.sum$Pen15 <- c(nrow(dayfile.bot), nrow(dayfile.top), nrow(dayfile.out), nrow(dayfile.edg), nrow(dayfile.bigc), nrow(dayfile.cen), nrow(dayfile.hid))
  location.sum$Pen15 <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
  location.sum
}

#------ 2b. Batch location summary for multiple day files SMEALOFF------
batch.locationssalfeedoff <- function()
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
    dayfile.bot <- subset(dayfile, SMEAL12 == 'N' & BOT == 'B' & PEN == '12' & SEC >= 0)
    dayfile.top <- subset(dayfile, SMEAL12 == 'N' & BOT == 'Z' & PEN == '12' & SEC >= 0)
    dayfile.out <- subset(dayfile, SMEAL12 == 'N' & OUT == '12OE' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & OUT == '12OS' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & OUT == '12ON' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & OUT == '12OW' & PEN == '12' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SMEAL12 == 'N' & EDG == '12EN' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & EDG == '12EW' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & EDG == '12ES' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & EDG == '12EE' & PEN == '12' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SMEAL12 == 'N' & BIGC == '12CNW' & PEN == '12' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SMEAL12 == 'N' & BIGC == '12CNE' & PEN == '12' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SMEAL12 == 'N' & BIGC == '12CSW' & PEN == '12' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SMEAL12 == 'N' & BIGC == '12CSE' & PEN == '12' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SMEAL12 == 'N' & CEN == '12MH' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & CEN == '12MM' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & CEN == '12ML' & PEN == '12' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SMEAL12 == 'N' & HID == 'P12HW' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & HID == 'P12HE' & PEN == '12' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SMEAL12 == 'N' & FS == 'FS12'  & PEN == '12' & SEC >= 0)
    locations.P12[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 14 location summary
    dayfile.bot <- subset(dayfile, SMEAL14 == 'N' & BOT == 'B' & PEN == '14' & SEC >= 0)
    dayfile.top <- subset(dayfile, SMEAL14 == 'N' & BOT == 'Z' & PEN == '14' & SEC >= 0)
    dayfile.out <- subset(dayfile, SMEAL14 == 'N' & OUT == '14OE' & PEN == '14' & SEC >= 0| SMEAL14 == 'N' & OUT == '14OS' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & OUT == '14ON' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & OUT == '14OW' & PEN == '14' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SMEAL14 == 'N' & EDG == '14EN' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & EDG == '14EW' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & EDG == '14ES' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & EDG == '14EE' & PEN == '14' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SMEAL14 == 'N' & BIGC == '14CNW' & PEN == '14' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SMEAL14 == 'N' & BIGC == '14CNE' & PEN == '14' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SMEAL14 == 'N' & BIGC == '14CSW' & PEN == '14' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SMEAL14 == 'N' & BIGC == '14CSE' & PEN == '14' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SMEAL14 == 'N' & CEN == '14MH' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & CEN == '14MM' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & CEN == '14ML' & PEN == '14' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SMEAL14 == 'N' & HID == 'P14HW' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & HID == 'P14HE' & PEN == '14' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SMEAL14 == 'N' & FS == 'FS14' & PEN == '14' & SEC >= 0)
    locations.P14[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 15 location summary
    dayfile.bot <- subset(dayfile, SMEAL15 == 'N' & BOT == 'B' & PEN == '15' & SEC >= 0)
    dayfile.top <- subset(dayfile, SMEAL15 == 'N' & BOT == 'Z' & PEN == '15' & SEC >= 0)
    dayfile.out <- subset(dayfile, SMEAL15 == 'N' & OUT == '15OE' & PEN == '15' & SEC >= 0| SMEAL15 == 'N' & OUT == '15OS' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & OUT == '15ON' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & OUT == '15OW' & PEN == '15' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SMEAL15 == 'N' & EDG == '15EN' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & EDG == '15EW' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & EDG == '15ES' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & EDG == '15EE' & PEN == '15' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SMEAL15 == 'N' & BIGC == '15CNW' & PEN == '15' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SMEAL15 == 'N' & BIGC == '15CNE' & PEN == '15' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SMEAL15 == 'N' & BIGC == '15CSW' & PEN == '15' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SMEAL15 == 'N' & BIGC == '15CSE' & PEN == '15' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SMEAL15 == 'N' & CEN == '15MH' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & CEN == '15MM' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & CEN == '15ML' & PEN == '15' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SMEAL15 == 'N' & HID == 'P15HW' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & HID == 'P15HE' & PEN == '15' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SMEAL15 == 'N' & FS == 'FS15' & PEN == '15' & SEC >= 0)
    locations.P15[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
  }
  location.sum <- rbind(locations.P12, locations.P14, locations.P15)  
  location.sum$ID <- NULL
  location.sum  
  
  #loadWorkbook('LocationsOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('LocationsOutput.xlsx', location.sum, 'Sheet 1')
  
  write.xlsx(location.sum, 'LocationsOutputSMEALoff.xlsx')
}

#------ 2b. Batch location summary for multiple day files SMEALOFF In the Day Sun == 'D'------
batch.locationssalfeedoffday <- function()
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
    dayfile.bot <- subset(dayfile, SUN == 'D' & SMEAL12 == 'N' & BOT == 'B' & PEN == '12' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'D' & SMEAL12 == 'N' & BOT == 'Z' & PEN == '12' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'D' & SMEAL12 == 'N' & OUT == '12OE' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & OUT == '12OS' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & OUT == '12ON' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & OUT == '12OW' & PEN == '12' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'D' & SMEAL12 == 'N' & EDG == '12EN' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & EDG == '12EW' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & EDG == '12ES' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & EDG == '12EE' & PEN == '12' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'D' & SMEAL12 == 'N' & BIGC == '12CNW' & PEN == '12' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'D' & SMEAL12 == 'N' & BIGC == '12CNE' & PEN == '12' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'D' & SMEAL12 == 'N' & BIGC == '12CSW' & PEN == '12' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'D' & SMEAL12 == 'N' & BIGC == '12CSE' & PEN == '12' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'D' & SMEAL12 == 'N' & CEN == '12MH' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & CEN == '12MM' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & CEN == '12ML' & PEN == '12' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'D' & SMEAL12 == 'N' & HID == 'P12HW' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & HID == 'P12HE' & PEN == '12' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'D' & SMEAL12 == 'N' & FS == 'FS12'  & PEN == '12' & SEC >= 0)
    locations.P12[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 14 location summary
    dayfile.bot <- subset(dayfile, SUN == 'D' & SMEAL14 == 'N' & BOT == 'B' & PEN == '14' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'D' & SMEAL14 == 'N' & BOT == 'Z' & PEN == '14' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'D' & SMEAL14 == 'N' & OUT == '14OE' & PEN == '14' & SEC >= 0| SMEAL14 == 'N' & OUT == '14OS' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & OUT == '14ON' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & OUT == '14OW' & PEN == '14' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'D' & SMEAL14 == 'N' & EDG == '14EN' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & EDG == '14EW' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & EDG == '14ES' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & EDG == '14EE' & PEN == '14' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'D' & SMEAL14 == 'N' & BIGC == '14CNW' & PEN == '14' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'D' & SMEAL14 == 'N' & BIGC == '14CNE' & PEN == '14' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'D' & SMEAL14 == 'N' & BIGC == '14CSW' & PEN == '14' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'D' & SMEAL14 == 'N' & BIGC == '14CSE' & PEN == '14' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'D' & SMEAL14 == 'N' & CEN == '14MH' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & CEN == '14MM' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & CEN == '14ML' & PEN == '14' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'D' & SMEAL14 == 'N' & HID == 'P14HW' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & HID == 'P14HE' & PEN == '14' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'D' & SMEAL14 == 'N' & FS == 'FS14' & PEN == '14' & SEC >= 0)
    locations.P14[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 15 location summary
    dayfile.bot <- subset(dayfile, SUN == 'D' & SMEAL15 == 'N' & BOT == 'B' & PEN == '15' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'D' & SMEAL15 == 'N' & BOT == 'Z' & PEN == '15' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'D' & SMEAL15 == 'N' & OUT == '15OE' & PEN == '15' & SEC >= 0| SMEAL15 == 'N' & OUT == '15OS' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & OUT == '15ON' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & OUT == '15OW' & PEN == '15' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'D' & SMEAL15 == 'N' & EDG == '15EN' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & EDG == '15EW' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & EDG == '15ES' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & EDG == '15EE' & PEN == '15' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'D' & SMEAL15 == 'N' & BIGC == '15CNW' & PEN == '15' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'D' & SMEAL15 == 'N' & BIGC == '15CNE' & PEN == '15' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'D' & SMEAL15 == 'N' & BIGC == '15CSW' & PEN == '15' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'D' & SMEAL15 == 'N' & BIGC == '15CSE' & PEN == '15' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'D' & SMEAL15 == 'N' & CEN == '15MH' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & CEN == '15MM' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & CEN == '15ML' & PEN == '15' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'D' & SMEAL15 == 'N' & HID == 'P15HW' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & HID == 'P15HE' & PEN == '15' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'D' & SMEAL15 == 'N' & FS == 'FS15' & PEN == '15' & SEC >= 0)
    locations.P15[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
  }
  location.sum <- rbind(locations.P12, locations.P14, locations.P15)  
  location.sum$ID <- NULL
  location.sum  
  
  #loadWorkbook('LocationsOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('LocationsOutput.xlsx', location.sum, 'Sheet 1')
  
  write.xlsx(location.sum, 'LocationsOutputSMEALoffday.xlsx')
}

#------ 2b. Batch location summary for multiple day files SMEALOFF In the Night Sun == 'N'------
batch.locationssalfeedoffnight <- function()
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
    dayfile.bot <- subset(dayfile, SUN == 'N' & SMEAL12 == 'N' & BOT == 'B' & PEN == '12' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'N' & SMEAL12 == 'N' & BOT == 'Z' & PEN == '12' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'N' & SMEAL12 == 'N' & OUT == '12OE' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & OUT == '12OS' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & OUT == '12ON' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & OUT == '12OW' & PEN == '12' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'N' & SMEAL12 == 'N' & EDG == '12EN' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & EDG == '12EW' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & EDG == '12ES' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & EDG == '12EE' & PEN == '12' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'N' & SMEAL12 == 'N' & BIGC == '12CNW' & PEN == '12' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'N' & SMEAL12 == 'N' & BIGC == '12CNE' & PEN == '12' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'N' & SMEAL12 == 'N' & BIGC == '12CSW' & PEN == '12' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'N' & SMEAL12 == 'N' & BIGC == '12CSE' & PEN == '12' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'N' & SMEAL12 == 'N' & CEN == '12MH' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & CEN == '12MM' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & CEN == '12ML' & PEN == '12' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'N' & SMEAL12 == 'N' & HID == 'P12HW' & PEN == '12' & SEC >= 0 | SMEAL12 == 'N' & HID == 'P12HE' & PEN == '12' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'N' & SMEAL12 == 'N' & FS == 'FS12'  & PEN == '12' & SEC >= 0)
    locations.P12[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 14 location summary
    dayfile.bot <- subset(dayfile, SUN == 'N' & SMEAL14 == 'N' & BOT == 'B' & PEN == '14' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'N' & SMEAL14 == 'N' & BOT == 'Z' & PEN == '14' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'N' & SMEAL14 == 'N' & OUT == '14OE' & PEN == '14' & SEC >= 0| SMEAL14 == 'N' & OUT == '14OS' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & OUT == '14ON' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & OUT == '14OW' & PEN == '14' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'N' & SMEAL14 == 'N' & EDG == '14EN' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & EDG == '14EW' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & EDG == '14ES' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & EDG == '14EE' & PEN == '14' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'N' & SMEAL14 == 'N' & BIGC == '14CNW' & PEN == '14' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'N' & SMEAL14 == 'N' & BIGC == '14CNE' & PEN == '14' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'N' & SMEAL14 == 'N' & BIGC == '14CSW' & PEN == '14' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'N' & SMEAL14 == 'N' & BIGC == '14CSE' & PEN == '14' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'N' & SMEAL14 == 'N' & CEN == '14MH' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & CEN == '14MM' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & CEN == '14ML' & PEN == '14' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'N' & SMEAL14 == 'N' & HID == 'P14HW' & PEN == '14' & SEC >= 0 | SMEAL14 == 'N' & HID == 'P14HE' & PEN == '14' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'N' & SMEAL14 == 'N' & FS == 'FS14' & PEN == '14' & SEC >= 0)
    locations.P14[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 15 location summary
    dayfile.bot <- subset(dayfile, SUN == 'N' & SMEAL15 == 'N' & BOT == 'B' & PEN == '15' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'N' & SMEAL15 == 'N' & BOT == 'Z' & PEN == '15' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'N' & SMEAL15 == 'N' & OUT == '15OE' & PEN == '15' & SEC >= 0| SMEAL15 == 'N' & OUT == '15OS' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & OUT == '15ON' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & OUT == '15OW' & PEN == '15' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'N' & SMEAL15 == 'N' & EDG == '15EN' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & EDG == '15EW' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & EDG == '15ES' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & EDG == '15EE' & PEN == '15' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'N' & SMEAL15 == 'N' & BIGC == '15CNW' & PEN == '15' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'N' & SMEAL15 == 'N' & BIGC == '15CNE' & PEN == '15' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'N' & SMEAL15 == 'N' & BIGC == '15CSW' & PEN == '15' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'N' & SMEAL15 == 'N' & BIGC == '15CSE' & PEN == '15' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'N' & SMEAL15 == 'N' & CEN == '15MH' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & CEN == '15MM' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & CEN == '15ML' & PEN == '15' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'N' & SMEAL15 == 'N' & HID == 'P15HW' & PEN == '15' & SEC >= 0 | SMEAL15 == 'N' & HID == 'P15HE' & PEN == '15' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'N' & SMEAL15 == 'N' & FS == 'FS15' & PEN == '15' & SEC >= 0)
    locations.P15[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
  }
  location.sum <- rbind(locations.P12, locations.P14, locations.P15)  
  location.sum$ID <- NULL
  location.sum  
  
  #loadWorkbook('LocationsOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('LocationsOutput.xlsx', location.sum, 'Sheet 1')
  
  write.xlsx(location.sum, 'LocationsOutputSMEALoffnight.xlsx')
}
##----------------------------------------------REDO SMEAL Coding for Day off and Night--------------
#------ 2b. Batch location summary for multiple day files SMEALOFF In the Day Sun == 'D'------
batch.locationssalfeedoffday2 <- function()
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
    dayfile.bot <- subset(dayfile, SUN == 'D' & SMEAL12 == 'N' & BOT == 'B' & PEN == '12' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'D' & SMEAL12 == 'N' & BOT == 'Z' & PEN == '12' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'D' & SMEAL12 == 'N' & OUT == '12OE' & PEN == '12' & SEC >= 0 | SUN == 'D' & SMEAL12 == 'N' & OUT == '12OS' & PEN == '12' & SEC >= 0 | SUN == 'D' & SMEAL12 == 'N' & OUT == '12ON' & PEN == '12' & SEC >= 0 | SUN == 'D' & SMEAL12 == 'N' & OUT == '12OW' & PEN == '12' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'D' & SMEAL12 == 'N' & EDG == '12EN' & PEN == '12' & SEC >= 0 | SUN == 'D' & SMEAL12 == 'N' & EDG == '12EW' & PEN == '12' & SEC >= 0 | SUN == 'D' & SMEAL12 == 'N' & EDG == '12ES' & PEN == '12' & SEC >= 0 | SUN == 'D' & SMEAL12 == 'N' & EDG == '12EE' & PEN == '12' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'D' & SMEAL12 == 'N' & BIGC == '12CNW' & PEN == '12' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'D' & SMEAL12 == 'N' & BIGC == '12CNE' & PEN == '12' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'D' & SMEAL12 == 'N' & BIGC == '12CSW' & PEN == '12' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'D' & SMEAL12 == 'N' & BIGC == '12CSE' & PEN == '12' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'D' & SMEAL12 == 'N' & CEN == '12MH' & PEN == '12' & SEC >= 0 | SUN == 'D' & SMEAL12 == 'N' & CEN == '12MM' & PEN == '12' & SEC >= 0 | SUN == 'D' & SMEAL12 == 'N' & CEN == '12ML' & PEN == '12' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'D' & SMEAL12 == 'N' & HID == 'P12HW' & PEN == '12' & SEC >= 0 | SUN == 'D' & SMEAL12 == 'N' & HID == 'P12HE' & PEN == '12' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'D' & SMEAL12 == 'N' & FS == 'FS12'  & PEN == '12' & SEC >= 0)
    locations.P12[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 14 location summary
    dayfile.bot <- subset(dayfile, SUN == 'D' & SMEAL14 == 'N' & BOT == 'B' & PEN == '14' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'D' & SMEAL14 == 'N' & BOT == 'Z' & PEN == '14' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'D' & SMEAL14 == 'N' & OUT == '14OE' & PEN == '14' & SEC >= 0| SUN == 'D' & SMEAL14 == 'N' & OUT == '14OS' & PEN == '14' & SEC >= 0 | SUN == 'D' & SMEAL14 == 'N' & OUT == '14ON' & PEN == '14' & SEC >= 0 | SUN == 'D' & SMEAL14 == 'N' & OUT == '14OW' & PEN == '14' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'D' & SMEAL14 == 'N' & EDG == '14EN' & PEN == '14' & SEC >= 0 | SUN == 'D' & SMEAL14 == 'N' & EDG == '14EW' & PEN == '14' & SEC >= 0 | SUN == 'D' & SMEAL14 == 'N' & EDG == '14ES' & PEN == '14' & SEC >= 0 | SUN == 'D' & SMEAL14 == 'N' & EDG == '14EE' & PEN == '14' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'D' & SMEAL14 == 'N' & BIGC == '14CNW' & PEN == '14' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'D' & SMEAL14 == 'N' & BIGC == '14CNE' & PEN == '14' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'D' & SMEAL14 == 'N' & BIGC == '14CSW' & PEN == '14' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'D' & SMEAL14 == 'N' & BIGC == '14CSE' & PEN == '14' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'D' & SMEAL14 == 'N' & CEN == '14MH' & PEN == '14' & SEC >= 0 | SUN == 'D' & SMEAL14 == 'N' & CEN == '14MM' & PEN == '14' & SEC >= 0 | SUN == 'D' & SMEAL14 == 'N' & CEN == '14ML' & PEN == '14' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'D' & SMEAL14 == 'N' & HID == 'P14HW' & PEN == '14' & SEC >= 0 | SUN == 'D' & SMEAL14 == 'N' & HID == 'P14HE' & PEN == '14' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'D' & SMEAL14 == 'N' & FS == 'FS14' & PEN == '14' & SEC >= 0)
    locations.P14[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 15 location summary
    dayfile.bot <- subset(dayfile, SUN == 'D' & SMEAL15 == 'N' & BOT == 'B' & PEN == '15' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'D' & SMEAL15 == 'N' & BOT == 'Z' & PEN == '15' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'D' & SMEAL15 == 'N' & OUT == '15OE' & PEN == '15' & SEC >= 0| SUN == 'D' & SMEAL15 == 'N' & OUT == '15OS' & PEN == '15' & SEC >= 0 | SUN == 'D' & SMEAL15 == 'N' & OUT == '15ON' & PEN == '15' & SEC >= 0 | SUN == 'D' & SMEAL15 == 'N' & OUT == '15OW' & PEN == '15' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'D' & SMEAL15 == 'N' & EDG == '15EN' & PEN == '15' & SEC >= 0 | SUN == 'D' & SMEAL15 == 'N' & EDG == '15EW' & PEN == '15' & SEC >= 0 | SUN == 'D' & SMEAL15 == 'N' & EDG == '15ES' & PEN == '15' & SEC >= 0 | SUN == 'D' & SMEAL15 == 'N' & EDG == '15EE' & PEN == '15' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'D' & SMEAL15 == 'N' & BIGC == '15CNW' & PEN == '15' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'D' & SMEAL15 == 'N' & BIGC == '15CNE' & PEN == '15' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'D' & SMEAL15 == 'N' & BIGC == '15CSW' & PEN == '15' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'D' & SMEAL15 == 'N' & BIGC == '15CSE' & PEN == '15' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'D' & SMEAL15 == 'N' & CEN == '15MH' & PEN == '15' & SEC >= 0 | SUN == 'D' & SMEAL15 == 'N' & CEN == '15MM' & PEN == '15' & SEC >= 0 | SUN == 'D' & SMEAL15 == 'N' & CEN == '15ML' & PEN == '15' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'D' & SMEAL15 == 'N' & HID == 'P15HW' & PEN == '15' & SEC >= 0 | SUN == 'D' & SMEAL15 == 'N' & HID == 'P15HE' & PEN == '15' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'D' & SMEAL15 == 'N' & FS == 'FS15' & PEN == '15' & SEC >= 0)
    locations.P15[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
  }
  location.sum <- rbind(locations.P12, locations.P14, locations.P15)  
  location.sum$ID <- NULL
  location.sum  
  
  #loadWorkbook('LocationsOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('LocationsOutput.xlsx', location.sum, 'Sheet 1')
  
  write.xlsx(location.sum, 'LocationsOutputSMEALoffday2.xlsx')
}

#------ 2b. Batch location summary for multiple day files SMEALOFF In the Night Sun == 'N'------
batch.locationssalfeedoffnight2 <- function()
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
    dayfile.bot <- subset(dayfile, SUN == 'N' & SMEAL12 == 'N' & BOT == 'B' & PEN == '12' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'N' & SMEAL12 == 'N' & BOT == 'Z' & PEN == '12' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'N' & SMEAL12 == 'N' & OUT == '12OE' & PEN == '12' & SEC >= 0 | SUN == 'N' &SMEAL12 == 'N' & OUT == '12OS' & PEN == '12' & SEC >= 0 | SUN == 'N' & SMEAL12 == 'N' & OUT == '12ON' & PEN == '12' & SEC >= 0 | SUN == 'N' & SMEAL12 == 'N' & OUT == '12OW' & PEN == '12' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'N' & SMEAL12 == 'N' & EDG == '12EN' & PEN == '12' & SEC >= 0 | SUN == 'N' &SMEAL12 == 'N' & EDG == '12EW' & PEN == '12' & SEC >= 0 | SUN == 'N' & SMEAL12 == 'N' & EDG == '12ES' & PEN == '12' & SEC >= 0 | SUN == 'N' & SMEAL12 == 'N' & EDG == '12EE' & PEN == '12' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'N' & SMEAL12 == 'N' & BIGC == '12CNW' & PEN == '12' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'N' & SMEAL12 == 'N' & BIGC == '12CNE' & PEN == '12' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'N' & SMEAL12 == 'N' & BIGC == '12CSW' & PEN == '12' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'N' & SMEAL12 == 'N' & BIGC == '12CSE' & PEN == '12' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'N' & SMEAL12 == 'N' & CEN == '12MH' & PEN == '12' & SEC >= 0 | SUN == 'N' & SMEAL12 == 'N' & CEN == '12MM' & PEN == '12' & SEC >= 0 | SUN == 'N' & SMEAL12 == 'N' & CEN == '12ML' & PEN == '12' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'N' & SMEAL12 == 'N' & HID == 'P12HW' & PEN == '12' & SEC >= 0 | SUN == 'N' & SMEAL12 == 'N' & HID == 'P12HE' & PEN == '12' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'N' & SMEAL12 == 'N' & FS == 'FS12'  & PEN == '12' & SEC >= 0)
    locations.P12[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 14 location summary
    dayfile.bot <- subset(dayfile, SUN == 'N' & SMEAL14 == 'N' & BOT == 'B' & PEN == '14' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'N' & SMEAL14 == 'N' & BOT == 'Z' & PEN == '14' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'N' & SMEAL14 == 'N' & OUT == '14OE' & PEN == '14' & SEC >= 0| SUN == 'N' & SMEAL14 == 'N' & OUT == '14OS' & PEN == '14' & SEC >= 0 | SUN == 'N' & SMEAL14 == 'N' & OUT == '14ON' & PEN == '14' & SEC >= 0 | SUN == 'N' & SMEAL14 == 'N' & OUT == '14OW' & PEN == '14' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'N' & SMEAL14 == 'N' & EDG == '14EN' & PEN == '14' & SEC >= 0 | SUN == 'N' & SMEAL14 == 'N' & EDG == '14EW' & PEN == '14' & SEC >= 0 | SUN == 'N' & SMEAL14 == 'N' & EDG == '14ES' & PEN == '14' & SEC >= 0 | SUN == 'N' & SMEAL14 == 'N' & EDG == '14EE' & PEN == '14' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'N' & SMEAL14 == 'N' & BIGC == '14CNW' & PEN == '14' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'N' & SMEAL14 == 'N' & BIGC == '14CNE' & PEN == '14' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'N' & SMEAL14 == 'N' & BIGC == '14CSW' & PEN == '14' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'N' & SMEAL14 == 'N' & BIGC == '14CSE' & PEN == '14' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'N' & SMEAL14 == 'N' & CEN == '14MH' & PEN == '14' & SEC >= 0 | SUN == 'N' & SMEAL14 == 'N' & CEN == '14MM' & PEN == '14' & SEC >= 0 | SUN == 'N' & SMEAL14 == 'N' & CEN == '14ML' & PEN == '14' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'N' & SMEAL14 == 'N' & HID == 'P14HW' & PEN == '14' & SEC >= 0 | SUN == 'N' & SMEAL14 == 'N' & HID == 'P14HE' & PEN == '14' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'N' & SMEAL14 == 'N' & FS == 'FS14' & PEN == '14' & SEC >= 0)
    locations.P14[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 15 location summary
    dayfile.bot <- subset(dayfile, SUN == 'N' & SMEAL15 == 'N' & BOT == 'B' & SEC >= 0 & PEN == '15' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'N' & SMEAL15 == 'N' & BOT == 'Z' & PEN == '15' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'N' & SMEAL15 == 'N' & OUT == '15OE' & PEN == '15' & SEC >= 0| SUN == 'N' & SMEAL15 == 'N' & OUT == '15OS' & PEN == '15' & SEC >= 0 | SUN == 'N' & SMEAL15 == 'N' & OUT == '15ON' & PEN == '15' & SEC >= 0 | SUN == 'N' & SMEAL15 == 'N' & OUT == '15OW' & PEN == '15' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'N' & SMEAL15 == 'N' & EDG == '15EN' & PEN == '15' & SEC >= 0 | SUN == 'N' & SMEAL15 == 'N' & EDG == '15EW' & PEN == '15' & SEC >= 0 | SUN == 'N' & SMEAL15 == 'N' & EDG == '15ES' & PEN == '15' & SEC >= 0 | SUN == 'N' & SMEAL15 == 'N' & EDG == '15EE' & PEN == '15' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'N' & SMEAL15 == 'N' & BIGC == '15CNW' & PEN == '15' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'N' & SMEAL15 == 'N' & BIGC == '15CNE' & PEN == '15' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'N' & SMEAL15 == 'N' & BIGC == '15CSW' & PEN == '15' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'N' & SMEAL15 == 'N' & BIGC == '15CSE' & PEN == '15' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'N' & SMEAL15 == 'N' & CEN == '15MH' & PEN == '15' & SEC >= 0 | SUN == 'N' & SMEAL15 == 'N' & CEN == '15MM' & PEN == '15' & SEC >= 0 | SUN == 'N' & SMEAL15 == 'N' & CEN == '15ML' & PEN == '15' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'N' & SMEAL15 == 'N' & HID == 'P15HW' & PEN == '15' & SEC >= 0 | SUN == 'N' & SMEAL15 == 'N' & HID == 'P15HE' & PEN == '15' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'N' & SMEAL15 == 'N' & FS == 'FS15' & PEN == '15' & SEC >= 0)
    locations.P15[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
  }
  location.sum <- rbind(locations.P12, locations.P14, locations.P15)  
  location.sum$ID <- NULL
  location.sum  
  
  #loadWorkbook('LocationsOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('LocationsOutput.xlsx', location.sum, 'Sheet 1')
  
  write.xlsx(location.sum, 'LocationsOutputSMEALoffnight2.xlsx')
}


#------ 2b. Batch location summary for multiple day files Before Net Swings: C ------
batch.locationsnetswingdayC <- function()
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
    dayfile.bot <- subset(dayfile, SUN == 'D' & BIOF12 == 'C' & BOT == 'B' & PEN == '12' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'D' & BIOF12 == 'C' & BOT == 'Z' & PEN == '12' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'D' & BIOF12 == 'C' & OUT == '12OE' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'C' & OUT == '12OS' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'C' & OUT == '12ON' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'C' & OUT == '12OW' & PEN == '12' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'D' & BIOF12 == 'C' & EDG == '12EN' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'C' & EDG == '12EW' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'C' & EDG == '12ES' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'C' & EDG == '12EE' & PEN == '12' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'D' & BIOF12 == 'C' & BIGC == '12CNW' & PEN == '12' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'D' & BIOF12 == 'C' & BIGC == '12CNE' & PEN == '12' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'D' & BIOF12 == 'C' & BIGC == '12CSW' & PEN == '12' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'D' & BIOF12 == 'C' & BIGC == '12CSE' & PEN == '12' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'D' & BIOF12 == 'C' & CEN == '12MH' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'C' & CEN == '12MM' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'C' & CEN == '12ML' & PEN == '12' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'D' & BIOF12 == 'C' & HID == 'P12HW' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'C' & HID == 'P12HE' & PEN == '12' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'D' & BIOF12 == 'C' & FS == 'FS12'  & PEN == '12' & SEC >= 0)
    locations.P12[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 14 location summary
    dayfile.bot <- subset(dayfile, SUN == 'D' & BIOF14 == 'C' & BOT == 'B' & PEN == '14' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'D' & BIOF14 == 'C' & BOT == 'Z' & PEN == '14' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'D' & BIOF14 == 'C' & OUT == '14OE' & PEN == '14' & SEC >= 0| SUN == 'D' & BIOF14 == 'C' & OUT == '14OS' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'C' & OUT == '14ON' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'C' & OUT == '14OW' & PEN == '14' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'D' & BIOF14 == 'C' & EDG == '14EN' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'C' & EDG == '14EW' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'C' & EDG == '14ES' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'C' & EDG == '14EE' & PEN == '14' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'D' & BIOF14 == 'C' & BIGC == '14CNW' & PEN == '14' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'D' & BIOF14 == 'C' & BIGC == '14CNE' & PEN == '14' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'D' & BIOF14 == 'C' & BIGC == '14CSW' & PEN == '14' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'D' & BIOF14 == 'C' & BIGC == '14CSE' & PEN == '14' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'D' & BIOF14 == 'C' & CEN == '14MH' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'C' & CEN == '14MM' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'C' & CEN == '14ML' & PEN == '14' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'D' & BIOF14 == 'C' & HID == 'P14HW' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'C' & HID == 'P14HE' & PEN == '14' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'D' & BIOF14 == 'C' & FS == 'FS14' & PEN == '14' & SEC >= 0)
    locations.P14[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 15 location summary
    dayfile.bot <- subset(dayfile, SUN == 'D' & BIOF15 == 'C' & BOT == 'B' & PEN == '15' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'D' & BIOF15 == 'C' & BOT == 'Z' & PEN == '15' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'D' & BIOF15 == 'C' & OUT == '15OE' & PEN == '15' & SEC >= 0| SUN == 'D' & BIOF15 == 'C' & OUT == '15OS' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'C' & OUT == '15ON' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'C' & OUT == '15OW' & PEN == '15' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'D' & BIOF15 == 'C' & EDG == '15EN' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'C' & EDG == '15EW' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'C' & EDG == '15ES' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'C' & EDG == '15EE' & PEN == '15' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'D' & BIOF15 == 'C' & BIGC == '15CNW' & PEN == '15' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'D' & BIOF15 == 'C' & BIGC == '15CNE' & PEN == '15' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'D' & BIOF15 == 'C' & BIGC == '15CSW' & PEN == '15' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'D' & BIOF15 == 'C' & BIGC == '15CSE' & PEN == '15' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'D' & BIOF15 == 'C' & CEN == '15MH' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'C' & CEN == '15MM' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'C' & CEN == '15ML' & PEN == '15' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'D' & BIOF15 == 'C' & HID == 'P15HW' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'C' & HID == 'P15HE' & PEN == '15' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'D' & BIOF15 == 'C' & FS == 'FS15' & PEN == '15' & SEC >= 0)
    locations.P15[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
  }
  location.sum <- rbind(locations.P12, locations.P14, locations.P15)  
  location.sum$ID <- NULL
  location.sum  
  
  #loadWorkbook('LocationsOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('LocationsOutput.xlsx', location.sum, 'Sheet 1')
  
  write.xlsx(location.sum, 'LocationsOutputNetSwingdayCOnly.xlsx')
}

#------ 2b. Batch location summary for multiple day files Before Net Swings: Cx2 ------
batch.locationsnetswingdayCx2 <- function()
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
    dayfile.bot <- subset(dayfile, SUN == 'D' & BIOF12 == 'CC' & BOT == 'B' & PEN == '12' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'D' & BIOF12 == 'CC' & BOT == 'Z' & PEN == '12' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'D' & BIOF12 == 'CC' & OUT == '12OE' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'CC' & OUT == '12OS' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'CC' & OUT == '12ON' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'CC' & OUT == '12OW' & PEN == '12' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'D' & BIOF12 == 'CC' & EDG == '12EN' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'CC' & EDG == '12EW' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'CC' & EDG == '12ES' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'CC' & EDG == '12EE' & PEN == '12' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'D' & BIOF12 == 'CC' & BIGC == '12CNW' & PEN == '12' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'D' & BIOF12 == 'CC' & BIGC == '12CNE' & PEN == '12' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'D' & BIOF12 == 'CC' & BIGC == '12CSW' & PEN == '12' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'D' & BIOF12 == 'CC' & BIGC == '12CSE' & PEN == '12' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'D' & BIOF12 == 'CC' & CEN == '12MH' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'CC' & CEN == '12MM' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'CC' & CEN == '12ML' & PEN == '12' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'D' & BIOF12 == 'CC' & HID == 'P12HW' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'CC' & HID == 'P12HE' & PEN == '12' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'D' & BIOF12 == 'CC' & FS == 'FS12'  & PEN == '12' & SEC >= 0)
    locations.P12[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 14 location summary
    dayfile.bot <- subset(dayfile, SUN == 'D' & BIOF14 == 'CC' & BOT == 'B' & PEN == '14' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'D' & BIOF14 == 'CC' & BOT == 'Z' & PEN == '14' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'D' & BIOF14 == 'CC' & OUT == '14OE' & PEN == '14' & SEC >= 0| SUN == 'D' & BIOF14 == 'CC' & OUT == '14OS' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'CC' & OUT == '14ON' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'CC' & OUT == '14OW' & PEN == '14' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'D' & BIOF14 == 'CC' & EDG == '14EN' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'CC' & EDG == '14EW' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'CC' & EDG == '14ES' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'CC' & EDG == '14EE' & PEN == '14' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'D' & BIOF14 == 'CC' & BIGC == '14CNW' & PEN == '14' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'D' & BIOF14 == 'CC' & BIGC == '14CNE' & PEN == '14' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'D' & BIOF14 == 'CC' & BIGC == '14CSW' & PEN == '14' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'D' & BIOF14 == 'CC' & BIGC == '14CSE' & PEN == '14' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'D' & BIOF14 == 'CC' & CEN == '14MH' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'CC' & CEN == '14MM' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'CC' & CEN == '14ML' & PEN == '14' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'D' & BIOF14 == 'CC' & HID == 'P14HW' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'CC' & HID == 'P14HE' & PEN == '14' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'D' & BIOF14 == 'CC' & FS == 'FS14' & PEN == '14' & SEC >= 0)
    locations.P14[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 15 location summary
    dayfile.bot <- subset(dayfile, SUN == 'D' & BIOF15 == 'CC' & BOT == 'B' & PEN == '15' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'D' & BIOF15 == 'CC' & BOT == 'Z' & PEN == '15' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'D' & BIOF15 == 'CC' & OUT == '15OE' & PEN == '15' & SEC >= 0| SUN == 'D' & BIOF15 == 'CC' & OUT == '15OS' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'CC' & OUT == '15ON' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'CC' & OUT == '15OW' & PEN == '15' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'D' & BIOF15 == 'CC' & EDG == '15EN' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'CC' & EDG == '15EW' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'CC' & EDG == '15ES' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'CC' & EDG == '15EE' & PEN == '15' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'D' & BIOF15 == 'CC' & BIGC == '15CNW' & PEN == '15' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'D' & BIOF15 == 'CC' & BIGC == '15CNE' & PEN == '15' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'D' & BIOF15 == 'CC' & BIGC == '15CSW' & PEN == '15' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'D' & BIOF15 == 'CC' & BIGC == '15CSE' & PEN == '15' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'D' & BIOF15 == 'CC' & CEN == '15MH' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'CC' & CEN == '15MM' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'CC' & CEN == '15ML' & PEN == '15' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'D' & BIOF15 == 'CC' & HID == 'P15HW' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'CC' & HID == 'P15HE' & PEN == '15' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'D' & BIOF15 == 'CC' & FS == 'FS15' & PEN == '15' & SEC >= 0)
    locations.P15[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
  }
  location.sum <- rbind(locations.P12, locations.P14, locations.P15)  
  location.sum$ID <- NULL
  location.sum  
  
  #loadWorkbook('LocationsOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('LocationsOutput.xlsx', location.sum, 'Sheet 1')
  
  write.xlsx(location.sum, 'LocationsOutputNetSwingdayCx2Only.xlsx')
}

#------ 2b. Batch location summary for multiple day files Before Net Swings: Cx3 ------
batch.locationsnetswingdayCx3 <- function()
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
    dayfile.bot <- subset(dayfile, SUN == 'D' & BIOF12 == 'CCC' & BOT == 'B' & PEN == '12' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'D' & BIOF12 == 'CCC' & BOT == 'Z' & PEN == '12' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'D' & BIOF12 == 'CCC' & OUT == '12OE' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'CCC' & OUT == '12OS' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'CCC' & OUT == '12ON' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'CCC' & OUT == '12OW' & PEN == '12' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'D' & BIOF12 == 'CCC' & EDG == '12EN' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'CCC' & EDG == '12EW' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'CCC' & EDG == '12ES' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'CCC' & EDG == '12EE' & PEN == '12' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'D' & BIOF12 == 'CCC' & BIGC == '12CNW' & PEN == '12' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'D' & BIOF12 == 'CCC' & BIGC == '12CNE' & PEN == '12' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'D' & BIOF12 == 'CCC' & BIGC == '12CSW' & PEN == '12' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'D' & BIOF12 == 'CCC' & BIGC == '12CSE' & PEN == '12' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'D' & BIOF12 == 'CCC' & CEN == '12MH' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'CCC' & CEN == '12MM' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'CCC' & CEN == '12ML' & PEN == '12' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'D' & BIOF12 == 'CCC' & HID == 'P12HW' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'CCC' & HID == 'P12HE' & PEN == '12' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'D' & BIOF12 == 'CCC' & FS == 'FS12'  & PEN == '12' & SEC >= 0)
    locations.P12[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 14 location summary
    dayfile.bot <- subset(dayfile, SUN == 'D' & BIOF14 == 'CCC' & BOT == 'B' & PEN == '14' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'D' & BIOF14 == 'CCC' & BOT == 'Z' & PEN == '14' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'D' & BIOF14 == 'CCC' & OUT == '14OE' & PEN == '14' & SEC >= 0| SUN == 'D' & BIOF14 == 'CCC' & OUT == '14OS' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'CCC' & OUT == '14ON' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'CCC' & OUT == '14OW' & PEN == '14' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'D' & BIOF14 == 'CCC' & EDG == '14EN' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'CCC' & EDG == '14EW' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'CCC' & EDG == '14ES' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'CCC' & EDG == '14EE' & PEN == '14' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'D' & BIOF14 == 'CCC' & BIGC == '14CNW' & PEN == '14' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'D' & BIOF14 == 'CCC' & BIGC == '14CNE' & PEN == '14' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'D' & BIOF14 == 'CCC' & BIGC == '14CSW' & PEN == '14' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'D' & BIOF14 == 'CCC' & BIGC == '14CSE' & PEN == '14' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'D' & BIOF14 == 'CCC' & CEN == '14MH' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'CCC' & CEN == '14MM' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'CCC' & CEN == '14ML' & PEN == '14' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'D' & BIOF14 == 'CCC' & HID == 'P14HW' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'CCC' & HID == 'P14HE' & PEN == '14' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'D' & BIOF14 == 'CCC' & FS == 'FS14' & PEN == '14' & SEC >= 0)
    locations.P14[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 15 location summary
    dayfile.bot <- subset(dayfile, SUN == 'D' & BIOF15 == 'CCC' & BOT == 'B' & PEN == '15' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'D' & BIOF15 == 'CCC' & BOT == 'Z' & PEN == '15' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'D' & BIOF15 == 'CCC' & OUT == '15OE' & PEN == '15' & SEC >= 0| SUN == 'D' & BIOF15 == 'CCC' & OUT == '15OS' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'CCC' & OUT == '15ON' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'CCC' & OUT == '15OW' & PEN == '15' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'D' & BIOF15 == 'CCC' & EDG == '15EN' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'CCC' & EDG == '15EW' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'CCC' & EDG == '15ES' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'CCC' & EDG == '15EE' & PEN == '15' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'D' & BIOF15 == 'CCC' & BIGC == '15CNW' & PEN == '15' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'D' & BIOF15 == 'CCC' & BIGC == '15CNE' & PEN == '15' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'D' & BIOF15 == 'CCC' & BIGC == '15CSW' & PEN == '15' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'D' & BIOF15 == 'CCC' & BIGC == '15CSE' & PEN == '15' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'D' & BIOF15 == 'CCC' & CEN == '15MH' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'CCC' & CEN == '15MM' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'CCC' & CEN == '15ML' & PEN == '15' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'D' & BIOF15 == 'CCC' & HID == 'P15HW' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'CCC' & HID == 'P15HE' & PEN == '15' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'D' & BIOF15 == 'CCC' & FS == 'FS15' & PEN == '15' & SEC >= 0)
    locations.P15[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
  }
  location.sum <- rbind(locations.P12, locations.P14, locations.P15)  
  location.sum$ID <- NULL
  location.sum  
  
  #loadWorkbook('LocationsOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('LocationsOutput.xlsx', location.sum, 'Sheet 1')
  
  write.xlsx(location.sum, 'LocationsOutputNetSwingdayCx3Only.xlsx')
}


#------ 2b. Batch location summary for multiple day files After Net Swings: D ------
batch.locationsnetswingdayD <- function()
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
    dayfile.bot <- subset(dayfile, SUN == 'D' & BIOF12 == 'D' & BOT == 'B' & PEN == '12' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'D' & BIOF12 == 'D' & BOT == 'Z' & PEN == '12' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'D' & BIOF12 == 'D' & OUT == '12OE' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'D' & OUT == '12OS' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'D' & OUT == '12ON' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'D' & OUT == '12OW' & PEN == '12' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'D' & BIOF12 == 'D' & EDG == '12EN' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'D' & EDG == '12EW' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'D' & EDG == '12ES' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'D' & EDG == '12EE' & PEN == '12' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'D' & BIOF12 == 'D' & BIGC == '12CNW' & PEN == '12' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'D' & BIOF12 == 'D' & BIGC == '12CNE' & PEN == '12' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'D' & BIOF12 == 'D' & BIGC == '12CSW' & PEN == '12' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'D' & BIOF12 == 'D' & BIGC == '12CSE' & PEN == '12' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'D' & BIOF12 == 'D' & CEN == '12MH' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'D' & CEN == '12MM' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'D' & CEN == '12ML' & PEN == '12' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'D' & BIOF12 == 'D' & HID == 'P12HW' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'D' & HID == 'P12HE' & PEN == '12' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'D' & BIOF12 == 'D' & FS == 'FS12'  & PEN == '12' & SEC >= 0)
    locations.P12[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 14 location summary
    dayfile.bot <- subset(dayfile, SUN == 'D' & BIOF14 == 'D' & BOT == 'B' & PEN == '14' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'D' & BIOF14 == 'D' & BOT == 'Z' & PEN == '14' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'D' & BIOF14 == 'D' & OUT == '14OE' & PEN == '14' & SEC >= 0| SUN == 'D' & BIOF14 == 'D' & OUT == '14OS' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'D' & OUT == '14ON' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'D' & OUT == '14OW' & PEN == '14' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'D' & BIOF14 == 'D' & EDG == '14EN' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'D' & EDG == '14EW' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'D' & EDG == '14ES' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'D' & EDG == '14EE' & PEN == '14' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'D' & BIOF14 == 'D' & BIGC == '14CNW' & PEN == '14' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'D' & BIOF14 == 'D' & BIGC == '14CNE' & PEN == '14' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'D' & BIOF14 == 'D' & BIGC == '14CSW' & PEN == '14' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'D' & BIOF14 == 'D' & BIGC == '14CSE' & PEN == '14' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'D' & BIOF14 == 'D' & CEN == '14MH' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'D' & CEN == '14MM' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'D' & CEN == '14ML' & PEN == '14' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'D' & BIOF14 == 'D' & HID == 'P14HW' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'D' & HID == 'P14HE' & PEN == '14' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'D' & BIOF14 == 'D' & FS == 'FS14' & PEN == '14' & SEC >= 0)
    locations.P14[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 15 location summary
    dayfile.bot <- subset(dayfile, SUN == 'D' & BIOF15 == 'D' & BOT == 'B' & PEN == '15' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'D' & BIOF15 == 'D' & BOT == 'Z' & PEN == '15' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'D' & BIOF15 == 'D' & OUT == '15OE' & PEN == '15' & SEC >= 0| SUN == 'D' & BIOF15 == 'D' & OUT == '15OS' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'D' & OUT == '15ON' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'D' & OUT == '15OW' & PEN == '15' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'D' & BIOF15 == 'D' & EDG == '15EN' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'D' & EDG == '15EW' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'D' & EDG == '15ES' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'D' & EDG == '15EE' & PEN == '15' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'D' & BIOF15 == 'D' & BIGC == '15CNW' & PEN == '15' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'D' & BIOF15 == 'D' & BIGC == '15CNE' & PEN == '15' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'D' & BIOF15 == 'D' & BIGC == '15CSW' & PEN == '15' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'D' & BIOF15 == 'D' & BIGC == '15CSE' & PEN == '15' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'D' & BIOF15 == 'D' & CEN == '15MH' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'D' & CEN == '15MM' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'D' & CEN == '15ML' & PEN == '15' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'D' & BIOF15 == 'D' & HID == 'P15HW' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'D' & HID == 'P15HE' & PEN == '15' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'D' & BIOF15 == 'D' & FS == 'FS15' & PEN == '15' & SEC >= 0)
    locations.P15[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
  }
  location.sum <- rbind(locations.P12, locations.P14, locations.P15)  
  location.sum$ID <- NULL
  location.sum  
  
  #loadWorkbook('LocationsOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('LocationsOutput.xlsx', location.sum, 'Sheet 1')
  
  write.xlsx(location.sum, 'LocationsOutputNetSwingdayDOnly.xlsx')
}

#------ 2b. Batch location summary for multiple day files Before Net Swings: Dx2 ------
batch.locationsnetswingdayDx2 <- function()
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
    dayfile.bot <- subset(dayfile, SUN == 'D' & BIOF12 == 'DD' & BOT == 'B' & PEN == '12' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'D' & BIOF12 == 'DD' & BOT == 'Z' & PEN == '12' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'D' & BIOF12 == 'DD' & OUT == '12OE' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'DD' & OUT == '12OS' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'DD' & OUT == '12ON' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'DD' & OUT == '12OW' & PEN == '12' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'D' & BIOF12 == 'DD' & EDG == '12EN' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'DD' & EDG == '12EW' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'DD' & EDG == '12ES' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'DD' & EDG == '12EE' & PEN == '12' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'D' & BIOF12 == 'DD' & BIGC == '12CNW' & PEN == '12' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'D' & BIOF12 == 'DD' & BIGC == '12CNE' & PEN == '12' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'D' & BIOF12 == 'DD' & BIGC == '12CSW' & PEN == '12' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'D' & BIOF12 == 'DD' & BIGC == '12CSE' & PEN == '12' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'D' & BIOF12 == 'DD' & CEN == '12MH' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'DD' & CEN == '12MM' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'DD' & CEN == '12ML' & PEN == '12' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'D' & BIOF12 == 'DD' & HID == 'P12HW' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'DD' & HID == 'P12HE' & PEN == '12' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'D' & BIOF12 == 'DD' & FS == 'FS12'  & PEN == '12' & SEC >= 0)
    locations.P12[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 14 location summary
    dayfile.bot <- subset(dayfile, SUN == 'D' & BIOF14 == 'DD' & BOT == 'B' & PEN == '14' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'D' & BIOF14 == 'DD' & BOT == 'Z' & PEN == '14' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'D' & BIOF14 == 'DD' & OUT == '14OE' & PEN == '14' & SEC >= 0| SUN == 'D' & BIOF14 == 'DD' & OUT == '14OS' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'DD' & OUT == '14ON' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'DD' & OUT == '14OW' & PEN == '14' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'D' & BIOF14 == 'DD' & EDG == '14EN' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'DD' & EDG == '14EW' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'DD' & EDG == '14ES' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'DD' & EDG == '14EE' & PEN == '14' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'D' & BIOF14 == 'DD' & BIGC == '14CNW' & PEN == '14' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'D' & BIOF14 == 'DD' & BIGC == '14CNE' & PEN == '14' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'D' & BIOF14 == 'DD' & BIGC == '14CSW' & PEN == '14' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'D' & BIOF14 == 'DD' & BIGC == '14CSE' & PEN == '14' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'D' & BIOF14 == 'DD' & CEN == '14MH' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'DD' & CEN == '14MM' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'DD' & CEN == '14ML' & PEN == '14' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'D' & BIOF14 == 'DD' & HID == 'P14HW' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'DD' & HID == 'P14HE' & PEN == '14' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'D' & BIOF14 == 'DD' & FS == 'FS14' & PEN == '14' & SEC >= 0)
    locations.P14[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 15 location summary
    dayfile.bot <- subset(dayfile, SUN == 'D' & BIOF15 == 'DD' & BOT == 'B' & PEN == '15' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'D' & BIOF15 == 'DD' & BOT == 'Z' & PEN == '15' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'D' & BIOF15 == 'DD' & OUT == '15OE' & PEN == '15' & SEC >= 0| SUN == 'D' & BIOF15 == 'DD' & OUT == '15OS' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'DD' & OUT == '15ON' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'DD' & OUT == '15OW' & PEN == '15' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'D' & BIOF15 == 'DD' & EDG == '15EN' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'DD' & EDG == '15EW' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'DD' & EDG == '15ES' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'DD' & EDG == '15EE' & PEN == '15' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'D' & BIOF15 == 'DD' & BIGC == '15CNW' & PEN == '15' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'D' & BIOF15 == 'DD' & BIGC == '15CNE' & PEN == '15' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'D' & BIOF15 == 'DD' & BIGC == '15CSW' & PEN == '15' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'D' & BIOF15 == 'DD' & BIGC == '15CSE' & PEN == '15' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'D' & BIOF15 == 'DD' & CEN == '15MH' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'DD' & CEN == '15MM' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'DD' & CEN == '15ML' & PEN == '15' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'D' & BIOF15 == 'DD' & HID == 'P15HW' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'DD' & HID == 'P15HE' & PEN == '15' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'D' & BIOF15 == 'DD' & FS == 'FS15' & PEN == '15' & SEC >= 0)
    locations.P15[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
  }
  location.sum <- rbind(locations.P12, locations.P14, locations.P15)  
  location.sum$ID <- NULL
  location.sum  
  
  #loadWorkbook('LocationsOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('LocationsOutput.xlsx', location.sum, 'Sheet 1')
  
  write.xlsx(location.sum, 'LocationsOutputNetSwingdayDx2Only.xlsx')
}

#------ 2b. Batch location summary for multiple day files Before Net Swings: Dx3 ------
batch.locationsnetswingdayDx3 <- function()
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
    dayfile.bot <- subset(dayfile, SUN == 'D' & BIOF12 == 'DDD' & BOT == 'B' & PEN == '12' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'D' & BIOF12 == 'DDD' & BOT == 'Z' & PEN == '12' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'D' & BIOF12 == 'DDD' & OUT == '12OE' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'DDD' & OUT == '12OS' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'DDD' & OUT == '12ON' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'DDD' & OUT == '12OW' & PEN == '12' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'D' & BIOF12 == 'DDD' & EDG == '12EN' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'DDD' & EDG == '12EW' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'DDD' & EDG == '12ES' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'DDD' & EDG == '12EE' & PEN == '12' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'D' & BIOF12 == 'DDD' & BIGC == '12CNW' & PEN == '12' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'D' & BIOF12 == 'DDD' & BIGC == '12CNE' & PEN == '12' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'D' & BIOF12 == 'DDD' & BIGC == '12CSW' & PEN == '12' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'D' & BIOF12 == 'DDD' & BIGC == '12CSE' & PEN == '12' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'D' & BIOF12 == 'DDD' & CEN == '12MH' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'DDD' & CEN == '12MM' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'DDD' & CEN == '12ML' & PEN == '12' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'D' & BIOF12 == 'DDD' & HID == 'P12HW' & PEN == '12' & SEC >= 0 | SUN == 'D' & BIOF12 == 'DDD' & HID == 'P12HE' & PEN == '12' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'D' & BIOF12 == 'DDD' & FS == 'FS12'  & PEN == '12' & SEC >= 0)
    locations.P12[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 14 location summary
    dayfile.bot <- subset(dayfile, SUN == 'D' & BIOF14 == 'DDD' & BOT == 'B' & PEN == '14' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'D' & BIOF14 == 'DDD' & BOT == 'Z' & PEN == '14' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'D' & BIOF14 == 'DDD' & OUT == '14OE' & PEN == '14' & SEC >= 0| SUN == 'D' & BIOF14 == 'DDD' & OUT == '14OS' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'DDD' & OUT == '14ON' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'DDD' & OUT == '14OW' & PEN == '14' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'D' & BIOF14 == 'DDD' & EDG == '14EN' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'DDD' & EDG == '14EW' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'DDD' & EDG == '14ES' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'DDD' & EDG == '14EE' & PEN == '14' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'D' & BIOF14 == 'DDD' & BIGC == '14CNW' & PEN == '14' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'D' & BIOF14 == 'DDD' & BIGC == '14CNE' & PEN == '14' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'D' & BIOF14 == 'DDD' & BIGC == '14CSW' & PEN == '14' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'D' & BIOF14 == 'DDD' & BIGC == '14CSE' & PEN == '14' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'D' & BIOF14 == 'DDD' & CEN == '14MH' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'DDD' & CEN == '14MM' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'DDD' & CEN == '14ML' & PEN == '14' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'D' & BIOF14 == 'DDD' & HID == 'P14HW' & PEN == '14' & SEC >= 0 | SUN == 'D' & BIOF14 == 'DDD' & HID == 'P14HE' & PEN == '14' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'D' & BIOF14 == 'DDD' & FS == 'FS14' & PEN == '14' & SEC >= 0)
    locations.P14[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 15 location summary
    dayfile.bot <- subset(dayfile, SUN == 'D' & BIOF15 == 'DDD' & BOT == 'B' & PEN == '15' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'D' & BIOF15 == 'DDD' & BOT == 'Z' & PEN == '15' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'D' & BIOF15 == 'DDD' & OUT == '15OE' & PEN == '15' & SEC >= 0| SUN == 'D' & BIOF15 == 'DDD' & OUT == '15OS' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'DDD' & OUT == '15ON' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'DDD' & OUT == '15OW' & PEN == '15' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'D' & BIOF15 == 'DDD' & EDG == '15EN' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'DDD' & EDG == '15EW' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'DDD' & EDG == '15ES' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'DDD' & EDG == '15EE' & PEN == '15' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'D' & BIOF15 == 'DDD' & BIGC == '15CNW' & PEN == '15' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'D' & BIOF15 == 'DDD' & BIGC == '15CNE' & PEN == '15' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'D' & BIOF15 == 'DDD' & BIGC == '15CSW' & PEN == '15' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'D' & BIOF15 == 'DDD' & BIGC == '15CSE' & PEN == '15' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'D' & BIOF15 == 'DDD' & CEN == '15MH' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'DDD' & CEN == '15MM' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'DDD' & CEN == '15ML' & PEN == '15' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'D' & BIOF15 == 'DDD' & HID == 'P15HW' & PEN == '15' & SEC >= 0 | SUN == 'D' & BIOF15 == 'DDD' & HID == 'P15HE' & PEN == '15' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'D' & BIOF15 == 'DDD' & FS == 'FS15' & PEN == '15' & SEC >= 0)
    locations.P15[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
  }
  location.sum <- rbind(locations.P12, locations.P14, locations.P15)  
  location.sum$ID <- NULL
  location.sum  
  
  #loadWorkbook('LocationsOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('LocationsOutput.xlsx', location.sum, 'Sheet 1')
  
  write.xlsx(location.sum, 'LocationsOutputNetSwingdayDx3Only.xlsx')
}



batch.locationssalfeedon()
batch.locationssalfeedoffday2()
batch.locationssalfeedoffnight2()
batch.locationsnetswingdayC()
batch.locationsnetswingdayCx2()
batch.locationsnetswingdayCx3()
batch.locationsnetswingdayD()
batch.locationsnetswingdayDx2()
batch.locationsnetswingdayDx3()

#####-------------------Net Swings Night --------------------------


#------ 2b. Batch location summary for multiple day files Before Net Swings: C ------
batch.locationsnetswingnightC <- function()
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
    dayfile.bot <- subset(dayfile, SUN == 'N' & BIOF12 == 'C' & BOT == 'B' & PEN == '12' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'N' & BIOF12 == 'C' & BOT == 'Z' & PEN == '12' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'N' & BIOF12 == 'C' & OUT == '12OE' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'C' & OUT == '12OS' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'C' & OUT == '12ON' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'C' & OUT == '12OW' & PEN == '12' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'N' & BIOF12 == 'C' & EDG == '12EN' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'C' & EDG == '12EW' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'C' & EDG == '12ES' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'C' & EDG == '12EE' & PEN == '12' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'N' & BIOF12 == 'C' & BIGC == '12CNW' & PEN == '12' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'N' & BIOF12 == 'C' & BIGC == '12CNE' & PEN == '12' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'N' & BIOF12 == 'C' & BIGC == '12CSW' & PEN == '12' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'N' & BIOF12 == 'C' & BIGC == '12CSE' & PEN == '12' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'N' & BIOF12 == 'C' & CEN == '12MH' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'C' & CEN == '12MM' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'C' & CEN == '12ML' & PEN == '12' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'N' & BIOF12 == 'C' & HID == 'P12HW' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'C' & HID == 'P12HE' & PEN == '12' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'N' & BIOF12 == 'C' & FS == 'FS12'  & PEN == '12' & SEC >= 0)
    locations.P12[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 14 location summary
    dayfile.bot <- subset(dayfile, SUN == 'N' & BIOF14 == 'C' & BOT == 'B' & PEN == '14' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'N' & BIOF14 == 'C' & BOT == 'Z' & PEN == '14' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'N' & BIOF14 == 'C' & OUT == '14OE' & PEN == '14' & SEC >= 0| SUN == 'N' & BIOF14 == 'C' & OUT == '14OS' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'C' & OUT == '14ON' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'C' & OUT == '14OW' & PEN == '14' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'N' & BIOF14 == 'C' & EDG == '14EN' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'C' & EDG == '14EW' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'C' & EDG == '14ES' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'C' & EDG == '14EE' & PEN == '14' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'N' & BIOF14 == 'C' & BIGC == '14CNW' & PEN == '14' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'N' & BIOF14 == 'C' & BIGC == '14CNE' & PEN == '14' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'N' & BIOF14 == 'C' & BIGC == '14CSW' & PEN == '14' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'N' & BIOF14 == 'C' & BIGC == '14CSE' & PEN == '14' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'N' & BIOF14 == 'C' & CEN == '14MH' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'C' & CEN == '14MM' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'C' & CEN == '14ML' & PEN == '14' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'N' & BIOF14 == 'C' & HID == 'P14HW' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'C' & HID == 'P14HE' & PEN == '14' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'N' & BIOF14 == 'C' & FS == 'FS14' & PEN == '14' & SEC >= 0)
    locations.P14[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 15 location summary
    dayfile.bot <- subset(dayfile, SUN == 'N' & BIOF15 == 'C' & BOT == 'B' & PEN == '15' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'N' & BIOF15 == 'C' & BOT == 'Z' & PEN == '15' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'N' & BIOF15 == 'C' & OUT == '15OE' & PEN == '15' & SEC >= 0| SUN == 'N' & BIOF15 == 'C' & OUT == '15OS' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'C' & OUT == '15ON' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'C' & OUT == '15OW' & PEN == '15' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'N' & BIOF15 == 'C' & EDG == '15EN' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'C' & EDG == '15EW' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'C' & EDG == '15ES' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'C' & EDG == '15EE' & PEN == '15' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'N' & BIOF15 == 'C' & BIGC == '15CNW' & PEN == '15' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'N' & BIOF15 == 'C' & BIGC == '15CNE' & PEN == '15' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'N' & BIOF15 == 'C' & BIGC == '15CSW' & PEN == '15' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'N' & BIOF15 == 'C' & BIGC == '15CSE' & PEN == '15' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'N' & BIOF15 == 'C' & CEN == '15MH' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'C' & CEN == '15MM' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'C' & CEN == '15ML' & PEN == '15' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'N' & BIOF15 == 'C' & HID == 'P15HW' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'C' & HID == 'P15HE' & PEN == '15' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'N' & BIOF15 == 'C' & FS == 'FS15' & PEN == '15' & SEC >= 0)
    locations.P15[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
  }
  location.sum <- rbind(locations.P12, locations.P14, locations.P15)  
  location.sum$ID <- NULL
  location.sum  
  
  #loadWorkbook('LocationsOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('LocationsOutput.xlsx', location.sum, 'Sheet 1')
  
  write.xlsx(location.sum, 'LocationsOutputNetSwingnightCOnly.xlsx')
}

#------ 2b. Batch location summary for multiple day files Before Net Swings: Cx2 ------
batch.locationsnetswingnightCx2 <- function()
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
    dayfile.bot <- subset(dayfile, SUN == 'N' & BIOF12 == 'CC' & BOT == 'B' & PEN == '12' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'N' & BIOF12 == 'CC' & BOT == 'Z' & PEN == '12' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'N' & BIOF12 == 'CC' & OUT == '12OE' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'CC' & OUT == '12OS' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'CC' & OUT == '12ON' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'CC' & OUT == '12OW' & PEN == '12' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'N' & BIOF12 == 'CC' & EDG == '12EN' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'CC' & EDG == '12EW' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'CC' & EDG == '12ES' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'CC' & EDG == '12EE' & PEN == '12' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'N' & BIOF12 == 'CC' & BIGC == '12CNW' & PEN == '12' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'N' & BIOF12 == 'CC' & BIGC == '12CNE' & PEN == '12' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'N' & BIOF12 == 'CC' & BIGC == '12CSW' & PEN == '12' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'N' & BIOF12 == 'CC' & BIGC == '12CSE' & PEN == '12' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'N' & BIOF12 == 'CC' & CEN == '12MH' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'CC' & CEN == '12MM' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'CC' & CEN == '12ML' & PEN == '12' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'N' & BIOF12 == 'CC' & HID == 'P12HW' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'CC' & HID == 'P12HE' & PEN == '12' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'N' & BIOF12 == 'CC' & FS == 'FS12'  & PEN == '12' & SEC >= 0)
    locations.P12[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 14 location summary
    dayfile.bot <- subset(dayfile, SUN == 'N' & BIOF14 == 'CC' & BOT == 'B' & PEN == '14' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'N' & BIOF14 == 'CC' & BOT == 'Z' & PEN == '14' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'N' & BIOF14 == 'CC' & OUT == '14OE' & PEN == '14' & SEC >= 0| SUN == 'N' & BIOF14 == 'CC' & OUT == '14OS' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'CC' & OUT == '14ON' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'CC' & OUT == '14OW' & PEN == '14' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'N' & BIOF14 == 'CC' & EDG == '14EN' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'CC' & EDG == '14EW' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'CC' & EDG == '14ES' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'CC' & EDG == '14EE' & PEN == '14' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'N' & BIOF14 == 'CC' & BIGC == '14CNW' & PEN == '14' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'N' & BIOF14 == 'CC' & BIGC == '14CNE' & PEN == '14' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'N' & BIOF14 == 'CC' & BIGC == '14CSW' & PEN == '14' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'N' & BIOF14 == 'CC' & BIGC == '14CSE' & PEN == '14' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'N' & BIOF14 == 'CC' & CEN == '14MH' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'CC' & CEN == '14MM' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'CC' & CEN == '14ML' & PEN == '14' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'N' & BIOF14 == 'CC' & HID == 'P14HW' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'CC' & HID == 'P14HE' & PEN == '14' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'N' & BIOF14 == 'CC' & FS == 'FS14' & PEN == '14' & SEC >= 0)
    locations.P14[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 15 location summary
    dayfile.bot <- subset(dayfile, SUN == 'N' & BIOF15 == 'CC' & BOT == 'B' & PEN == '15' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'N' & BIOF15 == 'CC' & BOT == 'Z' & PEN == '15' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'N' & BIOF15 == 'CC' & OUT == '15OE' & PEN == '15' & SEC >= 0| SUN == 'N' & BIOF15 == 'CC' & OUT == '15OS' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'CC' & OUT == '15ON' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'CC' & OUT == '15OW' & PEN == '15' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'N' & BIOF15 == 'CC' & EDG == '15EN' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'CC' & EDG == '15EW' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'CC' & EDG == '15ES' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'CC' & EDG == '15EE' & PEN == '15' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'N' & BIOF15 == 'CC' & BIGC == '15CNW' & PEN == '15' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'N' & BIOF15 == 'CC' & BIGC == '15CNE' & PEN == '15' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'N' & BIOF15 == 'CC' & BIGC == '15CSW' & PEN == '15' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'N' & BIOF15 == 'CC' & BIGC == '15CSE' & PEN == '15' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'N' & BIOF15 == 'CC' & CEN == '15MH' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'CC' & CEN == '15MM' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'CC' & CEN == '15ML' & PEN == '15' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'N' & BIOF15 == 'CC' & HID == 'P15HW' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'CC' & HID == 'P15HE' & PEN == '15' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'N' & BIOF15 == 'CC' & FS == 'FS15' & PEN == '15' & SEC >= 0)
    locations.P15[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
  }
  location.sum <- rbind(locations.P12, locations.P14, locations.P15)  
  location.sum$ID <- NULL
  location.sum  
  
  #loadWorkbook('LocationsOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('LocationsOutput.xlsx', location.sum, 'Sheet 1')
  
  write.xlsx(location.sum, 'LocationsOutputNetSwingnightCx2Only.xlsx')
}

#------ 2b. Batch location summary for multiple day files Before Net Swings: Cx3 ------
batch.locationsnetswingnightCx3 <- function()
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
    dayfile.bot <- subset(dayfile, SUN == 'N' & BIOF12 == 'CCC' & BOT == 'B' & PEN == '12' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'N' & BIOF12 == 'CCC' & BOT == 'Z' & PEN == '12' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'N' & BIOF12 == 'CCC' & OUT == '12OE' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'CCC' & OUT == '12OS' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'CCC' & OUT == '12ON' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'CCC' & OUT == '12OW' & PEN == '12' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'N' & BIOF12 == 'CCC' & EDG == '12EN' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'CCC' & EDG == '12EW' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'CCC' & EDG == '12ES' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'CCC' & EDG == '12EE' & PEN == '12' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'N' & BIOF12 == 'CCC' & BIGC == '12CNW' & PEN == '12' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'N' & BIOF12 == 'CCC' & BIGC == '12CNE' & PEN == '12' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'N' & BIOF12 == 'CCC' & BIGC == '12CSW' & PEN == '12' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'N' & BIOF12 == 'CCC' & BIGC == '12CSE' & PEN == '12' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'N' & BIOF12 == 'CCC' & CEN == '12MH' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'CCC' & CEN == '12MM' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'CCC' & CEN == '12ML' & PEN == '12' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'N' & BIOF12 == 'CCC' & HID == 'P12HW' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'CCC' & HID == 'P12HE' & PEN == '12' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'N' & BIOF12 == 'CCC' & FS == 'FS12'  & PEN == '12' & SEC >= 0)
    locations.P12[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 14 location summary
    dayfile.bot <- subset(dayfile, SUN == 'N' & BIOF14 == 'CCC' & BOT == 'B' & PEN == '14' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'N' & BIOF14 == 'CCC' & BOT == 'Z' & PEN == '14' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'N' & BIOF14 == 'CCC' & OUT == '14OE' & PEN == '14' & SEC >= 0| SUN == 'N' & BIOF14 == 'CCC' & OUT == '14OS' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'CCC' & OUT == '14ON' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'CCC' & OUT == '14OW' & PEN == '14' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'N' & BIOF14 == 'CCC' & EDG == '14EN' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'CCC' & EDG == '14EW' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'CCC' & EDG == '14ES' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'CCC' & EDG == '14EE' & PEN == '14' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'N' & BIOF14 == 'CCC' & BIGC == '14CNW' & PEN == '14' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'N' & BIOF14 == 'CCC' & BIGC == '14CNE' & PEN == '14' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'N' & BIOF14 == 'CCC' & BIGC == '14CSW' & PEN == '14' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'N' & BIOF14 == 'CCC' & BIGC == '14CSE' & PEN == '14' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'N' & BIOF14 == 'CCC' & CEN == '14MH' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'CCC' & CEN == '14MM' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'CCC' & CEN == '14ML' & PEN == '14' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'N' & BIOF14 == 'CCC' & HID == 'P14HW' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'CCC' & HID == 'P14HE' & PEN == '14' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'N' & BIOF14 == 'CCC' & FS == 'FS14' & PEN == '14' & SEC >= 0)
    locations.P14[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 15 location summary
    dayfile.bot <- subset(dayfile, SUN == 'N' & BIOF15 == 'CCC' & BOT == 'B' & PEN == '15' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'N' & BIOF15 == 'CCC' & BOT == 'Z' & PEN == '15' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'N' & BIOF15 == 'CCC' & OUT == '15OE' & PEN == '15' & SEC >= 0| SUN == 'N' & BIOF15 == 'CCC' & OUT == '15OS' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'CCC' & OUT == '15ON' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'CCC' & OUT == '15OW' & PEN == '15' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'N' & BIOF15 == 'CCC' & EDG == '15EN' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'CCC' & EDG == '15EW' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'CCC' & EDG == '15ES' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'CCC' & EDG == '15EE' & PEN == '15' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'N' & BIOF15 == 'CCC' & BIGC == '15CNW' & PEN == '15' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'N' & BIOF15 == 'CCC' & BIGC == '15CNE' & PEN == '15' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'N' & BIOF15 == 'CCC' & BIGC == '15CSW' & PEN == '15' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'N' & BIOF15 == 'CCC' & BIGC == '15CSE' & PEN == '15' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'N' & BIOF15 == 'CCC' & CEN == '15MH' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'CCC' & CEN == '15MM' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'CCC' & CEN == '15ML' & PEN == '15' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'N' & BIOF15 == 'CCC' & HID == 'P15HW' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'CCC' & HID == 'P15HE' & PEN == '15' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'N' & BIOF15 == 'CCC' & FS == 'FS15' & PEN == '15' & SEC >= 0)
    locations.P15[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
  }
  location.sum <- rbind(locations.P12, locations.P14, locations.P15)  
  location.sum$ID <- NULL
  location.sum  
  
  #loadWorkbook('LocationsOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('LocationsOutput.xlsx', location.sum, 'Sheet 1')
  
  write.xlsx(location.sum, 'LocationsOutputNetSwingnightCx3Only.xlsx')
}


#------ 2b. Batch location summary for multiple day files After Net Swings: D ------
batch.locationsnetswingnightD <- function()
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
    dayfile.bot <- subset(dayfile, SUN == 'N' & BIOF12 == 'D' & BOT == 'B' & PEN == '12' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'N' & BIOF12 == 'D' & BOT == 'Z' & PEN == '12' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'N' & BIOF12 == 'D' & OUT == '12OE' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'D' & OUT == '12OS' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'D' & OUT == '12ON' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'D' & OUT == '12OW' & PEN == '12' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'N' & BIOF12 == 'D' & EDG == '12EN' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'D' & EDG == '12EW' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'D' & EDG == '12ES' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'D' & EDG == '12EE' & PEN == '12' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'N' & BIOF12 == 'D' & BIGC == '12CNW' & PEN == '12' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'N' & BIOF12 == 'D' & BIGC == '12CNE' & PEN == '12' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'N' & BIOF12 == 'D' & BIGC == '12CSW' & PEN == '12' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'N' & BIOF12 == 'D' & BIGC == '12CSE' & PEN == '12' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'N' & BIOF12 == 'D' & CEN == '12MH' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'D' & CEN == '12MM' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'D' & CEN == '12ML' & PEN == '12' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'N' & BIOF12 == 'D' & HID == 'P12HW' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'D' & HID == 'P12HE' & PEN == '12' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'N' & BIOF12 == 'D' & FS == 'FS12'  & PEN == '12' & SEC >= 0)
    locations.P12[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 14 location summary
    dayfile.bot <- subset(dayfile, SUN == 'N' & BIOF14 == 'D' & BOT == 'B' & PEN == '14' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'N' & BIOF14 == 'D' & BOT == 'Z' & PEN == '14' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'N' & BIOF14 == 'D' & OUT == '14OE' & PEN == '14' & SEC >= 0| SUN == 'N' & BIOF14 == 'D' & OUT == '14OS' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'D' & OUT == '14ON' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'D' & OUT == '14OW' & PEN == '14' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'N' & BIOF14 == 'D' & EDG == '14EN' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'D' & EDG == '14EW' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'D' & EDG == '14ES' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'D' & EDG == '14EE' & PEN == '14' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'N' & BIOF14 == 'D' & BIGC == '14CNW' & PEN == '14' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'N' & BIOF14 == 'D' & BIGC == '14CNE' & PEN == '14' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'N' & BIOF14 == 'D' & BIGC == '14CSW' & PEN == '14' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'N' & BIOF14 == 'D' & BIGC == '14CSE' & PEN == '14' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'N' & BIOF14 == 'D' & CEN == '14MH' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'D' & CEN == '14MM' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'D' & CEN == '14ML' & PEN == '14' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'N' & BIOF14 == 'D' & HID == 'P14HW' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'D' & HID == 'P14HE' & PEN == '14' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'N' & BIOF14 == 'D' & FS == 'FS14' & PEN == '14' & SEC >= 0)
    locations.P14[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 15 location summary
    dayfile.bot <- subset(dayfile, SUN == 'N' & BIOF15 == 'D' & BOT == 'B' & PEN == '15' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'N' & BIOF15 == 'D' & BOT == 'Z' & PEN == '15' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'N' & BIOF15 == 'D' & OUT == '15OE' & PEN == '15' & SEC >= 0| SUN == 'N' & BIOF15 == 'D' & OUT == '15OS' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'D' & OUT == '15ON' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'D' & OUT == '15OW' & PEN == '15' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'N' & BIOF15 == 'D' & EDG == '15EN' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'D' & EDG == '15EW' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'D' & EDG == '15ES' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'D' & EDG == '15EE' & PEN == '15' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'N' & BIOF15 == 'D' & BIGC == '15CNW' & PEN == '15' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'N' & BIOF15 == 'D' & BIGC == '15CNE' & PEN == '15' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'N' & BIOF15 == 'D' & BIGC == '15CSW' & PEN == '15' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'N' & BIOF15 == 'D' & BIGC == '15CSE' & PEN == '15' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'N' & BIOF15 == 'D' & CEN == '15MH' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'D' & CEN == '15MM' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'D' & CEN == '15ML' & PEN == '15' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'N' & BIOF15 == 'D' & HID == 'P15HW' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'D' & HID == 'P15HE' & PEN == '15' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'N' & BIOF15 == 'D' & FS == 'FS15' & PEN == '15' & SEC >= 0)
    locations.P15[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
  }
  location.sum <- rbind(locations.P12, locations.P14, locations.P15)  
  location.sum$ID <- NULL
  location.sum  
  
  #loadWorkbook('LocationsOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('LocationsOutput.xlsx', location.sum, 'Sheet 1')
  
  write.xlsx(location.sum, 'LocationsOutputNetSwingnightDOnly.xlsx')
}

#------ 2b. Batch location summary for multiple day files Before Net Swings: Dx2 ------
batch.locationsnetswingnightDx2 <- function()
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
    dayfile.bot <- subset(dayfile, SUN == 'N' & BIOF12 == 'DD' & BOT == 'B' & PEN == '12' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'N' & BIOF12 == 'DD' & BOT == 'Z' & PEN == '12' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'N' & BIOF12 == 'DD' & OUT == '12OE' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'DD' & OUT == '12OS' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'DD' & OUT == '12ON' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'DD' & OUT == '12OW' & PEN == '12' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'N' & BIOF12 == 'DD' & EDG == '12EN' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'DD' & EDG == '12EW' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'DD' & EDG == '12ES' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'DD' & EDG == '12EE' & PEN == '12' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'N' & BIOF12 == 'DD' & BIGC == '12CNW' & PEN == '12' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'N' & BIOF12 == 'DD' & BIGC == '12CNE' & PEN == '12' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'N' & BIOF12 == 'DD' & BIGC == '12CSW' & PEN == '12' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'N' & BIOF12 == 'DD' & BIGC == '12CSE' & PEN == '12' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'N' & BIOF12 == 'DD' & CEN == '12MH' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'DD' & CEN == '12MM' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'DD' & CEN == '12ML' & PEN == '12' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'N' & BIOF12 == 'DD' & HID == 'P12HW' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'DD' & HID == 'P12HE' & PEN == '12' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'N' & BIOF12 == 'DD' & FS == 'FS12'  & PEN == '12' & SEC >= 0)
    locations.P12[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 14 location summary
    dayfile.bot <- subset(dayfile, SUN == 'N' & BIOF14 == 'DD' & BOT == 'B' & PEN == '14' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'N' & BIOF14 == 'DD' & BOT == 'Z' & PEN == '14' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'N' & BIOF14 == 'DD' & OUT == '14OE' & PEN == '14' & SEC >= 0| SUN == 'N' & BIOF14 == 'DD' & OUT == '14OS' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'DD' & OUT == '14ON' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'DD' & OUT == '14OW' & PEN == '14' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'N' & BIOF14 == 'DD' & EDG == '14EN' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'DD' & EDG == '14EW' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'DD' & EDG == '14ES' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'DD' & EDG == '14EE' & PEN == '14' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'N' & BIOF14 == 'DD' & BIGC == '14CNW' & PEN == '14' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'N' & BIOF14 == 'DD' & BIGC == '14CNE' & PEN == '14' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'N' & BIOF14 == 'DD' & BIGC == '14CSW' & PEN == '14' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'N' & BIOF14 == 'DD' & BIGC == '14CSE' & PEN == '14' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'N' & BIOF14 == 'DD' & CEN == '14MH' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'DD' & CEN == '14MM' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'DD' & CEN == '14ML' & PEN == '14' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'N' & BIOF14 == 'DD' & HID == 'P14HW' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'DD' & HID == 'P14HE' & PEN == '14' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'N' & BIOF14 == 'DD' & FS == 'FS14' & PEN == '14' & SEC >= 0)
    locations.P14[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 15 location summary
    dayfile.bot <- subset(dayfile, SUN == 'N' & BIOF15 == 'DD' & BOT == 'B' & PEN == '15' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'N' & BIOF15 == 'DD' & BOT == 'Z' & PEN == '15' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'N' & BIOF15 == 'DD' & OUT == '15OE' & PEN == '15' & SEC >= 0| SUN == 'N' & BIOF15 == 'DD' & OUT == '15OS' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'DD' & OUT == '15ON' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'DD' & OUT == '15OW' & PEN == '15' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'N' & BIOF15 == 'DD' & EDG == '15EN' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'DD' & EDG == '15EW' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'DD' & EDG == '15ES' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'DD' & EDG == '15EE' & PEN == '15' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'N' & BIOF15 == 'DD' & BIGC == '15CNW' & PEN == '15' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'N' & BIOF15 == 'DD' & BIGC == '15CNE' & PEN == '15' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'N' & BIOF15 == 'DD' & BIGC == '15CSW' & PEN == '15' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'N' & BIOF15 == 'DD' & BIGC == '15CSE' & PEN == '15' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'N' & BIOF15 == 'DD' & CEN == '15MH' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'DD' & CEN == '15MM' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'DD' & CEN == '15ML' & PEN == '15' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'N' & BIOF15 == 'DD' & HID == 'P15HW' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'DD' & HID == 'P15HE' & PEN == '15' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'N' & BIOF15 == 'DD' & FS == 'FS15' & PEN == '15' & SEC >= 0)
    locations.P15[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
  }
  location.sum <- rbind(locations.P12, locations.P14, locations.P15)  
  location.sum$ID <- NULL
  location.sum  
  
  #loadWorkbook('LocationsOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('LocationsOutput.xlsx', location.sum, 'Sheet 1')
  
  write.xlsx(location.sum, 'LocationsOutputNetSwingnightDx2Only.xlsx')
}

#------ 2b. Batch location summary for multiple day files Before Net Swings: Dx3 ------
batch.locationsnetswingnightDx3 <- function()
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
    dayfile.bot <- subset(dayfile, SUN == 'N' & BIOF12 == 'DDD' & BOT == 'B' & PEN == '12' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'N' & BIOF12 == 'DDD' & BOT == 'Z' & PEN == '12' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'N' & BIOF12 == 'DDD' & OUT == '12OE' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'DDD' & OUT == '12OS' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'DDD' & OUT == '12ON' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'DDD' & OUT == '12OW' & PEN == '12' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'N' & BIOF12 == 'DDD' & EDG == '12EN' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'DDD' & EDG == '12EW' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'DDD' & EDG == '12ES' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'DDD' & EDG == '12EE' & PEN == '12' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'N' & BIOF12 == 'DDD' & BIGC == '12CNW' & PEN == '12' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'N' & BIOF12 == 'DDD' & BIGC == '12CNE' & PEN == '12' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'N' & BIOF12 == 'DDD' & BIGC == '12CSW' & PEN == '12' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'N' & BIOF12 == 'DDD' & BIGC == '12CSE' & PEN == '12' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'N' & BIOF12 == 'DDD' & CEN == '12MH' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'DDD' & CEN == '12MM' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'DDD' & CEN == '12ML' & PEN == '12' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'N' & BIOF12 == 'DDD' & HID == 'P12HW' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'DDD' & HID == 'P12HE' & PEN == '12' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'N' & BIOF12 == 'DDD' & FS == 'FS12'  & PEN == '12' & SEC >= 0)
    locations.P12[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 14 location summary
    dayfile.bot <- subset(dayfile, SUN == 'N' & BIOF14 == 'DDD' & BOT == 'B' & PEN == '14' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'N' & BIOF14 == 'DDD' & BOT == 'Z' & PEN == '14' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'N' & BIOF14 == 'DDD' & OUT == '14OE' & PEN == '14' & SEC >= 0| SUN == 'N' & BIOF14 == 'DDD' & OUT == '14OS' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'DDD' & OUT == '14ON' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'DDD' & OUT == '14OW' & PEN == '14' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'N' & BIOF14 == 'DDD' & EDG == '14EN' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'DDD' & EDG == '14EW' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'DDD' & EDG == '14ES' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'DDD' & EDG == '14EE' & PEN == '14' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'N' & BIOF14 == 'DDD' & BIGC == '14CNW' & PEN == '14' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'N' & BIOF14 == 'DDD' & BIGC == '14CNE' & PEN == '14' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'N' & BIOF14 == 'DDD' & BIGC == '14CSW' & PEN == '14' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'N' & BIOF14 == 'DDD' & BIGC == '14CSE' & PEN == '14' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'N' & BIOF14 == 'DDD' & CEN == '14MH' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'DDD' & CEN == '14MM' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'DDD' & CEN == '14ML' & PEN == '14' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'N' & BIOF14 == 'DDD' & HID == 'P14HW' & PEN == '14' & SEC >= 0 | SUN == 'N' & BIOF14 == 'DDD' & HID == 'P14HE' & PEN == '14' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'N' & BIOF14 == 'DDD' & FS == 'FS14' & PEN == '14' & SEC >= 0)
    locations.P14[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 15 location summary
    dayfile.bot <- subset(dayfile, SUN == 'N' & BIOF15 == 'DDD' & BOT == 'B' & PEN == '15' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'N' & BIOF15 == 'DDD' & BOT == 'Z' & PEN == '15' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'N' & BIOF15 == 'DDD' & OUT == '15OE' & PEN == '15' & SEC >= 0| SUN == 'N' & BIOF15 == 'DDD' & OUT == '15OS' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'DDD' & OUT == '15ON' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'DDD' & OUT == '15OW' & PEN == '15' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'N' & BIOF15 == 'DDD' & EDG == '15EN' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'DDD' & EDG == '15EW' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'DDD' & EDG == '15ES' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'DDD' & EDG == '15EE' & PEN == '15' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'N' & BIOF15 == 'DDD' & BIGC == '15CNW' & PEN == '15' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'N' & BIOF15 == 'DDD' & BIGC == '15CNE' & PEN == '15' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'N' & BIOF15 == 'DDD' & BIGC == '15CSW' & PEN == '15' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'N' & BIOF15 == 'DDD' & BIGC == '15CSE' & PEN == '15' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'N' & BIOF15 == 'DDD' & CEN == '15MH' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'DDD' & CEN == '15MM' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'DDD' & CEN == '15ML' & PEN == '15' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'N' & BIOF15 == 'DDD' & HID == 'P15HW' & PEN == '15' & SEC >= 0 | SUN == 'N' & BIOF15 == 'DDD' & HID == 'P15HE' & PEN == '15' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'N' & BIOF15 == 'DDD' & FS == 'FS15' & PEN == '15' & SEC >= 0)
    locations.P15[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
  }
  location.sum <- rbind(locations.P12, locations.P14, locations.P15)  
  location.sum$ID <- NULL
  location.sum  
  
  #loadWorkbook('LocationsOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('LocationsOutput.xlsx', location.sum, 'Sheet 1')
  
  write.xlsx(location.sum, 'LocationsOutputNetSwingNightDx3Only.xlsx')
}


#------ DO NOT USE----2b. Batch location summary for multiple day files Before Net Swings: CCC, CC, C ------
batch.locationsnetswingday <- function()
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
    dayfile.bot <- subset(dayfile, SUN == 'D' & BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & BOT == 'B' & PEN == '12' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'D' & BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & BOT == 'Z' & PEN == '12' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'D' & BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & OUT == '12OE' & PEN == '12' & SEC >= 0 | BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & OUT == '12OS' & PEN == '12' & SEC >= 0 | BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & OUT == '12ON' & PEN == '12' & SEC >= 0 | BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & OUT == '12OW' & PEN == '12' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'D' & BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & EDG == '12EN' & PEN == '12' & SEC >= 0 | BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & EDG == '12EW' & PEN == '12' & SEC >= 0 | BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & EDG == '12ES' & PEN == '12' & SEC >= 0 | BIOF12 == 'CCC' & EDG == '12EE' & BIOF12 == 'CC' & BIOF12 == 'C' & PEN == '12' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'D' & BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & BIGC == '12CNW' & PEN == '12' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'D' & BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & BIGC == '12CNE' & PEN == '12' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'D' & BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & BIGC == '12CSW' & PEN == '12' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'D' & BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & BIGC == '12CSE' & PEN == '12' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'D' & BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & CEN == '12MH' & PEN == '12' & SEC >= 0 | BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & CEN == '12MM' & PEN == '12' & SEC >= 0 | BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & CEN == '12ML' & PEN == '12' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'D' & BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & HID == 'P12HW' & PEN == '12' & SEC >= 0 | BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & HID == 'P12HE' & PEN == '12' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'D' & BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & FS == 'FS12'  & PEN == '12' & SEC >= 0)
    locations.P12[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 14 location summary
    dayfile.bot <- subset(dayfile, SUN == 'D' & BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & BOT == 'B' & PEN == '14' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'D' & BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & BOT == 'Z' & PEN == '14' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'D' & BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & OUT == '14OE' & PEN == '14' & SEC >= 0| BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & OUT == '14OS' & PEN == '14' & SEC >= 0 | BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & OUT == '14ON' & PEN == '14' & SEC >= 0 | BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & OUT == '14OW' & PEN == '14' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'D' & BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & EDG == '14EN' & PEN == '14' & SEC >= 0 | BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & EDG == '14EW' & PEN == '14' & SEC >= 0 | BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & EDG == '14ES' & PEN == '14' & SEC >= 0 | BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & EDG == '14EE' & PEN == '14' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'D' & BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & BIGC == '14CNW' & PEN == '14' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'D' & BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & BIGC == '14CNE' & PEN == '14' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'D' & BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & BIGC == '14CSW' & PEN == '14' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'D' & BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & BIGC == '14CSE' & PEN == '14' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'D' & BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & CEN == '14MH' & PEN == '14' & SEC >= 0 | BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & CEN == '14MM' & PEN == '14' & SEC >= 0 | BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & CEN == '14ML' & PEN == '14' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'D' & BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & HID == 'P14HW' & PEN == '14' & SEC >= 0 | BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & HID == 'P14HE' & PEN == '14' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'D' & BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & FS == 'FS14' & PEN == '14' & SEC >= 0)
    locations.P14[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 15 location summary
    dayfile.bot <- subset(dayfile, SUN == 'D' & BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & BOT == 'B' & PEN == '15' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'D' & BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & BOT == 'Z' & PEN == '15' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'D' & BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & OUT == '15OE' & PEN == '15' & SEC >= 0| BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & OUT == '15OS' & PEN == '15' & SEC >= 0 | BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & OUT == '15ON' & PEN == '15' & SEC >= 0 | BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & OUT == '15OW' & PEN == '15' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'D' & BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & EDG == '15EN' & PEN == '15' & SEC >= 0 | BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & EDG == '15EW' & PEN == '15' & SEC >= 0 | BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & EDG == '15ES' & PEN == '15' & SEC >= 0 | BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & EDG == '15EE' & PEN == '15' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'D' & BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & BIGC == '15CNW' & PEN == '15' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'D' & BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & BIGC == '15CNE' & PEN == '15' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'D' & BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & BIGC == '15CSW' & PEN == '15' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'D' & BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & BIGC == '15CSE' & PEN == '15' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'D' & BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & CEN == '15MH' & PEN == '15' & SEC >= 0 | BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & CEN == '15MM' & PEN == '15' & SEC >= 0 | BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & CEN == '15ML' & PEN == '15' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'D' & BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & HID == 'P15HW' & PEN == '15' & SEC >= 0 | BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & HID == 'P15HE' & PEN == '15' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'D' & BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & FS == 'FS15' & PEN == '15' & SEC >= 0)
    locations.P15[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
  }
  location.sum <- rbind(locations.P12, locations.P14, locations.P15)  
  location.sum$ID <- NULL
  location.sum  
  
  #loadWorkbook('LocationsOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('LocationsOutput.xlsx', location.sum, 'Sheet 1')
  
  write.xlsx(location.sum, 'LocationsOutputNetSwingday.xlsx')
}

#------DO NOT USE---------- 2b. Batch location summary for multiple day files Before Net Swings NIGHT: CCC, CC, C ------DO NOT USE----------
batch.locationsnetswingnight <- function()
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
    dayfile.bot <- subset(dayfile, SUN == 'N' & BIOF12 == 'CCC' & BOT == 'B' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'CC' & BOT == 'B' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'C' & BOT == 'B' & PEN == '12' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'N' & BIOF12 == 'CCC' & BOT == 'Z' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'CC' & BOT == 'Z' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'C' & BOT == 'Z' & PEN == '12' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'N' & BIOF12 == 'CCC' & OUT == '12OE' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'CCC' & OUT == '12OS' & PEN == '12' & SEC >= 0 | BIOF12 == 'CCC' & OUT == '12ON' & PEN == '12' & SEC >= 0 | BIOF12 == 'CCC' & OUT == '12OW' & PEN == '12' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'N' & BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & EDG == '12EN' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & EDG == '12EW' & PEN == '12' & SEC >= 0 | SUN == 'N' & BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & EDG == '12ES' & PEN == '12' & SEC >= 0 | BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & EDG == '12EE' & PEN == '12' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'N' & BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & BIGC == '12CNW' & PEN == '12' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'N' & BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & BIGC == '12CNE' & PEN == '12' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'N' & BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & BIGC == '12CSW' & PEN == '12' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'N' & BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & BIGC == '12CSE' & PEN == '12' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'N' & BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & CEN == '12MH' & PEN == '12' & SEC >= 0 | BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & CEN == '12MM' & PEN == '12' & SEC >= 0 | BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & CEN == '12ML' & PEN == '12' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'N' & BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & HID == 'P12HW' & PEN == '12' & SEC >= 0 | BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & HID == 'P12HE' & PEN == '12' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'N' & BIOF12 == 'CCC' & BIOF12 == 'CC' & BIOF12 == 'C' & FS == 'FS12'  & PEN == '12' & SEC >= 0)
    locations.P12[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 14 location summary
    dayfile.bot <- subset(dayfile, SUN == 'N' & BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & BOT == 'B' & PEN == '14' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'N' & BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & BOT == 'Z' & PEN == '14' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'N' & BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & OUT == '14OE' & PEN == '14' & SEC >= 0| BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & OUT == '14OS' & PEN == '14' & SEC >= 0 | BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & OUT == '14ON' & PEN == '14' & SEC >= 0 | BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & OUT == '14OW' & PEN == '14' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'N' & BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & EDG == '14EN' & PEN == '14' & SEC >= 0 | BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & EDG == '14EW' & PEN == '14' & SEC >= 0 | BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & EDG == '14ES' & PEN == '14' & SEC >= 0 | BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & EDG == '14EE' & PEN == '14' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'N' & BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & BIGC == '14CNW' & PEN == '14' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'N' & BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & BIGC == '14CNE' & PEN == '14' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'N' & BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & BIGC == '14CSW' & PEN == '14' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'N' & BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & BIGC == '14CSE' & PEN == '14' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'N' & BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & CEN == '14MH' & PEN == '14' & SEC >= 0 | BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & CEN == '14MM' & PEN == '14' & SEC >= 0 | BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & CEN == '14ML' & PEN == '14' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'N' & BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & HID == 'P14HW' & PEN == '14' & SEC >= 0 | BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & HID == 'P14HE' & PEN == '14' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'N' & BIOF14 == 'CCC' & BIOF14 == 'CC' & BIOF14 == 'C' & FS == 'FS14' & PEN == '14' & SEC >= 0)
    locations.P14[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
    # pen 15 location summary
    dayfile.bot <- subset(dayfile, SUN == 'N' & BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & BOT == 'B' & PEN == '15' & SEC >= 0)
    dayfile.top <- subset(dayfile, SUN == 'N' & BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & BOT == 'Z' & PEN == '15' & SEC >= 0)
    dayfile.out <- subset(dayfile, SUN == 'N' & BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & OUT == '15OE' & PEN == '15' & SEC >= 0| BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & OUT == '15OS' & PEN == '15' & SEC >= 0 | BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & OUT == '15ON' & PEN == '15' & SEC >= 0 | BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & OUT == '15OW' & PEN == '15' & SEC >= 0)
    dayfile.edg <- subset(dayfile, SUN == 'N' & BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & EDG == '15EN' & PEN == '15' & SEC >= 0 | BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & EDG == '15EW' & PEN == '15' & SEC >= 0 | BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & EDG == '15ES' & PEN == '15' & SEC >= 0 | BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & EDG == '15EE' & PEN == '15' & SEC >= 0)
    dayfile.NWCor <- subset(dayfile, SUN == 'N' & BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & BIGC == '15CNW' & PEN == '15' & SEC >= 0)
    dayfile.NECor <- subset(dayfile, SUN == 'N' & BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & BIGC == '15CNE' & PEN == '15' & SEC >= 0)
    dayfile.SWCor <- subset(dayfile, SUN == 'N' & BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & BIGC == '15CSW' & PEN == '15' & SEC >= 0)
    dayfile.SECor <- subset(dayfile, SUN == 'N' & BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & BIGC == '15CSE' & PEN == '15' & SEC >= 0)
    dayfile.cen <- subset(dayfile, SUN == 'N' & BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & CEN == '15MH' & PEN == '15' & SEC >= 0 | BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & CEN == '15MM' & PEN == '15' & SEC >= 0 | BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & CEN == '15ML' & PEN == '15' & SEC >= 0)
    dayfile.hid <- subset(dayfile, SUN == 'N' & BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & HID == 'P15HW' & PEN == '15' & SEC >= 0 | BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & HID == 'P15HE' & PEN == '15' & SEC >= 0)
    dayfile.fs <- subset(dayfile, SUN == 'N' & BIOF15 == 'CCC' & BIOF15 == 'CC' & BIOF15 == 'C' & FS == 'FS15' & PEN == '15' & SEC >= 0)
    locations.P15[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.NWCor$SEC, na.rm = T)/3600, sum(dayfile.NECor$SEC, na.rm = T)/3600, sum(dayfile.SWCor$SEC, na.rm = T)/3600, sum(dayfile.SECor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fs$SEC, na.rm = T)/3600)
    
  }
  location.sum <- rbind(locations.P12, locations.P14, locations.P15)  
  location.sum$ID <- NULL
  location.sum  
  
  #loadWorkbook('LocationsOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('LocationsOutput.xlsx', location.sum, 'Sheet 1')
  
  write.xlsx(location.sum, 'LocationsOutputNetSwingNight.xlsx')
}


##--------------Depth SD Script----------------------
# 6. batch function to return matrix of mean and standard error depths for all fish combined over multiple days

batch.totdepthSD <- function(){
  
  sumfunc <- function(x){ c(min = min(x), max = max(x), range = max(x)-min(x), mean = mean(x), median = median(x), std = sd(x)) }
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  depth.P12 <- data.frame(c('P12_dawn_mean', 'P12_dawn_stdev', 'P12_day_mean', 'P12_day_stdev', 'P12_dusk_mean', 'P12_dusk_stdev', 'P12_night_mean', 'P12_night_stdev'))
  colnames(depth.P12) <- 'ID'
  rownames(depth.P12) <- c('P12_dawn_mean', 'P12_dawn_stdev', 'P12_day_mean', 'P12_day_stdev', 'P12_dusk_mean', 'P12_dusk_stdev', 'P12_night_mean', 'P12_night_stdev')
  depth.P14 <- data.frame(c('P14_dawn_mean', 'P14_dawn_stdev', 'P14_day_mean', 'P14_day_stdev', 'P14_dusk_mean', 'P14_dusk_stdev', 'P14_night_mean', 'P14_night_stdev'))
  colnames(depth.P14) <- 'ID'
  rownames(depth.P14) <- c('P14_dawn_mean', 'P14_dawn_stdev', 'P14_day_mean', 'P14_day_stdev', 'P14_dusk_mean', 'P14_dusk_stdev', 'P14_night_mean', 'P14_night_stdev')
  depth.P15 <- data.frame(c('P15_dawn_mean', 'P15_dawn_stdev', 'P15_day_mean', 'P15_day_stdev', 'P15_dusk_mean', 'P15_dusk_stdev', 'P15_night_mean', 'P15_night_stdev'))
  colnames(depth.P15) <- 'ID'
  rownames(depth.P15) <- c('P15_dawn_mean', 'P15_dawn_stdev', 'P15_day_mean', 'P15_day_stdev', 'P15_dusk_mean', 'P15_dusk_stdev', 'P15_night_mean', 'P15_night_stdev')
  
  for (i in 1:length(files))
  {
    dayfile.loc <- files[[i]]
    dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = dayfile.classes) #read data into table
    
    
    depth.dawn <- subset(dayfile, SUN == 'W' & PEN == '12')
    depth.day <- subset(dayfile, SUN == 'D' & PEN == '12')
    depth.dusk <- subset(dayfile, SUN == 'K' & PEN == '12')
    depth.night <- subset(dayfile, SUN == 'N' & PEN == '12')
    depth.P12[,as.character(i)] <- c(mean(depth.dawn$PosZ), sd(depth.dawn$PosZ), mean(depth.day$PosZ), sd(depth.day$PosZ), mean(depth.dusk$PosZ), sd(depth.dusk$PosZ), mean(depth.night$PosZ), sd(depth.night$PosZ))
    
    depth.dawn <- subset(dayfile, SUN == 'W' & PEN == '14')
    depth.day <- subset(dayfile, SUN == 'D' & PEN == '14')
    depth.dusk <- subset(dayfile, SUN == 'K' & PEN == '14')
    depth.night <- subset(dayfile, SUN == 'N' & PEN == '14')
    depth.P14[,as.character(i)] <- c(mean(depth.dawn$PosZ), sd(depth.dawn$PosZ), mean(depth.day$PosZ), sd(depth.day$PosZ), mean(depth.dusk$PosZ), sd(depth.dusk$PosZ), mean(depth.night$PosZ), sd(depth.night$PosZ))
    
    depth.dawn <- subset(dayfile, SUN == 'W' & PEN == '15')
    depth.day <- subset(dayfile, SUN == 'D' & PEN == '15')
    depth.dusk <- subset(dayfile, SUN == 'K' & PEN == '15')
    depth.night <- subset(dayfile, SUN == 'N' & PEN == '15')
    depth.P15[,as.character(i)] <- c(mean(depth.dawn$PosZ), sd(depth.dawn$PosZ), mean(depth.day$PosZ), sd(depth.day$PosZ), mean(depth.dusk$PosZ), sd(depth.dusk$PosZ), mean(depth.night$PosZ), sd(depth.night$PosZ))
    
  }
  
  depths.sum <- rbind(depth.P12, depth.P14, depth.P15)  
  #depths.sum$ID <- NULL
  depths.sum    
  #loadWorkbook('DepthTotOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('DepthTotOutput.xlsx', depths.sum, 'Sheet 1')
  
  write.xlsx(depths.sum, 'DepthTotOutputStdev.xlsx')
}

