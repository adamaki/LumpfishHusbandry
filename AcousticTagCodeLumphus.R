#Delousing efficiency project data coding
#Lumpfish husbandry study
#Adam Brooker
#12th June 2018

# library(RMySQL)
library(rJava)
library(XLConnectJars)
library(XLConnect) 
library(openxlsx)
options(java.parameters = "-Xmx32000m")
library(dplyr)
library(tidyr)

#ENTER YOUR VARIABLES HERE
workingdir <- ifelse(Sys.info()['user'] == 'Laptop', "G:/Data/2018 Lumpfish Husbandry/Data processing/5. Day CSV", 'C:/Macpath') # change to location of data
dayfile.loc <- "R1_LBF18S100192_day.csv" # change to file to be analysed
masterfileloc <- "G:/Data/2018 Lumpfish Husbandry/AcousticTagFile_2018v6.xlsx" # change to location of AcousticTagFile.xlsx
day <- '192' # day of the year
bottom.threshold <- 15 # threshold for fish at bottom of cage coding (depth in metres)
water.height <- 35
rot.ang <- 14.24 # grid rotation angle in radians
UTMeast <- -1230064.57 # grid origin x-axis
UTMnorth <- 6170474.26 #  grid origin y-axis

# Enter periods of hide tags
hidetag12ET <- 15919
hidetag12EB <- 15863
hidetag12WT <- 15695
hidetag12WB <- 15471
hidetag14ET <- 15751
hidetag14EB <- 15527
hidetag14WT <- 15639
hidetag14WB <- 15807
hidetag15ET <- 15415
hidetag15EB <- 15583
hidetag15WT <- 15975
hidetag15WB <- 15359

# Toggle hides in or out of water

io12E <- F
io12W <- T
io14E <- F
io14W <- T
io15E <- T
io15W <- F

# DON'T CHANGE ANYTHING AFTER THIS LINE UNLESS YOU KNOW WHAT YOU ARE DOING!

#------------------------------------------------------------------------------------------------------------------------------
# LOAD LOOKUP TABLES


# LOAD MASTERCODE
mastercode <- readWorksheetFromFile(masterfileloc, sheet = 10, startRow = 6, endCol = 100, colTypes = 'character') # read in mastercode from Acoustic Tag File
#mastercode <- read.xlsx(masterfileloc, sheetName = 'MasterCode', startRow = 6, endRow = 99, colIndex = seq(5, 41), colClasses = rep('character', 37))
rownames(mastercode) <- mastercode$DAY # rename mastercode rows by day
mastercode$DATE <- substr(mastercode$DATE, 1, 10)

mastercode$SUN_N_S <- convert.to.date(col = mastercode$SUN_N_S)
mastercode$SUN_N_E <- convert.to.date(col = mastercode$SUN_N_E)
mastercode$SUN_W_S <- convert.to.date(col = mastercode$SUN_W_S)
mastercode$SUN_W_E <- convert.to.date(col = mastercode$SUN_W_E)
mastercode$SUN_D_S <- convert.to.date(col = mastercode$SUN_D_S)
mastercode$SUN_D_E <- convert.to.date(col = mastercode$SUN_D_E)
mastercode$SUN_K_S <- convert.to.date(col = mastercode$SUN_K_S)
mastercode$SUN_K_E <- convert.to.date(col = mastercode$SUN_K_E)
mastercode$SUN_N_S2 <- convert.to.date(col = mastercode$SUN_N_S2)
mastercode$SUN_N_E2 <- convert.to.date(col = mastercode$SUN_N_E2)

mastercode$TID_L_S <- convert.to.date(col = mastercode$TID_L_S)
mastercode$TID_L_E <- convert.to.date(col = mastercode$TID_L_E)
mastercode$TID_LH_S <- convert.to.date(col = mastercode$TID_LH_S)
mastercode$TID_LH_E <- convert.to.date(col = mastercode$TID_LH_E)
mastercode$TID_H_S <- convert.to.date(col = mastercode$TID_H_S)
mastercode$TID_H_E <- convert.to.date(col = mastercode$TID_H_E)
mastercode$TID_HL_S <- convert.to.date(col = mastercode$TID_HL_S)
mastercode$TID_HL_E <- convert.to.date(col = mastercode$TID_HL_E)
mastercode$TID_L_S2 <- convert.to.date(col = mastercode$TID_L_S2)
mastercode$TID_L_E2 <- convert.to.date(col = mastercode$TID_L_E2)
mastercode$TID_LH_S2 <- convert.to.date(col = mastercode$TID_LH_S2)
mastercode$TID_LH_E2 <- convert.to.date(col = mastercode$TID_LH_E2)
mastercode$TID_H_S2 <- convert.to.date(col = mastercode$TID_H_S2)
mastercode$TID_H_E2 <- convert.to.date(col = mastercode$TID_H_E2)
mastercode$TID_HL_S2 <- convert.to.date(col = mastercode$TID_HL_S2)
mastercode$TID_HL_E2 <- convert.to.date(col = mastercode$TID_HL_E2)

mastercode$SMEAL_P12_N_S <- convert.to.date(col = mastercode$SMEAL_P12_N_S)
mastercode$SMEAL_P12_N_E <- convert.to.date(col = mastercode$SMEAL_P12_N_E)
mastercode$SMEAL_P12_Y_S <- convert.to.date(col = mastercode$SMEAL_P12_Y_S)
mastercode$SMEAL_P12_Y_E <- convert.to.date(col = mastercode$SMEAL_P12_Y_E)
mastercode$SMEAL_P12_N_S2 <- convert.to.date(col = mastercode$SMEAL_P12_N_S2)
mastercode$SMEAL_P12_N_E2 <- convert.to.date(col = mastercode$SMEAL_P12_N_E2)
mastercode$SMEAL_P12_Y_S2 <- convert.to.date(col = mastercode$SMEAL_P12_Y_S2)
mastercode$SMEAL_P12_Y_E2 <- convert.to.date(col = mastercode$SMEAL_P12_Y_E2)
mastercode$SMEAL_P12_N_S3 <- convert.to.date(col = mastercode$SMEAL_P12_N_S3)
mastercode$SMEAL_P12_N_E3 <- convert.to.date(col = mastercode$SMEAL_P12_N_E3)
mastercode$SMEAL_P14_N_S <- convert.to.date(col = mastercode$SMEAL_P14_N_S)
mastercode$SMEAL_P14_N_E <- convert.to.date(col = mastercode$SMEAL_P14_N_E)
mastercode$SMEAL_P14_Y_S <- convert.to.date(col = mastercode$SMEAL_P14_Y_S)
mastercode$SMEAL_P14_Y_E <- convert.to.date(col = mastercode$SMEAL_P14_Y_E)
mastercode$SMEAL_P14_N_S2 <- convert.to.date(col = mastercode$SMEAL_P14_N_S2)
mastercode$SMEAL_P14_N_E2 <- convert.to.date(col = mastercode$SMEAL_P14_N_E2)
mastercode$SMEAL_P14_Y_S2 <- convert.to.date(col = mastercode$SMEAL_P14_Y_S2)
mastercode$SMEAL_P14_Y_E2 <- convert.to.date(col = mastercode$SMEAL_P14_Y_E2)
mastercode$SMEAL_P14_N_S3 <- convert.to.date(col = mastercode$SMEAL_P14_N_S3)
mastercode$SMEAL_P14_N_E3 <- convert.to.date(col = mastercode$SMEAL_P14_N_E3)
mastercode$SMEAL_P15_N_S <- convert.to.date(col = mastercode$SMEAL_P15_N_S)
mastercode$SMEAL_P15_N_E <- convert.to.date(col = mastercode$SMEAL_P15_N_E)
mastercode$SMEAL_P15_Y_S <- convert.to.date(col = mastercode$SMEAL_P15_Y_S)
mastercode$SMEAL_P15_Y_E <- convert.to.date(col = mastercode$SMEAL_P15_Y_E)
mastercode$SMEAL_P15_N_S2 <- convert.to.date(col = mastercode$SMEAL_P15_N_S2)
mastercode$SMEAL_P15_N_E2 <- convert.to.date(col = mastercode$SMEAL_P15_N_E2)
mastercode$SMEAL_P15_Y_S2 <- convert.to.date(col = mastercode$SMEAL_P15_Y_S2)
mastercode$SMEAL_P15_Y_E2 <- convert.to.date(col = mastercode$SMEAL_P15_Y_E2)
mastercode$SMEAL_P15_N_S3 <- convert.to.date(col = mastercode$SMEAL_P15_N_S3)
mastercode$SMEAL_P15_N_E3 <- convert.to.date(col = mastercode$SMEAL_P15_N_E3)

detach("package:xlsx") ##so that read.xlsx will work for loading fish id and making csv.


#LOAD FISH ID DATA
fishid_tbl <- read.xlsx(masterfileloc, sheet = 3, rows = seq(19, 103), cols = c(1, 4, 5, 8)) # read in code from Fish ID lookup table
fishid_tbl$L_m <- round(as.numeric(fishid_tbl$L_m), digits = 3)

#LOAD LOCATIONS CODING DATA
locations.lookup <- read.xlsx(masterfileloc, sheet = 11, startRow = 1, cols = seq(1, 7)) # read in codes from Locations Coding spreadsheet
rownames(locations.lookup) <- locations.lookup$Code

#LOAD ENVIRONMENTAL PROBE READINGS
probe.DOT2 <- read.xlsx(masterfileloc, sheet = 12, startRow = 3, cols = c(1, 2, 3))
probe.DOT2$DO.time.2m <- as.POSIXct(strptime(probe.DOT2$DO.time.2m, "%Y-%m-%d %H:%M:%S", tz = 'UTC'))
probe.DOT2$DO.time.2m <- probe.DOT2$DO.time.2m - as.difftime(1, unit = 'hours')
#probe.DOT2 <- probe.DOT2 %>% mutate_each(funs(round(.,2)), DO.2m, Temp.2m)
probe.DOT2 <- probe.DOT2 %>% mutate_at(vars(DO.2m, Temp.2m), funs(round(.,2)))
probe.sal2 <- read.xlsx(masterfileloc, sheet = 12, startRow = 3, cols = c(4, 5))
probe.sal2$Sal.time.2m <- as.POSIXct(strptime(probe.sal2$Sal.time.2m, "%Y-%m-%d %H:%M:%S", tz = 'UTC'))
probe.sal2$Sal.time.2m <- probe.sal2$Sal.time.2m - as.difftime(1, unit = 'hours')
probe.sal2 <- probe.sal2 %>% mutate(Sal.2m = round(Sal.2m, 2))
probe.DOT4 <- read.xlsx(masterfileloc, sheet = 12, startRow = 3, cols = c(6, 7, 8))
probe.DOT4$DO.time.4m <- as.POSIXct(strptime(probe.DOT4$DO.time.4m, "%Y-%m-%d %H:%M:%S", tz = 'UTC'))
probe.DOT4$DO.time.4m <- probe.DOT4$DO.time.4m - as.difftime(1, unit = 'hours')
#probe.DOT4 <- probe.DOT4 %>% mutate_each(funs(round(.,2)), DO.4m, Temp.4m)
probe.DOT4 <- probe.DOT4 %>% mutate_at(vars(DO.4m, Temp.4m), funs(round(.,2)))
probe.sal4 <- read.xlsx(masterfileloc, sheet = 12, startRow = 3, cols = c(9, 10))
probe.sal4$Sal.time.4m <- as.POSIXct(strptime(probe.sal4$Sal.time.4m, "%Y-%m-%d %H:%M:%S", tz = 'UTC'))
probe.sal4$Sal.time.4m <- probe.sal4$Sal.time.4m - as.difftime(1, unit = 'hours')
probe.sal4 <- probe.sal4 %>% mutate(Sal.4m = round(Sal.4m, 2))
probe.DOT7 <- read.xlsx(masterfileloc, sheet = 12, startRow = 3, cols = c(11, 12, 13))
probe.DOT7$DO.time.7m <- as.POSIXct(strptime(probe.DOT7$DO.time.7m, "%Y-%m-%d %H:%M:%S", tz = 'UTC'))
probe.DOT7$DO.time.7m <- probe.DOT7$DO.time.7m - as.difftime(1, unit = 'hours')
#probe.DOT7 <- probe.DOT7 %>% mutate_each(funs(round(.,2)), DO.7m, Temp.7m)
probe.DOT7 <- probe.DOT7 %>% mutate_at(vars(DO.7m, Temp.7m), funs(round(.,2)))
probe.sal7 <- read.xlsx(masterfileloc, sheet = 12, startRow = 3, cols = c(14, 15))
probe.sal7$Sal.time.7m <- as.POSIXct(strptime(probe.sal7$Sal.time.7m, "%Y-%m-%d %H:%M:%S", tz = 'UTC'))
probe.sal7$Sal.time.7m <- probe.sal7$Sal.time.7m - as.difftime(1, unit = 'hours')
probe.sal7 <- probe.sal7 %>% mutate(Sal.7m = round(Sal.7m, 2))
probe.DOT10 <- read.xlsx(masterfileloc, sheet = 12, startRow = 3, cols = c(16, 17, 18))
probe.DOT10$DO.time.10m <- as.POSIXct(strptime(probe.DOT10$DO.time.10m, "%Y-%m-%d %H:%M:%S", tz = 'UTC'))
probe.DOT10$DO.time.10m <- probe.DOT10$DO.time.10m - as.difftime(1, unit = 'hours')
#probe.DOT10 <- probe.DOT10 %>% mutate_each(funs(round(.,2)), DO.10m, Temp.10m)
probe.DOT10 <- probe.DOT10 %>% mutate_at(vars(DO.10m, Temp.10m), funs(round(.,2)))
probe.sal10 <- read.xlsx(masterfileloc, sheet = 12, startRow = 3, cols = c(19, 20))
probe.sal10$Sal.time.10m <- as.POSIXct(strptime(probe.sal10$Sal.time.10m, "%Y-%m-%d %H:%M:%S", tz = 'UTC'))
probe.sal10$Sal.time.10m <- probe.sal10$Sal.time.10m - as.difftime(1, unit = 'hours')
probe.sal10 <- probe.sal10 %>% mutate(Sal.10m = round(Sal.10m, 2))


# ----------------------------------------------------------------------------------------------------------------------------------------


#CODING

setwd(workingdir) 


# LOAD HOURFILE (for when coding hourfiles instead of dayfiles)
#dayfile_tbl <- read.csv(dayfile, header = TRUE, sep = ",", colClasses = c('NULL', 'NULL', 'NULL', 'character', 'character', 'NULL', 
#                                                                           'character', 'character', 'character', 'character', 'NULL', 'NULL', 
#                                                                           'NULL', 'NULL', 'NULL', 'NULL', 'NULL', 'NULL', 
#                                                                           'NULL')) #read data into table

# LOAD DAYFILE
dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = c('NULL', 'NULL', 'NULL', 'NULL', 'character', 'character', 'NULL', 
                                                                          'character', 'character', 'character', 'character', 'NULL', 'NULL', 
                                                                          'NULL', 'NULL', 'NULL', 'NULL', 'NULL', 'NULL', 
                                                                          'NULL')) #read data into table


#CONVERT FIELDS INTO CORRECT FORMATS
dayfile$Period <- sapply(dayfile$Period, as.numeric)
dayfile$SubCode <- sapply(dayfile$SubCode, as.numeric)
dayfile[, 'EchoTime'] <- as.POSIXct(strptime(dayfile[,'EchoTime'], "%d/%m/%Y %H:%M:%S", tz = "UTC")) # convert character format to date and time format
dayfile$PosX <- as.numeric(dayfile$PosX)
dayfile$PosY <- as.numeric(dayfile$PosY)
dayfile$PosZ <- as.numeric(dayfile$PosZ)

# TRANSLATE  COORDINATES INTO POSITIVE DEPTH AND ZERO ORIGIN

dayfile$PosX2 <- round((cos(rot.ang*pi/180)*dayfile$PosX-sin(rot.ang*pi/180)*dayfile$PosY)-UTMeast, digits = 2)
dayfile$PosY2 <- round((sin(rot.ang*pi/180)*dayfile$PosX+cos(rot.ang*pi/180)*dayfile$PosY)-UTMnorth, digits = 2)
dayfile$PosX <- dayfile$PosX2
dayfile$PosY <- dayfile$PosY2
dayfile$PosX2 <- NULL
dayfile$PosY2 <- NULL
dayfile$PosZ <- water.height-dayfile$PosZ

#REMOVE HIDE TAGS
#dayfile_tbl <- dayfile_tbl[!(dayfile_tbl$Period == hidetag1 | dayfile_tbl$Period == hidetag2 | dayfile_tbl$Period == hidetag3 | 
#               dayfile_tbl$Period == hidetag4 | dayfile_tbl$Period == hidetag5 | dayfile_tbl$Period == hidetag6 | dayfile_tbl$Period == hidetag7 | dayfile_tbl$Period == hidetag8),]

#REMOVE PINGS ABOVE WATER SURFACE
dayfile <- dayfile[!(dayfile$PosZ < 0),]

#SORT BY TIME AND TAG
dayfile <- arrange(dayfile, Period, EchoTime) # sort by time and tag

#ADD PEN NUMBER
pen.lookup <- fishid_tbl$Pen # create pen lookup table
names(pen.lookup) <- fishid_tbl$Period
dayfile$PEN <- as.numeric(pen.lookup[as.character(dayfile$Period)]) # add pen number to day file
dayfile <- dayfile[,c('Period', 'SubCode', 'PEN', 'EchoTime', 'PosX', 'PosY', 'PosZ')] # reorder fields


# HIDE CODING

#select either west or east hides in water
if(io12E == T){hidetag12T <- hidetag12ET} else {hidetag12T <- hidetag12WT}
if(io12E == T){hidetag12B <- hidetag12EB} else {hidetag12B <- hidetag12WB}
if(io14E == T){hidetag14T <- hidetag14ET} else {hidetag14T <- hidetag14WT}
if(io14E == T){hidetag14B <- hidetag14EB} else {hidetag14B <- hidetag14WB}
if(io15E == T){hidetag15T <- hidetag15ET} else {hidetag15T <- hidetag15WT}
if(io15E == T){hidetag15B <- hidetag15EB} else {hidetag15B <- hidetag15WB}

hides <- subset(dayfile, (Period == hidetag12T | Period ==  hidetag12B | Period == hidetag14T | Period == hidetag14B | Period == hidetag15T | Period == hidetag15B)) # subset hide data to hides dataset
dayfile <- subset(dayfile, !(Period == hidetag12T | Period ==  hidetag12B | Period == hidetag14T | Period == hidetag14B | Period == hidetag15T | Period == hidetag15B)) # remove hide data from dayfile


#CALCULATE TIMES AND SPEEDS
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
dayfile <- subset(dayfile, dayfile$SEC >5 | is.na(dayfile$SEC) == TRUE) # remove entries where time delay too low


#CALCULATE BODY LENGTHS/SEC
fishid.bl.lookup <- fishid_tbl$L_m # create fish ID lookup table
names(fishid.bl.lookup) <- fishid_tbl$Period
dayfile$BL <- as.numeric(fishid.bl.lookup[as.character(dayfile$Period)]) # add fish lengths to day file
dayfile$BLSEC <- round(dayfile$MSEC/dayfile$BL, 3) # calculate BL per sec
dayfile <- subset(dayfile, dayfile$BLSEC < 10 | is.na(dayfile$BLSEC) == TRUE) # remove entries where swimming speed is greater than 20 BL/sec (likely multipath)

#CALCULATE HEADING, TURN ANGLES AND TURN RATE
heading.func()
dayfile$HEAD <- c(NA, heading)
rm(heading)

turn.angles()
dayfile$TURN <- c(NA, NA, theta)
dayfile$TURNRATE <- dayfile$TURN/dayfile$SEC
rm(theta)

#ENTER CODES FROM MASTERCODE
dayfile$BIOF12 <- as.factor(mastercode[day,'BIOF12'])         
dayfile$BIOF14 <- as.factor(mastercode[day,'BIOF14']) 
dayfile$BIOF15 <- as.factor(mastercode[day,'BIOF15'])
dayfile$WVIS <- as.factor(mastercode[day,'WVIS'])                          
dayfile$MOON <- as.factor(mastercode[day,'MOON']) 

#LICE DATA # commented out where no lice data collected yet
dayfile$TOT_P12 <- as.numeric(mastercode[day,'LICE_P12_TOT']) 
dayfile$PA_A_P12 <- as.numeric(mastercode[day,'LICE_P12_FGPAA']) 
dayfile$FG_P12 <- as.numeric(mastercode[day,'LICE_P12_FG']) 
dayfile$A_P12 <- as.numeric(mastercode[day,'LICE_P12_A']) 
dayfile$PA_P12 <- as.numeric(mastercode[day,'LICE_P12_PA']) 
dayfile$CHAL_P12 <- as.numeric(mastercode[day,'LICE_P12_CHAL']) 
dayfile$CAL_P12 <- as.numeric(mastercode[day,'LICE_P12_CAL']) 
dayfile$TOT_P14 <- as.numeric(mastercode[day,'LICE_P14_TOT']) 
dayfile$PA_A_P14 <- as.numeric(mastercode[day,'LICE_P14_FGPAA']) 
dayfile$FG_P14 <- as.numeric(mastercode[day,'LICE_P14_FG']) 
dayfile$A_P14 <- as.numeric(mastercode[day,'LICE_P14_A']) 
dayfile$PA_P14 <- as.numeric(mastercode[day,'LICE_P14_PA']) 
dayfile$CHAL_P14 <- as.numeric(mastercode[day,'LICE_P14_CHAL']) 
dayfile$CAL_P14 <- as.numeric(mastercode[day,'LICE_P14_CAL']) 
dayfile$TOT_P15 <- as.numeric(mastercode[day,'LICE_P15_TOT']) 
dayfile$PA_A_P15 <- as.numeric(mastercode[day,'LICE_P15_FGPAA']) 
dayfile$FG_P15 <- as.numeric(mastercode[day,'LICE_P15_FG']) 
dayfile$A_P15 <- as.numeric(mastercode[day,'LICE_P15_A']) 
dayfile$PA_P15 <- as.numeric(mastercode[day,'LICE_P15_PA']) 
dayfile$CHAL_P15 <- as.numeric(mastercode[day,'LICE_P15_CHAL']) 
dayfile$CAL_P15 <- as.numeric(mastercode[day,'LICE_P15_CAL']) 

# temporary NAs where no data collected yet
#dayfile$TOT_P12 <- NA 
#dayfile$PA_A_P12 <- NA 
#dayfile$FG_P12 <- NA 
#dayfile$A_P12 <- NA 
#dayfile$PA_P12 <- NA 
#dayfile$CHAL_P12 <- NA 
#dayfile$CAL_P12 <- NA 
#dayfile$TOT_P14 <- NA 
#dayfile$PA_A_P14 <- NA 
#dayfile$FG_P14 <- NA
#dayfile$A_P14 <- NA
#dayfile$PA_P14 <- NA
#dayfile$CHAL_P14 <- NA
#dayfile$CAL_P14 <- NA 
#dayfile$TOT_P15 <- NA
#dayfile$PA_A_P15 <- NA
#dayfile$FG_P15 <- NA
#dayfile$A_P15 <- NA 
#dayfile$PA_P15 <- NA
#dayfile$CHAL_P15 <- NA
#dayfile$CAL_P15 <- NA


#LOCATIONS CODING
dayfile$BOT <- as.factor(ifelse(dayfile$PosZ >= bottom.threshold, 'B', 'Z')) # at cage bottom
dayfile$OUT <- as.factor(locationcode(n12code = '12ON', w12code = '12OW', s12code = '12OS', e12code = '12OE', n14code = '14ON', w14code = '14OW', s14code = '14OS', e14code = '14OE', n15code = '15ON', w15code = '15OW', s15code = '15OS', e15code = '15OE')) # fish outside cage
dayfile$EDG <- as.factor(locationcode(n12code = '12EN', w12code = '12EW', s12code = '12ES', e12code = '12EE', n14code = '14EN', w14code = '14EW', s14code = '14ES', e14code = '14EE', n15code = '15EN', w15code = '15EW', s15code = '15ES', e15code = '15EE')) # fish at edge of cage
dayfile$BIGC <- as.factor(locationcode(n12code = '12CNW', w12code = '12CSW', s12code = '12CSE', e12code = '12CNE', n14code = '14CNW', w14code = '14CSW', s14code = '14CSE', e14code = '14CNE', n15code = '15CNW', w15code = '15CSW', s15code = '15CSE', e15code = '15CNE')) # fish in corners
dayfile$HID <- as.factor(hidecode())
#dayfile$HID <- as.factor(dynamic.hidecode(p7nwhide = '7WHNW', p7sehide = '7WHSE', p8swhide = '8WHSW', p8nehide = '8WHNE', radius = 1, depth = 2, height = 1)) # fish in hides
dayfile$CEN <- as.factor(centrecode(highcode12 = '12MH', midcode12 = '12MM', lowcode12 = '12ML', highcode14 = '14MH', midcode14 = '14MM', lowcode14 = '14ML', highcode15 = '15MH', midcode15 = '15MM', lowcode15 = '15ML')) # fish in centre of cage
dayfile$FS <- as.factor(atfeedcode(p12fscode = 'FS12', p14fscode = 'FS14', p15fscode = 'FS15')) # fish at feed stations


#SUN AND TIDES CODING
dayfile$SUN <- suncode() # sun phase code
dayfile$TID <- tidecode() # tide phase code
dayfile$PHASE <- as.factor(mastercode[day,'PHASE']) # tidal phase (spring/neap)


#MEAL TIMES CODING # commented out where no data collected yet
dayfile$SMEAL12 <- smealcode(pen = 'P12') # salmon feeding times cage 12 code
dayfile$SMEAL14 <- smealcode(pen = 'P14') # salmon feeding times cage 14 code
dayfile$SMEAL15 <- smealcode(pen = 'P15') # salmon feeding times cage 14 code

# temporary NAs where no salmon meal data collected yet
#dayfile$SMEAL12 <- NA
#dayfile$SMEAL14 <- NA
#dayfile$SMEAL15 <- NA

#ENVIRONMENTAL DATA

all <- data.frame(ts = unique(unlist(c(dayfile$EchoTime, probe.DOT2$DO.time.2m))))

all <- all %>%
  left_join(dayfile, by=c("ts"="EchoTime")) %>%
  left_join(probe.DOT2, by=c("ts" = "DO.time.2m")) %>%
  arrange(ts) %>%
  fill(DO.2m) %>%
  fill(Temp.2m) %>%
  filter(!is.na(PosX)) %>%
  arrange(Period, ts) 

dayfile$O2 <- all$DO.2m
dayfile$T2 <- all$Temp.2m

all <- data.frame(ts = unique(unlist(c(dayfile$EchoTime, probe.sal2$Sal.time.2m))))

all <- all %>%
  left_join(dayfile, by=c("ts"="EchoTime")) %>%
  left_join(probe.sal2, by=c("ts" = "Sal.time.2m")) %>%
  arrange(ts) %>%
  fill(Sal.2m) %>%
  filter(!is.na(PosX)) %>%
  arrange(Period, ts) 

dayfile$S2 <- all$Sal.2m

all <- data.frame(ts = unique(unlist(c(dayfile$EchoTime, probe.DOT4$DO.time.4m))))

all <- all %>%
  left_join(dayfile, by=c("ts"="EchoTime")) %>%
  left_join(probe.DOT4, by=c("ts" = "DO.time.4m")) %>%
  arrange(ts) %>%
  fill(DO.4m) %>%
  fill(Temp.4m) %>%
  filter(!is.na(PosX)) %>%
  arrange(Period, ts) 

dayfile$O4 <- all$DO.4m
dayfile$T4 <- all$Temp.4m

all <- data.frame(ts = unique(unlist(c(dayfile$EchoTime, probe.sal4$Sal.time.4m))))

all <- all %>%
  left_join(dayfile, by=c("ts"="EchoTime")) %>%
  left_join(probe.sal4, by=c("ts" = "Sal.time.4m")) %>%
  arrange(ts) %>%
  fill(Sal.4m) %>%
  filter(!is.na(PosX)) %>%
  arrange(Period, ts) 

dayfile$S4 <- all$Sal.4m

all <- data.frame(ts = unique(unlist(c(dayfile$EchoTime, probe.DOT7$DO.time.7m))))

all <- all %>%
  left_join(dayfile, by=c("ts"="EchoTime")) %>%
  left_join(probe.DOT7, by=c("ts" = "DO.time.7m")) %>%
  arrange(ts) %>%
  fill(DO.7m) %>%
  fill(Temp.7m) %>%
  filter(!is.na(PosX)) %>%
  arrange(Period, ts) 

dayfile$O7 <- all$DO.7m
dayfile$T7 <- all$Temp.7m

all <- data.frame(ts = unique(unlist(c(dayfile$EchoTime, probe.sal7$Sal.time.7m))))

all <- all %>%
  left_join(dayfile, by=c("ts"="EchoTime")) %>%
  left_join(probe.sal7, by=c("ts" = "Sal.time.7m")) %>%
  arrange(ts) %>%
  fill(Sal.7m) %>%
  filter(!is.na(PosX)) %>%
  arrange(Period, ts) 

dayfile$S7 <- all$Sal.7m

all <- data.frame(ts = unique(unlist(c(dayfile$EchoTime, probe.DOT10$DO.time.10m))))

all <- all %>%
  left_join(dayfile, by=c("ts"="EchoTime")) %>%
  left_join(probe.DOT10, by=c("ts" = "DO.time.10m")) %>%
  arrange(ts) %>%
  fill(DO.10m) %>%
  fill(Temp.10m) %>%
  filter(!is.na(PosX)) %>%
  arrange(Period, ts) 

dayfile$O10 <- all$DO.10m
dayfile$T10 <- all$Temp.10m

all <- data.frame(ts = unique(unlist(c(dayfile$EchoTime, probe.sal10$Sal.time.10m))))

all <- all %>%
  left_join(dayfile, by=c("ts"="EchoTime")) %>%
  left_join(probe.sal10, by=c("ts" = "Sal.time.10m")) %>%
  arrange(ts) %>%
  fill(Sal.10m) %>%
  filter(!is.na(PosX)) %>%
  arrange(Period, ts) 

dayfile$S10 <- all$Sal.10m

rm(all)

#FINISH CODE
write.csv(dayfile, file = sub(".csv", "_coded.csv", dayfile.loc, ignore.case = FALSE, fixed = T)) #write output to file
write.csv(hides, file = sub(".csv", "_hides.csv", dayfile.loc, ignore.case = FALSE, fixed = T)) #write output to file








# script to recode feeding stations retrospectively------------------------------------------------------------------------------

system.time({
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  
  for(i in 1:length(files)){
    
    dayfile <- read.csv(files[[i]], header = TRUE, sep = ",", colClasses = dayfile.classes)  
    
    dayfile$FS <- as.factor(atfeedcode(p12fscode = 'FS12', p14fscode = 'FS14', p15fscode = 'FS15')) # fish at feed stations
    
    write.csv(dayfile, file = files[[i]]) #write output to file
    
  }
  
  
})


#----------------------------------------------------------------------------------------------------------------------------------------

# FUNCTIONS

convert.to.date <- function(column = col) {
  as.POSIXct(strptime(paste(mastercode$DATE, substr(column, 12, 19), sep = " "), "%Y-%m-%d %H:%M:%S", tz = "UTC"))
}

# function to code for sun phase
suncode <- function(daycode = day) {
  ifelse(as.numeric(dayfile$EchoTime) > as.numeric(mastercode[daycode,'SUN_N_S']) & as.numeric(dayfile$EchoTime) < as.numeric(mastercode[daycode,'SUN_N_E']), 'N', ifelse
         (as.numeric(dayfile$EchoTime) > as.numeric(mastercode[daycode,'SUN_W_S']) & as.numeric(dayfile$EchoTime) < as.numeric(mastercode[daycode,'SUN_W_E']), 'W', ifelse
         (as.numeric(dayfile$EchoTime) > as.numeric(mastercode[daycode,'SUN_D_S']) & as.numeric(dayfile$EchoTime) < as.numeric(mastercode[daycode,'SUN_D_E']), 'D', ifelse
         (as.numeric(dayfile$EchoTime) > as.numeric(mastercode[daycode,'SUN_K_S']) & as.numeric(dayfile$EchoTime) < as.numeric(mastercode[daycode,'SUN_K_E']), 'K', ifelse
         (as.numeric(dayfile$EchoTime) > as.numeric(mastercode[daycode,'SUN_N_S2']) & as.numeric(dayfile$EchoTime) < as.numeric(mastercode[daycode,'SUN_N_E2']), 'N', ' '
         )))))
}

# function to code for tide phase
tidecode <- function(daycode = day) {
  ifelse(as.numeric(dayfile$EchoTime) > as.numeric(mastercode[daycode,'TID_L_S']) & as.numeric(dayfile$EchoTime) < as.numeric(mastercode[daycode,'TID_L_E']), 'L', ifelse
         (as.numeric(dayfile$EchoTime) > as.numeric(mastercode[daycode,'TID_LH_S']) & as.numeric(dayfile$EchoTime) < as.numeric(mastercode[daycode,'TID_LH_E']), 'LH', ifelse
         (as.numeric(dayfile$EchoTime) > as.numeric(mastercode[daycode,'TID_H_S']) & as.numeric(dayfile$EchoTime) < as.numeric(mastercode[daycode,'TID_H_E']), 'H', ifelse
         (as.numeric(dayfile$EchoTime) > as.numeric(mastercode[daycode,'TID_HL_S']) & as.numeric(dayfile$EchoTime) < as.numeric(mastercode[daycode,'TID_HL_E']), 'HL', ifelse
         (as.numeric(dayfile$EchoTime) > as.numeric(mastercode[daycode,'TID_L_S2']) & as.numeric(dayfile$EchoTime) < as.numeric(mastercode[daycode,'TID_L_E2']), 'L', ifelse
         (as.numeric(dayfile$EchoTime) > as.numeric(mastercode[daycode,'TID_LH_S2']) & as.numeric(dayfile$EchoTime) < as.numeric(mastercode[daycode,'TID_LH_E2']), 'LH', ifelse
         (as.numeric(dayfile$EchoTime) > as.numeric(mastercode[daycode,'TID_H_S2']) & as.numeric(dayfile$EchoTime) < as.numeric(mastercode[daycode,'TID_H_E2']), 'H', ifelse
         (as.numeric(dayfile$EchoTime) > as.numeric(mastercode[daycode,'TID_HL_S2']) & as.numeric(dayfile$EchoTime) < as.numeric(mastercode[daycode,'TID_HL_E2']), 'HL', 'Z'
         ))))))))
}

# function to code for salmon feeding times
smealcode <- function(daycode = day, pennum = pen) {
  ifelse(as.numeric(dayfile$EchoTime) > as.numeric(mastercode[daycode, paste('SMEAL_', pennum, '_N_S', sep = "")]) &
           as.numeric(dayfile$EchoTime) < as.numeric(mastercode[daycode, paste('SMEAL_', pennum, '_N_E', sep = "")]), 'N', ifelse
         (as.numeric(dayfile$EchoTime) > as.numeric(mastercode[daycode, paste('SMEAL_', pennum, '_Y_S', sep = "")]) &
           as.numeric(dayfile$EchoTime) < as.numeric(mastercode[daycode, paste('SMEAL_', pennum, '_Y_E', sep = "")]), 'Y', ifelse
         (as.numeric(dayfile$EchoTime) > as.numeric(mastercode[daycode, paste('SMEAL_', pennum, '_N_S2', sep = "")]) &
           as.numeric(dayfile$EchoTime) < as.numeric(mastercode[daycode, paste('SMEAL_', pennum, '_N_E2', sep = "")]), 'N', ifelse
         (as.numeric(dayfile$EchoTime) > as.numeric(mastercode[daycode, paste('SMEAL_', pennum, '_Y_S2', sep = "")]) &
           as.numeric(dayfile$EchoTime) < as.numeric(mastercode[daycode, paste('SMEAL_', pennum, '_Y_E2', sep = "")]), 'Y', ifelse
         (as.numeric(dayfile$EchoTime) > as.numeric(mastercode[daycode, paste('SMEAL_', pennum, '_N_S3', sep = "")]) &
           as.numeric(dayfile$EchoTime) < as.numeric(mastercode[daycode, paste('SMEAL_', pennum, '_N_E3', sep = "")]), 'N', 'Z'
         )))))
}


# function to code for fish location
locationcode <- function(n12code, w12code, s12code, e12code, n14code, w14code, s14code, e14code, n15code, w15code, s15code, e15code) {
  ifelse(dayfile$PEN == 12 & dayfile$PosX > locations.lookup[n12code, 'xmin'] & dayfile$PosX < locations.lookup[n12code, 'xmax'] & dayfile$PosY > locations.lookup[n12code, 'ymin'] & 
           dayfile$PosY < locations.lookup[n12code, 'ymax'] & dayfile$PosZ > locations.lookup[n12code, 'zmin'] & dayfile$PosZ < locations.lookup[n12code, 'zmax'], n12code, ifelse
         (dayfile$PEN == 12 & dayfile$PosX > locations.lookup[w12code, 'xmin'] & dayfile$PosX < locations.lookup[w12code, 'xmax'] & dayfile$PosY > locations.lookup[w12code, 'ymin'] & 
           dayfile$PosY < locations.lookup[w12code, 'ymax'] & dayfile$PosZ > locations.lookup[w12code, 'zmin'] & dayfile$PosZ < locations.lookup[w12code, 'zmax'], w12code, ifelse
         (dayfile$PEN == 12 & dayfile$PosX > locations.lookup[s12code, 'xmin'] & dayfile$PosX < locations.lookup[s12code, 'xmax'] & dayfile$PosY > locations.lookup[s12code, 'ymin'] & 
           dayfile$PosY < locations.lookup[s12code, 'ymax'] & dayfile$PosZ > locations.lookup[s12code, 'zmin'] & dayfile$PosZ < locations.lookup[s12code, 'zmax'], s12code, ifelse
         (dayfile$PEN == 12 & dayfile$PosX > locations.lookup[e12code, 'xmin'] & dayfile$PosX < locations.lookup[e12code, 'xmax'] & dayfile$PosY > locations.lookup[e12code, 'ymin'] & 
           dayfile$PosY < locations.lookup[e12code, 'ymax'] & dayfile$PosZ > locations.lookup[e12code, 'zmin'] & dayfile$PosZ < locations.lookup[e12code, 'zmax'], e12code, ifelse
         (dayfile$PEN == 14 & dayfile$PosX > locations.lookup[n14code, 'xmin'] & dayfile$PosX < locations.lookup[n14code, 'xmax'] & dayfile$PosY > locations.lookup[n14code, 'ymin'] & 
           dayfile$PosY < locations.lookup[n14code, 'ymax'] & dayfile$PosZ > locations.lookup[n14code, 'zmin'] & dayfile$PosZ < locations.lookup[n14code, 'zmax'], n14code, ifelse
         (dayfile$PEN == 14 & dayfile$PosX > locations.lookup[w14code, 'xmin'] & dayfile$PosX < locations.lookup[w14code, 'xmax'] & dayfile$PosY > locations.lookup[w14code, 'ymin'] & 
           dayfile$PosY < locations.lookup[w14code, 'ymax'] & dayfile$PosZ > locations.lookup[w14code, 'zmin'] & dayfile$PosZ < locations.lookup[w14code, 'zmax'], w14code, ifelse
         (dayfile$PEN == 14 & dayfile$PosX > locations.lookup[s14code, 'xmin'] & dayfile$PosX < locations.lookup[s14code, 'xmax'] & dayfile$PosY > locations.lookup[s14code, 'ymin'] & 
           dayfile$PosY < locations.lookup[s14code, 'ymax'] & dayfile$PosZ > locations.lookup[s14code, 'zmin'] & dayfile$PosZ < locations.lookup[s14code, 'zmax'], s14code, ifelse
         (dayfile$PEN == 14 & dayfile$PosX > locations.lookup[e14code, 'xmin'] & dayfile$PosX < locations.lookup[e14code, 'xmax'] & dayfile$PosY > locations.lookup[e14code, 'ymin'] & 
           dayfile$PosY < locations.lookup[e14code, 'ymax'] & dayfile$PosZ > locations.lookup[e14code, 'zmin'] & dayfile$PosZ < locations.lookup[e14code, 'zmax'], e14code, ifelse 
           (dayfile$PEN == 15 & dayfile$PosX > locations.lookup[n15code, 'xmin'] & dayfile$PosX < locations.lookup[n15code, 'xmax'] & dayfile$PosY > locations.lookup[n15code, 'ymin'] & 
             dayfile$PosY < locations.lookup[n15code, 'ymax'] & dayfile$PosZ > locations.lookup[n15code, 'zmin'] & dayfile$PosZ < locations.lookup[n15code, 'zmax'], n15code, ifelse
           (dayfile$PEN == 15 & dayfile$PosX > locations.lookup[w15code, 'xmin'] & dayfile$PosX < locations.lookup[w15code, 'xmax'] & dayfile$PosY > locations.lookup[w15code, 'ymin'] & 
               dayfile$PosY < locations.lookup[w15code, 'ymax'] & dayfile$PosZ > locations.lookup[w15code, 'zmin'] & dayfile$PosZ < locations.lookup[w15code, 'zmax'], w15code, ifelse
             (dayfile$PEN == 15 & dayfile$PosX > locations.lookup[s15code, 'xmin'] & dayfile$PosX < locations.lookup[s15code, 'xmax'] & dayfile$PosY > locations.lookup[s15code, 'ymin'] & 
                 dayfile$PosY < locations.lookup[s15code, 'ymax'] & dayfile$PosZ > locations.lookup[s15code, 'zmin'] & dayfile$PosZ < locations.lookup[s15code, 'zmax'], s15code, ifelse
               (dayfile$PEN == 15 & dayfile$PosX > locations.lookup[e15code, 'xmin'] & dayfile$PosX < locations.lookup[e15code, 'xmax'] & dayfile$PosY > locations.lookup[e15code, 'ymin'] & 
                   dayfile$PosY < locations.lookup[e15code, 'ymax'] & dayfile$PosZ > locations.lookup[e15code, 'zmin'] & dayfile$PosZ < locations.lookup[e15code, 'zmax'], e15code, ''
           
         ))))))))))))
}


# function to code for fish in combi hide with top and bottom tags
hidecode <- function() {
   
  # values based on trig function for calculating opp side from angle and adj side, except tan removed as subsequent calculation requires atan so they cancel out
  p12he.xdiff <- (locations.lookup['12HEB', 'xmin']-locations.lookup['12HET', 'xmin'])/(locations.lookup['12HEB', 'zmax']-locations.lookup['12HET', 'zmin'])
  p12he.ydiff <- (locations.lookup['12HEB', 'ymin']-locations.lookup['12HET', 'ymin'])/(locations.lookup['12HEB', 'zmax']-locations.lookup['12HET', 'zmin'])
  p12hw.xdiff <- (locations.lookup['12HWB', 'xmin']-locations.lookup['12HWT', 'xmin'])/(locations.lookup['12HWB', 'zmax']-locations.lookup['12HWT', 'zmin'])
  p12hw.ydiff <- (locations.lookup['12HWB', 'ymin']-locations.lookup['12HWT', 'ymin'])/(locations.lookup['12HWB', 'zmax']-locations.lookup['12HWT', 'zmin'])
  p14he.xdiff <- (locations.lookup['14HEB', 'xmin']-locations.lookup['14HET', 'xmin'])/(locations.lookup['14HEB', 'zmax']-locations.lookup['14HET', 'zmin'])
  p14he.ydiff <- (locations.lookup['14HEB', 'ymin']-locations.lookup['14HET', 'ymin'])/(locations.lookup['14HEB', 'zmax']-locations.lookup['14HET', 'zmin'])
  p14hw.xdiff <- (locations.lookup['14HWB', 'xmin']-locations.lookup['14HWT', 'xmin'])/(locations.lookup['14HWB', 'zmax']-locations.lookup['14HWT', 'zmin'])
  p14hw.ydiff <- (locations.lookup['14HWB', 'ymin']-locations.lookup['14HWT', 'ymin'])/(locations.lookup['14HWB', 'zmax']-locations.lookup['14HWT', 'zmin'])
  p15he.xdiff <- (locations.lookup['15HEB', 'xmin']-locations.lookup['15HET', 'xmin'])/(locations.lookup['15HEB', 'zmax']-locations.lookup['15HET', 'zmin'])
  p15he.ydiff <- (locations.lookup['15HEB', 'ymin']-locations.lookup['15HET', 'ymin'])/(locations.lookup['15HEB', 'zmax']-locations.lookup['15HET', 'zmin'])
  p15hw.xdiff <- (locations.lookup['15HWB', 'xmin']-locations.lookup['15HWT', 'xmin'])/(locations.lookup['15HWB', 'zmax']-locations.lookup['15HWT', 'zmin'])
  p15hw.ydiff <- (locations.lookup['15HWB', 'ymin']-locations.lookup['15HWT', 'ymin'])/(locations.lookup['15HWB', 'zmax']-locations.lookup['15HWT', 'zmin'])
  
  
  ifelse(dayfile$PEN == 12 & io12E == T 
        & dayfile$PosX > (p12he.xdiff*(locations.lookup['12HEB', 'zmax']-dayfile$PosZ))+locations.lookup['12HET', 'xmin']
        & dayfile$PosX < (p12he.xdiff*(locations.lookup['12HEB', 'zmax']-dayfile$PosZ))+locations.lookup['12HET', 'xmax']
        & dayfile$PosY > (p12he.ydiff*(locations.lookup['12HEB', 'zmax']-dayfile$PosZ))+locations.lookup['12HET', 'ymin']
        & dayfile$PosY < (p12he.ydiff*(locations.lookup['12HEB', 'zmax']-dayfile$PosZ))+locations.lookup['12HET', 'ymax']
        & dayfile$PosZ > locations.lookup['12HET', 'zmin'] & dayfile$PosZ < locations.lookup['12HEB', 'zmax'], 'P12HE', 
      ifelse(dayfile$PEN == 12 & io12W == T 
             & dayfile$PosX > (p12hw.xdiff*(locations.lookup['12HWB', 'zmax']-dayfile$PosZ))+locations.lookup['12HWT', 'xmin']
             & dayfile$PosX < (p12hw.xdiff*(locations.lookup['12HWB', 'zmax']-dayfile$PosZ))+locations.lookup['12HWT', 'xmax']
             & dayfile$PosY > (p12hw.ydiff*(locations.lookup['12HWB', 'zmax']-dayfile$PosZ))+locations.lookup['12HWT', 'ymin']
             & dayfile$PosY < (p12hw.ydiff*(locations.lookup['12HWB', 'zmax']-dayfile$PosZ))+locations.lookup['12HWT', 'ymax']
             & dayfile$PosZ > locations.lookup['12HWT', 'zmin'] & dayfile$PosZ < locations.lookup['12HWB', 'zmax'], 'P12HW', 
          ifelse(dayfile$PEN == 14 & io14E == T 
                 & dayfile$PosX > (p14he.xdiff*(locations.lookup['14HEB', 'zmax']-dayfile$PosZ))+locations.lookup['14HET', 'xmin']
                 & dayfile$PosX < (p14he.xdiff*(locations.lookup['14HEB', 'zmax']-dayfile$PosZ))+locations.lookup['14HET', 'xmax']
                 & dayfile$PosY > (p14he.ydiff*(locations.lookup['14HEB', 'zmax']-dayfile$PosZ))+locations.lookup['14HET', 'ymin']
                 & dayfile$PosY < (p14he.ydiff*(locations.lookup['14HEB', 'zmax']-dayfile$PosZ))+locations.lookup['14HET', 'ymax']
                 & dayfile$PosZ > locations.lookup['14HET', 'zmin'] & dayfile$PosZ < locations.lookup['14HEB', 'zmax'], 'P14HE',
              ifelse(dayfile$PEN == 14 & io14W == T 
                     & dayfile$PosX > (p14hw.xdiff*(locations.lookup['14HWB', 'zmax']-dayfile$PosZ))+locations.lookup['14HWT', 'xmin']
                     & dayfile$PosX < (p14hw.xdiff*(locations.lookup['14HWB', 'zmax']-dayfile$PosZ))+locations.lookup['14HWT', 'xmax']
                     & dayfile$PosY > (p14hw.ydiff*(locations.lookup['14HWB', 'zmax']-dayfile$PosZ))+locations.lookup['14HWT', 'ymin']
                     & dayfile$PosY < (p14hw.ydiff*(locations.lookup['14HWB', 'zmax']-dayfile$PosZ))+locations.lookup['14HWT', 'ymax']
                     & dayfile$PosZ > locations.lookup['14HWT', 'zmin'] & dayfile$PosZ < locations.lookup['14HWB', 'zmax'], 'P14HW',
                  ifelse(dayfile$PEN == 15 & io15E == T 
                         & dayfile$PosX > (p15he.xdiff*(locations.lookup['15HEB', 'zmax']-dayfile$PosZ))+locations.lookup['15HET', 'xmin']
                         & dayfile$PosX < (p15he.xdiff*(locations.lookup['15HEB', 'zmax']-dayfile$PosZ))+locations.lookup['15HET', 'xmax']
                         & dayfile$PosY > (p15he.ydiff*(locations.lookup['15HEB', 'zmax']-dayfile$PosZ))+locations.lookup['15HET', 'ymin']
                         & dayfile$PosY < (p15he.ydiff*(locations.lookup['15HEB', 'zmax']-dayfile$PosZ))+locations.lookup['15HET', 'ymax']
                         & dayfile$PosZ > locations.lookup['15HET', 'zmin'] & dayfile$PosZ < locations.lookup['15HEB', 'zmax'], 'P15HE',
                       ifelse(dayfile$PEN == 15 & io15W == T 
                              & dayfile$PosX > (p15hw.xdiff*(locations.lookup['15HWB', 'zmax']-dayfile$PosZ))+locations.lookup['15HWT', 'xmin']
                              & dayfile$PosX < (p15hw.xdiff*(locations.lookup['15HWB', 'zmax']-dayfile$PosZ))+locations.lookup['15HWT', 'xmax']
                              & dayfile$PosY > (p15hw.ydiff*(locations.lookup['15HWB', 'zmax']-dayfile$PosZ))+locations.lookup['15HWT', 'ymin']
                              & dayfile$PosY < (p15hw.ydiff*(locations.lookup['15HWB', 'zmax']-dayfile$PosZ))+locations.lookup['15HWT', 'ymax']
                              & dayfile$PosZ > locations.lookup['15HWT', 'zmin'] & dayfile$PosZ < locations.lookup['15HWB', 'zmax'], 'P15HW', ''
                         ))))))
  
}  


# function to code for fish at feed blocks
atfeedcode <- function(p12fscode, p14fscode, p15fscode) {
  ifelse(dayfile$PEN == 12 & dayfile$PosX > locations.lookup[p12fscode, 'xmin'] & dayfile$PosX < locations.lookup[p12fscode, 'xmax'] & dayfile$PosY > locations.lookup[p12fscode, 'ymin'] & 
           dayfile$PosY < locations.lookup[p12fscode, 'ymax'] & dayfile$PosZ > locations.lookup[p12fscode, 'zmin'] & dayfile$PosZ < locations.lookup[p12fscode, 'zmax'], p12fscode, ifelse
         (dayfile$PEN == 14 & dayfile$PosX > locations.lookup[p14fscode, 'xmin'] & dayfile$PosX < locations.lookup[p14fscode, 'xmax'] & dayfile$PosY > locations.lookup[p14fscode, 'ymin'] & 
           dayfile$PosY < locations.lookup[p14fscode, 'ymax'] & dayfile$PosZ > locations.lookup[p14fscode, 'zmin'] & dayfile$PosZ < locations.lookup[p14fscode, 'zmax'], p14fscode, ifelse
         (dayfile$PEN == 15 & dayfile$PosX > locations.lookup[p15fscode, 'xmin'] & dayfile$PosX < locations.lookup[p15fscode, 'xmax'] & dayfile$PosY > locations.lookup[p15fscode, 'ymin'] & 
           dayfile$PosY < locations.lookup[p15fscode, 'ymax'] & dayfile$PosZ > locations.lookup[p15fscode, 'zmin'] & dayfile$PosZ < locations.lookup[p15fscode, 'zmax'], p15fscode, ''
         
         )))
}

# function to code for fish in moving hide
dynamic.hidecode <- function(p12whide, p12ehide, p14whide, p14ehide, p15whide, p15ehide, radius, depth, height) {
  ifelse(dayfile$PEN == 12 & dayfile$PosX > (dayfile$P12W.x - radius) & dayfile$PosX < (dayfile$P12W.x + radius) & dayfile$PosY > (dayfile$P12W.y - radius) & 
           dayfile$PosY < (dayfile$P12W.y + radius) & dayfile$PosZ > (dayfile$P12W.z - height) & dayfile$PosZ < (dayfile$P12W.z + depth), p12whide, ifelse
         (dayfile$PEN == 12 & dayfile$PosX > (dayfile$P12E.x - radius) & dayfile$PosX < (dayfile$P12E.x + radius) & dayfile$PosY > (dayfile$P12E.y - radius) & 
           dayfile$PosY < (dayfile$P12E.y + radius) & dayfile$PosZ > (dayfile$P12E.z - height) & dayfile$PosZ < (dayfile$P12E.z + depth), p12ehide, ifelse
         (dayfile$PEN == 14 & dayfile$PosX > (dayfile$P14W.x - radius) & dayfile$PosX < (dayfile$P14W.x + radius) & dayfile$PosY > (dayfile$P14W.y - radius) & 
           dayfile$PosY < (dayfile$P14W.y + radius) & dayfile$PosZ > (dayfile$P14W.z - height) & dayfile$PosZ < (dayfile$P14W.z + depth), p14whide, ifelse
         (dayfile$PEN == 14 & dayfile$PosX > (dayfile$P14E.x - radius) & dayfile$PosX < (dayfile$P14E.x + radius) & dayfile$PosY > (dayfile$P14E.y - radius) & 
           dayfile$PosY < (dayfile$P14E.y + radius) & dayfile$PosZ > (dayfile$P14E.z - height) & dayfile$PosZ < (dayfile$P14E.z + depth), p14ehide, ifelse
           (dayfile$PEN == 15 & dayfile$PosX > (dayfile$P15W.x - radius) & dayfile$PosX < (dayfile$P15W.x + radius) & dayfile$PosY > (dayfile$P15W.y - radius) & 
               dayfile$PosY < (dayfile$P15W.y + radius) & dayfile$PosZ > (dayfile$P15W.z - height) & dayfile$PosZ < (dayfile$P15W.z + depth), p15whide, ifelse
             (dayfile$PEN == 15 & dayfile$PosX > (dayfile$P15E.x - radius) & dayfile$PosX < (dayfile$P15E.x + radius) & dayfile$PosY > (dayfile$P15E.y - radius) & 
                 dayfile$PosY < (dayfile$P15E.y + radius) & dayfile$PosZ > (dayfile$P15E.z - height) & dayfile$PosZ < (dayfile$P15E.z + depth), p15ehide, ''
               
         ))))))
}





# function to code for fish at centre of cage
centrecode <- function(highcode12, midcode12, lowcode12, highcode14, midcode14, lowcode14, highcode15, midcode15, lowcode15) {
  ifelse(dayfile$PosX > locations.lookup[highcode12, 'xmin'] & dayfile$PosX < locations.lookup[highcode12, 'xmax'] & dayfile$PosY > locations.lookup[highcode12, 'ymin'] & 
           dayfile$PosY < locations.lookup[highcode12, 'ymax'] & dayfile$PosZ > locations.lookup[highcode12, 'zmin'] & dayfile$PosZ < locations.lookup[highcode12, 'zmax'], highcode12, ifelse
         (dayfile$PosX > locations.lookup[midcode12, 'xmin'] & dayfile$PosX < locations.lookup[midcode12, 'xmax'] & dayfile$PosY > locations.lookup[midcode12, 'ymin'] & 
           dayfile$PosY < locations.lookup[midcode12, 'ymax'] & dayfile$PosZ > locations.lookup[midcode12, 'zmin'] & dayfile$PosZ < locations.lookup[midcode12, 'zmax'], midcode12, ifelse
         (dayfile$PosX > locations.lookup[lowcode12, 'xmin'] & dayfile$PosX < locations.lookup[lowcode12, 'xmax'] & dayfile$PosY > locations.lookup[lowcode12, 'ymin'] & 
           dayfile$PosY < locations.lookup[lowcode12, 'ymax'] & dayfile$PosZ > locations.lookup[lowcode12, 'zmin'] & dayfile$PosZ < locations.lookup[lowcode12, 'zmax'], lowcode12, ifelse
         (dayfile$PosX > locations.lookup[highcode14, 'xmin'] & dayfile$PosX < locations.lookup[highcode14, 'xmax'] & dayfile$PosY > locations.lookup[highcode14, 'ymin'] & 
           dayfile$PosY < locations.lookup[highcode14, 'ymax'] & dayfile$PosZ > locations.lookup[highcode14, 'zmin'] & dayfile$PosZ < locations.lookup[highcode14, 'zmax'], highcode14, ifelse
         (dayfile$PosX > locations.lookup[midcode14, 'xmin'] & dayfile$PosX < locations.lookup[midcode14, 'xmax'] & dayfile$PosY > locations.lookup[midcode14, 'ymin'] & 
           dayfile$PosY < locations.lookup[midcode14, 'ymax'] & dayfile$PosZ > locations.lookup[midcode14, 'zmin'] & dayfile$PosZ < locations.lookup[midcode14, 'zmax'], midcode14, ifelse
         (dayfile$PosX > locations.lookup[lowcode14, 'xmin'] & dayfile$PosX < locations.lookup[lowcode14, 'xmax'] & dayfile$PosY > locations.lookup[lowcode14, 'ymin'] & 
           dayfile$PosY < locations.lookup[lowcode14, 'ymax'] & dayfile$PosZ > locations.lookup[lowcode14, 'zmin'] & dayfile$PosZ < locations.lookup[lowcode14, 'zmax'], lowcode14, ifelse
           (dayfile$PosX > locations.lookup[highcode15, 'xmin'] & dayfile$PosX < locations.lookup[highcode15, 'xmax'] & dayfile$PosY > locations.lookup[highcode15, 'ymin'] & 
               dayfile$PosY < locations.lookup[highcode15, 'ymax'] & dayfile$PosZ > locations.lookup[highcode15, 'zmin'] & dayfile$PosZ < locations.lookup[highcode15, 'zmax'], highcode15, ifelse
             (dayfile$PosX > locations.lookup[midcode15, 'xmin'] & dayfile$PosX < locations.lookup[midcode15, 'xmax'] & dayfile$PosY > locations.lookup[midcode15, 'ymin'] & 
                 dayfile$PosY < locations.lookup[midcode15, 'ymax'] & dayfile$PosZ > locations.lookup[midcode15, 'zmin'] & dayfile$PosZ < locations.lookup[midcode15, 'zmax'], midcode15, ifelse
               (dayfile$PosX > locations.lookup[lowcode15, 'xmin'] & dayfile$PosX < locations.lookup[lowcode15, 'xmax'] & dayfile$PosY > locations.lookup[lowcode15, 'ymin'] & 
                   dayfile$PosY < locations.lookup[lowcode15, 'ymax'] & dayfile$PosZ > locations.lookup[lowcode15, 'zmin'] & dayfile$PosZ < locations.lookup[lowcode15, 'zmax'], lowcode15, ''
         
         )))))))))
}


# generates moving average data from hide positions, compares to original hide positions and removes positions beyond moving average threshold distance

hide.filter <- function(hide, smooth = 10, thresh = 2.5){
  
  fish.id <- hide
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
  
  fish.id$PosX.ma <- NULL
  fish.id$PosY.ma <- NULL
  fish.id$PosZ.ma <- NULL
  
  fish.id <<- fish.id
  rem.tot <<- rem.tot
  
}


# function to calculate fish headings from positions (outputs vector of fish headings)

heading.func <- function(){
  
  diffx <- diff(dayfile$PosX)
  diffy <- diff(dayfile$PosY)
  heading <- numeric()
  
  for (i in 1:length(diffx)){
    
    
    if(diffx[[i]] > 0.02 & diffy[[i]] > 0.02) {
      
      heading <- c(heading, round((atan(diffy[[i]]/diffx[[i]]))*180/pi, 2))
      
    } else {
      
      if(diffx[[i]] > 0.02 & diffy[[i]] < -0.02) {
        
        heading <- c(heading, round(90+((atan((diffy[[i]]*-1)/diffx[[i]]))*180/pi), 2)) 
        
      } else {
        
        if(diffx[[i]] < -0.02 & diffy[[i]] < -0.02) {
          
          heading <- c(heading, round(270-((atan((diffy[[i]]*-1)/(diffx[[i]]*-1)))*180/pi), 2))
          
        } else {
          
          if(diffx[[i]] < -0.02 & diffy[[i]] > 0.02){
            
            heading <- c(heading, round(270+((atan(diffy[[i]]/(diffx[[i]]*-1)))*180/pi), 2)) 
            
          } else {
            
            if(diffx[[i]] == 0 & diffy[[i]] > 0.02) {
              
              heading <- c(heading, 0)
              
            } else {
              
              if(diffx[[i]] > 0.02 & diffy[[i]] == 0) {
                
                heading <- c(heading, 90)
                
              } else {
                
                if(diffx[[i]] == 0 & diffy[[i]] < -0.02) {
                  
                  heading <- c(heading, 180)
                  
                } else {
                  
                  if(diffx[[i]] < -0.02 & diffy[[i]] == 0) {
                    
                    heading <- c(heading, 270)
                    
                  } else {
                    
                    heading <- c(heading, NA)
                    
                  }
                  
                }
                
                
              }
              
              
            }
            
          }
          
        }
        
      }
      
    }
    
  }
  
  heading <<- heading
  
}




# function to calculate fish turn angles from positions (outputs vector of fish turn angles)

turn.angles <- function(){
  
  theta <- numeric()
  
  d1 <- sqrt(diff(dayfile$PosX)^2+diff(dayfile$PosY)^2+diff(dayfile$PosZ)^2)
  d1 <- head(d1, length(d1)-1)
  d2 <- sqrt(diff(dayfile$PosX)^2+diff(dayfile$PosY)^2+diff(dayfile$PosZ)^2)
  d2 <- tail(d2, length(d2)-1)
  d3 <- sqrt(diff(dayfile$PosX, lag = 2)^2+diff(dayfile$PosY, lag = 2)^2+diff(dayfile$PosZ, lag = 2)^2)
  
  for (i in 1:(nrow(dayfile)-2)){
    
    theta <- c(theta, 180-(acos(((d1[[i]])^2+(d2[[i]])^2-(d3[[i]])^2)/(2*d1[[i]]*d2[[i]]))*180/pi))
    
  }
  
  theta <<- theta
  
}









