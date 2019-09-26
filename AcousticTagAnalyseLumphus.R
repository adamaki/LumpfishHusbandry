#Delousing efficiency project data analysis
#Adam Brooker
#29th August 2016

# LIST OF FUNCTIONS ------------------------------------------------------------------------------------------------

# 1. locations() = returns a summary matrix of pen locations for all fish
# 2a. batch.locations() = returns a summary matrix of locations for all dayfiles in working directory and saves to an Excel spreadsheet
# 2b. daily.locations() = Daily location summary for loaded dataset 
# 3a. depact() = returns depth and activity summary for all fish with standard deviations
# 3b. depact.se() = returns depth and activity summary for all fish with standard errors
# 4. depth.sum() = returns depth summary for each fish
# 5. batch.depth() = creates spreadsheet of mean depths +/- st dev for individual fish over multiple days
# 6. batch.totdepth() = batch function to return matrix of mean and standard error depths for all fish combined over multiple days
# 7. batch.activity() = creates spreadsheet of mean activity +/- st dev for each dayfile in working dir
# 8. batch.totactivity() = batch function to return matrix of mean and standard error activity for all fish combined over multiple days
# 9a. prop.coverage() = calculates fish coverage of pens 7 and 8
# 9b. hmean.prop.coverage() = calculates hourly mean fish coverage of pens 7 and 8
# 10a. batch.coverage() = calculates fish coverage of pens 7 and 8 over multiple days
# 10b. hmean.batch.coverage() = caculates hourly mean fish coverage of pens 7 and 8 over multiple days
# 10c. hmean.day.coverage() = caculates hourly mean fish coverage of pens 12, 14 and 15 over multiple days for loaded dayfile
# 10d. hmean.perfish.coverage() = calculated hourly mean fish coverage per fish over multiple days for loaded dayfile
# 11a. fish.depth(period) = draws a plot of fish depth for the fish id specified
# 11b. fish.act(period) = draws a plot of fish activity for the fish id specified
# 12. fish.3depth(period1, period2, period3) = draws a plot of depths of 3 fish
# 13. fish.plot(period) = draws a plot of fish location for the fish id specified
# 14. fish.3plot(period1, period2, period3) = draws a plot of locations of 3 fish
# 15. add.fish(period, fishcol) = add a fish to the current plot (period = fish id, fishcol = number from 1-20)
# 16a. fish.hexplot(period) = draws a plot of fish location density for the fish id specified 
# 16b. hexplot.all(pen) = draws a plot of fish location density for all fish in the specified pen (7 or 8)
# 17. fish.3dplot(period) = draws a 3d plot of fish location and depth
# 18. fish.3dmove(period) = draws a 3d interactive plot of fish location and depth
# 19a. plot.bydepth(period) = draws a plot of fish locations coloured by depth (blue = >15m, red = <15m)
# 19b. plot.byactivity(period) = draws a plot of fish locations coloured by activity
# 19c. plot.bylight(period) = draws a plot of fish locations coloured by time of day (dawn, day, dusk, night)
# 20. add.depthfish(period) = add a fish to the current plot coloured by depth
# 21. fractal() = calculate fractal dimensions for pens 7 & 8 using the box counting method. Returns plot of box counts with fractal dimension and R2
# 22. batch.fractals() = calculate fractal dimensions for each fish over several day files in a folder. Returns an Excel spreadsheet of fractal dimension and R2 for all fish each day
# 23. id.fractals() = calculate fractal dimensions for each fish on one day file. Returns table of fractal dimesions and R2 values and saves to Excel spreadsheet
# 24. plot.bytime(period) = draws a plot of fish locations colour coded according number of time divisions (bins)
# 25. batch.remove(period, start.day, no.days) = Removes single fish id from specified day files
# 26. prop.coverage.3d() = proportion coverage 3D (not sure this is working properly!)
# 27. ma.filter(period, smooth, thresh) = moving average filter function. Period = fish id, smooth = size of smoothing filter, thresh = data removal threshold in metres
# 28. add(period)  = add a single fish to a dayfile after cleaning data using ma.filter function
# 29. recode() = function to recode fish speeds and save to dayfile after cleaning data
# 30. batch.subset(variable, factors) = batch function to subset and save data according to specified variable and factors, variable = column to subset by, factors = list of variables in column
# 31a. heatplot.anim(pen, frames) = Create series of plots for animation (pen = pen number 7 or 8, frames = No. of frames, set to No. of hours in dataset)
# 31b. fishplot.anim <- function(pen, frames, framedur, animdur) = Create series of individual fish plots for animation. pen = pen to plot, frames = No. of frames to create, framedur = portion of time to plot for each frame in secs, animdur = length of fish trails in No. of frames (0 = cumulative frames)
# 32. fish.hist(pt) = draw histogram of fish depth or activity from fish files (pt = 'activity' or 'depth')
# 33. load.all() = Load all data files (.csv) in folder into single data frame
# 34. crop(xmin, xmax, ymin, ymax) = Crop edges of dataset to remove multipath
# 35. save() = Save loaded dayfile to .csv file of original name
# 36. distance() = calculate distance travelled for all individual fish in day file
# 37. batch.dist() = calculate distance travelled for all fish files in a folder
# 38. Load.dayfile() = load specified dayfile
# 39. multiplot() = off-the-shelf function to draw multiple ggplots
# 40. headplot() = draws two polar plots of headings for pens 7 and 8
# 41. turnplot() = draws two polar plots of turn angles for pens 7 and 8
# 42. bplot(period, step) = draw turn and velocity plots for period and step specified for whole dayfile
# 43. bcalc() = Perform behaviour calculations for loaded dayfile and add to dayfile
# 44. batch.bscalc() = calculate behaviour states for all dayfiles in working directory and save results to dayfiles
# 45. batch.bsprop() = calculate proportions of behaviour states for each dayfile in working directory
# 50a. bsf(static, cruise, save) = calculate behaviour state frequencies (static, cruise, burst) for pens 7 and 8. static = upper limit of static state, cruise = upper limit of cruise state, save = save plot and data file(T/F)
# 50b. bsf2(save) = calculate behaviour state frequencies (Rr, Rf, Ra, Ep, Ef, Ea) for pens 7 and 8. save = save plot and data file(T/F)
# 51. add.daynum(startday = 1) = Add day number column to dayfile. Specify start day if required.
# 52. save.dayfiles() = Save dataset as dayfiles. Working directory must be location of dataset to save. Creates new folder called 'saved'


# NOTES -------------------------------------------------------------------------------------------------------------

# coverage grid size:
# mean swimming speed = 0.03m/s, max ping rate = 10 sec. Mean distan ce covered between pings = 0.03*10 = 0.3m
# Therefore: grid size = 0.3m


# Need to run the code below to get some functions to work (maybe!)
#library(devtools)
#install_github("plotflow", "trinker")


# ------
library(hexbin)
library(scatterplot3d)
library(rgl)
#Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre7')
library(rJava)
library(XLConnectJars)
library(XLConnect) 
library(RColorBrewer)
library(colorspace)
library(colorRamps)
library(stats)
library(ggplot2)
library(animation)
detach("package:dplyr")
library(openxlsx)
library(xlsx)
library(chron)
library(lubridate)
library(magick)
#library(plyr)
#library(plotflow)
library(gridExtra)
library(cowplot)
library(zoo)
library(adehabitat)
library(adehabitatHR)
library(maptools)
library(sp)
library(Rwave)
#library(sowas)
library(WaveletComp)
library(dplyr)
library(tidyr)
library(data.table)
library(emdbook)
library(scales)



#ENTER YOUR VARIABLES HERE
#workingdir = "G:/Data/2018 Lumpfish Husbandry/Data processing/6a. Coded Day CSV" # change to location of data
workingdir <- ifelse(Sys.info()['user'] == 'Laptop', "G:/Data/2018 Lumpfish Husbandry/Data processing/6a. Coded Day CSV/substudies/1. General behaviour/cropped", '/Volumes/My Book/Lumpfish Husbandry') # change to location of data
setwd(workingdir)  

dayfile.loc = "R1_LBF18S100197_1_day_coded.csv" # change to file to be analysed
masterfileloc = "G:/Data/2018 Lumpfish Husbandry/AcousticTagFile_2018v6.xlsx" # change to location of AcousticTagFile.xlsx
masterfileloc <- ifelse(Sys.info()['user'] == 'Laptop', "G:/Data/2018 Lumpfish Husbandry/AcousticTagFile_2018v6.xlsx", '/Volumes/My Book/Lumpfish Husbandry/Results/AcousticTagFile_2018v6.xlsx') # change to location of data


workingdir = "H:/Data processing/2016 Conditioning study B/Filtered Data/Recoded Fish CSV/Unconditioned" # change to location of data
dayfile.loc = "run_1LLF16S1007045_fish_coded.csv" # change to file to be analysed
masterfileloc = "H:/Data processing/AcousticTagFile_2016.xlsx" # change to location of AcousticTagFile.xlsx


#new dayfile classes
dayfile.classes = c('NULL', 'numeric', 'numeric', # Period
                    'factor', # Pen number
                    'POSIXct', 'double', 'double', 'double', # EchoTime and coordinates
                    'double', 'double', 'double', 'double', 'double', 'double', 'double', 'double', # calculated swimming parameters
                    'factor', 'factor', 'factor', 'factor', 'factor', # biofouling, visibility, moon
                    'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                    'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', # sea lice numbers
                    'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', # Locations
                    'factor', 'factor', 'factor', # Sun, tide stage and phase
                    'factor', 'factor', 'factor', # salmon feeding times
                    'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                    'factor', 'factor', 'factor', 'factor', 'factor' # Environmental probes
)

#half-coded dayfile classes
dayfile.classes = c('NULL', 'numeric', 'factor', 'factor', 'POSIXct', 'double', 'double', 
                    'double', 'double', 'double', 'double', 'double', 'double', 'double', 'double', 'double', 'factor',
                    'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                    'factor', 'factor', 'factor', 'factor', 'factor', 'factor'
)

#old dayfile classes
dayfile.classes = c('NULL', 'numeric', 'factor', 'factor', 'POSIXct', 'double', 'double', 
                    'double', 'double', 'double', 'double', 'double', 'double', 'factor',
                    'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                    'double', 'double', 'double', 'double', 'double', 'double', 'double',
                    'double', 'double', 'double', 'double', 'double', 'double', 'double',
                    'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                    'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                    'factor', 'factor', 'double', 'double', 'double', 'double', 'double', 
                    'double', 'double', 'double', 'double', 'double', 'double', 'double'
)

# uncoded CSV dayfile classes
dayfile.classes = c('NULL', 'NULL', 'NULL', 'NULL', 'character', 'character', 'NULL', 
                                 'character', 'character', 'character', 'character', 'NULL', 'NULL', 
                                 'NULL', 'NULL', 'NULL', 'NULL', 'NULL', 'NULL', 
                                 'NULL')


workingdir = "G:/Data/2018 Lumpfish Husbandry/Data processing/6a. Coded Day CSV/hides" # change to location of data
hidefile.loc = "run_1LLF16S100258_day_hides.csv" # change to file to be analysed
hidefile.classes = c('NULL', 'factor', 'NULL', 'factor', 'POSIXct', 'double', 'double', 'double')


# LOAD FILES-------------------------------------------------------------------------------------------------------------------

#LOAD LOCATIONS CODING DATA
locations.lookup <- read.xlsx(masterfileloc, sheetIndex = 11, startRow = 1, endRow = 61, colIndex = seq(1, 7)) # read in codes from Locations Coding spreadsheet

#locations.lookup <- readWorksheetFromFile(masterfileloc, sheet = 12, startRow = 1, endCol = 7) # read in codes from Locations Coding spreadsheet
rownames(locations.lookup) <- locations.lookup$Code

# Load fish IDs
fishids <- xlsx::read.xlsx(masterfileloc, sheetIndex = 3, startRow = 19, endRow = 103, colIndex = 8)
fishids <- as.vector(fishids$Period)

# LOAD DAYFILE
setwd(workingdir)                                                                                                    
dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = dayfile.classes)
dayfile[,c(seq(20, 40, 1), seq(54,65, 1))] <- apply(dayfile[,c(seq(20, 40, 1), seq(54,65, 1))], 2, function(x) as.numeric(as.character(x))) # convert factors to numbers
                                                                          
#)) #read data into table

#SORT BY TIME AND TAG
dayfile <- dayfile[order(dayfile$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
dayfile <- dayfile[order(dayfile$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag


#LOAD HIDEFILE
setwd(workingdir)                                                                                                    
hidefile <- read.csv(hidefile.loc, header = TRUE, sep = ",", colClasses = hidefile.classes) 
load.all.hides()

# Temporary code to convert uncoded data into correct formats-------------------------------------
 
rot.ang <- 14.24 # grid rotation angle in radians
UTMeast <- -1230064.57 # grid origin x-axis
UTMnorth <- 6170474.26 #  grid origin y-axis

dayfile$PosX2 <- round((cos(rot.ang*pi/180)*dayfile$PosX-sin(rot.ang*pi/180)*dayfile$PosY)-UTMeast, digits = 2)
dayfile$PosY2 <- round((sin(rot.ang*pi/180)*dayfile$PosX+cos(rot.ang*pi/180)*dayfile$PosY)-UTMnorth, digits = 2)
dayfile$PosX <- dayfile$PosX2
dayfile$PosY <- dayfile$PosY2
dayfile$PosX2 <- NULL
dayfile$PosY2 <- NULL

#CONVERT FIELDS INTO CORRECT FORMATS
water.height <- 35
dayfile$Period <- sapply(dayfile$Period, as.numeric)
dayfile$SubCode <- sapply(dayfile$SubCode, as.numeric)
dayfile[, 'EchoTime'] <- as.POSIXct(strptime(dayfile[,'EchoTime'], "%d/%m/%Y %H:%M:%S", tz = "UTC")) # convert character format to date and time format
dayfile$PosX <- as.numeric(dayfile$PosX)
dayfile$PosY <- as.numeric(dayfile$PosY)
dayfile$PosZ <- as.numeric(dayfile$PosZ)
dayfile$PosZ <- water.height-dayfile$PosZ



# SANDPIT-----------------------------------------------------------------------------------------------------------------

days <- sort(paste0(unique(as.Date(dayfile$EchoTime)), ' 00:00:00')) # create vector of all days in dayfile at midnight for subsetting


# animated 3d plot
plot3d(fish.id$PosX, fish.id$PosY, fish.id$PosZ, pch = 20, xlim =  c(0, 35), ylim = c(5, 40), zlim = c(0, 26), xlab = 'X', ylab = 'Y', zlab = 'Z', type = 'l')
dir.create("animation")
for (i in 1:1000){
  view3d(userMatrix=rotationMatrix(pi/2 * i/1000, 0, 1, -1))
  rgl.snapshot(filename=paste("animation/frame-", sprintf("%03d", i), ".png", sep=""))
}


# hexplot for all fish
bin <- hexbin(dayfile$PosX, dayfile$PosY, xbins = 50)
plot(hexbin(dayfile$PosX, dayfile$PosY, xbins = 50), xlab = 'X', ylab = 'Y')


# pen 7 x,y plots
par(mfrow=c(3,3))
fish.3plot('7829', '8081', '7213')
fish.3plot('7269', '7045', '9229')
fish.3plot('9873', '7381', '9901')
fish.3plot('9453', '7129', '9397')
fish.3plot('8025', '8417', '9649')
fish.plot(9425)
fish.plot(8053)


# pen 8 x,y plots
par(mfrow=c(3,3))
fish.3plot('7857', '7773', '7437')
fish.3plot('9145', '8165', '9173')
fish.3plot('9677', '7745', '8529')
fish.3plot('9033', '7101', '8277')
fish.3plot('8109', '9537', '7661')
fish.plot('7241')
fish.plot('7409')


# pen 7 x,y plot
par(mfrow=c(1,1))
fishpal <- rainbow_hcl(20, c=100, l=63, start=-360, end=-32, alpha = 0.2)
fish.plot('9425')
add.fish('7829', fishcol = fishpal[19])
add.fish('8081', fishcol = fishpal[18])
add.fish('7213', fishcol = fishpal[17])
add.fish('7269', fishcol = fishpal[16])
add.fish('7045', fishcol = fishpal[15])
add.fish('9229', fishcol = fishpal[14])
add.fish('9873', fishcol = fishpal[13])
add.fish('7381', fishcol = fishpal[12])
add.fish('9901', fishcol = fishpal[11])
add.fish('9453', fishcol = fishpal[10])
add.fish('7129', fishcol = fishpal[9])
add.fish('9397', fishcol = fishpal[8])
add.fish('8025', fishcol = fishpal[7])
add.fish('8417', fishcol = fishpal[6])
add.fish('9649', fishcol = fishpal[5])
add.fish('8053', fishcol = fishpal[4])



# pen 8 x,y plot
par(mfrow=c(1,1))
fishpal <- rainbow_hcl(20, c=100, l=63, start=-360, end=-32, alpha = 0.2)
fish.plot('7857')
add.fish('7773', fishcol = fishpal[19])
add.fish('7437', fishcol = fishpal[18])
add.fish('9145', fishcol = fishpal[17])
add.fish('8165', fishcol = fishpal[16])
add.fish('9173', fishcol = fishpal[15])
add.fish('9677', fishcol = fishpal[14])
add.fish('7745', fishcol = fishpal[13])
add.fish('8529', fishcol = fishpal[12])
add.fish('9033', fishcol = fishpal[11])
add.fish('7101', fishcol = fishpal[10])
add.fish('8277', fishcol = fishpal[9])
add.fish('8109', fishcol = fishpal[8])
add.fish('9537', fishcol = fishpal[7])
add.fish('7661', fishcol = fishpal[6])
add.fish('7241', fishcol = fishpal[5])
add.fish('7409', fishcol = fishpal[4])



# pen 7 depth plots
par(mfrow=c(3,3))
fish.3depth('7829', '8081', '7213')
fish.3depth('7269', '7045', '9229')
fish.3depth('9873', '7381', '9901')
fish.3depth('9453', '7129', '9397')
fish.3depth('8025', '8417', '9649')
fish.depth(9425)
fish.depth(8053)


# pen 8 depth plots
par(mfrow=c(3,3))
fish.3depth('7857', '7773', '7437')
fish.3depth('9145', '8165', '9173')
fish.3depth('9677', '7745', '8529')
fish.3depth('9033', '7101', '8277')
fish.3depth('8109', '9537', '7661')
fish.depth('7241')
fish.depth('7409')

# pen 7 x,y plot by depth
par(mfrow=c(1,1))
depthpal <- diverge_hcl(30, h = c(11,266), c = 100, l = c(21,85), power = 0.6, alpha = 0.2)
plot.bydepth('9425')
add.depthfish('7829')
add.depthfish('8081')
add.depthfish('7213')
add.depthfish('7269')
add.depthfish('7045')
add.depthfish('9229')
add.depthfish('9873')
add.depthfish('7381')
add.depthfish('9901')
add.depthfish('9453')
add.depthfish('7129')
add.depthfish('9397')
add.depthfish('8025')
add.depthfish('8417')
add.depthfish('9649')
add.depthfish('8053')
rect(locations.lookup['7EW', 'xmin'], locations.lookup['7EW', 'ymin'], locations.lookup['7EW', 'xmax'], locations.lookup['7EW', 'ymax'], lty = 2) # 7EW edge
rect(locations.lookup['7ES', 'xmin'], locations.lookup['7ES', 'ymin'], locations.lookup['7ES', 'xmax'], locations.lookup['7ES', 'ymax'], lty = 2) # 7ES edge
rect(locations.lookup['7EE', 'xmin'], locations.lookup['7EE', 'ymin'], locations.lookup['7EE', 'xmax'], locations.lookup['7EE', 'ymax'], lty = 2) # 7EE edge
rect(locations.lookup['7EN', 'xmin'], locations.lookup['7EN', 'ymin'], locations.lookup['7EN', 'xmax'], locations.lookup['7EN', 'ymax'], lty = 2) # 7EN edge
rect(locations.lookup['7WHSE', 'xmin'], locations.lookup['7WHSE', 'ymin'], locations.lookup['7WHSE', 'xmax'], locations.lookup['7WHSE', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHSE
rect(locations.lookup['7WHNW', 'xmin'], locations.lookup['7WHNW', 'ymin'], locations.lookup['7WHNW', 'xmax'], locations.lookup['7WHNW', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHNW
rect(locations.lookup['7EW', 'xmin'], locations.lookup['7ES', 'ymin'], locations.lookup['7EE', 'xmax'], locations.lookup['7EN', 'ymax'], lwd = 2) # cage limits

#legend(32, 42, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30), fill = depthpal, pch = 20, cex = 0.7)


# pen 8 x,y plot by depth
par(mfrow=c(1,1))
depthpal <- diverge_hcl(30, h = c(11,266), c = 100, l = c(21,85), power = 0.6, alpha = 0.2)
plot.bydepth('7857')
add.depthfish('7773')
add.depthfish('7437')
add.depthfish('9145')
add.depthfish('8165')
add.depthfish('9173')
add.depthfish('9677')
add.depthfish('7745')
add.depthfish('8529')
add.depthfish('9033')
add.depthfish('7101')
add.depthfish('8277')
add.depthfish('8109')
add.depthfish('9537')
add.depthfish('7661')
add.depthfish('7241')
add.depthfish('7409')
rect(locations.lookup['8EW', 'xmin'], locations.lookup['8EW', 'ymin'], locations.lookup['8EW', 'xmax'], locations.lookup['8EW', 'ymax'], lty = 2) # 7EW edge
rect(locations.lookup['8ES', 'xmin'], locations.lookup['8ES', 'ymin'], locations.lookup['8ES', 'xmax'], locations.lookup['8ES', 'ymax'], lty = 2) # 7ES edge
rect(locations.lookup['8EE', 'xmin'], locations.lookup['8EE', 'ymin'], locations.lookup['8EE', 'xmax'], locations.lookup['8EE', 'ymax'], lty = 2) # 7EE edge
rect(locations.lookup['8EN', 'xmin'], locations.lookup['8EN', 'ymin'], locations.lookup['8EN', 'xmax'], locations.lookup['8EN', 'ymax'], lty = 2) # 7EN edge
rect(locations.lookup['8WHSW', 'xmin'], locations.lookup['8WHSW', 'ymin'], locations.lookup['8WHSW', 'xmax'], locations.lookup['8WHSW', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHSE
rect(locations.lookup['8WHNE', 'xmin'], locations.lookup['8WHNE', 'ymin'], locations.lookup['8WHNE', 'xmax'], locations.lookup['8WHNE', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHNW
rect(locations.lookup['8EW', 'xmin'], locations.lookup['8ES', 'ymin'], locations.lookup['8EE', 'xmax'], locations.lookup['8EN', 'ymax'], lwd = 2) # cage limits


# plot hides
temp <- dayfile
dayfile <- subset(temp, temp$Period == '11805' | temp$Period == '11553' | temp$Period == '11217' | temp$Period == '10965' | temp$Period == '10657' | temp$Period == '10377' | temp$Period == '9761' | temp$Period == '9313')
fishpal <- rainbow_hcl(20, c=100, l=63, start=-360, end=-32, alpha = 0.2)
dayfile$PEN <- '7'
fish.plot(11805)
add.fish('11553', fishcol = fishpal[1])
add.fish('11217', fishcol = fishpal[15])
add.fish('10965', fishcol = fishpal[5])
dayfile$PEN <- '8'
fish.plot(10657)
add.fish('10377', fishcol = fishpal[1])
add.fish('9761', fishcol = fishpal[13])
add.fish('9313', fishcol = fishpal[4])


#1 plot
par(mfrow=c(1,1))

#subset all fish from 1 pen
fish.id <- subset(dayfile, PEN == '7')

#mean fish swim speed
mean(fish.id$MSEC)

#create list of all files in working directory
files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)

# code for manaully removing dead fish ------------------------------------------------------------------------------------

tot.days <- unique(format(as.Date(dayfile$EchoTime, format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d')) # returns list of days in file
tot.days

dayfile <- dayfile[!(dayfile$Period == 7017),] # remove dead fish

write.csv(dayfile, file = dayfile.loc) #write output to file


# probability density functions

df = ggplot(subset(dayfile, PEN == '7' | PEN == '8'), aes(x=BLSEC, colour = PEN)) + theme(panel.background = element_rect(fill = 'white', colour = 'black'))
df = df + scale_x_log10(breaks = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), labels = c(0.01, '', '', '', '', '', '', '', '', 0.1, '', '', '', '', '', '', '', '', 1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100))
df = df + scale_y_log10(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000)) 
df + geom_freqpoly(binwidth = 0.5, size = 1)



# calculate behaviour state frequencies

bsffile <- dayfile[,c('Period', 'PEN', 'SEC', 'BLSEC')]
bsffile$BSF <- ifelse(bsffile$BLSEC <= 0.1, 'static', ifelse(bsffile$BLSEC > 0.1 & bsffile$BLSEC <= 1, 'cruise', 'burst'))
#dayfile$BSF <- ifelse(dayfile$BLSEC <= 0.1, 1, ifelse(dayfile$BLSEC > 0.1 & dayfile$BLSEC <= 1, 2, 3))
bsffile$BSFcount <- sequence(rle(bsffile$BSF)$lengths)
bsffile$CountTF <- c(ifelse(diff(bsffile$BSFcount, 1, 1) < 1, T, F), F)

bsfsum <- 0

for (i in 1:nrow(bsffile)){
  
  bsfsum <- bsfsum + bsffile[i, 'SEC']
  
  if(bsffile[i, 'CountTF'] == T & is.na(bsffile[i, 'SEC']) == F){
    
  bsffile[i,'BSFdur'] <- bsfsum
  bsfsum <- 0
    
  } else {
  
  bsffile[i,'BSFdur'] <- NA    
    
  }
  
}


# bsffile$round <- ifelse(bsffile$BSFdur < 100, round(bsffile$BSFdur, -1), ifelse(bsffile$BSFdur > 99, bsffile$round <- round(bsffile$BSFdur, -2), bsffile$round <- round(bsffile$BSFdur, -3)))

bsffile$round <- as.numeric(as.character(cut(bsffile$BSFdur, breaks = c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000), labels = c('1', '2', '5', '10', '20', '50', '100', '200', '500', '1000'))))


# generates table of BSF frequencies and draws plot

dayfile$BSF <- as.factor(dayfile$BSF)

bsftab <- as.data.frame(table(bsffile$round, bsffile$BSF, bsffile$PEN)) # tabulate frequencies of each duration and BSF
names(bsftab) <- c('dur', 'BSF', 'pen', 'freq')
bsftab$dur <- as.numeric(as.character(bsftab$dur))
bsftab$freq <- as.numeric(bsftab$freq)

# bsftab$bin <- .bincode(bsftab$freq, seq(1, 10000, 10))


# df = ggplot(bsftab, aes(x=dur, y=freq, colour = BSF)) + theme(panel.background = element_rect(fill = 'white', colour = 'black'))

sp = ggplot(subset(bsftab, BSF == 'static'), aes(x=dur, y=freq, colour = pen)) + theme(panel.background = element_rect(fill = 'white', colour = 'black'))
sp = sp + scale_x_log10(limits = c(10, 1000), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000))
sp = sp + scale_y_log10(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000)) 
sp = sp + geom_path(size = 1) + labs(title = 'Static', x = 'duration', y = 'frequency')

cp = ggplot(subset(bsftab, BSF == 'cruise'), aes(x=dur, y=freq, colour = pen)) + theme(panel.background = element_rect(fill = 'white', colour = 'black'))
cp = cp + scale_x_log10(limits = c(10, 1000), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000))
cp = cp + scale_y_log10(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000)) 
cp = cp + geom_path(size = 1) + labs(title = 'Cruise', x = 'duration', y = 'frequency')

bp = ggplot(subset(bsftab, BSF == 'burst'), aes(x=dur, y=freq, colour = pen)) + theme(panel.background = element_rect(fill = 'white', colour = 'black'))
bp = bp + scale_x_log10(limits = c(10, 1000), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000))
bp = bp + scale_y_log10(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000)) 
bp = bp + geom_path(size = 1) + labs(title = 'Burst', x = 'duration', y = 'frequency')


plot_grid(sp, cp, bp, align = 'hv', nrow = 2, ncol = 2)

#-------------------------------------------------------------------------------------------------------------------------------


ani.options(interval = 0.01)

saveGIF({  
  
  
  for (i in 1:100){
    plot(fish.id[1:i,'PosX'], fish.id[1:i,'PosY'], xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 0.8, xlim = c(5, 36), ylim = c(8, 41), type = 'p', col = fishpal[20]) # tight plot
    
    
  }
  
})



fish.id <- subset(dayfile, dayfile$PosY > 10 & dayfile$Period == 7409)
dayfile <- subset(dayfile, !(dayfile$Period == 7409))
dayfile <- rbind(dayfile, fish.id)


fish.id <- subset(dayfile, dayfile$Period == 8949)
fish.id <- subset(fish.id, duplicated(fish.id$EchoTime) == FALSE)

# code to day average env probe data
probe <- probe.DOT2
probe$day <- as.Date(probe$DO.time.2m)
mean.temp2m <- tapply(probe$Temp.2m, probe$day, mean)


# code to create animated gif from sequence of plot images

system.time({
  setwd(paste0(workingdir, '/animate'))
  files <- list.files(path = paste0(workingdir, '/animate'), pattern = '*.png', all.files = FALSE, recursive = FALSE)
  
  anim.frames <- image_read(files)
  
  animation <- image_animate(anim.frames, fps = 2, loop = 0, dispose = 'previous')
  
  image_write(animation, 'anim.gif')
}
)



# log scale and labels for activity histograms

# conditioned wrasse
hdep + scale_x_log10(breaks = c(0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008, 0.009, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.8, 0.09, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels = c('0.001', '', '', '', '', '', '', '', '', '0.01', '', '', '', '', '', '', '', '', '0.1', '', '', '', '', '', '', '', '', '1', '', '', '', '', '', '', '', '', '10')) + scale_y_continuous(limits = c(0, 100000)) + ggtitle('Conditioned wrasse activity histogram')

# unconditioned wrasse
hdep + scale_x_log10(breaks = c(0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008, 0.009, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.8, 0.09, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels = c('0.001', '', '', '', '', '', '', '', '', '0.01', '', '', '', '', '', '', '', '', '0.1', '', '', '', '', '', '', '', '', '1', '', '', '', '', '', '', '', '', '10')) + scale_y_continuous(limits = c(0, 100000)) + ggtitle('Unconditioned wrasse activity histogram')



# Analysis of behaviour states

daytemp <- dayfile
dayfile <- subset(daytemp, BS == 'Rr')

bstab <- table(dayfile$BS)
round(bstab/sum(bstab)*100, 2)


# code to create sequence of date/times for env. probe data

start <- as.POSIXct('05/28/18 16:30:00', format = '%m/%d/%y %H:%M:%S', tz = 'GMT')
interval <- 60
end <- start + as.difftime(67, units = 'days')
timeseq <- as.character(seq(from = start, by = interval*30, to = end))
write.csv(timeseq, 'timesequence.csv')



# kernel utilisation distributions using adehabitat

kudcols <- c(brewer.pal(4, 'Accent')[[1]], brewer.pal(4, 'Accent')[[4]]) # create colour palette for KUDs

x <- seq(0, 60, by = 0.5)
y <- seq(0, 60, by = 0.5)
xy <- expand.grid(x=x, y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE
class(xy)

coords <- dayfile[,c(1, 5, 6)] # extract x,y coords and fish id from dayfile
coordinates(coords) <- c('PosX', 'PosY') # convert to spatial points data frame object
ud <- kernelUD(coords, h = 'href', grid = xy, kern = 'bivnorm') # KUD calculation for adehabitatHR package

mcp100 <- mcp(coords, percent = 100)
ver50 <- getverticeshr(ud, 50) # extract 50% vertex for plotting
ver95 <- getverticeshr(ud, 95) # extract 95% vertex for plotting
plot(mcp100, col = NULL, axes = T, xlim = c(10, 45), ylim = c(0, 45)) # plot MCP100
plot(ver95, col = kudcols[[1]], axes = F, xlim = c(10, 45), ylim = c(0, 45), add=T) # plot KUD95
plot(ver50, col = kudcols[[2]], axes = F, xlim = c(10, 45), ylim = c(0, 45), add=T) # plot KUD50

ka <- kernel.area(ud, percent = c(50, 95), unin = 'm', unout = 'm2') # calculates area of KUD50, KUD95 and MCP100

ud <- kernelUD(dayfile[,5:6], h = 'href', grid = 50, same4all = T, kern = 'bivnorm') # KUD calculation for adehabitat package

mcpest <- mcp(dayfile[,5:6], id = dayfile$Period, percent = 100)
getvolumeUD(ud)

image(ud, axes = T, addpoints = F, xlim = c(0, 45))
image.khr(ud, addpoints = F, axes = T, xlim = c(10, 45), ylim = c(10, 50))

plot(mcpest, xlim = c(10, 45), ylim = c(0, 45))
par(new=T)
plot(ver, xlim = c(10, 45), ylim = c(0, 45), axes = F, fg = 'blue')
par(new=F)


# Code to calculate KUD50 and KUD95 for all fish in loaded dayfile and save as csv-----------------------------------------------------------
# N.B. this code will only work properly if all fish are from the same pen, so loading all fish from one group works best

fish <- unique(dayfile$Period)

if(unique(dayfile$PEN == 7)){
  
  x <- seq(0, 50, by = 0.5)
  y <- seq(0, 50, by = 0.5)
  xy <- expand.grid(x=x, y=y)
  coordinates(xy) <- ~x+y
  gridded(xy) <- TRUE
  class(xy)
  
} else {
  
  x <- seq(25, 70, by = 0.5)
  y <- seq(0, 50, by = 0.5)
  xy <- expand.grid(x=x, y=y)
  coordinates(xy) <- ~x+y
  gridded(xy) <- TRUE
  class(xy)  

}  

kud50 <- numeric()
kud95 <- numeric()
kudtab <- data.frame()

for (i in 1:length(fish)){
  
  daytemp <- subset(dayfile, Period == fish[[i]])
  
  coords <- daytemp[,c(1, 5, 6)] # extract x,y coords and fish id from dayfile
  coordinates(coords) <- c('PosX', 'PosY') # convert to spatial points data frame object
  ud <- kernelUD(coords, h = 'href', grid = xy, kern = 'bivnorm') # KUD calculation for adehabitatHR package
  
  ka <- kernel.area(ud, percent = c(50, 95), unin = 'm', unout = 'm2') # calculates area of KUD50, KUD95
  
  kud50 <- c(kud50, ka[1,1])
  kud95 <- c(kud95, ka[2,1])
  
}

kudtab <- cbind(fish, kud50, kud95)


# Code to calculate 3D KUD50 and KUD95 for all fish in loaded dayfile and save as csv-----------------------------------------------------------

fish <- unique(dayfile$Period)

if(unique(dayfile$PEN == 7)){
  
  x <- seq(0, 50, by = 0.5)
  y <- seq(0, 50, by = 0.5)
  xy <- expand.grid(x=x, y=y)
  coordinates(xy) <- ~x+y
  gridded(xy) <- TRUE
  class(xy)
  
} else {
  
  x <- seq(25, 70, by = 0.5)
  y <- seq(0, 50, by = 0.5)
  xy <- expand.grid(x=x, y=y)
  coordinates(xy) <- ~x+y
  gridded(xy) <- TRUE
  class(xy)  
  
}  

kud50 <- numeric()
kud95 <- numeric()
kudtab <- data.frame()
max.depth <- ceiling(max(dayfile$PosZ))

for (i in 1:length(fish)){
  
  daytemp <- subset(dayfile, Period == fish[[i]])
  #depth <- 0
  k50 <- 0
  k95 <- 0
  
  for (j in 1:max.depth){
  
  depthtemp <- subset(daytemp, PosZ > j-1 & PosZ < j)
    
  if (nrow(depthtemp) >4){
    
  coords <- depthtemp[,c(1, 5, 6)] # extract x,y coords and fish id from dayfile
  coordinates(coords) <- c('PosX', 'PosY') # convert to spatial points data frame object
  ud <- kernelUD(coords, h = 'href', grid = xy, kern = 'bivnorm') # KUD calculation for adehabitatHR package
  
  ka <- kernel.area(ud, percent = c(50, 95), unin = 'm', unout = 'm2') # calculates area of KUD50, KUD95
  k50 <- k50 + ka[1,1]
  k95 <- k95 + ka[2,1]
  #depth <- depth+1
  } else {
    k50 <- k50 + 0
    k95 <- k95 + 0
    #depth <- depth+1
  }

  }
  
  kud50 <- c(kud50, k50)
  kud95 <- c(kud95, k95)
  
}

kudtab <- cbind(fish, kud50, kud95)


# code to add day number to dayfile (need to do this for KUD code to work)

exp.dates <- unique(as.Date(dayfile$EchoTime))
exp.start <- 258
exp.length <- 29
exp.days <- seq(exp.start, exp.start+exp.length-1, 1)
names(exp.days) <- exp.dates
dayfile$day <- as.numeric(exp.days[as.character(as.Date(dayfile$EchoTime))])

# code to calculate cumulative KUD50s and KUD95s for each fish in loaded dayfile and save as csv -------------------------------------------


fish <- unique(dayfile$Period)

if(unique(dayfile$PEN == 7)){
  
  x <- seq(0, 50, by = 0.5)
  y <- seq(0, 50, by = 0.5)
  xy <- expand.grid(x=x, y=y)
  coordinates(xy) <- ~x+y
  gridded(xy) <- TRUE
  class(xy)
  
} else {
  
  x <- seq(25, 70, by = 0.5)
  y <- seq(0, 50, by = 0.5)
  xy <- expand.grid(x=x, y=y)
  coordinates(xy) <- ~x+y
  gridded(xy) <- TRUE
  class(xy)  
  
}  

kud50.cum <- data.frame()
kud95.cum <- data.frame()

for (i in 1:length(fish))
  
  {
  
  kud50 <- numeric()
  kud95 <- numeric()
  fishsub <- subset(dayfile, Period == fish[[i]])
  days <- unique(fishsub$day)
  prevdays <- dayfile[1,]
  prevdays <- prevdays[-c(1),]

  for (j in 1:length(unique(fishsub$day)))
    
    {
  
    daysub <- rbind(prevdays, subset(fishsub, day == days[[j]]))
    
    coords <- daysub[,c(1, 5, 6)] # extract x,y coords and fish id from dayfile
    coordinates(coords) <- c('PosX', 'PosY') # convert to spatial points data frame object
    ud <- kernelUD(coords, h = 'href', grid = xy, kern = 'bivnorm') # KUD calculation for adehabitatHR package
  
    ka <- kernel.area(ud, percent = c(50, 95), unin = 'm', unout = 'm2') # calculates area of KUD50, KUD95
  
    kud50 <- c(kud50, ka[1,1])
    kud95 <- c(kud95, ka[2,1])
    
    prevdays <- daysub
  
    }
  
  kud50.cum <- rbind(kud50.cum, kud50)
  kud95.cum <- rbind(kud95.cum, kud95)
  
  }

kud50.cum <- t(kud50.cum)
rownames(kud50.cum) <- days
colnames(kud50.cum) <- fish

kud95.cum <- t(kud95.cum)
rownames(kud95.cum) <- days
colnames(kud95.cum) <- fish

#plot cumulative kuds for all fish

par(mfrow=c(1,2))
plot(kud50.cum[,as.character(fish[1])], type = 'o', ylim = c(0,signif(max(kud50.cum), 2)))
for (k in 2:length(fish)){lines(kud50.cum[,as.character(fish[k])], type = 'o')}

plot(kud95.cum[,as.character(fish[1])], type = 'o', ylim = c(0,signif(max(kud95.cum), 2)))
for (k in 2:length(fish)){lines(kud95.cum[,as.character(fish[k])], type = 'o')}
par(mfrow=c(1,1))

# calculate asymptotes

asym <- numeric()

for (m in 1:length(fish))
  {
  
  kuddiff <- round(c(NA, abs(diff(kud50.cum[,as.character(fish[m])], 1)))/kud50.cum[,as.character(fish[m])], 3)

  for (n in 2:(length(kuddiff)-1))
    {
    if (kuddiff[n] <0.05 & kuddiff[n+1] <0.05){
    day.asym <- n+1
    break
    } else {
    day.asym <- NA
  }
  }
  asym <- c(asym, day.asym)
}

asym <- as.double(rownames(kud50.cum)[1])+asym-1
kud50.cum <- rbind(kud50.cum, asym)

asym <- numeric()

for (m in 1:length(fish))
{
  
  kuddiff <- round(c(NA, abs(diff(kud95.cum[,as.character(fish[m])], 1)))/kud95.cum[,as.character(fish[m])], 3)
  
  for (n in 2:(length(kuddiff)-1))
  {
    if (kuddiff[n] <0.05 & kuddiff[n+1] <0.05){
      day.asym <- n+1
      break
    } else {
      day.asym <- NA
    }
  }
  asym <- c(asym, day.asym)
}

asym <- as.double(rownames(kud95.cum)[1])+asym-1
kud95.cum <- rbind(kud95.cum, asym)

write.csv(kud50.cum, 'cumulativeKUD50.csv')
write.csv(kud95.cum, 'cumulativeKUD95.csv')


# code to calculate index of reuse (IOR) for each fish in loaded dayfile and save as csv -------------------------------------------


fish <- unique(dayfile$Period)

if(unique(dayfile$PEN == 7)){
  
  x <- seq(0, 50, by = 0.5)
  y <- seq(0, 50, by = 0.5)
  xy <- expand.grid(x=x, y=y)
  coordinates(xy) <- ~x+y
  gridded(xy) <- TRUE
  class(xy)
  
} else {
  
  x <- seq(25, 70, by = 0.5)
  y <- seq(0, 50, by = 0.5)
  xy <- expand.grid(x=x, y=y)
  coordinates(xy) <- ~x+y
  gridded(xy) <- TRUE
  class(xy)  
  
}  

kud50.ior <- data.frame()
kud95.ior <- data.frame()

for (i in 1:length(fish))
  
{
  
  ior50 <- numeric()
  ior95 <- numeric()
  fishsub <- subset(dayfile, Period == fish[[i]])
  days <- unique(fishsub$day)
  #prevdays <- dayfile[1,]
  #prevdays <- prevdays[-c(1),]
  
  
  daysub <- subset(fishsub, day == days[[1]])
  
  # calculate kuds for day 1
  
  coords <- daysub[,c(1, 5, 6)] # extract x,y coords and fish id from dayfile
  coordinates(coords) <- c('PosX', 'PosY') # convert to spatial points data frame object
  ud <- kernelUD(coords, h = 'href', grid = xy, kern = 'bivnorm') # KUD calculation for adehabitatHR package
  ka1 <- kernel.area(ud, percent = c(50, 95), unin = 'm', unout = 'm2') # calculates area of KUD50, KUD95
  
  kud1 <- coords # send coords to day 1 kud matrix
  kud1$Period <- 1 # recode ID to 1
  
  # calculae kuds for subsequent days and calculate iors
  
  for (j in 2:length(unique(fishsub$day)))
    
  {
    
    daysub <- subset(fishsub, day == days[[j]])
    
    coords <- daysub[,c(1, 5, 6)] # extract x,y coords and fish id from dayfile
    coordinates(coords) <- c('PosX', 'PosY') # convert to spatial points data frame object
    ud <- kernelUD(coords, h = 'href', grid = xy, kern = 'bivnorm') # KUD calculation for adehabitatHR package
    
    ka2 <- kernel.area(ud, percent = c(50, 95), unin = 'm', unout = 'm2') # calculates area of KUD50, KUD95
    
    kud2 <- coords # send coords to day 2 kud matrix
    kud2$Period <- 2 # recode ID to 2
    
    kud <- rbind(kud2, kud1) # combine coords for 2 days
    ov95 <- kerneloverlap(kud, method = 'HR', percent = 95) # calculate proportion 95% overlap of 2 days
    ov50 <- kerneloverlap(kud, method = 'HR', percent = 50) # calculate proportion 50% overlap of 2 days
    
    ov95 <- ov95[1,2]*ka2[2,1] # calculate area of kud95 overlap from proportion
    ov50 <- ov50[1,2]*ka2[1,1] # calculate area of kud50 overlap from proportion
    
    ta95 <- ka1[2,1]+ka2[2,1]
    ta50 <- ka1[1,1]+ka2[1,1]
    
    ior95 <- c(ior95, ov95/ta95)
    ior50 <- c(ior50, ov50/ta50)
    
    
    #kud50 <- c(kud50, ka[1,1])
    #kud95 <- c(kud95, ka[2,1])
    
    #prevdays <- daysub
    
    kud1 <- kud2
    kud1$Period <- 1
    ka1 <- ka2
    
  }
  
  kud50.ior <- rbind(kud50.ior, ior50)
  kud95.ior <- rbind(kud95.ior, ior95)
  
}

kud50.ior <- t(kud50.ior)
rownames(kud50.ior) <- days[-1]
colnames(kud50.ior) <- fish
kud50.ior[is.nan(kud50.ior)] <- 0 # replace NaNs with 0

kud95.ior <- t(kud95.ior)
rownames(kud95.ior) <- days[-1]
colnames(kud95.ior) <- fish
kud95.ior[is.nan(kud95.ior)] <- 0 # replace NaNs with 0

#plot daily IORs for all fish

par(mfrow=c(1,2))
plot(kud50.ior[,as.character(fish[1])], type = 'o', ylim = c(0,signif(max(kud50.ior), 2)))
for (k in 2:length(fish)){lines(kud50.ior[,as.character(fish[k])], type = 'o')}

plot(kud95.ior[,as.character(fish[1])], type = 'o', ylim = c(0,signif(max(kud95.ior), 2)))
for (k in 2:length(fish)){lines(kud95.ior[,as.character(fish[k])], type = 'o')}
par(mfrow=c(1,1))


write.csv(kud50.ior, 'IOR50.csv')
write.csv(kud95.ior, 'IOR95.csv')


# Spectrum wavelet sampling ---------------------------------------------------------------------------------------

# load all data into dayfile then move to daytemp and subset single fish to dayfile, then subset by parameter to test for periodicity, e.g. below 15m

#daytemp <- dayfile

wavfunc <- function(fish.id, subtype, subcode){
  
  # fish/group subset
  dayfile <- subset(daytemp, Period == fish.id) # fish subset
  #dayfile <- subset(daytemp, PEN == 7) # pen subset
  
  #calculate standardised detection frequencies
  
  dayfile <- dayfile[order(dayfile$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
  
  datacut <- data.frame(dayfile$EchoTime, cuts = cut.POSIXt(dayfile$EchoTime, breaks = 'hour', labels = F)) # code hour by factor
  datacut$floor <- floor_date(datacut$dayfile.EchoTime, unit = 'hour') # floor dates to nearest hour
  
  hourbins <- data.frame(unique(datacut$floor), rle(datacut$cuts)$lengths) # create new data frame of hours and sum of pings for each hour
  colnames(hourbins) <- c('Date', 'sum')
  
  binlist <- data.frame(seq(floor_date(min(daytemp$EchoTime), unit = 'hour'), floor_date(max(daytemp$EchoTime), unit = 'hour'), by = 'hour')) # create list of all hours in dataset
  colnames(binlist) <- 'Date'
  
  hourbins <- binlist %>%
    left_join(hourbins, by = c('Date'='Date')) %>% # join time list to hourly ping sum list
    replace_na(list(sum = 1)) # replace nas with 1
  
  rownames(hourbins) <- hourbins$Date
  
  #hourbins <- data.frame(hourbins, daycuts = cut.POSIXt(hourbins$Date, breaks = 'day', labels = F)) # code day by factor
  #daymeans <- data.frame(tapply(hourbins$sum, hourbins$daycuts, mean)) # calculate daily mean ping rate and create new data frame of results
  
  #hourbins$daymean <- as.numeric(daymeans[as.character(hourbins$daycuts),]) # add daily mean ping rate to hourbins dataset
  #hourbins$SDF <- hourbins$sum/hourbins$daymean

  hourmean <- mean(hourbins$sum)
  SF <- hourbins$sum/hourmean # calculate standardising factor from control data
  
  rm(binlist, datacut, hourbins)
  
  
  # location subset
  if (subtype == 'c'){
    dayfile <- subset(dayfile, BIGC == '8CNW' | BIGC == '8CSW' | BIGC == '8CNE' | BIGC == '8CSE') # corner subset
    title <- paste0(as.character(fish.id), ' corners')
  }
  if (subtype == 'h'){
    dayfile <- subset(dayfile, HID == '8WHSW' | HID == '8WHNE') # hide subset
    title <- paste0(as.character(fish.id), ' hides')
  }
  if (subtype == 'd'){
    dayfile <- subset(dayfile, PosZ > 15) # depth subset
    title <- paste0(as.character(fish.id), ' >15m')
  }
  if (subtype == 'fb'){
    dayfile <- subset(dayfile, FDB == '8FBNE' | FDB == '8FBSW') # at feedblock subset
    title <- paste0(as.character(fish.id), ' feed blocks')
  }
  if (subtype == 'bs'){
    dayfile <- subset(dayfile, BS == subcode) # behaviour state subset
    title <- paste0(as.character(fish.id), ' ', as.character(subcode))
  }
  
  # Bin observations into hourly bins
  
  dayfile <- dayfile[order(dayfile$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
  
  datacut <- data.frame(dayfile$EchoTime, cuts = cut.POSIXt(dayfile$EchoTime, breaks = 'hour', labels = F)) # code hour by factor
  datacut$floor <- floor_date(datacut$dayfile.EchoTime, unit = 'hour') # floor dates to nearest hour
  
  hourbins <- data.frame(unique(datacut$floor), rle(datacut$cuts)$lengths) # create new data frame of hours and sum of pings for each hour
  colnames(hourbins) <- c('Date', 'sum')
  
  binlist <- data.frame(seq(floor_date(min(daytemp$EchoTime), unit = 'hour'), floor_date(max(daytemp$EchoTime), unit = 'hour'), by = 'hour')) # create list of all hours in dataset
  colnames(binlist) <- 'Date'
  
  hourbins <- binlist %>%
    left_join(hourbins, by = c('Date'='Date')) %>% # join time list to hourly ping sum list
    replace_na(list(sum = 1)) # replace nas with 1 (0 gives errors!)
  
  rownames(hourbins) <- hourbins$Date
  
  hourbins$SDF <- hourbins$sum/SF # calculate standardised detection frequencies
  
  rm(binlist, datacut)
  
  # create wavelets using WaveletComp package
  
  fish.wav <- analyze.wavelet(hourbins, "SDF",
                              loess.span = 0,
                              dt = 1, dj = 1/50,
                              lowerPeriod = 2,
                              upperPeriod = 48,
                              make.pval = T, n.sim = 10)
  
  # normalise power levels to 1
  pm <- fish.wav$Power # extract power matrix
  pfac <- 1/max(pm) # calculate normalising factor
  fish.wav$Power <- pm*pfac # normalise power matrix so max = 1
  rm(pm, pfac)
  
  wt.image2(fish.wav, color.key = 'i', n.levels = 250, show.date = T, col.contour = 'black', plot.ridge = F, siglvl = 0.05, 
            timelab = 'Date', periodlab = 'scale (h)', main = title,
            legend.params = list(lab = "wavelet power levels", label.digits = 2))
  
}



reconstruct(fish.wav, lwd = c(1,2), legend.coords = "bottomleft", plot.waves = F)#, sel.period = 24)

wt.avg(fish.wav, 'sum')


# code for removing dead fish from data-------------------------------------

livefish <- c(8051, 8667, 8723, 7239, 7771)

deadfish <- subset(fishids, !(fishids %in% livefish)) # create vector of dead fish

multi.batch.remove(deadfish, '213', 1)

# Workings for hexplot distribution analysis by various factors------------------------------------------------------------
# hexplot all angles and grid by tide
dayfile <- filter(genbehav.df, TID != 'Z')

hexplot.all(15, 'xy', 'E')
hexplot.all(15, 'yz', 'E')
hexplot.all(15, 'xz', 'E')

hexplot + 
  facet_wrap(~TID, labeller = as_labeller(c('H' = 'High', 'HL' = 'Ebb', 'L' = 'Low', 'LH' = 'Flood'))) + 
  ggtitle('Pen 15 distribution by tide')

# hexplot by time of day and grid by angle
#dayfile <- filter(genbehav.df, SUN == 'W')
dayfile <- filter(genbehav.df, SUN == 'W')
hexplot.all(12, 'xy', 'W')
hexxy <- hexplot + theme(legend.position = 'none', plot.margin = unit(c(30, 5.5, 5.5, 5.5), 'points'))
hexplot.all(12, 'xz', 'W')
hexxz <- hexplot + theme(legend.position = 'none', plot.margin = unit(c(30, 5.5, 5.5, 5.5), 'points'))
hexplot.all(12, 'yz', 'W')
hexyz <- hexplot + theme(plot.margin = unit(c(30, 5.5, 5.5, 5.5), 'points'))
plot_grid(hexxy, hexxz, hexyz, ncol = 3, rel_widths = c(0.315, 0.315, 0.37)) + draw_label('Pen 12 dawn distribution', 0.1, 0.95)

dayfile <- filter(genbehav.df, SUN == 'D')
hexplot.all(12, 'xy', 'W')
hexxy <- hexplot + theme(legend.position = 'none', plot.margin = unit(c(30, 5.5, 5.5, 5.5), 'points'))
hexplot.all(12, 'xz', 'W')
hexxz <- hexplot + theme(legend.position = 'none', plot.margin = unit(c(30, 5.5, 5.5, 5.5), 'points'))
hexplot.all(12, 'yz', 'W')
hexyz <- hexplot + theme(plot.margin = unit(c(30, 5.5, 5.5, 5.5), 'points'))
plot_grid(hexxy, hexxz, hexyz, ncol = 3, rel_widths = c(0.315, 0.315, 0.37)) + draw_label('Pen 12 day distribution', 0.1, 0.95)

dayfile <- filter(genbehav.df, SUN == 'K')
hexplot.all(12,'xy', 'W')
hexxy <- hexplot + theme(legend.position = 'none', plot.margin = unit(c(30, 5.5, 5.5, 5.5), 'points'))
hexplot.all(12, 'xz', 'W')
hexxz <- hexplot + theme(legend.position = 'none', plot.margin = unit(c(30, 5.5, 5.5, 5.5), 'points'))
hexplot.all(12,'yz', 'W')
hexyz <- hexplot + theme(plot.margin = unit(c(30, 5.5, 5.5, 5.5), 'points'))
plot_grid(hexxy, hexxz, hexyz, ncol = 3, rel_widths = c(0.315, 0.315, 0.37)) + draw_label('Pen 12 dusk distribution', 0.1, 0.95)

#dayfile <- filter(genbehav.df, SUN == 'N')
dayfile <- filter(genbehav.df, SUN == 'N')
hexplot.all(15, 'xy', 'W')
hexxy <- hexplot + theme(legend.position = 'none', plot.margin = unit(c(30, 5.5, 5.5, 5.5), 'points'))
hexplot.all(15, 'xz', 'W')
hexxz <- hexplot + theme(legend.position = 'none', plot.margin = unit(c(30, 5.5, 5.5, 5.5), 'points'))
hexplot.all(15, 'yz', 'W')
hexyz <- hexplot + theme(plot.margin = unit(c(30, 5.5, 5.5, 5.5), 'points'))
plot_grid(hexxy, hexxz, hexyz, ncol = 3, rel_widths = c(0.315, 0.315, 0.37)) + draw_label('Pen 15 night distribution', 0.1, 0.95)

# daily hexplots of day and night
dayfile <- filter(genbehav.df, SUN == 'D')
hexplot.all(15, 'xz', 'E')
hexplot + 
  facet_wrap(~day) + 
  ggtitle('Pen 15 daytime daily distribution')

dayfile <- filter(genbehav.df, SUN == 'N')
hexplot.all(12, 'xz', 'W')
hexplot + 
  facet_wrap(~day) + 
  ggtitle('Pen 12 night time daily distribution')

#--------------------------------------------------------------------------------------------------------------------------------------------

#STATS

#one-way anova
dayfile2 <- subset(dayfile, BLSEC <10)
dayfile2$log_BLSEC <- log(dayfile2$BLSEC) # log transformation for normality
dayfile2 <- dayfile2[!is.infinite(dayfile2$log_BLSEC),] # remove infinite observations and copy to new data frame
dayfile2$log_BLSEC_trans <- dayfile2$log_BLSEC - floor(min(dayfile2$log_BLSEC)) # transpose to make all values positive (needs testing)
dayfile2$day <- ceiling((as.integer(dayfile2$EchoTime)-(17058*86400))/86400) # add new column of day No. (17058 is days since 1st Jan 1970 for start of experiment)


anova1 <- aov(log_BLSEC_trans~PEN, data = dayfile2) # one-way anova
summary(anova1)
sumanova <- unlist(summary(anova1)) # turns anova summary into vector

hist(unlist(subset(dayfile, PEN == 8, select = PosZ))) # histogram of pen 8 depths


# One-way anova to compare activity at different times of day-------------------------------------------------------------
# all comparisons are highly significant due to the big dataset (big Df)
# use eta squared to measure effect size

# extract required data
actdf <- dayfile[c(1, 3, 12, 46)] # extract required variables
actdf <- subset(actdf, Period == 8081) # extract single fish
actdf <- subset(actdf, SUN == 'D' | SUN == 'W' | SUN == 'K' | SUN == 'N') # extract observations with time of day codes
#actdf <- subset(actdf, SUN == 'D' | SUN == 'N') # extract observations with time of day codes
actdf$log_BLSEC <- log(actdf$BLSEC) # log transform data
actdf <- actdf[!is.infinite(actdf$log_BLSEC),] # remove infinite observations
actdf <- na.omit(actdf) # remove NAs
actdf$log_BLSEC_trans <- actdf$log_BLSEC - floor(min(actdf$log_BLSEC)) # transpose so all observations are positive


aovact <- aov(log_BLSEC_trans~SUN, data = actdf)
summary(aovact)
TukeyHSD(aovact)

boxplot(log_BLSEC_trans~SUN, data = actdf)
hist(log(actdf[which(actdf$SUN == 'W'),'BLSEC']))

library(lsr)
etaSquared(aovact)
# significant effect sizes
# small >0.01, medium >0.06, large >0.14

library(effsize)
cohen.d(actdf$log_BLSEC_trans, actdf$SUN)
# significant effect sizes
# small >0.2, medium >0.5, large >0.8


# Power analysis using pwr package-----------------------------

library(pwr)
pwr.anova.test(k = 3, f = 0.1, sig.level = 0.05, n = 24)

pwr.t.test(n = 36, d = 0.5, sig.level = 0.05, type = 'paired')


# One-way anova to compare depth and activity at different times of day for Precon B study -------------------------------------------------------------


# extract required data
statdf <- dayfile[c(1, 3, 4, 7, 12, 46)] # extract required variables
statdf <- subset(statdf, PEN == 7) # extract group
#statdf <- subset(statdf, SUN == 'D')
statdf <- subset(statdf, SUN == 'D' | SUN == 'N') # extract observations with day and night codes
statdf$date <- as.Date(statdf$EchoTime + hours(1))
statdf <- subset(statdf, date == '2016-09-20')
#actdf <- subset(actdf, SUN == 'D' | SUN == 'N') # extract observations with time of day codes


statdf$log_BLSEC <- log(statdf$BLSEC) # log transform data
statdf <- statdf[!is.infinite(statdf$log_BLSEC),] # remove infinite observations
statdf <- na.omit(statdf) # remove NAs
statdf$log_BLSEC_trans <- statdf$log_BLSEC - floor(min(statdf$log_BLSEC)) # transpose so all observations are positive

# one-way anova for activity
aovact <- aov(log_BLSEC_trans~SUN, data = statdf)
summary(aovact)
TukeyHSD(aovact)

# Mann-Whitney U Test for non-parametric depth data (coded in R as wilcox.test)
mwdep <- wilcox.test(statdf$PosZ~statdf$SUN)


library(effsize)
cohen.d(statdf$log_BLSEC_trans, statdf$SUN)
# significant effect sizes
# small >0.2, medium >0.5, large >0.8



# Calculation of detection rates for methods paper

dayfile$date <- as.Date(dayfile$EchoTime)
dayfile <- dayfile %>% filter(Period %in% c(7351, 7071, 8499, 8387, 7939, 8919, 8023, 8947, 8107, 8219, 7463, 7435, 7211, 8751, 8555,
                                            7099, 8415, 8275, 7967, 8583, 8611, 8779, 7659, 8667, 7799, 8723, 7239, 7855, 8331, 7771,
                                            8527, 8303, 8247, 8471, 8443, 8639, 7995, 7547, 8135, 7127, 7519, 7743, 8807, 7715, 8975,
                                            7911, 8359, 7687, 7603, 8863, 7407, 7827, 8191, 7323, 8835, 8695, 7155, 8163, 8079, 7043))

lhag <- select(dayfile, Period, PEN, date) %>% 
  count(Period, date, name = 'dpings') %>% 
  mutate(ppings = round((60/(Period/1000))*1440)) %>% # calculate No. of pings in one day based on PRI
  mutate(pdiff = ppings-dpings) %>%
  mutate(pcdet = (dpings/ppings)*100) %>% 
  filter(date != '2018-06-01') %>% # run to here for plot of each tag
  group_by(date) %>% 
  summarise(mean = mean(pcdet), sd = sd(pcdet)) %>%
  mutate(day = seq(1, 30, 1)) %>%
  filter(day != 13 & day != 14)

write.csv(lhag, 'G:/Data/2018 Lumpfish Husbandry/Data processing/6a. Coded Day CSV/30days/DetectionRates.csv')

ggplot(lhag) + 
  geom_line(aes(x = day, y = mean)) +
  geom_point(aes(x = day, y = mean)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(10, 100, 10), name = 'Detection rate (%)') +
  scale_x_continuous(limits = c(0, 31), breaks = seq(0, 30, 5), name = 'Experiment day') +
  geom_errorbar(aes(x = day, ymin = mean-sd, ymax = mean+sd))

# plot of daily detection rates for each tag
ggplot(lhag) +
  geom_line(aes(x = date, y = pcdet, group = Period)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(10, 100, 10), name = 'Detection rate (%)') +
  facet_wrap(~Period)

# plot detection rates for all trials
wlag$trial <- 'wrasse vs. lumpfish'
wfag$trial <- 'wild vs. farmed wrasse'
paag$trial <- 'hatchery acclimated'
pbag$trial <- 'hatchery & pen acclimated'
lhag$trial <- 'lumpfish husbandry'
wlag$studynum <- '(a) Trial 1'
wfag$studynum <- '(b) Trial 2'
paag$studynum <- '(c) Trial 3'
pbag$studynum <- '(d) Trial 4'
lhag$studynum <- '(e) Trial 5'

detrates <- bind_rows(wlag, wfag, paag, pbag, lhag)

dratesplot <- ggplot(detrates) + 
  geom_line(aes(x = day, y = mean, group = trial)) +
  geom_point(aes(x = day, y = mean, group = trial)) +
  #geom_errorbar(data = lhag, aes(x = day, ymin = mean-sd, ymax = mean+sd)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(10, 100, 10), name = 'Detection rate (%)', expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 31), breaks = seq(0, 30, 5), name = 'Experiment day', expand = c(0, 0))

# facet plot detection rates for all trials
dratesplotf <- ggplot(detrates) + 
  geom_line(aes(x = day, y = mean, group = studynum)) +
  geom_point(aes(x = day, y = mean, group = studynum)) +
  geom_errorbar(aes(x = day, ymin = mean-sd, ymax = mean+sd, group = studynum)) +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 10), name = 'Detection rate (%)', expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 31), breaks = seq(0, 30, 5), name = 'Experiment day', expand = c(0, 0)) +
  facet_wrap(~studynum, ncol = 2, scales = 'free')

model <- aov(mean~trial, detrates)
TukeyHSD(model)

# FUNCTIONS----------------------------------------------------------------------------------------------------------------------------------


# 1. FUNCTION TO CALCULATE SUMMARY OF FISH LOCATIONS
locations <- function()
{
  # pen 12 location summary
  dayfile.bot <- subset(dayfile, BOT == 'B' & PEN == '12' & SEC >= 0)
  dayfile.top <- subset(dayfile, BOT == 'Z' & PEN == '12' & SEC >= 0)
  dayfile.out <- subset(dayfile, OUT == '12OE' & PEN == '12' & SEC >= 0 | OUT == '12OS' & PEN == '12' & SEC >= 0 | OUT == '12ON' & PEN == '12' & SEC >= 0 | OUT == '12OW' & PEN == '12' & SEC >= 0)
  dayfile.edg <- subset(dayfile, EDG == '12EN' & PEN == '12' & SEC >= 0 | EDG == '12EW' & PEN == '12' & SEC >= 0 | EDG == '12ES' & PEN == '12' & SEC >= 0 | EDG == '12EE' & PEN == '12' & SEC >= 0)
  dayfile.hidc <- subset(dayfile, BIGC == '12CNW' & PEN == '12' & SEC >= 0 | BIGC == '12CSE' & PEN == '12' & SEC >= 0)
  dayfile.mtc <- subset(dayfile, BIGC == '12CSW' & PEN == '12' & SEC >= 0 | BIGC == '12CNE' & PEN == '12' & SEC >= 0)
  dayfile.cen <- subset(dayfile, CEN == '12MH' & PEN == '12' & SEC >= 0 | CEN == '12MM' & PEN == '12' & SEC >= 0 | CEN == '12ML' & PEN == '12' & SEC >= 0)
  dayfile.hid <- subset(dayfile, HID == 'P12HW' & PEN == '12' & SEC >= 0 | HID == 'P12HE' & PEN == '12' & SEC >= 0)
  dayfile.fdb <- subset(dayfile, FS == 'FS12'  & PEN == '12' & SEC >= 0| FS == 'FS12' & PEN == '12' & SEC >= 0)
  #location.sum <- data.frame(c(nrow(dayfile.bot), nrow(dayfile.top), nrow(dayfile.out), nrow(dayfile.edg), nrow(dayfile.bigc), nrow(dayfile.cen), nrow(dayfile.hid)))
  location.sum <- data.frame(c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.hidc$SEC, na.rm = T)/3600, sum(dayfile.mtc$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fdb$SEC, na.rm = T)/3600))
  rownames(location.sum) <- c('<15m', '>15m', 'outer', 'edge', 'hide_corner', 'empty_corner', 'centre', 'hides', 'feed block')
  colnames(location.sum) <- 'P12'
  
  # pen 14 location summary
  dayfile.bot <- subset(dayfile, BOT == 'B' & PEN == '14' & SEC >= 0)
  dayfile.top <- subset(dayfile, BOT == 'Z' & PEN == '14' & SEC >= 0)
  dayfile.out <- subset(dayfile, OUT == '14OE' & PEN == '14' & SEC >= 0 | OUT == '14OS' & PEN == '14' & SEC >= 0 | OUT == '14ON' & PEN == '14' & SEC >= 0 | OUT == '14OW' & PEN == '14' & SEC >= 0)
  dayfile.edg <- subset(dayfile, EDG == '14EN' & PEN == '14' & SEC >= 0 | EDG == '14EW' & PEN == '14' & SEC >= 0 | EDG == '14ES' & PEN == '14' & SEC >= 0 | EDG == '14EE' & PEN == '14' & SEC >= 0)
  dayfile.hidc <- subset(dayfile, BIGC == '14CNW' & PEN == '14' & SEC >= 0 | BIGC == '14CSE' & PEN == '14' & SEC >= 0)
  dayfile.mtc <- subset(dayfile, BIGC == '14CSW' & PEN == '14' & SEC >= 0 | BIGC == '14CNE' & PEN == '14' & SEC >= 0)
  dayfile.cen <- subset(dayfile, CEN == '14MH' & PEN == '14' & SEC >= 0 | CEN == '14MM' & PEN == '14' & SEC >= 0 | CEN == '14ML' & PEN == '14' & SEC >= 0)
  dayfile.hid <- subset(dayfile, HID == 'P14HW' & PEN == '14' & SEC >= 0 | HID == 'P14HE' & PEN == '14' & SEC >= 0)
  dayfile.fdb <- subset(dayfile, FS == 'FS14'  & PEN == '14' & SEC >= 0| FS == 'FS14' & PEN == '14' & SEC >= 0)
  #location.sum <- data.frame(c(nrow(dayfile.bot), nrow(dayfile.top), nrow(dayfile.out), nrow(dayfile.edg), nrow(dayfile.bigc), nrow(dayfile.cen), nrow(dayfile.hid)))
  location.sum$P14 <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.hidc$SEC, na.rm = T)/3600, sum(dayfile.mtc$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fdb$SEC, na.rm = T)/3600)
  rownames(location.sum) <- c('<15m', '>15m', 'outer', 'edge', 'hide_corner', 'empty_corner', 'centre', 'hides', 'feed block')
  
  # pen 15 location summary
  dayfile.bot <- subset(dayfile, BOT == 'B' & PEN == '15' & SEC >= 0)
  dayfile.top <- subset(dayfile, BOT == 'Z' & PEN == '15' & SEC >= 0)
  dayfile.out <- subset(dayfile, OUT == '15OE' & PEN == '15' & SEC >= 0 | OUT == '15OS' & PEN == '15' & SEC >= 0 | OUT == '15ON' & PEN == '15' & SEC >= 0 | OUT == '15OW' & PEN == '15' & SEC >= 0)
  dayfile.edg <- subset(dayfile, EDG == '15EN' & PEN == '15' & SEC >= 0 | EDG == '15EW' & PEN == '15' & SEC >= 0 | EDG == '15ES' & PEN == '15' & SEC >= 0 | EDG == '15EE' & PEN == '15' & SEC >= 0)
  dayfile.hidc <- subset(dayfile, BIGC == '15CNW' & PEN == '15' & SEC >= 0 | BIGC == '15CSE' & PEN == '15' & SEC >= 0)
  dayfile.mtc <- subset(dayfile, BIGC == '15CSW' & PEN == '15' & SEC >= 0 | BIGC == '15CNE' & PEN == '15' & SEC >= 0)
  dayfile.cen <- subset(dayfile, CEN == '15MH' & PEN == '15' & SEC >= 0 | CEN == '15MM' & PEN == '15' & SEC >= 0 | CEN == '15ML' & PEN == '15' & SEC >= 0)
  dayfile.hid <- subset(dayfile, HID == 'P15HW' & PEN == '15' & SEC >= 0 | HID == 'P15HE' & PEN == '15' & SEC >= 0)
  dayfile.fdb <- subset(dayfile, FS == 'FS15'  & PEN == '15' & SEC >= 0)
  #location.sum <- data.frame(c(nrow(dayfile.bot), nrow(dayfile.top), nrow(dayfile.out), nrow(dayfile.edg), nrow(dayfile.bigc), nrow(dayfile.cen), nrow(dayfile.hid)))
  location.sum$P15 <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.hidc$SEC, na.rm = T)/3600, sum(dayfile.mtc$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fdb$SEC, na.rm = T)/3600)
  rownames(location.sum) <- c('<15m', '>15m', 'outer', 'edge', 'hide_corner', 'empty_corner', 'centre', 'hides', 'feed block')
  location.sum
  location.sum <<- location.sum
  
}

# 2a. location summary for multiple day files
batch.locations <- function()
{
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  locations.P7 <- data.frame(c('0', '0', '0', '0', '0', '0', '0', '0', '0'))
  colnames(locations.P7) <- 'ID'
  rownames(locations.P7) <- c('P7_<15m', 'P7_>15m', 'P7_outer', 'P7_edge', 'P7_hidecorner', 'P7_emptycorner', 'P7_centre', 'P7_hides', 'P7_feedblock')
  locations.P8 <- data.frame(c('0', '0', '0', '0', '0', '0', '0', '0', '0'))
  colnames(locations.P8) <- 'ID'
  rownames(locations.P8) <- c('P8_<15m', 'P8_>15m', 'P8_outer', 'P8_edge', 'P8_hidecorner', 'P8_emptycorner', 'P8_centre', 'P8_hides', 'P8_feedblock')
  
  for (i in 1:length(files))
  {
    dayfile.loc <- files[[i]]
    dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = dayfile.classes)
                      
    
    #SORT BY TIME AND TAG
    dayfile <- dayfile[order(dayfile$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
    dayfile <- dayfile[order(dayfile$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
    
    
    # pen 7 location summary
    dayfile.bot <- subset(dayfile, BOT == 'B' & PEN == '7' & SEC >= 0)
    dayfile.top <- subset(dayfile, BOT == 'Z' & PEN == '7' & SEC >= 0)
    dayfile.out <- subset(dayfile, OUT == '7OE' & PEN == '7' & SEC >= 0 | OUT == '7OS' & PEN == '7' & SEC >= 0 | OUT == '7ON' & PEN == '7' & SEC >= 0 | OUT == '7OW' & PEN == '7' & SEC >= 0)
    dayfile.edg <- subset(dayfile, EDG == '7EN' & PEN == '7' & SEC >= 0 | EDG == '7EW' & PEN == '7' & SEC >= 0 | EDG == '7ES' & PEN == '7' & SEC >= 0 | EDG == '7EE' & PEN == '7' & SEC >= 0)
    dayfile.hidc <- subset(dayfile, BIGC == '7CNW' & PEN == '7' & SEC >= 0 | BIGC == '7CSE' & PEN == '7' & SEC >= 0)
    dayfile.mtc <- subset(dayfile, BIGC == '7CSW' & PEN == '7' & SEC >= 0 | BIGC == '7CNE' & PEN == '7' & SEC >= 0)
    dayfile.cen <- subset(dayfile, CEN == '7MH' & PEN == '7' & SEC >= 0 | CEN == '7MM' & PEN == '7' & SEC >= 0 | CEN == '7ML' & PEN == '7' & SEC >= 0)
    dayfile.hid <- subset(dayfile, HID == '7WHSE' & PEN == '7' & SEC >= 0 | HID == '7WHNW' & PEN == '7' & SEC >= 0)
    dayfile.fdb <- subset(dayfile, FDB == '7FBSE'  & PEN == '7' & SEC >= 0| FDB == '7FBNW' & PEN == '7' & SEC >= 0)
    locations.P7[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.hidc$SEC, na.rm = T)/3600, sum(dayfile.mtc$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fdb$SEC, na.rm = T)/3600)
    
    # pen 8 location summary
    dayfile.bot <- subset(dayfile, BOT == 'B' & SEC >= 0 & PEN == '8' & SEC >= 0)
    dayfile.top <- subset(dayfile, BOT == 'Z' & PEN == '8' & SEC >= 0)
    dayfile.out <- subset(dayfile, OUT == '8OE' & PEN == '8' & SEC >= 0| OUT == '8OS' & PEN == '8' & SEC >= 0 | OUT == '8ON' & PEN == '8' & SEC >= 0 | OUT == '8OW' & PEN == '8' & SEC >= 0)
    dayfile.edg <- subset(dayfile, EDG == '8EN' & PEN == '8' & SEC >= 0 | EDG == '8EW' & PEN == '8' & SEC >= 0 | EDG == '8ES' & PEN == '8' & SEC >= 0 | EDG == '8EE' & PEN == '8' & SEC >= 0)
    dayfile.hidc <- subset(dayfile, BIGC == '8CSW' & PEN == '8' & SEC >= 0 | BIGC == '8CNE' & PEN == '8' & SEC >= 0)
    dayfile.mtc <- subset(dayfile, BIGC == '8CNW' & PEN == '8' & SEC >= 0 | BIGC == '8CSE' & PEN == '8' & SEC >= 0)
    dayfile.cen <- subset(dayfile, CEN == '8MH' & PEN == '8' & SEC >= 0 | CEN == '8MM' & PEN == '8' & SEC >= 0 | CEN == '8ML' & PEN == '8' & SEC >= 0)
    dayfile.hid <- subset(dayfile, HID == '8WHSW' & PEN == '8' & SEC >= 0 | HID == '8WHNE' & PEN == '8' & SEC >= 0)
    dayfile.fdb <- subset(dayfile, FDB == '8FBSW' & PEN == '8' & SEC >= 0 | FDB == '8FBNE' & PEN == '8' & SEC >= 0)
    locations.P8[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.hidc$SEC, na.rm = T)/3600, sum(dayfile.mtc$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fdb$SEC, na.rm = T)/3600)
    
  }
  location.sum <- rbind(locations.P7, locations.P8)  
  location.sum$ID <- NULL
  location.sum  
  
  #loadWorkbook('LocationsOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('LocationsOutput.xlsx', location.sum, 'Sheet 1')
  
  write.xlsx(location.sum, 'LocationsOutput.xlsx')
}



# 2b. Daily location summary for loaded dataset
daily.locations <- function()
{
  #files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  locations.P12 <- data.frame(c('0', '0', '0', '0', '0', '0', '0'))
  colnames(locations.P12) <- 'ID'
  rownames(locations.P12) <- c('P12_<15m', 'P12_outer', 'P12_edge', 'P12_corner', 'P12_centre', 'P12_hides', 'P12_feedblock')
  locations.P14 <- data.frame(c('0', '0', '0', '0', '0', '0', '0'))
  colnames(locations.P14) <- 'ID'
  rownames(locations.P14) <- c('P14_<15m', 'P14_outer', 'P14_edge', 'P14_corner', 'P14_centre', 'P14_hides', 'P14_feedblock')
  locations.P15 <- data.frame(c('0', '0', '0', '0', '0', '0', '0'))
  colnames(locations.P15) <- 'ID'
  rownames(locations.P15) <- c('P15_<15m', 'P15_outer', 'P15_edge', 'P15_corner', 'P15_centre', 'P15_hides', 'P15_feedblock')
  
  for (i in 1:length(unique(dayfile$day)))
  {
    #dayfile.loc <- files[[i]]
    #dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = dayfile.classes)
    daysub <- subset(dayfile, day == unique(dayfile$day)[[i]])
    
    #SORT BY TIME AND TAG
    #dayfile <- dayfile[order(dayfile$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
    #dayfile <- dayfile[order(dayfile$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
    
    
    # pen 12 location summary
    pen12 <- filter(daysub, PEN == '12' & SEC >= 0)
    dayfile.bot <- subset(pen12, BOT == 'B')
    #dayfile.top <- subset(pen12, BOT == 'Z' & PEN == '12' & SEC >= 0)
    dayfile.cor <- subset(pen12, BIGC == '12CNW' | BIGC == '12CSE' | BIGC == '12CSW' | BIGC == '12CNE')
    dayfile.out <- subset(pen12, OUT == '12OE' | OUT == '12OS' | OUT == '12ON' | OUT == '12OW')
    dayfile.edg <- subset(pen12, EDG == '12EN' & HID == '' | EDG == '12EW' & HID == ''| EDG == '12ES'& HID == ''| EDG == '12EE' & HID == '')
    #dayfile.mtc <- subset(pen12, BIGC == '12CSW' & PEN == '12' & SEC >= 0 | BIGC == '12CNE' & PEN == '12' & SEC >= 0)
    dayfile.cen <- subset(pen12, CEN == '12MH'  & HID == '' | CEN == '12MM'  & HID == '' | CEN == '12ML' & HID == '')
    dayfile.hid <- subset(pen12, HID == 'P12HE' | HID == 'P12HW')
    dayfile.fdb <- subset(pen12, FS == 'FS12')
    #locations.P12[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.hidc$SEC, na.rm = T)/3600, sum(dayfile.mtc$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fdb$SEC, na.rm = T)/3600)
    locations.P12[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.cor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fdb$SEC, na.rm = T)/3600)
    
    # pen 14 location summary
    pen14 <- filter(daysub, PEN == '14' & SEC >= 0)
    dayfile.bot <- subset(pen14, BOT == 'B')
    #dayfile.top <- subset(pen14, BOT == 'Z' & PEN == '14' & SEC >= 0)
    dayfile.cor <- subset(pen14, BIGC == '14CNW' | BIGC == '14CSE' | BIGC == '14CSW' | BIGC == '14CNE')
    dayfile.out <- subset(pen14, OUT == '14OE' | OUT == '14OS' | OUT == '14ON' | OUT == '14OW')
    dayfile.edg <- subset(pen14, EDG == '14EN' & HID == '' | EDG == '14EW' & HID == ''| EDG == '14ES'& HID == ''| EDG == '14EE' & HID == '')
    #dayfile.mtc <- subset(pen14, BIGC == '14CSW' & PEN == '14' & SEC >= 0 | BIGC == '14CNE' & PEN == '14' & SEC >= 0)
    dayfile.cen <- subset(pen14, CEN == '14MH'  & HID == '' | CEN == '14MM'  & HID == '' | CEN == '14ML' & HID == '')
    dayfile.hid <- subset(pen14, HID == 'P14HE' | HID == 'P14HW')
    dayfile.fdb <- subset(pen14, FS == 'FS14')
    #locations.P14[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.hidc$SEC, na.rm = T)/3600, sum(dayfile.mtc$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fdb$SEC, na.rm = T)/3600)
    locations.P14[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.cor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fdb$SEC, na.rm = T)/3600)
    
    # pen 15 location summary
    pen15 <- filter(daysub, PEN == '15' & SEC >= 0)
    dayfile.bot <- subset(pen15, BOT == 'B')
    #dayfile.top <- subset(pen15, BOT == 'Z' & PEN == '15' & SEC >= 0)
    dayfile.cor <- subset(pen15, BIGC == '15CNW' | BIGC == '15CSE' | BIGC == '15CSW' | BIGC == '15CNE')
    dayfile.out <- subset(pen15, OUT == '15OE' | OUT == '15OS' | OUT == '15ON' | OUT == '15OW')
    dayfile.edg <- subset(pen15, EDG == '15EN' & HID == '' | EDG == '15EW' & HID == ''| EDG == '15ES'& HID == ''| EDG == '15EE' & HID == '')
    #dayfile.mtc <- subset(pen15, BIGC == '15CSW' & PEN == '15' & SEC >= 0 | BIGC == '15CNE' & PEN == '15' & SEC >= 0)
    dayfile.cen <- subset(pen15, CEN == '15MH'  & HID == '' | CEN == '15MM'  & HID == '' | CEN == '15ML' & HID == '')
    dayfile.hid <- subset(pen15, HID == 'P15HE' | HID == 'P15HW')
    dayfile.fdb <- subset(pen15, FS == 'FS15')
    #locations.P15[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.hidc$SEC, na.rm = T)/3600, sum(dayfile.mtc$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fdb$SEC, na.rm = T)/3600)
    locations.P15[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.cor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fdb$SEC, na.rm = T)/3600)
    
  }
  location.sum <- rbind(locations.P12, locations.P14, locations.P15)  
  location.sum$ID <- NULL
  location.sum  
  
  #loadWorkbook('LocationsOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('LocationsOutput.xlsx', location.sum, 'Sheet 1')
  
  write.xlsx(location.sum, 'LocationsOutput.xlsx')
}


# 2c. Location summary by fish for loaded dataset
fish.locations <- function()
{
  locations.P12 <- data.frame(c('0', '0', '0', '0', '0', '0', '0'))
  colnames(locations.P12) <- 'ID'
  rownames(locations.P12) <- c('P12_<15m', 'P12_outer', 'P12_edge', 'P12_corner', 'P12_centre', 'P12_hides', 'P12_feedblock')
  locations.P14 <- data.frame(c('0', '0', '0', '0', '0', '0', '0'))
  colnames(locations.P14) <- 'ID'
  rownames(locations.P14) <- c('P14_<15m', 'P14_outer', 'P14_edge', 'P14_corner', 'P14_centre', 'P14_hides', 'P14_feedblock')
  locations.P15 <- data.frame(c('0', '0', '0', '0', '0', '0', '0'))
  colnames(locations.P15) <- 'ID'
  rownames(locations.P15) <- c('P15_<15m', 'P15_outer', 'P15_edge', 'P15_corner', 'P15_centre', 'P15_hides', 'P15_feedblock')
  
  for (i in 1:length(unique(dayfile$Period)))
  {
    daysub <- subset(dayfile, Period == unique(dayfile$Period)[[i]])
    
    # pen 12 location summary
    pen12 <- filter(daysub, PEN == '12' & SEC >= 0)
    dayfile.bot <- subset(pen12, BOT == 'B')
    dayfile.cor <- subset(pen12, BIGC == '12CNW' | BIGC == '12CSE' | BIGC == '12CSW' | BIGC == '12CNE')
    dayfile.out <- subset(pen12, OUT == '12OE' | OUT == '12OS' | OUT == '12ON' | OUT == '12OW')
    dayfile.edg <- subset(pen12, EDG == '12EN' & HID == '' | EDG == '12EW' & HID == ''| EDG == '12ES'& HID == ''| EDG == '12EE' & HID == '')
    dayfile.cen <- subset(pen12, CEN == '12MH'  & HID == '' | CEN == '12MM'  & HID == '' | CEN == '12ML' & HID == '')
    dayfile.hid <- subset(pen12, HID == 'P12HE' | HID == 'P12HW')
    dayfile.fdb <- subset(pen12, FS == 'FS12')
    locations.P12[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.cor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fdb$SEC, na.rm = T)/3600)
    
    # pen 14 location summary
    pen14 <- filter(daysub, PEN == '14' & SEC >= 0)
    dayfile.bot <- subset(pen14, BOT == 'B')
    dayfile.cor <- subset(pen14, BIGC == '14CNW' | BIGC == '14CSE' | BIGC == '14CSW' | BIGC == '14CNE')
    dayfile.out <- subset(pen14, OUT == '14OE' | OUT == '14OS' | OUT == '14ON' | OUT == '14OW')
    dayfile.edg <- subset(pen14, EDG == '14EN' & HID == '' | EDG == '14EW' & HID == ''| EDG == '14ES'& HID == ''| EDG == '14EE' & HID == '')
    dayfile.cen <- subset(pen14, CEN == '14MH'  & HID == '' | CEN == '14MM'  & HID == '' | CEN == '14ML' & HID == '')
    dayfile.hid <- subset(pen14, HID == 'P14HE' | HID == 'P14HW')
    dayfile.fdb <- subset(pen14, FS == 'FS14')
    locations.P14[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.cor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fdb$SEC, na.rm = T)/3600)
    
    # pen 15 location summary
    pen15 <- filter(daysub, PEN == '15' & SEC >= 0)
    dayfile.bot <- subset(pen15, BOT == 'B')
    dayfile.cor <- subset(pen15, BIGC == '15CNW' | BIGC == '15CSE' | BIGC == '15CSW' | BIGC == '15CNE')
    dayfile.out <- subset(pen15, OUT == '15OE' | OUT == '15OS' | OUT == '15ON' | OUT == '15OW')
    dayfile.edg <- subset(pen15, EDG == '15EN' & HID == '' | EDG == '15EW' & HID == ''| EDG == '15ES'& HID == ''| EDG == '15EE' & HID == '')
    dayfile.cen <- subset(pen15, CEN == '15MH'  & HID == '' | CEN == '15MM'  & HID == '' | CEN == '15ML' & HID == '')
    dayfile.hid <- subset(pen15, HID == 'P15HE' | HID == 'P15HW')
    dayfile.fdb <- subset(pen15, FS == 'FS15')
    locations.P15[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.cor$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fdb$SEC, na.rm = T)/3600)

  }
  location.sum <- rbind(locations.P12, locations.P14, locations.P15)  
  location.sum$ID <- NULL
  location.sum  
  
  write.xlsx(location.sum, 'LocationsOutputByFish.xlsx')
}


# 3a. depth and activity summary
depact <- function()
{
  day <- subset(dayfile, SUN == 'D' & PEN == '7')
  night <- subset(dayfile, SUN == 'N' & PEN == '7')
  depact.sum <- data.frame(c(format(mean(day$PosZ), digits = 4), format(mean(night$PosZ), digits = 4), format(mean(day$MSEC), digits = 4), format(mean(night$MSEC), digits = 4)))
  rownames(depact.sum) <- c('mean depth day (m)', 'mean depth night (m)', 'mean activity day (BL/sec)', 'mean activity night (BL/sec)')
  colnames(depact.sum) <- 'mean.ConP7'
  depact.sum$sd.conP7 <-c(format(sd(day$PosZ), digits = 4), format(sd(night$PosZ), digits = 4), format(sd(day$MSEC), digits = 4), format(sd(night$MSEC), digits = 4))
  
  
  day <- subset(dayfile, SUN == 'D' & PEN == '8')
  night <- subset(dayfile, SUN == 'N' & PEN == '8')
  depact.sum$mean.UnconP8 <-c(format(mean(day$PosZ), digits = 4), format(mean(night$PosZ), digits = 4), format(mean(day$MSEC), digits = 4), format(mean(night$MSEC), digits = 4))
  depact.sum$sd.conP8 <-c(format(sd(day$PosZ), digits = 4), format(sd(night$PosZ), digits = 4), format(sd(day$MSEC), digits = 4), format(sd(night$MSEC), digits = 4))
  depact.sum
}


# 3b. depth and activity summary
depact.se <- function()
{
  day <- subset(dayfile, SUN == 'D' & PEN == '7')
  night <- subset(dayfile, SUN == 'N' & PEN == '7')
  depact.sum <- data.frame(c(format(mean(day$PosZ), digits = 4), format(mean(night$PosZ), digits = 4), format(mean(day$MSEC), digits = 4), format(mean(night$MSEC), digits = 4)))
  rownames(depact.sum) <- c('mean depth day (m)', 'mean depth night (m)', 'mean activity day (BL/sec)', 'mean activity night (BL/sec)')
  colnames(depact.sum) <- 'mean.ConP7'
  depact.sum$sd.conP7 <-c(format(sd(day$PosZ)/sqrt(length(day$PosZ)), digits = 4), format(sd(night$PosZ)/sqrt(length(night$PosZ)), digits = 4), format(sd(day$MSEC)/sqrt(length(day$MSEC)), digits = 4), format(sd(night$MSEC)/sqrt(length(night$MSEC)), digits = 4))
  
  
  day <- subset(dayfile, SUN == 'D' & PEN == '8')
  night <- subset(dayfile, SUN == 'N' & PEN == '8')
  depact.sum$mean.UnconP8 <-c(format(mean(day$PosZ), digits = 4), format(mean(night$PosZ), digits = 4), format(mean(day$MSEC), digits = 4), format(mean(night$MSEC), digits = 4))
  depact.sum$sd.conP8 <-c(format(sd(day$PosZ)/sqrt(length(day$PosZ)), digits = 4), format(sd(night$PosZ)/sqrt(length(night$PosZ)), digits = 4), format(sd(day$MSEC)/sqrt(length(day$MSEC)), digits = 4), format(sd(night$MSEC)/sqrt(length(night$MSEC)), digits = 4))
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
  depths.P7 <- data.frame(c('P7_dawn_mean', 'P7_dawn_stdev', 'P7_day_mean', 'P7_day_stdev', 'P7_dusk_mean', 'P7_dusk_stdev', 'P7_night_mean', 'P7_night_stdev'))
  colnames(depths.P7) <- 'ID'
  rownames(depths.P7) <- c('P7_dawn_mean', 'P7_dawn_stdev', 'P7_day_mean', 'P7_day_stdev', 'P7_dusk_mean', 'P7_dusk_stdev', 'P7_night_mean', 'P7_night_stdev')
  depths.P8 <- data.frame(c('P8_dawn_mean', 'P8_dawn_stdev', 'P8_day_mean', 'P8_day_stdev', 'P8_dusk_mean', 'P8_dusk_stdev', 'P8_night_mean', 'P8_night_stdev'))
  colnames(depths.P8) <- 'ID'
  rownames(depths.P8) <- c('P8_dawn_mean', 'P8_dawn_stdev', 'P8_day_mean', 'P8_day_stdev', 'P8_dusk_mean', 'P8_dusk_stdev', 'P8_night_mean', 'P8_night_stdev')
  
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
    
    
    depths.dawn <- subset(dayfile, SUN == 'W' & PEN == '7')
    depths.day <- subset(dayfile, SUN == 'D' & PEN == '7')
    depths.dusk <- subset(dayfile, SUN == 'K' & PEN == '7')
    depths.night <- subset(dayfile, SUN == 'N' & PEN == '7')
    dawn.sum <- cbind(Period = unique(depths.dawn$Period), do.call(rbind, tapply(depths.dawn$PosZ, depths.dawn$Period, sumfunc)))
    day.sum <- cbind(Period = unique(depths.day$Period), do.call(rbind, tapply(depths.day$PosZ, depths.day$Period, sumfunc)))
    dusk.sum <- cbind(Period = unique(depths.dusk$Period), do.call(rbind, tapply(depths.dusk$PosZ, depths.dusk$Period, sumfunc)))
    night.sum <- cbind(Period = unique(depths.night$Period), do.call(rbind, tapply(depths.night$PosZ, depths.night$Period, sumfunc)))
    dawn.sum[is.na(dawn.sum)] <- 0
    day.sum[is.na(day.sum)] <- 0
    dusk.sum[is.na(dusk.sum)] <- 0
    night.sum[is.na(night.sum)] <- 0
    depths.P7[,as.character(i)] <- c(mean(dawn.sum[,'mean']), mean(dawn.sum[,'std']), mean(day.sum[,'mean']), mean(day.sum[,'std']), mean(dusk.sum[,'mean']), mean(dusk.sum[,'std']), mean(night.sum[,'mean']), mean(night.sum[,'std']))
    
    depths.dawn <- subset(dayfile, SUN == 'W' & PEN == '8')
    depths.day <- subset(dayfile, SUN == 'D' & PEN == '8')
    depths.dusk <- subset(dayfile, SUN == 'K' & PEN == '8')
    depths.night <- subset(dayfile, SUN == 'N' & PEN == '8')
    dawn.sum <- cbind(Period = unique(depths.dawn$Period), do.call(rbind, tapply(depths.dawn$PosZ, depths.dawn$Period, sumfunc)))
    day.sum <- cbind(Period = unique(depths.day$Period), do.call(rbind, tapply(depths.day$PosZ, depths.day$Period, sumfunc)))
    dusk.sum <- cbind(Period = unique(depths.dusk$Period), do.call(rbind, tapply(depths.dusk$PosZ, depths.dusk$Period, sumfunc)))
    night.sum <- cbind(Period = unique(depths.night$Period), do.call(rbind, tapply(depths.night$PosZ, depths.night$Period, sumfunc)))
    dawn.sum[is.na(dawn.sum)] <- 0
    day.sum[is.na(day.sum)] <- 0
    dusk.sum[is.na(dusk.sum)] <- 0
    night.sum[is.na(night.sum)] <- 0
    depths.P8[,as.character(i)] <- c(mean(dawn.sum[,'mean']), mean(dawn.sum[,'std']), mean(day.sum[,'mean']), mean(day.sum[,'std']), mean(dusk.sum[,'mean']), mean(dusk.sum[,'std']), mean(night.sum[,'mean']), mean(night.sum[,'std']))
  }
  
  depths.sum <- rbind(depths.P7, depths.P8)  
  #depths.sum$ID <- NULL
  depths.sum    
  loadWorkbook('DepthsOutput.xlsx', create = TRUE)
  writeWorksheetToFile('DepthsOutput.xlsx', depths.sum, 'Sheet 1')
}


# 6. batch function to return matrix of mean and standard error depths for all fish combined over multiple days

batch.totdepth <- function(){
  
  #sumfunc <- function(x){ c(min = min(x), max = max(x), range = max(x)-min(x), mean = mean(x), median = median(x), std = sd(x)) }
  
  depth.P12 <- data.frame(c('P12_dawn_mean', 'P12_dawn_se', 'P12_day_mean', 'P12_day_se', 'P12_dusk_mean', 'P12_dusk_se', 'P12_night_mean', 'P12_night_se'))
  colnames(depth.P12) <- 'ID'
  rownames(depth.P12) <- c('P12_dawn_mean', 'P12_dawn_se', 'P12_day_mean', 'P12_day_se', 'P12_dusk_mean', 'P12_dusk_se', 'P12_night_mean', 'P12_night_se')
  depth.P14 <- data.frame(c('P14_dawn_mean', 'P14_dawn_se', 'P14_day_mean', 'P14_day_se', 'P14_dusk_mean', 'P14_dusk_se', 'P14_night_mean', 'P14_night_se'))
  colnames(depth.P14) <- 'ID'
  rownames(depth.P14) <- c('P14_dawn_mean', 'P14_dawn_se', 'P14_day_mean', 'P14_day_se', 'P14_dusk_mean', 'P14_dusk_se', 'P14_night_mean', 'P14_night_se')
  depth.P15 <- data.frame(c('P15_dawn_mean', 'P15_dawn_se', 'P15_day_mean', 'P15_day_se', 'P15_dusk_mean', 'P15_dusk_se', 'P15_night_mean', 'P15_night_se'))
  colnames(depth.P15) <- 'ID'
  rownames(depth.P15) <- c('P15_dawn_mean', 'P15_dawn_se', 'P15_day_mean', 'P15_day_se', 'P15_dusk_mean', 'P15_dusk_se', 'P15_night_mean', 'P15_night_se')
  
  for (i in 1:length(unique(dayfile$day)))
  {
    daysub <- subset(dayfile, day == unique(dayfile$day)[[i]])
    
    depth.dawn <- subset(daysub, SUN == 'W' & PEN == '12')
    depth.day <- subset(daysub, SUN == 'D' & PEN == '12')
    depth.dusk <- subset(daysub, SUN == 'K' & PEN == '12')
    depth.night <- subset(daysub, SUN == 'N' & PEN == '12')
    depth.P12[,as.character(i)] <- c(mean(depth.dawn$PosZ), sd(depth.dawn$PosZ)/sqrt(length(depth.dawn)), mean(depth.day$PosZ), sd(depth.day$PosZ)/sqrt(length(depth.day)), mean(depth.dusk$PosZ), sd(depth.dusk$PosZ)/sqrt(length(depth.dusk)), mean(depth.night$PosZ), sd(depth.night$PosZ)/sqrt(length(depth.night)))
    
    depth.dawn <- subset(daysub, SUN == 'W' & PEN == '14')
    depth.day <- subset(daysub, SUN == 'D' & PEN == '14')
    depth.dusk <- subset(daysub, SUN == 'K' & PEN == '14')
    depth.night <- subset(daysub, SUN == 'N' & PEN == '14')
    depth.P14[,as.character(i)] <- c(mean(depth.dawn$PosZ), sd(depth.dawn$PosZ)/sqrt(length(depth.dawn)), mean(depth.day$PosZ), sd(depth.day$PosZ)/sqrt(length(depth.day)), mean(depth.dusk$PosZ), sd(depth.dusk$PosZ)/sqrt(length(depth.dusk)), mean(depth.night$PosZ), sd(depth.night$PosZ)/sqrt(length(depth.night)))
    
    depth.dawn <- subset(daysub, SUN == 'W' & PEN == '15')
    depth.day <- subset(daysub, SUN == 'D' & PEN == '15')
    depth.dusk <- subset(daysub, SUN == 'K' & PEN == '15')
    depth.night <- subset(daysub, SUN == 'N' & PEN == '15')
    depth.P15[,as.character(i)] <- c(mean(depth.dawn$PosZ), sd(depth.dawn$PosZ)/sqrt(length(depth.dawn)), mean(depth.day$PosZ), sd(depth.day$PosZ)/sqrt(length(depth.day)), mean(depth.dusk$PosZ), sd(depth.dusk$PosZ)/sqrt(length(depth.dusk)), mean(depth.night$PosZ), sd(depth.night$PosZ)/sqrt(length(depth.night)))
    }
  
  depths.sum <- rbind(depth.P12, depth.P14, depth.P15)  
  depths.sum    

  write.xlsx(depths.sum, 'DepthTotOutput.xlsx')
}


# 7. batch function to return matrix of mean and standard deviation activity for individual fish over multiple days

batch.activity <- function(){
  
  sumfunc <- function(x){ c(min = min(x), max = max(x), range = max(x)-min(x), mean = mean(x), median = median(x), std = sd(x)) }
  
  activity.P12 <- data.frame(c('P12_dawn_mean', 'P12_dawn_stdev', 'P12_day_mean', 'P12_day_stdev', 'P12_dusk_mean', 'P12_dusk_stdev', 'P12_night_mean', 'P12_night_stdev'))
  colnames(activity.P12) <- 'ID'
  rownames(activity.P12) <- c('P12_dawn_mean', 'P12_dawn_stdev', 'P12_day_mean', 'P12_day_stdev', 'P12_dusk_mean', 'P12_dusk_stdev', 'P12_night_mean', 'P12_night_stdev')
  activity.P14 <- data.frame(c('P14_dawn_mean', 'P14_dawn_stdev', 'P14_day_mean', 'P14_day_stdev', 'P14_dusk_mean', 'P14_dusk_stdev', 'P14_night_mean', 'P14_night_stdev'))
  colnames(activity.P14) <- 'ID'
  rownames(activity.P14) <- c('P14_dawn_mean', 'P14_dawn_stdev', 'P14_day_mean', 'P14_day_stdev', 'P14_dusk_mean', 'P14_dusk_stdev', 'P14_night_mean', 'P14_night_stdev')
  activity.P15 <- data.frame(c('P15_dawn_mean', 'P15_dawn_stdev', 'P15_day_mean', 'P15_day_stdev', 'P15_dusk_mean', 'P15_dusk_stdev', 'P15_night_mean', 'P15_night_stdev'))
  colnames(activity.P15) <- 'ID'
  rownames(activity.P15) <- c('P15_dawn_mean', 'P15_dawn_stdev', 'P15_day_mean', 'P15_day_stdev', 'P15_dusk_mean', 'P15_dusk_stdev', 'P15_night_mean', 'P15_night_stdev')
  
  for (i in 1:length(unique(dayfile$day)))
  {
    daysub <- subset(dayfile, day == unique(dayfile$day)[[i]])
    
    
    activity.dawn <- subset(daysub, SUN == 'W' & PEN == '12')
    activity.day <- subset(daysub, SUN == 'D' & PEN == '12')
    activity.dusk <- subset(daysub, SUN == 'K' & PEN == '12')
    activity.night <- subset(daysub, SUN == 'N' & PEN == '12')
    dawn.sum <- cbind(Period = unique(activity.dawn$Period), do.call(rbind, tapply(activity.dawn$BLSEC, activity.dawn$Period, sumfunc)))
    day.sum <- cbind(Period = unique(activity.day$Period), do.call(rbind, tapply(activity.day$BLSEC, activity.day$Period, sumfunc)))
    dusk.sum <- cbind(Period = unique(activity.dusk$Period), do.call(rbind, tapply(activity.dusk$BLSEC, activity.dusk$Period, sumfunc)))
    night.sum <- cbind(Period = unique(activity.night$Period), do.call(rbind, tapply(activity.night$BLSEC, activity.night$Period, sumfunc)))
    dawn.sum[is.na(dawn.sum)] <- 0
    day.sum[is.na(day.sum)] <- 0
    dusk.sum[is.na(dusk.sum)] <- 0
    night.sum[is.na(night.sum)] <- 0
    activity.P12[,as.character(i)] <- c(mean(dawn.sum[,'mean']), mean(dawn.sum[,'std']), mean(day.sum[,'mean']), mean(day.sum[,'std']), mean(dusk.sum[,'mean']), mean(dusk.sum[,'std']), mean(night.sum[,'mean']), mean(night.sum[,'std']))
    
    activity.dawn <- subset(daysub, SUN == 'W' & PEN == '14')
    activity.day <- subset(daysub, SUN == 'D' & PEN == '14')
    activity.dusk <- subset(daysub, SUN == 'K' & PEN == '14')
    activity.night <- subset(daysub, SUN == 'N' & PEN == '14')
    dawn.sum <- cbind(Period = unique(activity.dawn$Period), do.call(rbind, tapply(activity.dawn$BLSEC, activity.dawn$Period, sumfunc)))
    day.sum <- cbind(Period = unique(activity.day$Period), do.call(rbind, tapply(activity.day$BLSEC, activity.day$Period, sumfunc)))
    dusk.sum <- cbind(Period = unique(activity.dusk$Period), do.call(rbind, tapply(activity.dusk$BLSEC, activity.dusk$Period, sumfunc)))
    night.sum <- cbind(Period = unique(activity.night$Period), do.call(rbind, tapply(activity.night$BLSEC, activity.night$Period, sumfunc)))
    dawn.sum[is.na(dawn.sum)] <- 0
    day.sum[is.na(day.sum)] <- 0
    dusk.sum[is.na(dusk.sum)] <- 0
    night.sum[is.na(night.sum)] <- 0
    activity.P14[,as.character(i)] <- c(mean(dawn.sum[,'mean']), mean(dawn.sum[,'std']), mean(day.sum[,'mean']), mean(day.sum[,'std']), mean(dusk.sum[,'mean']), mean(dusk.sum[,'std']), mean(night.sum[,'mean']), mean(night.sum[,'std']))
    
    activity.dawn <- subset(daysub, SUN == 'W' & PEN == '15')
    activity.day <- subset(daysub, SUN == 'D' & PEN == '15')
    activity.dusk <- subset(daysub, SUN == 'K' & PEN == '15')
    activity.night <- subset(daysub, SUN == 'N' & PEN == '15')
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
  activity.sum

  write.xlsx(activity.sum, 'ActivityOutput.xlsx')
}


# 8. batch function to return matrix of mean and standard error activity for all fish combined over multiple days

batch.totactivity <- function(){
  
  sumfunc <- function(x){ c(min = min(x), max = max(x), range = max(x)-min(x), mean = mean(x), median = median(x), std = sd(x)) }
  
  activity.P12 <- data.frame(c('P12_dawn_mean', 'P12_dawn_se', 'P12_day_mean', 'P12_day_se', 'P12_dusk_mean', 'P12_dusk_se', 'P12_night_mean', 'P12_night_se'))
  colnames(activity.P12) <- 'ID'
  rownames(activity.P12) <- c('P12_dawn_mean', 'P12_dawn_se', 'P12_day_mean', 'P12_day_se', 'P12_dusk_mean', 'P12_dusk_se', 'P12_night_mean', 'P12_night_se')
  activity.P14 <- data.frame(c('P14_dawn_mean', 'P14_dawn_se', 'P14_day_mean', 'P14_day_se', 'P14_dusk_mean', 'P14_dusk_se', 'P14_night_mean', 'P14_night_se'))
  colnames(activity.P14) <- 'ID'
  rownames(activity.P14) <- c('P14_dawn_mean', 'P14_dawn_se', 'P14_day_mean', 'P14_day_se', 'P14_dusk_mean', 'P14_dusk_se', 'P14_night_mean', 'P14_night_se')
  activity.P15 <- data.frame(c('P15_dawn_mean', 'P15_dawn_se', 'P15_day_mean', 'P15_day_se', 'P15_dusk_mean', 'P15_dusk_se', 'P15_night_mean', 'P15_night_se'))
  colnames(activity.P15) <- 'ID'
  rownames(activity.P15) <- c('P15_dawn_mean', 'P15_dawn_se', 'P15_day_mean', 'P15_day_se', 'P15_dusk_mean', 'P15_dusk_se', 'P15_night_mean', 'P15_night_se')
  
  for (i in 1:length(unique(dayfile$day)))
  {
    daysub <- subset(dayfile, day == unique(dayfile$day)[[i]])
    
    activity.dawn <- subset(daysub, SUN == 'W' & PEN == '12')
    activity.day <- subset(daysub, SUN == 'D' & PEN == '12')
    activity.dusk <- subset(daysub, SUN == 'K' & PEN == '12')
    activity.night <- subset(daysub, SUN == 'N' & PEN == '12')
    activity.P12[,as.character(i)] <- c(mean(activity.dawn$BLSEC, na.rm = T), sd(activity.dawn$BLSEC, na.rm = T)/sqrt(length(activity.dawn)), mean(activity.day$BLSEC, na.rm = T), sd(activity.day$BLSEC, na.rm = T)/sqrt(length(activity.day)), mean(activity.dusk$BLSEC, na.rm = T), sd(activity.dusk$BLSEC, na.rm = T)/sqrt(length(activity.dusk)), mean(activity.night$BLSEC, na.rm = T), sd(activity.night$BLSEC, na.rm = T)/sqrt(length(activity.night)))
    
    activity.dawn <- subset(daysub, SUN == 'W' & PEN == '14')
    activity.day <- subset(daysub, SUN == 'D' & PEN == '14')
    activity.dusk <- subset(daysub, SUN == 'K' & PEN == '14')
    activity.night <- subset(daysub, SUN == 'N' & PEN == '14')
    activity.P14[,as.character(i)] <- c(mean(activity.dawn$BLSEC, na.rm = T), sd(activity.dawn$BLSEC, na.rm = T)/sqrt(length(activity.dawn)), mean(activity.day$BLSEC, na.rm = T), sd(activity.day$BLSEC, na.rm = T)/sqrt(length(activity.day)), mean(activity.dusk$BLSEC, na.rm = T), sd(activity.dusk$BLSEC, na.rm = T)/sqrt(length(activity.dusk)), mean(activity.night$BLSEC, na.rm = T), sd(activity.night$BLSEC, na.rm = T)/sqrt(length(activity.night)))
    
    activity.dawn <- subset(daysub, SUN == 'W' & PEN == '15')
    activity.day <- subset(daysub, SUN == 'D' & PEN == '15')
    activity.dusk <- subset(daysub, SUN == 'K' & PEN == '15')
    activity.night <- subset(daysub, SUN == 'N' & PEN == '15')
    activity.P15[,as.character(i)] <- c(mean(activity.dawn$BLSEC, na.rm = T), sd(activity.dawn$BLSEC, na.rm = T)/sqrt(length(activity.dawn)), mean(activity.day$BLSEC, na.rm = T), sd(activity.day$BLSEC, na.rm = T)/sqrt(length(activity.day)), mean(activity.dusk$BLSEC, na.rm = T), sd(activity.dusk$BLSEC, na.rm = T)/sqrt(length(activity.dusk)), mean(activity.night$BLSEC, na.rm = T), sd(activity.night$BLSEC, na.rm = T)/sqrt(length(activity.night)))
  }
  
  activity.sum <- rbind(activity.P12, activity.P14, activity.P15)  
  activity.sum    

  write.xlsx(activity.sum, 'ActivityTotOutput.xlsx')
}


# 9a. proportion coverage

prop.coverage <- function(xmin7 = 15, xmax7 = 40, ymin7 = 15, ymax7 = 40, xmin8 = 42, xmax8 = 67, ymin8 = 15, ymax8 = 40, boxsize = 0.3) {
  fish.id <- subset(dayfile, PEN == '7')
  x.grid <- floor((fish.id$PosX - xmin7) / boxsize) + 1
  y.grid <- floor((fish.id$PosY - ymin7) / boxsize) + 1
  x.grid.max <- floor((xmax7 - xmin7) / boxsize) + 1
  y.grid.max <- floor((ymax7 - ymin7) / boxsize) + 1
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
  coverage.P7 <- matrix(c(length(which(grid.cov > 0)), length(grid.cov), length(which(grid.cov > 0))/length(grid.cov)), ncol = 3)
  colnames(coverage.P7) <- c('occupied', 'total', 'proportion')
  
  fish.id <- subset(dayfile, PEN == '8')
  x.grid <- floor((fish.id$PosX - xmin8) / boxsize) + 1
  y.grid <- floor((fish.id$PosY - ymin8) / boxsize) + 1
  x.grid.max <- floor((xmax8 - xmin8) / boxsize) + 1
  y.grid.max <- floor((ymax8 - ymin8) / boxsize) + 1
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
  coverage.P8 <- matrix(c(length(which(grid.cov > 0)), length(grid.cov), length(which(grid.cov > 0))/length(grid.cov)), ncol = 3)
  colnames(coverage.P8) <- c('occupied', 'total', 'proportion')
  
  coverage <- rbind(coverage.P7, coverage.P8) 
  rownames(coverage) <- c('P7', 'P8')
  coverage
}


# 9b. mean proportion coverage per hour

hmean.prop.coverage <- function(xmin7 = 15, xmax7 = 39, ymin7 = 15, ymax7 = 39, xmin8 = 41, xmax8 = 65, ymin8 = 15, ymax8 = 39, boxsize = 0.3) {
  
  fish.id <- subset(dayfile, PEN == '7')
  
  fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
  starttime <- fish.id[1,'EchoTime']-seconds(1)
  nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
  fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
  
  occupied <- numeric()
  total <- numeric()
  proportion <- numeric()
  
  for (i in 1:nhours){
    
    hoursub <- fish.id[fish.id$EchoTime > starttime & fish.id$EchoTime < starttime+hours(1),]   
    
    x.grid <- floor((hoursub$PosX - xmin7) / boxsize) + 1
    y.grid <- floor((hoursub$PosY - ymin7) / boxsize) + 1
    x.grid.max <- floor((xmax7 - xmin7) / boxsize) + 1
    y.grid.max <- floor((ymax7 - ymin7) / boxsize) + 1
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
  
  coverage.P7 <- matrix(c(mean(occupied), mean(total), mean(proportion)), ncol = 3)
  colnames(coverage.P7) <- c('occupied', 'total', 'proportion')
  
  
  fish.id <- subset(dayfile, PEN == '8')
  
  fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
  starttime <- fish.id[1,'EchoTime']-seconds(1)
  nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
  fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
  
  occupied <- numeric()
  total <- numeric()
  proportion <- numeric()
  
  for (i in 1:nhours){
    
    hoursub <- fish.id[fish.id$EchoTime >starttime & fish.id$EchoTime <starttime+hours(1),]   
    
    x.grid <- floor((hoursub$PosX - xmin8) / boxsize) + 1
    y.grid <- floor((hoursub$PosY - ymin8) / boxsize) + 1
    x.grid.max <- floor((xmax8 - xmin8) / boxsize) + 1
    y.grid.max <- floor((ymax8 - ymin8) / boxsize) + 1
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
  
  coverage.P8 <- matrix(c(mean(occupied), mean(total), mean(proportion)), ncol = 3)
  colnames(coverage.P8) <- c('occupied', 'total', 'proportion')
  
  coverage <- rbind(coverage.P7, coverage.P8) 
  rownames(coverage) <- c('P7', 'P8')
  coverage
}



# 10a. batch proportion coverage

batch.coverage <- function(xmin7 = 15, xmax7 = 39, ymin7 = 15, ymax7 = 39, xmin8 = 41, xmax8 = 65, ymin8 = 15, ymax8 = 39, boxsize = 0.3) {
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  coverage.P7 <- data.frame(c('P7'))
  colnames(coverage.P7) <- 'ID'
  rownames(coverage.P7) <- c('P7')
  coverage.P8 <- data.frame(c('P8'))
  colnames(coverage.P8) <- 'ID'
  rownames(coverage.P8) <- c('P8')
  
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
    
    if(length(unique(dayfile$Period)) == 1) {
      
      if(unique(dayfile$PEN) == '7'){
        
        fish.id <- subset(dayfile, PEN == '7')
        x.grid <- floor((fish.id$PosX - xmin7) / boxsize) + 1
        y.grid <- floor((fish.id$PosY - ymin7) / boxsize) + 1
        x.grid.max <- floor((xmax7 - xmin7) / boxsize) + 1
        y.grid.max <- floor((ymax7 - ymin7) / boxsize) + 1
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
        coverage.P7[,as.character(i)] <-  length(which(grid.cov > 0))/length(grid.cov)
        coverage.P8[,as.character(i)] <- 'NA'
        
      }else{
        
        fish.id <- subset(dayfile, PEN == '8')
        x.grid <- floor((fish.id$PosX - xmin8) / boxsize) + 1
        y.grid <- floor((fish.id$PosY - ymin8) / boxsize) + 1
        x.grid.max <- floor((xmax8 - xmin8) / boxsize) + 1
        y.grid.max <- floor((ymax8 - ymin8) / boxsize) + 1
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
        coverage.P8[,as.character(i)] <- length(which(grid.cov > 0))/length(grid.cov)
        coverage.P7[,as.character(i)] <- 'NA'
      }
    }
    
    else {
      
      fish.id <- subset(dayfile, PEN == '7')
      x.grid <- floor((fish.id$PosX - xmin7) / boxsize) + 1
      y.grid <- floor((fish.id$PosY - ymin7) / boxsize) + 1
      x.grid.max <- floor((xmax7 - xmin7) / boxsize) + 1
      y.grid.max <- floor((ymax7 - ymin7) / boxsize) + 1
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
      coverage.P7[,as.character(i)] <-  length(which(grid.cov > 0))/length(grid.cov)
      
      fish.id <- subset(dayfile, PEN == '8')
      x.grid <- floor((fish.id$PosX - xmin8) / boxsize) + 1
      y.grid <- floor((fish.id$PosY - ymin8) / boxsize) + 1
      x.grid.max <- floor((xmax8 - xmin8) / boxsize) + 1
      y.grid.max <- floor((ymax8 - ymin8) / boxsize) + 1
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
      coverage.P8[,as.character(i)] <- length(which(grid.cov > 0))/length(grid.cov)
      
    }  
    
  }  
  
  coverage <- rbind(coverage.P7, coverage.P8)
  print(coverage)
  #loadWorkbook('CoverageOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('CoverageOutput.xlsx', coverage, 'Sheet 1')
  
  write.xlsx(coverage, 'CoverageOutput.xlsx')
}



# 10b. batch mean proportion coverage per hour

hmean.batch.coverage <- function(xmin7 = 15, xmax7 = 39, ymin7 = 15, ymax7 = 39, xmin8 = 41, xmax8 = 65, ymin8 = 15, ymax8 = 39, boxsize = 0.3) {
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  coverage.P7 <- data.frame(c('P7_mean_coverage', 'P7_sd'))
  colnames(coverage.P7) <- 'ID'
  rownames(coverage.P7) <- c('P7_mean_coverage', 'P7_sd')
  coverage.P8 <- data.frame(c('P8_mean_coverage', 'P8_sd'))
  colnames(coverage.P8) <- 'ID'
  rownames(coverage.P8) <- c('P8_mean_coverage', 'P8_sd')
  
  anova.list <- data.frame('P value')
  colnames(anova.list) <- 'ID'
  rownames(anova.list) <- 'P value'
  
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
    
    if(length(unique(dayfile$Period)) == 1) {
      
      if(unique(dayfile$PEN) == '7'){
        
        fish.id <- subset(dayfile, PEN == '7')
        
        
        fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
        starttime <- fish.id[1,'EchoTime']-seconds(1)
        nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
        fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
        
        proportion.P7 <- numeric()
        
        for (j in 1:nhours){
          
          hoursub <- fish.id[fish.id$EchoTime > starttime & fish.id$EchoTime < starttime+hours(1),]  
          
          if (nrow(hoursub) > 1){
          
          
          x.grid <- floor((hoursub$PosX - xmin7) / boxsize) + 1
          y.grid <- floor((hoursub$PosY - ymin7) / boxsize) + 1
          x.grid.max <- floor((xmax7 - xmin7) / boxsize) + 1
          y.grid.max <- floor((ymax7 - ymin7) / boxsize) + 1
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
          
          proportion.P7 <- c(proportion.P7, length(which(grid.cov > 0))/length(grid.cov))
          
          } else {
            
          proportion.P7 <- c(proportion.P7, 0)  
            
          }
          
          starttime <- starttime+hours(1)
          
        }
        
        proportion.P7[proportion.P7 == 0] <- NA
        #coverage.P7[,as.character(i)] <-  mean(proportion, na.rm = T)
        coverage.P7[,as.character(i)] <-  c(mean(proportion.P7, na.rm = T), sd(proportion.P7, na.rm = T))
        coverage.P8[,as.character(i)] <- c('NA', 'NA')
        
      }else{
        
        fish.id <- subset(dayfile, PEN == '8')
        
        fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
        starttime <- fish.id[1,'EchoTime']-seconds(1)
        nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
        fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
        
        proportion.P8 <- numeric()
        
        for (j in 1:nhours){
          
          hoursub <- fish.id[fish.id$EchoTime >starttime & fish.id$EchoTime <starttime+hours(1),]   
          
          if (nrow(hoursub) > 1){
          
          x.grid <- floor((hoursub$PosX - xmin8) / boxsize) + 1
          y.grid <- floor((hoursub$PosY - ymin8) / boxsize) + 1
          x.grid.max <- floor((xmax8 - xmin8) / boxsize) + 1
          y.grid.max <- floor((ymax8 - ymin8) / boxsize) + 1
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
          
          proportion.P8 <- c(proportion.P8, length(which(grid.cov > 0))/length(grid.cov))
          
          } else {
            
            proportion.P8 <- c(proportion.P8, 0)  
            
          }
          
          
          starttime <- starttime+hours(1)
          
        }
        
        proportion.P8[proportion.P8 == 0] <- NA
        #coverage.P8[,as.character(i)] <-  mean(proportion, na.rm = T)
        coverage.P8[,as.character(i)] <- c(mean(proportion.P8, na.rm = T), sd(proportion.P8, na.rm = T))
        coverage.P7[,as.character(i)] <- c('NA', 'NA')
        
      }
    }
    
    else {
      
      fish.id <- subset(dayfile, PEN == '7')

      
      fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
      starttime <- fish.id[1,'EchoTime']-seconds(1)
      nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
      fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
      
      proportion.P7 <- numeric()
      
      for (j in 1:nhours){
        
        hoursub <- fish.id[fish.id$EchoTime > starttime & fish.id$EchoTime < starttime+hours(1),]   
        
        if (nrow(hoursub) > 1){
        
        x.grid <- floor((hoursub$PosX - xmin7) / boxsize) + 1
        y.grid <- floor((hoursub$PosY - ymin7) / boxsize) + 1
        x.grid.max <- floor((xmax7 - xmin7) / boxsize) + 1
        y.grid.max <- floor((ymax7 - ymin7) / boxsize) + 1
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
        
        proportion.P7 <- c(proportion.P7, length(which(grid.cov > 0))/length(grid.cov))
        
        } else {
          
          proportion.P7 <- c(proportion.P7, 0)  
          
        }
        
        starttime <- starttime+hours(1)
        
      }
      
      proportion.P7[proportion.P7 == 0] <- NA
      #coverage.P7[,as.character(i)] <-  mean(proportion, na.rm = T)
      coverage.P7[,as.character(i)] <-  c(mean(proportion.P7, na.rm = T), sd(proportion.P7, na.rm = T))
      
      
      proportion.P7 <- as.data.frame(proportion.P7)
      proportion.P7$pen <- 7
      names(proportion.P7) <- c('proportion', 'pen')
      
      
      fish.id <- subset(dayfile, PEN == '8')
      
      fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
      starttime <- fish.id[1,'EchoTime']-seconds(1)
      nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
      fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
      
      proportion.P8 <- numeric()

      
      for (j in 1:nhours){
        
        hoursub <- fish.id[fish.id$EchoTime >starttime & fish.id$EchoTime <starttime+hours(1),]   
        
        if (nrow(hoursub) > 1){
        
        x.grid <- floor((hoursub$PosX - xmin8) / boxsize) + 1
        y.grid <- floor((hoursub$PosY - ymin8) / boxsize) + 1
        x.grid.max <- floor((xmax8 - xmin8) / boxsize) + 1
        y.grid.max <- floor((ymax8 - ymin8) / boxsize) + 1
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
        
        proportion.P8 <- c(proportion.P8, length(which(grid.cov > 0))/length(grid.cov))
        
        } else {
          
          proportion.P8 <- c(proportion.P8, 0)  
          
        }
        
        starttime <- starttime+hours(1)
        
      }
      
      proportion.P8[proportion.P8 == 0] <- NA
      #coverage.P8[,as.character(i)] <- mean(proportion, na.rm = T)
      coverage.P8[,as.character(i)] <- c(mean(proportion.P8, na.rm = T), sd(proportion.P8, na.rm = T))
      
      proportion.P8 <- as.data.frame(proportion.P8)
      proportion.P8$pen <- 8
      names(proportion.P8) <- c('proportion', 'pen')
      
      prop.perhr <- rbind(proportion.P7, proportion.P8)
      cov.anova <- aov(proportion~pen, data = prop.perhr)
      anova.sum <- unlist(summary(cov.anova))
      anova.list[,as.character(i)] <- anova.sum[9]
      
      
    }  
    
  }  
  
  coverage.df <- rbind(coverage.P7, coverage.P8, anova.list)
  print(coverage.df)
  
  write.xlsx(coverage.df, 'CoverageOutput_hmean.xlsx')
}

# basic pen coverage function to run inside other coverage functions
coverage <- function(df, xmin, xmax, ymin, ymax, boxsize){
  
    x.grid <- floor((df$PosX - xmin) / boxsize) + 1
    y.grid <- floor((df$PosY - ymin) / boxsize) + 1
    x.grid.max <- floor((xmax - xmin) / boxsize) + 1
    y.grid.max <- floor((ymax - ymin) / boxsize) + 1
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
    
    return(grid.cov)
    
    #grid.cov <<- grid.cov
}


# 10c. daily mean proportion coverage per hour for loaded dayfile

hmean.day.coverage <- function(xmin12 = 40.5, xmax12 = 64.5, ymin12 = 40.5, ymax12 = 64.5, xmin14 = 15, xmax14 = 39, ymin14 = 40.5, ymax14 = 64.5, xmin15 = 15, xmax15 = 39, ymin15 = 15, ymax15 = 39, boxsize = 0.3) {
  
  #files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
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
  
    
    for (i in 1:length(unique(dayfile$day)))
    {
      #dayfile.loc <- files[[i]]
      #dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = dayfile.classes)
      daysub <- subset(dayfile, day == unique(dayfile$day)[[i]])
      
    
    if(length(unique(daysub$Period)) == 1) {
      
      if(unique(daysub$PEN) == '12'){
        
        fish.id <- subset(daysub, PEN == '12')
        
        
        fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
        starttime <- fish.id[1,'EchoTime']-seconds(1)
        nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
        fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
        
        proportion.P12 <- numeric()
        
        for (j in 1:nhours){
          
          hoursub <- fish.id[fish.id$EchoTime > starttime & fish.id$EchoTime < starttime+hours(1),]  
          
          if (nrow(hoursub) > 1){
            
            grid.cov <- coverage(df = hoursub, xmin = xmin12, xmax = xmax12, ymin = ymin12, ymax = ymax12, boxsize = boxsize) # run coverage function to calculate pen coverage
            
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
        
      } else {
        
        if(unique(daysub$PEN) == '14'){
        
        fish.id <- subset(daysub, PEN == '14')
        
        fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
        starttime <- fish.id[1,'EchoTime']-seconds(1)
        nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
        fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
        
        proportion.P14 <- numeric()
        
        for (j in 1:nhours){
          
          hoursub <- fish.id[fish.id$EchoTime >starttime & fish.id$EchoTime <starttime+hours(1),]   
          
          if (nrow(hoursub) > 1){
            
            grid.cov <- coverage(df = hoursub, xmin = xmin14, xmax = xmax14, ymin = ymin14, ymax = ymax14, boxsize = boxsize) # run coverage function to calculate pen coverage

            proportion.P14 <- c(proportion.P14, length(which(grid.cov > 0))/length(grid.cov))
            
          } else {
            
            proportion.P14 <- c(proportion.P14, 0)  
            
          }
          
          
          starttime <- starttime+hours(1)
          
        }
        
        proportion.P14[proportion.P8 == 0] <- NA
        #coverage.P8[,as.character(i)] <-  mean(proportion, na.rm = T)
        coverage.P14[,as.character(i)] <- c(mean(proportion.P14, na.rm = T), sd(proportion.P14, na.rm = T))
        coverage.P12[,as.character(i)] <- c('NA', 'NA')
        coverage.P15[,as.character(i)] <- c('NA', 'NA')
        
      } else {
        
        
        fish.id <- subset(daysub, PEN == '15')
        
        fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
        starttime <- fish.id[1,'EchoTime']-seconds(1)
        nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
        fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
        
        proportion.P15 <- numeric()
        
        for (j in 1:nhours){
          
          hoursub <- fish.id[fish.id$EchoTime >starttime & fish.id$EchoTime <starttime+hours(1),]   
          
          if (nrow(hoursub) > 1){
            
            grid.cov <- coverage(df = hoursub, xmin = xmin15, xmax = xmax15, ymin = ymin15, ymax = ymax15, boxsize = boxsize) # run coverage function to calculate pen coverage
            
            proportion.P15 <- c(proportion.P15, length(which(grid.cov > 0))/length(grid.cov))
            
          } else {
            
            proportion.P15 <- c(proportion.P15, 0)  
            
          }
          
          
          starttime <- starttime+hours(1)
          
        }
        
        proportion.P15[proportion.P8 == 0] <- NA
        #coverage.P8[,as.character(i)] <-  mean(proportion, na.rm = T)
        coverage.P15[,as.character(i)] <- c(mean(proportion.P15, na.rm = T), sd(proportion.P15, na.rm = T))
        coverage.P12[,as.character(i)] <- c('NA', 'NA')
        coverage.P14[,as.character(i)] <- c('NA', 'NA')
        
        
      }
        
      }
    }
    
    else {
      
      fish.id <- subset(daysub, PEN == '12')
      
      
      fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
      starttime <- fish.id[1,'EchoTime']-seconds(1)
      nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
      fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
      
      proportion.P12 <- numeric()
      
      for (j in 1:nhours){
        #browser()
        hoursub <- fish.id[fish.id$EchoTime > starttime & fish.id$EchoTime < starttime+hours(1),]   
        
        if (nrow(hoursub) > 1){
          
          grid.cov <- coverage(df = hoursub, xmin = xmin12, xmax = xmax12, ymin = ymin12, ymax = ymax12, boxsize = boxsize) # run coverage function to calculate pen coverage

          proportion.P12 <- c(proportion.P12, length(which(grid.cov > 0))/length(grid.cov))
          
        } else {
          
          proportion.P12 <- c(proportion.P12, 0)  
          
        }
        
        starttime <- starttime+hours(1)
        
      }
      
      proportion.P12[proportion.P12 == 0] <- NA
      #coverage.P7[,as.character(i)] <-  mean(proportion, na.rm = T)
      coverage.P12[,as.character(i)] <-  c(mean(proportion.P12, na.rm = T), sd(proportion.P12, na.rm = T))
      
      
      proportion.P12 <- as.data.frame(proportion.P12)
      proportion.P12$pen <- 12
      names(proportion.P12) <- c('proportion', 'pen')
      
      
      fish.id <- subset(daysub, PEN == '14')
      
      fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
      starttime <- fish.id[1,'EchoTime']-seconds(1)
      nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
      fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
      
      proportion.P14 <- numeric()
      
      
      for (j in 1:nhours){
        
        hoursub <- fish.id[fish.id$EchoTime > starttime & fish.id$EchoTime < starttime+hours(1),]   
        
        if (nrow(hoursub) > 1){
          
          grid.cov <- coverage(df = hoursub, xmin = xmin14, xmax = xmax14, ymin = ymin14, ymax = ymax14, boxsize = boxsize) # run coverage function to calculate pen coverage

          proportion.P14 <- c(proportion.P14, length(which(grid.cov > 0))/length(grid.cov))
          
        } else {
          
          proportion.P14 <- c(proportion.P14, 0)  
          
        }
        
        starttime <- starttime+hours(1)
        
      }
      
      proportion.P14[proportion.P14 == 0] <- NA
      #coverage.P8[,as.character(i)] <- mean(proportion, na.rm = T)
      coverage.P14[,as.character(i)] <- c(mean(proportion.P14, na.rm = T), sd(proportion.P14, na.rm = T))
      
      proportion.P14 <- as.data.frame(proportion.P14)
      proportion.P14$pen <- 14
      names(proportion.P14) <- c('proportion', 'pen')
      
      fish.id <- subset(daysub, PEN == '15')
      
      fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
      starttime <- fish.id[1,'EchoTime']-seconds(1)
      nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
      fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
      
      proportion.P15 <- numeric()
      
      
      for (j in 1:nhours){
        
        hoursub <- fish.id[fish.id$EchoTime > starttime & fish.id$EchoTime < starttime+hours(1),]   
        
        if (nrow(hoursub) > 1){
          
          grid.cov <- coverage(df = hoursub, xmin = xmin15, xmax = xmax15, ymin = ymin15, ymax = ymax15, boxsize = boxsize) # run coverage function to calculate pen coverage
          
          proportion.P15 <- c(proportion.P15, length(which(grid.cov > 0))/length(grid.cov))
          
        } else {
          
          proportion.P15 <- c(proportion.P15, 0)  
          
        }
        
        starttime <- starttime+hours(1)
        
      }
      
      proportion.P15[proportion.P15 == 0] <- NA
      #coverage.P8[,as.character(i)] <- mean(proportion, na.rm = T)
      coverage.P15[,as.character(i)] <- c(mean(proportion.P15, na.rm = T), sd(proportion.P15, na.rm = T))
      
      proportion.P15 <- as.data.frame(proportion.P15)
      proportion.P15$pen <- 15
      names(proportion.P15) <- c('proportion', 'pen')
      
      prop.perhr <- rbind(proportion.P12, proportion.P14, proportion.P15)
      cov.anova <- aov(proportion~pen, data = prop.perhr)
      anova.sum <- unlist(summary(cov.anova))
      anova.list[,as.character(i)] <- anova.sum[9]
      
    }  
    
  }  
  
  coverage.df <- rbind(coverage.P12, coverage.P14, coverage.P15, anova.list)
  print(coverage.df)
  
  write.xlsx(coverage.df, 'CoverageOutput_hmean.xlsx')
}



# 10d. hmean.perfish.coverage - daily hourly coverage per fish for all days loaded as one file using load.all()

hmean.perfish.coverage <- function(xmin12 = 40.5, xmax12 = 64.5, ymin12 = 40.5, ymax12 = 64.5, xmin14 = 15, xmax14 = 39, ymin14 = 40.5, ymax14 = 64.5, xmin15 = 15, xmax15 = 39, ymin15 = 15, ymax15 = 39, boxsize = 0.3) {
  
  #dayfile <- read.csv(files[1], header = TRUE, sep = ",", colClasses = dayfile.classes)
  fish12 <- sort(unique(dayfile$Period[dayfile$PEN == '12']))
  fish14 <- sort(unique(dayfile$Period[dayfile$PEN == '14']))
  fish15 <- sort(unique(dayfile$Period[dayfile$PEN == '15']))
  
  days <- c(paste0(sort(unique(as.Date(dayfile$EchoTime))), ' 00:00:00'), paste0(max(unique(as.Date(dayfile$EchoTime)))+days(1), ' 00:00:00'))
  
  coverage.P12 <- data.frame(fish = fish12, pen = rep(12, length(fish12)))
  coverage.P14 <- data.frame(fish = fish14, pen = rep(14, length(fish14)))
  coverage.P15 <- data.frame(fish = fish15, pen = rep(15, length(fish15)))
  
  pencut <- subset(dayfile, PEN == '12')
  
  for(d in 1:length(days)-1){
    
    daycut <- pencut[pencut$EchoTime > days[d] & pencut$EchoTime < days[d+1],]
    daymean <- numeric()
    daysd <- numeric()
    
    for (f in 1:length(fish12)){
      
      fishcut <- daycut[daycut$Period == fish12[f],]  
      
      fishcut <- fishcut[order(fishcut$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
      starttime <- fishcut[1,'EchoTime']-(hours(1) + seconds(1))
      nhours <- length(unique(hour(fishcut[,'EchoTime'])))-1
      #fishcut <- fishcut[order(fishcut$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
      
      occupied <- numeric()
      total <- numeric()
      proportion <- numeric()
      
      for (i in 1:nhours){
        
        hoursub <- fishcut[fishcut$EchoTime > starttime & fishcut$EchoTime < starttime+hours(1),]   
        
        if(nrow(hoursub) > 1 & mean(hoursub$PosX) > xmin12 & mean(hoursub$PosX) < xmax12 & mean(hoursub$PosY) > ymin12 & mean(hoursub$PosY) < ymax12){
          
          grid.cov <- coverage(df = hoursub, xmin = xmin12, xmax = xmax12, ymin = ymin12, ymax = ymax12, boxsize = boxsize) # run coverage function to calculate pen coverage

          occupied <- c(occupied, length(which(grid.cov > 0)))
          total <- c(total, length(grid.cov))
          proportion <- c(proportion, length(which(grid.cov > 0))/length(grid.cov))
          
        } else {proportion <- c(proportion, 0) }
        
        starttime <- starttime+hours(1)
        
        
      } # end of hour cut loop
      
      daymean <- c(daymean, mean(proportion))
      daysd <- c(daysd, sd(proportion))
      
    } # end of fishcut loop
    
    coverage.P12[,paste0(as.character(d), '_mean')] <- daymean
    coverage.P12[,paste0(as.character(d), '_sd')] <- daysd
    
  } # end of daycut loop
  
  
  pencut <- subset(dayfile, PEN == '14')
  
  for(d in 1:length(days)-1){
    
    daycut <- pencut[pencut$EchoTime > days[d] & pencut$EchoTime < days[d+1],]
    daymean <- numeric()
    daysd <- numeric()
    
    for (f in 1:length(fish14)){
      
      fishcut <- daycut[daycut$Period == fish14[f],]  
      
      fishcut <- fishcut[order(fishcut$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
      starttime <- fishcut[1,'EchoTime']-(hours(1) + seconds(1))
      nhours <- length(unique(hour(fishcut[,'EchoTime'])))-1
      #fishcut <- fishcut[order(fishcut$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
      
      occupied <- numeric()
      total <- numeric()
      proportion <- numeric()
      
      for (i in 1:nhours){
        
        hoursub <- fishcut[fishcut$EchoTime > starttime & fishcut$EchoTime < starttime+hours(1),]   
        
        if(nrow(hoursub) > 1 & mean(hoursub$PosX) > xmin14 & mean(hoursub$PosX) < xmax14 & mean(hoursub$PosY) > ymin14 & mean(hoursub$PosY) < ymax14){
          
          grid.cov <- coverage(df = hoursub, xmin = xmin14, xmax = xmax14, ymin = ymin14, ymax = ymax14, boxsize = boxsize) # run coverage function to calculate pen coverage

          occupied <- c(occupied, length(which(grid.cov > 0)))
          total <- c(total, length(grid.cov))
          proportion <- c(proportion, length(which(grid.cov > 0))/length(grid.cov))
          
        } else {proportion <- c(proportion, 0) }
        
        starttime <- starttime+hours(1)
        
        
      } # end of hour cut loop
      
      daymean <- c(daymean, mean(proportion))
      daysd <- c(daysd, sd(proportion))
      
    } # end of fishcut loop
    
    coverage.P14[,paste0(as.character(d), '_mean')] <- daymean
    coverage.P14[,paste0(as.character(d), '_sd')] <- daysd
    
  } # end of daycut loop
  
  pencut <- subset(dayfile, PEN == '15')
  
  for(d in 1:length(days)-1){
    
    daycut <- pencut[pencut$EchoTime > days[d] & pencut$EchoTime < days[d+1],]
    daymean <- numeric()
    daysd <- numeric()
    
    for (f in 1:length(fish15)){
      
      fishcut <- daycut[daycut$Period == fish15[f],]  
      
      fishcut <- fishcut[order(fishcut$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
      starttime <- fishcut[1,'EchoTime']-(hours(1) + seconds(1))
      nhours <- length(unique(hour(fishcut[,'EchoTime'])))-1
      #fishcut <- fishcut[order(fishcut$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
      
      occupied <- numeric()
      total <- numeric()
      proportion <- numeric()
      
      for (i in 1:nhours){
        
        hoursub <- fishcut[fishcut$EchoTime > starttime & fishcut$EchoTime < starttime+hours(1),]   
        
        if(nrow(hoursub) > 1 & mean(hoursub$PosX) > xmin15 & mean(hoursub$PosX) < xmax15 & mean(hoursub$PosY) > ymin15 & mean(hoursub$PosY) < ymax15){
          
          grid.cov <- coverage(df = hoursub, xmin = xmin15, xmax = xmax15, ymin = ymin15, ymax = ymax15, boxsize = boxsize) # run coverage function to calculate pen coverage

          occupied <- c(occupied, length(which(grid.cov > 0)))
          total <- c(total, length(grid.cov))
          proportion <- c(proportion, length(which(grid.cov > 0))/length(grid.cov))
          
        } else {proportion <- c(proportion, 0) }
        
        starttime <- starttime+hours(1)
        
        
      } # end of hour cut loop
      
      daymean <- c(daymean, mean(proportion))
      daysd <- c(daysd, sd(proportion))
      
    } # end of fishcut loop
    
    coverage.P15[,paste0(as.character(d), '_mean')] <- daymean
    coverage.P15[,paste0(as.character(d), '_sd')] <- daysd
    
  } # end of daycut loop

  
  coverage.df <- rbind(coverage.P12, coverage.P14, coverage.P15) 
  coverage.df[,'0_mean'] <- NULL
  coverage.df[,'0_sd'] <- NULL
  write.csv(coverage.df, 'CoverageOutput_hmeanperfish.csv')
  coverage.df <<- coverage.df
}


# 11a. draws a plot of fish depth for the fish id specified

fish.depth <- function(period)
{
  fish.id <- subset(dayfile, Period == period)
  plot(fish.id$EchoTime, fish.id$PosZ, xlab = 'Time', ylab = 'Depth (m)', ylim = c(35, 0), type = 'l', col = '#26b426')
  #segments(fish.id[1,3], 15, fish.id[nrow(fish.id), 3], 15, lty = 2)
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

fish.plot <- function(period, alims = 't')
{
  fishpal <- rainbow_hcl(20, c=100, l=63, start=-360, end=-32, alpha = 0.2)
  fish.id <- subset(dayfile, Period == period)
  par(mfrow=c(1,1))
  
  if(fish.id[1,'PEN'] == '12')
  {
    if(alims == 't'){plot(fish.id$PosX, fish.id$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(35, 70), ylim = c(35, 70), type = 'l', col = fishpal[20])}
    if(alims == 'w'){plot(fish.id$PosX, fish.id$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, type = 'l', col = fishpal[20])}
    rect(locations.lookup['12EW', 'xmin'], locations.lookup['12EW', 'ymin'], locations.lookup['12EW', 'xmax'], locations.lookup['12EW', 'ymax'], lty = 2) # 12EW edge
    rect(locations.lookup['12ES', 'xmin'], locations.lookup['12ES', 'ymin'], locations.lookup['12ES', 'xmax'], locations.lookup['12ES', 'ymax'], lty = 2) # 12ES edge
    rect(locations.lookup['12EE', 'xmin'], locations.lookup['12EE', 'ymin'], locations.lookup['12EE', 'xmax'], locations.lookup['12EE', 'ymax'], lty = 2) # 12EE edge
    rect(locations.lookup['12EN', 'xmin'], locations.lookup['12EN', 'ymin'], locations.lookup['12EN', 'xmax'], locations.lookup['12EN', 'ymax'], lty = 2) # 12EN edge
    rect(locations.lookup['12HET', 'xmin'], locations.lookup['12HET', 'ymin'], locations.lookup['12HET', 'xmax'], locations.lookup['12HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HET
    rect(locations.lookup['12HWT', 'xmin'], locations.lookup['12HWT', 'ymin'], locations.lookup['12HWT', 'xmax'], locations.lookup['12HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HWT
    rect(locations.lookup['12FS', 'xmin'], locations.lookup['12FS', 'ymin'], locations.lookup['12FS', 'xmax'], locations.lookup['12FS', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 12FS
    rect(locations.lookup['12EW', 'xmin'], locations.lookup['12ES', 'ymin'], locations.lookup['12EE', 'xmax'], locations.lookup['12EN', 'ymax'], lwd = 2) # cage limits

  }
    
    if(fish.id[1,'PEN'] == '14'){
      
      if(alims == 't'){plot(fish.id$PosX, fish.id$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(10, 45), ylim = c(35, 70), type = 'l', col = fishpal[20])}
      if(alims == 'w'){plot(fish.id$PosX, fish.id$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, type = 'l', col = fishpal[20])}
      rect(locations.lookup['14EW', 'xmin'], locations.lookup['14EW', 'ymin'], locations.lookup['14EW', 'xmax'], locations.lookup['14EW', 'ymax'], lty = 2) # 14EW edge
      rect(locations.lookup['14ES', 'xmin'], locations.lookup['14ES', 'ymin'], locations.lookup['14ES', 'xmax'], locations.lookup['14ES', 'ymax'], lty = 2) # 14ES edge
      rect(locations.lookup['14EE', 'xmin'], locations.lookup['14EE', 'ymin'], locations.lookup['14EE', 'xmax'], locations.lookup['14EE', 'ymax'], lty = 2) # 14EE edge
      rect(locations.lookup['14EN', 'xmin'], locations.lookup['14EN', 'ymin'], locations.lookup['14EN', 'xmax'], locations.lookup['14EN', 'ymax'], lty = 2) # 14EN edge
      rect(locations.lookup['14HET', 'xmin'], locations.lookup['14HET', 'ymin'], locations.lookup['14HET', 'xmax'], locations.lookup['14HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HET
      rect(locations.lookup['14HWT', 'xmin'], locations.lookup['14HWT', 'ymin'], locations.lookup['14HWT', 'xmax'], locations.lookup['14HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HWT
      rect(locations.lookup['14FS', 'xmin'], locations.lookup['14FS', 'ymin'], locations.lookup['14FS', 'xmax'], locations.lookup['14FS', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 14FS
      rect(locations.lookup['14EW', 'xmin'], locations.lookup['14ES', 'ymin'], locations.lookup['14EE', 'xmax'], locations.lookup['14EN', 'ymax'], lwd = 2) # cage limits
      
    }
  
  if(fish.id[1,'PEN'] == '15'){
    
      if(alims == 't'){plot(fish.id$PosX, fish.id$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(10, 45), ylim = c(10, 45), type = 'l', col = fishpal[20])}
      if(alims == 'w'){plot(fish.id$PosX, fish.id$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(10, 45), ylim = c(10, 45), type = 'l', col = fishpal[20])}
      rect(locations.lookup['15EW', 'xmin'], locations.lookup['15EW', 'ymin'], locations.lookup['15EW', 'xmax'], locations.lookup['15EW', 'ymax'], lty = 2) # 15EW edge
      rect(locations.lookup['15ES', 'xmin'], locations.lookup['15ES', 'ymin'], locations.lookup['15ES', 'xmax'], locations.lookup['15ES', 'ymax'], lty = 2) # 15ES edge
      rect(locations.lookup['15EE', 'xmin'], locations.lookup['15EE', 'ymin'], locations.lookup['15EE', 'xmax'], locations.lookup['15EE', 'ymax'], lty = 2) # 15EE edge
      rect(locations.lookup['15EN', 'xmin'], locations.lookup['15EN', 'ymin'], locations.lookup['15EN', 'xmax'], locations.lookup['15EN', 'ymax'], lty = 2) # 15EN edge
      rect(locations.lookup['15HET', 'xmin'], locations.lookup['15HET', 'ymin'], locations.lookup['15HET', 'xmax'], locations.lookup['15HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HET
      rect(locations.lookup['15HWT', 'xmin'], locations.lookup['15HWT', 'ymin'], locations.lookup['15HWT', 'xmax'], locations.lookup['15HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HWT
      rect(locations.lookup['15FS', 'xmin'], locations.lookup['15FS', 'ymin'], locations.lookup['15FS', 'xmax'], locations.lookup['15FS', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 15FS
      rect(locations.lookup['15EW', 'xmin'], locations.lookup['15ES', 'ymin'], locations.lookup['15EE', 'xmax'], locations.lookup['15EN', 'ymax'], lwd = 2) # cage limits
    
  }

}


# 14. Draws a plot of fish locations for 3 fish

fish.3plot <- function(period1, period2, period3)
{
  fish.id <- subset(dayfile, Period == period1)
  if(fish.id[1,3] == '7')
  {
    
    plot(fish.id$PosX, fish.id$PosY, xlab = 'X', ylab = 'Y', pch = 20, xlim = c(10, 45), ylim = c(10, 45), type = 'l', col = '#26b426')
    fish.id <- subset(dayfile, Period == period2)
    lines(fish.id$PosX, fish.id$PosY, pch = 20, col = '#d80000')
    fish.id <- subset(dayfile, Period == period3)
    lines(fish.id$PosX, fish.id$PosY, pch = 20, col = '#038ef0')
    rect(locations.lookup['7EW', 'xmin'], locations.lookup['7EW', 'ymin'], locations.lookup['7EW', 'xmax'], locations.lookup['7EW', 'ymax'], lty = 2) # 7EW edge
    rect(locations.lookup['7ES', 'xmin'], locations.lookup['7ES', 'ymin'], locations.lookup['7ES', 'xmax'], locations.lookup['7ES', 'ymax'], lty = 2) # 7ES edge
    rect(locations.lookup['7EE', 'xmin'], locations.lookup['7EE', 'ymin'], locations.lookup['7EE', 'xmax'], locations.lookup['7EE', 'ymax'], lty = 2) # 7EE edge
    rect(locations.lookup['7EN', 'xmin'], locations.lookup['7EN', 'ymin'], locations.lookup['7EN', 'xmax'], locations.lookup['7EN', 'ymax'], lty = 2) # 7EN edge
    rect(locations.lookup['7WHSE', 'xmin'], locations.lookup['7WHSE', 'ymin'], locations.lookup['7WHSE', 'xmax'], locations.lookup['7WHSE', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHSE
    rect(locations.lookup['7WHNW', 'xmin'], locations.lookup['7WHNW', 'ymin'], locations.lookup['7WHNW', 'xmax'], locations.lookup['7WHNW', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHNW
    rect(locations.lookup['7EW', 'xmin'], locations.lookup['7ES', 'ymin'], locations.lookup['7EE', 'xmax'], locations.lookup['7EN', 'ymax'], lwd = 2) # cage limits
    legend(1, 10, as.character(c(period1, period2, period3)), col = c('#26b426', '#d80000', '#038ef0'), pch = 20, bty = 'n', pt.cex = 1.5, horiz = TRUE)
    
  }else{
    
    plot(fish.id$PosX, fish.id$PosY, xlab = 'X', ylab = 'Y', pch = 20, xlim = c(37, 72), ylim = c(10, 45), type = 'l', col = '#26b426')
    fish.id <- subset(dayfile, Period == period2)
    lines(fish.id$PosX, fish.id$PosY, pch = 20, col = '#d80000')
    fish.id <- subset(dayfile, Period == period3)
    lines(fish.id$PosX, fish.id$PosY, pch = 20, col = '#038ef0')
    rect(locations.lookup['8EW', 'xmin'], locations.lookup['8EW', 'ymin'], locations.lookup['8EW', 'xmax'], locations.lookup['8EW', 'ymax'], lty = 2) # 7EW edge
    rect(locations.lookup['8ES', 'xmin'], locations.lookup['8ES', 'ymin'], locations.lookup['8ES', 'xmax'], locations.lookup['8ES', 'ymax'], lty = 2) # 7ES edge
    rect(locations.lookup['8EE', 'xmin'], locations.lookup['8EE', 'ymin'], locations.lookup['8EE', 'xmax'], locations.lookup['8EE', 'ymax'], lty = 2) # 7EE edge
    rect(locations.lookup['8EN', 'xmin'], locations.lookup['8EN', 'ymin'], locations.lookup['8EN', 'xmax'], locations.lookup['8EN', 'ymax'], lty = 2) # 7EN edge
    rect(locations.lookup['8WHSW', 'xmin'], locations.lookup['8WHSW', 'ymin'], locations.lookup['8WHSW', 'xmax'], locations.lookup['8WHSW', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHSE
    rect(locations.lookup['8WHNE', 'xmin'], locations.lookup['8WHNE', 'ymin'], locations.lookup['8WHNE', 'xmax'], locations.lookup['8WHNE', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHNW
    rect(locations.lookup['8EW', 'xmin'], locations.lookup['8ES', 'ymin'], locations.lookup['8EE', 'xmax'], locations.lookup['8EN', 'ymax'], lwd = 2) # cage limits
    legend(25, 10, as.character(c(period1, period2, period3)), col = c('#26b426', '#d80000', '#038ef0'), pch = 20, bty = 'n', pt.cex = 1.5, horiz = TRUE)
    
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
  pingmax <- as.integer((as.double(max(fish.id$EchoTime))-as.double(min(fish.id$EchoTime)))/3000)
  #pingmax <- 100
  
  if(fish.id[1, 'PEN'] == 12){  
    
    ggplot(fish.id, aes(fish.id$PosX, fish.id$PosY)) +
      geom_hex(bins = 55, alpha = 0.6) + scale_fill_gradientn(colours=plot.col, space = 'Lab', limits = c(0, pingmax), na.value = plot.col[length(plot.col)], name = 'No. pings') +
      annotate('segment', x = locations.lookup['12CSW', 'xmin'], xend = locations.lookup['12CSE', 'xmax'], y = locations.lookup['12CSW', 'ymin'], yend = locations.lookup['12CSE', 'ymin'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['12CSW', 'xmin'], xend = locations.lookup['12CNW', 'xmin'], y = locations.lookup['12CSW', 'ymin'], yend = locations.lookup['12CNW', 'ymax'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['12CNW', 'xmin'], xend = locations.lookup['12CNE', 'xmax'], y = locations.lookup['12CNW', 'ymax'], yend = locations.lookup['12CNE', 'ymax'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['12CNE', 'xmax'], xend = locations.lookup['12CSE', 'xmax'], y = locations.lookup['12CNE', 'ymax'], yend = locations.lookup['12CSE', 'ymin'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['12CSW', 'xmin'], xend = locations.lookup['12CSE', 'xmax'], y = locations.lookup['12CSW', 'ymax'], yend = locations.lookup['12CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('segment', x = locations.lookup['12CSW', 'xmax'], xend = locations.lookup['12CNW', 'xmax'], y = locations.lookup['12CSW', 'ymin'], yend = locations.lookup['12CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('segment', x = locations.lookup['12CNW', 'xmin'], xend = locations.lookup['12CNE', 'xmax'], y = locations.lookup['12CNW', 'ymin'], yend = locations.lookup['12CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('segment', x = locations.lookup['12CNE', 'xmin'], xend = locations.lookup['12CSE', 'xmin'], y = locations.lookup['12CNE', 'ymax'], yend = locations.lookup['12CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
      scale_x_continuous('x (m)', limits = c(35, 70)) + 
      scale_y_continuous('y (m)', limits = c(35,70))
    
  } else {
  
  
  if(fish.id[1, 'PEN'] == 14) {
    
    ggplot(fish.id, aes(fish.id$PosX, fish.id$PosY)) +
      geom_hex(bins = 55, alpha = 0.6) + scale_fill_gradientn(colours=plot.col, space = 'Lab', limits = c(0, pingmax), na.value = plot.col[length(plot.col)], name = 'No. pings') +
      annotate('segment', x = locations.lookup['14CSW', 'xmin'], xend = locations.lookup['14CSE', 'xmax'], y = locations.lookup['14CSW', 'ymin'], yend = locations.lookup['14CSE', 'ymin'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['14CSW', 'xmin'], xend = locations.lookup['14CNW', 'xmin'], y = locations.lookup['14CSW', 'ymin'], yend = locations.lookup['14CNW', 'ymax'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['14CNW', 'xmin'], xend = locations.lookup['14CNE', 'xmax'], y = locations.lookup['14CNW', 'ymax'], yend = locations.lookup['14CNE', 'ymax'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['14CNE', 'xmax'], xend = locations.lookup['14CSE', 'xmax'], y = locations.lookup['14CNE', 'ymax'], yend = locations.lookup['14CSE', 'ymin'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['14CSW', 'xmin'], xend = locations.lookup['14CSE', 'xmax'], y = locations.lookup['14CSW', 'ymax'], yend = locations.lookup['14CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('segment', x = locations.lookup['14CSW', 'xmax'], xend = locations.lookup['14CNW', 'xmax'], y = locations.lookup['14CSW', 'ymin'], yend = locations.lookup['14CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('segment', x = locations.lookup['14CNW', 'xmin'], xend = locations.lookup['14CNE', 'xmax'], y = locations.lookup['14CNW', 'ymin'], yend = locations.lookup['14CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('segment', x = locations.lookup['14CNE', 'xmin'], xend = locations.lookup['14CSE', 'xmin'], y = locations.lookup['14CNE', 'ymax'], yend = locations.lookup['14CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      #annotate('curve', x = locations.lookup['8WHSW', 'xmin']+1, xend = locations.lookup['8WHSW', 'xmax']-1, y = locations.lookup['8WHSW', 'ymin']+1, yend = locations.lookup['8WHSW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
      #annotate('curve', x = locations.lookup['8WHSW', 'xmin']+1, xend = locations.lookup['8WHSW', 'xmax']-1, y = locations.lookup['8WHSW', 'ymin']+1, yend = locations.lookup['8WHSW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
      #annotate('curve', x = locations.lookup['8WHNE', 'xmin']+1, xend = locations.lookup['8WHNE', 'xmax']-1, y = locations.lookup['8WHNE', 'ymin']+1, yend = locations.lookup['8WHNE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
      #annotate('curve', x = locations.lookup['8WHNE', 'xmin']+1, xend = locations.lookup['8WHNE', 'xmax']-1, y = locations.lookup['8WHNE', 'ymin']+1, yend = locations.lookup['8WHNE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
      theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
      scale_x_continuous('x (m)', limits = c(10,45)) + 
      scale_y_continuous('y (m)', limits = c(35,70))  
    
  } else {
  
  if(fish.id[1, 'PEN'] == 15) {
    
    ggplot(fish.id, aes(fish.id$PosX, fish.id$PosY)) +
      geom_hex(bins = 55, alpha = 0.6) + scale_fill_gradientn(colours=plot.col, space = 'Lab', limits = c(0, pingmax), na.value = plot.col[length(plot.col)], name = 'No. pings') +
      annotate('segment', x = locations.lookup['15CSW', 'xmin'], xend = locations.lookup['15CSE', 'xmax'], y = locations.lookup['15CSW', 'ymin'], yend = locations.lookup['15CSE', 'ymin'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['15CSW', 'xmin'], xend = locations.lookup['15CNW', 'xmin'], y = locations.lookup['15CSW', 'ymin'], yend = locations.lookup['15CNW', 'ymax'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['15CNW', 'xmin'], xend = locations.lookup['15CNE', 'xmax'], y = locations.lookup['15CNW', 'ymax'], yend = locations.lookup['15CNE', 'ymax'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['15CNE', 'xmax'], xend = locations.lookup['15CSE', 'xmax'], y = locations.lookup['15CNE', 'ymax'], yend = locations.lookup['15CSE', 'ymin'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['15CSW', 'xmin'], xend = locations.lookup['15CSE', 'xmax'], y = locations.lookup['15CSW', 'ymax'], yend = locations.lookup['15CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('segment', x = locations.lookup['15CSW', 'xmax'], xend = locations.lookup['15CNW', 'xmax'], y = locations.lookup['15CSW', 'ymin'], yend = locations.lookup['15CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('segment', x = locations.lookup['15CNW', 'xmin'], xend = locations.lookup['15CNE', 'xmax'], y = locations.lookup['15CNW', 'ymin'], yend = locations.lookup['15CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('segment', x = locations.lookup['15CNE', 'xmin'], xend = locations.lookup['15CSE', 'xmin'], y = locations.lookup['15CNE', 'ymax'], yend = locations.lookup['15CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
      scale_x_continuous('x (m)', limits = c(10,45)) + 
      scale_y_continuous('y (m)', limits = c(10,45))  
    
    
  }
  }
  }
}


#16b. draws a plot of fish location density for all fish in the specified pen (7 or 8)
hexplot.all <- function(pen, axes = 'xy', hideselect)
{
  
  pen.col <- 'black'
  pen.size <- 1.4
  #plot.col <- rev(heat.colors(2, alpha = 1))
  plot.col <- matlab.like(50)  
  #pingmax <- as.integer((as.double(max(fish.id$EchoTime))-as.double(min(fish.id$EchoTime)))/diviser)
  #pingmax <- 1000
  #pingmax <- diviser  
  #colvals <- rescale(lseq(1, pingmax, length.out = 50), c(0, 1))
  #colvals <- seq(0.001, 1, length.out = 50)

  if(pen == 12){  
    
    fish.id <- subset(dayfile, PEN == '12')  
    #pingmax <- as.integer((as.double(max(fish.id$EchoTime))-as.double(min(fish.id$EchoTime)))/diviser)

    if(axes == 'xz') {
      hexplot <- ggplot(fish.id, aes(fish.id$PosX, fish.id$PosZ))
      hexplot <- hexplot + geom_hex(bins = 55, alpha = 0.6) + scale_fill_gradientn(colours = plot.col, space = 'Lab', limits = c(10, NA), na.value = NA, trans = 'log', breaks = c(10, 100, 1000), name = 'No. pings')
      hexplot <- hexplot +
        annotate('segment', x = locations.lookup['12EW', 'xmin'], xend = locations.lookup['12EE', 'xmax'], y = 0, yend = 0, colour = pen.col, size = pen.size) + # 
        annotate('segment', x = locations.lookup['12EW', 'xmin'], xend = locations.lookup['12EW', 'xmin'], y = 0, yend = 15, colour = pen.col, size = pen.size) +  # 
        annotate('segment', x = locations.lookup['12EE', 'xmax'], xend = locations.lookup['12EE', 'xmax'], y = 0, yend = 15, colour = pen.col, size = pen.size) + # 
        annotate('segment', x = locations.lookup['12EW', 'xmin'], xend = ((locations.lookup['12EE', 'xmax'] - locations.lookup['12EW', 'xmin'])/2 + locations.lookup['12EW', 'xmin']), y = 15, yend = 20, colour = pen.col, size = pen.size) + # 
        annotate('segment', x = locations.lookup['12EE', 'xmax'], xend = ((locations.lookup['12EE', 'xmax'] - locations.lookup['12EW', 'xmin'])/2 + locations.lookup['12EW', 'xmin']), y = 15, yend = 20, colour = pen.col, size = pen.size) + # 
        annotate('segment', x = locations.lookup['12EW', 'xmax'], xend = locations.lookup['12EW', 'xmax'], y = 0, yend = 15, colour = pen.col, linetype = 'dotted', size = pen.size) + # 
        annotate('segment', x = locations.lookup['12EE', 'xmin'], xend = locations.lookup['12EE', 'xmin'], y = 0, yend = 15, colour = pen.col, linetype = 'dotted', size = pen.size) + # 
        annotate('segment', x = locations.lookup['12EW', 'xmin'], xend = locations.lookup['12EE', 'xmax'], y = 15, yend = 15, colour = pen.col, linetype = 'dotted', size = pen.size) + # 
        annotate('rect', xmin = locations.lookup['FS12', 'xmin'], xmax = locations.lookup['FS12', 'xmax'], ymin = locations.lookup['FS12', 'zmin'], ymax = locations.lookup['FS12', 'zmax'], colour = pen.col, size = pen.size, alpha = 0.3) + # feed station
        theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
        scale_x_continuous('x (m)', limits = c(35, 70)) + scale_y_reverse('z (m)', limits = c(35,0))
      
        if(hideselect == 'E'){ # east hide
            hexplot <- hexplot +
            annotate('polygon', x = c(locations.lookup['12HET', 'xmin'], locations.lookup['12HET', 'xmax'], locations.lookup['12HEB', 'xmax'], locations.lookup['12HEB', 'xmin']), y = c(locations.lookup['12HET', 'zmin'], locations.lookup['12HET', 'zmin'], locations.lookup['12HET', 'zmax'], locations.lookup['12HET', 'zmax']), colour = pen.col, size = pen.size, alpha = 0.3)
        }
      
        if(hideselect == 'W'){ # west hide
          hexplot <- hexplot +
          annotate('polygon', x = c(locations.lookup['12HWT', 'xmin'], locations.lookup['12HWT', 'xmax'], locations.lookup['12HWB', 'xmax'], locations.lookup['12HWB', 'xmin']), y = c(locations.lookup['12HWT', 'zmin'], locations.lookup['12HWT', 'zmin'], locations.lookup['12HWT', 'zmax'], locations.lookup['12HWT', 'zmax']), colour = pen.col, size = pen.size, alpha = 0.3) # west hide
        }
            
    }
      
      if(axes == 'yz') {
        
        hexplot <- ggplot(fish.id, aes(fish.id$PosY, fish.id$PosZ))
        hexplot <- hexplot + geom_hex(bins = 55, alpha = 0.6) + scale_fill_gradientn(colours = plot.col, space = 'Lab', limits = c(10, NA), na.value = NA, trans = 'log', breaks = c(10, 100, 1000), name = 'No. pings')
        hexplot <- hexplot +
          annotate('segment', x = locations.lookup['12ES', 'ymin'], xend = locations.lookup['12EN', 'ymax'], y = 0, yend = 0, colour = pen.col, size = pen.size) + # pEE boundary
          annotate('segment', x = locations.lookup['12ES', 'ymin'], xend = locations.lookup['12ES', 'ymin'], y = 0, yend = 15, colour = pen.col, size = pen.size) +  # pEE boundary
          annotate('segment', x = locations.lookup['12EN', 'ymax'], xend = locations.lookup['12EN', 'ymax'], y = 0, yend = 15, colour = pen.col, size = pen.size) + # pEE boundary
          annotate('segment', x = locations.lookup['12ES', 'ymin'], xend = ((locations.lookup['12EN', 'ymax'] - locations.lookup['12ES', 'ymin'])/2 + locations.lookup['12ES', 'ymin']), y = 15, yend = 20, colour = pen.col, size = pen.size) + # pEE boundary
          annotate('segment', x = locations.lookup['12EN', 'ymax'], xend = ((locations.lookup['12EN', 'ymax'] - locations.lookup['12ES', 'ymin'])/2 + locations.lookup['12ES', 'ymin']), y = 15, yend = 20, colour = pen.col, size = pen.size) + # pEE boundary
          annotate('segment', x = locations.lookup['12ES', 'ymax'], xend = locations.lookup['12ES', 'ymax'], y = 0, yend = 15, colour = pen.col, linetype = 'dotted', size = pen.size) + # pEE location boundary
          annotate('segment', x = locations.lookup['12EN', 'ymin'], xend = locations.lookup['12EN', 'ymin'], y = 0, yend = 15, colour = pen.col, linetype = 'dotted', size = pen.size) + # pEE location boundary
          annotate('segment', x = locations.lookup['12ES', 'ymin'], xend = locations.lookup['12EN', 'ymax'], y = 15, yend = 15, colour = pen.col, linetype = 'dotted', size = pen.size) + # pEE location boundary
          annotate('rect', xmin = locations.lookup['FS12', 'ymin'], xmax = locations.lookup['FS12', 'ymax'], ymin = locations.lookup['FS12', 'zmin'], ymax = locations.lookup['FS12', 'zmax'], colour = pen.col, size = pen.size, alpha = 0.3) + # hide boundary
          theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
          scale_x_continuous('y (m)', limits = c(35, 70)) + scale_y_reverse('z (m)', limits = c(35,0))
        
        if(hideselect == 'E'){ # east hide
          hexplot <- hexplot +
          annotate('polygon', x = c(locations.lookup['12HET', 'ymin'], locations.lookup['12HET', 'ymax'], locations.lookup['12HEB', 'ymax'], locations.lookup['12HEB', 'ymin']), y = c(locations.lookup['12HET', 'zmin'], locations.lookup['12HET', 'zmin'], locations.lookup['12HET', 'zmax'], locations.lookup['12HET', 'zmax']), colour = pen.col, size = pen.size, alpha = 0.3)
        }
        
        if(hideselect == 'W'){ # west hide
          hexplot <- hexplot +
          annotate('polygon', x = c(locations.lookup['12HWT', 'ymin'], locations.lookup['12HWT', 'ymax'], locations.lookup['12HWB', 'ymax'], locations.lookup['12HWB', 'ymin']), y = c(locations.lookup['12HWT', 'zmin'], locations.lookup['12HWT', 'zmin'], locations.lookup['12HWT', 'zmax'], locations.lookup['12HWT', 'zmax']), colour = pen.col, size = pen.size, alpha = 0.3)
        }
            
      } 
    
    if(axes == 'xy') {
        
      hexplot <- ggplot(fish.id, aes(fish.id$PosX, fish.id$PosY))
      hexplot <- hexplot + geom_hex(bins = 55, alpha = 0.6) + scale_fill_gradientn(colours = plot.col, space = 'Lab', limits = c(10, NA), na.value = NA, trans = 'log', breaks = c(10, 100, 1000), name = 'No. pings')
      hexplot <- hexplot +
        annotate('segment', x = locations.lookup['12CSW', 'xmin'], xend = locations.lookup['12CSE', 'xmax'], y = locations.lookup['12CSW', 'ymin'], yend = locations.lookup['12CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
        annotate('segment', x = locations.lookup['12CSW', 'xmin'], xend = locations.lookup['12CNW', 'xmin'], y = locations.lookup['12CSW', 'ymin'], yend = locations.lookup['12CNW', 'ymax'], colour = pen.col, size = pen.size) +  # pen boundary
        annotate('segment', x = locations.lookup['12CNW', 'xmin'], xend = locations.lookup['12CNE', 'xmax'], y = locations.lookup['12CNW', 'ymax'], yend = locations.lookup['12CNE', 'ymax'], colour = pen.col, size = pen.size) + # pen boundary
        annotate('segment', x = locations.lookup['12CNE', 'xmax'], xend = locations.lookup['12CSE', 'xmax'], y = locations.lookup['12CNE', 'ymax'], yend = locations.lookup['12CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
        annotate('segment', x = locations.lookup['12CSW', 'xmin'], xend = locations.lookup['12CSE', 'xmax'], y = locations.lookup['12CSW', 'ymax'], yend = locations.lookup['12CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
        annotate('segment', x = locations.lookup['12CSW', 'xmax'], xend = locations.lookup['12CNW', 'xmax'], y = locations.lookup['12CSW', 'ymin'], yend = locations.lookup['12CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
        annotate('segment', x = locations.lookup['12CNW', 'xmin'], xend = locations.lookup['12CNE', 'xmax'], y = locations.lookup['12CNW', 'ymin'], yend = locations.lookup['12CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
        annotate('segment', x = locations.lookup['12CNE', 'xmin'], xend = locations.lookup['12CSE', 'xmin'], y = locations.lookup['12CNE', 'ymax'], yend = locations.lookup['12CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
        annotate('rect', xmin = locations.lookup['FS12', 'xmin'], xmax = locations.lookup['FS12', 'xmax'], ymin = locations.lookup['FS12', 'ymin'], ymax = locations.lookup['FS12', 'ymax'], colour = pen.col, size = pen.size, alpha = 0.3) + # hide boundary
        theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
        scale_x_continuous('x (m)', limits = c(35, 70)) + scale_y_continuous('y (m)', limits = c(35,70))
      
      if(hideselect == 'E'){ # east hide
        hexplot <- hexplot +
        annotate('rect', xmin = locations.lookup['12HET', 'xmin'], xmax = locations.lookup['12HET', 'xmax'], ymin = locations.lookup['12HET', 'ymin'], ymax = locations.lookup['12HET', 'ymax'], colour = pen.col, size = pen.size, alpha = 0.3)
      }
      
      if(hideselect == 'W'){ # west hide
        hexplot <- hexplot +
        annotate('rect', xmin = locations.lookup['12HWT', 'xmin'], xmax = locations.lookup['12HWT', 'xmax'], ymin = locations.lookup['12HWT', 'ymin'], ymax = locations.lookup['12HWT', 'ymax'], colour = pen.col, size = pen.size, alpha = 0.3)
      }
      
        }
    
    } 
  
  if(pen == 14){
    
    
    fish.id <- subset(dayfile, PEN == 14)  
    #pingmax <- as.integer((as.double(max(fish.id$EchoTime))-as.double(min(fish.id$EchoTime)))/diviser)
    
    if(axes == 'xz') {
      hexplot <- ggplot(fish.id, aes(fish.id$PosX, fish.id$PosZ))
      hexplot <- hexplot + geom_hex(bins = 55, alpha = 0.6) + scale_fill_gradientn(colours = plot.col, space = 'Lab', limits = c(10, NA), na.value = NA, trans = 'log', breaks = c(10, 100, 1000), name = 'No. pings')
      hexplot <- hexplot +
        annotate('segment', x = locations.lookup['14EW', 'xmin'], xend = locations.lookup['14EE', 'xmax'], y = 0, yend = 0, colour = pen.col, size = pen.size) + # pEE boundary
        annotate('segment', x = locations.lookup['14EW', 'xmin'], xend = locations.lookup['14EW', 'xmin'], y = 0, yend = 15, colour = pen.col, size = pen.size) +  # pEE boundary
        annotate('segment', x = locations.lookup['14EE', 'xmax'], xend = locations.lookup['14EE', 'xmax'], y = 0, yend = 15, colour = pen.col, size = pen.size) + # pEE boundary
        annotate('segment', x = locations.lookup['14EW', 'xmin'], xend = ((locations.lookup['14EE', 'xmax'] - locations.lookup['14EW', 'xmin'])/2 + locations.lookup['14EW', 'xmin']), y = 15, yend = 20, colour = pen.col, size = pen.size) + # pEE boundary
        annotate('segment', x = locations.lookup['14EE', 'xmax'], xend = ((locations.lookup['14EE', 'xmax'] - locations.lookup['14EW', 'xmin'])/2 + locations.lookup['14EW', 'xmin']), y = 15, yend = 20, colour = pen.col, size = pen.size) + # pEE boundary
        annotate('segment', x = locations.lookup['14EW', 'xmax'], xend = locations.lookup['14EW', 'xmax'], y = 0, yend = 15, colour = pen.col, linetype = 'dotted', size = pen.size) + # pEE location boundary
        annotate('segment', x = locations.lookup['14EE', 'xmin'], xend = locations.lookup['14EE', 'xmin'], y = 0, yend = 15, colour = pen.col, linetype = 'dotted', size = pen.size) + # pEE location boundary
        annotate('segment', x = locations.lookup['14EW', 'xmin'], xend = locations.lookup['14EE', 'xmax'], y = 15, yend = 15, colour = pen.col, linetype = 'dotted', size = pen.size) + # pEE location boundary
        #annotate('polygon', x = c(locations.lookup['14HET', 'xmin'], locations.lookup['14HET', 'xmax'], locations.lookup['14HEB', 'xmax'], locations.lookup['14HEB', 'xmin']), y = c(locations.lookup['14HET', 'zmin'], locations.lookup['14HET', 'zmin'], locations.lookup['14HET', 'zmax'], locations.lookup['14HET', 'zmax']), colour = pen.col, size = pen.size, alpha = 0.3) + # hide boundary
        #annotate('polygon', x = c(locations.lookup['14HWT', 'xmin'], locations.lookup['14HWT', 'xmax'], locations.lookup['14HWB', 'xmax'], locations.lookup['14HWB', 'xmin']), y = c(locations.lookup['14HWT', 'zmin'], locations.lookup['14HWT', 'zmin'], locations.lookup['14HWT', 'zmax'], locations.lookup['14HWT', 'zmax']), colour = pen.col, size = pen.size, alpha = 0.3) + # hide boundary
        annotate('rect', xmin = locations.lookup['FS14', 'xmin'], xmax = locations.lookup['FS14', 'xmax'], ymin = locations.lookup['FS14', 'zmin'], ymax = locations.lookup['FS14', 'zmax'], colour = pen.col, size = pen.size, alpha = 0.3) + # hide boundary
        theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
        scale_x_continuous('x (m)', limits = c(10, 45)) + scale_y_reverse('z (m)', limits = c(35,0))
      
      if(hideselect == 'E'){ # east hide
        hexplot <- hexplot +
          annotate('polygon', x = c(locations.lookup['14HET', 'xmin'], locations.lookup['14HET', 'xmax'], locations.lookup['14HEB', 'xmax'], locations.lookup['14HEB', 'xmin']), y = c(locations.lookup['14HET', 'zmin'], locations.lookup['14HET', 'zmin'], locations.lookup['14HET', 'zmax'], locations.lookup['14HET', 'zmax']), colour = pen.col, size = pen.size, alpha = 0.3)
      }
      
      if(hideselect == 'W'){ # west hide
        hexplot <- hexplot +
          annotate('polygon', x = c(locations.lookup['14HWT', 'xmin'], locations.lookup['14HWT', 'xmax'], locations.lookup['14HWB', 'xmax'], locations.lookup['14HWB', 'xmin']), y = c(locations.lookup['14HWT', 'zmin'], locations.lookup['14HWT', 'zmin'], locations.lookup['14HWT', 'zmax'], locations.lookup['14HWT', 'zmax']), colour = pen.col, size = pen.size, alpha = 0.3) # west hide
      }
      
      
    }
    
    if(axes == 'yz') {
      
      hexplot <- ggplot(fish.id, aes(fish.id$PosY, fish.id$PosZ))
      hexplot <- hexplot + geom_hex(bins = 55, alpha = 0.6) + scale_fill_gradientn(colours = plot.col, space = 'Lab', limits = c(10, NA), na.value = NA, trans = 'log', breaks = c(10, 100, 1000), name = 'No. pings')
      hexplot <- hexplot +
        annotate('segment', x = locations.lookup['14ES', 'ymin'], xend = locations.lookup['14EN', 'ymax'], y = 0, yend = 0, colour = pen.col, size = pen.size) + # pEE boundary
        annotate('segment', x = locations.lookup['14ES', 'ymin'], xend = locations.lookup['14ES', 'ymin'], y = 0, yend = 15, colour = pen.col, size = pen.size) +  # pEE boundary
        annotate('segment', x = locations.lookup['14EN', 'ymax'], xend = locations.lookup['14EN', 'ymax'], y = 0, yend = 15, colour = pen.col, size = pen.size) + # pEE boundary
        annotate('segment', x = locations.lookup['14ES', 'ymin'], xend = ((locations.lookup['14EN', 'ymax'] - locations.lookup['14ES', 'ymin'])/2 + locations.lookup['14ES', 'ymin']), y = 15, yend = 20, colour = pen.col, size = pen.size) + # pEE boundary
        annotate('segment', x = locations.lookup['14EN', 'ymax'], xend = ((locations.lookup['14EN', 'ymax'] - locations.lookup['14ES', 'ymin'])/2 + locations.lookup['14ES', 'ymin']), y = 15, yend = 20, colour = pen.col, size = pen.size) + # pEE boundary
        annotate('segment', x = locations.lookup['14ES', 'ymax'], xend = locations.lookup['14ES', 'ymax'], y = 0, yend = 15, colour = pen.col, linetype = 'dotted', size = pen.size) + # pEE location boundary
        annotate('segment', x = locations.lookup['14EN', 'ymin'], xend = locations.lookup['14EN', 'ymin'], y = 0, yend = 15, colour = pen.col, linetype = 'dotted', size = pen.size) + # pEE location boundary
        annotate('segment', x = locations.lookup['14ES', 'ymin'], xend = locations.lookup['14EN', 'ymax'], y = 15, yend = 15, colour = pen.col, linetype = 'dotted', size = pen.size) + # pEE location boundary
        #annotate('polygon', x = c(locations.lookup['14HET', 'ymin'], locations.lookup['14HET', 'ymax'], locations.lookup['14HEB', 'ymax'], locations.lookup['14HEB', 'ymin']), y = c(locations.lookup['14HET', 'zmin'], locations.lookup['14HET', 'zmin'], locations.lookup['14HET', 'zmax'], locations.lookup['14HET', 'zmax']), colour = pen.col, size = pen.size, alpha = 0.3) + # hide boundary
        #annotate('polygon', x = c(locations.lookup['14HWT', 'ymin'], locations.lookup['14HWT', 'ymax'], locations.lookup['14HWB', 'ymax'], locations.lookup['14HWB', 'ymin']), y = c(locations.lookup['14HWT', 'zmin'], locations.lookup['14HWT', 'zmin'], locations.lookup['14HWT', 'zmax'], locations.lookup['14HWT', 'zmax']), colour = pen.col, size = pen.size, alpha = 0.3) + # hide boundary
        annotate('rect', xmin = locations.lookup['FS14', 'ymin'], xmax = locations.lookup['FS14', 'ymax'], ymin = locations.lookup['FS14', 'zmin'], ymax = locations.lookup['FS14', 'zmax'], colour = pen.col, size = pen.size, alpha = 0.3) + # hide boundary
        theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
        scale_x_continuous('y (m)', limits = c(35, 70)) + scale_y_reverse('z (m)', limits = c(35,0))
      
      if(hideselect == 'E'){ # east hide
        hexplot <- hexplot +
          annotate('polygon', x = c(locations.lookup['14HET', 'ymin'], locations.lookup['14HET', 'ymax'], locations.lookup['14HEB', 'ymax'], locations.lookup['14HEB', 'ymin']), y = c(locations.lookup['14HET', 'zmin'], locations.lookup['14HET', 'zmin'], locations.lookup['14HET', 'zmax'], locations.lookup['14HET', 'zmax']), colour = pen.col, size = pen.size, alpha = 0.3)
      }
      
      if(hideselect == 'W'){ # west hide
        hexplot <- hexplot +
          annotate('polygon', x = c(locations.lookup['14HWT', 'ymin'], locations.lookup['14HWT', 'ymax'], locations.lookup['14HWB', 'ymax'], locations.lookup['14HWB', 'ymin']), y = c(locations.lookup['14HWT', 'zmin'], locations.lookup['14HWT', 'zmin'], locations.lookup['14HWT', 'zmax'], locations.lookup['14HWT', 'zmax']), colour = pen.col, size = pen.size, alpha = 0.3)
      }
      
      
    } 
    
    if(axes == 'xy') {
      
    hexplot <- ggplot(fish.id, aes(fish.id$PosX, fish.id$PosY))
    hexplot <- hexplot + geom_hex(bins = 55, alpha = 0.6) + scale_fill_gradientn(colours = plot.col, space = 'Lab', limits = c(10, NA), na.value = NA, trans = 'log', breaks = c(10, 100, 1000), name = 'No. pings')
    hexplot <- hexplot +
      annotate('segment', x = locations.lookup['14CSW', 'xmin'], xend = locations.lookup['14CSE', 'xmax'], y = locations.lookup['14CSW', 'ymin'], yend = locations.lookup['14CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['14CSW', 'xmin'], xend = locations.lookup['14CNW', 'xmin'], y = locations.lookup['14CSW', 'ymin'], yend = locations.lookup['14CNW', 'ymax'], colour = pen.col, size = pen.size) +  # pen boundary
      annotate('segment', x = locations.lookup['14CNW', 'xmin'], xend = locations.lookup['14CNE', 'xmax'], y = locations.lookup['14CNW', 'ymax'], yend = locations.lookup['14CNE', 'ymax'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['14CNE', 'xmax'], xend = locations.lookup['14CSE', 'xmax'], y = locations.lookup['14CNE', 'ymax'], yend = locations.lookup['14CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['14CSW', 'xmin'], xend = locations.lookup['14CSE', 'xmax'], y = locations.lookup['14CSW', 'ymax'], yend = locations.lookup['14CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['14CSW', 'xmax'], xend = locations.lookup['14CNW', 'xmax'], y = locations.lookup['14CSW', 'ymin'], yend = locations.lookup['14CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['14CNW', 'xmin'], xend = locations.lookup['14CNE', 'xmax'], y = locations.lookup['14CNW', 'ymin'], yend = locations.lookup['14CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['14CNE', 'xmin'], xend = locations.lookup['14CSE', 'xmin'], y = locations.lookup['14CNE', 'ymax'], yend = locations.lookup['14CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      #annotate('rect', xmin = locations.lookup['14HET', 'xmin'], xmax = locations.lookup['14HET', 'xmax'], ymin = locations.lookup['14HET', 'ymin'], ymax = locations.lookup['14HET', 'ymax'], colour = pen.col, size = pen.size, alpha = 0.3) + # hide boundary
      #annotate('rect', xmin = locations.lookup['14HWT', 'xmin'], xmax = locations.lookup['14HWT', 'xmax'], ymin = locations.lookup['14HWT', 'ymin'], ymax = locations.lookup['14HWT', 'ymax'], colour = pen.col, size = pen.size, alpha = 0.3) + # hide boundary
      annotate('rect', xmin = locations.lookup['FS14', 'xmin'], xmax = locations.lookup['FS14', 'xmax'], ymin = locations.lookup['FS14', 'ymin'], ymax = locations.lookup['FS14', 'ymax'], colour = pen.col, size = pen.size, alpha = 0.3) + # hide boundary
      theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
      scale_x_continuous('x (m)', limits = c(10, 45)) + scale_y_continuous('y (m)', limits = c(35,70))
    
    if(hideselect == 'E'){ # east hide
      hexplot <- hexplot +
        annotate('rect', xmin = locations.lookup['14HET', 'xmin'], xmax = locations.lookup['14HET', 'xmax'], ymin = locations.lookup['14HET', 'ymin'], ymax = locations.lookup['14HET', 'ymax'], colour = pen.col, size = pen.size, alpha = 0.3)
    }
    
    if(hideselect == 'W'){ # west hide
      hexplot <- hexplot +
        annotate('rect', xmin = locations.lookup['14HWT', 'xmin'], xmax = locations.lookup['14HWT', 'xmax'], ymin = locations.lookup['14HWT', 'ymin'], ymax = locations.lookup['14HWT', 'ymax'], colour = pen.col, size = pen.size, alpha = 0.3)
    }
    
    
    }
    
    }  
  
  if(pen == 15){
    
    
    fish.id <- subset(dayfile, PEN == 15)  
    #pingmax <- as.integer((as.double(max(fish.id$EchoTime))-as.double(min(fish.id$EchoTime)))/diviser)
    
    if(axes == 'xz') {
      hexplot <- ggplot(fish.id, aes(fish.id$PosX, fish.id$PosZ))
      hexplot <- hexplot + geom_hex(bins = 55, alpha = 0.6) + scale_fill_gradientn(colours = plot.col, space = 'Lab', limits = c(10, NA), na.value = NA, trans = 'log', breaks = c(10, 100, 1000), name = 'No. pings')
      hexplot <- hexplot +
        annotate('segment', x = locations.lookup['15EW', 'xmin'], xend = locations.lookup['15EE', 'xmax'], y = 0, yend = 0, colour = pen.col, size = pen.size) + # pEE boundary
        annotate('segment', x = locations.lookup['15EW', 'xmin'], xend = locations.lookup['15EW', 'xmin'], y = 0, yend = 15, colour = pen.col, size = pen.size) +  # pEE boundary
        annotate('segment', x = locations.lookup['15EE', 'xmax'], xend = locations.lookup['15EE', 'xmax'], y = 0, yend = 15, colour = pen.col, size = pen.size) + # pEE boundary
        annotate('segment', x = locations.lookup['15EW', 'xmin'], xend = ((locations.lookup['15EE', 'xmax'] - locations.lookup['15EW', 'xmin'])/2 + locations.lookup['15EW', 'xmin']), y = 15, yend = 20, colour = pen.col, size = pen.size) + # pEE boundary
        annotate('segment', x = locations.lookup['15EE', 'xmax'], xend = ((locations.lookup['15EE', 'xmax'] - locations.lookup['15EW', 'xmin'])/2 + locations.lookup['15EW', 'xmin']), y = 15, yend = 20, colour = pen.col, size = pen.size) + # pEE boundary
        annotate('segment', x = locations.lookup['15EW', 'xmax'], xend = locations.lookup['15EW', 'xmax'], y = 0, yend = 15, colour = pen.col, linetype = 'dotted', size = pen.size) + # pEE location boundary
        annotate('segment', x = locations.lookup['15EE', 'xmin'], xend = locations.lookup['15EE', 'xmin'], y = 0, yend = 15, colour = pen.col, linetype = 'dotted', size = pen.size) + # pEE location boundary
        annotate('segment', x = locations.lookup['15EW', 'xmin'], xend = locations.lookup['15EE', 'xmax'], y = 15, yend = 15, colour = pen.col, linetype = 'dotted', size = pen.size) + # pEE location boundary
        #annotate('polygon', x = c(locations.lookup['15HET', 'xmin'], locations.lookup['15HET', 'xmax'], locations.lookup['15HEB', 'xmax'], locations.lookup['15HEB', 'xmin']), y = c(locations.lookup['15HET', 'zmin'], locations.lookup['15HET', 'zmin'], locations.lookup['15HET', 'zmax'], locations.lookup['15HET', 'zmax']), colour = pen.col, size = pen.size, alpha = 0.3) + # hide boundary
        #annotate('polygon', x = c(locations.lookup['15HWT', 'xmin'], locations.lookup['15HWT', 'xmax'], locations.lookup['15HWB', 'xmax'], locations.lookup['15HWB', 'xmin']), y = c(locations.lookup['15HWT', 'zmin'], locations.lookup['15HWT', 'zmin'], locations.lookup['15HWT', 'zmax'], locations.lookup['15HWT', 'zmax']), colour = pen.col, size = pen.size, alpha = 0.3) + # hide boundary
        annotate('rect', xmin = locations.lookup['FS15', 'xmin'], xmax = locations.lookup['FS15', 'xmax'], ymin = locations.lookup['FS15', 'zmin'], ymax = locations.lookup['FS15', 'zmax'], colour = pen.col, size = pen.size, alpha = 0.3) + # hide boundary
        theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
        scale_x_continuous('x (m)', limits = c(10, 45)) + scale_y_reverse('z (m)', limits = c(35,0))
      
      if(hideselect == 'E'){ # east hide
        hexplot <- hexplot +
          annotate('polygon', x = c(locations.lookup['15HET', 'xmin'], locations.lookup['15HET', 'xmax'], locations.lookup['15HEB', 'xmax'], locations.lookup['15HEB', 'xmin']), y = c(locations.lookup['15HET', 'zmin'], locations.lookup['15HET', 'zmin'], locations.lookup['15HET', 'zmax'], locations.lookup['15HET', 'zmax']), colour = pen.col, size = pen.size, alpha = 0.3)
      }
      
      if(hideselect == 'W'){ # west hide
        hexplot <- hexplot +
          annotate('polygon', x = c(locations.lookup['15HWT', 'xmin'], locations.lookup['15HWT', 'xmax'], locations.lookup['15HWB', 'xmax'], locations.lookup['15HWB', 'xmin']), y = c(locations.lookup['15HWT', 'zmin'], locations.lookup['15HWT', 'zmin'], locations.lookup['15HWT', 'zmax'], locations.lookup['15HWT', 'zmax']), colour = pen.col, size = pen.size, alpha = 0.3) # west hide
      }
      
    }
    
    if(axes == 'yz') {
      
      hexplot <- ggplot(fish.id, aes(fish.id$PosY, fish.id$PosZ))
      hexplot <- hexplot + geom_hex(bins = 55, alpha = 0.6) + scale_fill_gradientn(colours = plot.col, space = 'Lab', limits = c(10, NA), na.value = NA, trans = 'log', breaks = c(10, 100, 1000), name = 'No. pings')
      hexplot <- hexplot +
        annotate('segment', x = locations.lookup['15ES', 'ymin'], xend = locations.lookup['15EN', 'ymax'], y = 0, yend = 0, colour = pen.col, size = pen.size) + # pEE boundary
        annotate('segment', x = locations.lookup['15ES', 'ymin'], xend = locations.lookup['15ES', 'ymin'], y = 0, yend = 15, colour = pen.col, size = pen.size) +  # pEE boundary
        annotate('segment', x = locations.lookup['15EN', 'ymax'], xend = locations.lookup['15EN', 'ymax'], y = 0, yend = 15, colour = pen.col, size = pen.size) + # pEE boundary
        annotate('segment', x = locations.lookup['15ES', 'ymin'], xend = ((locations.lookup['15EN', 'ymax'] - locations.lookup['15ES', 'ymin'])/2 + locations.lookup['15ES', 'ymin']), y = 15, yend = 20, colour = pen.col, size = pen.size) + # pEE boundary
        annotate('segment', x = locations.lookup['15EN', 'ymax'], xend = ((locations.lookup['15EN', 'ymax'] - locations.lookup['15ES', 'ymin'])/2 + locations.lookup['15ES', 'ymin']), y = 15, yend = 20, colour = pen.col, size = pen.size) + # pEE boundary
        annotate('segment', x = locations.lookup['15ES', 'ymax'], xend = locations.lookup['15ES', 'ymax'], y = 0, yend = 15, colour = pen.col, linetype = 'dotted', size = pen.size) + # pEE location boundary
        annotate('segment', x = locations.lookup['15EN', 'ymin'], xend = locations.lookup['15EN', 'ymin'], y = 0, yend = 15, colour = pen.col, linetype = 'dotted', size = pen.size) + # pEE location boundary
        annotate('segment', x = locations.lookup['15ES', 'ymin'], xend = locations.lookup['15EN', 'ymax'], y = 15, yend = 15, colour = pen.col, linetype = 'dotted', size = pen.size) + # pEE location boundary
        #annotate('polygon', x = c(locations.lookup['15HET', 'ymin'], locations.lookup['15HET', 'ymax'], locations.lookup['15HEB', 'ymax'], locations.lookup['15HEB', 'ymin']), y = c(locations.lookup['15HET', 'zmin'], locations.lookup['15HET', 'zmin'], locations.lookup['15HET', 'zmax'], locations.lookup['15HET', 'zmax']), colour = pen.col, size = pen.size, alpha = 0.3) + # hide boundary
        #annotate('polygon', x = c(locations.lookup['15HWT', 'ymin'], locations.lookup['15HWT', 'ymax'], locations.lookup['15HWB', 'ymax'], locations.lookup['15HWB', 'ymin']), y = c(locations.lookup['15HWT', 'zmin'], locations.lookup['15HWT', 'zmin'], locations.lookup['15HWT', 'zmax'], locations.lookup['15HWT', 'zmax']), colour = pen.col, size = pen.size, alpha = 0.3) + # hide boundary
        annotate('rect', xmin = locations.lookup['FS15', 'ymin'], xmax = locations.lookup['FS15', 'ymax'], ymin = locations.lookup['FS15', 'zmin'], ymax = locations.lookup['FS15', 'zmax'], colour = pen.col, size = pen.size, alpha = 0.3) + # hide boundary
        theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
        scale_x_continuous('y (m)', limits = c(10, 45)) + scale_y_reverse('z (m)', limits = c(35,0))
      
      if(hideselect == 'E'){ # east hide
        hexplot <- hexplot +
          annotate('polygon', x = c(locations.lookup['15HET', 'ymin'], locations.lookup['15HET', 'ymax'], locations.lookup['15HEB', 'ymax'], locations.lookup['15HEB', 'ymin']), y = c(locations.lookup['15HET', 'zmin'], locations.lookup['15HET', 'zmin'], locations.lookup['15HET', 'zmax'], locations.lookup['15HET', 'zmax']), colour = pen.col, size = pen.size, alpha = 0.3)
      }
      
      if(hideselect == 'W'){ # west hide
        hexplot <- hexplot +
          annotate('polygon', x = c(locations.lookup['15HWT', 'ymin'], locations.lookup['15HWT', 'ymax'], locations.lookup['15HWB', 'ymax'], locations.lookup['15HWB', 'ymin']), y = c(locations.lookup['15HWT', 'zmin'], locations.lookup['15HWT', 'zmin'], locations.lookup['15HWT', 'zmax'], locations.lookup['15HWT', 'zmax']), colour = pen.col, size = pen.size, alpha = 0.3)
      }
      
    } 
    
    
    if(axes == 'xy') {
      
    hexplot <- ggplot(fish.id, aes(fish.id$PosX, fish.id$PosY))
    hexplot <- hexplot + geom_hex(bins = 55, alpha = 0.6) + scale_fill_gradientn(colours = plot.col, space = 'Lab', limits = c(10, NA), na.value = NA, trans = 'log', breaks = c(10, 100, 1000), name = 'No. pings')
    hexplot <- hexplot +
      annotate('segment', x = locations.lookup['15CSW', 'xmin'], xend = locations.lookup['15CSE', 'xmax'], y = locations.lookup['15CSW', 'ymin'], yend = locations.lookup['15CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['15CSW', 'xmin'], xend = locations.lookup['15CNW', 'xmin'], y = locations.lookup['15CSW', 'ymin'], yend = locations.lookup['15CNW', 'ymax'], colour = pen.col, size = pen.size) +  # pen boundary
      annotate('segment', x = locations.lookup['15CNW', 'xmin'], xend = locations.lookup['15CNE', 'xmax'], y = locations.lookup['15CNW', 'ymax'], yend = locations.lookup['15CNE', 'ymax'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['15CNE', 'xmax'], xend = locations.lookup['15CSE', 'xmax'], y = locations.lookup['15CNE', 'ymax'], yend = locations.lookup['15CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['15CSW', 'xmin'], xend = locations.lookup['15CSE', 'xmax'], y = locations.lookup['15CSW', 'ymax'], yend = locations.lookup['15CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['15CSW', 'xmax'], xend = locations.lookup['15CNW', 'xmax'], y = locations.lookup['15CSW', 'ymin'], yend = locations.lookup['15CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['15CNW', 'xmin'], xend = locations.lookup['15CNE', 'xmax'], y = locations.lookup['15CNW', 'ymin'], yend = locations.lookup['15CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['15CNE', 'xmin'], xend = locations.lookup['15CSE', 'xmin'], y = locations.lookup['15CNE', 'ymax'], yend = locations.lookup['15CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      #annotate('rect', xmin = locations.lookup['15HET', 'xmin'], xmax = locations.lookup['15HET', 'xmax'], ymin = locations.lookup['15HET', 'ymin'], ymax = locations.lookup['15HET', 'ymax'], colour = pen.col, size = pen.size, alpha = 0.3) + # hide boundary
      #annotate('rect', xmin = locations.lookup['15HWT', 'xmin'], xmax = locations.lookup['15HWT', 'xmax'], ymin = locations.lookup['15HWT', 'ymin'], ymax = locations.lookup['15HWT', 'ymax'], colour = pen.col, size = pen.size, alpha = 0.3) + # hide boundary
      annotate('rect', xmin = locations.lookup['FS15', 'xmin'], xmax = locations.lookup['FS15', 'xmax'], ymin = locations.lookup['FS15', 'ymin'], ymax = locations.lookup['FS15', 'ymax'], colour = pen.col, size = pen.size, alpha = 0.3) + # hide boundary
      theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
      scale_x_continuous('x (m)', limits = c(10, 45)) + scale_y_continuous('y (m)', limits = c(10,45))
    
    if(hideselect == 'E'){ # east hide
      hexplot <- hexplot +
        annotate('rect', xmin = locations.lookup['15HET', 'xmin'], xmax = locations.lookup['15HET', 'xmax'], ymin = locations.lookup['15HET', 'ymin'], ymax = locations.lookup['15HET', 'ymax'], colour = pen.col, size = pen.size, alpha = 0.3)
    }
    
    if(hideselect == 'W'){ # west hide
      hexplot <- hexplot +
        annotate('rect', xmin = locations.lookup['15HWT', 'xmin'], xmax = locations.lookup['15HWT', 'xmax'], ymin = locations.lookup['15HWT', 'ymin'], ymax = locations.lookup['15HWT', 'ymax'], colour = pen.col, size = pen.size, alpha = 0.3)
    }
    
    
    }
    
    }  
  
  print(hexplot)
  hexplot <<- hexplot
  fish.id <<- fish.id
  
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
  fish.id <- dayfile
  #fish.id <- subset(dayfile, Period == period)
  
  if(fish.id[1,'PEN'] == '12')
  {
#    if(alims == 't'){plot(fish.id$PosX, fish.id$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(35, 70), ylim = c(35, 70), type = 'l', col = fishpal[20])}
#    if(alims == 'w'){plot(fish.id$PosX, fish.id$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, type = 'l', col = fishpal[20])}
    plot(fish.id$PosX, fish.id$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(35, 70), ylim = c(35, 70), type = 'p', col = depthpal[round(fish.id$PosZ)]) # tight plot
    rect(locations.lookup['12EW', 'xmin'], locations.lookup['12EW', 'ymin'], locations.lookup['12EW', 'xmax'], locations.lookup['12EW', 'ymax'], lty = 2) # 12EW edge
    rect(locations.lookup['12ES', 'xmin'], locations.lookup['12ES', 'ymin'], locations.lookup['12ES', 'xmax'], locations.lookup['12ES', 'ymax'], lty = 2) # 12ES edge
    rect(locations.lookup['12EE', 'xmin'], locations.lookup['12EE', 'ymin'], locations.lookup['12EE', 'xmax'], locations.lookup['12EE', 'ymax'], lty = 2) # 12EE edge
    rect(locations.lookup['12EN', 'xmin'], locations.lookup['12EN', 'ymin'], locations.lookup['12EN', 'xmax'], locations.lookup['12EN', 'ymax'], lty = 2) # 12EN edge
    rect(locations.lookup['12HET', 'xmin'], locations.lookup['12HET', 'ymin'], locations.lookup['12HET', 'xmax'], locations.lookup['12HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HET
    rect(locations.lookup['12HWT', 'xmin'], locations.lookup['12HWT', 'ymin'], locations.lookup['12HWT', 'xmax'], locations.lookup['12HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HWT
    rect(locations.lookup['12FS', 'xmin'], locations.lookup['12FS', 'ymin'], locations.lookup['12FS', 'xmax'], locations.lookup['12FS', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 12FS
    rect(locations.lookup['12EW', 'xmin'], locations.lookup['12ES', 'ymin'], locations.lookup['12EE', 'xmax'], locations.lookup['12EN', 'ymax'], lwd = 2) # cage limits
    
  }
  
  if(fish.id[1,'PEN'] == '14'){
    
#    if(alims == 't'){plot(fish.id$PosX, fish.id$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(10, 45), ylim = c(35, 70), type = 'l', col = fishpal[20])}
#    if(alims == 'w'){plot(fish.id$PosX, fish.id$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, type = 'l', col = fishpal[20])}
    plot(fish.id$PosX, fish.id$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(10, 45), ylim = c(35, 70), type = 'p', col = depthpal[round(fish.id$PosZ)]) # tight plot
    rect(locations.lookup['14EW', 'xmin'], locations.lookup['14EW', 'ymin'], locations.lookup['14EW', 'xmax'], locations.lookup['14EW', 'ymax'], lty = 2) # 14EW edge
    rect(locations.lookup['14ES', 'xmin'], locations.lookup['14ES', 'ymin'], locations.lookup['14ES', 'xmax'], locations.lookup['14ES', 'ymax'], lty = 2) # 14ES edge
    rect(locations.lookup['14EE', 'xmin'], locations.lookup['14EE', 'ymin'], locations.lookup['14EE', 'xmax'], locations.lookup['14EE', 'ymax'], lty = 2) # 14EE edge
    rect(locations.lookup['14EN', 'xmin'], locations.lookup['14EN', 'ymin'], locations.lookup['14EN', 'xmax'], locations.lookup['14EN', 'ymax'], lty = 2) # 14EN edge
    rect(locations.lookup['14HET', 'xmin'], locations.lookup['14HET', 'ymin'], locations.lookup['14HET', 'xmax'], locations.lookup['14HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HET
    rect(locations.lookup['14HWT', 'xmin'], locations.lookup['14HWT', 'ymin'], locations.lookup['14HWT', 'xmax'], locations.lookup['14HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HWT
    rect(locations.lookup['14FS', 'xmin'], locations.lookup['14FS', 'ymin'], locations.lookup['14FS', 'xmax'], locations.lookup['14FS', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 14FS
    rect(locations.lookup['14EW', 'xmin'], locations.lookup['14ES', 'ymin'], locations.lookup['14EE', 'xmax'], locations.lookup['14EN', 'ymax'], lwd = 2) # cage limits
    
  }
  
  if(fish.id[1,'PEN'] == '15'){
    
#    if(alims == 't'){plot(fish.id$PosX, fish.id$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(10, 45), ylim = c(10, 45), type = 'l', col = fishpal[20])}
#    if(alims == 'w'){plot(fish.id$PosX, fish.id$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(10, 45), ylim = c(10, 45), type = 'l', col = fishpal[20])}
    plot(fish.id$PosX, fish.id$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(10, 45), ylim = c(10, 45), type = 'p', col = depthpal[round(fish.id$PosZ)]) # tight plot
    rect(locations.lookup['15EW', 'xmin'], locations.lookup['15EW', 'ymin'], locations.lookup['15EW', 'xmax'], locations.lookup['15EW', 'ymax'], lty = 2) # 15EW edge
    rect(locations.lookup['15ES', 'xmin'], locations.lookup['15ES', 'ymin'], locations.lookup['15ES', 'xmax'], locations.lookup['15ES', 'ymax'], lty = 2) # 15ES edge
    rect(locations.lookup['15EE', 'xmin'], locations.lookup['15EE', 'ymin'], locations.lookup['15EE', 'xmax'], locations.lookup['15EE', 'ymax'], lty = 2) # 15EE edge
    rect(locations.lookup['15EN', 'xmin'], locations.lookup['15EN', 'ymin'], locations.lookup['15EN', 'xmax'], locations.lookup['15EN', 'ymax'], lty = 2) # 15EN edge
    rect(locations.lookup['15HET', 'xmin'], locations.lookup['15HET', 'ymin'], locations.lookup['15HET', 'xmax'], locations.lookup['15HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HET
    rect(locations.lookup['15HWT', 'xmin'], locations.lookup['15HWT', 'ymin'], locations.lookup['15HWT', 'xmax'], locations.lookup['15HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HWT
    rect(locations.lookup['15FS', 'xmin'], locations.lookup['15FS', 'ymin'], locations.lookup['15FS', 'xmax'], locations.lookup['15FS', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 15FS
    rect(locations.lookup['15EW', 'xmin'], locations.lookup['15ES', 'ymin'], locations.lookup['15EE', 'xmax'], locations.lookup['15EN', 'ymax'], lwd = 2) # cage limits
    
  }
  
} # end of function



# 19b. draws a plot of fish location by activity behaviour state

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
  
  if (unique(fish.id$PEN == '7')) {
    
    fish.plot <- ggplot(fish.id, aes(PosX, PosY)) +
      scale_x_continuous('x (m)', limits = c(10, 45)) + scale_y_continuous('y (m)', limits = c(10,45)) + # set scale limits      
      theme(panel.background = element_rect(fill = 'white', colour = 'black')) + # white background, black lines
      geom_point(aes(colour = SUN), size = 3)  + scale_color_manual(name = 'Time of day', values = lightpal, labels = c('Dawn', 'Day', 'Dusk', 'Night')) +
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
      geom_point(aes(colour = SUN), size = 3)  + scale_color_manual(name = 'Time of day', values = lightpal, labels = c('Dawn', 'Day', 'Dusk', 'Night')) +
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

# 20. Add a fish to the current plot

add.depthfish <- function(period)
{
  depthpal <- diverge_hcl(30, h = c(11,266), c = 100, l = c(21,85), power = 0.6, alpha = 0.2)
  fish.id <- subset(dayfile, Period == period)
  points(fish.id$PosX, fish.id$PosY, pch = 20, cex = 1, col = depthpal[round(fish.id$PosZ)])
}




# 21. Fractal dimension

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
  
  if(fish.id[1,2] == '12'){
    xlims <- c(35, 70)
    ylims <- c(35, 70)   
  } else {
    if(fish.id[1,2] == '14'){
      xlims <- c(10, 45)
      ylims <- c(35, 70)  
    } else {
      xlims <- c(10, 45)
      ylims <- c(10, 45) 
    }
  }

    if(units == 'd'){
      plot(fish.id[which(format(as.Date(dayfile$EchoTime, format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d') == timepoints[[1]]),'PosX'], fish.id[which(format(as.Date(dayfile$EchoTime, format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d') == timepoints[[1]]),'PosY'], xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = xlims, ylim = ylims, type = 'p', col = timepal[1])
    }else{
      plot(fish.id[which(trunc(dayfile$EchoTime, "hour") == timepoints[1]),'PosX'], fish.id[which(trunc(dayfile$EchoTime, "hour") == timepoints[1]),'PosY'], xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = xlims, ylim = ylims, type = 'p', col = timepal[1])
    }
    
    legend(xlims[[1]], ylims[[2]], as.character(1:bins), col = rainbow(bins, alpha = 1) , pch = 15, bty = 'n', pt.cex = 1.5, horiz = FALSE, y.intersp = 1, cex = (100-bins)/100)
    
    
    if(units == 'd'){
      for (i in 2:bins){
        points(fish.id[which(format(as.Date(dayfile$EchoTime, format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d') == timepoints[[i]]),'PosX'], fish.id[which(format(as.Date(dayfile$EchoTime, format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d') == timepoints[[i]]),'PosY'], pch = 20, cex = 1, col = timepal[i])
      }
    }else{
      for (i in 2:bins){   
        points(fish.id[which(trunc(dayfile$EchoTime, "hour") == timepoints[i]),'PosX'], fish.id[which(trunc(dayfile$EchoTime, "hour") == timepoints[i]),'PosY'], pch = 20, cex = 1, col = timepal[i])
      }
    }
    
    
    if(fish.id[1,2] == '12')
    {    
      rect(locations.lookup['12EW', 'xmin'], locations.lookup['12EW', 'ymin'], locations.lookup['12EW', 'xmax'], locations.lookup['12EW', 'ymax'], lty = 2) # 12EW edge
      rect(locations.lookup['12ES', 'xmin'], locations.lookup['12ES', 'ymin'], locations.lookup['12ES', 'xmax'], locations.lookup['12ES', 'ymax'], lty = 2) # 12ES edge
      rect(locations.lookup['12EE', 'xmin'], locations.lookup['12EE', 'ymin'], locations.lookup['12EE', 'xmax'], locations.lookup['12EE', 'ymax'], lty = 2) # 12EE edge
      rect(locations.lookup['12EN', 'xmin'], locations.lookup['12EN', 'ymin'], locations.lookup['12EN', 'xmax'], locations.lookup['12EN', 'ymax'], lty = 2) # 12EN edge
      rect(locations.lookup['12HET', 'xmin'], locations.lookup['12HET', 'ymin'], locations.lookup['12HET', 'xmax'], locations.lookup['12HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HET
      rect(locations.lookup['12HWT', 'xmin'], locations.lookup['12HWT', 'ymin'], locations.lookup['12HWT', 'xmax'], locations.lookup['12HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 12HWT
      rect(locations.lookup['12FS', 'xmin'], locations.lookup['12FS', 'ymin'], locations.lookup['12FS', 'xmax'], locations.lookup['12FS', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 12FS
      rect(locations.lookup['12EW', 'xmin'], locations.lookup['12ES', 'ymin'], locations.lookup['12EE', 'xmax'], locations.lookup['12EN', 'ymax'], lwd = 2) # cage limits
      
  } else {
    
    if(fish.id[1,2] == '14'){
    
      rect(locations.lookup['14EW', 'xmin'], locations.lookup['14EW', 'ymin'], locations.lookup['14EW', 'xmax'], locations.lookup['14EW', 'ymax'], lty = 2) # 14EW edge
      rect(locations.lookup['14ES', 'xmin'], locations.lookup['14ES', 'ymin'], locations.lookup['14ES', 'xmax'], locations.lookup['14ES', 'ymax'], lty = 2) # 14ES edge
      rect(locations.lookup['14EE', 'xmin'], locations.lookup['14EE', 'ymin'], locations.lookup['14EE', 'xmax'], locations.lookup['14EE', 'ymax'], lty = 2) # 14EE edge
      rect(locations.lookup['14EN', 'xmin'], locations.lookup['14EN', 'ymin'], locations.lookup['14EN', 'xmax'], locations.lookup['14EN', 'ymax'], lty = 2) # 14EN edge
      rect(locations.lookup['14HET', 'xmin'], locations.lookup['14HET', 'ymin'], locations.lookup['14HET', 'xmax'], locations.lookup['14HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HET
      rect(locations.lookup['14HWT', 'xmin'], locations.lookup['14HWT', 'ymin'], locations.lookup['14HWT', 'xmax'], locations.lookup['14HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 14HWT
      rect(locations.lookup['14FS', 'xmin'], locations.lookup['14FS', 'ymin'], locations.lookup['14FS', 'xmax'], locations.lookup['14FS', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 14FS
      rect(locations.lookup['14EW', 'xmin'], locations.lookup['14ES', 'ymin'], locations.lookup['14EE', 'xmax'], locations.lookup['14EN', 'ymax'], lwd = 2) # cage limits
      
    } else {
      
      rect(locations.lookup['15EW', 'xmin'], locations.lookup['15EW', 'ymin'], locations.lookup['15EW', 'xmax'], locations.lookup['15EW', 'ymax'], lty = 2) # 15EW edge
      rect(locations.lookup['15ES', 'xmin'], locations.lookup['15ES', 'ymin'], locations.lookup['15ES', 'xmax'], locations.lookup['15ES', 'ymax'], lty = 2) # 15ES edge
      rect(locations.lookup['15EE', 'xmin'], locations.lookup['15EE', 'ymin'], locations.lookup['15EE', 'xmax'], locations.lookup['15EE', 'ymax'], lty = 2) # 15EE edge
      rect(locations.lookup['15EN', 'xmin'], locations.lookup['15EN', 'ymin'], locations.lookup['15EN', 'xmax'], locations.lookup['15EN', 'ymax'], lty = 2) # 15EN edge
      rect(locations.lookup['15HET', 'xmin'], locations.lookup['15HET', 'ymin'], locations.lookup['15HET', 'xmax'], locations.lookup['15HET', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HET
      rect(locations.lookup['15HWT', 'xmin'], locations.lookup['15HWT', 'ymin'], locations.lookup['15HWT', 'xmax'], locations.lookup['15HWT', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 15HWT
      rect(locations.lookup['15FS', 'xmin'], locations.lookup['15FS', 'ymin'], locations.lookup['15FS', 'xmax'], locations.lookup['15FS', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 15FS
      rect(locations.lookup['15EW', 'xmin'], locations.lookup['15ES', 'ymin'], locations.lookup['15EE', 'xmax'], locations.lookup['15EN', 'ymax'], lwd = 2) # cage limits
      
    }
    
  }
    
  remove(timepoints)
}

# 25a. Removes single fish id from specified day files

batch.remove <- function(period, start.day, no.days){
  
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  #day1 <- grep(paste0('^..............', start.day, '_day_coded.csv'), files)
  day1 <- str_which(files, paste0(start.day, '_day_coded.csv$'))
  
  end.day <- day1+(no.days-1)
  # dayfile.loc <- files[[grep(paste0('^..............', start.day, '_day_coded.csv'), files)]]
  
  for (i in day1:end.day) {
    dayfile <- read.csv(files[[i]], header = TRUE, sep = ",", colClasses = dayfile.classes) 
  
    dayfile <- dayfile[!(dayfile$Period == period),] # remove dead fish
    write.csv(dayfile, file = files[[i]]) #write output to file
    
  } 
  
}


# 25b. Removes multiple fish id from specified day files (pass vector of fish ids to period variable)

multi.batch.remove <- function(period, start.day, no.days){
  
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  #day1 <- grep(paste0('^..............', start.day, '_day_coded.csv'), files)
  day1 <- str_which(files, paste0(start.day, '_day_coded.csv$'))
  
  end.day <- day1+(no.days-1)
  # dayfile.loc <- files[[grep(paste0('^..............', start.day, '_day_coded.csv'), files)]]
  
  for (i in day1:end.day) {
    #dayfile <- read.csv(files[[i]], header = TRUE, sep = ",", colClasses = dayfile.classes) 
    dayfile <- fread(files[[i]])
    dayfile$EchoTime <- as.POSIXct(dayfile$EchoTime)
    dayfile$PEN <- as.factor(dayfile$PEN)
    dayfile$V1 <- NULL
    
    for (j in 1:length(period)){
    dayfile <- dayfile[!(dayfile$Period == period[j]),] # remove dead fish
    }
    
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
    
    fish.id$PosX.ma <- stats::filter(fish.id$PosX, filt, sides = 1)
    fish.id$PosY.ma <- stats::filter(fish.id$PosY, filt, sides = 1)
    fish.id$PosZ.ma <- stats::filter(fish.id$PosZ, filt, sides = 1)
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
                                                                          'double', 'double', 'double', 'double', 'double', 'double', 'factor',
                                                                          'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                                                                          'double', 'double', 'double', 'double', 'double', 'double', 'double',
                                                                          'double', 'double', 'double', 'double', 'double', 'double', 'double',
                                                                          'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                                                                          'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                                                                          'factor', 'factor', 'double', 'double', 'double', 'double', 'double', 
                                                                          'double', 'double', 'double', 'double', 'double', 'double', 'double'
                                                                          
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


# 33a. Load all data into single data frame. Specify which pen to load with 'pen' parameter or 'all' to load all data.

load.all <- function(pen){

files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  
dayfile <- data.frame()

for(i in 1:length(files)){

  #daytemp <- read.csv(files[[i]], header = TRUE, sep = ",", colClasses = dayfile.classes)
  daytemp <- fread(files[[i]]) #, colClasses = dayfile.classes)
  
  if(pen == 'all'){
    
    daytemp <- daytemp
    
  } else {
    
    daytemp <- daytemp[daytemp$PEN == pen,]
    
  }

  dayfile <- rbind(dayfile, daytemp)

}

# convert factors to numbers
#dayfile[,c(seq(20, 40, 1), seq(54,65, 1))] <- apply(dayfile[,c(seq(20, 40, 1), seq(54,65, 1))], 2, function(x) as.numeric(as.character(x)))

dayfile$EchoTime <- as.POSIXct(dayfile$EchoTime)
dayfile$PEN <- as.factor(dayfile$PEN)
dayfile$V1 <- NULL

#SORT BY TIME AND TAG
dayfile <- arrange(dayfile, Period, EchoTime)

dayfile <<- dayfile

}

# 33b. Load all hide data

load.all.hides <- function(){
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  
  hidefile <- data.frame()
  
  for(i in 1:length(files)){
    
    hidetemp <- read.csv(files[[i]], header = TRUE, sep = ",", colClasses = hidefile.classes)
    
    hidefile <- rbind(hidefile, hidetemp)
    
    hidefile <<- hidefile
  }

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
                                                                             'double', 'double', 'double', 'double', 'double', 'double', 'factor',
                                                                             'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                                                                             'double', 'double', 'double', 'double', 'double', 'double', 'double',
                                                                             'double', 'double', 'double', 'double', 'double', 'double', 'double',
                                                                             'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                                                                             'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                                                                             'factor', 'factor', 'double', 'double', 'double', 'double', 'double', 
                                                                             'double', 'double', 'double', 'double', 'double', 'double', 'double'
                                                                             
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

headplot <- function(threshold = 0.1, penoffset = 14.24){
  
  dayfile$HEAD <- ifelse(dayfile$HEAD + penoffset > 359.99, 0 + (penoffset - (359.99 - dayfile$HEAD)), dayfile$HEAD + penoffset) # recalculate headings with pen offset from north
  
  p12 <- subset(dayfile, PEN == 12 & MSEC >= threshold)
  p14 <- subset(dayfile, PEN == 14 & MSEC >= threshold)
  p15 <- subset(dayfile, PEN == 15 & MSEC >= threshold)
  
  pplot12 <- ggplot(p12, aes(HEAD))
  pplot12 <- pplot12 + geom_histogram(breaks = seq(0, 360, 10), color = 'black', alpha = 0, size = 0.75, closed = 'left') + 
    theme_minimal() + theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
    scale_x_continuous('', limits = c(0, 360), expand = c(0, 0), breaks = c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330)) +
    #scale_y_continuous(limits = c(0, 1500)) +
    coord_polar(theta = 'x', start = 0) +
    ggtitle('Pen 12') + theme(plot.title = element_text(hjust = 0.5))
  
  pplot14 <- ggplot(p14, aes(HEAD))
  pplot14 <- pplot14 + geom_histogram(breaks = seq(0, 360, 10), color = 'black', alpha = 0, size = 0.75) + 
    theme_minimal() + theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
    scale_x_continuous('', limits = c(0, 360), breaks = c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330)) +
    # scale_y_continuous(limits = c(0, 1500)) +
    coord_polar(theta = 'x', start = 0) +
    ggtitle('Pen 14') + theme(plot.title = element_text(hjust = 0.5))
  
  pplot15 <- ggplot(p15, aes(HEAD))
  pplot15 <- pplot15 + geom_histogram(breaks = seq(0, 360, 10), color = 'black', alpha = 0, size = 0.75) + 
    theme_minimal() + theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
    scale_x_continuous('', limits = c(0, 360), breaks = c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330)) +
    # scale_y_continuous(limits = c(0, 1500)) +
    coord_polar(theta = 'x', start = 0) +
    ggtitle('Pen 15') + theme(plot.title = element_text(hjust = 0.5))
  
  pplottot <- ggplot(dayfile[dayfile$MSEC >= threshold,], aes(HEAD))
  pplottot <- pplottot + geom_histogram(breaks = seq(0, 360, 10), color = 'black', alpha = 0, size = 0.75) + 
    theme_minimal() + theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
    scale_x_continuous('', limits = c(0, 360), breaks = c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330)) +
    # scale_y_continuous(limits = c(0, 1500)) +
    coord_polar(theta = 'x', start = 0) +
    ggtitle('All pens') + theme(plot.title = element_text(hjust = 0.5))
  
  multiplot(pplot12, pplot14, pplot15, pplottot, cols = 2)
  
}


# 41. Polar plots of turn rates

turnplot <- function(){
  
  p7 <- subset(dayfile, PEN == 7)
  p7$TURNRATE <- p7$TURN/p7$SEC
  p8 <- subset(dayfile, PEN == 8)
  p8$TURNRATE <- p8$TURN/p8$SEC
  
  pplot7 <- ggplot(p7, aes(TURNRATE))
  pplot7 <- pplot7 + geom_histogram(breaks = seq(0, 30, 1), color = 'black', alpha = 0, size = 0.75, closed = 'left') + 
    theme_minimal() + theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
    scale_x_continuous('', limits = c(0, 30), expand = c(0, 0), breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 330)) +
    #scale_y_continuous(limits = c(0, 1500)) +
    coord_polar(theta = 'x', start = 0) +
    ggtitle('Wild wrasse') + theme(plot.title = element_text(hjust = 0.5))
  
  pplot8 <- ggplot(p8, aes(TURNRATE))
  pplot8 <- pplot8 + geom_histogram(breaks = seq(0, 30, 1), color = 'black', alpha = 0, size = 0.75) + 
    theme_minimal() + theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
    scale_x_continuous('', limits = c(0, 30), expand = c(0, 0), breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 330)) +
    # scale_y_continuous(limits = c(0, 1500)) +
    coord_polar(theta = 'x', start = 0) +
    ggtitle('Farmed wrasse') + theme(plot.title = element_text(hjust = 0.5))
  
  multiplot(pplot7, pplot8, cols = 2)
  
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
    
    par(mfrow=c(2,2))
    layout(matrix(c(1,2,3,3), 2, 2,byrow = T))
    par(new=F)
    par(mar = c(4, 4, 4, 4))# + 0.1)
    fishpal <- rainbow_hcl(20, c=100, l=63, start=-360, end=-32, alpha = 0.2)
    #fish.id <- subset(dayfile, Period == period)
    
    # position plot
    
    #if(fish.id[1,2] == '12')
    if(sect[1,2] == '12')
    {
      
      plot(sect$PosX, sect$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(35, 70), ylim = c(35, 70), type = 'l', col = '#26b426') # tight plot
      plot.pen(12)

      text(64, 70, adj = c(0, 1), label = paste0('Salmon feeding: ', sect[1,'SMEAL12'], '\nBiofouling: ', sect[1, 'BIOF12'], '\nSun: ', sect[1,'SUN'], '\nTide: ', sect[1, 'TID']), cex = 1) 
      text(70, 38, adj = c(1, 1), label = paste0(sect[1, 'EchoTime'], ' to ', sect[nrow(sect), 'EchoTime'], '\n', start, ' - ', end))
      
    }else{
      
      if(sect[1,2] == '14') {
      
      plot(sect$PosX, sect$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(10, 45), ylim = c(35, 70), type = 'l', col = '#26b426') # tight plot
      plot.pen(14)

      text(39, 70, adj = c(0, 1), label = paste0('Salmon feeding: ', sect[1,'SMEAL14'], '\nBiofouling: ', sect[1, 'BIOF14'], '\nSun: ', sect[1,'SUN'], '\nTide: ', sect[1, 'TID']), cex = 1) 
      text(45, 38, adj = c(1, 1), label = paste0(sect[1, 'EchoTime'], ' to ', sect[nrow(sect), 'EchoTime'], '\n', start, ' - ', end))
      
      } else {
        
        plot(sect$PosX, sect$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(10, 45), ylim = c(10, 45), type = 'l', col = '#26b426') # tight plot
        plot.pen(15)
        text(37, 45, adj = c(0, 1), label = paste0('Salmon feeding: ', sect[1,'SMEAL15'], '\nBiofouling: ', sect[1, 'BIOF15'], '\nSun: ', sect[1,'SUN'], '\nTide: ', sect[1, 'TID']), cex = 1) 
        text(45, 13, adj = c(1, 1), label = paste0(sect[1, 'EchoTime'], ' to ', sect[nrow(sect), 'EchoTime'], '\n', start, ' - ', end))
        
        
      }
      
    }
    
    #depth plot
    
    plot(sect$EchoTime, sect$PosZ, xlab = 'Time', ylab = 'Depth (m)', ylim = c(35, 0), type = 'l', col = '#26b426')
    #segments(sect[1,4], 15, sect[nrow(fish.id), 4], 15, lty = 2)
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
    plot(sect$EchoTime, sect$HEAD, xlab = 'Time', type = 'l', lwd = 2, col = 'lightblue', ylab = '', yaxt = 'n', ylim = c(0, 360)) # plot turn
    axis(2, ylim = c(0, 360), line = 2, at = c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330, 360), labels = c('0', '30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330', '360'))
    mtext(2, text = 'Turn/heading (degrees)', line = 4.5)
    
    par(new = T)
    plot(sect$EchoTime, sect$MSEC, col = 'red', axes = F, xlab = '', ylab = '', type = 'l', lwd = 2, ylim = c(0, 0.8))
    axis(4, ylim = c(0, 1), line = 2, at = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8), labels = c('0', '0.1', '0.2', '0.3', '0.4', '0.5', '0.6', '0.7', '0.8'))
    #mtext(text = 'velocity (m/sec)', side = 4, line = 3.5)
    #vellag <- filter(sect$MSEC, f5, sides=1) # filter turn
    #lines(sect$EchoTime, vellag, col = 'darkgreen') # add moving average to plot
    
    #par(new = T)
    #plot(sect$EchoTime, sect$BLSEC, col = 'red', axes = F, xlab = '', ylab = '', type = 'l', lwd = 2, ylim = c(0, 6))
    #axis(4, ylim = c(0, 6), at = c(0, 1, 2, 3, 4, 5, 6), labels = c('0', '1', '2', '3', '4', '5', '6'))
    #mtext(text = 'velocity (BL/sec)', side = 4, line = 2.5)
    #vellag <- filter(sect$BLSEC, f5, sides=1) # filter turn
    #lines(sect$EchoTime, vellag, col = 'pink') # add moving average to plot
    
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
kudcols <- terrain.colors(4, alpha = 0.6)

if(unique(dayfile$PEN == 7)){
  
  x <- seq(0, 50, by = 0.5)
  y <- seq(0, 50, by = 0.5)
  xy <- expand.grid(x=x, y=y)
  coordinates(xy) <- ~x+y
  gridded(xy) <- TRUE
  class(xy)

} else {
  
  x <- seq(25, 70, by = 0.5)
  y <- seq(0, 50, by = 0.5)
  xy <- expand.grid(x=x, y=y)
  coordinates(xy) <- ~x+y
  gridded(xy) <- TRUE
  class(xy)  
  
  
}

coords <- dayfile[,c(1, 5, 6)] # extract x,y coords and fish id from dayfile
coordinates(coords) <- c('PosX', 'PosY') # convert to spatial points data frame object
ud <- kernelUD(coords, h = 'href', grid = xy, kern = 'bivnorm') # KUD calculation for adehabitatHR package

#mcp100 <- mcp(coords, percent = 100)
ver50 <- getverticeshr(ud, 50) # extract 50% vertex for plotting
ver95 <- getverticeshr(ud, 95) # extract 95% vertex for plotting
#plot(mcp100, col = NULL, axes = T, xlim = c(10, 45), ylim = c(0, 45)) # plot MCP100
ka <- kernel.area(ud, percent = c(50, 95), unin = 'm', unout = 'm2') # calculates area of KUD50, KUD95 and MCP100

if (unique(dayfile$PEN == 7)){

  plot(ver95, col = kudcols[[1]], axes = T, xlim = c(10, 45), ylim = c(10, 45), xlab = 'x (m)', ylab = 'y (m)') # plot KUD95
  plot(ver50, col = kudcols[[4]], axes = F, xlim = c(10, 45), ylim = c(10, 45), add=T) # plot KUD50

  rect(locations.lookup['7EW', 'xmin'], locations.lookup['7EW', 'ymin'], locations.lookup['7EW', 'xmax'], locations.lookup['7EW', 'ymax'], lty = 2) # 7EW edge
  rect(locations.lookup['7ES', 'xmin'], locations.lookup['7ES', 'ymin'], locations.lookup['7ES', 'xmax'], locations.lookup['7ES', 'ymax'], lty = 2) # 7ES edge
  rect(locations.lookup['7EE', 'xmin'], locations.lookup['7EE', 'ymin'], locations.lookup['7EE', 'xmax'], locations.lookup['7EE', 'ymax'], lty = 2) # 7EE edge
  rect(locations.lookup['7EN', 'xmin'], locations.lookup['7EN', 'ymin'], locations.lookup['7EN', 'xmax'], locations.lookup['7EN', 'ymax'], lty = 2) # 7EN edge
  rect(locations.lookup['7WHSE', 'xmin'], locations.lookup['7WHSE', 'ymin'], locations.lookup['7WHSE', 'xmax'], locations.lookup['7WHSE', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHSE
  rect(locations.lookup['7WHNW', 'xmin'], locations.lookup['7WHNW', 'ymin'], locations.lookup['7WHNW', 'xmax'], locations.lookup['7WHNW', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHNW
  rect(locations.lookup['7FBSE', 'xmin'], locations.lookup['7FBSE', 'ymin'], locations.lookup['7FBSE', 'xmax'], locations.lookup['7FBSE', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 7FBSE
  rect(locations.lookup['7FBNW', 'xmin'], locations.lookup['7FBNW', 'ymin'], locations.lookup['7FBNW', 'xmax'], locations.lookup['7FBNW', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 7FBNW
  rect(locations.lookup['7EW', 'xmin'], locations.lookup['7ES', 'ymin'], locations.lookup['7EE', 'xmax'], locations.lookup['7EN', 'ymax'], lwd = 2) # cage limits
  text(6, 44, labels = bquote(paste(KUD[50], ' = ', .(ka[1,1]), m^2)), adj = c(0,0))
  text(6, 42.5, labels = bquote(paste(KUD[95], ' = ', .(ka[2,1]), m^2)), adj = c(0,0))
  

} else{
  
  plot(ver95, col = kudcols[[1]], axes = T, xlim = c(40, 65), ylim = c(10, 45), xlab = 'x (m)', ylab = 'y (m)') # plot KUD95
  plot(ver50, col = kudcols[[4]], axes = F, xlim = c(40, 65), ylim = c(10, 45), add=T) # plot KUD50
  
  rect(locations.lookup['8EW', 'xmin'], locations.lookup['8EW', 'ymin'], locations.lookup['8EW', 'xmax'], locations.lookup['8EW', 'ymax'], lty = 2) # 7EW edge
  rect(locations.lookup['8ES', 'xmin'], locations.lookup['8ES', 'ymin'], locations.lookup['8ES', 'xmax'], locations.lookup['8ES', 'ymax'], lty = 2) # 7ES edge
  rect(locations.lookup['8EE', 'xmin'], locations.lookup['8EE', 'ymin'], locations.lookup['8EE', 'xmax'], locations.lookup['8EE', 'ymax'], lty = 2) # 7EE edge
  rect(locations.lookup['8EN', 'xmin'], locations.lookup['8EN', 'ymin'], locations.lookup['8EN', 'xmax'], locations.lookup['8EN', 'ymax'], lty = 2) # 7EN edge
  rect(locations.lookup['8WHSW', 'xmin'], locations.lookup['8WHSW', 'ymin'], locations.lookup['8WHSW', 'xmax'], locations.lookup['8WHSW', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHSE
  rect(locations.lookup['8WHNE', 'xmin'], locations.lookup['8WHNE', 'ymin'], locations.lookup['8WHNE', 'xmax'], locations.lookup['8WHNE', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHNW
  rect(locations.lookup['8FBNE', 'xmin'], locations.lookup['8FBNE', 'ymin'], locations.lookup['8FBNE', 'xmax'], locations.lookup['8FBNE', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 7FBNE
  rect(locations.lookup['8FBSW', 'xmin'], locations.lookup['8FBSW', 'ymin'], locations.lookup['8FBSW', 'xmax'], locations.lookup['8FBSW', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 7FBSW
  rect(locations.lookup['8EW', 'xmin'], locations.lookup['8ES', 'ymin'], locations.lookup['8EE', 'xmax'], locations.lookup['8EN', 'ymax'], lwd = 2) # cage limits
  text(31, 44, labels = bquote(paste(KUD[50], ' = ', .(ka[1,1]), m^2)), adj = c(0,0))
  text(31, 42.5, labels = bquote(paste(KUD[95], ' = ', .(ka[2,1]), m^2)), adj = c(0,0))
  
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


# 48. Plot outline of pen 7 or 8

plot.pen <- function(pen){
  
  if (pen == 12){
    
    #plot(1, 1, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(35, 70), ylim = c(35, 70), type = 'l', col = 'black') 
    rect(locations.lookup['12EW', 'xmin'], locations.lookup['12EW', 'ymin'], locations.lookup['12EW', 'xmax'], locations.lookup['12EW', 'ymax'], lty = 2) # 12EW edge
    rect(locations.lookup['12ES', 'xmin'], locations.lookup['12ES', 'ymin'], locations.lookup['12ES', 'xmax'], locations.lookup['12ES', 'ymax'], lty = 2) # 12ES edge
    rect(locations.lookup['12EE', 'xmin'], locations.lookup['12EE', 'ymin'], locations.lookup['12EE', 'xmax'], locations.lookup['12EE', 'ymax'], lty = 2) # 12EE edge
    rect(locations.lookup['12EN', 'xmin'], locations.lookup['12EN', 'ymin'], locations.lookup['12EN', 'xmax'], locations.lookup['12EN', 'ymax'], lty = 2) # 12EN edge
    rect(locations.lookup['12HET', 'xmin'], locations.lookup['12HET', 'ymin'], locations.lookup['12HET', 'xmax'], locations.lookup['12HET', 'ymax'], lty = 3, col = rgb(56, 185, 255, 100, maxColorValue = 255)) # 12HET
    rect(locations.lookup['12HEB', 'xmin'], locations.lookup['12HEB', 'ymin'], locations.lookup['12HEB', 'xmax'], locations.lookup['12HEB', 'ymax'], lty = 3, col = rgb(56, 72, 255, 100, maxColorValue = 255)) # 12HEB
    segments(locations.lookup['12HET', 'xmin'], locations.lookup['12HET', 'ymin'], locations.lookup['12HEB', 'xmin'], locations.lookup['12HEB', 'ymin'], lty = 3) # 12HEB
    segments(locations.lookup['12HET', 'xmax'], locations.lookup['12HET', 'ymin'], locations.lookup['12HEB', 'xmax'], locations.lookup['12HEB', 'ymin'], lty = 3) # 12HEB
    segments(locations.lookup['12HET', 'xmax'], locations.lookup['12HET', 'ymax'], locations.lookup['12HEB', 'xmax'], locations.lookup['12HEB', 'ymax'], lty = 3) # 12HEB
    segments(locations.lookup['12HET', 'xmin'], locations.lookup['12HET', 'ymax'], locations.lookup['12HEB', 'xmin'], locations.lookup['12HEB', 'ymax'], lty = 3) # 12HEB
    rect(locations.lookup['12HWT', 'xmin'], locations.lookup['12HWT', 'ymin'], locations.lookup['12HWT', 'xmax'], locations.lookup['12HWT', 'ymax'], lty = 3, col = rgb(56, 185, 255, 100, maxColorValue = 255)) # 12HWT
    rect(locations.lookup['12HWB', 'xmin'], locations.lookup['12HWB', 'ymin'], locations.lookup['12HWB', 'xmax'], locations.lookup['12HWB', 'ymax'], lty = 3, col = rgb(56, 72, 255, 100, maxColorValue = 255)) # 12HWT
    segments(locations.lookup['12HWT', 'xmin'], locations.lookup['12HWT', 'ymin'], locations.lookup['12HWB', 'xmin'], locations.lookup['12HWB', 'ymin'], lty = 3) # 12HWB
    segments(locations.lookup['12HWT', 'xmax'], locations.lookup['12HWT', 'ymin'], locations.lookup['12HWB', 'xmax'], locations.lookup['12HWB', 'ymin'], lty = 3) # 12HWB
    segments(locations.lookup['12HWT', 'xmax'], locations.lookup['12HWT', 'ymax'], locations.lookup['12HWB', 'xmax'], locations.lookup['12HWB', 'ymax'], lty = 3) # 12HWB
    segments(locations.lookup['12HWT', 'xmin'], locations.lookup['12HWT', 'ymax'], locations.lookup['12HWB', 'xmin'], locations.lookup['12HWB', 'ymax'], lty = 3) # 12HWB
    rect(locations.lookup['FS12', 'xmin'], locations.lookup['FS12', 'ymin'], locations.lookup['FS12', 'xmax'], locations.lookup['FS12', 'ymax'], lty = 3, col = rgb(255, 130, 5, 100, maxColorValue = 255)) # FS12
    rect(locations.lookup['12EW', 'xmin'], locations.lookup['12ES', 'ymin'], locations.lookup['12EE', 'xmax'], locations.lookup['12EN', 'ymax'], lwd = 2) # cage limits
    
  } else {
    
    if(pen == 14) {
    
      #plot(1, 1, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(10, 45), ylim = c(35, 70), type = 'l', col = 'black') 
      rect(locations.lookup['14EW', 'xmin'], locations.lookup['14EW', 'ymin'], locations.lookup['14EW', 'xmax'], locations.lookup['14EW', 'ymax'], lty = 2) # 14EW edge
      rect(locations.lookup['14ES', 'xmin'], locations.lookup['14ES', 'ymin'], locations.lookup['14ES', 'xmax'], locations.lookup['14ES', 'ymax'], lty = 2) # 14ES edge
      rect(locations.lookup['14EE', 'xmin'], locations.lookup['14EE', 'ymin'], locations.lookup['14EE', 'xmax'], locations.lookup['14EE', 'ymax'], lty = 2) # 14EE edge
      rect(locations.lookup['14EN', 'xmin'], locations.lookup['14EN', 'ymin'], locations.lookup['14EN', 'xmax'], locations.lookup['14EN', 'ymax'], lty = 2) # 14EN edge
      rect(locations.lookup['14HET', 'xmin'], locations.lookup['14HET', 'ymin'], locations.lookup['14HET', 'xmax'], locations.lookup['14HET', 'ymax'], lty = 3, col = rgb(56, 185, 255, 100, maxColorValue = 255)) # 14HET
      rect(locations.lookup['14HEB', 'xmin'], locations.lookup['14HEB', 'ymin'], locations.lookup['14HEB', 'xmax'], locations.lookup['14HEB', 'ymax'], lty = 3, col = rgb(56, 72, 255, 100, maxColorValue = 255)) # 14HEB
      segments(locations.lookup['14HET', 'xmin'], locations.lookup['14HET', 'ymin'], locations.lookup['14HEB', 'xmin'], locations.lookup['14HEB', 'ymin'], lty = 3) # 14HEB
      segments(locations.lookup['14HET', 'xmax'], locations.lookup['14HET', 'ymin'], locations.lookup['14HEB', 'xmax'], locations.lookup['14HEB', 'ymin'], lty = 3) # 14HEB
      segments(locations.lookup['14HET', 'xmax'], locations.lookup['14HET', 'ymax'], locations.lookup['14HEB', 'xmax'], locations.lookup['14HEB', 'ymax'], lty = 3) # 14HEB
      segments(locations.lookup['14HET', 'xmin'], locations.lookup['14HET', 'ymax'], locations.lookup['14HEB', 'xmin'], locations.lookup['14HEB', 'ymax'], lty = 3) # 14HEB
      rect(locations.lookup['14HWT', 'xmin'], locations.lookup['14HWT', 'ymin'], locations.lookup['14HWT', 'xmax'], locations.lookup['14HWT', 'ymax'], lty = 3, col = rgb(56, 185, 255, 100, maxColorValue = 255)) # 14HWT
      rect(locations.lookup['14HWB', 'xmin'], locations.lookup['14HWB', 'ymin'], locations.lookup['14HWB', 'xmax'], locations.lookup['14HWB', 'ymax'], lty = 3, col = rgb(56, 72, 255, 100, maxColorValue = 255)) # 14HWT
      segments(locations.lookup['14HWT', 'xmin'], locations.lookup['14HWT', 'ymin'], locations.lookup['14HWB', 'xmin'], locations.lookup['14HWB', 'ymin'], lty = 3) # 14HWB
      segments(locations.lookup['14HWT', 'xmax'], locations.lookup['14HWT', 'ymin'], locations.lookup['14HWB', 'xmax'], locations.lookup['14HWB', 'ymin'], lty = 3) # 14HWB
      segments(locations.lookup['14HWT', 'xmax'], locations.lookup['14HWT', 'ymax'], locations.lookup['14HWB', 'xmax'], locations.lookup['14HWB', 'ymax'], lty = 3) # 14HWB
      segments(locations.lookup['14HWT', 'xmin'], locations.lookup['14HWT', 'ymax'], locations.lookup['14HWB', 'xmin'], locations.lookup['14HWB', 'ymax'], lty = 3) # 14HWB
      rect(locations.lookup['FS14', 'xmin'], locations.lookup['FS14', 'ymin'], locations.lookup['FS14', 'xmax'], locations.lookup['FS14', 'ymax'], lty = 3, col = rgb(255, 130, 5, 100, maxColorValue = 255)) # FS14
      rect(locations.lookup['14EW', 'xmin'], locations.lookup['14ES', 'ymin'], locations.lookup['14EE', 'xmax'], locations.lookup['14EN', 'ymax'], lwd = 2) # cage limits
      
    } else {
      
      #plot(1, 1, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(10, 45), ylim = c(10, 45), type = 'l', col = 'black') 
      rect(locations.lookup['15EW', 'xmin'], locations.lookup['15EW', 'ymin'], locations.lookup['15EW', 'xmax'], locations.lookup['15EW', 'ymax'], lty = 2) # 15EW edge
      rect(locations.lookup['15ES', 'xmin'], locations.lookup['15ES', 'ymin'], locations.lookup['15ES', 'xmax'], locations.lookup['15ES', 'ymax'], lty = 2) # 15ES edge
      rect(locations.lookup['15EE', 'xmin'], locations.lookup['15EE', 'ymin'], locations.lookup['15EE', 'xmax'], locations.lookup['15EE', 'ymax'], lty = 2) # 15EE edge
      rect(locations.lookup['15EN', 'xmin'], locations.lookup['15EN', 'ymin'], locations.lookup['15EN', 'xmax'], locations.lookup['15EN', 'ymax'], lty = 2) # 15EN edge
      rect(locations.lookup['15HET', 'xmin'], locations.lookup['15HET', 'ymin'], locations.lookup['15HET', 'xmax'], locations.lookup['15HET', 'ymax'], lty = 3, col = rgb(56, 185, 255, 100, maxColorValue = 255)) # 15HET
      rect(locations.lookup['15HEB', 'xmin'], locations.lookup['15HEB', 'ymin'], locations.lookup['15HEB', 'xmax'], locations.lookup['15HEB', 'ymax'], lty = 3, col = rgb(56, 72, 255, 100, maxColorValue = 255)) # 15HEB
      segments(locations.lookup['15HET', 'xmin'], locations.lookup['15HET', 'ymin'], locations.lookup['15HEB', 'xmin'], locations.lookup['15HEB', 'ymin'], lty = 3) # 15HEB
      segments(locations.lookup['15HET', 'xmax'], locations.lookup['15HET', 'ymin'], locations.lookup['15HEB', 'xmax'], locations.lookup['15HEB', 'ymin'], lty = 3) # 15HEB
      segments(locations.lookup['15HET', 'xmax'], locations.lookup['15HET', 'ymax'], locations.lookup['15HEB', 'xmax'], locations.lookup['15HEB', 'ymax'], lty = 3) # 15HEB
      segments(locations.lookup['15HET', 'xmin'], locations.lookup['15HET', 'ymax'], locations.lookup['15HEB', 'xmin'], locations.lookup['15HEB', 'ymax'], lty = 3) # 15HEB
      rect(locations.lookup['15HWT', 'xmin'], locations.lookup['15HWT', 'ymin'], locations.lookup['15HWT', 'xmax'], locations.lookup['15HWT', 'ymax'], lty = 3, col = rgb(56, 185, 255, 100, maxColorValue = 255)) # 15HWT
      rect(locations.lookup['15HWB', 'xmin'], locations.lookup['15HWB', 'ymin'], locations.lookup['15HWB', 'xmax'], locations.lookup['15HWB', 'ymax'], lty = 3, col = rgb(56, 72, 255, 100, maxColorValue = 255)) # 15HWT
      segments(locations.lookup['15HWT', 'xmin'], locations.lookup['15HWT', 'ymin'], locations.lookup['15HWB', 'xmin'], locations.lookup['15HWB', 'ymin'], lty = 3) # 15HWB
      segments(locations.lookup['15HWT', 'xmax'], locations.lookup['15HWT', 'ymin'], locations.lookup['15HWB', 'xmax'], locations.lookup['15HWB', 'ymin'], lty = 3) # 15HWB
      segments(locations.lookup['15HWT', 'xmax'], locations.lookup['15HWT', 'ymax'], locations.lookup['15HWB', 'xmax'], locations.lookup['15HWB', 'ymax'], lty = 3) # 15HWB
      segments(locations.lookup['15HWT', 'xmin'], locations.lookup['15HWT', 'ymax'], locations.lookup['15HWB', 'xmin'], locations.lookup['15HWB', 'ymax'], lty = 3) # 15HWB
      rect(locations.lookup['FS15', 'xmin'], locations.lookup['FS15', 'ymin'], locations.lookup['FS15', 'xmax'], locations.lookup['FS15', 'ymax'], lty = 3, col = rgb(255, 130, 5, 100, maxColorValue = 255)) # FS15
      rect(locations.lookup['15EW', 'xmin'], locations.lookup['15ES', 'ymin'], locations.lookup['15EE', 'xmax'], locations.lookup['15EN', 'ymax'], lwd = 2) # cage limits
      
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

# 51. Add day number column to dayfile ----------------------------------------------

add.daynum <- function(startday = 1){
  
 dayfile$day <- as.integer(as.factor(as.Date(dayfile$EchoTime)))
 dayfile$day <- dayfile$day + (startday-1)
 dayfile <<- dayfile
 
}


# 52. Save dataset as dayfiles---------------------------------------------------------

save.dayfiles <- function(){
  
  readline(prompt = 'Make sure the dataset and file list are in chronological order!')
  
 files <-  list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
 days <- unique(dayfile$day)
  if(length(files) != length(days)){
    stop('Number of days in dayfile is not equal to the number of dayfiles in working directory.\n  Change the working directory to the location of the dataset you want to resave.')
  }
  
 dir.create(paste0(getwd(), '/saved'))
 setwd(paste0(getwd(), '/saved'))
 
 for(i in 1:length(files)){
   
   daysub <- subset(dayfile, day == days[i])
   fwrite(daysub, files[i], dateTimeAs = 'write.csv')
   
 }
 
 setwd(workingdir)
  
}



