# Delousing efficiency project data processing
# Combine hourly files into day file
# Adam Brooker
# 31st August 2016

# LIST OF FUNCTIONS-------------------------------------------------------------------------------------------------------------------------

# hour.combine(filename) = combines 24 hour files into 1 day file. (filename) is the name of the midnight hour file for the day to be combined.
# fish.extract(fish.id, start.day, no.days) = extract individual fish from coded dayfiles and combine into a single file of a specified number of days.
# batch.fish.extract(start.day, no.days) = extract all individual fish from day files into seperate files for a specific time period
# batch.fish.extract.recoded(start.day, no.days) = extract all individual fish from recoded day files into seperate files for a specific time period


library(stringr)


# ENTER YOUR VARIABLES HERE-----------------------------------------------------------------------------------------------------------------

workingdir = "G:/Data/2018 Lumpfish Husbandry/Data processing/4. Hourly CSV" # change to location of data
#file.name = "run2_LLF16S1001990000POS.csv" # change to name of midnight hour file for day to be combined
setwd(workingdir)



#new dayfile classes
dayfile.classes = c('NULL', 'numeric', 'factor', 'factor', 'POSIXct', 'double', 'double', 
                    'double', 'double', 'double', 'double', 'double', 'double', 'double', 'double', 'double', 'factor',
                    'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                    'double', 'double', 'double', 'double', 'double', 'double', 'double',
                    'double', 'double', 'double', 'double', 'double', 'double', 'double',
                    'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                    'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                    'factor', 'factor', 'double', 'double', 'double', 'double', 'double', 
                    'double', 'double', 'double', 'double', 'double', 'double', 'double'
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


# FUNCTIONS--------------------------------------------------------------------------------------------------------------------------------

# combine hour files into 1 day file

hour.combine <- function(daynum){ 

files <- list.files(path = workingdir, pattern = daynum, all.files = FALSE, recursive = FALSE)

files <- subset(files, str_sub(files, -14, -12) == daynum) # subset filenames with day number in correct place in filename

file1 <- read.csv(files[[1]], header = TRUE, sep = ",", colClasses = c(rep('character', 19)))

for (n in 2:length(files)){
  
  file2 <- read.csv(files[[n]], header = TRUE, sep = ",", colClasses = c(rep('character', 19)))
  file1 <- rbind(file1, file2)
  
}

write.csv(file1, file = sub(str_sub(files[[1]], -11, -1), "_day.csv", files[[1]], ignore.case = FALSE, fixed = T))

}



# extracts single fish id and combines multiple days into one file

fish.extract <- function(fish.id, start.day, no.days) 
  {
  

files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
day1 <- grep(paste0('^..............', start.day, '_day_coded.csv'), files)
end.day <- day1+(no.days-1)
dayfile.loc <- files[[grep(paste0('^..............', start.day, '_day_coded.csv'), files)]]

dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = c('character', 'numeric', 'factor', 'factor', 'POSIXct', 'double', 'double', 
                                                                          'double', 'double', 'double', 'double', 'double', 'double', 'factor',
                                                                          'factor', 'factor', 'factor', 'factor', 'integer', 'factor', 'factor',
                                                                          'double', 'double', 'double', 'double', 'double', 'double', 'double',
                                                                          'double', 'double', 'double', 'double', 'double', 'double', 'double',
                                                                          'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                                                                          'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                                                                          'factor', 'factor', 'double', 'double', 'double', 'double', 'double', 
                                                                          'double', 'double', 'double', 'double', 'double', 'double', 'double' 
                                                                          )) #read data into table
dayfile[,1] <- NULL

dayfile <- dayfile[which(dayfile$Period == fish.id), ]
  
for (i in day1+1:end.day-1) {
  dayfile2 <- read.csv(files[[i]], header = TRUE, sep = ",", colClasses = c('character', 'numeric', 'factor', 'factor', 'POSIXct', 'double', 'double', 
                                                                            'double', 'double', 'double', 'double', 'double', 'double', 'factor',
                                                                            'factor', 'factor', 'factor', 'factor', 'integer', 'factor', 'factor',
                                                                            'double', 'double', 'double', 'double', 'double', 'double', 'double',
                                                                            'double', 'double', 'double', 'double', 'double', 'double', 'double',
                                                                            'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                                                                            'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                                                                            'factor', 'factor', 'double', 'double', 'double', 'double', 'double', 
                                                                            'double', 'double', 'double', 'double', 'double', 'double', 'double' 
                                                                            )) #read data into table
  dayfile2[,1] <- NULL
  dayfile2 <- dayfile2[which(dayfile2$Period == fish.id), ]
  dayfile <- rbind(dayfile, dayfile2) 
} 
  
write.csv(dayfile, file = sub(paste0(start.day, '_day_coded.csv'), paste0(fish.id, '_fish_coded.csv'), dayfile.loc, ignore.case = FALSE, fixed = T)) #write output to file  
  
}




# extract all individual fish from day files into seperate files for a specific time period

batch.fish.extract <- function(start.day, no.days) {
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  day1 <- grep(paste0('^..............', start.day, '_day_coded.csv'), files)
  end.day <- day1+(no.days-1)
  dayfile.loc <- files[[grep(paste0('^..............', start.day, '_day_coded.csv'), files)]]
  
 
  
  dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = dayfile.classes)
  
  fish.ids <- unique(dayfile$Period)
  
  for (i in 1:length(fish.ids)){
    assign(paste0('fish.id_', as.character(fish.ids[[i]])), dayfile[which(dayfile$Period == fish.ids[[i]]), ])
  }
  
  
  for (i in (day1+1):(end.day-1)) {
 

    dayfile2 <- read.csv(files[[i]], header = TRUE, sep = ",", colClasses = dayfile.classes)   
    
    for (i in 1:length(fish.ids)){
      
      assign(paste0(as.character(fish.ids[[i]]), '_2'), dayfile2[which(dayfile2$Period == fish.ids[[i]]), ])
      assign(paste0('fish.id_', as.character(fish.ids[[i]])),  rbind(get(paste0('fish.id_', as.character(fish.ids[[i]]))), get(paste0(as.character(fish.ids[[i]]), '_2'))))
      
    }
    
    
  } 
  
  for (i in 1:length(fish.ids)){
    write.csv(get(paste0('fish.id_', as.character(fish.ids[[i]]))), file = sub(paste0(start.day, '_day_coded.csv'), paste0(fish.ids[[i]], '_fish_coded.csv'), dayfile.loc, ignore.case = FALSE, fixed = T))
  }
  
  remove(list = ls(pattern = 'fish.id_'))
  remove(list = ls(pattern = '_2'))
  
}



# extract all individual fish from day files into seperate files for a specific time period

batch.fish.extract.recoded <- function(start.day, no.days) {
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  day1 <- grep(paste0('^..............', start.day, '_day_recoded.csv'), files)
  end.day <- day1+(no.days-1)
  dayfile.loc <- files[[grep(paste0('^..............', start.day, '_day_recoded.csv'), files)]]
  
  dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = c('NULL', 'integer', 'character', 'character', 'POSIXct', 'double', 'double', 
                                                                            'double', 'double', 'double', 'double', 'double', 'double', 'character',
                                                                            'character', 'character', 'character', 'character', 'character', 'character', 'character',
                                                                            'double', 'double', 'double', 'double', 'double', 'double', 'double',
                                                                            'double', 'double', 'double', 'double', 'double', 'double', 'double',
                                                                            'character', 'character', 'character', 'character', 'character', 'character', 'character', 
                                                                            'character', 'character', 'character', 'character', 'character', 'character', 'character', 
                                                                            'character', 'character', 'double', 'double', 'double', 'double', 'double', 
                                                                            'double', 'double', 'double', 'double', 'double', 'double', 'double' 
  )) #read data into table
  
  #dayfile[,1] <- NULL
  
  fish.ids <- unique(dayfile$Period)
  
  for (i in 1:length(fish.ids)){
    assign(paste0('fish.id_', as.character(fish.ids[[i]])), dayfile[which(dayfile$Period == fish.ids[[i]]), ])
  }
  
  
  for (i in (day1+1):(end.day)) {
    dayfile2 <- read.csv(files[[i]], header = TRUE, sep = ",", colClasses = c('NULL', 'integer', 'character', 'character', 'POSIXct', 'double', 'double', 
                                                                              'double', 'double', 'double', 'double', 'double', 'double', 'character',
                                                                              'character', 'character', 'character', 'character', 'character', 'character', 'character',
                                                                              'double', 'double', 'double', 'double', 'double', 'double', 'double',
                                                                              'double', 'double', 'double', 'double', 'double', 'double', 'double',
                                                                              'character', 'character', 'character', 'character', 'character', 'character', 'character', 
                                                                              'character', 'character', 'character', 'character', 'character', 'character', 'character', 
                                                                              'character', 'character', 'double', 'double', 'double', 'double', 'double', 
                                                                              'double', 'double', 'double', 'double', 'double', 'double', 'double' 
    )) #read data into table
    #dayfile2[,1] <- NULL
    
    for (i in 1:length(fish.ids)){
      
      assign(paste0(as.character(fish.ids[[i]]), '_2'), dayfile2[which(dayfile2$Period == fish.ids[[i]]), ])
      assign(paste0('fish.id_', as.character(fish.ids[[i]])),  rbind(get(paste0('fish.id_', as.character(fish.ids[[i]]))), get(paste0(as.character(fish.ids[[i]]), '_2'))))
      
    }
    
    
  } 
  
  for (i in 1:length(fish.ids)){
    write.csv(get(paste0('fish.id_', as.character(fish.ids[[i]]))), file = sub(paste0(start.day, '_day_recoded.csv'), paste0(fish.ids[[i]], '_fish_recoded.csv'), dayfile.loc, ignore.case = FALSE, fixed = T))
  }
  
  remove(list = ls(pattern = 'fish.id_'))
  remove(list = ls(pattern = '_2'))
  
}

