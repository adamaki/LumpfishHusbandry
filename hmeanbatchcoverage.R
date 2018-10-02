
# 10b. batch mean proportion coverage per hour

hmean.batch.coverage <- function(xmin12 = 40.5, xmax12 = 64.5, ymin12 = 40.5, ymax12 = 64.5, xmin14 = 15, xmax14 = 39, ymin14 = 40.5, ymax14 = 64.5, xmin15 = 15, xmax15 = 39, ymin15 = 15, ymax15 = 39, boxsize = 0.3) {
  
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
    
    
    if(length(unique(dayfile$Period)) == 1) { # check if a fish file is loaded
      
      if(unique(dayfile$PEN) == '12'){
        
        fish.id <- subset(dayfile, PEN == '12')
        
        
        fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
        starttime <- fish.id[1,'EchoTime']-seconds(1)
        nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
        fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
        
        proportion.P12 <- numeric()
        
        for (j in 1:nhours){
          
          hoursub <- fish.id[fish.id$EchoTime > starttime & fish.id$EchoTime < starttime+hours(1),]  
          
          if (nrow(hoursub) > 2){
            
            
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
          #print(nrow(hoursub))
        }
        
        proportion.P12[proportion.P12 == 0] <- NA
        coverage.P12[,as.character(i)] <-  c(mean(proportion.P12, na.rm = T), sd(proportion.P12, na.rm = T))
        coverage.P14[,as.character(i)] <- c('NA', 'NA')
        coverage.P15[,as.character(i)] <- c('NA', 'NA')
        
      }
      
      if(unique(dayfile$PEN) == '14'){
        
        fish.id <- subset(dayfile, PEN == '14')
        
        fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
        starttime <- fish.id[1,'EchoTime']-seconds(1)
        nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
        fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
        
        proportion.P14 <- numeric()
        
        for (j in 1:nhours){
          
          hoursub <- fish.id[fish.id$EchoTime >starttime & fish.id$EchoTime <starttime+hours(1),]   
          
          if (nrow(hoursub) > 2){
            
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
        coverage.P14[,as.character(i)] <- c(mean(proportion.P14, na.rm = T), sd(proportion.P14, na.rm = T))
        coverage.P12[,as.character(i)] <- c('NA', 'NA')
        coverage.P15[,as.character(i)] <- c('NA', 'NA')
        
      }
      
      if(unique(dayfile$PEN) == '15'){
        
        fish.id <- subset(dayfile, PEN == '15')
        
        fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
        starttime <- fish.id[1,'EchoTime']-seconds(1)
        nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
        fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
        
        proportion.P15 <- numeric()
        
        for (j in 1:nhours){
          
          hoursub <- fish.id[fish.id$EchoTime >starttime & fish.id$EchoTime <starttime+hours(1),]   
          
          if (nrow(hoursub) > 2){
            
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
        coverage.P15[,as.character(i)] <- c(mean(proportion.P15, na.rm = T), sd(proportion.P15, na.rm = T))
        coverage.P12[,as.character(i)] <- c('NA', 'NA')
        coverage.P14[,as.character(i)] <- c('NA', 'NA')
        
      }      
      
    }
    
    else { # else assume a dayfile is loaded
      
      fish.id <- subset(dayfile, PEN == '12')
      
      
      fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
      starttime <- fish.id[1,'EchoTime']-seconds(1)
      nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
      fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
      
      proportion.P12 <- numeric()
      
      for (j in 1:nhours){
        
        hoursub <- fish.id[fish.id$EchoTime > starttime & fish.id$EchoTime < starttime+hours(1),]   
        
        if (nrow(hoursub) > 2){
          
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
        
        if (nrow(hoursub) > 2){
          
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
      coverage.P14[,as.character(i)] <- c(mean(proportion.P14, na.rm = T), sd(proportion.P14, na.rm = T))
      
      proportion.P14 <- as.data.frame(proportion.P14)
      proportion.P14$pen <- 14
      names(proportion.P14) <- c('proportion', 'pen')
      
      
      fish.id <- subset(dayfile, PEN == '15')
      
      fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
      starttime <- fish.id[1,'EchoTime']-seconds(1)
      nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
      fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
      
      proportion.P15 <- numeric()
      
      
      for (j in 1:nhours){
        
        hoursub <- fish.id[fish.id$EchoTime >starttime & fish.id$EchoTime <starttime+hours(1),]   
        
        if (nrow(hoursub) > 2){
          
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
      coverage.P15[,as.character(i)] <- c(mean(proportion.P15, na.rm = T), sd(proportion.P15, na.rm = T))
      
      proportion.P15 <- as.data.frame(proportion.P15)
      proportion.P15$pen <- 15
      names(proportion.P15) <- c('proportion', 'pen')      
      
      
      #prop.perhr <- rbind(proportion.P12, proportion.P14, proportion.P15)
      #cov.anova <- aov(proportion~pen, data = prop.perhr)
      #anova.sum <- unlist(summary(cov.anova))
      #anova.list[,as.character(i)] <- anova.sum[9]
      
      
    }  
    
  }  
  
  coverage <- rbind(coverage.P12, coverage.P14, coverage.P15) #, anova.list)
  print(coverage)
  
  write.xlsx(coverage, 'CoverageOutput_hmean.xlsx')
}
