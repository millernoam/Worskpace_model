#need to have a directory with all the conditions
library(plotrix)
happiness <- function(directory){
  setwd(directory)                                               #setwd
  folders <<- list.files()                                       #list all folders
  hapslist <<- rep(0, length = length(folders))                  #rep function to make list of length folders to add happiness means from each condition to it 
  errorlist <<- rep(0, length = (length(folders)))               #make empty list for errors
  
  for(i in 1:length(folders)){                                   #iterate through all folders
    tempmean <<- c()                                             #create empty vector for temp mean
    setwd(directory)                                             #setwd to directory stated so it can go into another folder
    setwd(folders[[i]])                                          #set wd as the folders iterated through
    files <<- list.files(pattern = "haps")                       #list all files in that folder

    for(a in 1:length(files)){
      file <- read.csv(files[[a]])                               #choose file 
      file = file[-1]                                            #take first column out
      tempmean <- append(tempmean, rowMeans(file[nrow(file),]))  #append each row mean to this vector
      
    }
    hapslist[[i]] = hapslist[[i]] + mean(tempmean)              #add the mean of the condition to hapslist
    errorlist[[i]] = errorlist[[i]] + std.error(tempmean)              #add the sandard deciation of condition to errorlist
  }
  plot(hapslist, type = "p", col = "blue", lwd = 2, ylim = c(1600,2000), xlab = "Conditions", ylab = "Happiness", main = "Happiness scores among conditions", pch = 16, cex = 2, xaxt = "n")
  arrows(x0 = 1:length(hapslist), y0 = hapslist - errorlist, x1 = 1:length(hapslist), y1 = hapslist + errorlist, code = 3, angle = 90, length = 0.1)
  axis(side=1,at=c(1:length(folders)),labels=folders)
}

library(questionr)
library(plotrix)
directory = "C:/Users/Jovan/Documents/Jovan sims/Jovan sims/N30"
states <- function(directory){
  setwd(directory)                                               #setwd
  folders <<- list.files()                                       #list all folders
  finallist <<- list()
  statelist <<- list()
  Walking <<- list()
  Working <<- list()
  Eating <<- list()
  Relaxing <<- list()
  Washroom <<- list()
  Socializing <<- list()
  Walkingmean <<- c()
  Workingmean <<- c()
  Eatingmean <<- c()
  Relaxingmean <<- c()
  Washroommean <<- c()
  Socializingmean <<- c()
  Walkingsem <<- c()
  Workingsem <<- c()
  Eatingsem <<- c()
  Relaxingsem <<- c()
  Washroomsem <<- c()
  Socializingsem <<- c()
  
  for(i in 1:length(folders)){                                   #iterate through all folders
    setwd(directory)                                             #setwd to directory stated so it can go into another folder
    setwd(folders[[i]])                                          #set wd as the folders iterated through
    filenames <<- list.files(pattern = "state")                      #list all files in that folder
    
    for(s in 1:length(filenames)){
      
      file <- read.csv(filenames[s])
      file <- file[-1]
      
      if(2 %in% as.matrix(file)){
        Walking <- append(Walking, freq(as.matrix(file))[[2]][1])
        Working <- append(Working, freq(as.matrix(file))[[2]][2])
        Eating <- append(Eating, freq(as.matrix(file))[[2]][3])
        Relaxing <- append(Relaxing, freq(as.matrix(file))[[2]][4])
        Washroom <- append(Washroom, freq(as.matrix(file))[[2]][5])
        Socializing <- append(Socializing, freq(as.matrix(file))[[2]][6])
      }else{
        Walking <- append(Walking, freq(as.matrix(file))[[2]][1])
        Working <- append(Working, freq(as.matrix(file))[[2]][2])
        Eating <- append(Eating, freq(as.matrix(file))[[2]][3])
        Washroom <- append(Washroom, freq(as.matrix(file))[[2]][4])
        Socializing <- append(Socializing, freq(as.matrix(file))[[2]][5])
      }
    }        
    
    Walkingmean <- append(Walkingmean, mean(as.numeric(Walking)))
    Workingmean <- append(Workingmean, mean(as.numeric(Working)))
    Eatingmean <- append(Eatingmean, mean(as.numeric(Eating)))
    Relaxingmean <- append(Relaxingmean, mean(as.numeric(Relaxing)))
    Washroommean <- append(Washroommean, mean(as.numeric(Washroom)))
    Socializingmean <- append(Socializingmean, mean(as.numeric(Socializing)))
    Walkingsem <- append(Walkingsem, std.error(as.matrix(Walking)))
    Workingsem <- append(Workingsem, std.error(as.matrix(Working)))
    Eatingsem <- append(Eatingsem, std.error(as.matrix(Eating)))
    Relaxingsem <- append(Relaxingsem, std.error(as.matrix(Relaxing)))
    Washroomsem <- append(Washroomsem, std.error(as.matrix(Washroom)))
    Socializingsem <- append(Socializingsem, std.error(as.matrix(Socializing)))
    
    files <- lapply(filenames, read.csv)                              #choose file 
    files <- do.call(rbind, files)
    files = files[-1]                                            #take first column out
    
    files <- do.call(rbind, as.data.frame(files))
    statelist <- append(statelist, freq(as.matrix(files))[2])   ###***get frequencies for individual files and append them to a list for each condition (seperate from all of this just to get the error) -> get errors for each state per condion
    
  }
  
  
  plot(as.matrix(Workingmean), ylim = c(20,40), type = "p", lwd = 4, xlab = "Condition", ylab = "Proportion of time spent working (%)", xaxt = "n")
  title(main = "Time spent working across each condition")
  arrows(x0 = 1:length(Workingmean), y0 = Workingmean - Workingsem, x1 = 1:length(Workingmean), y1 = Workingmean + Workingsem, code = 3, angle = 90, length = 0.1)
  axis(side=1,at=c(1:length(folders)),labels=folders)
  
  plot(as.matrix(Walkingmean), ylim = c(20,40), type = "p", lwd = 4, xlab = "Condition", ylab = "Proportion of time spent walking (%)", xaxt = "n")
  title(main = "Time spent walking across each condition")
  arrows(x0 = 1:length(Walkingmean), y0 = Walkingmean - Walkingsem, x1 = 1:length(Walkingmean), y1 = Walkingmean + Walkingsem, code = 3, angle = 90, length = 0.1)
  axis(side=1,at=c(1:length(folders)),labels=folders)
  
  plot(as.matrix(Washroommean), ylim = c(0,2), type = "p", lwd = 4, xlab = "Condition", ylab = "Proportion of time spent washroom (%)", xaxt = "n")
  title(main = "Time spent washroom across each condition")
  arrows(x0 = 1:length(Washroommean), y0 = Washroommean - Washroomsem, x1 = 1:length(Washroommean), y1 = Washroommean + Washroomsem, code = 3, angle = 90, length = 0.1)
  axis(side=1,at=c(1:length(folders)),labels=folders)
  
  plot(as.matrix(Socializingmean), ylim = c(0,20), type = "p", lwd = 4, xlab = "Condition", ylab = "Proportion of time spent socializing (%)", xaxt = "n")
  title(main = "Time spent socializing across each condition")
  arrows(x0 = 1:length(Socializingmean), y0 = Socializingmean - Socializingsem, x1 = 1:length(Socializingmean), y1 = Socializingmean + Socializingsem, code = 3, angle = 90, length = 0.1)
  axis(side=1,at=c(1:length(folders)),labels=folders)
  
  plot(as.matrix(Relaxingmean), ylim = c(0,2), type = "p", lwd = 4, xlab = "Condition", ylab = "Proportion of time spent relaxing (%)", xaxt = "n")
  title(main = "Time spent relaxing across each condition")
  arrows(x0 = 1:length(Relaxingmean), y0 = Relaxingmean - Relaxingsem, x1 = 1:length(Relaxingmean), y1 = Relaxingmean + Relaxingsem, code = 3, angle = 90, length = 0.1)
  axis(side=1,at=c(1:length(folders)),labels=folders)
  
  plot(as.matrix(Eatingmean), ylim = c(20,40), type = "p", lwd = 4, xlab = "Condition", ylab = "Proportion of time spent eating (%)", xaxt = "n")
  title(main = "Time spent eating across each condition")
  arrows(x0 = 1:length(Eatingmean), y0 = Eatingmean - Eatingsem, x1 = 1:length(Eatingmean), y1 = Eatingmean + Eatingsem, code = 3, angle = 90, length = 0.1)
  axis(side=1,at=c(1:length(folders)),labels=folders)
}


#Chill spots ranked by number of visitors in each condition
library(plotrix)
library(questionr)
directory = "C:/Users/Jovan/Documents/Jovan sims/Jovan sims/A3"
bestsr <- function(directory){
  chillspots <- c(18,19,21,22,32,33,34)     ###take from excel file
  spot1 <<- list()
  spot2 <<- list()
  spot3 <<- list()
  spot4 <<- list()
  spot5 <<- list()
  spot6 <<- list()
  spot7 <<- list()
  spot1mean <<- c()
  spot2mean <<- c()
  spot3mean <<- c()
  spot4mean <<- c()
  spot5mean <<- c()
  spot6mean <<- c()
  spot7mean <<- c()
  spot1sem <<- c()
  spot2sem <<- c()
  spot3sem <<- c()
  spot4sem <<- c()
  spot5sem <<- c()
  spot6sem <<- c()
  spot7sem <<- c()
                                                                 #chill_spots <- na.omit(c(read_excel("network node codebook.xlsx",3)[[3]]))     #chill spots
  setwd(directory)                                               #setwd
  folders <<- list.files()                                       #list all folders

    for(i in 1:length(folders)){                                   #iterate through all folders

    setwd(directory)                                             #setwd to directory stated so it can go into another folder
    setwd(folders[[i]])                                          #set wd as the folders iterated through
    filenames <<- list.files(pattern = "node")                  #list all files in that folder
    
    for(s in 1:length(filenames)){
      file <- read.csv(filenames[s])
      file <- file[-1]
      
      spot1 <- append(spot1, freq(as.matrix(file))[[2]][which(rownames(lis) %in% chillspots[[1]])])
      spot2 <- append(spot2, freq(as.matrix(file))[[2]][which(rownames(lis) %in% chillspots[[2]])])
      spot3 <- append(spot3, freq(as.matrix(file))[[2]][which(rownames(lis) %in% chillspots[[3]])])
      spot4 <- append(spot4, freq(as.matrix(file))[[2]][which(rownames(lis) %in% chillspots[[4]])])
      spot5 <- append(spot5, freq(as.matrix(file))[[2]][which(rownames(lis) %in% chillspots[[5]])])    #problem here because there are different nodes used in each repetition
      spot6 <- append(spot6, freq(as.matrix(file))[[2]][which(rownames(lis) %in% chillspots[[6]])])
      spot7 <- append(spot7, freq(as.matrix(file))[[2]][which(rownames(lis) %in% chillspots[[7]])])
      
    }
    spot1mean <- append(spot1mean, mean(as.numeric(spot1)))
    spot2mean <- append(spot2mean, mean(as.numeric(spot2)))
    spot3mean <- append(spot3mean, mean(as.numeric(spot3)))
    spot4mean <- append(spot4mean, mean(as.numeric(spot4)))
    spot5mean <- append(spot5mean, mean(as.numeric(spot5)))
    spot6mean <- append(spot6mean, mean(as.numeric(spot6)))
    spot7mean <- append(spot7mean, mean(as.numeric(spot7)))
    spot1sem <- append(spot1sem, std.error(as.numeric(spot1)))
    spot2sem <- append(spot2sem, std.error(as.numeric(spot2)))
    spot3sem <- append(spot3sem, std.error(as.numeric(spot3)))
    spot4sem <- append(spot4sem, std.error(as.numeric(spot4)))
    spot5sem <- append(spot5sem, std.error(as.numeric(spot5)))
    spot6sem <- append(spot6sem, std.error(as.numeric(spot6)))
    spot7sem <- append(spot7sem, std.error(as.numeric(spot7)))
    }
  plot(as.matrix(spot1mean), ylim = c(20,40), type = "p", lwd = 4, xlab = "Condition", ylab = "Proportion of time spent at SRspot1", xaxt = "n")
  title(main = "Time spent working across each condition")
  arrows(x0 = 1:length(Workingmean), y0 = Workingmean - Workingsem, x1 = 1:length(Workingmean), y1 = Workingmean + Workingsem, code = 3, angle = 90, length = 0.1)
  axis(side=1,at=c(1:length(folders)),labels=folders)
  
  plot(as.matrix(Workingmean), ylim = c(20,40), type = "p", lwd = 4, xlab = "Condition", ylab = "Proportion of time spent working (%)", xaxt = "n")
  title(main = "Time spent working across each condition")
  arrows(x0 = 1:length(Workingmean), y0 = Workingmean - Workingsem, x1 = 1:length(Workingmean), y1 = Workingmean + Workingsem, code = 3, angle = 90, length = 0.1)
  axis(side=1,at=c(1:length(folders)),labels=folders)
  
  plot(as.matrix(Workingmean), ylim = c(20,40), type = "p", lwd = 4, xlab = "Condition", ylab = "Proportion of time spent working (%)", xaxt = "n")
  title(main = "Time spent working across each condition")
  arrows(x0 = 1:length(Workingmean), y0 = Workingmean - Workingsem, x1 = 1:length(Workingmean), y1 = Workingmean + Workingsem, code = 3, angle = 90, length = 0.1)
  axis(side=1,at=c(1:length(folders)),labels=folders)
  
  plot(as.matrix(Workingmean), ylim = c(20,40), type = "p", lwd = 4, xlab = "Condition", ylab = "Proportion of time spent working (%)", xaxt = "n")
  title(main = "Time spent working across each condition")
  arrows(x0 = 1:length(Workingmean), y0 = Workingmean - Workingsem, x1 = 1:length(Workingmean), y1 = Workingmean + Workingsem, code = 3, angle = 90, length = 0.1)
  axis(side=1,at=c(1:length(folders)),labels=folders)
  
  plot(as.matrix(Workingmean), ylim = c(20,40), type = "p", lwd = 4, xlab = "Condition", ylab = "Proportion of time spent working (%)", xaxt = "n")
  title(main = "Time spent working across each condition")
  arrows(x0 = 1:length(Workingmean), y0 = Workingmean - Workingsem, x1 = 1:length(Workingmean), y1 = Workingmean + Workingsem, code = 3, angle = 90, length = 0.1)
  axis(side=1,at=c(1:length(folders)),labels=folders)
  
  plot(as.matrix(Workingmean), ylim = c(20,40), type = "p", lwd = 4, xlab = "Condition", ylab = "Proportion of time spent working (%)", xaxt = "n")
  title(main = "Time spent working across each condition")
  arrows(x0 = 1:length(Workingmean), y0 = Workingmean - Workingsem, x1 = 1:length(Workingmean), y1 = Workingmean + Workingsem, code = 3, angle = 90, length = 0.1)
  axis(side=1,at=c(1:length(folders)),labels=folders)
  
  plot(as.matrix(Workingmean), ylim = c(20,40), type = "p", lwd = 4, xlab = "Condition", ylab = "Proportion of time spent working (%)", xaxt = "n")
  title(main = "Time spent working across each condition")
  arrows(x0 = 1:length(Workingmean), y0 = Workingmean - Workingsem, x1 = 1:length(Workingmean), y1 = Workingmean + Workingsem, code = 3, angle = 90, length = 0.1)
  axis(side=1,at=c(1:length(folders)),labels=folders)
}

table(as.matrix(files))[34]
chillspots <- c(18,19,21,22,32,33,34)
lis <- freq(as.matrix(file))
which(rownames(lis) %in% chillspots[[1]])
lis[18]
lis[[2]][which(rownames(lis) %in% chillspots[[1]])]
