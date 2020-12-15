#need to have a directory with all the conditions
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
    errorlist[[i]] = errorlist[[i]] + sd(tempmean)              #add the sandard deciation of condition to errorlist
  }
  plot(hapslist, type = "p", col = "blue", lwd = 2, ylim = c(1600,2000), xlab = "Conditions", ylab = "Happiness", main = "Happiness scores among conditions", pch = 16, cex = 2)
  arrows(x0 = 1:length(hapslist), y0 = hapslist - errorlist, x1 = 1:length(hapslist), y1 = hapslist + errorlist, code = 3, angle = 90, length = 0.1)
  axis(side=1,at=c(1:length(folders)),labels=folders)
}


states <- function(directory){
  setwd(directory)                                               #setwd
  folders <<- list.files()                                       #list all folders
  freqs <<- list()
  statelist <<- rep(0, length = length(folders))                 #rep function to make list of length folders to add happiness means from each condition to it 
  errorlist <<- rep(0, length = (length(folders)))               #make empty list for errors
  
  for(i in 1:length(folders)){                                   #iterate through all folders
    setwd(directory)                                             #setwd to directory stated so it can go into another folder
    setwd(folders[[i]])                                          #set wd as the folders iterated through
    files <<- list.files(pattern = "state")                      #list all files in that folder
    
    for(a in 1:length(files)){
      file <- read.csv(files[[a]])                               #choose file 
      file = file[-1]                                            #take first column out
      freqs = freqs + table(as.matrix(file))                     #add
    }
    
  }
  plot(ylim = c(0,100), type = "p", lwd = 2, xlab = "Agent state", ylab = "Proportion of time spent in state")
  title(main = "Time spent in state across each condition")
}
list <- append(list, freqs)
