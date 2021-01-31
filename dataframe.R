directory = "C:/Users/Jovan/Documents/Jovan sims/Jovan sims/A3"
dataframe <- function(directory){
  setwd(directory)                                               #setwd
  folders <<- list.files()                                       #list all folders
  hapslist <<- rep(list(), length = length(folders))
  perslist <<- rep(list(), length = length(folders))
  list5 <- rep(list(), length = 5)
  
  for(i in 1:length(folders)){                                   #iterate through all folders
    setwd(directory)                                             #setwd to directory stated so it can go into another folder
    setwd(folders[[i]])                                          #set wd as the folders iterated through
    hapsfiles <- list.files(pattern = "haps")                    #list all files in that folder
    persfiles <- list.files(pattern = "pers")
    listhapsfiles <- rep(list(), length = length(hapsfiles))
    listpersfiles <- rep(list(), length = length(persfiles))
    hapslist[[i]] <- append(hapslist[[i]], listhapsfiles)
    perslist[[i]] <- append(perslist[[i]], listpersfiles)
    
    for(p in 1:length(persfiles)){                            
      file <- read.csv(persfiles[[p]])                               #choose file from pers files
      file = file[-1]                                                #take first column out
      perslist[[i]][[p]] <- append(perslist[[i]][[p]], list5)
      
      for(c in 1:5){
         perslist[[i]][[p]][[c]] <- append(perslist[[i]][[p]][[c]], file[,c])

      }
    }
      for(h in 1:length(hapsfiles)){
        file <- read.csv(hapsfiles[[h]])                               #choose file from haps files
        file = file[-1]                                            #take first column out
        finalhaps <- file[nrow(file),]
        
        for(a in 1:length(finalhaps)){
          hapslist[[i]][[h]] <- append(hapslist[[i]][[h]], finalhaps[[a]])
      } 
    }
  }
}
