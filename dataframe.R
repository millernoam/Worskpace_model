dir = "/Volumes/External SSD/Jovan sims/Jovan sims/A3"
library(questionr)
dataframe <- function(dir){
  setwd(dir)                                               #setwd
  folders <<- list.files()                                       #list all folders
  
  for(i in 1:length(folders)){                                   #iterate through all folders
    setwd(dir)                                             #setwd to directory stated so it can go into another folder
    setwd(folders[[i]])                                          #set wd as the folders iterated through
    hapsfiles <<- list.files(pattern = "haps")                    #list all files in that folder
    persfiles <<- list.files(pattern = "pers")
    statefiles <<- list.files(pattern = "state")
    attsfiles <<- list.files(pattern = "atts")
    
    for(p in 1:length(persfiles)){                            
      file <- read.csv(persfiles[[p]])                               #choose file from pers files
      file = file[-1]                                                #take first column out
      
      
      for(r in 1:nrow(file)){
        df1[nrow(na.omit(df1[2]))+1,2] <- r                          #take note of agent
        df1[nrow(na.omit(df1[1]))+1,1] <- p                          #take note of replication
        for(c in 1:5){
          
          df1[nrow(na.omit(df1[c+2]))+1,c+2] = file[r,c]                 
          
        }
      }
    }  
    for(h in 1:length(hapsfiles)){
      file <- read.csv(hapsfiles[[h]])                               #choose file from haps files
      file = file[-1]                                            #take first column out
      file = file[-1,]                                           #omit first row b/c there is an unever number of rows (2501)
      
      for(d in 1:num_days){
        dayhaps <<- file[tsperday * d,]
        
        
        
        for(a in 1:length(dayhaps)){
          df2[nrow(na.omit(df2[4]))+1,4] = dayhaps[a]                
          df2[nrow(na.omit(df2[2]))+1,2] = a                          #take note of agent
          df2[nrow(na.omit(df2[3]))+1,3] = d                          #take note of days
          df2[nrow(na.omit(df2[1]))+1,1] = h                          #take note of replication
        }
      } 
    } 
    for(s in 1:length(statefiles)){
      file = read.csv(statefiles[[s]])                               #choose file from state files
      file = file[-1]                                                 #read in the file
      
      for(f in 1:ncol(file)){                                        #iterate over the columns/agents
        agentstates <<- file[[f]]                                             #put agent's column in a vector
        for(c in 1:num_days){
          if(c==2){            
            daystates <<- freq(agentstates[tsperday:(tsperday*c)])[[1]]      #get states for agent per day
           }
          if(c>=3){
            daystates <<- freq(agentstates[(tsperday*(c-1)):(tsperday*c)])[[1]]
          }else{
            daystates <<- freq(agentstates[1:tsperday])[[1]]
          }
          
          if(length(daystates) == 5){                                     #if relax is not included in the frequencies: do this
            df2[nrow(na.omit(df2[5]))+1,5] = daystates[3]                #update eating
            df2[nrow(na.omit(df2[6]))+1,6] = daystates[5]                #update social
            df2[nrow(na.omit(df2[7]))+1,7] = daystates[2]                #update working
            df2[nrow(na.omit(df2[8]))+1,8] = daystates[4]                #update washroom
            df2[nrow(na.omit(df2[9]))+1,9] = 0                           #add 0 for relaxing
            df2[nrow(na.omit(df2[10]))+1,10] = daystates[1]              #update walking
          }
          else{                                                         #relax is included in these frequencies
            df2[nrow(na.omit(df2[5]))+1,5] = daystates[3]                #update eating
            df2[nrow(na.omit(df2[6]))+1,6] = daystates[6]                #update social
            df2[nrow(na.omit(df2[7]))+1,7] = daystates[2]                #update working
            df2[nrow(na.omit(df2[8]))+1,8] = daystates[5]                #update washroom
            df2[nrow(na.omit(df2[9]))+1,9] = daystates[4]                #update relaxing
            df2[nrow(na.omit(df2[10]))+1,10] = daystates[1]              #update walking
          }

        }
      }  
    }
    
  }
}
# make df1 for replication and personality since it does not go in a day formation
library(readxl)
setwd("/Volumes/External SSD/Jovan sims/Jovan sims")
chillspots <- (read_excel("network node codebook n2.xlsx",3)[[3]])
df1 <- data.frame(matrix(ncol = 7 + length(chillspots)))
columns1 <- c("Replication", "Agent", "Foodiness", "Sociability", "Work Ethic", "Bladder Size", "Workstation") 
colnames(df1) <- c(columns1,chillspots)


#make df2 for everything else
num_days <- (read_excel("network node codebook n2.xlsx",5)[[3]])
tsperday <- (read_excel("network node codebook n2.xlsx",5)[[1]])
chillspots <- na.omit(chillspots)
df2 <- data.frame(matrix(ncol = 10))
columns2 <- c("Replication","Agent","Day","Happiness","Eating","Social","Working","Washroom","Relaxing","Walking") 
colnames(df2) <- c(columns2)
