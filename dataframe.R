# make dataframe for replication and personality since it does not go in a day formation
library(checkmate)
library(readxl)
library(readr)
library(xlsx)
library(dplyr)
library(questionr)
setwd("/Volumes/External SSD/Jovan sims/Jovan sims")                                     #set directory which should have your codebook handy
chillspots <<- (read_excel("network node codebook n2.xlsx",3)[[3]])                      #extract chill spots from the codebook file
chillspots <<- na.omit(chillspots)                                                       #omit NA's if there are any because columns in that sheet most likely have different lengths
numagents = (c(read_excel("network node codebook n2.xlsx",5)[[2]]))                                                #number of agents
num_days <<- (read_excel("network node codebook n2.xlsx",5)[[3]])                        #extract number of days from the codebook
tsperday <<- (read_excel("network node codebook n2.xlsx",5)[[1]])                        #extract timesteps per day from the codebook
columns <<- c("Replication", "Agent", "Foodiness", "Sociability", "Work Ethic", "Bladder Size", "Workstation","Walking","Working","Eating","Relaxing","Washroom","Socializing","Happiness")                                                  #insert column names above into data frame

dir = "/Volumes/External SSD/Jovan sims/Jovan sims/A3/N10_A3" 
setwd(dir)
dataframe <- function(dir){
  df = data.frame(matrix(ncol = 14))                                #create data frame of length columns(basic) and however many chill spots chosen in the codebook
  colnames(df) <- c(columns)  
  dfsr <- data.frame(matrix(ncol = length(chillspots)))
  colnames(dfsr) <- c(chillspots)
  hapsfiles <<- list.files(pattern = "haps")                    #list all happiness files
  persfiles <<- list.files(pattern = "pers")                    #list all personality files
  statefiles <<- list.files(pattern = "state")                  #list all state history files
  attsfiles <<- list.files(pattern = "atts")                    #list all s/r spot attractiveness files
  

  for(p in 1:length(persfiles)){                            
    file <<- read.csv(persfiles[[p]])                               #choose file from pers files
    file <<- file[-1]                                                #take first column out
    
    
    for(r in 1:nrow(file)){
      df[nrow(na.omit(df[2]))+1,2] = r                          #take note of agent
      df[nrow(na.omit(df[1]))+1,1] = p                          #take note of replication
      for(c in 1:5){
        
        df[nrow(na.omit(df[c+2]))+1,c+2] = file[r,c]          #add personality of agents to data frame               
        
      }
    }
  }  
  for(h in 1:length(hapsfiles)){
    file <<- read.csv(hapsfiles[[h]])                            #choose file from haps files
    file <<- file[-1]                                            #take first column out
    file <<- file[-1,]                                           #omit first row b/c there is an unever number of rows (2501)

    finalhaps = file[tsperday * num_days,]                     #put final row in vector
      
    for(a in 1:length(finalhaps)){                             #iterate over columns (agents) of finalhaps
      df[nrow(na.omit(df[14]))+1,14] = finalhaps[a]                
    }
  } 
  for(s in 1:length(statefiles)){
    file <<- read.csv(statefiles[[s]])                               #choose file from state files
    file <<- file[-1]                                                 #read in the file
    
    for(f in 1:ncol(file)){                                        #iterate over the columns/agents
      agentstates <<- freq(file[[f]])                           #put agent's column in a vector
      
      if(length(rownames(agentstates)) == 5){                                     #if relax is not included in the frequencies: do this
        df[nrow(na.omit(df[8]))+1,8] = agentstates[[1]][1]                #update walking
        df[nrow(na.omit(df[9]))+1,9] = agentstates[[1]][2]                #update working
        df[nrow(na.omit(df[10]))+1,10] = agentstates[[1]][3]                #update eating
        df[nrow(na.omit(df[11]))+1,11] = 0                #add 0 for relaxing
        df[nrow(na.omit(df[12]))+1,12] = agentstates[[1]][4]                           #update washroom
        df[nrow(na.omit(df[13]))+1,13] = agentstates[[1]][5]              #update social
      }
      else{                                                         #relax is included in these frequencies
        for(y in 1:6){
        df[nrow(na.omit(df[(y+7)]))+1,(y+7)] = agentstates[[1]][y]                #update walking
          }
        }
      }  
    }
for(o in 1:length(attsfiles)){
  file <<- read.csv(attsfiles[[o]])                               #choose file from atts files
  dftemp <- data.frame(matrix(ncol = length(chillspots), nrow = numagents))
  colnames(dftemp) <- c(chillspots)
  
  for(j in length(file[[4]]):1){                            #iterate backwards on the column which displays the sr spot
    spot <- which(chillspots %in% file[[4]][j])             #take note of index of spot
    agent <- file[j,3]                                      #take note of agent at spot
    if(is.na(dftemp[agent,spot]) == T){                      #is the cell with the agent and the chillspot empty?
      dftemp[agent,spot] = file[j,6]                         #if so, note the final rating of that spot for that agent
    }
    if(anyMissing(as.matrix(dftemp)) == F){                  #as soon as all the cells are filled, the loop ends
      break
    }
  }
  dfsr <<- bind_rows(dftemp, dfsr)                 #append the data to its respective column in this data frame and keep adding
  
}
  dfsr = dfsr[-1,]
  df <- bind_cols(df, dfsr) 
  setwd("/Volumes/Macintosh HD/Users/jovanpoposki/Documents")
  write.xlsx(df, file="Data Frame.xlsx")
}
