#This script runs the main parts of the model

#States: 0 = Working, 1=Eating; 2=Relax, 3=Washroom, 4=Socialize, -10=walking
#Nodes: ws_spots = working, sr_spots = social/relax
#Personality: Hunger, Sociability, Work Ethic, Bladder_cap

# initialize the model  ------------------------------
initialize()  

# run the model -------------------------------
for (d in 1:num_days){            #iterate over days
  initializeday()
  
  for (t in 1:total_time){         # iterate over the day
    
    dosr(d, t)  #manage social dynamics
    
    for (a in 1:numagents){        # iterate over agents 
      
      curhunger[[a]] = curhunger[[a]] + runif(1)    #get hungrier
      curbladder[[a]] = curbladder[[a]] + runif(1)  #bladder fills
      
      
      if(agentstates[[a]] == -10){         # if moving, continue
        tosearch = agentpaths[[a]][1:length(agentpaths[[a]])-1]
        currspot = which(tosearch == nodeloci[[a]])   # current location of agent
        if(currspot < length(agentpaths[[a]])-1){   # if current location smaller than length of path -1
          nodeloci[[a]] = agentpaths[[a]][[currspot+1]]      # keep moving
        } else {                                      # if not moving
          agentstates[[a]] = agentpaths[[a]][[currspot+1]]  #assign endstate
          agentpaths[[a]] = list(0)    #delist path
          telapsed[[a]] = 0            #restart time elapsed in state
        }
      }
      
      
      if(nodeloci[[a]] %in% kitchen_spots && agentstates[[a]] != -10){    # if in the kitchen
        curhunger[[a]] = 0    #reset hunger
      } else if(nodeloci[[a]] %in% wc_spots && agentstates[[a]] != -10){  # if in the washroom
        curbladder[[a]] = 0   #empty bladder
      }
      
      
      if(nodeloci[[a]] != workstation[[a]] && agentstates[[a]] == 0){  #if not @ desk & working
        agentstates[[a]] = -10                                       #then: go to your desk
        pickapath(nodeloci[[a]], workstation[[a]], a, 0)
      }
      
      if(nodeloci[[a]] %notin% kitchen_spots && agentstates[[a]] == 1){   #if not @ kitchen & eating
        agentstates[[a]] = -10                                #then: go to kitchen
        maxatt = -10000
        for(p in 1:length(kitchen_spots)){                    #iterate over kitchen spots
          attlone(nodeloci[[a]], kitchen_spots[[p]], a)       #find attractiveness of all paths
          bestpath <- sort(unlist(attlist), decreasing = T)[1]          #find best path
          if(bestpath > maxatt){                                #if better than older ones
            maxatt = bestpath
            mypath <- pathlist[[which(attlist == bestpath)[1]]]   #go there. TEMP SOLUTION!!!!!
          }
        }                                      
        movepath(mypath,a,1)                                  #move along that path
      }
      
      if(nodeloci[[a]] %notin% wc_spots && agentstates[[a]]==3){   #if not @ WC & peeing
        agentstates[[a]] = -10                        #then: go to WC
        maxatt = -10000
        for(p in 1:length(wc_spots)){              #iterate over washroom spots
          attlone(nodeloci[[a]], wc_spots[[p]], a)  #find attractiveness of all paths
          bestpath <- sort(unlist(attlist), decreasing = T)[1]          #find best path
          if(bestpath > maxatt){                                #if better than older ones
            maxatt = bestpath
            mypath <- pathlist[[which(attlist == bestpath)[1]]]   #go there. TEMP SOLUTION!!!!!
          }
        }                                      
        movepath(mypath,a,3)          #move along that path
      }
      
      if(nodeloci[[a]] %notin% sr_spots && agentstates[[a]]==2){   #if not @ relax & relaxing
        agentstates[[a]] = -10                        #then: go to relax
        maxatt = -10000
        for(p in 1:length(sr_spots)){               #iterate over relax spots
          attlone(nodeloci[[a]], sr_spots[[p]], a)  #find attractiveness of all paths
          bestpath <- sort(unlist(attlist), decreasing = T)[1]          #find best path
          if(bestpath > maxatt){                                #if better than older ones
            maxatt = bestpath
            mypath <- pathlist[[which(attlist == bestpath)[1]]]   #go there. TEMP SOLUTION!!!!!
          }
        }                                      
        movepath(mypath,a,2)          #move along that path
      }
      
      if(nodeloci[[a]] %notin% sr_spots && agentstates[[a]]==4){   #if not @ social & social
        agentstates[[a]] = -10                        #then: go to socializing spot
        maxatt = -10000
        for(p in 1:length(sr_spots)){              #iterate over soc spots
          attlone(nodeloci[[a]], sr_spots[[p]], a)  #find attractiveness of all paths
          bestpath <- sort(unlist(attlist), decreasing = T)[1]          #find best path
          if(bestpath > maxatt){                                #if better than older ones
            maxatt = bestpath
            mypath <- pathlist[[which(attlist == bestpath)[1]]]   #go there. TEMP SOLUTION!!!!!
          }
        }                                                  
        movepath(mypath,a,4)          #move along that path
      }

      
      happy(a)  #update agent happiness
      
      
      if(agentstates[[a]] != -10){  #if not moving
        telapsed[[a]] = telapsed[[a]] + 1  #add time
        oldstate = agentstates[[a]]   #current agent state
        curmatrix = makeprobs(t, telapsed[[a]], personality[[a]], curbladder[[a]], curhunger[[a]])  #calculate transition matrix
        curprobs = curmatrix[ ,agentstates[[a]]+1]   # relevant column of matrix
        curprobs = curprobs/sum(curprobs)  #normalize it
        
        myrand = runif(1) 
        if(myrand <= curprobs[[1]]){     # choose transition by weighted random value
          agentstates[[a]] = 0
        } else if(myrand <= sum(curprobs[1:2])){
          agentstates[[a]] = 1
        } else if(myrand <= sum(curprobs[1:3])){
          agentstates[[a]] = 2
        } else if(myrand <= sum(curprobs[1:4])){
          agentstates[[a]] = 3
        } else {
          agentstates[[a]] = 4
        }
        
        if (agentstates[[a]] != oldstate) {  #if transitioning
          telapsed[[a]] = 0      # reset telapsed to 0
        }
      }
    }
    
    #record history
    statehistory[length(statehistory)+1] = list(agentstates)
    nodehistory[length(nodehistory)+1] = list(nodeloci) 
    thistory[length(thistory)+1] = list(telapsed)
    haphistory[length(haphistory)+1] = list(happiness)
  }
}


# --  save the data -----------------------------------

state_h_data <- as.data.frame(t(as.data.frame(statehistory, col.names=1:length(statehistory), optional = TRUE)))
node_h_data <- as.data.frame(t(as.data.frame(nodehistory, col.names=1:length(nodehistory), optional = TRUE)))
te_h_data <- as.data.frame(t(as.data.frame(thistory, col.names=1:length(thistory), optional = TRUE)))
haps_h_data <- as.data.frame(t(as.data.frame(haphistory, col.names=1:length(haphistory), optional = TRUE)))
att_h_data <- as.data.frame(t(as.data.frame(atthistory,col.names=NA, optional = TRUE)))


for(i in 1:numagents){   #append workstation to personality (for reference)
  personality[[i]][length(personality[[i]])+1] = workstation[[i]]
}
pers_data <- as.data.frame(t(as.data.frame(personality, col.names=1:numagents, optional = TRUE)))

attcols = c("Day","Time","Agent","Node","delta","value")
setwd(paste0(getwd(), "/sims"))

freenum = (length(list.files(pattern = "\\.csv$", ignore.case=TRUE))/6) + 1
myname = paste("R",toString(freenum),sep="")
write.table(state_h_data, file = paste(myname,"state.csv", sep=""), sep=",", col.names=NA)
write.table(node_h_data, file = paste(myname,"node.csv", sep=""), sep=",", col.names=NA)
write.table(te_h_data, file = paste(myname,"te.csv",  sep=""), sep=",", col.names=NA)
write.table(pers_data, file = paste(myname,"pers.csv",  sep=""), sep=",", col.names=NA)
write.table(haps_h_data, file = paste(myname,"haps.csv",  sep=""), sep=",", col.names=NA)
write.table(att_h_data, file = paste(myname,"atts.csv",  sep=""), sep=",", col.names=attcols, row.names=FALSE)

setwd(mydir)

