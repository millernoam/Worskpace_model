#States: 0 = Working, 1=Eating; 2=Relax, 3=Washroom, 4=Socialize, -10=walking
#Nodes: ws_spots = working, sr_spots = social/relax
#Personality: Hunger, Sociability, Work Ethic, Bladder_cap

`%notin%` <- Negate(`%in%`)

ws_spots = c(7,17,28,24)  #list of workstations
sr_spots = c(6,14,35)    #socializing/relaxing spots
kitchen_spot = c(14)    #kitchens/dining areas
wc_spot = c(34)         #washrooms
out_spots = c(1,2,3,4)   #outside spots

numagents = 10      #number of agents
total_time = 500    #length of the day
num_days = 25       #number of days to run

foodpersmean = 10     #means and SDs of personality distributions
foodperssd = 6
socpersmean = 10
socperssd = 6
workpersmean = 10
workperssd = 6
bladpersmean = 10
bladperssd = 6

floorG = F1Graph    #network graph
floor = Floor1      #raw network
nodeatt = F1nodeatt  #list of node attractivenesses

# initialize the model  ------------------------------
initialize()  

# run the model -------------------------------
for (d in 1:num_days){            #iterate over days
  
  #things remembered across days...!!!!!!!!!!!!!!!!!!!!!!!!
  
  for (t in 1:total_time){         # iterate over the day
    
    dosr()  #manage social dynamics
    
    for (a in 1:numagents){        # iterate over agents
      
      curhunger[[a]] = curhunger[[a]] + runif(1)  #get hungrier
      curbladder[[a]] = curbladder[[a]] + runif(1)  #bladder fills
      
      if(agentstates[[a]] == -10){         # if moving, continue
        tosearch = agentpaths[[a]][1:length(agentpaths[[a]])-1]
        currspot = which(tosearch == nodeloci[[a]])   # current location of agent
        if(currspot < (length(agentpaths[[a]])-1)){   # if current location smaller than length of path -1
          nodeloci[[a]] = agentpaths[[a]][[currspot+1]]      # keep moving
        } else {                                      # if not moving
          agentstates[[a]] = agentpaths[[a]][[currspot+1]]  #assign endstate
          agentpaths[[a]] = list(0)    #delist path
          telapsed[[a]] = 0            #restart time elapsed in state
        }
      }
      
      if(nodeloci[[a]] %in% kitchen_spot && agentstates[[a]] != -10){    # if in the kitchen
        curhunger[[a]] = 0   #reset hunger
      } else if(nodeloci[[a]] %in% wc_spot && agentstates[[a]] != -10){  # if in the washroom
        curbladder[[a]] = 0   #empty bladder
     }
      
      if(nodeloci[[a]] != workstation[[a]] && agentstates[[a]] == 0){  #if not @ desk & working
        agentstates[[a]] = -10                                       #then: go to your desk
        pickapath(nodeloci[[a]],workstation[[a]],a,0)
      }
      if(nodeloci[[a]] %notin% kitchen_spot && agentstates[[a]] == 1){   #if not @ kitchen & eating
        agentstates[[a]] = -10                        #then: go to kitchen
        pickapath(nodeloci[[a]],kitchen_spot,a,1)
      }
      if(nodeloci[[a]] %notin% wc_spot && agentstates[[a]]==3){   #if not @ WC & peeing
        agentstates[[a]] = -10                        #then: go to WC
        pickapath(nodeloci[[a]],wc_spot,a,3)
      }
      
      if(nodeloci[[a]] %notin% sr_spots && agentstates[[a]]==2){   #if not @ relax & relaxing
        agentstates[[a]] = -10                        #then: go to relax
        for(p in 1:length(sr_spots)){              #iterate over relax spots
          attlone(nodeloci[[a]], sr_spots[[p]])  #find attractivenesses of all paths to all relax spots and place in global environment
        }                                       #attractiveness function is part 1 of pickapath and movepath function is part 2 of pickapath
        bestpath <- sort(attlist, decreasing = T)[1]       #find best path
        mypath <- pathlist[[which(attlist == bestpath)]]
        movepath(mypath,a,2)          #move along that path
      }
      
      if(nodeloci[[a]] %notin% sr_spots && agentstates[[a]]==4){   #if not @ social & social
        agentstates[[a]] = -10                        #then: go to socializing spot
        for(p in 1:length(sr_spots)){              #iterate over soc spots
          attlone(nodeloci[[a]], sr_spots[[p]])  #find attractivenesses of all paths to all soc spots and place in global environment
        }                                                 #attractiveness function is part 1 of pickapath and movepath function is part 2 of pickapath
        bestpath <- sort(attlist, decreasing = T)[1]       #find best path
        mypath <- pathlist[[which(attlist == bestpath)]]
        movepath(mypath,a,4)          #move along that path
      }
      
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
  
    #record history
    statehistory[length(statehistory)+1] = list(agentstates)
    nodehistory[length(nodehistory)+1] = list(nodeloci) 
    thistory[length(thistory)+1] = list(telapsed)
  }
}

  
# --  save the data -----------------------------------
  
personastate_h_data <- as.data.frame(t(as.data.frame(statehistory, col.names=0:total_time+1, optional = TRUE)))
node_h_data <- as.data.frame(t(as.data.frame(nodehistory, col.names=0:total_time+1, optional = TRUE)))
te_h_data <- as.data.frame(t(as.data.frame(thistory, col.names=0:total_time+1, optional = TRUE)))

for(i in 1:numagents){   #append workstation to personality (for reference)
  personality[[i]][length(personality[[i]])+1] = workstation[[i]]
}
pers_data <- as.data.frame(t(as.data.frame(personality, col.names=1:numagents, optional = TRUE)))

setwd("C:/Research/projects/Jovan model/sims")
freenum = (length(list.files())/4) + 1
myname = paste("_N",toString(numagents),"_P",toString(personsd),"_",toString(freenum),".csv",sep="")
write.table(state_h_data, file = paste("state", myname, sep=""), sep=",")
write.table(node_h_data, file = paste("node", myname, sep=""), sep=",")
write.table(te_h_data, file = paste("te", myname, sep=""), sep=",")
write.table(pers_data, file = paste("pers", myname, sep=""), sep=",")
setwd("C:/Research/projects/Jovan model")