#This script stores all the functions

#temporary functions for debugging:  -----------------------------------------
#gives the probabilities for testing, for one agent at one time in one state
giveprob <- function(time, tspent, person, blad, hung, state){  
  cm = makeprobs(time, tspent, person, blad, hung)
  output = cm[ ,state+1]/sum(cm[ ,state+1])
  return(zapsmall(output,3))
}
#gives the matrix for testing, [not normalized!]
giveallprob <- function(time, tspent, person, blad, hung){  
  cm=makeprobs(time,tspent,person,blad,hung)
  return(zapsmall(cm,4))
}
#just like pickapath, but returns the path rather than attaching it to 
#any agent. For testing purposes. no agent or endstate needed.
givepath <- function(startnode, endnode, me){
  paths <<- all_simple_paths(F1Graph, from = names(floorPlan)[[startnode]], to = names(floorPlan)[[endnode]]) #all possible simple paths
  attractiveness <<- rep(NA, length = length(paths)) #empty list for path attractiveness
  
  for (i in 1:length(paths)){  # iterate over paths
    attractiveness[[i]] <- 100  #all start w att = 100
    for (n in 1:length(agentnodeatt[[me]])){  #iterate over all possible nodes
      if (is.element(toString(n), paths[[i]]) == TRUE){  #adjust attractiveness of path
        attractiveness[[i]] = attractiveness[[i]] + agentnodeatt[[me]][[n]]
      }
    }
    attractiveness[[i]] = attractiveness[[i]] - (length(paths[[i]]) * attrade)  # att -= length of path x 3
  }
  
  bestpath <<- sort(attractiveness, decreasing = T)[1]  #most attractive path
  mypath <<- paths[[which(attractiveness == bestpath)]]
  pathbynums <- vector(mode = "list", length = length(mypath))  #vector for path
  for (j in 1:length(mypath)){                                  # iterate through the path
    pathbynums[[j]] = which(colnames(floor)==mypath[[j]]$name) # add node numbers to list
  }
  return(unlist(pathbynums))
}
#---------------------------------------------------------------------------

`%notin%` <- Negate(`%in%`)

#shows the shortest path from startnode to endnode
showapath <- function(startnode, endnode){
  short <- all_shortest_paths(F1Graph, from = names(floorPlan)[[startnode]], to = names(floorPlan)[[endnode]])    # Shortest path from the startnode to the endnode
  mypath = short$res[[1]]  # agent's path is the first shortest one
  pathbynums <- vector(mode = "list", length = length(mypath))    # create vector list with the length of agents' path
  for (i in 1:length(mypath)){                           # iterate through the path
    pathbynums[[i]] = which(colnames(floor)==mypath[[i]]$name)     # add node numbers to the list pathbynums by finding where in the Floor matrix the agent's path lies
  }
  return(list(unlist(pathbynums),mypath))   # return the path
}


#Move agent along a designated path with a determined end state
movepath <- function(mypath, me, endstate){
  pathbynums <- vector(mode = "list", length = length(mypath))  #vector for path
  for (j in 1:length(mypath)){                                  # iterate through the path
    pathbynums[[j]] = which(colnames(floorPlan)==mypath[[j]]$name) # add node numbers to list
  }
  pathbynums[length(pathbynums)+1] = endstate  # add endstate @ end of path
  agentpaths[[me]] <<- pathbynums # add path to agentpaths for this agent
}


#find the most attractive path from startnode to endnode
attlone <- function(startnode, endnode, me){
  attlist <<- list()
  pathlist <<- list()
  paths <<- all_simple_paths(F1Graph, from = names(floorPlan)[[startnode]], to = names(floorPlan)[[endnode]]) #all possible simple paths
  attractiveness <<- rep(NA, length = length(paths)) #empty list for path attractiveness
  myattvalues = agentnodeatt[[me]]
  for (i in 1:length(paths)){  # iterate over paths
    attractiveness[[i]] <- 100  #all start w att = 100
    for (n in 1:length(myattvalues)){  #iterate over all possible nodes
      if (is.element(toString(n), paths[[i]]) == TRUE){  #adjust attractiveness of path
        attractiveness[[i]] = attractiveness[[i]] + myattvalues[[n]]
      }
    }
    attractiveness[[i]] <<- attractiveness[[i]] - (length(paths[[i]]) * attrade)  # att -= length of path x attrade
  }
  pathlist <<- append(pathlist, paths)
  attlist <<- append(attlist, attractiveness)
}


#selects the most attractive path from startnode to endnode
#for one agent, me. Sets agent state to endstate at the end
#places path as a vector of nodes (with endstate @ the end) into agentpaths
#[returns nothing]
pickapath <- function(startnode, endnode, me, endstate){
  paths <<- all_simple_paths(F1Graph, from = names(floorPlan)[[startnode]], to = names(floorPlan)[[endnode]]) #all possible simple paths
  attractiveness <<- rep(NA, length = length(paths)) #empty list for path attractiveness
  myattvalues = agentnodeatt[[me]]              #my attraction to each node
  
  for (i in 1:length(paths)){                    #iterate over all possible paths
    attractiveness[[i]] <- 100                   #all paths start with attraction = 100
    for (n in 1:length(myattvalues)){            #add each node's attractiveness... 
      if (is.element(toString(n), paths[[i]]) == TRUE){  #...that is in the path
        attractiveness[[i]] = attractiveness[[i]] + myattvalues[[n]]
      }
    }
    # longer paths are less attractive. each extra node -= attrade
    attractiveness[[i]] = attractiveness[[i]] - (length(paths[[i]]) * attrade)  
  }
  
  bestpath <<- sort(attractiveness, decreasing = T)[1]  #most attractive path
  mypath <<- paths[[which(attractiveness == bestpath)]]
  pathbynums <- vector(mode = "list", length = length(mypath))  #path as a vector
  for (j in 1:length(mypath)){                                  #iterate through the path
    pathbynums[[j]] = which(colnames(floorPlan)==mypath[[j]]$name)  #add nodes to list
  }
  pathbynums[length(pathbynums)+1] = endstate                  #add endstate @ end of path
  agentpaths[[me]] <<- pathbynums                              #assign path to agent
}


#Initializes the model at the start of each day so as to keep history from the last run
initializeday <- function(){
  agentpaths <<- list()   #list of current path for each agent   
  agentstates <<- c()     #list of current state of each agent
  nodeloci <<- c()        #list of current position for each agent
  telapsed <<- c()        #list of time elapsed for each agent
  curbladder <<- list()   #current bladder state of each agent
  curhunger <<- list()    #current hunger level of each agent
  blanklist = list(0)     #a blank list
  for(i in 1:numagents){        # for each agent:
    agentpaths <<- append(agentpaths, blanklist)          #no current path
    agentstates <<- c(agentstates, 0)                     #state = 0
    telapsed <<- c(telapsed, 0)                           #tealpsed = 0
    curbladder <<- c(curbladder, 0)                       #bladder empty
    curhunger <<- c(curhunger, 0)                         #not hungry
    nodeloci <<- c(nodeloci, sample(out_spots,1))         #start outside
  }
  
  #fill in initial states into histories
  statehistory[length(statehistory)+1] <<- list(agentstates)
  nodehistory[length(nodehistory)+1] <<- list(nodeloci)
  thistory[length(thistory)+1] <<- list(telapsed)
}


#Initializes the model; sets up one-time things
initialize <- function(){
  workstation <<- sample(ws_spots,numagents,replace=T)  #work node for each agent
  personality <<- list()  #personality of each agent
  happiness <<- c()       #happiness of each agent
  agentnodeatt <<- list() #node attractiveness for each agent
  
  haphistory <<- list()   #history of happiness
  thistory <<- list()     #history of telapsed
  statehistory <<- c()   #history of agent states
  nodehistory <<- c()    #history of agent positions
  atthistory <<- list()     #history of node attractiveness
  
  for(i in 1:numagents){        # for each agent:
    happiness <<- append(happiness, 0)                    #start happiness at 0
    agentnodeatt <<- append(agentnodeatt, list(node_attractiveness))  #attractiveness of nodes
    
    foodiness <- rnorm(1, mean = foodpersmean, sd = foodperssd)  #personality 
    sociability <- rnorm(1, mean = socpersmean, sd = socperssd)
    workethic <- rnorm(1, mean = workpersmean, sd = workperssd)
    bladsize <- rnorm(1, mean = bladpersmean, sd = bladperssd)
    traits <- list(c(foodiness, sociability, workethic, bladsize))
    personality[length(personality)+1] <<- traits
  }
  
  haphistory[length(haphistory)+1] <<- list(happiness)
  
  perscdf <<- makecdf(socpersmean, socperssd)
}


#updates the social dynamics
dosr <- function(d, t){  
  
  tempstates <- c()  #temporary storage for new states
  
  for(a in 1:numagents){  #for each agent
    
    if(nodeloci[[a]] %in% sr_spots){                    #if in an sr_spot
      fellows <- which(nodeloci == nodeloci[[a]])         #who else is there?
      
      if(agentstates[[a]] == 2){          #[if relaxing]
        
        if(length(fellows) == 1){                         #nobody but me, relax
          tempstates <- append(tempstates, 2)
          agentnodeatt[[a]][nodeloci[[a]]] <<- agentnodeatt[[a]][nodeloci[[a]]] + alpha
          #record change in attractiveness: [day, time, agent#, node#, 1, new value]
          atthistory[length(atthistory)+1] <<- list(c(d, t, a, nodeloci[[a]], 1, agentnodeatt[[a]][nodeloci[[a]]]))
          
        } else {                                          #somebody
          localchatters <- which(agentstates[fellows] == 4)  #are they social?
          if(length(localchatters == 0)){                      #no, relax
            tempstates <- append(tempstates, 2)
            agentnodeatt[[a]][nodeloci[[a]]] <<- agentnodeatt[[a]][nodeloci[[a]]] + alpha
            #record change in attractiveness: [day, time, agent#, node#, 1, new value]
            atthistory[length(atthistory)+1] <<- list(c(d, t, a, nodeloci[[a]], 1, agentnodeatt[[a]][nodeloci[[a]]]))
            
          } else {                                             #yes, MIX
            localrelaxers <- which(agentstates[fellows] == 2)  #others relaxing
            
            relaxsum = 0        #sum of relaxers' sociability
            for(i in fellows[localrelaxers]){
              relaxsum = relaxsum + personality[[i]][2]
            }
            socialsum = 0       #sum of socializers' sociability
            for(i in fellows[localchatters]){
              socialsum = socialsum + personality[[i]][2]
            }
            
            if(socialsum >= relaxsum){   #strongest force wins
              tempstates <- append(tempstates, 4)
              agentnodeatt[[a]][nodeloci[[a]]] <<- agentnodeatt[[a]][nodeloci[[a]]] - alpha
              #record change in attractiveness: [day, time, agent#, node#, -1, new value]
              atthistory[length(atthistory)+1] <<- list(c(d, t, a, nodeloci[[a]], -1, agentnodeatt[[a]][nodeloci[[a]]]))
              
            } else {
              tempstates <- append(tempstates, 2)
              agentnodeatt[[a]][nodeloci[[a]]] <<- agentnodeatt[[a]][nodeloci[[a]]] + alpha
              #record change in attractiveness: [day, time, agent#, node#, 1, new value]
              atthistory[length(atthistory)+1] <<- list(c(d, t, a, nodeloci[[a]], 1, agentnodeatt[[a]][nodeloci[[a]]]))
            }
          }
        }
        
      } else if(agentstates[[a]] == 4){   #[if being social]
        
        if(length(fellows) == 1){                      #nobody but me
          probleave = perscdf(personality[[a]][2])      #P(leave) ~ sociability
          rando = runif(1)
          if(rando <= probleave){
            tempstates <- append(tempstates, -10)
            agentnodeatt[[a]][nodeloci[[a]]] <<- agentnodeatt[[a]][nodeloci[[a]]] - alpha
            #record change in attractiveness: [day, time, agent#, node#, -1, new value]
            atthistory[length(atthistory)+1] <<- list(c(d, t, a, nodeloci[[a]], -1, agentnodeatt[[a]][nodeloci[[a]]]))
            
            #choose another place to go to!!!!!!!!!!!!!!!
            pickapath(nodeloci[[a]], workstation[[a]], a, 0) #stopgap: go back to desk & work
            
          } else{
            tempstates <- append(tempstates, 2)     #don't leave, relax
            agentnodeatt[[a]][nodeloci[[a]]] <<- agentnodeatt[[a]][nodeloci[[a]]] - alpha
            #record change in attractiveness: [day, time, agent#, node#, -1, new value]
            atthistory[length(atthistory)+1] <<- list(c(d, t, a, nodeloci[[a]], -1, agentnodeatt[[a]][nodeloci[[a]]]))
          }
          
        } else {                                          #somebody
          localchatters <- which(agentstates[fellows] == 4)  #are they social?
          if(length(localchatters > 1)){          #yes (other than self), be social 
            tempstates <- append(tempstates, 4)
            agentnodeatt[[a]][nodeloci[[a]]] <<- agentnodeatt[[a]][nodeloci[[a]]] + alpha
            #record change in attractiveness: [day, time, agent#, node#, 1, new value]
            atthistory[length(atthistory)+1] <<- list(c(d, t, a, nodeloci[[a]], 1, agentnodeatt[[a]][nodeloci[[a]]]))
            
          } else {                                             #no, MIX
            localrelaxers <- which(agentstates[fellows] == 2)  #others relaxing
            
            relaxsum = 0        #sum of relaxers' sociability
            for(i in fellows[localrelaxers]){
              relaxsum = relaxsum + personality[[i]][2]
            }
            socialsum = 0       #sum of socializers' sociability
            for(i in fellows[localchatters]){
              socialsum = socialsum + personality[[i]][2]
            }
            
            if(socialsum >= relaxsum){   #strongest force wins
              tempstates <- append(tempstates, 4)
              agentnodeatt[[a]][nodeloci[[a]]] <<- agentnodeatt[[a]][nodeloci[[a]]] + alpha
              #record change in attractiveness: [day, time, agent#, node#, 1, new value]
              atthistory[length(atthistory)+1] <<- list(c(d, t, a, nodeloci[[a]], 1, agentnodeatt[[a]][nodeloci[[a]]]))
              
            } else {
              tempstates <- append(tempstates, 2)
              agentnodeatt[[a]][nodeloci[[a]]] <<- agentnodeatt[[a]][nodeloci[[a]]] - alpha
              #record change in attractiveness: [day, time, agent#, node#, -1, new value]
              atthistory[length(atthistory)+1] <<- list(c(d, t, a, nodeloci[[a]], -1, agentnodeatt[[a]][nodeloci[[a]]]))
            }
          }
        }
      }
    }
  }
  agentstates = tempstates  #set all agents' new states synchronously
}


#returns a cdf of a normal distribution
makecdf <- function(mu, sig){
  samples = rnorm(10000, mean = mu, sd = sig)
  return(ecdf(samples))
}


#calculate the transition matrix, by time
#takes as input: current sim time + agent values: tspent, personality, bladder, hunger
makeprobs <- function(time, tspent, person, blad, hung){
  output = data.frame(Work=c(0,0,0,0,0),
                      Eat=c(0,0,0,0,0),
                      Relax=c(0,0,0,0,0),
                      Washroom=c(0,0,0,0,0),
                      Socialize=c(0,0,0,0,0))
  
  #functions for time of day effects:
  if(time <= 150){   #W1 function
    width1 = 120 + (8*person[3])
  } else if(time <= 210){
    width1 = 40
  } else {
    width1 = 90
  }
  if(time <= 150 - (8*person[1])){  #W2 function
    width2 = 220
  } else if(time <= 330 - (8*person[1])){
    width2 = 40
  } else {
    width2 = 150 - (8*person[1])
  }
  
  #to working
  output[1,1] = 1/(1 + exp(-0.5*((width1/2)-tspent)))     #P(work -> work)
  output[1,2] = 1/(1 + exp(-3-(person[3]*3)))             #P(eat -> work)
  output[1,3] = 1/(1 + exp(-3-(person[3]*3)))             #P(relax -> work)
  output[1,4] = 1/(1 + exp(-3-(person[3]*3)))             #P(toilet -> work)
  output[1,5] = 1/(1 + exp(-3-(person[3]*3)))             #P(social -> work)
  #to eating
  output[2,1] = 1/(1 + exp(0.5*((width2/2)-hung)))    #P(work -> eat)
  output[2,2] = 0                                     #P(eat -> eat)
  output[2,3] = 1/(1 + exp(0.5*((width2/2)-hung)))    #P(relax -> eat)
  output[2,4] = 1/(1 + exp(0.5*((width2/2)-hung)))    #P(toilet -> eat)
  output[2,5] = 1/(1 + exp(0.5*((width2/2)-hung)))    #P(social -> eat)
  #to relaxing
  output[3,1] = 1/(1 + exp(50+(5*person[3])-tspent))  #P(work -> relax)
  output[3,2] = 1/(1 + exp(person[3]*3))        #P(eat -> relax)
  output[3,3] = 1/(1 + exp(person[3]*3))        #P(relax -> relax)
  output[3,4] = 1/(1 + exp(person[3]*3))        #P(toilet -> relax)
  output[3,5] = 1/(1 + exp(person[3]*3))        #P(social -> relax)
  #to WC
  output[4,1] = 1/(1 + exp((8*person[4])-(blad-60)))  #P(work -> toilet)
  output[4,2] = 1/(1 + exp((8*person[4])-(blad-60)))  #P(eat -> toilet)
  output[4,3] = 1/(1 + exp((8*person[4])-(blad-60)))  #P(relax -> toilet)
  output[4,4] = 0                                   #P(toilet -> toilet)
  output[4,5] = 1/(1 + exp((8*person[4])-(blad-60)))  #P(social -> toilet)
  #to social
  output[5,1] = 1/(1 + exp(50-(5*person[2])+(5*person[3])-tspent))  #P(work -> social)
  output[5,2] = 1/(1 + exp(person[3]-(2*person[2])))       #P(eat -> social)
  output[5,3] = 1/(1 + exp(person[3]-(2*person[2])))       #P(relax -> social)
  output[5,4] = 1/(1 + exp(person[3]-(2*person[2])))       #P(toilet -> social)
  output[5,5] = 1/(1 + exp(person[3]-(2*person[2])))       #P(social -> social)
  
  if(tspent < 3){   #in toilet: min 3 min
    output[4,4] = 1000
  }
  if(tspent < 10){  #relaxing or social: min 10 min
    output[3,3] = 1000 #P(relax -> relax)
    output[5,5] = 1000 #P(social -> social)
  }
  if(tspent < 30){   #eating: min 30 min
    output[2,2] = 1000 #P(eating -> eating)
  }
  
  return(output)
}


#update an agent's happiness
happy <- function(agent){
  if(agentstates[[agent]] != -10 && agentstates[[agent]] != 3){
    if(agentstates[agent] == 0){
      trait = personality[[agent]][3]
      traitmean = workpersmean
    } else if(agentstates[[agent]] == 1){
      trait = personality[[agent]][1]
      traitmean = foodpersmean
    } else if(agentstates[[agent]] == 2){
      trait = (workpersmean*2) - (personality[[agent]][3])
      traitmean = workpersmean
    } else if(agentstates[[agent]] == 3){
      trait = personality[[agent]][4]
      traitmean = bladpersmean
    } else if(agentstates[agent] == 4){
      trait = personality[[agent]][2]
      traitmean = socpersmean
    }
    happiness[[agent]] <<- (happiness[[agent]] + (trait/traitmean))  #divide trait by trait mean and add that to happiness
  }
}

