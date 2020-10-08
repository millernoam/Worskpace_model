#This script stores all the functions


#gives the probabilities for testing, for one agent at one time in one state
giveprob <- function(time, tspent, person, blad, hung, state){  
  cm=makeprobs(time,tspent,person,blad,hung)
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
givepath <- function(startnode, endnode){
  paths <<- all_simple_paths(floorG, from = names(floor)[[startnode]], to = names(floor)[[endnode]]) #all possible simple paths
  attractiveness <<- rep(NA, length = length(paths)) #empty list for path attractiveness
  
  for (i in 1:length(paths)){  # iterate over paths
    attractiveness[[i]] <- 100  #all start w att = 100
    for (n in 1:length(nodeatt)){  #iterate over all possible nodes
      if (is.element(toString(n), paths[[i]]) == TRUE){  #adjust attractiveness of path
        attractiveness[[i]] = attractiveness[[i]] + nodeatt[[n]]
      }
    }
    attractiveness[[i]] = attractiveness[[i]] - (length(paths[[i]]) * 3)  # att -= length of path x 3
  }
  
  bestpath <<- sort(attractiveness, decreasing = T)[1]  #most attractive path
  mypath <<- paths[[which(attractiveness == bestpath)]]
  pathbynums <- vector(mode = "list", length = length(mypath))  #vector for path
  for (j in 1:length(mypath)){                                  # iterate through the path
    pathbynums[[j]] = which(colnames(floor)==mypath[[j]]$name) # add node numbers to list
  }
  return(unlist(pathbynums))
}


#[describe function]
showapath <- function(startnode, endnode){
  short <- all_shortest_paths(floorG, from = names(floor)[[startnode]], to = names(floor)[[endnode]])    # Shortest path from the startnode to the endnode
  mypath = short$res[[1]]  # agent's path is the first shortest one
  pathbynums <- vector(mode = "list", length = length(mypath))    # create vector list with the length of agents' path
  for (i in 1:length(mypath)){                           # iterate through the path
    pathbynums[[i]] = which(colnames(floor)==mypath[[i]]$name)     # add node numbers to the list pathbynums by finding where in the Floor matrix the agent's path lies
  }
  return(list(unlist(pathbynums),mypath))   # return the path
}


#[describe function]
movepath <- function(mypath, me, endstate){
  pathbynums <- vector(mode = "list", length = length(mypath))  #vector for path
  for (j in 1:length(mypath)){                                  # iterate through the path
    pathbynums[[j]] = which(colnames(floor)==mypath[[j]]$name) # add node numbers to list
  }
  pathbynums[length(pathbynums)+1] = endstate  # add endstate @ end of path
  agentpaths[[me]] <<- pathbynums # add path to agentpaths for this agent
}


#[describe function]
attlone <- function(startnode, endnode){
  attlist <<- list()
  pathlist <<- list()
  paths <<- all_simple_paths(floorG, from = names(floor)[[startnode]], to = names(floor)[[endnode]]) #all possible simple paths
  attractiveness <<- rep(NA, length = length(paths)) #empty list for path attractiveness
  for (i in 1:length(paths)){  # iterate over paths
    attractiveness[[i]] <- 100  #all start w att = 100
    for (n in 1:length(nodeatt)){  #iterate over all possible nodes
      if (is.element(toString(n), paths[[i]]) == TRUE){  #adjust attractiveness of path
        attractiveness[[i]] = attractiveness[[i]] + nodeatt[[n]]
      }
    }
    attractiveness[[i]] = attractiveness[[i]] - (length(paths[[i]]) * 3)  # att -= length of path x 3
  }
  pathlist <- append(pathlist, paths)
  attlist <- append(attlist, attractiveness)
}


#selects the most attractive path from startnode to endnode
#for one agent, me. Sets agent state to endstate at the end
#places path as a vector of nodes (with endstate @ the end) into agentpaths
#[returns nothing]
pickapath <- function(startnode, endnode, me, endstate){
  paths <<- all_simple_paths(floorG, from = names(floor)[[startnode]], to = names(floor)[[endnode]]) #all possible simple paths
  attractiveness <<- rep(NA, length = length(paths)) #empty list for path attractiveness
  
  for (i in 1:length(paths)){  # iterate over paths
    attractiveness[[i]] <- 100  #all start w att = 100
    for (n in 1:length(nodeatt)){  #iterate over all possible nodes
      if (is.element(toString(n), paths[[i]]) == TRUE){  #adjust attractiveness of path
        attractiveness[[i]] = attractiveness[[i]] + nodeatt[[n]]
      }
    }
    attractiveness[[i]] = attractiveness[[i]] - (length(paths[[i]]) * 3)  # att -= length of path x 3
  }
  
  bestpath <<- sort(attractiveness, decreasing = T)[1]  #most attractive path
  mypath <<- paths[[which(attractiveness == bestpath)]]
  pathbynums <- vector(mode = "list", length = length(mypath))  #vector for path
  for (j in 1:length(mypath)){                                  # iterate through the path
    pathbynums[[j]] = which(colnames(floor)==mypath[[j]]$name) # add node numbers to list
  }
  pathbynums[length(pathbynums)+1] = endstate  # add endstate @ end of path
  agentpaths[[me]] <<- pathbynums # add path to agentpaths for this agent
}


#Initializes the model; sets everything up
initialize <- function(){
  agentpaths <<- list()   #list of current path for each agent   
  agentstates <<- c()     #list of current state of each agent
  nodeloci <<- c()        #list of current position for each agent
  telapsed <<- c()        #list of time elapsed for each agent
  workstation <<- sample(ws_spots,numagents,replace=T)  #work node for each agent
  personality <<- list()  #personality of each agent
  curbladder <<- list()   #current bladder state of each agent
  curhunger <<- list()    #current hunger level of each agent
  happiness <<- c()       #happiness of each agent
  agentnodeatt <<- list() #node attractiveness for each agent
  blanklist = list(0)     #a blank list
  
  thistory <<- list()     #history of telapsed
  statehistory <<- c()   #history of agent states
  nodehistory <<- c()    #history of agent positions
  
  for(i in 1:numagents){        # for each agent:
    happiness <<- append(happiness, 0)                    #start happiness at 0
    agentnodeatt <<- append(agentnodeatt, list(nodeatt))  #attractiveness of nodes
    agentpaths <<- append(agentpaths, blanklist)          #no current path
    agentstates <<- c(agentstates, 0)                     #state = 0
    telapsed <<- c(telapsed, 0)                           #tealpsed = 0
    curbladder <<- c(curbladder, 0)                       #bladder empty
    curhunger <<- c(curhunger, 0)                         #not hungry
    nodeloci <<- c(nodeloci, sample(out_spots,1))         #start outside
    
    foodiness <- rnorm(1, mean = foodpersmean, sd = foodperssd)  #personality 
    sociability <- rnorm(1, mean = socpersmean, sd = socperssd)
    workethic <- rnorm(1, mean = workpersmean, sd = workperssd)
    bladsize <- rnorm(1, mean = bladpersmean, sd = bladperssd)
    traits <- c(foodiness, sociability, workethic, bladsize)  
    mylist <- list(traits)
    personality[length(personality)+1] <<- mylist
  }
  
  #fill in initial states into histories
  statehistory[length(statehistory)+1] <<- list(agentstates)
  nodehistory[length(nodehistory)+1] <<- list(nodeloci)
  thistory[length(thistory)+1] <<- list(telapsed)
  
  perscdf <<- makecdf(socpersmean, socperssd)
}


#updates the social dynamics
dosr <- function(){  
  for(a in 1:numagents){
    
    if(nodeloci[[a]] %in% sr_spots){                    #if in an sr_spot
      fellows <- which(nodeloci == nodeloci[[a]])         #who else is there?
      
      if(agentstates[[a]] == 2){          #[if relaxing]
        
        if(length(fellows) == 0){                         #nobody, relax
          agentstates[[a]] = 2
        } else {                                          #somebody
          localchatters <- which(agentstates[fellows] == 4)  #are they social?
          if(length(localchatters == 0)){                      #no, relax
            agentstates[[a]] = 2
          } else {                                             #yes, MIX
            #MIX code here!!
          }
        }
        
      } else if(agentstates[[a]] == 4){   #[if being social]
        
        if(length(fellows) == 0){                         #nobody
          probleave = perscdf(personality[a][[2]])
          rando = runif(1)
          if(rando <= probleave){
            #choose another place to go to
            agentstates[[a]] = -10  #leave
          } else{
            agentstates[[a]] = 2     #relax
          }
          
        } else {                                          #somebody
          localchatters <- which(agentstates[fellows] == 4)  #are they social?
          if(length(localchatters > 0)){                      #yes, be social
            agentstates[[a]] = 4
          } else {                                             #no, MIX
            #MIX code here!!
          }
        }
      }
    }
  }
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
      traitmean = workmean
    } else if(agentstates[[agent]] == 2){
      trait = (workmean*2) - (personality[[agent]][3])
      traitmean = workmean
    }
    happiness[[agent]] = (happiness[[agent]] + (trait/traitmean))  #divide trait by trait mean and add that to happiness
  }
}


#functions that display the results -------------------------------------------


#plots the current network. could be made more complex
showthenet <- function(){
  layt = layout.fruchterman.reingold(floorG)
  tkplot(floorG, canvas.width=750,canvas.height=550, layout=layt, vertex.color='white')
}


#show a range of info on one agent
showagent <- function(a){   
  windows(width=10, height=7)
  per=personality[[a]]
  par(mfrow=c(2,2),oma=c(0,0,2,0), mar=c(3,3,3,3))
  
  title="Time in each state"
  bpdat= table(factor(state_h_data[[a]],levels=c(-10,0,1,2,3,4)))
  barplot(bpdat/total_time, main=title, axis.lty=1,
          names.arg=c("Move","Work","Eat","Relax","WC","Social"))
  
  title=paste("Time @ node. Desk = ", toString(workstation[[a]]), sep="")
  barplot(table(node_h_data[[a]])/total_time, main=title, axis.lty=1)
  
  title="Personality"
  bp <- barplot(per, main=title,names.arg=c("Hunger","Social","Ethic","Bladder"),ylim=c(-5,5),axis.lty=1)
  text(bp,-4,signif(per,2), col="blue")
  bp
  
  vr = rle(state_h_data[[a]])
  seq0 = vr$lengths[which(vr$values==0)]
  seq0 = seq0[-which(seq0<=1)]
  seq1 = vr$lengths[which(vr$values==1)]
  seq1 = seq1[-which(seq1<=1)]
  seq2 = vr$lengths[which(vr$values==2)]
  seq2 = seq2[-which(seq2<=1)]
  seq3 = vr$lengths[which(vr$values==3)]
  seq3 = seq3[-which(seq3<=1)]
  seq4 = vr$lengths[which(vr$values==4)]
  seq4 = seq4[-which(seq4<=1)]
  
  if(length(seq0)>1){
    dat0 = seq0
  } else{ 
    dat0 = rep(-100,100)
  }
  if(length(seq1)>1){
    dat1 = seq1
  } else{ 
    dat1 = rep(-100,100)
  }
  if(length(seq2)>1){
    dat2 = seq2
  } else{ 
    dat2 = rep(-100,100)
  }
  if(length(seq3)>1){
    dat3 = seq3
  } else{ 
    dat3 = rep(-100,100)
  }
  if(length(seq4)>1){
    dat4 = seq4
  } else{ 
    dat4 = rep(-100,100)
  }
  mymax=max(density(dat0)$y,density(dat1)$y,density(dat2)$y,density(dat3)$y,density(dat4)$y)
  mxmax=max(density(dat0)$x,density(dat1)$x,density(dat2)$x,density(dat3)$x,density(dat4)$x)
  title="Time elapsed dists"
  plot(density(dat0),col="red",ylim=c(0,mymax+0.01),xlim=c(0,mxmax),main=title)
  lines(density(dat1), col="blue")
  lines(density(dat2),col="green")
  lines(density(dat3),col="brown")
  lines(density(dat4),col="orange")
  legend("topright",legend=c("Work","Eat","Relax","WC","Social"),
         col=c("red","blue","green","brown","orange"),lty=1)
  
  mtext(paste("Agent ", toString(a), " summary", sep=""), outer=TRUE, cex=1.5)
}


#show distributions of time in state for all agents
showtimedists <-function(){   
  par(mfrow=c(1,1))
  data = list()
  for(a in 1:numagents){
    da=table(factor(state_h_data[[a]],levels=c(-10,0,1,2,3,4)))/total_time
    data[length(data)+1] = list(da)
  }
  ordata = do.call(rbind,data)
  mymax=max(density(ordata[,1])$y,density(ordata[,2])$y,density(ordata[,3])$y,
            density(ordata[,4])$y,density(ordata[,5])$y,density(ordata[,6])$y)
  mxmax=max(density(ordata[,1])$x,density(ordata[,2])$x,density(ordata[,3])$x,
            density(ordata[,4])$x,density(ordata[,5])$x,density(ordata[,6])$x)
  title="Distributions of time in state (all agents)"
  plot(density(ordata[,1]),col="red",ylim=c(0,mymax+0.001),xlim=c(0,mxmax),main=title)
  lines(density(ordata[,2]), col="blue")
  lines(density(ordata[,3]), col="green")
  lines(density(ordata[,4]), col="brown")
  lines(density(ordata[,5]), col="orange")
  lines(density(ordata[,6]), col="black")
  legend("topright",legend=c("Move","Work","Eat","Relax","WC","Social"),
         col=c("red","blue","green","brown","orange","black"),lty=1)
}