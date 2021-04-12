#functions for analyzing networks

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

######### Network Creation #########

#More of any variable gives you more integration because they are all related to connecting more edges to vertices, either retroactively or proactively
#age = the number of time steps passed since the vertex is added. Vertex age is divided into aging bins
#I would suggest changing them all at once to the same number chosen because they play off each other. If you do change them, keep them close to each other
#Do not go over 5% of N for the variables or else the integration caps at its minimum (1.96)
#leave m at 1, it is the amount of edges each new node creates
#the more integrated it is from the variables, the more rooms closer to the atrium for example, until every single room connects to just the atrium (the starting node in the network generation)

createnetwork <<- function(numnodes,paexp,ae,ab){
  network <<- sample_pa_age(numnodes, pa.exp=paexp, aging.exp=-ae, aging.bin=ab, m = 1, out.pref = T)
  network <- as.undirected(network)
  network <<- as_adjacency_matrix(network)
  network <- graph_from_adjacency_matrix(network)
}

#### Mean depth calculation #### need graph object

meanglobalintegration2 <<- function(g){
allnodedepths <<- distances(g)
listofdepths <<- list()

  for (n in 1:ncol(allnodedepths)){
  nodedepth <- distances(g)[n,]    # takes the first row of the distances table (distances of nodes from the first node generated) into a temporary vector
  meandepth <- sum(nodedepth)/(length(nodedepth)-1)  # sum all of the distances, then divide by N-1, N being number of nodes
  listofdepths <<- append(listofdepths, meandepth)
  }
  total <<- mean(as.numeric(listofdepths))
  print(total)
}

##### Easier and faster way to do it ######

meanglobalintegration <<- function(g){
  nodedepths <- distances(g)
  tmd <- (sum(nodedepths))/(length(nodedepths) - length(nodedepths[1,]))
  print(tmd)
}
