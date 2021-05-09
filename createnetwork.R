######### Network Creation #########

#More of any variable gives you more integration because they are all related to connecting more edges to vertices, either retroactively or proactively
#age = the number of time steps passed since the vertex is added. Vertex age is divided into aging bins
#I would suggest changing them all at once to the same number chosen because they play off each other. If you do change them, keep them close to each other
#Do not go over 5% of N for the variables or else the integration caps
#leave m at 1, it is the amount of edges each new node creates
#the more integrated it is from the variables, the more rooms closer to the atrium for example, until every single room connects to just the atrium (the starting node in the network generation)
library(igraph)
createnetwork = function(numnodes,pe,ae,ab){
  network = sample_pa_age(numnodes, pa.exp=pe, aging.exp=-ae, aging.bin=ab, m = 1, out.pref = T)
  network = as.undirected(network)
  networkadj <<- as_adjacency_matrix(network)
  network <<- graph_from_adjacency_matrix(networkadj)
}

### Create a codebook ###
library(xlsx)
codebook = function(numnodes, numagents, time, days, alpha, attrade, pmean, psd){
nodelist = c(1:numnodes)
srspots = list()
workstations = list()
outsidespots = list()
nodes = list()
clus <- cluster_edge_betweenness(network)                              #this is more easily used by other functions of igraph
comm <- groups(clus)                                                #list the communities in this vector
for(o in 1:length(comm)){                                             #make an outside entrance to each community (best way to keep the outdoor spots away from each other and most realistic in terms of configuration)
  outsidespots <- append(outsidespots, sample(comm[[o]], 1))         #sample one node from each community to be the outside spot
  nodelist = nodelist[-outsidespots[[o]]]                          #take out entrance to community from the list
  }
workstations = append(workstations, sample(nodelist, round(length(nodelist)/3), replace = F))     #assign workspace designation to a third of the nodes remaining, no replacement
nodelist = nodelist[-which(nodelist %in% as.numeric(workstations))]                               #remove workstations from nodelist
srspots = append(srspots, sample(nodelist, round(length(nodelist)/5), replace = F))     #assign srspot designation to a third of the nodes remaining, no replacement
nodelist = nodelist[-which(nodelist %in% as.numeric(srspots))]                               #remove srspots from nodelist
kitchenspot = c(sample(nodelist,1))
nodelist = nodelist[-which(nodelist == kitchenspot)]      #omit the kitchen spot from the 
spots = list(unlist(as.numeric(workstations)),kitchenspot,unlist(as.numeric(srspots)),unlist(as.numeric(outsidespots)), check.rows = F)   #make spots in a format that the next line can handle
spots=data.frame(lapply(spots, "length<-", max(lengths(spots))))    #make data frame of spots
colnames(spots) <- c("Workspaces","Kitchen","SRspots","Outside")                                                                          #set the column names to something legible
means = rep(pmean, 3)                                     #set personality mean
sds = rep(psd, 3)                                         #personality sd set
trait = c("Social", "Work", "Eating")                     #set trait names
personality = data.frame(trait,means,sds)    #make data frame of personality
colnames(personality) = c("Traits", "Mean", "SD")
parameters = data.frame(time,numagents,days,alpha,attrade)                   #make a data frame with the parameters
colnames(parameters) = c("Time", "N", "Days", "Alpha", "Attrade")            #set names of columns so it's more legible
nodes = append(nodes, c(kitchenspot,outsidespots,srspots,workstations,nodelist))       #append all designated nodes to this list so we have an order
nodenames = c("Kitchen Spot", rep("Outside",length(outsidespots)), rep("SRspots",length(srspots)), rep("Workstations",length(workstations)), rep("Travel between nodes",length(nodelist)))        #make vector with node names
attractiveness = c(0,rep(-50,length(outsidespots)),round(runif(min = 0,max = 20,length(srspots))), rep(0,length(workstations)),round(runif(min = 0,max = 10,length(nodelist)))) #make vector with attractiveness scores
descriptives = data.frame(unlist(nodes),nodenames,attractiveness)     #combine all of these to make the a data frame of descriptives (Sheet1)
colnames(descriptives) = c("Nodes","Names","Attractiveness")
write.xlsx(descriptives,file = "codebook.xlsx", sheetName = "Descriptives", append = F, row.names = F, col.names = T)                    #make a sheet for the adjacency matrix                  #render excel sheet, omitting the NAs of the irregular length columns
write.xlsx(as.matrix(networkadj),file = "codebook.xlsx", sheetName = "Matrix", append = T, row.names = F, col.names = T)                    #make a sheet for the adjacency matrix                  #render excel sheet, omitting the NAs of the irregular length columns
write.xlsx(spots,file = "codebook.xlsx", sheetName = "Spots", append = T, showNA = F, row.names = F, col.names = T)                              #render excel sheet, omitting the NAs of the irregular length columns
write.xlsx(personality,file = "codebook.xlsx", sheetName = "Personality", append = T, showNA = F, row.names = F, col.names = T)                              #render excel sheet, omitting the NAs of the irregular length columns
write.xlsx(parameters,file = "codebook.xlsx", sheetName = "Parameters", append = T, showNA = F, row.names = F, col.names = T)                              #render excel sheet, omitting the NAs of the irregular length columns
}
