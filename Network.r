#This script loads in and sets up the physical space networks

library(readxl)
library(igraph)

#read in floor plan and clean up data
floorPlan <- read_excel(mynet, 2)
floorPlan <- `rownames<-`(floorPlan, colnames(floorPlan))
F1 <- data.matrix(floorPlan)  #convert data into matrix

#create graph
F1Graph <- graph_from_adjacency_matrix(F1)

#read in node attractiveness list
node_attractiveness<-c(read_xlsx(mynet, 1)[[3]])  #list of node attractiveness

#read in other parameters
ws_spots = na.omit(c(read_excel(mynet,3)[[1]]))        #list of workstations
kitchen_spot = na.omit(c(read_excel(mynet,3)[[2]]))    #kitchen
sr_spots = na.omit(c(read_excel(mynet,3)[[3]]))        #socializing/relaxing spots
wc_spot = na.omit(c(read_excel(mynet,3)[[4]]))         #washroom
out_spots = na.omit(c(read_excel(mynet,3)[[5]]))       #outside spots

total_time = (c(read_excel(mynet,5)[[1]]))              #length of the day
numagents = (c(read_excel(mynet,5)[[2]]))               #number of agents
num_days = (c(read_excel(mynet,5)[[3]]))                #number of days to run

foodmean = (c(read_excel(mynet,4)[[2]][3]))             #Mean of foodiness
foodsd = (c(read_excel(mynet,4)[[3]][3]))               #SD of foodiness
socmean = (c(read_excel(mynet,4)[[2]][1]))              #Mean of sociability
socsd = (c(read_excel(mynet,4)[[3]][1]))                #SD of sociability
workmean = (c(read_excel(mynet,4)[[2]][2]))             #Mean of work ethic
worksd = (c(read_excel(mynet,4)[[3]][2]))               #SD of work ethic
bladmean = (c(read_excel(mynet,4)[[2]][4]))             #Mean of bladdercap
bladsd = (c(read_excel(mynet,4)[[3]][4]))               #SD of bladdercap

########################      Make random network       ###########################################################################################################################3

#g1 <- sample_pa_age(50, pa.exp=1, aging.exp=0, aging.bin=1000)             #pa.exp=higher = more centrally bound/less dense (less nodes more integrated) ## aging only goes negative
#g2 <- as.undirected(g1)                                                    #make all nodes two way
#g3 <- as_adjacency_matrix(g2)                                              #turn into adjacency matrix
#g4 <- graph_from_adjacency_matrix(g3)                                      #graph as per usual

