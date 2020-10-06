#This script loads in and sets up the physical space networks

library(readxl)
library(igraph)

setwd("C:/Research/projects/Jovan model")             #folder where the data are
floorPlan <- read_excel("network node codebook.xlsx",2)

#clean up data
floorPlan <- subset(floorPlan, select = -c(1))
floorPlan <- `rownames<-`(floorPlan, colnames(floorPlan))

#convert data into matrix
F1 <- data.matrix(floorPlan)

#create graph
F1Graph <- graph_from_adjacency_matrix(F1)

#read in node attractiveness list
node_attractiveness<-c(read_xlsx("network node codebook.xlsx", 1)[[3]])  #list of node attractiveness

########################      Make random network       ###########################################################################################################################3

#g1 <- sample_pa_age(50, pa.exp=1, aging.exp=0, aging.bin=1000)         #pa.exp=higher = more centrally bound/less dense (less nodes more integrated) ## aging only goes negative
#g2<-as.undirected(g1)                                                      #make all nodes two way
#g3 <- as_adjacency_matrix(g2)                                              #turn into adjacency matrix
#g4 <- graph_from_adjacency_matrix(g3)                                      #graph as per usual

