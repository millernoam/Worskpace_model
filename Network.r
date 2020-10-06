#This script loads in and sets up the physical space networks

library(readxl)
library(igraph)

setwd("C:/Research/projects/Jovan model")             #folder where the data are
floorPlan <- read_excel("Floor 1 Network.xlsx")     #read in the network

#clean up data
floorPlan <- subset(floorPlan, select = -c(1))
floorPlan <- `rownames<-`(floorPlan, colnames(floorPlan))

#convert data into matrix
F1 <- data.matrix(floorPlan)

#create graph
F1Graph <- graph_from_adjacency_matrix(F1)

#read in node attractiveness list
F1att <- read_xlsx("network node codebook.xlsx", 1)
F1nodeatt = as.list(F1att[3])[[1]]
