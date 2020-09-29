library(readxl)
library(igraph) 
setwd("C:/Research/projects/Jovan model")
Floor1 <- read_excel("Floor 1 Network.xlsx")
Floor2 <- read_excel("Floor 2 Network.xlsx")
Floor3 <- read_excel("Floor 3 Network.xlsx")
#clean up data
Floor1 <- subset(Floor1, select = -c(1))
Floor2 <- subset(Floor2, select = -c(1))
Floor3 <- subset(Floor3, select = -c(1))
Floor1 <- `rownames<-`(Floor1, colnames(Floor1))
Floor2 <- `rownames<-`(Floor2, colnames(Floor2))
Floor3 <- `rownames<-`(Floor3, colnames(Floor3))
#convert data into matrix
F1 <- data.matrix(Floor1)
F2 <- data.matrix(Floor2)
F3 <- data.matrix(Floor3)
#create graph
F1Graph <- graph_from_adjacency_matrix(F1)
F2Graph <- graph_from_adjacency_matrix(F2)
F3Graph <- graph_from_adjacency_matrix(F3)

#read in attractivenesses
F1att <- read_xlsx("network node codebook.xlsx", 1)
F1nodeatt = as.list(att[3])[[1]]  #list of node attractivenesses
