setwd("C:/Research/projects/Jovan model/model files")  #enter the path to all files

mynet = "network node codebook new.xlsx"              #enter file name for network

source("Network.R")
source("functions.R")

lapply(1:10, function(n)source("runfilenew.R"))

########################      Make random network       ###########################################################################################################################3

#g1 <- sample_pa_age(50, pa.exp=1, aging.exp=0, aging.bin=1000)             #pa.exp=higher = more centrally bound/less dense (less nodes more integrated) ## aging only goes negative
#g2 <- as.undirected(g1)                                                    #make all nodes two way
#g3 <- as_adjacency_matrix(g2)                                              #turn into adjacency matrix
#g4 <- graph_from_adjacency_matrix(g3)                                      #graph as per usual
