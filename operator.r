mydir <<- "C:/Research/projects/Jovan model/model2"  #enter the path to all files
mynet <<- "network node codebook n2.xlsx"              #enter file name for network

setwd(mydir)
source("Network.R")
source("functions.R")

lapply(1:2, function(n)source("runfilenew.R"))
