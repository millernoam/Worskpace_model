#this script runs the model

setwd("C:/Research/projects/Jovan model")  #folder where the data are

source("Network.R")
source("functions.R")

#run the model 10 times
lapply(1:10, function(n)source("runfilenew.R"))
