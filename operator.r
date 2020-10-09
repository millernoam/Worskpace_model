setwd("C:/Research/projects/Jovan model/model files")  #enter the path to all files

mynet = "network node codebook new.xlsx"              #enter file name for network

source("Network.R")
source("functions.R")

lapply(1:10, function(n)source("runfilenew.R"))
