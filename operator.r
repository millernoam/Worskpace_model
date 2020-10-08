setwd("C:/Users/Jovan/Documents/Model split up")  #enter the path to all files

mynet = "network node codebook.xlsx"              #enter file name for network

source("Network.R")
source("functions.R")

lapply(1:10, function(n)source("runfilenew.R"))
