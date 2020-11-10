mydir <<- "C:/Users/nmiller/Desktop/Jovan model"  #enter the path to all files
mynet <<- "network node codebook n2.xlsx"              #enter file name for network

setwd(mydir)
source("Network.R")
source("functions.R")

lapply(1:10, function(n)source("runfilenew.R"))
