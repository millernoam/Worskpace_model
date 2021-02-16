mydir <<- "C:/Users/nmiller/Desktop/Jovan model"  #enter the path to all files
mynet <<- "network node codebook n2.xlsx"              #enter file name for network

setwd(mydir)
source("Network.R")
source("functions.R")

lapply(1:10, function(n)source("runfilenew.R"))




############## Rip data out of simulation output ###################
#make sure you 

codebookdir <<- ""            #directory of the codebook
conditiondir <<- ""           #directory of the condition you want to rip data from


#change directory of condition and run again, excel file rendered in the directory of the condition for that condition

source("dataframe.R")
dataframe(conditiondir)
