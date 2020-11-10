#This script loads in and sets up the physical space networks

library(readxl)
library(igraph)

#read in floor plan and clean up data
floorPlan <- read_excel(mynet, 2)
floorPlan <- `rownames<-`(floorPlan, colnames(floorPlan))
F1 <- data.matrix(floorPlan)  #convert data into matrix

#create graph
F1Graph <- graph_from_adjacency_matrix(F1)

#read in node attractiveness list
node_attractiveness <<- c(read_xlsx(mynet, 1)[[3]])  #list of node attractiveness

#read in other parameters
ws_spots = c(read_excel(mynet,3)[[1]])        #list of workstations
kitchen_spots = c(read_excel(mynet,3)[[2]])    #kitchen
sr_spots = c(read_excel(mynet,3)[[3]])        #socializing/relaxing spots
wc_spots = c(read_excel(mynet,3)[[4]])       #washroom
out_spots = c(read_excel(mynet,3)[[5]])       #outside spots
 
ws_spots = ws_spots[!is.na(ws_spots)]
kitchen_spots = kitchen_spots[!is.na(kitchen_spots)]
sr_spots = sr_spots[!is.na(sr_spots)]
wc_spots = wc_spots[!is.na(wc_spots)]
out_spots = out_spots[!is.na(out_spots)]

total_time = (c(read_excel(mynet,5)[[1]]))              #length of the day
numagents = (c(read_excel(mynet,5)[[2]]))               #number of agents
num_days = (c(read_excel(mynet,5)[[3]]))                #number of days to run
alpha = (c(read_excel(mynet,5)[[4]]))                   #learning rate
attrade = (c(read_excel(mynet,5)[[5]]))                 #attractiveness/distance tradeoff

foodpersmean = (c(read_excel(mynet,4)[[2]][3]))             #Mean of foodiness
foodperssd = (c(read_excel(mynet,4)[[3]][3]))               #SD of foodiness
socpersmean = (c(read_excel(mynet,4)[[2]][1]))              #Mean of sociability
socperssd = (c(read_excel(mynet,4)[[3]][1]))                #SD of sociability
workpersmean = (c(read_excel(mynet,4)[[2]][2]))             #Mean of work ethic
workperssd = (c(read_excel(mynet,4)[[3]][2]))               #SD of work ethic
bladpersmean = (c(read_excel(mynet,4)[[2]][4]))             #Mean of bladdercap
bladperssd = (c(read_excel(mynet,4)[[3]][4]))               #SD of bladdercap