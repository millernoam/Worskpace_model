#function to find absence of an item from a list
`%notin%` <- Negate(`%in%`)  


#User-defined parameters------------------------------------------

ws_spots = na.omit(c(read_excel("network node codebook.xlsx",3)[[1]]))        #list of workstations
kitchen_spot = na.omit(c(read_excel("network node codebook.xlsx",3)[[2]]))    #kitchen
sr_spots = na.omit(c(read_excel("network node codebook.xlsx",3)[[3]]))        #socializing/relaxing spots
wc_spot = na.omit(c(read_excel("network node codebook.xlsx",3)[[4]]))         #washroom
out_spots = na.omit(c(read_excel("network node codebook.xlsx",3)[[5]]))       #outside spots

total_time = (c(read_excel("network node codebook.xlsx",5)[[1]]))              #length of the day
numagents = (c(read_excel("network node codebook.xlsx",5)[[2]]))               #number of agents
num_days = (c(read_excel("network node codebook.xlsx",5)[[3]]))                #number of days to run

foodmean = (c(read_excel("network node codebook.xlsx",4)[[2]][3]))             #Mean of foodiness
foodsd = (c(read_excel("network node codebook.xlsx",4)[[3]][3]))               #SD of foodiness
socmean = (c(read_excel("network node codebook.xlsx",4)[[2]][1]))              #Mean of sociability
socsd = (c(read_excel("network node codebook.xlsx",4)[[3]][1]))                #SD of sociability
workmean = (c(read_excel("network node codebook.xlsx",4)[[2]][2]))             #Mean of work ethic
worksd = (c(read_excel("network node codebook.xlsx",4)[[3]][2]))               #SD of work ethic
bladmean = (c(read_excel("network node codebook.xlsx",4)[[2]][4]))             #Mean of bladdercap
bladsd = (c(read_excel("network node codebook.xlsx",4)[[3]][4]))               #SD of bladdercap