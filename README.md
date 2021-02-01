# Worskpace_model

Current repository contains all the necessary components for running the workplace simulation Jovan Poposki and Dr. Noam Miller created. This program simulates an agent-based workplace where agents move on a node-based network where nodes represent spaces and vertices represent hallways or means of getting from node to node. There are 5 states agents can be in (working, socializing, eating, relaxing, and walking) with 500 timesteps to imitate an 8-hour workday (just about one timestep per minute). Agent’s state is determined by a transition matrix at every timestep based on certain parameters that are always being monitored. Such parameters include time elapsed in current state, time of day, personality, hunger, and bladder, # of workspaces. 
Parameters that can be changed include the mean and SD of the distribution from which the personality traits have been randomly assigned, number of replications, number of days per replication, number of agents, alpha (magnitude of change in attractiveness for social/relax spots), amount of influence length of path has on attractiveness of paths. 
What is being extracted from the simulation is the personality of each agent along with their workspace, happiness of each agent, attractiveness of social/relax spots for each agent, history of states and nodes agents were in and time elapsed in each state. 
List of files and their descriptions: 
-	Operator
o	Runs all the files necessary for the simulation
-	Network
o	Reads in the adjacency matrix that makes up the floor plan that the agents move on
-	Analysis
o	Contains functions that visualize the network, show the agent’s personality along with their workstation and distributions of time in sate for all agents (new network function in here too but it will be in another file)
-	Runfilenew
o	The engine of the code
-	Network node codebook
o	Contains descriptions of each node, adjacency matrix, spot allocations (social/relax spots, workstations, kitchen, washroom, outside), # of  timesteps to run in each replication, # of days for each replication, # of agents, alpha, and the magnitude of influence of length of path on attractiveness of path (choice of path)
-	Dataframe
o	Program that creates a data frame with the output data including personality, happiness, attractiveness of social/relax spots, proportion of time spent in states
o	To use this program, make sure the directory is set to a folder containing subfolders with levels of the IV
To use this simulation, open the operator file and make sure to input the directory of the R files. 
