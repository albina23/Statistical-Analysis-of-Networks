# Set working directory
setwd("~/MethodsR/Statistical-Analysis-of-Networks")

# Install packages
install_package <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

install_package(c(
  "dplyr",
  "igraph",
  "data.table",
  "RSiena",
  "network",
  "sna"
))

###############################
# High Tribe Example
###############################

#Read in data: HighTribe
HighTribe <- read.table('./Data/RedHighlandTribes.txt')
HighTribePos <- HighTribe[1:16,] #all columns, only rows from 1 to 16
rowSums(HighTribePos) #Nodal degrees
table(rowSums(HighTribePos)) #Degree distribution

#Plotting degree distribution:
plot(table(rowSums(HighTribePos)), ylab='number',xlab='degree')

#Find the total number of ties
sum(HighTribePos)/2

#Plotting network:
HighNet <- as.network(HighTribePos, directed= FALSE)
plot(HighNet)

#Calculating Density
n <- dim(HighTribePos)
sum(HighTribePos)/(n*(n-1)) #24% of all possible ties exist in the network

gden(HighNet) #Easy function to find density from SNA package

#Geodesic distances 
Distances <- geodist(HighNet)
Distances$gdist #Print geodesic matrix
Distances$counts #of geodecis
table( Distances$gdist[ lower.tri(Distances$gdist)] ) #To tabulate:
plot(table( Distances$gdist[ lower.tri(Distances$gdist) ] ) ) #Plot

#Triad (SNA used mode ='graph' OR 'di-graph' where Network uses T/F for directed.)
#For any three nodes in the network, it can either have 0-3 ties.
tricen <- triad.census(HighNet) #not telling the function that it's undirected
tricen <- triad.census(HighNet, mode='graph')
tricen[4]*3/(tricen[3]+3*(tricen[4])) #calculate the density of triads, roughly 63%
gtrans(HighNet) #easy function to compute above

#Dyad Census
dyad.census(HighNet) #not interesting since undirected

###############################
# Zachary Example
###############################

#Read in data: Zachary
Zachary <- read.table('./Data/zachary.txt')

#Plotting network:
aNet <- as.network(Zachary[1:34,] , directed= FALSE)
plot(aNet)
plot(table(rowSums(Zachary)),ylab='') #Plotting degree distribution:

gden(aNet) 


###############################
# s-50 data set Example
###############################
#Read in data and convert to matrix format
friend.data.w1 <- read.table("./Data/s50-network1.dat")
friendMat <- as.matrix(friend.data.w1)

plot(as.network(friendMat)) #Plotting network

#Calculate the ‘dyad census’
#Adding up the total number gives the number of directed pairs
#Transitive vs Cyclical traids
dyad.census(friendMat)
triad.census(friendMat) #Holland & Lienhart -> U=Up D=Down T=Transitive C=Cyclical
gden(friendMat)
plot(table(colSums(friendMat))) #in-degree distribution

#Create a random graph
g <- rgraph(50) #Not a meaninful comparison if it's exactly random, might be worth having same # of nodes and density
g <-rgraph(50, tprob= gden(friendMat))
class(g) #it's a matrix!
plot(as.network(g))
dyad.census(g)
triad.census(g)
gden(g)
plot(table(colSums(g))) #in-degree distribution

par(mfrow=c(1,2))

