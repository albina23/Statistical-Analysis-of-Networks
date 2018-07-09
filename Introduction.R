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

