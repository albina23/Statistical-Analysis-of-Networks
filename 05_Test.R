###############################
# File Setup
###############################

# Set working directory
setwd("~/MethodsR/Statistical-Analysis-of-Networks") # REPLACE WITH OWN WORKING DIRECTORY

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
  "sna",
  "ergm"
))

###############################
# Importing the data
###############################

# Import the dataset using data.table and remove the rows with incomplete data
dt = fread("./Data/RBtestdata.csv") # REPLACE WITH OWN FILE PATH
dt <- na.omit(test_data , cols='From')

# Create an iGraph Object
g <- graph.data.frame(d = dt, directed = TRUE)

# Convert the iGraph Object to a regular matrix
m <- as_adjacency_matrix(g, sparse = FALSE)

# Plot the matrix using as.network from the network package
plot(as.network(m))

###############################
# Describing the network
###############################

# Plotting degree distribution:
plot(table(rowSums(m)), ylab='number',xlab='degree')

# Find the total number of ties
sum(m)/2

# Calculating Density
gden(m) 

# Calculating Geodesic distances 
Distances <- geodist(m)
Distances$gdist #Print geodesic matrix
Distances$counts #of geodecis
table( Distances$gdist[ lower.tri(Distances$gdist)] ) # Tabulate the distances
plot(table( Distances$gdist[ lower.tri(Distances$gdist) ] ) ) # Plot the distances

# Triad Census
triad.census(m, mode='digraph')
gtrans(m) # Transitivty

# Dyad Census
dyad.census(m)



