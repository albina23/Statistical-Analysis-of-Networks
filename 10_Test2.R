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
dt = fread("./Data/loan.csv")

g <- graph.data.frame(d = dt, directed = TRUE)

# Convert the iGraph Object to a regular matrix
m <- as_adjacency_matrix(g, sparse = FALSE)

# Plot the matrix using as.network from the network package
plot(as.network(m))

###############################
# Describing the network
###############################
# Plotting degree distribution:
plot(table(rowSums(g)), ylab='number',xlab='degree')



###############################
# in/out Degree
###############################
dt <- dt[,ID:=NULL]
hist(rowSums(dt))
hist(colSums(dt))

dt_sums <- data.table(rowSums(dt))


