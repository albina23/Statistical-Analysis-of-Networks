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
  "ergm"
))

###############################
# Importing the data
###############################
dt = fread("./Data/loan.csv")

g <- graph.data.frame(d = dt, directed = TRUE)
g <- delete.vertices(g, degree(g)==0)
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

# Convert the iGraph Object to a regular matrix
m <- as_adjacency_matrix(g, sparse = FALSE)

# Plot the graph
plot (g)

###############################
# Describing the network
###############################
# Plotting degree distribution:
plot(table(rowSums(dt)), ylab='number',xlab='degree')

###############################
# in/out Degree
###############################
dt <- dt[,ID:=NULL]
hist(rowSums(dt))
hist(colSums(dt))

dt_sums <- data.table(rowSums(dt))


