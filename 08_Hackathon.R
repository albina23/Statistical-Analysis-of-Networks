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
dt_all = fread("./Data/noordin_big.csv")
dt_traning = fread("./Data/noordin_training.csv") 
dt_meetings = fread("./Data/noordin_meetings.csv") 
dt_operations = fread("./Data/noordin_operations.csv") 

dt_all[1,] # Events
dt_all[2,] # Variable
dt_all[,1] # People

AffMat <- dt_all[3:88, 2:56]

###############################
# Convert it to a bipartitie network
###############################
g = igraph::graph.incidence(AffMat)

bipartite.mapping(g)

V(g)$type 

# Plot the network
shape = ifelse(V(bg)$type, "circle", "square") # assign shape by node type
col = ifelse(V(bg)$type, "red", "yellow") # assign color by node type

plot(bg, vertex.shape = shape, vertex.color = col,vertex.label.cex = 0.2, vertex.size =3 )

###############################
# Describing the network
###############################
# Convert the iGraph Object to a regular matrix
m <- as_adjacency_matrix(g, sparse = FALSE)

# Plot the matrix using as.network from the network package
plot(as.network(m))

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
