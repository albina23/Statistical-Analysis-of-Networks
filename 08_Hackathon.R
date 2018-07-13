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

Mat <- dt_all[3:88, 2:56]

###############################
# Convert it to a bipartitie network
###############################
g = igraph::graph.incidence(Mat)

bipartite.mapping(g)

V(g)$type 

plot_bi_graph <- function(g){
  shape = ifelse(V(g)$type, "circle", "square") # assign shape by node type
  col = ifelse(V(g)$type, "red", "yellow") # assign color by node type
  plot(g, vertex.shape = shape, vertex.color = col,vertex.label.cex = 0.5, vertex.size =3 )
}

plot_bi_graph(g)

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

###############################
# Johan's Code - Affiliations
###############################
# Load the data
Aff <- read.csv("./Data/noordin_big.csv",stringsAsFactors=FALSE)
head(Aff) #The first 2 rows are variable names
Aff[2,] 
Aff[3,] # Row 3 is data
Aff[,1] # Column 1 contains people names
dim(Aff) # Find out how many rows and columns we should take
AffMat <- Aff[3:81,2:51]

# Read in the matrix, only select data, no variables
AffMat <- as.matrix(dt_all[3:88, 2:56])
nPeople <- nrow(AffMat)
nOps <- ncol(AffMat)

# Plot to take a look
gm = igraph::graph.incidence(AffMat)
plot_bi_graph(gm)

# Ensure numbers are interpretated as strings
class(AffMat[1,2])
as.numeric(AffMat[1,2])
AffMat<- mapply(AffMat, FUN=as.numeric)
AffMat <- matrix(data=AffMat, nrow=nPeople, ncol=nOps)
dim(AffMat)

# Check the degree distribution of people
par(mfrow=c(1,2))
plot(table( rowSums(AffMat)), main='people degree')

# Check the degree distribution of operations
plot(table( colSums(AffMat)), main='operation degree')
BigAdj <- cbind(matrix(0,nPeople,nPeople),AffMat)
BigAdjBottom <- cbind(t(AffMat),matrix(0,nOps,nOps))
BigAdj <- rbind(BigAdj,BigAdjBottom)
colours <- matrix('red',nPeople+nOps,1)
colours[1:nPeople,1] <- 'blue'
plot(as.network(BigAdj,directed=FALSE),vertex.col=colours)

# Save for PNet
write.table(AffMat,file='./Data/affiliation.txt',sep=' ',row.names=FALSE,col.names=FALSE)

###############################
# Johan's Code - Friendship
###############################
# Read in the Friendship matrix
Friend <- read.csv("./Data/friendship.csv",stringsAsFactors=FALSE)# a dataframe
class(Friend)
head(Friend)
dim(Friend)

# Firs column has names
Friend[,1] # Are they in the same order as for the affiliations?
cbind(Friend[,1],Aff[3:81,1])

# Extract adjacency matrix:
ADJ <- as.matrix(Friend[1:nPeople,2:(nPeople+1)])
plot(as.network(ADJ,directed=FALSE))

# Save to PNet
write.table(ADJ,file='./Data/friendship.txt',sep=' ',row.names=FALSE,col.names=FALSE)

# Add the friendship matrix to the BIG MATRIX
BigAdj[1:nPeople,1:nPeople] <- ADJ
# plot the two types of ties:
plot(as.network(BigAdj,directed=FALSE),vertex.col=colours)
