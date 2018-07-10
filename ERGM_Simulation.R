# Exploring ERGM through simularion in R and MPNet (Chapters 1-4 in Lusher et al., 2013)
# Subgraphs and null distributions and ERGM rationale

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
  "sna",
  'devtools'
))

###############################
# ERGM Simulation (Paget Dataset)
###############################
BusyNet <- as.matrix(read.table('./Data/PADGB.txt', header=FALSE) )
BusyNetNet <- network(BusyNet, directed=FALSE) # Convert to network object

g <- simulate(network(16, directed=FALSE) ~ edges, coef=c(-1.946), nsim=9) 

par(mfrow=c(2,5))
plot(BusyNetNet)
lapply(g,function(x) plot(x,main=dyad.census(x) ) )
plot(x,main=dyad.census(c))

# Simulate a model that has edges, two-stars, three-stars and triangles 
# On 9 graphs with the same number (16) of actors as the Padgett data

g.sim <- simulate(network(16, directed=FALSE)
                  ~ kstar(1:3) + triangles,
                  coef=c(-2.1113, 1.046,-0.6318 ,1.3064), nsim=9)

# Plot the simulated networks and compare them to the observed network
par(mfrow=c(2,5))
plot(BusyNetNet, main = triad.census(BusyNetNet,mode='graph') )
lapply(g.sim,function(x) plot(x,main=triad.census(x,mode='graph') ) ) 

#Calculate the triad census for the simulated networks 
# (0 - 1 star, 1 - 2 star, 2 - 3 star, 3 - triangle)
triad.census(g.sim,mode='graph')

# Do the degree distributions of the simulated networks observed networks?
par(mfrow=c(2,5))
plot(table(rowSums(BusyNet) ) )
lapply(g.sim,function(x) plot(table(rowSums(as.matrix.network(x)) ) ) )

# Estimate coefficients for 1-3 star and triangles
estim.obj <- ergm(BusyNetNet ~ kstar(1:3)+triangles)

GofPadg <- gof(estim.obj)
par(mfrow=c(1,3))
par(oma=c(0.5,2,1,0.5))
plot(GofPadg)

# Create a statistical summary of the estim.obj
# If |Estimate| > 2*Std. Error -> Coefficient is statistically significant
summary(estim.obj)

# Create goodness of fit estimates
mygof <- gof(estim.obj)
plot(mygof)
summary(mygof)
