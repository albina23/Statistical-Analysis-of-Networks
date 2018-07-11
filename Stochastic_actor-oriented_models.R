# Simulating time 2 from time 1, Two continuousQtime models: LERGM (Ch. 11, Lusher et al., 2013;  Koskinen & Lomi, 2013), 
# and SAOM (Snijders, Steglich, and van de Bunt, 2010; RSiena webQportal;  Ripley et al., 2014). 

# Specifying effects in SAOM  SAOM (Snijders, Steglich, and van de Bunt, 2010; RSiena webQportal; Ripley et al., 2014) and  
# estimation (Snijders, Steglich, and van de Bunt, 2010; Snijders, Koskinen, Schweinberger, 2010;  Koskinen and Snijders, 2007) 

# https://www.stats.ox.ac.uk/~snijders/siena/

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
  "ergm"
))

# Load package and data 
tmp4[is.na(tmp4)] <- 0 # remove missing
par(mfrow = c(1,2))
coordin <- plot(as.network(tmp3))
plot(as.network(tmp4),coord=coordin)

#Let us simulate a model that only ‘cares’ about density to see how well this 
# replicates the observation at t1.
mynet1 <- sienaDependent(array(c(tmp3, tmp4), dim=c(32, 32,2)))
mydata <- sienaDataCreate(mynet1)
myeff <- getEffects(mydata)
myeff <- includeEffects(myeff, recip,include=FALSE)
myeff$initialValue[myeff$shortName=='Rate'] <- 3.8311
myeff$initialValue[myeff$shortName=='density'][1] <- -1.1059

sim_model  <-  sienaAlgorithmCreate( projname = 'sim_model', cond = FALSE, useStdInits = FALSE, nsub = 0 , n3 = 9 , simOnly = TRUE)
sim_ans <- siena07( sim_model, data = mydata, effects = myeff,
                    returnDeps = TRUE,  batch=TRUE )

# Author: Johan Koskinen 
reshapeRSienaDeps <- function(sim_ans,n) {
  numSimulated <- length(sim_ans$sims)
  mySimNets <- array(0,dim=c(numSimulated,n,n))
  theseNets <- seq(1,numSimulated)
  for (i in c(1:numSimulated ) )
  {
    adj <- matrix(0, n, n)
    # Make shorter notation for edge list
    edges <- sim_ans$sims[[theseNets[i]]][[1]][[1]][[1]] # put edge values in desired places
    adj[edges[, 1:2]] <- edges[, 3]
    mySimNets[i,,] <- adj
  }
  mySimNets
}

n <- dim(tmp4)[1]
reshapeRSienaDeps(sim_ans, n)





