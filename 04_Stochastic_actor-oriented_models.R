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

###############################
# Simulating SAOM with Rate & Density
###############################
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

n <- dim(tmp4)[1]
mySimNets <- reshapeRSienaDeps(sim_ans,n)     
par(mfrow=c(2,5))
plot(as.network(tmp4),coord=coordin,main=paste('ties: ',sum(tmp4) ) ) 
apply(mySimNets,1,function(x) plot(as.network(x),coord=coordin,main=paste('ties: ',sum(x))) )

# detach(package: igraph)
dyad.census(mySimNets)
dyad.census(tmp4)

###############################
# Reciprocity Effect
###############################
dyad.census(tmp4)  
dyad.census(mySimNets)

triad.census(tmp4)
triad.census(mySimNets)

#Add effects and check if adding a ‘bias’ for reciprocity makes the simulated networks more realistic
myeff <- includeEffects(myeff, recip,include=TRUE)
myeff$initialValue[myeff$shortName=='Rate'] <- 4.2525
myeff$initialValue[myeff$shortName=='density'][1] <- -1.4163
myeff$initialValue[myeff$shortName=='recip'][1] <- 1.1383 # NEW EFFECT
sim_model  <-  sienaAlgorithmCreate( projname = 'sim_model', cond = FALSE, useStdInits = FALSE, nsub = 0, n3 = 9 , simOnly = TRUE)
sim_ans <- siena07( sim_model, data = mydata, effects = myeff,
                    returnDeps = TRUE,  batch=TRUE )

mySimNets <- reshapeRSienaDeps(sim_ans,n)

# Recalculate the dyad-census
dyad.census(mySimNets)

# Also check the triad-census
triad.census(mySimNets)[,16]

###############################
# TransTrip Effect added
###############################

# Is it OK? What effect should we include to improve?
myeff <- includeEffects(myeff, recip, transTrip, include=TRUE)

myeff$initialValue[myeff$shortName=='Rate'] <- 4.5017
myeff$initialValue[myeff$shortName=='density'][1] <- -1.9024
myeff$initialValue[myeff$shortName=='recip'][1] <- 0.6794
myeff$initialValue[myeff$shortName=='transTrip'][1] <- 0.3183 # NEW EFFECT

sim_model  <-  sienaAlgorithmCreate( projname = 'sim_model', cond = FALSE, useStdInits = FALSE, nsub = 0, n3 = 9 , simOnly = TRUE)
sim_ans <- siena07( sim_model, data = mydata, effects = myeff,
                    returnDeps = TRUE,  batch=TRUE )

# Let's test the model again to see the impact of a 4th effect
mySimNets <- reshapeRSienaDeps(sim_ans,n)
dyad.census(tmp4)  
dyad.census(mySimNets)
triad.census(tmp4)
triad.census(mySimNets)

###############################
# Estimation to get effect values
###############################
est_model  <-  sienaAlgorithmCreate( projname = 'est_model')
sim_ans <- siena07( est_model, data = mydata, effects = myeff)
summary(sim_ans)
