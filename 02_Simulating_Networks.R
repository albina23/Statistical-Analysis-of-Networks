# Exploring ERGM through simularion in R and MPNet (Chapters 1-4 in Lusher et al., 2013)

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


###############################
# Observed(friendMat) vs Random
###############################

# Comparing the in-degrees of random vs observed
par(mfrow=c(1,2))
plot(table(colSums(friendMat)))
plot(table(colSums(g[1,,])), xlim=range(colSums(friendMat))) #(g[1,,]) (First slice, all rows, all columns)

# Unifrom MAN
num_rand <- 1 # How many random grapgs to generate.
dyad.census(friendMat) # Run to find out how many to spexify
g <- rguman(num_rand,50, mut =39 , asym =35 , nul =1151 , method = 'exact') # Exact makes the same dyad census

# We observe that both grapgs have the same density
gden(g)
gden(friendMat)

# Why use exact?
# Produces the same dyad census as observed, eventually will converge enough random graphs are generated
num_rand <- 1
dyad.census(friendMat) # Run to find out how many to spexify
g <- rguman(num_rand,50, mut =39 , asym =35 , nul =1151) # Exact makes the same dyad census
hist(dyad.census(g)[,1])

# Let's compare the exact random versus observed
par(mfrow=c(1,2))
plot(as.network(g), main = 'Exact Random Graph')
plot(as.network(friendMat), main = 'Friend Mat Graph') 

# The triad census is still different
triad.census(g)
triad.census(friendMat)

# Exploring the different amount of traids
num_rand <- 1000
g <- rguman(num_rand,50, mut =39 , asym =35 , nul =1151) # Exact makes the same dyad census
BigTriad <- triad.census(g)

# 030T
hist(BigTriad[,9],xlab="030T", xlim=c(0,5)) # Observed.030T = 5
sum(BigTriad[,9]>=5)/1000 # Calculate p-val as proporition

# Triad Checker
name = '120C'
position = 14
hist(BigTriad[,position],xlab=name, xlim=c(0,5), main = name) # Observed.120U = 5
sum(BigTriad[,position]>=2)/1000 # 20% of graphs have at least greater than 2 traids


