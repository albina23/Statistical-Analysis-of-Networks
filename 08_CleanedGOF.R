# Source: http://www.stats.ox.ac.uk/~snijders/siena/sienaGOF_new.R

###############################
# File Setup
###############################

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
# Function: Holland and Leinhardt Triad Census
###############################
TriadCensus <- function(i, data, sims, wave, groupName, varName, levls=1:16){
  unloadNamespace("igraph") # to avoid package clashes
  require(sna)
  require(network)
  x <- networkExtraction(i, data, sims, wave, groupName, varName)
  if (network.edgecount(x) <= 0){x <- symmetrize(x)}
  # because else triad.census(x) will lead to an error
  tc <- sna::triad.census(x)[1,levls]
  # triad names are transferred automatically
  tc
}

###############################
# Function: GeodesicDistribution
###############################
GeodesicDistribution <- function (i, data, sims, period, groupName,
                                  varName, levls=c(1:5,Inf), cumulative=TRUE, ...) {
  x <- networkExtraction(i, data, sims, period, groupName, varName)
  require(sna)
  a <- sna::geodist(symmetrize(x))$gdist
  if (cumulative)
  {
    gdi <- sapply(levls, function(i){ sum(a<=i) })
  }
  else
  {
    gdi <- sapply(levls, function(i){ sum(a==i) })
  }
  names(gdi) <- as.character(levls)
  gdi
}

###############################
# Importing the data
###############################

friendship <- sienaDependent(array(c(s501, s502, s503), dim=c(50, 50, 3)))
s50data <- sienaDataCreate(friendship)

# Inspect what has been created
s50data

# Now, give some estimation settings.
estimationSettings <- sienaAlgorithmCreate(projname='s50_GoF')

# included from getEffects()
model1 <- getEffects(s50data)

###############################
# Add transitive triplets effects -> Model 1
###############################
model1 <- includeEffects(model1, transTrip, name="friendship")
model1

#Run the Model
( results1 <- siena07(estimationSettings, data=s50data,
                      effects=model1, returnDeps=TRUE) )

# Now we go on to assess the fit with respect to the outdegree distribution.
gof1.od <- sienaGOF(results1, verbose=TRUE, varName="friendship",
                    OutdegreeDistribution)
gof1.od
plot(gof1.od)

###############################
# Add OUT-DEGREEE EFFECT
###############################
model1 <- includeEffects(model1, outActSqrt, name="friendship")
(results1 <- siena07(estimationSettings, data=s50data,
                     effects=model1, returnDeps=TRUE))

gof1.od <- sienaGOF(results1, verbose=TRUE, varName="friendship",
                    OutdegreeDistribution)
gof1.od
plot(gof1.od)

###############################
# Add TRANSITIVE RECIPROCATED TRIADS
###############################
model1 <- includeEffects(model1, transRecTrip,cycle3, name="friendship")

(results1 <- siena07(estimationSettings, data=s50data,
                     effects=model1, returnDeps=TRUE))
# Run it again
(results1 <- siena07(estimationSettings, data=s50data,
                     effects=model1, returnDeps=TRUE, prevAns = results1))

gof1.tc <- sienaGOF(results1, verbose=TRUE,
                    varName="friendship", TriadCensus)
gof1.tc
plot(gof1.tc, scale=TRUE, center=TRUE)

###############################
# Run GOF for GeodesicDistribution
###############################
gof1.gd <- sienaGOF(results1, verbose=TRUE,
                    varName="friendship", GeodesicDistribution)
gof1.gd
plot(gof1.gd)

###############################
# Add gwespFF effect -> Model 2
###############################
model2 <- setEffect(model1, cycle3, fix=TRUE, test=TRUE, include=TRUE)
model2 <- setEffect(model2, gwespFF, fix=TRUE, test=TRUE, include=TRUE, parameter=69)
model2 <- setEffect(model2, transTies, fix=TRUE, test=TRUE, include=TRUE)
model2 <- setEffect(model2, transRecTrip, fix=TRUE, test=TRUE, include=TRUE)
model2

( results2 <- siena07(estimationSettings, data=s50data,
                      effects=model2, returnDeps=TRUE) )

summary(results2)

###############################
# Add more effects -> Model 3
###############################

model3 <- includeEffects(model2, transTies)
( results3 <- siena07(estimationSettings, data=s50data,
                      effects=model3, returnDeps=TRUE, prevAns=results2) )
# If any convergence t-ratios for the estimated effects are
# greater than 0.1 in absolute value, we have to estimate again,
# now using prevAns=results3:
( results3 <- siena07(estimationSettings, data=s50data,
                      effects=model3, returnDeps=TRUE, prevAns=results3) )
summary(results3)

# Let us also add the transitive reciprocated triplets effect
model4 <- includeEffects(model3, transRecTrip)
( results4 <- siena07(estimationSettings, data=s50data,
                      effects=model4, returnDeps=TRUE, prevAns=results3) )
summary(results4)

# The 3-cycle effect still is significant; we add it, too:
model5 <- includeEffects(model4, cycle3)
( results5 <- siena07(estimationSettings, data=s50data,
                      effects=model5, returnDeps=TRUE, prevAns=results4) )
summary(results5)

###############################
# Evaluate Model 5
###############################

# For the new model, we apply the same goodness of fit tests.
(gof5.id <- sienaGOF(results5, verbose=TRUE,
                     varName="friendship", IndegreeDistribution))
plot(gof5.id)

(gof5.od <- sienaGOF(results5, verbose=TRUE, varName="friendship",
                     OutdegreeDistribution))
plot(gof5.od)

(gof5.gd <- sienaGOF(results5, verbose=TRUE,
                     varName="friendship", GeodesicDistribution))
plot(gof5.gd)

(gof5.tc <- sienaGOF(results5, verbose=TRUE,
                     varName="friendship", TriadCensus))
plot(gof5.tc, scale=TRUE, center=TRUE)

