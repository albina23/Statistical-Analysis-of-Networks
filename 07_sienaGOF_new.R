################################################################################
## Model selection and RSiena                                                  #
## Script prepared by Josh Lospinoso and Tom Snijders                          #
## Date: January 1 2015                                                        #
## Version: 13                                                                 #
################################################################################

# In this script, we will load the s50 data (type ?s50 for more information)
# and go through an iteration of forward model selection. The idea is to specify
# a relatively simple model containing a conservative number of effects and then
# evaluate evidence for inclusion of more elaborate terms. This approach
# contrasts with backward model selection, which would entail inclusion of a
# large number of terms and subsequent evaluation for omission of terms.

# It is strongly emphasized here that theory
# should always guide model selection!
# We follow a data-driven approach, and do not imply that this is always best.
# But it often is a meaningful component of model building.
# In this script, our intent is to illustrate some of the tools that
# are available to you and not necessarily to provide a "cookie cutter" solution
# to fitting SAOMs to all datasets:
#
# 1) sienaGOF
# 2) The score-type test of Schweinberger
# 3) sienaTimeTest

# First, install "RSiena" or "RSienaTest".
# It is advisable to use the most recent version which can be obtained
# from R-Forge or from the Siena website, depending on your hardware
# (see http://www.stats.ox.ac.uk/~snijders/siena/siena_news.htm).
# The treatment of structural zeros has improved in more recent versions.

# Later, we will use the sna and network packages to show GOF functionality.
# If they are not installed yet, install them now:
# install.packages(c("network", "sna"))

# After these packages have been installed, you must tell R to load the
# RSiena or RSienaTest package:
library(RSiena) # or library(RSienaTest)

# First, specify the data.
# The s50 dataset is already loaded into the R environment once the
# library() function is called.
# See e.g. ?s501 for information on these data objects.

# Now we load three waves of 50 actors into a sienaDependent object.
friendship <- sienaDependent(array(c(s501, s502, s503), dim=c(50, 50, 3)))
s50data <- sienaDataCreate(friendship)
# Inspect what has been created:
s50data

# Now, give some estimation settings.
# Here we will use 4 phase 2 subphases and 1000 phase 3 iterations.
# These settings are the standard and work well for most datasets.
# They are the default values and therefore do not need to be
# specified explicitly.
estimationSettings <- sienaAlgorithmCreate(projname='s50_GoF')
# For publication-grade results, especially for stability of the
# standard errors, it is advisable to use more (e.g., 3000) phase 3 iterations.

# First we begin to build up a preliminary model selection.
# By default, this model will have density and reciprocity effects
# included from getEffects()
model1 <- getEffects(s50data)
# We add the transitive triplets effects.
model1 <- includeEffects(model1, transTrip, name="friendship")

# Check what is specified in model1:
model1

# We use the siena07() function to estimate the parameters of model1.
# To use the goodness of fit testing functionality, it is necessary
# to specify returnDeps=TRUE; this will include the simulated networks
# in the results object, and will permit goodness of fit checking later on.
( results1 <- siena07(estimationSettings, data=s50data,
                effects=model1, returnDeps=TRUE) )

# Estimate   Standard   Convergence 
# Error      t-ratio   
# 
# Rate parameters: 
# 0.1      Rate parameter period 1  6.4549  ( 1.1506   )             
# 0.2      Rate parameter period 2  5.1875  ( 0.8939   )             
# 
# Other parameters: 
# 1.  eval outdegree (density)     -2.6810  ( 0.1155   )   0.0511    
# 2.  eval reciprocity              2.4541  ( 0.1901   )   0.0297    
# 3.  eval transitive triplets      0.6239  ( 0.0751   )   0.0391    
# 
# Overall maximum convergence ratio:    0.0736 
# 
# 
# Total of 2185 iteration steps.


# If not all t-ratios for convergence are less than 0.10,
# then re-estimate using results1 as the new starting point:
# results1 <- siena07(estimationSettings, data=s50data,
#                effects=model1, returnDeps=TRUE, prevAns=results1)
# You always need to check this, and estimate repeatedly
# until converge is good!

# Note that putting the entire command between parentheses leads to
# printing the results obtained after the finish of computations on the screen.

# ADD OUT-DEGREEE EFFECT
model1 <- getEffects(s50data)
# We add the transitive triplets effects.
model1 <- includeEffects(model1, transTrip, name="friendship")


# For the purpose of illustrating the goodness of fit test,
# we first look at what the goodness of fit tells us.
# Look at the help page:
?sienaGOF
# and read especially the "Description" and the first couple of lines
# of the "Details".

# Let us calculate the fit with with respect to the indegree distribution.
# By specifying "verbose=TRUE" we get information on the screen telling us
# how far calculations have progressed.
# (You may see a note about "method with signature ?CsparseMatrix# (etc.)
# which you can ignore.)
gof1.id <- sienaGOF(results1, verbose=TRUE,
                    varName="friendship", IndegreeDistribution)
gof1.id
plot(gof1.id)
# The default for the InDegreeDistribution is to study the fit of the
# cumulative distribution of the indegrees in the network, summed over all waves
# except for the first, for indegrees up to 8.
# See the help page for sienaGOF if you wish to find out about other options.
# The plot shows the observed values as the red dots and numbers;
# e.g., the sum over waves 2 and wave 3 of the numbers of actors with indegree 0
# is 12; the sum of the numbers of actors with indegree at most 3 is 75.
# This applies to the observed data set.
# The rest of the plot refers to the simulated networks in Phase 3
# of the algorithm, where it is supposed to have converged
# (if t-ratios for convergence all are less than 0.10).
# The shapes are so-called violin plots, which combine box plots
# with smoothed density plots.
# The dotted band is a pointwise 90% relative frequency region
# calculated for the simulated data.
# The p-value shows that the indegree distribution is represented well.
# The plot shows the same, as the data are within the band.

# Now we go on to assess the fit with respect to the outdegree distribution.
gof1.od <- sienaGOF(results1, verbose=TRUE, varName="friendship",
                OutdegreeDistribution)
gof1.od
plot(gof1.od)
# Here also the p-value and the plot show that the
# outdegree distribution is well represented.

# This call of sienaGOF() uses the function OutdegreeDistribution(),
# which is called an auxiliary function.
# See
?sienaGOF
# and
?OutdegreeDistribution
# for further explanation.
# Pay particular attention to the parameter levls in Outdegreedistribution;
# if you wish to see more, or fewer, levels of the outdegree distribution,
# you can use this parameter, e.g., as follows:
gof1.od5 <- sienaGOF(results1, verbose=TRUE, varName="friendship", levls=0:5,
                OutdegreeDistribution)
gof1.od5
plot(gof1.od5)
# The use of the levls parameter in sienaGOF() is an example
# of the "..." argument mentioned in the "Usage" of sienaGOF().
# The "..." argument in a given function in R is an argument of
# another function called by this given function
# (here, the "given function" is sienaGOF(),
# and the "another function" is OutdegreeDistribution()).
# In this case the maximum observed degree is 5, which is in line with
# the observed value 100 in the cumulative plot for outdegrees 5 and higher;
# note that there are 50 actors, and the statistics are sums over 2 waves,
# so the highest value for the cumulative plots is 2*50 = 100.
# Some of the simulated networks do have outdegrees more than 5;
# in a more refined analysis for this data set,
#  where at most 5 friends were requested in the data collection,
# the parameter MaxDegree could be used in sienaAlgorithmCreate
# to force simulated networks to have no outdegrees larger than 5.

# If the fit of one or both degree distributions had been poor,
# it would have been good to include some degree-related effects
# (outdegree-activity, indegree-popularity, etc.);
# since the fit for the degree distributions is good,
# this is not directly necessary here.

# Another important set of statistics is the distribution of
# geodesic distances (shortest undirected path lengths between actors).
# We use package sna for calculating the geodesic distances.
# This is not built into RSiena (or RSienaTest) but the help page
# for sienaGOF-auxiliary shows how it can be done:
?"sienaGOF-auxiliary"
# (quotes needed because of the "-" in the function name)

# This help page contains the following auxiliary function
# to work with the geodesic distances:

################################################################################
# GeodesicDistribution calculates the distribution of directed
# geodesic distances; see ?sna::geodist
# The default for \code{levls} reflects that geodesic distances larger than 5
# do not differ appreciably with respect to interpretation.
# Note that the levels of the result are named;
# these names are used in the \code{plot} method.
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
################################################################################

# This function is not internal to RSiena/Test, therefore
# it must be input in the R session, to make it available.
# After having done this, we calculate the fit with respect to the
# distribution of geodesic distances:
gof1.gd <- sienaGOF(results1, verbose=TRUE,
                    varName="friendship", GeodesicDistribution)
gof1.gd
plot(gof1.gd)

# The fit is very poor, and from the plot we can conclude
# that (except for distances 1, i.e., direct ties)
# there are too many dyads in the simulated networks with small distances.
# Our simulations are "too connected".
# The numbers represented by the plot can be obtained from
descriptives.sienaGOF(gof1.gd, showAll=TRUE)
# where the argument "showAll=TRUE" is used to show also the column
# for the cumulative distribution function at Infinity,
# which is degenerate at the value of 5000, being the
# total number of pairs of nodes 50*50.

# Before we try to remedy this, let us also look at the
# fit for the triad census.
# This also goes by means of a function which you can find on the help page
# for sienaGOF-auxiliary.

################################################################################
# Holland and Leinhardt Triad Census; see ?sna::triad.census.
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
################################################################################

gof1.tc <- sienaGOF(results1, verbose=TRUE,
                    varName="friendship", TriadCensus)
# This is more time-consuming...
gof1.tc

# Since the triad counts are wildly different in average and scale,
# we plot them using the scale and center options:
plot(gof1.tc, scale=TRUE, center=TRUE)
# If you don't know the codes for the triads, google for them;
# e.g., http://www.stats.ox.ac.uk/~snijders/Trans_Triads_ha.pdf
# or appendix 24 in http://eprints.jcu.edu.au/1751/14/14Appendices_23-25.pdf

# The triad census also is unacceptable, although the
# marginal fit across the statistics is not too egregious
# for any particular configuration by itself.
# 111U is over-represented in the simulations, which suggests
# that the precise representation of transitivity is not adequate.

# At this point let us check that the poor fit is not due
# to time heterogeneity:
tt1 <- sienaTimeTest(results1)
summary(tt1)
# No it isn't.

# Let us now see how the model may be extended to obtain a better fit.
# We do this by a theory-guided exploration.
# In the first place, it is known that the tendency to transitive closure
# might depend less strongly on the number of indirect connections
# than represented by the transitive triplets effect
# (see, e.g., section 2.3.2 in
# Snijders, van de Bunt, and Steglich, Social Networks 2010);
# good alternatives may be the transitive ties effect or
# the geometrically weighted edgewise shared partner effect.
# We also know that the 3-cycle effect may be important
# as an inverse indication of local hierarchy (same reference).
# Further, Block (Social Networks, 2015) has shown that
# the interaction between reciprocity and transitivity may be important,
# and may be more important than the 3-cycle effect.
# Consult the manual for the definition of transRecTrip.

# Based on this, we do a forward model selection
# and use the score-type test to see which of these effects
# could improve the model; for the score-type test,
# see Section 8.2 of the RSiena manual, and
# - if you wish to know more of the mathematical background - Schweinberger
# (British Journal of Statistical and Mathematical Psychology, 2012).
# For the gwespFF effect, we use parameter=69 (see the manual);
# this is the default if we would add this effect using includeEffects(),
# but for setEffect we need to specify it explicitly.

model2 <- setEffect(model1, cycle3, fix=TRUE, test=TRUE, include=TRUE)
model2 <- setEffect(model2, gwespFF, fix=TRUE, test=TRUE, include=TRUE, parameter=69)
model2 <- setEffect(model2, transTies, fix=TRUE, test=TRUE, include=TRUE)
model2 <- setEffect(model2, transRecTrip, fix=TRUE, test=TRUE, include=TRUE)
model2

( results2 <- siena07(estimationSettings, data=s50data,
                effects=model2, returnDeps=TRUE) )

# The convergence t-ratios will be large for some of the fixed effects;
# this is not a problem at all, because the requirement that they
# should be small applies only to the estimated parameters, not the fixed ones.
# However, these printed results do not contain the score-type tests
# requested by "test=TRUE".
# Therefore we request more complete information.
# In the section "Generalised score test <c>", we will see the results.

summary(results2)

# The joint test has a chi-squared statistic of 16 with d.f.=4,
# (the precise chi-squared value will vary, depending on the randomness
# inherent in the simulation-based procedures),
# which is highly significant.
# Note that the tests for the four parameters
# are not controlled for each other, only for the estimated parameters!
# Based on this, we first include the transitive ties effect.
# For estimation, we use the previous result as the starting value
# (we could have done that in the previous run of siena07, too).

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

# For the new model, we apply the same goodness of fit tests.
(gof5.id <- sienaGOF(results5, verbose=TRUE,
                    varName="friendship", IndegreeDistribution))
plot(gof5.id)
# Still good.

(gof5.od <- sienaGOF(results5, verbose=TRUE, varName="friendship",
                OutdegreeDistribution))
plot(gof5.od)
# Still good.

(gof5.gd <- sienaGOF(results5, verbose=TRUE,
                    varName="friendship", GeodesicDistribution))
plot(gof5.gd)
# There still are too many short distances.
# But the p-value has increased from 0 to 0.001, which shows
# that at least the observation is near the region of simulated values.

(gof5.tc <- sienaGOF(results5, verbose=TRUE,
                    varName="friendship", TriadCensus))
plot(gof5.tc, scale=TRUE, center=TRUE)
# Improved but still not so good.
# Is there now evidence for time heterogeneity?
(tt5 <- sienaTimeTest(results5))
# No.

# As a next step, the three basic degree-related effects are added.

model6 <- includeEffects(model5, outAct, inPop, outPop)
model6

( results6 <- siena07(estimationSettings, data=s50data,
                effects=model6, returnDeps=TRUE, prevAns=results5) )
# In my case there were some high convergence t-ratios, so I estimated again:
( results6 <- siena07(estimationSettings, data=s50data,
                effects=model6, returnDeps=TRUE, prevAns=results6) )
# Indegree popularity is not significant, so I drop it.

model7 <- setEffect(model6, inPop, fix=TRUE, test=TRUE, include=TRUE)
( results7 <- siena07(estimationSettings, data=s50data,
                effects=model7, returnDeps=TRUE) )
( results7 <- siena07(estimationSettings, data=s50data,
                effects=model7, returnDeps=TRUE, prevAns=results7) )
summary(results7)

# For the new model, we apply the same goodness of fit tests.
(gof7.id <- sienaGOF(results7, verbose=TRUE,
                    varName="friendship", IndegreeDistribution))
plot(gof7.id)
# Still good.

(gof7.od <- sienaGOF(results7, verbose=TRUE, varName="friendship",
                OutdegreeDistribution))
plot(gof7.od)
# Still good.

(gof7.gd <- sienaGOF(results7, verbose=TRUE,
                    varName="friendship", GeodesicDistribution))
plot(gof7.gd)
# Much improved. There now are moderately too many short distances.

(gof7.tc <- sienaGOF(results7, verbose=TRUE,
                    varName="friendship", TriadCensus))
plot(gof7.tc, scale=TRUE, center=TRUE)
# Good.
# Is there now evidence for time heterogeneity?
(tt7 <- sienaTimeTest(results7))
# No.

# Let us see now if the gwespFF effect may replace
# the combination of transTrip and transTies;
model8 <- includeEffects(model7, gwespFF)
model8 <- setEffect(model8, transTies, fix=TRUE, test=TRUE, include=TRUE)
model8 <- setEffect(model8, transTrip, fix=TRUE, test=TRUE, include=TRUE)
# First estimate without the prevAns option, because the nonzero estimates
# for transTrip and transTies should not be used:
( results8 <- siena07(estimationSettings, data=s50data,
                effects=model8, returnDeps=TRUE) )
( results8 <- siena07(estimationSettings, data=s50data,
                effects=model8, returnDeps=TRUE, prevAns=results8) )
summary(results8)
# In this model, transitive triplets and transitive ties
# are not significant any more, so it seems that their
# joint effects are adequately represented by the gwespFF effect.
# Let us check the goodness of fit for this model.

# For the new model, we apply the same goodness of fit tests.
(gof8.id <- sienaGOF(results8, verbose=TRUE,
                    varName="friendship", IndegreeDistribution))
plot(gof8.id)
# Still good.

(gof8.od <- sienaGOF(results8, verbose=TRUE, varName="friendship",
                OutdegreeDistribution))
plot(gof8.od)
# Still good.

(gof8.gd <- sienaGOF(results8, verbose=TRUE,
                    varName="friendship", GeodesicDistribution))
plot(gof8.gd)
# On the low side but reasonable. There are moderately too many short distances.

(gof8.tc <- sienaGOF(results8, verbose=TRUE,
                    varName="friendship", TriadCensus))
plot(gof8.tc, scale=TRUE, center=TRUE)
# Good.
# Any time heterogeneity?
(tt8 <- sienaTimeTest(results8))
# No.

# Concluding:
# Significant effects are reciprocity, geometrically weighted edgewise shared partners,
# and 3-cycles (all positive), and transitive reciprocated triplets,
# outdegree-popularity, and outdegree-activity (all negative).
# These effects jointly give a good representation of the
# distributions of indegrees, outdegrees, and geodesic distances,
# and of the triad census.