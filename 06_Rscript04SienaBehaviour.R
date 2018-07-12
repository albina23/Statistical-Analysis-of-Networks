# Taken from: https://www.stats.ox.ac.uk/~snijders/siena/Rscript04SienaBehaviour.R

################################################################################
###
### ---- Rscript04SienaBehaviour.R: a script for the introduction to RSiena ----
###
###                         version June 15, 2015
################################################################################
#
# The introductory script is divided into the following script files:
# Rscript01DataFormat.R, followed by
# RScriptSNADescriptives.R, code for descriptive analysis of the data, and
# Rscript02SienaVariableFormat.R, that formats data and specifies the model, and
# Rscript03SienaRunModel.R, that runs the model and estimates parameters
# Rscript04SienaBehaviour.R, that illustrates an example of analysing the
# coevolution of networks and behaviour
# Written with contributions by Robin Gauthier, Tom Snijders, Ruth Ripley,
# and Johan Koskinen.
#

# Here is a short script for analysing the co-evolution of the
# friendship network and drinking behaviour for the 50 girls in the
# Teenage Friends and Lifestyle Study data
# (http://www.stats.ox.ac.uk/~snijders/siena/s50_data.zip), described in
# http://www.stats.ox.ac.uk/~snijders/siena/s50_data.htm

# Read in the adjacency matrices, covariates and dependent behavioural variable
# assuming data are in current working directory

        friend.data.w1 <- as.matrix(read.table("./Data/s50_data/s50-network1.dat")) # network
        friend.data.w2 <- as.matrix(read.table("./Data/s50_data/s50-network2.dat"))
        friend.data.w3 <- as.matrix(read.table("./Data/s50_data/s50-network3.dat"))
        drink <- as.matrix(read.table("./Data/s50_data/s50-alcohol.dat")) # behaviour
        smoke <- as.matrix(read.table("./Data/s50_data/s50-smoke.dat")) # covariate

# If you wish to make it easier, you can use this data set as included
# in the package - but the above is included to show you
# how to use data from files.
# To use the internal data set:

        friend.data.w1 <- s501
        friend.data.w2 <- s502
        friend.data.w3 <- s503
        drink <- s50a
        smoke <- s50s
        
        par(mfrow=c(1,3))
        coord <- plot(as.network(friend.data.w1))
        plot(as.network(friend.data.w2), coord=coord)
        plot(as.network(friend.data.w3), coord=coord)
        
# At this point it is a good idea to use the sna package to plot the networks
# and the behavioural variable. Descriptive measures of the similarity of
# "friends" with respect to behaviour (like Moran's I) are given by the function
# nacf() in the sna package.

# Tell RSiena that the adjacency matrices are network data and in what order
# they should be treated

    friendship <- sienaDependent( array( c( friend.data.w1, friend.data.w2,
                            friend.data.w3 ),
                            dim = c( 50, 50, 3 ) ) )# 50x50 matricies for 3 waves

# Tell RSiena that the variable "drink" should be treated
# as a dependent variable

        drinkingbeh <- sienaDependent( drink, type = "behavior" )
        smoke1 <- coCovar( smoke[ , 1 ] ) # Why?

# Define the data set and obtain the basic effects object
        myCoEvolutionData <- sienaDataCreate( friendship, smoke1, drinkingbeh )
        myCoEvolutionEff <- getEffects( myCoEvolutionData )

# Run reports to check that data is properly formated and
# to get some basic descriptives

        print01Report( myCoEvolutionData, modelname = 's50_3_CoEvinit' )

# Define the effects to include in the coevolution model
# Start with some structural effects (use the shortnames that you find in
# effectsDocumentation(myCoEvolutionEff) )

        myCoEvolutionEff <- includeEffects( myCoEvolutionEff, transTrip, cycle3)

# Include a homophily effect for the constant covariate smoking

        myCoEvolutionEff <- includeEffects( myCoEvolutionEff, simX,
                                            interaction1 = "smoke1" )

# If we want to parse out whether there is a selection or influence (or both)
# effect for drinking behaviour,
# we need to also include sender, receiver and homophily effects
# of drinking for friendship formation:

        myCoEvolutionEff <- includeEffects(myCoEvolutionEff, egoX, altX, simX,
                                           interaction1 = "drinkingbeh" )

# For the influence part, i.e. the effect of the network on behaviour,
# we specify the following effects:
# indegree, outdegree and assimilation effects for drinking

        myCoEvolutionEff <- includeEffects( myCoEvolutionEff,
                                name = "drinkingbeh",
                                avAlt,indeg, outdeg,
                                interaction1 = "friendship" )

# Check what effects you have decided to include:

        myCoEvolutionEff

#
# Now we have to define the algorithm settings.
# Siena is in the course of transitioning to a better convergence criterion
# and better settings of the algorithm.
# Therefore we do not use the default settings, but settings
# that are hopefully better and may become the new default.

        myCoEvAlgorithm <- sienaAlgorithmCreate( projname = 's50CoEv_3' )
        betterCoEvAlgorithm <- sienaAlgorithmCreate( projname = 's50CoEv_3',
                                diagonalize = 0.2, doubleAveraging = 0)

# Finally, estimate the model; the whole command is put in parentheses
# to have the results printed directly to the screen.

        (ans <- siena07( betterCoEvAlgorithm, data = myCoEvolutionData,
                        effects = myCoEvolutionEff ))

# THE RESULTS

# Note that the "convergence t-ratio" is the t-ratio for convergence checking,
# not the t statistic for testing the significance of this effect.
# (See Section 6.1.2 of the manual.)
# For good convergence, the t-ratios for convergence
# all should be less than .1 in absolute value,
# and the overall maximum convergence ratio should be less than 0.25.
# If this is not yet the case, you should try again:

        (ans1 <- siena07( betterCoEvAlgorithm, data = myCoEvolutionData,
                        effects = myCoEvolutionEff, prevAns = ans ))

# Estimate   Standard   Convergence 
# Error      t-ratio   
# Network Dynamics 
# 1. rate constant friendship rate (period 1)  6.5633  (  1.1021  )   -0.0245   
# 2. rate constant friendship rate (period 2)  5.2264  (  0.9208  )    0.0665   
# 3. eval outdegree (density)                 -2.7715  (  0.1433  )   -0.0230   
# 4. eval reciprocity                          2.3827  (  0.2122  )   -0.0039   
# 5. eval transitive triplets                  0.6363  (  0.1356  )    0.0105   
# 6. eval 3-cycles                            -0.0517  (  0.2976  )    0.0145   
# 7. eval smoke1 similarity                    0.1777  (  0.2835  )   -0.0225   
# 8. eval drinkingbeh alter                   -0.0418  (  0.1206  )    0.0211   
# 9. eval drinkingbeh ego                      0.0796  (  0.1219  )    0.0406   
# 10. eval drinkingbeh similarity               1.3297  (  0.6946  )    0.0076   
# 
# Behavior Dynamics
# 11. rate rate drinkingbeh (period 1)          1.2397  (  0.5006  )   -0.0083   
# 12. rate rate drinkingbeh (period 2)          1.7186  (  0.5574  )    0.0583   
# 13. eval drinkingbeh linear shape            -0.9662  (  3.2321  )    0.0248   
# 14. eval drinkingbeh quadratic shape         -1.9451  (  5.1761  )   -0.0237   
# 15. eval drinkingbeh indegree                -1.1185  (  4.2518  )    0.0345   
# 16. eval drinkingbeh outdegree                1.9271  (  6.4425  )    0.0173   
# 17. eval drinkingbeh average alter            4.2445  ( 11.2827  )    0.0172   
# 
# Overall maximum convergence ratio:    0.1551 
        
# which can be repeated if necessary.

# For this small data set, the model for behavior dynamics is over-specified,
# leading to some very large standard errors.
# For this data set it is better to drop the degree effects on behaviour,
# because the data does not contain enough information to estimate them.

        myCoEvolutionEff2 <- includeEffects( myCoEvolutionEff,
                                name = "drinkingbeh", indeg, outdeg,
                                interaction1 = "friendship", include = FALSE)

       (ans2 <- siena07( betterCoEvAlgorithm, data = myCoEvolutionData,
                       effects = myCoEvolutionEff2 ))

###############################################################################
##                              Some other effects                           ##
###############################################################################
#
# The set of available effects for this data set can be obtained by requesting
        effectsDocumentation(  myCoEvolutionEff )
# See the manual, Chapter 12, for the meaning of these effects.
# To study the direct effect of the actor covariate smoking on the dependent
# variable drinking, use the effFrom effect:

        myCoEvolutionEff3 <- includeEffects( myCoEvolutionEff2,
                                name = "drinkingbeh", effFrom,
                                interaction1 = "smoke1")

# Since we already have a good result for a simpler model,
# it is helpful to start estimating from these estimates
# as starting values:

       (ans3 <- siena07( betterCoEvAlgorithm, data = myCoEvolutionData,
                       effects = myCoEvolutionEff3, prevAns = ans2 ))
# In my case, convergence was not good enough:
       (ans3 <- siena07( betterCoEvAlgorithm, data = myCoEvolutionData,
                       effects = myCoEvolutionEff3, prevAns = ans3 ))
# You can get a nicer presentation of the results in a file
# in your working directory in LaTeX by
        siena.table(ans3)
# and in html (can be imported into MS-Word) by
        siena.table(ans3, type="html")


