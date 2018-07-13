2+2
x <- 2
y <- 3
z <- y+x-2*x
# pc: ctrl+r
# mac: cmd+enter
# dir()
# set working directory:
setwd("/Users/johankoskinen/Documents/manchester admin/manchester summer school/2018/LABWORKS/1_2_networks R/")
HighTribe <- read.table('RedHighlandTribes.txt')HighTribePos <- HighTribe[1:16,]# take the first 16 rowsrowSums(HighTribePos)
table( rowSums(HighTribePos) )
plot( table( rowSums(HighTribePos) ) )
sum( rowSums(HighTribePos) )
sum( HighTribePos)

plot( HighTribePos)

HighNet <- as.network(HighTribePos, directed= FALSE)
# haven't installed 'network':
install.packages('network')
library('network')
library('sna')
triad.census(HighNet, mode='graph')
# directed network:
friend.data.w1 <- read.table("s50-network1.dat")friendMat <- as.matrix(friend.data.w1)plot(as.network(friendMat))
dyad.census( friendMat)
g <- rgraph(50)
plot(as.network(g))# too dense to be of any use
g <- rgraph(50, tprob = gden(friendMat))# set density to observed density
plot(as.network(g))# looks much more realistic
dyad.census(g)# with more realistic limit on degree hardly any mutual dyads
par(mfrow=c(1,2))# set up two side-by-side panels
plot(table(colSums(g )),xlim=range(colSums( friendMat )))# random degree distribution (same x-range as observed)
plot(table(colSums( friendMat )))

g <- rgraph(50,m=1, tprob = gden(friendMat))#
par( mfrow=c(1,2) )# set up two side-by-side panels
plot(as.network(g))#
plot( as.network( friendMat) )
par(mfrow=c(1,2))
plot( table( colSums(friendMat)))
plot( table( colSums(g[100,,])),xlim=range(colSums(friendMat)))

## Uniform MAN: rguman
?rguman
g <- rguman(1,50,mut=39,asym = 35, null=1151, method = 'exact')
par( mfrow=c(1,2) )# set up two side-by-side panels
plot(as.network(g))#
plot( as.network( friendMat) )

triad.census(g)
triad.census(friendMat)
# simulating ERGM in R
# load Padget dataset:
BusyNet <- as.matrix(read.table('PADGB.txt', header=FALSE) )
BusyNetNet <- network(BusyNet, directed=FALSE)
library('ergm')# load package 'ergm'
# if it doesnt load: install.packages('ergm')
g <- simulate(network(16, directed=FALSE) ~ edges, coef=c(-1.946), nsim=9) 

par(mfrow=c(2,5))plot(BusyNetNet)lapply(g,function(x) plot(x,main=dyad.census(x) ) )
### set weight for star 1,2, 3, and triangles to non-zero:
g.sim <- simulate(network(16, directed=FALSE)		 ~ kstar(1:3) + triangles, 				coef=c(-2.1113, 1.046,-0.6318 ,1.3064), nsim=9)
# plot the networks
par(mfrow=c(2,5))plot(BusyNetNet, main = triad.census(BusyNetNet,mode='graph') )lapply(g.sim,function(x) plot(x,main=triad.census(x,mode='graph') ) ) 
# how to get these coefficients:
estim.obj <- ergm( BusyNetNet
		 ~ kstar(1:3) + triangles )
summary( estim.obj )mygof <- gof( estim.obj )
plot(mygof)

##### check in and outdegree distribution:
setwd('/Users/johankoskinen/Documents/manchester admin/manchester summer school/2018/7_8_9_Directed and selection/')
ADJ <- read.table('communication.txt')
plot(jitter(rowSums(ADJ)),jitter(colSums(ADJ)) )
plot(as.network(ADJ))

#### SAOM
library('RSiena')library('network')library('sna')tmp4[is.na(tmp4)] <- 0 # remove missingpar(mfrow = c(1,2))coordin <- plot(as.network(tmp3))plot(as.network(tmp4),coord=coordin)

mynet1 <- sienaDependent(array(c(tmp3, tmp4), dim=c(32, 32,2)))mydata <- sienaDataCreate(mynet1)myeff <- getEffects(mydata)myeff <- includeEffects(myeff, recip,include=FALSE)myeff$initialValue[myeff$shortName=='Rate'] <- 3.8311myeff$initialValue[myeff$shortName=='density'][1] <- -1.1059sim_model  <-  sienaAlgorithmCreate( projname = 'sim_model', cond = FALSE, useStdInits = FALSE, nsub = 0 , n3 = 9 , simOnly = TRUE)sim_ans <- siena07( sim_model, data = mydata, effects = myeff,                    returnDeps = TRUE,  batch=TRUE )
                    
n <- dim(tmp4)[1]mySimNets <- reshapeRSienaDeps(sim_ans,n)     par(mfrow=c(2,5))plot(as.network(tmp4),coord=coordin,main=paste('ties: ',sum(tmp4) ) ) apply(mySimNets,1,function(x) plot(as.network(x),coord=coordin,main=paste('ties: ',sum(x))) )
dyad.census(mySimNets)

# add preference for reciprochation
myeff <- includeEffects(myeff, recip,include=TRUE)myeff$initialValue[myeff$shortName=='Rate'] <- 4.2525myeff$initialValue[myeff$shortName=='density'][1] <- -1.4163myeff$initialValue[myeff$shortName=='recip'][1] <- 1.1383sim_model  <-  sienaAlgorithmCreate( projname = 'sim_model', cond = FALSE, useStdInits = FALSE, nsub = 0, n3 = 9 , simOnly = TRUE)sim_ans <- siena07( sim_model, data = mydata, effects = myeff,                    returnDeps = TRUE,  batch=TRUE )mySimNets <- reshapeRSienaDeps(sim_ans,n)
dyad.census(mySimNets)
## add preference for transitivity:
myeff <- includeEffects(myeff, transTrip)myeff$initialValue[myeff$shortName=='Rate'] <- 4.5017myeff$initialValue[myeff$shortName=='density'][1] <- -1.9024myeff$initialValue[myeff$shortName=='recip'][1] <- 0.6794myeff$initialValue[myeff$shortName=='transTrip'][1] <- 0.3183sim_model  <-  sienaAlgorithmCreate( projname = 'sim_model', cond = FALSE, useStdInits = FALSE, nsub = 0, n3 = 9 , simOnly = TRUE)
sim_ans <- siena07( sim_model, data = mydata, effects = myeff,
                    returnDeps = TRUE,  batch=TRUE )
mySimNets <- reshapeRSienaDeps(sim_ans,n)
triad.census(mySimNets)#### ESTIMATION
est_model  <-  sienaAlgorithmCreate( projname = 'est_model')
est_ans <- siena07( est_model, data = mydata, effects = myeff)summary(est_ans)

### TURNING EDGELIST into MATRIX
setwd("/Users/johankoskinen/Documents/manchester admin/manchester summer school/2018/LABWORKS/")
myedge <- read.csv('edgelist.csv',sep=',',header=TRUE, row.names=1)

sender <- c('dor','dor','sco','cha')
rec <- c('joh','sco','dor','dor')

muppet <- data.frame(sender=sender,receiver=rec)
?network
mymuppet <- as.network(muppet ,matrix.type = 'edgelist')
plot(mymuppet)
ADJ <- as.matrix.network(mymuppet)


# set working directory:
setwd("/Users/johankoskinen/Documents/manchester admin/manchester summer school/2018/LABWORKS/")
# check what is in there:
dir()
###### Reading in affiliation: 
Aff <- read.csv("affiliations.csv",stringsAsFactors=FALSE)# this require that yo usaved a wrokbook as csv
# I don't like factors and I much prefer strings
# check the first 6 rows:
head(Aff)# obviously the first 2 rows are variable names:
Aff[2,]
# but 3 is data:
Aff[3,]
# first column
Aff[,1]# contains people names
# find out how many rows and columns we should take:
dim(Aff)
AffMat <- Aff[3:81,2:51]
class(AffMat)
head(AffMat)
# for reasons of plotting, create a people by people by affiliation matrix
AffMat <- as.matrix(AffMat)
nPeople <- nrow(AffMat)
nOps <- ncol(AffMat)
class(AffMat[1,2])# now the numbers are interpreted as strings
as.numeric(AffMat[1,2])
AffMat<- mapply(AffMat, FUN=as.numeric)
AffMat <- matrix(data=AffMat, nrow=nPeople, ncol=nOps)
### check the degree distribution of people
par( mfrow=c(1,2))
plot(table( rowSums(AffMat)) ,main= 'people degree')
### check the degree distribution of operations
plot(table( colSums(AffMat)) ,main='operation degree')
BigAdj <- cbind(matrix(0,nPeople,nPeople),AffMat)
BigAdjBottom <- cbind(t(AffMat),matrix(0,nOps,nOps))
BigAdj <- rbind(BigAdj,BigAdjBottom)
colours <- matrix('red',nPeople+nOps,1)
colours[1:nPeople,1] <- 'blue'
plot(as.network(BigAdj,directed=FALSE),vertex.col=colours)
### save to PNet:
write.table(AffMat,file='/Users/johankoskinen/Documents/manchester admin/manchester summer school/2018/LABWORKS/affiliation.txt',sep=' ',row.names=FALSE,col.names=FALSE)

#### READING IN friendship network
Friend <- read.csv("friendship.csv",stringsAsFactors=FALSE)# a dataframe
class(Friend)
head(Friend)
dim(Friend)# it should have nPeople rows
# firs column has names
Friend[,1]# are they in the same order as for the affiliations?
cbind(Friend[,1],Aff[3:81,1])
# seems like it
# extract adjacency matrix:
ADJ <- as.matrix(Friend[1:nPeople,2:(nPeople+1)])
plot(as.network(ADJ,directed=FALSE))
### save to PNet:
write.table(ADJ,file='/Users/johankoskinen/Documents/manchester admin/manchester summer school/2018/LABWORKS/friendship.txt',sep=' ',row.names=FALSE,col.names=FALSE)
# ADD friendship matrix to the BIG MATRIX:
BigAdj[1:nPeople,1:nPeople] <- ADJ
# plot the two types of ties:
plot(as.network(BigAdj,directed=FALSE),vertex.col=colours)

