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
  "sna"
))

#Read in data:
HighTribe <- read.table('RedHighlandTribes.txt')
HighTribePos <- HighTribe[1:16,]
rowSums(HighTribePos) #Nodal degrees
table(rowSums(HighTribePos)) #Degree distribution

#Plotting degree distribution:
plot( table(rowSums(HighTribePos)), ylab='number',xlab='degree')

#Plotting network:
HighNet <- as.network(HighTribePos, directed= FALSE)
plot(HighNet)



