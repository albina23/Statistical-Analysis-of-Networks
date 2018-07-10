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
  "sna"
))

# Comparing the in-degrees of random vs observed
par(mfrow=c(1,2))
plot(table(colSums(friendMat)))
plot(table(colSums(g[1,,])), xlim=range(colSums(friendMat))) #(g[1,,]) (First slice, all rows, all columns)


