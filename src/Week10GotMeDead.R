## Dong R Code for Week 10####
library(tidyverse)
library(caret)
library(caTools)
set.seed(1)
 data(iris)
iris
iris %>%
  ggplot(aes(x=Petal.Length, y=Petal.Width, color=Species)) +
  geom_point() + geom_smooth()
## using Kmeans with centroids (how many clusters) 
irisNoLabs <- iris %>%
  select(-Species)
# the centers thing tells you what your K value is 
clusters <- kmeans(irisNoLabs, centers = 3)
kmeans
?kmeans
#some notes on how it all works ##
## kmeans Arguments #####
#Arguments
# #
# x	
# # numeric matrix of data, or an object 
# that can be coerced to such a matrix (such as a numeric 
#       vector or a data frame with all numeric columns).
# # 
# # centers	
# # either the number of clusters, say k, or a 
# set of initial (distinct) cluster centres. If a number, 
# a random set of (distinct) rows 
# in x is chosen as the initial centres.
# # 
# 
# # iter.max	
# # the maximum number of iterations allowed.
# # 
# 
# # nstart	
# # if centers is a number, how many 
# random sets should be chosen?
# #   
#   
# #   algorithm	
# # character: may be abbreviated. 
#   Note that "Lloyd" and "Forgy" are alternative
# names for one algorithm.
# # 
# 
# # object	
# # an R object of class 
# "kmeans", typically the result ob of ob <- kmeans(..).
# # 
# 
# # method	
# # character: may be abbreviated. "centers" 
# causes fitted to return cluster centers (one for 
#   each input point) and "classes" causes fitted 
# to return a vector of class assignments.
# # 
# 
# # trace	
# # logical or integer number, currently only
# used in the default method ("Hartigan-Wong"): if positive (or true), 
# tracing information on the progress of the algorithm is 
# produced. Higher values may produce more tracing information.


clusters$cluster
table(clusters$cluster, iris$Species)

print("cluster	
A vector of integers (from 1:k) indicating 
the cluster to which each point is allocated.

centers	
A matrix of cluster centres.

totss	
The total sum of squares.

withinss	
Vector of within-cluster sum of squares, one component per cluster.

tot.withinss	
Total within-cluster sum of squares, i.e. sum(withinss).

betweenss	
The between-cluster sum of squares, i.e. totss-tot.withinss.

size	
The number of points in each cluster.

iter	
The number of (outer) iterations.

ifault	
integer: indicator of a possible algorithm problem - for experts.")

## making a for loop to find optimal amt of K####

clusters$centers
clusters$withinss
clusters$tot.withinss
tot_within_ss <- numeric(length = 20) #k val)
for (k in seq(1,20)) {
  clusters <- kmeans(irisNoLabs, centers=k)
  tot_within_ss[k] <- clusters$tot.withinss
  
  
  
  
}
tot_within_ss
## checking optimal amt of k w/ line plot ####
optimus <- data.frame(k=seq(1,20), tot_within_ss) 

## making the plot ####
optimus %>%
  ggplot(aes(x=k, y=tot_within_ss)) +
  geom_point() + geom_smooth()

## now let's see how different k clusters affect the sizes on the plot####
# k = 3 in this one
clusters <- kmeans(irisNoLabs, centers = 3)
iris$clusters3 <- clusters$cluster

#paritinoned 4 of dem
clusters <- kmeans(irisNoLabs, centers=4)
iris$clusters4 <- clusters$cluster

data(iris)
irisSmol <- iris 

iris%>%
  ggplot(aes(x=Petal.Length, y=Petal.Width, color=factor(clusters3))) +
  geom_point() + geom_smooth()

 ## getting rid of other attributes into Smol####
irisSmol <- iris%>%
  select(Petal.Length, Petal.Width)

# k = 3 in this one part 2 #####
clusters <- kmeans(irisSmol, centers = 3)
iris$clusters3 <- clusters$cluster

#paritinoned 4 of dem part 2 ####
clusters <- kmeans(irisSmol, centers=4)
iris$clusters4 <- clusters$cluster



## Hw 4, DOOD LAST ASSIGNMENT #####


centXYellow <- mean(c(4,5,5,5,5,6))
centXYellow
centYYellow <- mean(c(1,2,3,4,4,5))
centYYellow
## Blue cluster Iteration 2 ####
centXBlue <- mean(c(2,2,3,3,3,3,4,4))
centXBlue
centYBlue <- mean(c(1,1,2,2,2,3,3,5))
centYBlue
1/(sqrt(2))^2
1.414214^2
sqrt(3)^2
