##########kmeans clustering#################
###         Yangming  Li           #########
############################################
library(MASS)
library(cluster)
library(factoextra)
library(scatterplot3d)
library(Gmedian) 
library(SpatialNP)
library(OjaNP)
setwd("~/Desktop")
set.seed(1)


######################
#function name: euclid
#summary:produce the euclid distance matrix 
#cacluate the euclid distance for the data sets to every i center
# and store the i center to the i column.

#input:the data matrix and center vector

#output: the distance matrix of the data matrix

euclid <- function(points1, points2) {
  
  # build an empty matrix, the row is equal to the row of the data matrix (how many data)
  # and the column is equal to the  row of the centers 
  distanceMatrix=matrix(NA, nrow=dim(points1)[1], ncol=dim(points2)[1])
  for(i in 1:nrow(points2)) { # for loop goes on all of center points 
    distanceMatrix[,i]=sqrt(rowSums(t(t(points1)-points2[i,])^2))
    # store the euclid distance every points to each i center into the corrsponding i column
  }
  distanceMatrix
}




#############################
#function name: kmeans 
#summary:produce the corrspond centers and corresponding clusters for the data sets

#input: x is a pure data set, n is the number of cluster of the data set
# disFun is a distance function you defined, the distance calulation method is euclidean distance
# delta is the tolerance and stopping creiteia

# output: the # of groups the corsponding clusters the data points assigned
# #the center of each groups
K_means <- function(x,n, distFun, delta){
  t=x
  # treat the data as a matrix 
  t=as.matrix(t)
  spatial.signs(XX)
  # random sample n point as a intial n centers
  centers=t[sample(nrow(t), n),]
  # begin the loop
  repeat{
    # computing distance from each point to the center
    dist=distFun(x, centers)
    
    #assign the each points to the nearest center
    # for the every row of the distance matrix 
    # return to the index of the  column
    clusters=apply(dist, 1, which.min)
    
    
    # recompute the new centroids, step move each centroid to the center of the data points assigned
    #for every column of the assigned index matrix calculate the corrsponding mean of original mean
   # tapply (is grouped by the clusters and calculate the mean)
    ncenters=apply(x, 2, tapply, clusters,spatial.median)
    #group every x by the corssponding cluster and calcuate the mean by column
    #ncenters=apply(x, 2, tapply, clusters, median)
    
    #ncenters=apply(x, 2, tapply, clusters,spatial.median)
    
    #ncenters=apply(x, 2, tapply, clusters,spatial.median)
    
    #stopping step use the distance between the center
    # distance =0
    d=0
    for(i in 1:n)
      d=d+sqrt(sum((centers[i]-ncenters[i])^2))
    
    # if distance is bigger recompute the center and do the loop
    if (d>delta) 
      centers=ncenters
    
    if(d<delta)  break
    
  }
  # return the list of the assigned center
  list(clusters=clusters, centers=centers) 
}

# algorithm dataset use a simple data set
#t=matrix(c(1,1,4,4,1,2,4,5),4,2)
#> t
#       [,1] [,2]
#[1,]    1    1
#[2,]    1    2
#[3,]    4    4
#[4,]    4    5
#centers=t[sample(nrow(t), 2),]
#> centers
#[,1] [,2]
#[1,]    4    5
#[2,]    1    2
#distanceMatrix=matrix(NA, nrow=dim(t)[1], ncol=dim(centers)[1])
#distanceMatrix
#> distanceMatrix
#[,1] [,2]
#[1,]   NA   NA
#[2,]   NA   NA
#[3,]   NA   NA
#[4,]   NA   NA
#dist=euclid(t, centers)
#> dist
#[,1]     [,2]
#[1,] 5.000000 1.000000
#[2,] 4.242641 0.000000
#[3,] 1.000000 3.605551
#[4,] 0.000000 4.242641

#clusters=apply(dist, 1, which.min)
#> clusters
#[1] 2 2 1 1

#apply(t, 2, tapply, clusters, mean)
#   [,1] [,2]
#1    4  4.5
#2    1  1.5
#



###########################
#test data_set
# dataset1: two demnsion 2 cluster data set
data_1 = "kMeansDataset/twoCircles.txt"

# dataset2: two dimension 4 cluster data set
data_2 = "kMeansDataset/fourCircles.txt"

# dataset3: three dimensional data set
data_3= iris[, 1:3]

# dataset4: high dimensional data set
data_4 =iris[,1:4]

data("woodmod.dat")
outlier <- c(4, 6, 8, 19)
XX <- as.matrix(woodmod.dat)

# data set we generate from the distributions

data_1=as.matrix(read.table(data_1))
data_2=as.matrix(read.table(data_2))





##########################################
#determine k 
#Elbow Method

#fviz_nbclust(x, kmeans, method = "wss")

#Average Silhouette Method
#fviz_nbclust(x, kmeans, method = "silhouette")



############################################
#function name: plot_kmean
#summary: this function visualize the clustering results from 
#any high dimensional data sets

#input: any pure dimensional data sets (dimensional>2)

#output: 
# the plot for two dimension 
# the 3d scatterplot and the matrix of scatter plots
# the matrix of scatterplots for high dimensional data sets
plot_kmean<-function(m,k){
  r=K_means(m,k,euclid,0.001)
  r$centers
  
  if (ncol(m)==3)
  { 
    
    scatterplot3d(m[, 1], m[, 2],m[, 3], pch = 16, main = 'Original groups')
    scatterplot3d(m[, 1], m[, 2], m[, 3], color = as.numeric(r$clusters), pch = 1, main = 'Groups by clustering')
  }
  
  if (ncol(m)>3)
  { 
    
    pairs(m,main="original data",)
    pairs(m,main="k spatial median", col = r$clusters)}
  
  
  else
  {  
    plot(m,main ="before clustering")
    plot(m,col=r$cluster,main ="after clustering")
    points(r$centers, col=(k+1):(k+k), pch=8, cex=2)}}

#fviz_nbclust(data_1, kmeans, method = "wss")
#fviz_nbclust(data_1, kmeans, method = "silhouette")
plot_kmean(XX,2)
#plot_kmean(data_1,2)
#plot_kmean(data_2,4)
#plot_kmean(data_3,3)
#plot_kmean(data_4,4)
