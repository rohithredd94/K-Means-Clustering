#!/usr/bin/env Rscript
setwd("~/MLAssign5/part1")
args = commandArgs(trailingOnly=TRUE)

centroidDists <- function(X, c) {
  dists = apply(X, 1, function(point)
    sapply(1:nrow(c), function(dim)
      dist(rbind(point, c[dim, ]))))
  return(t(dists))
}

assignClusters <- function(X, c) {
  cDists = centroidDists(X, c)
  clusterAndDist = sapply(1:n, function(x) which.min(cDists[x, ]))
  return(clusterAndDist)
}

printclusters <- function(clusters,X,k){
  cat("Final Iteration Count:", iter, "\n")
  cat("Cluster No.\t Cluster Contents\n")
  
  clusarr <- NULL
  clusarr <- as.data.frame(clusarr)
  track <- NULL
  for(i in 1:k)
  {
    track[i] <- 1
  }
  
  for(i in 1:nrow(X)){
    for(j in 1:k){
      if(clusters[i] == j){
        clusarr[j,track[j]] <- i
        track[j] = track[j] + 1
      }
      
    }
  }
  
  for (i in 1:k) {
    cat(i,"\t \t")
    temp <- clusarr[i,]
    temp <- temp[!is.na(temp)]
    cat(temp, sep = ",")
    cat("\n")
    write(c(i,'\t',temp), file = args[3], ncolumns = length(temp)+2,append = TRUE,sep = ",")
  }
}

calculatesse <- function(X,centroids,clusters){
  SSE <- 0
  X1 <- 0
  Y1 <- 0
  
  for(i in 1:length(clusters)){
    X1 = sum((X[i,1] - centroids[clusters[i],1])^2)
    Y1 = sum((X[i,2] - centroids[clusters[i],2])^2)
    SSE = SSE + (X1+Y1)
  }
  cat("SSE:",SSE)
  write(c("SSE:",SSE),file = args[3],ncolumns = 2,append = TRUE)
}
#Main Sub routine
## read data
#X = read.table('http://www.utdallas.edu/~axn112530/cs6375/unsupervised/test_data.txt', stringsAsFactors = FALSE)
X = read.table(args[2], stringsAsFactors = FALSE)
X$V1 <- NULL
X <- X[-c(1),]
X1 <- X
#X$V2 <- factor(X$V2)
#X$V3 <- factor(X$V3)
X$V2 <- as.numeric(X$V2)
X$V3 <- as.numeric(X$V3)
X <- as.matrix(X)

k = args[1]
maxIter = 25
cutoff = 0.001

## determine number of points and dimensions in data
n = dim(X)[1]
d = dim(X)[2]

## select k random points for initial means
ids = sample(1:n, k)
centroids = X[ids, ]

## initialize cluster assignments for points
clusters = matrix(0, 1, n)

for(iter in 1:maxIter) {
  
  clusters = assignClusters(X, centroids)
  
  newCentroids = t(sapply(1:k, function(c) colMeans(X[which(clusters == c), ])))
  
  ## check if change in centroids is significant
  delta = sum((newCentroids - centroids) ^ 2)

  if (delta > cutoff) {
    ## use new centroids for next iteration
    centroids = newCentroids
  
  } else {
    break
  }
}

#Print Clusters
printclusters(clusters,X,k)

#Calculating Sum of squared errors
calculatesse(X,centroids,clusters)


