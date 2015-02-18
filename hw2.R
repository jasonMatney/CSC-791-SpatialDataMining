####################################
## CSC 791          
## unityID: jamatney 
## ID: 200098849
####################################
set.seed(1)

rm(list=ls(all=T))

library(tiff)
library(mclust)
library(lattice)
library(MASS)
library(car)
library(utils)          # for read/write csv files
library(flexclust)      # for kcca
library(raster)
library(dismo)
library(fpc)

# Set the working directory path
setwd("C:\\Users\\jamatney\\Documents\\CSC791\\homework\\hw2\\hw2resources")

# read the data
files <- list.files(pattern=".tif")
ILK <- brick(files) 

# get field names 
names(ILK)

# plot the data 
plotRGB(ILK, 3,2,1)

# create data frame
ILK.df <- as.data.frame(ILK)

## perform kmeans with 4 clusters
nc = 4
ILK.kmeans <- kmeans(ILK[], nc) 

## summary
summary(ILK.kmeans)

## create an empty raster with same extent as ILK
kmeansraster <- raster(ILK) 

## set values to empty raster from cluster
kmeansraster[] <- ILK.kmeans$cluster

# plot the clusters
plot(kmeansraster)

# Do model based clustering
#
num <- length(ILK) * 0.1
ILK.samp = sampleRandom(ILK, num)

g = 1:4
ILK.mc <- Mclust(ILK.samp[], G=g)

# Load cancer data
cancer <- read.table("cancer-data.csv", sep=",", header=TRUE)
head(cancer)
plot(cancer, xlab='x', ylab='y', main="Cancer data")

# Run dbscan & plot results
cancer.ds <- dbscan(cancer, 1)
plot(cancer.ds, cancer)

