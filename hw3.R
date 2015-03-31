#   Do classification with at least one classifier from each group: 
#   Bayesian, Trees, Neural Networks, and SVMs; and compare (accuracy) 
#   and contrast (describe major differences, advantages/disadvantages). 
#   Be concise (use tabular format). 

rm(list=ls())
set.seed(0)

library(rgdal)
library(raster)

# Load satellite image
ilk <- raster("data\\ilk-3b-1024.tif")

# Load ground truth (training / test data)
train <- read.csv("data\\ilk-tr-xy.txt", header=FALSE, sep=",")
test <- read.csv("data\\ilk-te-xy.txt", header=FALSE, sep=",")

# Label ground truth data
colnames(train) <- c("id","x","y","label")
colnames(test) <- c("id","x","y","label")

# ilk <- brick(ilk)
plot(ilk)

