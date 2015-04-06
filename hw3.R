<<<<<<< HEAD
rm(list=ls())
dir = getwd()
library(sp)
library(rgeos)
library(rgdal)
library(fields)
library(spBayes)

################
## Question 1 ##
################
# 1. For given satellite image 
# (ilk-3b-1024.tif; 3-dimensional, 1024x1024), 
# and ground-truth (training/test) data. 
# Ground-truth format is: <id, x, y, label>
# (a) Do classification with at least one classifier from each group: 
#     Bayesian, 
#     Trees, 
#     Neural Networks,
#     SVMs; 
#     and compare (accuracy) and contrast (describe major differences, advantages/disadvantages). 
#     Be concise (use tabular format). 
#
# (b)	Do one spatial classification
#     (any method and freely available s/w is fine). 
#     Compare the output with non-spatial
#     (any classifier used to answer first part of the question). 
#     Submit classified images (tiff format) as separate zip file.
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
