#   Do classification with at least one classifier from Naive Bayes and SVM ##

rm(list=ls())
set.seed(0)
library(caret)
library(klaR)
library(rgdal)
library(raster)
library(rasclass)
library(rpart)

# Satellite image is shown below with ground-truth data. 
# There are 5 classes (buildings, roads, grass, trees, and water). 
# Training file contains image coordinates for each ground-truth location on this map. 
# You can extract data (3x3 or 5x5 window) centered on each point.
#
# Answer to your question lies in the image shown in the Appendix. 
# Image classification consists of the following steps:
#   
# 1) select few sample locations (only x,y coordinate) 
#    (highlighted by red color points on the image)
# 
# 2) assign labels to these points 
#   (different symbols represents different classes -- see the legend)
# 
# 3) divide this data (sample locations) into training and test data.
# 
# 4) use training data 
#    (that is, extract RGB values from the 
#     image for each location; this could be 
#     single value or 3x3 window -- to increase sample size) to build the "model"
# 
# 5) use test data to evaluate the "model"
# 
# 6) Finally, use the "model" to predict labels for every pixel in the image.
# 
# Also, note these are image coordinates 
# (not spatial or projection coordinates), 
# so it would be easy for every one, 
# otherwise you have to deal with projections.
# 
# set working directory
setwd("C:/Users/jamatney/Documents/GitHub/CSC-791-SpatialDataMining")

# Load satellite image
ilk <- raster("hw3-gt-data\\ilk-3b-1024.tif")
plot(ilk)

# Load ground truth (training / test data)
train <- read.csv("hw3-gt-data\\ilk-tr-xy.txt", header=FALSE, sep=",")
test <- read.csv("hw3-gt-data\\ilk-te-xy.txt", header=FALSE, sep=",")

# Label ground truth data
colnames(train) <- c("id","x","y","label")
colnames(test) <- c("id","x","y","label")

# set new extent
r <- raster()
bb <- extent(0, 1024, 0, 1024)
extent(r) <- bb
r <- setExtent(ilk, bb, keepres=TRUE)

# stacks
r <- stack(r)

# plot a rgb
plotRGB(r ,3, 2, 1)

# add some points
points(train[["x"]], train[["y"]], col='red', pch=20)
points(test[["x"]], test[["y"]], col='blue', pch=12)

# isolate coords
trcoords <- cbind(train[["x"]],train[["y"]])
tecoords <- cbind(test[["x"]],test[["y"]])

# extract raster values
trex <- extract(r, trcoords)
teex <- extract(r, tecoords)

# bind data to data frames
train <- cbind(train,trex)
test <- cbind(test,teex)

# reset colanmes
colnames(train) <- c("id","x","y","label","extraction") 
colnames(test) <- c("id","x","y","label","extraction")

# check if points match by adding triangles
points(978, 384, col="green")
points(851, 926, col="green")
points(978, 384, col="green", pch=2, cex=4)
points(851, 926, col="green", pch=2, cex=4)

###############
# Naive Bayes #
###############

require(e1071)

## Example of using a contingency table:
label <- as.data.frame(train[,4])
extraction <- as.data.frame(train[,5])
m <- naiveBayes(label ~ ., data=train)
m

###########################
# Support Vector Machines #
###########################

svm.model <- svm(label ~ ., data = train, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, test[,-4])

## compute svm confusion matrix
table(pred = svm.pred, true = test[,4])

##################
# Neural Network #
##################

require(NeuralNetTools)
require(nnet)
data(neuraldat)
set.seed(123)

mod <- nnet(label ~ extraction, data = train, size = 5)

#########################
# Spaial Classification #
#########################

logo <- brick(system.file("external/rlogo.grd", package="raster"))
names(logo)

## Not run: 
# the predictor variables
par(mfrow=c(2,2))
plotRGB(logo, main='logo')
plot(logo, 1, col=rgb(cbind(0:255,0,0), maxColorValue=255))
plot(logo, 2, col=rgb(cbind(0,0:255,0), maxColorValue=255))
plot(logo, 3, col=rgb(cbind(0,0,0:255), maxColorValue=255))
par(mfrow=c(1,1))

## End(Not run)

# known presence and absence points
p <- matrix(c(48, 48, 48, 53, 50, 46, 54, 70, 84, 85, 74, 84, 95, 85, 
              66, 42, 26, 4, 19, 17, 7, 14, 26, 29, 39, 45, 51, 56, 46, 38, 31, 
              22, 34, 60, 70, 73, 63, 46, 43, 28), ncol=2)

a <- matrix(c(22, 33, 64, 85, 92, 94, 59, 27, 30, 64, 60, 33, 31, 9,
              99, 67, 15, 5, 4, 30, 8, 37, 42, 27, 19, 69, 60, 73, 3, 5, 21,
              37, 52, 70, 74, 9, 13, 4, 17, 47), ncol=2)

# extract values for points
xy <- rbind(cbind(1, p), cbind(0, a))
v <- data.frame(cbind(xy[,1], extract(logo, xy[,2:3])))
colnames(v)[1] <- 'pa'

#build a model, here an example with glm 
model <- glm(formula=label~., data=train)

#predict to a raster
r1 <- predict(ilk, model)

##########################
# Spatial Classification # 
##########################
# Add required libraries
require(randomForest)

# CREATE RF MODEL
( rf.mdl <- randomForest(x=train[,5], y=train[,4],
                         ntree=501, proximity=TRUE, importance=TRUE) )

# PREDICT SINGLE CLASSIFIED RASTER                     
predict(r, rf.mdl, filename="ClassPred.img", type="response", na.rm=TRUE, 
        overwrite=FALSE, progress="window")

################
## Question 2 ##
################
require(rpart)
play_base <- read.csv("Book1.csv")
colnames(play_base)[5] <- "Play"
print(play_base)

# build the decision tree
fit <- rpart(Play ~ Outlook + Temperature + Humidity , 
             method="class", data=play_base,
             control=rpart.control(minsplit=1))

# see listing of loaded decision tree
summary(fit)

# View the decision tree
print(fit)

# plot an rpart object as a decision tree
plot(fit, uniform=TRUE, main="Decision Tree - Play?")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

