rm(list=ls())
dir = getwd()
set.seed(0)
library(raster)
library(sp)
library(rgeos)
library(rgdal)
library(fields)
library(spBayes)
library(e1071)
library(RColorBrewer)
library(nnet)
library(randomForest)
library(tcltk)

# ======== Load and Label ==============

# Load satellite image
r <- stack("data/ilk-3b-1024.tif")

# Load ground truth (training / test data)
train <- read.csv("data/ilk-tr-xy.txt", header=FALSE, sep=",")
test <- read.csv("data/ilk-te-xy.txt", header=FALSE, sep=",")

# Label ground truth data
colnames(test) <- c("id","x","y","class")
colnames(train) <- c("id","x","y","class")

# train reverse
y_reversed <- NULL
for(i in 1:nrow(train))
{
  y_reversed_temp = 1024- train[,3][i]
  y_reversed<-c(y_reversed,y_reversed_temp)
}
train_reversed<-cbind(train[,-c(3)],y_reversed)

# test reverse
y_reversed <- NULL
for(i in 1:nrow(test))
{
  y_reversed_temp = 1024- test[,3][i]
  y_reversed<-c(y_reversed,y_reversed_temp)
}
test_reversed<-cbind(test[,-c(3)],y_reversed)

# plot

extent(r) <- c(0,1024,0,1024)
plotRGB(r)
points(train_reversed$x, train_reversed$y, col='green', pch=20)
points(test_reversed$x, test_reversed$y, col='red', pch=20)

# extract train data
sp_train <- as.data.frame(cbind(train_reversed$x, train_reversed$y))
colnames(sp_train) <- c("x","y")
coordinates(sp_train)  <- ~x+y

train_raster_data <- extract(r, sp_train)
colnames(train_raster_data) <- c("r","g","b")

training <- as.data.frame(cbind(train_reversed$x, train_reversed$y_reversed, train_reversed$class, train_raster_data))
colnames(training) <- c("x","y","class","r","g","b")

# subset train by class
train5 <- subset(training, class>4)
train4 <- subset(training, class>3 & class<5)
train3 <- subset(training, class>2 & class<4)
train2 <- subset(training, class>1 & class<3)
train1 <- subset(training, class>0 & class<2)

points(train5$x, train5$y, col='purple', pch=20)
points(train4$x, train4$y, col='blue', pch=20)
points(train3$x, train3$y, col='red', pch=20)
points(train2$x, train2$y, col='yellow', pch=20)
points(train1$x, train1$y, col='green', pch=20)

########################
# DO the same for Test #
########################
# # extract test data
# sp_test <- as.data.frame(cbind(test_reversed$x, test_reversed$y))
# colnames(sp_test) <- c("x","y")
# coordinates(sp_test)  <- ~x+y
# 
# test_raster_data <- extract(r, sp_test)
# colnames(test_raster_data) <- c("r","g","b")
# 
# testing <- as.data.frame(cbind(test_reversed$x, test_reversed$y_reversed, test_reversed$class, test_raster_data))
# colnames(testing) <- c("x","y","class","r","g","b")
# 
# # subset test by class
# test5 <- subset(testing, class>4)
# test4 <- subset(testing, class>3 & class<5)
# test3 <- subset(testing, class>2 & class<4)
# test2 <- subset(testing, class>1 & class<3)
# test1 <- subset(testing, class>0 & class<2)
# 
# plotRGB(r)
# points(test5$x, test5$y, col='purple', pch=20)
# points(test4$x, test4$y, col='blue', pch=20)
# points(test3$x, test3$y, col='red', pch=20)
# points(test2$x, test2$y, col='yellow', pch=20)
# points(test1$x, test1$y, col='green', pch=20)

###############
# Naive Bayes #
###############

model <- naiveBayes(class ~ r + g + b, data = training)
model
table(predict(model, testing$class))
