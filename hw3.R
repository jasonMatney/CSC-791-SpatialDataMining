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
# (b)	Do one spatial classification - markov random fields. 
#     (any method and freely available s/w is fine). 
#     Compare the output with non-spatial
#     (any classifier used to answer first part of the question). 
#     Submit classified images (tiff format) as separate zip file.
#
# =====================================
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
# change extent 
# extent(r) <- c(0,1023,0,1023)

##rf <- writeRaster(r, filename="ilk.tif", format="GTiff", overwrite=TRUE) 

# Load ground truth (training / test data)
train <- read.csv("data/ilk-tr-xy.txt", header=FALSE, sep=",")
test <- read.csv("data/ilk-te-xy.txt", header=FALSE, sep=",")

# Label ground truth data
colnames(test) <- c("id","x","y","class")
colnames(train) <- c("id","x","y","class")


########################
# make spatial objects #
########################
#dsn=getwd()
#coordinates(test)  <- ~x+y
#coordinates(train)  <- ~x+y
#writeOGR(test, dsn=dsn, driver="ESRI Shapefile")

# ==============================
# spatial plot
spplot(r, col.regions = colorRampPalette(brewer.pal(9, "YlGnBu"))(18), 
     col = "#081D58", main = "ILK Raster File")

extent(r) <- c(0,1024,0,1024)
plotRGB(r)
points(train$x, train$y, col='green', pch=20)
points(test$x, test$y, col='purple', pch=20)
points(c(0,0),col='red', pch=20, cex=10)

# isolate coords
trcoords <- cbind(train[["x"]],train[["y"]])
tecoords <- cbind(test[["x"]],test[["y"]])

# extract raster values
trex <- extract(r, trcoords)
teex <- extract(r, tecoords)

# factor 
train$class <- factor(train$class)
test$class <- factor(test$class)

# bind data to data frames
training <- cbind(train,trex)
testing <- cbind(test,teex)

# relabel
colnames(testing) <- c("id","y","x","class","r","g","b")
colnames(training) <- c("id","y","x","class","r","g","b")

# remove first column
testing  <- testing[,-1]
training <- training[,-1]

head(training)
head(testing)
###############
# Naive Bayes #
###############

## Example of using a contingency table:
m <- naiveBayes(training[,4], training[,5])
m
table(predict(m, testing), testing[,5])

#####################################
# Classificaition with randomForest #
#####################################

#fit the randomforest model
model <- randomForest(class ~ ilk.3b.1024, 
                      data = training, 
                      importance=TRUE,
                      keep.forest=TRUE )
print(model)

##################
# Neural Network #
##################

idC <-class.ind(training$class)
NN1=nnet(training, idC[,-5], size=15, maxit = 200, softmax=TRUE)
table(predict(NN1, data=testing,type = "class"))

###########################
# Support Vector Machines #
###########################

svm.model <- svm(class ~ ., data = training, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testing[,-5])

## compute svm confusion matrix
table(pred = svm.pred, true = test[,4])


###########
## 1 (b) ##
###########
###########
##########################
# Spatial Classification # 
##########################
## =============
# RUN RF MODEL
rf.mdl <- randomForest(x=training[,-5], y=training[,5], ntree=501, importance=TRUE)

# CHECK ERROR CONVERGENCE
plot(rf.mdl)

# PLOT mean decrease in accuracy VARIABLE IMPORTANCE
varImpPlot(rf.mdl, type=1)

# PREDICT MODEL
predict(test, rf.mdl, type="response", index=1, progress="window")

# ==============================================
# CREATE RF MODEL
( rf.mdl <- randomForest(x=training[,-5], y=training[,5],
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

