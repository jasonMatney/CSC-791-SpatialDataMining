rm(list=ls())
require(sp)
require(rgdal)
require(raster)
require(randomForest)
library(tictoc)
library(devtools)
library(teamlucc)
setwd("C:/Users/jamatney/Documents/CSC791")
if (!require(devtools)) {
  install.packages('devtools')
  library(devtools)
}
if (!require(doParallel)) {
  install.packages('doParallel')
  library(doParallel)
}
if (!require(teamlucc)) {
  install_github('azvoleff/teamlucc')
  library(teamlucc)
}
if (!require(doParallel)) {
  install.packages('doParallel')
  library(doParallel)
}

# ################################################################
# # CREATE LIST OF RASTERS
# rlist=list.files(getwd(), pattern="img$", full.names=TRUE) 
# 
# # CREATE RASTER STACK
# xvars <- stack(rlist[1])     
# dsn=getwd()
# xvars_shape <- readOGR(dsn=dsn, layer="xvars_clip")
# C <- crop(xvars, xvars_shape)

library(doParallel)
registerDoParallel(2)
set.seed(0) # Set a random seed so results match what we got earlier
train_polys <- L5TSR_1986_2001_training
train_data_par <- get_pixels(L5TSR_1986, train_polys, class_col="class_1986", 
                             training=.6)
clfr_par <- train_classifier(train_data)
cls_par <- classify(L5TSR_1986, clfr)
spplot(cls_par$classes)
spplot(cls_par$probs)

###