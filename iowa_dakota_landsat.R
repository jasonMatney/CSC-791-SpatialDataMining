rm(list=ls())
require(sp)
require(rgdal)
require(raster)
require(randomForest)
library(tictoc)
setwd("C:/Users/jamatney/Documents/CSC791")


################################################################
# CREATE LIST OF RASTERS
rlist=list.files(getwd(), pattern="img$", full.names=TRUE) 

# CREATE RASTER STACK
xvars <- stack(rlist[1])     
dsn=getwd()
xvars_shape <- readOGR(dsn=dsn, layer="xvars_clip")
C <- crop(xvars, xvars_shape)

# READ POINT SHAPEFILE TRAINING DATAchrome

ogrListLayers(dsn)
sdata <- readOGR(dsn=dsn, layer="Export_Output")

# ASSIGN RASTER VALUES TO TRAINING DATA
## This method fails 
## v <- as.data.frame(extract(xvars, sdata))
##

tmp <- extract(xvars, sdata)
mat = c()

for (i in 1:length(tmp)) {
  tmp_b <- cbind(sdata@data[i,], as.data.frame(tmp[i]))
  mat <- rbind(mat,tmp_b)
}

v <- mat
sdata@data = data.frame(sdata@data, v[match(rownames(sdata@data), rownames(v)),])

# RUN RF MODEL
rf.mdl <- randomForest(x=sdata@data[-1,3:ncol(sdata@data)], y=as.factor(sdata@data[-1,"X2014_30m_cdls"]),
                       ntree=501, importance=TRUE)

# CHECK ERROR CONVERGENCE
plot(rf.mdl)

# PLOT mean decrease in accuracy VARIABLE IMPORTANCE
varImpPlot(rf.mdl, type=1)

# PREDICT MODEL
predict(xvars, rf.mdl, filename="RfClassPred.img", type="response", 
        index=1, na.rm=TRUE, progress="window", overwrite=TRUE)


# examples

r <- raster(nrow=45, ncol=90)
r[] <- 1:ncell(r)
e <- extent(-160, 10, 30, 60)
rc <- crop(r, e)  

# use row and column numbers:
rc2 <- crop(r, extent(r, 5, 10, 7, 15))

# crop Raster* with Spatial* object
b <- as(extent(6, 6.4, 49.75, 50), 'SpatialPolygons')
crs(b) <- crs(r)
rb <- crop(r, b)

# crop a SpatialPolygon* object with another one
if (require(rgdal) & require(rgeos)) {
  p <- shapefile(system.file("external/lux.shp", package="raster"))
  pb <- crop(p, b)
}

#
#

# Create list of Rasters
rlist=list.files(getwd(), pattern="img$", full.names=TRUE) 

# Create raster stack
xvars <- raster(rlist[1])      
sdata <- raster("dakota_landcover.tif")


# PlotRGB
plot(xvars)
plot(sdata)

# draw extent 
ext <- drawExtent()
C <- crop(sdata, ext)

# plot the satellite image as RGB
plotRGB(C, 3,2,1)

# unsupervised randomForests
C.df <- as.data.frame(C)

# unsupervised random Forests
tic()
C.rf <- randomForest(C.df, importance=TRUE, proximity=FALSE, ntree=500, type=unsupervised, forest=NULL)
toc()

# kmeans
C.kmeans <- kmeans(C[], 5, iter.max = 100, nstart = 3)
kmeansraster<-raster(C)
kmeansraster[]<-C.kmeans$cluster
plot(kmeansraster)

# export the resulting unsupervised classification
# to your file system
writeRaster(kmeansraster,"landsat_unsup_5_classif.tif", overwrite=TRUE)

######################
# check files
dsn=getwd()
ogrListLayers(dsn)

# supervised randomForests
# Read raster of the "Greater Dakotas" 

# Assign Raster values to training data
v <- as.data.frame(extract(xvars, coordinates(C)))

sdata@data = data.frame(sdata@data, v[match(rownames(sdata@data), rownames(v)),])

# Run randomForests Model
rf.mdl <- randomForest(x=sdata@data[,3:ncol(sdata@data)], y=as.factor(sdata@data[,"train"]),
                       ntree=501, importance=TRUE)

# Check Error Convergence 
plot(rf.mdl)

# PLOT mean decrease in accuracy VARIABLE IMPORTANCE
varImpPlot(rf.mdl, type=1)

# Predict model
predict(xvars, rf.mdl, filename="RfClassPred.img", type="response", 
        index=1, na.rm=TRUE, progress="window", overwrite=TRUE)