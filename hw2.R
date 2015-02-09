##############################################################
# CSC 591
# Clustering Example Code
# Raju Vatsavai
##############################################################

rm(list=ls(all=T))

library(mclust)
library(lattice)
library(MASS)
library(car)
library(utils)          # for read/write csv files
library(flexclust)      # for kcca

# read the data
# Change this to correct path
d = read.table(file = "data/ds1-with-3classes.csv", header=T, sep=",")

# get field names
names (d)

# remove unnessary fields
#
du = d[,-c(1,4)]
summary(du)


# plot the data (set margins)
x1 = min(du[,1]) - 10
x2 = max(du[,1]) + 10
y1 = min(du[,2]) - 10
y2 = max(du[,2]) + 10

plot(du, main = "Data Distributions", xlim=c(x1,x2), ylim=c(y1,y2), pch=c(19), sub="2D Data")

nc = 3
km = kcca(du, k=nc)

#more about km object
summary(km)

# get cluster labels
du.cl = clusters(km)

# you can subset data by cluster id
du.cl1 = subset(du, du.cl == 1)
du.cl2 = subset(du, du.cl == 2)
du.cl3 = subset(du, du.cl == 3)

# you can plot the data
# first plot all data points
plot(du, col=c(1), main = "Cluster Distributions", xlim=c(x1,x2), ylim=c(y1,y2), pch=c(19), sub="KMeans Results")

# you should do this in a for loop: for (i in 1:3)
i = 1
points(du.cl1, col=c(i+1), pch=c(1+i))
points(du.cl2, col=c(i+2), pch=c(2+i))
points(du.cl3, col=c(i+3), pch=c(3+i))

# see how clusters map to classes
table(d[,4], du.cl)

# Do model based clustering
#
g = 1:5
du.mc = Mclust(du, G=g)

summary(du.mc)
plot(du.mc$BIC[,1])
plot(du.mc$BIC[,9])  # best

# compare kmeans quality with model based clustering
table(d$cl, du.mc$classification)

# plot mean and covariance (as ellipsoid plots)
mc.d = cbind(du, cl=du.mc$classification)
plotGMM(mc.d, du.mc$G, 1, lt=1, st = paste("Clusters Found by Model Selection (BIC) = ", du.mc$G))


# plot both kmeans and model beased clustering resuls
opar <- par(mfrow = c(1, 1))
plot(du, col=c(1), main = "Cluster Distributions", xlim=c(x1,x2), ylim=c(y1,y2), pch=c(19), sub="KMeans Results")
i = 1
points(du.cl1, col=c(i+1), pch=c(1+i))
points(du.cl2, col=c(i+2), pch=c(2+i))
points(du.cl3, col=c(i+3), pch=c(3+i))
#
plotGMM(mc.d, du.mc$G, 1, lt=1, st = paste("Clusters Found by Model Selection (BIC) = ", du.mc$G))
par(opar)

# analytical comparision
table(d$cl, du.mc$classification)
table(d$cl, du.cl)


# additional methods
# first run this funtion before calling it
#
plotGMM = function(d, nClasses, nWindow, lt=2, st=c(""))
{
  #lType1 = lt  # solid
  #lType2 = 2 # dotted
  # pch = 20
  
  print("GMM plot in progress")
  print(paste("Data dimensions ", dim(d)))
  print(paste("No of classes ", nClasses))
  
  if (nWindow) dev.new()
  #du = d[,-dim(d)[2]]
  du = d[,c(1,2)]
  x1 = 0
  x2 = 100
  y1 = 0
  y2 = 100
  
  if (dim(d)[2] > 4)
  {
    du = d[,c(3,4)]
    x2 = max(d[,3]) + 10
    y2 = max(d[,4]) + 10
    x1 = min(d[,3])
    y1 = min(d[,4])
  }
  
  
  # main plot - all points
  if (nWindow) plot(du, col=c(1), main = "Cluster Distributions",
                    xlim=c(x1,x2), ylim=c(y1,y2), pch=c(19), sub=st)
  
  for (i in 1:nClasses)
  {
    di = subset(du, d$cl == i)
    di.mu = colMeans(di)
    di.sigma = stats::cov(di)
    
    points(di, col=c(i), pch=c(0+i))
    ellipse(di.mu, di.sigma, 2, col=c(i), lty = lt)
    text(di.mu[1]+1, di.mu[2]+1, labels=c(i), col=c(i))
  }
}