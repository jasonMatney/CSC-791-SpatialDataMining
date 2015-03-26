### R code from vignette source 'predictive-process-spLM.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: libs
###################################################
options(width = 85) 
library(yaImpute)
library(spBayes)
library(MBA)
library(geoR)
library(fields)
library(rgdal)
library(RgoogleMaps)
library(raster)
library(glcm)


###################################################
### code chunk number 2: coords
###################################################
#get data
PEF.shp <- readOGR("PEF-data","PEF-bounds")
PEF.poly <- as.matrix(PEF.shp@polygons[[1]]@Polygons[[1]]@coords)

PEF.plots <- read.csv("PEF-data/PEF-plots.csv")

##drop rows with NA biomass
PEF.plots <- PEF.plots[sapply(PEF.plots[,"biomass.mg.ha"], function(x){!any(is.na(x))}),]

##get plot coordinates and transform biomass
coords <- PEF.plots[,c("easting", "northing")]
bio <- sqrt(PEF.plots$biomass.mg.ha)

##plot
plot(coords, pch=19, cex=0.5, xlab="Easting (m)", ylab="Northing (m)")
plot(PEF.shp, add=TRUE, usePolypath=FALSE)


###################################################
### code chunk number 3: MBASurf
###################################################
surf <- mba.surf(cbind(coords, bio), no.X=200, no.Y=200, extend=FALSE)$xyz.est
image.plot(surf, xaxs = "r", yaxs = "r", xlab="Easting (m)", ylab="Northing (m)")
plot(PEF.shp, add=TRUE, usePolypath = FALSE)
points(coords)


###################################################
### code chunk number 4: SVD
###################################################
##some useful functions
eof.fit <- function(X, p){s <- svd(X);s$u[,1:p]%*%diag(s$d[1:p])%*%t(s$v[,1:p])}

eof <- function(X, p){svd(X)$u[,1:p]}

eof.summary <- function(X, per=c(25,50,95), col=c(1:3)){
  
  s <- svd(X)
  svd.var <- s$d^2/sum(s$d^2)*100
  svd.cum.var <- cumsum(s$d^2/sum(s$d^2))*100
  svd.per <- (1:ncol(X))[sapply(per, function(p){min(which(svd.cum.var>=p))})]
  svd.col <- cumsum(s$d[col]^2/sum(s$d^2))*100
  
  list(var=svd.var, cum.var=svd.cum.var, per.exp=svd.per, per=per, col.exp=svd.col, col=col)
}

##get LiDAR data
lvis <- as.matrix(read.csv("PEF-data/PEF-LVIS.csv"))

##extract signals and scale
X <- scale(lvis[,3:ncol(lvis)], scale=FALSE)

##svd summary
eof.var <- eof.summary(X, per=c(50,90,95,99), col=c(1:5))

par(mfrow=c(1,2), "mar"=c(4, 6, 4, 2))##bottom, left, top and right 
plot(eof.var[[1]], ylim=c(0,100), cex.lab=2, xlab="SVD column", ylab="Percent variance explained", pch=19, cex=0.2)
abline(v = eof.var[[3]], col=rainbow(length(eof.var[[3]])))
legend("topright", lty=1, legend=paste("Column ",eof.var[[3]],": ",eof.var[[4]],"%",sep=""), col=rainbow(length(eof.var[[3]])), bty="n")

plot(eof.var[[2]], ylim=c(0,100),cex.lab=2, xlab="SVD column", ylab="Cumulative percent variance explained", pch=19, cex=0.2)
abline(v = eof.var[[3]], col=rainbow(length(eof.var[[3]])))
legend("bottomright", lty=1, legend=paste("Column ",eof.var[[3]],": ",eof.var[[4]],"%",sep=""), col=rainbow(length(eof.var[[3]])), bty="n")


###################################################
### code chunk number 5: SVDFitted
###################################################
par(mfrow=c(2,2), "mar"=c(4, 6, 4, 2))##bottom, left, top and right
X.fit <- eof.fit(X,4)
 for(i in 1:4){
   plot(X[i,], 1:ncol(X), typ="l", ylab="Vertical (Canopy height)", xlab="Standardized signal")
   lines(X.fit[i,], 1:ncol(X), col="blue")
   ##readline(prompt = "Pause. Press <Enter> to continue...")
 }


###################################################
### code chunk number 6: SVDSurf
###################################################
lvis.coords <- lvis[,1:2]
Z <- eof(X, 4)

z.1 <- mba.surf(cbind(lvis.coords, Z[,1]), no.X=200, no.Y=200, h=10, extend=TRUE)$xyz.est
z.2 <- mba.surf(cbind(lvis.coords, Z[,2]), no.X=200, no.Y=200, h=10, extend=TRUE)$xyz.est
z.3 <- mba.surf(cbind(lvis.coords, Z[,3]), no.X=200, no.Y=200, h=10, extend=TRUE)$xyz.est
z.4 <- mba.surf(cbind(lvis.coords, Z[,4]), no.X=200, no.Y=200, h=10, extend=TRUE)$xyz.est

par(mfrow=c(2,2))
image.plot(z.1, xaxs = "r", yaxs = "r", xlab="", ylab="Northing (m)")
image.plot(z.2, xaxs = "r", yaxs = "r", xlab="", ylab="")
image.plot(z.3, xaxs = "r", yaxs = "r", xlab="Easting (m)", ylab="Northing (m)")
image.plot(z.4, xaxs = "r", yaxs = "r", xlab="Easting (m)", ylab="")


###################################################
### code chunk number 7: Google
###################################################
##try and guess the different PCs using a Google image
##set shapefiel projection (missing .prj file)
proj4string(PEF.shp) <- CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") ## set the projection
PEF.shp.LL <- spTransform(PEF.shp, CRS("+proj=longlat")) ## reproject to lat/long

##grab a Google image
bounds <- bbox(PEF.shp.LL) 
img <- GetMap.bbox(bounds[1,],bounds[2,], maptype = "satellite")

bounds <- img$BBOX
img <- stack("MyTile.png") ## create a RasterStack
extent(img)=extent(bounds$ll[,2],bounds$ur[,2],bounds$ll[,1],bounds$ur[,1]) ## set the bounds of the RasterStack

plotRGB(img)
plot(PEF.shp.LL, add = TRUE, border = "yellow", usePolypath = FALSE)



###################################################
### code chunk number 8: MBAResids
###################################################
##match lvis to PEF.plots
PEF.plots.Z <- Z[ann(as.matrix(lvis.coords), as.matrix(coords))$knnIndexDist[,1],]

##non-spatial model
bio <- sqrt(PEF.plots$biomass.mg.ha)

m <- lm(bio ~ PEF.plots.Z)
summary(m)
bio.resid <- resid(m)

resid.surf <- mba.surf(cbind(coords, bio.resid), no.X=200, no.Y=200, extend=FALSE)$xyz.est
image.plot(resid.surf, xaxs = "r", yaxs = "r", main = "Square-root metric tons of biomass residuals", xlab="Easting (m)", ylab="Northing (m)")


###################################################
### code chunk number 9: geoRVariog
###################################################
max.dist <- 0.5*max(iDist(coords))
bins <- 15

vario.bio <- variog(coords=coords, data=bio, uvec=(seq(0, max.dist, length=bins)))

fit.bio <-variofit(vario.bio, ini.cov.pars=c(5, 500), ##sigma^2 and 1/phi 
                         cov.model="exponential", minimisation.function="nls", weights="equal")

vario.bio.resid <- variog(coords=coords, data=bio.resid, uvec=(seq(0, max.dist, length=bins)))

fit.bio.resid <-variofit(vario.bio.resid, ini.cov.pars=c(3, 500),
                         cov.model="exponential", minimisation.function="nls", weights="equal")

par(mfrow=c(1,2))
plot(vario.bio, main="Square-root metric tons of biomass")
lines(fit.bio)
abline(h=fit.bio$nugget, col="blue")##nugget
abline(h=fit.bio$cov.pars[1]+fit.bio$nugget, col="green")##sill
abline(v=-log(0.05)*fit.bio$cov.pars[2], col="red3")##effective range

plot(vario.bio.resid, main="Square-root metric tons of biomass residuals")
lines(fit.bio.resid)
abline(h=fit.bio.resid$nugget, col="blue")
abline(h=fit.bio.resid$cov.pars[1]+fit.bio.resid$nugget, col="green")
abline(v=-log(0.05)*fit.bio.resid$cov.pars[2], col="red3")



###################################################
### code chunk number 10: spLM
###################################################
n.samples <- 2000

starting <- list("phi"=3/1000, "sigma.sq"=2, "tau.sq"=2)

tuning <- list("phi"=0.5, "sigma.sq"=0.01, "tau.sq"=0.01)

p <- ncol(PEF.plots.Z)+1

priors <- list("beta.Norm"=list(rep(0,p), diag(1000000,p)),
               "phi.Unif"=c(3/max.dist, 3/10), "sigma.sq.IG"=c(2, 2),
               "tau.sq.IG"=c(2, 2))

cov.model <- "exponential"

m.1 <- spLM(bio ~ PEF.plots.Z, coords=as.matrix(coords), starting=starting, knots=c(10,10),
            tuning=tuning, priors=priors, cov.model=cov.model,
            n.samples=n.samples, n.report=1000)


###################################################
### code chunk number 11: spLM2
###################################################
n.batch <- 40
batch.length <- 50
n.samples <- n.batch*batch.length

m.2 <- spLM(bio ~ PEF.plots.Z, coords=as.matrix(coords), starting=starting, knots=c(10,10),
            tuning=tuning, priors=priors, cov.model=cov.model,
            amcmc=list("n.batch"=n.batch, "batch.length"=batch.length, "accept.rate"=0.43),
            n.report=10)


###################################################
### code chunk number 12: thetaSamples
###################################################
theta.samps <- mcmc.list(m.1$p.theta.samples, m.2$p.theta.samples)
plot(theta.samps, density=FALSE)


###################################################
### code chunk number 13: betaSamples
###################################################
beta.samps <- mcmc.list(m.1$p.beta.samples, m.2$p.beta.samples)
plot(beta.samps, density=FALSE)


###################################################
### code chunk number 14: spSummary
###################################################
theta.samps <- mcmc.list(m.1$p.theta.samples, m.2$p.theta.samples)
round(summary(theta.samps)$quantiles,3)


###################################################
### code chunk number 15: wSamples
###################################################
burn.in <- 0.75*n.samples
m.1 <- spRecover(m.1, start=burn.in, thin=10)
m.2 <- spRecover(m.2, start=burn.in, thin=10)
m.1.w <- m.1$p.w.recover.samples
m.2.w <- m.2$p.w.recover.samples

w.samps <- cbind(m.1.w, m.2.w)

w.summary <- apply(w.samps, 1, function(x){quantile(x, prob=c(0.025,0.5,0.975))})

w.surf <- mba.surf(cbind(coords, w.summary[2,]), no.X=200, no.Y=200)$xyz.est

z.lim <- range(w.surf[["z"]], resid.surf[["z"]], na.rm=TRUE)

par(mfrow=c(1,2))
image.plot(resid.surf, zlim=z.lim, xaxs = "r", yaxs = "r", main="Non-spatial model residuals", xlab="Easting (m)", ylab="Northing (m)")
plot(PEF.shp, add=TRUE, usePolypath = FALSE)
points(coords)
image.plot(w.surf, zlim=z.lim, xaxs = "r", yaxs = "r", main="Spatial random effects", xlab="Easting (m)", ylab="Northing (m)")
plot(PEF.shp, add=TRUE, usePolypath = FALSE)
points(coords)
points(m.1$knot.coords, pch=19, cex=0.5)


###################################################
### code chunk number 16: modelComp
###################################################
##model comparison
m <- lm(bio ~ PEF.plots.Z)
m.4 <- bayesLMRef(m, n.samples)
spDiag(m.4, start=burn.in)
spDiag(m.1, start=burn.in)

##################
### GLCM 
##################
## Not run:
require(raster)
raster(L5TSR_1986, layer=1)
par(mfrow=c(1,1))
plotRGB(L5TSR_1986, 3, 2, 1, stretch='lin')

# Calculate GLCM textures using default 90 degree shift
textures_shift1 <- glcm(raster(L5TSR_1986, layer=1))
plot(textures_shift1)
# Calculate GLCM textures over all directions
textures_all_dir <- glcm(raster(L5TSR_1986, layer=1),
                         shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)))
plot(textures_all_dir)

## End(Not run)


