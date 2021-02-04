#install packages and load in data 
install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(c("graph", "Rgraphviz"), dep=TRUE)

library(INLA)

xdata <- readRDS("fnbasedata_full.Rda")

#for all of the data together 
Locations = cbind(xdata$x, xdata$y) # using nest locations

MeshA.nest.all <- inla.mesh.2d(jitter(Locations), max.edge = c(20, 40))
MeshB.nest.all <- inla.mesh.2d(Locations, max.edge = c(20, 40))
MeshC.nest.all <- inla.mesh.2d(Locations, max.edge = c(10, 20))


#2012
xdata2012 <- xdata[ which(xdata$year.s==2012),]
Locations = cbind(xdata2012$x, xdata2012$y) # using nest locations

MeshA.nest.2012 <- inla.mesh.2d(jitter(Locations), max.edge = c(20, 40))
MeshB.nest.2012 <- inla.mesh.2d(Locations, max.edge = c(20, 40))
MeshC.nest.2012 <- inla.mesh.2d(Locations, max.edge = c(10, 20))


#2013
xdata2013 <- xdata[ which(xdata$year.s==2013),]
Locations = cbind(xdata2013$x, xdata2013$y) # using nest locations

MeshA.nest.2013 <- inla.mesh.2d(jitter(Locations), max.edge = c(20, 40))
MeshB.nest.2013 <- inla.mesh.2d(Locations, max.edge = c(20, 40))
MeshC.nest.2013 <- inla.mesh.2d(Locations, max.edge = c(10, 20))

#2014
xdata2014 <- xdata[ which(xdata$year.s==2014),]
Locations = cbind(xdata2014$x, xdata2014$y) # using nest locations

MeshA.nest.2014 <- inla.mesh.2d(jitter(Locations), max.edge = c(20, 40))
MeshB.nest.2014 <- inla.mesh.2d(Locations, max.edge = c(20, 40))
MeshC.nest.2014 <- inla.mesh.2d(Locations, max.edge = c(10, 20))

