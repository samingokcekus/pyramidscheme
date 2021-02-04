setwd("~/Documents/2/Familiar_neighbors/DATA")

#part one##### 
###########download packages, load in data 
if(!require(ggregplot)) devtools::install_github("gfalbery/ggregplot") # Installing Greg's package for plotting functions!
install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(c("graph", "Rgraphviz"), dep=TRUE)

library(INLA); library(ggplot2); library(ggregplot)
library(tidyverse)
library(RColorBrewer)

root <- "~/Documents/2/Familiar_neighbors/DATA" #path to working directory
  
xdata <- readRDS("~/Documents/2/Familiar_neighbors/DATA/fnbasedata_full.Rda")
#xdata <- readRDS("fnbasedata_full.Rda")

##### 
phen <- c("box", "focal.ring", "x", "y") #b columns with spatial information we'll need

resp <- "num.fledglings" #response variable

covar <- c("focal.age", #age
           "focal.sex", #sex
           "year.s", # year ??
           "focal.wborn") # wytham born 

Testxdata <- na.omit(xdata[, c(phen, resp, covar)]) #remove NA
#using [] to subset and only extract specific columns

#turning variables into factors
Testxdata$year.s <- as.factor(Testxdata$year.s)
Testxdata$box <- as.factor(Testxdata$box)

###########PLOT THE SAMPLING LOCATIONS IN SPACE
#setting up a custom theme
THEME <- theme(axis.text.x = element_text(size = 12,colour = "black"),
               axis.text.y = element_text(size = 12, colour = "black"),
               axis.title.x = element_text(vjust = -0.35),
               axis.title.y = element_text(vjust = 1.2)) + theme_bw()

(samp_locations <- ggplot(Testxdata, aes(x, y)) + 
    geom_jitter(aes(colour = factor(box))) + coord_fixed() + 
    THEME + 
    labs(colour = "box")) #this is not working? 


#part two, perform model selection in INLA#####
###########set up a full analysis using all covariates that we think will influence data 

#first without random effects####

#specify the formula
f0.1 <- as.formula(paste0(resp, " ~ ", # Response first
                          paste(covar, collapse = " + ") # Collapse the vector of covariates
))

#trying it with lm - works ... 
trylm <- lm(Testxdata$num.fledglings ~ Testxdata$focal.age + Testxdata$focal.sex + Testxdata$year.s + Testxdata$focal.wborn)


#STUCK HERE 

# Run the model
IM0.1  <- inla(num.fledglings ~ focal.age + focal.sex + year.s + focal.wborn, 
               family = "gaussian", # Specify the family. Can be a wide range (see r-inla.org).
               data = Testxdata, verbose=TRUE) # Specify the data

# Run the model # (This is the same thing)
IM0.1  <- inla(f0.1, 
               family = "nbinomial", # Specify the family. Can be a wide range (see r-inla.org).
               data = Testxdata) # Specify the data









#INLA tutorial notes ####
# Then with an ID random effect ####
#
#f0.2 <- as.formula(paste0(resp, " ~ ", 
#                          paste(covar, collapse = " + "), 
#                          " +  f(ID, model = 'iid')")) # This is how you include  a typical random effect.
#
#IM0.2  <- inla(f0.2, 
#               family = "nbinomial",
#               data = Testxdata) 
#
#summary(IM0.1)
#summary(IM0.2)
#
##visualize
#Efxplot(list(IM0.1, IM0.2))
#
##Carry out model selection to remove the covariates that are unimportant. 
##This involves removing covariates one by one and seeing how this changes model fit according to the model’s Deviance Information Criterion (DIC, a Bayesian measure analogous to the Akaike Information Criterion (AIC)). 
##If removing any number of covariates does not increase a model’s DIC by a threshold number (I use 2 DIC) then the covariate with the lowest impact is removed. 
##This process is repeated, using fewer and fewer covariates each time, until eventually you end up with a minimal model where removing any covariates increases the DIC by greater than the threshold value.
##There's a function for this: INLAModelSel 
## Let's try it on our data
#
#HostModelSel <- INLAModelSel(resp, covar, "ID", "iid", "nbinomial", Testxdata)
#
#Finalcovar <- HostModelSel$Removed[[length(HostModelSel$Removed)]]
#
##no p-values in INLA, importance or significance of variables can be deduced by examining the overlap of their 2.5% and 97.5% posterior estimates with zero
##easiest to look at this by plotting 
#
#f1 <- as.formula(paste0(resp, " ~ ", 
#                        paste(Finalcovar, collapse = " + "), 
#                        "+  f(ID, model = 'iid')")) 
#
#IM1 <- inla(f1,
#            family = "nbinomial",
#            data = Testxdata,
#            control.compute = list(dic = TRUE)) 
#
#summary(IM1)
#
##part three, learn the components of INLA#####
##uses a SPDE (Stochastic Partial Differentiation Equation) to estimate the spatial autocorrelation of the data
##creates a "mesh" using discrete sampling locations to estimate a continuous process in space 
#
##part four, set up a spatial analysis#####
##setting up a mesh
#Locations = cbind(Testxdata$X, Testxdata$Y) # using the sampling locations 
#
#MeshA <- inla.mesh.2d(jitter(Locations), max.edge = c(20, 40))
#MeshB <- inla.mesh.2d(Locations, max.edge = c(20, 40))
#MeshC <- inla.mesh.2d(Locations, max.edge = c(10, 20))
#
#Mesh <- MeshB
#
#plot(MeshA)
#
#plot(MeshB) #good for exploration and preliminary analayses
#
#plot(MeshC) #good for analyses being reported in a paper
#
#points(Locations, col = "red", pch = 2)
#
##several imporant aspects of mesh...
##triangle size is determined by the max.edge and cutoff
##this determins how precisely the equations are tailored by data 
##using smaller triangles increases precision but increases computing pwoer 
#
##allow some space around sampling area for INLA to estimate
##edge triangles can be bigger to reduce computing power 
#
##after setting up mesh, need to convert to model format 
##using an "A matrix"  - translates spatial locations on the mesh into vectors in the model 
#
## Making the A matrix
#
#HostsA <- inla.spde.make.A(Mesh, loc = Locations) # Making A matrix
#Hosts.spde = inla.spde2.pcmatern(mesh = Mesh, prior.range = c(10, 0.5), prior.sigma = c(.5, .5)) # Making SPDE
#w.Host <- inla.spde.make.index('w', n.spde = Hosts.spde$n.spde) # making the w
#
##combine the A matrix with the model matrix and random effects in the "stack" format 
#
#
## Making the model matrix 
#
#X0 <- model.matrix(as.formula(paste0(" ~ -1 + ", paste(Finalcovar, collapse = " + "))), data = Testxdata) # make the model matrix using the final model selection formula without a response variable.
#
#X <- as.data.frame(X0[,-which(colnames(X0)%in%c("Month7"))]) # convert to a data frame. Eliminate the base level of the first categorical variable if applicable (you will manually specify an intercept below) 
#
#head(X)
#
## Making the stack
#
#N <- nrow(Testxdata)
#
#StackHost <- inla.stack(
#  data = list(y = Testxdata[,resp]), # specify the response variable
#  
#  A = list(1, 1, 1, HostsA), # Vector of Multiplication factors for random and fixed effects              
#  
#  effects = list(
#    
#    Intercept = rep(1, N), # specify the manual intercept!
#    
#    X = X, # attach the model matrix
#    
#    ID = Testxdata$ID, # insert vectors of any random effects
#    
#    w = w.Host)) # attach the w 
#
##the stack includes...
##the response variable (y)
##a vector of multiplication factors (a series of 1s for the itnercept, random effects, and fixed effects)...
##..followed by the spatial A matrix 
##the effects - intercept, random effects, model matrix, and spde need to specified separately 
#
##adding an effect necessitates adding another 1 to the multiplication factors... 
#
#
##for eample, add a random effect of grid 
#N <- nrow(Testxdata)
#
#GOODSTACK <- inla.stack(
#  data = list(y = Testxdata[,resp]), # specify the response variable
#  
#  A = list(1, 1, 1, 1, HostsA), # Vector of Multiplication factors for random and fixed effects              
#  
#  effects = list(
#    
#    Intercept = rep(1, N), # specify the manual intercept!
#    
#    X = X, # attach the model matrix
#    
#    ID = Testxdata$ID, # insert vectors of any random effects
#    Grid = Testxdata$Grid,
#    
#    w = w.Host)) # Leave
#
#
##running the model... 
##put all the components in to the inla function 
#
##trying three competing models 
##only fixed effects
##fixed + ID random effects 
##fixed + ID + SPDE random effects
#
#f1 <- as.formula(paste0("y ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + ")))
#f2 <- as.formula(paste0("y ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + "), " +  f(ID, model = 'iid')"))
#f3 <- as.formula(paste0("y ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + "), " +  f(ID, model = 'iid') + f(w, model = Hosts.spde)"))
#
#
#IM1 <- inla(f1, # Base model (no random effects)
#            family = "nbinomial",
#            data = inla.stack.data(StackHost),
#            control.compute = list(dic = TRUE),
#            control.predictor = list(A = inla.stack.A(StackHost))
#)
#
#IM2 <- inla(f2, # f1 + Year and ID random effects
#            family = "nbinomial",
#            data = inla.stack.data(StackHost),
#            control.compute = list(dic = TRUE),
#            control.predictor = list(A = inla.stack.A(StackHost))
#)
#
#IM3 <- inla(f3, # f2 + SPDE random effect 
#            family = "nbinomial",
#            data = inla.stack.data(StackHost),
#            control.compute = list(dic = TRUE),
#            control.predictor = list(A = inla.stack.A(StackHost))
#)
#
#SpatialHostList <- list(IM1, IM2, IM3)
#
##plotting the spatial field... 
#ggField(IM3, Mesh, Groups = 1) +
#  scale_fill_brewer(palette = "Blues") 
#
## always use a single-dimension colour palette if you can! It's just easier on the eyes, 
## better for colourblind people, makes sense in black and white, etc.
#
## ignore the Groups part of the function for now. That'll come later.
#
#
##looking at the range
## function takes (a list of) models and plots the decay of spatial autocorrelation across a user-defined range
#
## let's try it on our model ###
#
## Define the maximum range as something reasonable: the study area is 80 eastings wide, so lets go for:
#
#Maxrange = 40
#
#INLARange(list(IM3), maxrange = Maxrange) #this didn't work???
#
##look at model fit to understand if autocorrelation is substantialy impacting model 
#sapply(SpatialHostList, function(f) f$dic$dic)
## Let's try it on our data #### visualize, 
#
#INLADICFig(SpatialHostList, ModelNames = c("Base", "IID", "SPDE"))
##so spatial autocorrelation does not impact the data here (in the way we coded it)
#
##if you didn't have any prior expectations about data, you would stop here...
#
##part five, modify and specific spatial INLA models ##### 
#
##what is spatial field varies seasonally? can specify A matrix, SPDE and model differently to produce different groups 
#
## Specifying a new set of SPDE components 
#
#Groups = "Month"
#
#NGroups <- length(unique(Testxdata[,Groups])) 
#
#HostA2 <- inla.spde.make.A(Mesh, # Leave
#                           loc = Locations, # Leave
#                           group = as.numeric(as.factor(Testxdata[,Groups])),# this must be a numeric value counting from 1. If the groups variable is a factor, this will happen by default.
#                           n.group = NGroups) 
#
#w.Host2 <- inla.spde.make.index(
#  name    = 'w', 
#  n.spde  = Hosts.spde$n.spde,
#  n.group = NGroups)  
#
#StackHost2 <- inla.stack( 
#  data = list(y = Testxdata[,resp]), # Leave
#  
#  A = list(1, 1, 1, HostA2), # Change the A matrix to the new one
#  
#  effects = list(
#    Intercept = rep(1, N), # Leave
#    X = X, # Leave
#    ID = Testxdata$ID, # Leave
#    
#    w = w.Host2)) # CHANGE
#
#
##run the model 
#f4 = as.formula(paste0("y ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + "), 
#                       " +  f(ID, model = 'iid') +  f(w, model = Hosts.spde, 
#group = w.group,                           # This bit is new! 
#control.group = list(model = 'iid'))"))
#
#inla.setOption(num.threads = 8) 
#
#IM4 <- inla(f4,
#            family = "nbinomial",
#            data = inla.stack.data(StackHost2), # Don't forget to change the stack!
#            control.compute = list(dic = TRUE),
#            control.predictor = list(A = inla.stack.A(StackHost2)) # Twice!
#)
#
#SpatialHostList[[4]] <- IM4
#
##and now plot it 
#
#Labels = c("July", "August", "September", "October", "November")
#names(Labels) <- c(1:NGroups)
#
#ggField(IM4, Mesh, Groups = NGroups) + # Notice the groups argument, using the number of unique months.
#  scale_fill_brewer(palette = "Reds") + 
#  facet_wrap( ~ Group, labeller = labeller(Group = Labels), ncol = 3) # Doing this manually changes the facet labels
#
#INLARange(SpatialHostList[3:4], maxrange = Maxrange, mesh = Mesh, ModelNames = c("Full", "Monthly")) #this function didn't work again... 
#
#
##part six, learn about spatiotemporal analysis ##### 
##can use an exchangeable model to force a correlation between temporal and spatial fields and derive rho correlation between them 
#
#f5 = as.formula(paste0("y ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + "), 
#                       "+ f(ID, model = 'iid') +  f(w, model = Hosts.spde, 
#                       group = w.group, # This bit is new! 
#                       control.group = list(model='exchangeable'))"))
#
##inla.setOption(num.threads = 8) 
#
#IM5 <- inla(f5,
#            family="nbinomial",
#            data = inla.stack.data(StackHost2),
#            control.compute = list(dic = TRUE),
#            control.predictor = list(A = inla.stack.A(StackHost2))
#)
#
#SpatialHostList[[5]] <- IM5
#
##with Exchangeable, all fields are correlated to the same extent. If we used AR1 (a typical temporal autocorrelation model used to link spatial fields), fields closer to each other in time would be more highly correlated than those further apart
#
## Same functions as above! 
#
#INLADICFig(SpatialHostList, ModelNames = c("Base", "IID", "SPDE", "SPDE2", "SPDE3"))
#
#ggField(IM5, Mesh, Groups = NGroups) + # Notice the groups argument, using the number of unique months.
#  scale_fill_brewer(palette = "Greens") 
#
#INLARange(SpatialHostList[3:5], maxrange = Maxrange, ModelNames = c("Full", "Monthly", "Monthly2")) #UGH
#
##within-grid model...  to see if restricting the study area to four identically-shaped grid meshes will improve fit
##you can use repl instead of group but this only works when you're not specifying a link between fields 
#
##need to recode data slightly 
#
#Group2 = "Grid"
#
#Testxdata$Easting2 <- Testxdata$Easting - with(Testxdata, tapply(Easting, Grid, min))[Testxdata$Grid]
#Testxdata$Northing2 <- Testxdata$Northing - with(Testxdata, tapply(Northing, Grid, min))[Testxdata$Grid]
#
#Locations2 = cbind(Testxdata$Easting2, Testxdata$Northing2)
#
#Mesh2 <- inla.mesh.2d(Locations2, max.edge = c(20, 40))#, cutoff = 0.8)
#
#NGroup2 <- length(unique(Testxdata[,Group2]))
#
#Hosts.spde2 = inla.spde2.pcmatern(mesh = Mesh2, prior.range = c(10, 0.5), prior.sigma = c(.5, .5)) # Making SPDE
#
#HostA3 <- inla.spde.make.A(Mesh2, loc = Locations2,
#                           repl = as.numeric(Testxdata[,Group2]),
#                           n.repl = NGroup2)
#
#w.Host3 <- inla.spde.make.index(
#  name    = 'w', 
#  n.spde  = Hosts.spde2$n.spde,
#  n.repl = NGroup2)  
#
#StackHost3 <- inla.stack(
#  data = list(y = Testxdata[,resp]),  
#  A = list(1, 1, 1, HostA3), # Change A matrix
#  effects = list(
#    
#    Intercept = rep(1, N), # Leave
#    
#    X = X, # Leave
#    
#    ID = Testxdata$ID, # Leave
#    
#    w = w.Host3)) # Change 
#
#f6 = as.formula(paste0("y ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + "), 
#                       " +  f(ID, model = 'iid') +   
#                       f(w, model = Hosts.spde2, replicate = w.repl)")) # Not necessary to specify a linking model
#
#IM6 <- inla(f6,
#            family = "nbinomial",
#            data = inla.stack.data(StackHost3),
#            control.compute = list(dic = TRUE),
#            control.predictor = list(A = inla.stack.A(StackHost3))
#)
#
#SpatialHostList[[6]] <- IM6
#
##does this fit the data better?
#INLADICFig(SpatialHostList, ModelNames = c("Base", "IID", "SPDE", "SPDE2", "SPDE3", "GridSPDE")) 
#
##no... 
#
#Testxdata$Group <- Testxdata$Grid
#
#Labels2 <- paste0("Grid ", 1:4)
#names(Labels2) <- 1:4
#
#ggField(IM6, Mesh2, Groups = NGroup2)  + 
#  facet_wrap(~Group, labeller = labeller(Group = Labels2)) + scale_fill_brewer(palette = "Oranges") + 
#  ggsave("Fields6.png", units = "mm", width = 120, height = 100, dpi = 300)
#
#
##the best fitting model is SPDE3 (model 5)
##features different spatial fields for each month, with correlation between the fields. However, this formulation only slightly improves model fit over the non-spatial models
#
##the effect estimates barely differ between these models. So, even though space has an effect, the effect is small and doesn’t modify our previous conclusions
#Efxplot(SpatialHostList, ModelNames = c("Base", "IID", "SPDE", "SPDE2", "SPDE3", "GridSPDE"))






