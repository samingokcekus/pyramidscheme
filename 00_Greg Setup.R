
# 00_Greg Setup ###

library(tidyverse); library(magrittr); library(ggregplot); library(cowplot); library(colorspace)
library(GGally)

theme_set(theme_cowplot())

# DF <- readRDS("fnbasedata_full.Rda")

zz2012 <- readRDS("Data/fnbasedata_2012.Rda")
zz2013 <- readRDS("Data/fnbasedata_2013.Rda")
zz2014 <- readRDS("Data/fnbasedata_2014.Rda")

zz2012 %>% bind_rows(zz2013, zz2014) -> Tits

# Density ####

library(adehabitatHR)

Tits %>% 
  group_by(Focal.ring) %>% 
  summarise_at(c("X", "Y"), ~mean(.x, na.rm = T)) ->
  LifetimeCentroids

LifetimeCentroids %<>% na.omit

SPDF <- SpatialPointsDataFrame(data = LifetimeCentroids[,c("X", "Y")], 
                               coords = LifetimeCentroids[,c("X", "Y")])

LifetimeKUDL <- kernelUD(SPDF, same4all = TRUE, grid = 500)

LifetimeKUDL %>% raster::raster() %>% raster::extract(BreedingTits[,c("X", "Y")]) ->
  
  BreedingTits$LifetimeDensity

BreedingTits %>% arrange(Year) %>% pull(Year) %>% unique %>% as.character -> FocalYears

FocalYears %<>% c(min(as.numeric(FocalYears))-1, .)

BreedingTits %>% 
  filter(Year %in% FocalYears) %>% 
  group_by(`Adult female id`, Year) %>% 
  summarise_at(c("X", "Y"), 
               ~mean(.x, na.rm = T)) %>% 
  rename(XCentroidAnnual = X, YCentroidAnnual = Y) -> 
  
  AnnualCentroids

AnnualCentroids %<>% filter(Year %in% FocalYears) %>% na.omit

SPDF <- SpatialPointsDataFrame(data = AnnualCentroids[,c("XCentroidAnnual", "YCentroidAnnual", "Year")], 
                               coords = AnnualCentroids[,c("XCentroidAnnual", "YCentroidAnnual")])

SPDF <- SPDF[,"Year"]

KUDL <- kernelUD(SPDF, same4all=TRUE, grid=500)

2:length(FocalYears) %>% lapply(function(a){
  
  print(FocalYears[a])
  
  DF <- BreedingTits %>% filter(Year == FocalYears[a])
  
  KUDL2 <- KUDL[[FocalYears[a]]]
  
  KUDL2 %>% raster::raster() %>% raster::extract(DF[,c("X", "Y")]) ->
    
    DF$AnnualDensity
  
  return(DF)
  
}) -> DensityList

DensityList %>% bind_rows -> BreedingTits