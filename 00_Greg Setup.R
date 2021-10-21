
# 00_Greg Setup ###

library(tidyverse); library(magrittr); library(ggregplot); library(cowplot); library(colorspace)
library(GGally)

theme_set(theme_cowplot())

Tits <- readRDS("Data/fn2.data.full.Rds")

Tits %<>% rename_all(CamelConvert)

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

LifetimeKUDL %>% raster::raster() %>% raster::extract(Tits[,c("X", "Y")]) ->
  
  Tits$LifetimeDensity

Tits %>% arrange(Year.w) %>% pull(Year.w) %>% unique %>% as.character -> FocalYear.ws

FocalYear.ws %<>% c(min(as.numeric(FocalYear.ws))-1, .)

Tits %>% 
  filter(Year.w %in% FocalYear.ws) %>% 
  group_by(Focal.ring, Year.w) %>% 
  summarise_at(c("X", "Y"), 
               ~mean(.x, na.rm = T)) %>% 
  rename(XCentroidAnnual = X, YCentroidAnnual = Y) -> 
  
  AnnualCentroids

AnnualCentroids %<>% filter(Year.w %in% FocalYear.ws) %>% na.omit

SPDF <- SpatialPointsDataFrame(data = AnnualCentroids[,c("XCentroidAnnual", "YCentroidAnnual", "Year.w")], 
                               coords = AnnualCentroids[,c("XCentroidAnnual", "YCentroidAnnual")])

SPDF <- SPDF[,"Year.w"]

KUDL <- kernelUD(SPDF, same4all=TRUE, grid=500)

2:length(FocalYear.ws) %>% lapply(function(a){
  
  print(FocalYear.ws[a])
  
  DF <- Tits %>% filter(Year.w == FocalYear.ws[a])
  
  KUDL2 <- KUDL[[FocalYear.ws[a]]]
  
  KUDL2 %>% raster::raster() %>% raster::extract(DF[,c("X", "Y")]) ->
    
    DF$AnnualDensity
  
  return(DF)
  
}) -> DensityList

DensityList %>% bind_rows -> Tits

#adding binary success column 
Tits$Binary.succ <- as.numeric(Tits$Binary.succ)

Tits %>% saveRDS("Data/CleanData.rds")

