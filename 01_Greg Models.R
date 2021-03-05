
# 0_Greg Script ####

library(tidyverse); library(magrittr); library(ggregplot); library(cowplot); library(colorspace)
library(GGally); library(patchwork)

theme_set(theme_cowplot())

DF <- readRDS("Data/CleanData.rds")

# DF$Num.fledglings %>% qplot

Resps <- c("April.hatch.date",
           "April.lay.date",
           "Clutch.size",
           "Incubation.duration",
           # "Incubation.started",
           # "Laying.rate",
           "Mean.chick.weight",
           "Num.chicks",
           # "Num.dead.chicks",
           # "Num.eggs.weighed",
           "Num.fledglings", 
           "Total.egg.weight")

SocialCovar <- c("Degree", "BondStrength", "Betweenness", "Eigenvector",
                 #"Strength", "Strength_Mean", 
                 "Clustering")

DensityCovar <- c("LifetimeDensity", "AnnualDensity")

# DF %>% mutate_at(SocialCovar, ~as.numeric(as.character(.x))) %>% 
# dplyr::select(all_of(SocialCovar)) %>% ggpairs()

# DF %>% mutate_at(Resps, ~as.numeric(as.character(.x))) %>%
#   dplyr::select(all_of(Resps)) %>%
#   ggpairs()

ClashList <- list(
  c(SocialCovar[c(1, 3:5)]),
  DensityCovar
  )

Covar <- c("Focal.age", "Focal.sex", "Year.w")

r <- 1

IMList <- list()

Resps %<>% sort

for(r in r:length(Resps)){
  
  print(Resps[r])
  
  if(0){
    
    TestDF <- DF %>% dplyr::select(all_of(Covar), Focal.ring, Resps[r], X, Y) %>% na.omit
    
    IM1 <- INLAModelAdd(Data = TestDF, Response = Resps[r], 
                        Explanatory = Covar, 
                        Add = "f(Focal.ring, model = 'iid')",
                        # Random = "Focal.ring", RandomModel = "iid", 
                        AddSpatial = T)
    
    IMList[[Resps[r]]]$Base <- IM1
    
    IM1$FinalModel %>% list(IM1$Spatial$Model) %>% INLADICFig()
    
    IM1$Spatial$Model %>% ggField(
      Mesh = IM1$Spatial$Mesh
      
    ) + scale_fill_discrete_sequential(palette = "Mint")
    
  }
  
  TestDF <- DF %>% 
    dplyr::select(all_of(Covar), all_of(SocialCovar), all_of(DensityCovar), 
                  Focal.ring, Resps[r], X, Y) %>% 
    mutate_at(SocialCovar, ~as.numeric(as.character(.x))) %>%
    mutate_at(SocialCovar[3], ~log(.x+1)) %>% mutate_at(SocialCovar[4], ~kader:::cuberoot(.x)) %>% 
    na.omit
  
  TestDF %<>% mutate_at(c(Resps[r], SocialCovar), ~c(scale(.x)))
  
  IM2 <- INLAModelAdd(Data = TestDF, 
                      Response = Resps[r], 
                      Explanatory = Covar, 
                      Add = SocialCovar %>% c(DensityCovar),
                      AllModels = T,
                      Base = T,
                      # Rounds = 1,
                      Clashes = ClashList,
                      Random = "Focal.ring", RandomModel = "iid",
                      AddSpatial = T)
  
  # IM2$FinalModel %>% Efxplot()
  
  IMList[[Resps[r]]] <- IM2
  
}

IMList %>% map("FinalModel") %>% 
  Efxplot(ModelNames = Resps, PointOutline = T) +
  scale_colour_brewer(palette = "Spectral") +
  IMList %>% map(c("Spatial", "Model")) %>% 
  Efxplot(ModelNames = Resps, PointOutline = T) +
  scale_colour_brewer(palette = "Spectral") +
  plot_layout(guides = "collect")

IMList %>% map(~list(.x$FinalModel, .x$Spatial$Model) %>% INLADICFig) %>% ArrangeCowplot()

IMList %>% names %>% 
  map(~ggField(IMList[[.x]]$Spatial$Model, IMList[[.x]]$Spatial$Mesh) + 
        labs(fill = .x) +
        scale_fill_discrete_sequential(palette = "Mint")) %>% 
  ArrangeCowplot() + 
  ggsave("Fields.jpeg", units = "mm", width = 400, height = 300)

# Sociality as a response ####

SpocialList <- list()

# Resps %<>% sort

r <- 1

TestDF <- DF %>% 
  dplyr::select(all_of(Covar), 
                all_of(DensityCovar), 
                all_of(SocialCovar), Focal.ring, X, Y) %>% 
  mutate_at(SocialCovar, ~as.numeric(as.character(.x))) %>%
  mutate_at(SocialCovar[3], ~log(.x+1)) %>% mutate_at(SocialCovar[4], ~kader:::cuberoot(.x)) %>% 
  na.omit

TestDF %<>% mutate_at(c(SocialCovar), ~c(scale(.x)))

for(r in r:length(SocialCovar)){
  
  print(Resps[r])
  
  IM2 <- INLAModelAdd(Data = TestDF, 
                      Response = SocialCovar[r], 
                      Explanatory = Covar, 
                      Add = DensityCovar,
                      Clashes = ClashList,
                      AllModels = T,
                      Base = T,
                      Random = "Focal.ring", RandomModel = "iid",
                      AddSpatial = T)
  
  # IM2$FinalModel %>% Efxplot()
  
  SpocialList[[SocialCovar[r]]] <- IM2
  
}

SpocialList %>% map(~list(.x$FinalModel, .x$Spatial$Model) %>% INLADICFig) %>% ArrangeCowplot()

SpocialList %>% names %>% 
  map(~ggField(SpocialList[[.x]]$Spatial$Model, 
               SpocialList[[.x]]$Spatial$Mesh,
               Points = SpocialList[[.x]]$Data[,c("X", "Y")], 
               PointAlpha = 0.2) + 
        labs(fill = .x) +
        scale_fill_discrete_sequential(palette = "Mint")) %>% 
  ArrangeCowplot() + 
  ggsave("SpocialFields.jpeg", units = "mm", width = 400, height = 300)

SpocialList %>% map("FinalModel") %>% Efxplot(ModelNames = Resps) +
  SpocialList %>% map(c("Spatial", "Model")) %>% Efxplot(ModelNames = Resps) +
  plot_layout(guides = "collect")
