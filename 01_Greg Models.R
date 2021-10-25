
# 0_Greg Script ####

library(tidyverse); library(magrittr); library(ggregplot); library(cowplot); library(colorspace)
library(GGally); library(patchwork); library(dplyr); library(beepr)

theme_set(theme_cowplot())

DF_all <- readRDS("Data/CleanData.rds")

Resps <- c("April.lay.date",
           "Binary.succ",
           "Clutch.size",
           "Mean.chick.weight",
           "Num.fledglings")

SocialCovar <- c("Strength_mean", "Degree", #social 
                 "Bondstrength", #pair bond
                 "N.avg.bs", "N.avg.female.bs", "N.avg.male.bs", #neighbors
                 "Spatial.assoc") #spatial 

DensityCovar <- c("LifetimeDensity", "AnnualDensity")

# DF %>% mutate_at(SocialCovar, ~as.numeric(as.character(.x))) %>% 
# dplyr::select(all_of(SocialCovar)) %>% ggpairs()

# DF %>% mutate_at(Resps, ~as.numeric(as.character(.x))) %>%
#   dplyr::select(all_of(Resps)) %>%
#   ggpairs()

ClashList <- list(
  c("Strength_mean", "Degree"),
  c(SocialCovar[c(4:6)]),
  DensityCovar
)

Covar <- c("Age_num", "Year.w", "Largeoaks")

r <- 1

IMList <- list()

Resps %<>% sort

####FEMALE####

# DF <- DF_all[which(DF_all$Focal.sex == "F"),]

for(r in r:length(Resps)){
  
  print(Resps[r])
  
  if(0){
    
    TestDF <- DF_all %>% 
      filter(Focal.sex == "F") %>% 
      dplyr::select(all_of(Covar), Focal.ring, Resps[r], X, Y) %>% na.omit
    
    if(Resps[r] == "April.lay.date"){
      
      TestDF %<>% 
        filter(April.lay.date < 55)
      
    }
    
    print("Female!") 
    
    IM1 <- INLAModelAdd(Data = TestDF,
                        Response = Resps[r], 
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
  
  TestDF <- DF_all %>% 
    filter(Focal.sex == "F") %>% 
    dplyr::select(all_of(Covar), all_of(SocialCovar), all_of(DensityCovar), 
                  Focal.ring, Resps[r], X, Y) %>% 
    mutate_at("Year.w", as.factor) %>%
    mutate_at(SocialCovar, ~as.numeric(as.character(.x))) %>%
    mutate_at(SocialCovar[3], ~log(.x+1)) %>% mutate_at(SocialCovar[4], ~kader:::cuberoot(.x)) %>% 
    na.omit
  
  TestDF %>% nrow %>% print
  
  # TestDF %<>% mutate_at(c(Resps[r], SocialCovar), ~c(scale(.x)))
  
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
  
  IMList[[Resps[r]]] <- list()
  
  IMList[[Resps[r]]]$Model1 <- IM2
  
  SocialKept <- SocialCovar %>% c(DensityCovar) %>% intersect(IM2$Kept)
  
  if(length(SocialKept) > 0){
    
    NewDF <- expand.grid(Var1 = SocialKept,
                         Var2 = Covar) %>% 
      mutate(Var = paste0(Var1, ":", Var2))
    
    IM3 <- INLAModelAdd(Data = TestDF, 
                        Response = Resps[r], 
                        Explanatory = IM2$Kept, 
                        Add = NewDF$Var,
                        AllModels = T,
                        Base = T,
                        # Rounds = 1,
                        Clashes = ClashList,
                        Random = "Focal.ring", RandomModel = "iid",
                        AddSpatial = T)
    
    IMList[[Resps[r]]]$Model2 <- IM3
    
  } 
}

IMListF <- IMList

female <- IMListF %>% map("FinalModel") %>% 
  Efxplot(ModelNames = Resps, PointOutline = T, Intercept = F) +
  scale_colour_brewer(palette = "Spectral") +
  IMListF %>% map(c("Spatial", "Model")) %>% 
  Efxplot(ModelNames = Resps, PointOutline = T, Intercept = F) +
  scale_colour_brewer(palette = "Spectral") +
  plot_layout(guides = "collect")

IMListF %>% map(~list(.x$FinalModel, .x$Spatial$Model) %>% INLADICFig) %>% ArrangeCowplot()

IMListF %>% names %>% 
  map(~ggField(IMListF[[.x]]$Spatial$Model, IMListF[[.x]]$Spatial$Mesh) + 
        labs(fill = .x) +
        scale_fill_discrete_sequential(palette = "Mint")) %>% 
  ArrangeCowplot() + 
  ggsave("Fields.jpeg", units = "mm", width = 400, height = 300)

###MALE####

# DF <- DF_all[which(DF_all$Focal.sex == "M"),]

for(r in r:length(Resps)){
  
  print(Resps[r])
  
  if(0){
    
    TestDF <- DF_all %>% 
      filter(Focal.sex == "M") %>% 
      dplyr::select(all_of(Covar), Focal.ring, Resps[r], X, Y) %>% na.omit
    
    if(Resps[r] == "April.lay.date"){
      
      TestDF %<>% 
        filter(April.lay.date < 55)
      
    }
    
    print("Male!") 
    
    IM1 <- INLAModelAdd(Data = TestDF,
                        Response = Resps[r], 
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
    filter(Focal.sex == "M") %>% 
    dplyr::select(all_of(Covar), all_of(SocialCovar), all_of(DensityCovar), 
                  Focal.ring, Resps[r], X, Y) %>% 
    mutate_at("Year.w", as.factor) %>%
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
  
  IMList[[Resps[r]]] <- list()
  IMList[[Resps[r]]]$Model1 <- IM2
  
  SocialKept <- SocialCovar %>% c(DensityCovar) %>% intersect(IM2$Kept)
  
  if(length(SocialKept) > 0){
    
    NewDF <- expand.grid(Var1 = SocialKept,
                         Var2 = Covar) %>% 
      mutate(Var = paste0(Var1, ":", Var2))
    
    IM3 <- INLAModelAdd(Data = TestDF, 
                        Response = Resps[r], 
                        Explanatory = IM2$Kept, 
                        Add = NewDF$Var,
                        AllModels = T,
                        Base = T,
                        # Rounds = 1,
                        Clashes = ClashList,
                        Random = "Focal.ring", RandomModel = "iid",
                        AddSpatial = T)
    
    IMList[[Resps[r]]]$Model2 <- IM3
    
  } 
}

IMListM <- IMList


male <- IMListM %>% map("FinalModel") %>% 
  Efxplot(ModelNames = Resps, PointOutline = T, Intercept=F) +
  scale_colour_brewer(palette = "Spectral") +
  IMListM %>% map(c("Spatial", "Model")) %>% 
  Efxplot(ModelNames = Resps, PointOutline = T, Intercept=F) +
  scale_colour_brewer(palette = "Spectral") +
  plot_layout(guides = "collect")

IMListM %>% map(~list(.x$FinalModel, .x$Spatial$Model) %>% INLADICFig) %>% ArrangeCowplot()

IMListM %>% names %>% 
  map(~ggField(IMListM[[.x]]$Spatial$Model, IMListM[[.x]]$Spatial$Mesh) + 
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
