
# 0_Greg Script ####

library(tidyverse); library(magrittr); library(ggregplot); library(cowplot); library(colorspace)
library(GGally); library(patchwork); library(dplyr); library(beepr); library(sf)

theme_set(theme_cowplot())

DF_all <- readRDS("Data/CleanData.rds")
DF_all$Age_cat <- as.factor(DF_all$Age_cat)

WoodOutline <- st_read("woodoutlinefiles")

WoodOutline %<>% slice(1)


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
  c("AnnualDensity", "Spatial.assoc", "LifetimeDensity"),
  c(SocialCovar[c(4:6)])
)

Covar <- c("Age_num", "Age_cat", "Year.w", "Largeoaks")

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
    mutate_at(SocialCovar, ~as.numeric(as.character(.x))) %>%
    mutate_at(SocialCovar[3], ~log(.x+1)) %>% mutate_at(SocialCovar[4], ~kader:::cuberoot(.x)) %>% 
    na.omit %>% droplevels %>% 
    mutate_at("Year.w", as.factor)
  
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

female <- IMListF %>% map(c("Model1", "FinalModel")) %>% 
  Efxplot(ModelNames = Resps, PointOutline = T, Intercept = F, Size = 3) +
  scale_colour_brewer(palette = "Set1") +
  guides(color = guide_legend(reverse = T)) +
  IMListF %>% map(c("Model1", "Spatial", "Model")) %>% 
  Efxplot(ModelNames = Resps, PointOutline = T, Intercept = F, Size = 3) +
  scale_colour_brewer(palette = "Set1") +
  guides(color = guide_legend(reverse = T)) +
  plot_layout(guides = "collect")


IMListF %>% map(~list(.x$FinalModel, .x$Spatial$Model) %>% INLADICFig) %>% ArrangeCowplot()

  
  IMListF %>% names %>% 
    map(~ggField(IMListF[[.x]]$Model1$Spatial$Model, IMListF[[.x]]$Model1$Spatial$Mesh) + 
          labs(fill = .x) +
          geom_sf(data = WoodOutline, inherit.aes = F, fill = NA, colour = "black") +
          scale_fill_discrete_sequential(palette = "SunsetDark")) %>% 
    ArrangeCowplot()
  
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
  
  TestDF <- DF_all %>% 
    filter(Focal.sex == "M") %>% 
    dplyr::select(all_of(Covar), all_of(SocialCovar), all_of(DensityCovar), 
                  Focal.ring, Resps[r], X, Y) %>%
    mutate_at(SocialCovar, ~as.numeric(as.character(.x))) %>%
    mutate_at(SocialCovar[3], ~log(.x+1)) %>% mutate_at(SocialCovar[4], ~kader:::cuberoot(.x)) %>% 
    na.omit %>% droplevels %>% 
    mutate_at("Year.w", as.factor)
  
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

male <- IMListM %>% map(c("Model1", "FinalModel")) %>% 
  Efxplot(ModelNames = Resps, PointOutline = T, Intercept = F, Size = 3) +
  scale_colour_brewer(palette = "Set1") +
  guides(color = guide_legend(reverse = T)) +
  IMListM %>% map(c("Model1", "Spatial", "Model")) %>% 
  Efxplot(ModelNames = Resps, PointOutline = T, Intercept = F, Size = 3) +
  scale_colour_brewer(palette = "Set1") +
  guides(color = guide_legend(reverse = T)) +
  plot_layout(guides = "collect")

IMListM %>% 
  map("Model1") %>% 
  map(~list(.x$FinalModel, .x$Spatial$Model) %>% INLADICFig) %>% ArrangeCowplot()

IMListM %>% names %>% 
  map(~ggField(IMListM[[.x]]$Model1$Spatial$Model, IMListM[[.x]]$Model1$Spatial$Mesh) + 
        labs(fill = .x) +
        geom_sf(data = WoodOutline, inherit.aes = F, fill = NA, colour = "black") +
        scale_fill_discrete_sequential(palette = "SunsetDark")) %>% 
  ArrangeCowplot() 


####only non-social ####
Resps <- c("April.lay.date",
           "Binary.succ",
           "Clutch.size",
           "Mean.chick.weight",
           "Num.fledglings")

IMListF %>% map(c("Model1", "FinalModel")) %>% 
  Efxplot(Intercept = F, Size = 3, PointOutline = T, 
          ModelNames = Resps %>%
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings"))#,
  ) +
  scale_x_discrete(limits = rev(c("Intercept", "Age_num", "Age_catjuvenile", "Year.w2012", 
                                  "Year.w2013", "Largeoaks", "Bondstrength", "Degree", 
                                   "AnnualDensity", "Spatial.assoc", "N.avg.male.bs")[-c(1,7:11)]),
                   labels = rev(c("Intercept", "Age (numeric)", "Age (juv vs. adult)", "Year2012",  
                                  "Year2013", "Habitat quality", "Pair bond strength", "Degree", "Density",
                                  "Spatial associations", 
                                  "Male N bond strength")[-c(1,7:11)])
  ) +
  scale_color_brewer(palette="Set2") + 
  guides(color = guide_legend(reverse = T)) +
  ggtitle("Non-social effects")


####only social ####
IMListF %>% map(c("Model1", "FinalModel")) %>% 
  Efxplot(Intercept = F, Size = 3, PointOutline = T, 
          ModelNames = Resps %>%
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings"))#,
  ) +
  scale_x_discrete(limits = rev(c("Intercept", "Age_num", "Age_catjuvenile", "Year.w2012", 
                                  "Year.w2013", "Largeoaks", "Bondstrength", "Degree", 
                                  "AnnualDensity", "Spatial.assoc", "N.avg.male.bs")[-c(1:6)]),
                   labels = rev(c("Intercept", "Age (numeric)", "Age (juv vs. adult)", "Year2012",  
                                  "Year2013", "Habitat quality", "Pair bond strength", "Degree", "Density",
                                  "Spatial associations", 
                                  "Male N bond strength")[-c(1:6)])
  ) +
  scale_color_brewer(palette="Set2") + 
  guides(color = guide_legend(reverse = T)) +
  ylim(c(-.5,.5)) + 
  
  ggtitle("Female") +
  
  IMListM %>% map(c("Model1", "FinalModel")) %>% 
  Efxplot(Intercept = F, Size = 3, PointOutline = T, 
          ModelNames = Resps %>%
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings"))#,
  ) +
  scale_x_discrete(limits = rev(c("Intercept", "Age_num", "Age_catjuvenile", "Year.w2012", 
                                  "Year.w2013", "Largeoaks", "Bondstrength", "Degree", 
                                  "AnnualDensity", "Spatial.assoc", "N.avg.male.bs")[-c(1:6)]),
                   labels = rev(c("Intercept", "Age (numeric)", "Age (juv vs. adult)", "Year2012",  
                                  "Year2013", "Habitat quality", "Pair bond strength", "Degree", "Density",
                                  "Spatial associations", 
                                  "Male N bond strength")[-c(1:6)])
  ) + 
  scale_color_brewer(palette= "Set2") + 
  guides(color = guide_legend(reverse = T)) +
  ylim(c(-.5,.5)) + 
  
  ggtitle("Male") +
  
  plot_layout(guides = "collect")


####only social with SPDE####
IMListF %>% map(c("Model1", "Spatial","Model")) %>% 
  Efxplot(Intercept = F, Size = 3, PointOutline = T, 
          ModelNames = Resps %>%
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings"))#,
  ) +
  scale_x_discrete(limits = rev(c("Intercept", "Age_num", "Age_catjuvenile", "Year.w2012", 
                                  "Year.w2013", "Largeoaks", "Bondstrength", "Degree", 
                                  "AnnualDensity", "Spatial.assoc", "N.avg.male.bs")[-c(1:6)]),
                   labels = rev(c("Intercept", "Age (numeric)", "Age (juv vs. adult)", "Year2012",  
                                  "Year2013", "Habitat quality", "Pair bond strength", "Degree", "Density",
                                  "Spatial associations", 
                                  "Male N bond strength")[-c(1:6)])
  ) +
  scale_color_brewer(palette="Set2") + 
  guides(color = guide_legend(reverse = T)) +
  ylim(c(-.5,.5)) + 
  
  ggtitle("Female") +
  
  IMListM %>% map(c("Model1", "Spatial", "Model")) %>% 
  Efxplot(Intercept = F, Size = 3, PointOutline = T, 
          ModelNames = Resps %>%
            str_replace_all(c("April.lay.date" = "Lay date",
                              "Binary.succ" = "Binary success",
                              "Clutch.size" = "Clutch size",
                              "Mean.chick.weight" = "Mean chick weight",
                              "Num.fledglings" = "Number of fledglings"))#,
  ) +
  scale_x_discrete(limits = rev(c("Intercept", "Age_num", "Age_catjuvenile", "Year.w2012", 
                                  "Year.w2013", "Largeoaks", "Bondstrength", "Degree", 
                                  "AnnualDensity", "Spatial.assoc", "N.avg.male.bs")[-c(1:6)]),
                   labels = rev(c("Intercept", "Age (numeric)", "Age (juv vs. adult)", "Year2012",  
                                  "Year2013", "Habitat quality", "Pair bond strength", "Degree", "Density",
                                  "Spatial associations", 
                                  "Male N bond strength")[-c(1:6)])
  ) + 
  scale_color_brewer(palette= "Set2") + 
  guides(color = guide_legend(reverse = T)) +
  ylim(c(-.5,.5)) + 
  
  ggtitle("Male") +
  
  plot_layout(guides = "collect")


#### exporting outputs ####

##DIC change 

maleDIC <- IMListM %>% 
  map("Model1") %>% 
  map(~MDIC(list(.x$FinalModel, .x$Spatial$Model)) %>% 
        as.data.frame %>% rename(Base = 1, SPDE = 2)) %>% 
  bind_rows(.id = "Response") %>% 
  mutate(DeltaDIC = SPDE - Base)

femaleDIC <- IMListF %>% 
  map("Model1") %>% 
  map(~MDIC(list(.x$FinalModel, .x$Spatial$Model)) %>% 
        as.data.frame %>% rename(Base = 1, SPDE = 2)) %>% 
  bind_rows(.id = "Response") %>% 
  mutate(DeltaDIC = SPDE - Base)

#female####

IMListF %>% 
  map(function(a){
    
    a %>% map(function(b){
      
      b$FinalModel$summary.fixed %>% as.data.frame() %>% 
        rownames_to_column() %>% 
        rename(Variable = rowname)
      
    }) %>% bind_rows(.id = "Model1") %>% 
      select(Model1, 
             Variable,
             Estimate = mean,
             Lower = `0.025quant`,
             Upper = `0.975quant`) %>% 
      mutate_at(2:4+1, ~round(.x, 3)) %>% 
      mutate(Significant = as.numeric(Lower*Upper > 0))
    
  })

IMListF %>% 
  map(function(a){
    
    a %>% map(function(b){
      
      b$Spatial$Model$summary.fixed %>% as.data.frame() %>% 
        rownames_to_column() %>% 
        rename(Variable = rowname)
      
    }) %>% bind_rows(.id = "Sex") %>% 
      select(Sex, 
             Variable,
             Estimate = mean,
             Lower = `0.025quant`,
             Upper = `0.975quant`) %>% 
      mutate_at(2:4+1, ~round(.x, 3)) %>% 
      mutate(Significant = as.numeric(Lower*Upper > 0))
    
  })


###male#### 



IMListM %>% 
  map(function(a){
    
    a %>% map(function(b){
      
      b$FinalModel$summary.fixed %>% as.data.frame() %>% 
        rownames_to_column() %>% 
        rename(Variable = rowname)
      
    }) %>% bind_rows(.id = "Model1") %>% 
      select(Model1, 
             Variable,
             Estimate = mean,
             Lower = `0.025quant`,
             Upper = `0.975quant`) %>% 
      mutate_at(2:4+1, ~round(.x, 3)) %>% 
      mutate(Significant = as.numeric(Lower*Upper > 0))
    
  })

IMListM %>% 
  map(function(a){
    
    a %>% map(function(b){
      
      b$Spatial$Model$summary.fixed %>% as.data.frame() %>% 
        rownames_to_column() %>% 
        rename(Variable = rowname)
      
    }) %>% bind_rows(.id = "Sex") %>% 
      select(Sex, 
             Variable,
             Estimate = mean,
             Lower = `0.025quant`,
             Upper = `0.975quant`) %>% 
      mutate_at(2:4+1, ~round(.x, 3)) %>% 
      mutate(Significant = as.numeric(Lower*Upper > 0))
    
  })




#### quick number of fledglings accounting for laydate #### 
Resps <- "Num.fledglings"

Covar <- c("Age_num", "Age_cat", "Year.w", "Largeoaks", "April.lay.date", "Degree", "N.avg.male.bs")

r <- 1

IMList <- list()

Resps %<>% sort


for(r in r:length(Resps)){
  
  print(Resps[r])
  
  if(0){
    
    TestDF <- DF_all %>% 
      filter(Focal.sex == "M") %>% 
      dplyr::select(all_of(Covar), Focal.ring, April.lay.date, Resps[r], X, Y) %>% na.omit
    
    
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
  
  TestDF <- DF_all %>% 
    filter(Focal.sex == "M") %>% 
    dplyr::select(all_of(Covar), 
                  Focal.ring, Resps[r], X, Y) %>%
    na.omit %>% droplevels %>% 
    mutate_at("Year.w", as.factor)
  
  TestDF %<>% mutate_at(c(Resps[r]), ~c(scale(.x)))
  
  IM2 <- INLAModelAdd(Data = TestDF, 
                      Response = Resps[r], 
                      Explanatory = Covar, 
                      AllModels = T,
                      Base = T,
                      # Rounds = 1,
                      Random = "Focal.ring", RandomModel = "iid",
                      AddSpatial = T)
  
  # IM2$FinalModel %>% Efxplot()
  
  IMList[[Resps[r]]] <- list()
  IMList[[Resps[r]]]$Model1 <- IM2
  
}

laycontrolList <- IMList


Efxplot(laycontrolList[["Num.fledglings"]][["Model1"]][["FinalModel"]])

Resps <- "Num.fledglings"
laycontrolList[["Num.fledglings"]][["Model1"]][["FinalModel"]]%>% 
  Efxplot(Intercept = F, Size = 3, PointOutline = T, 
          ModelNames = Resps %>%
            str_replace_all(c("Num.fledglings" = "Number of fledglings"))#,
  ) +
  scale_x_discrete(limits = rev(c("Intercept", "Age_num", "Age_catjuvenile", "Year.w2012", 
                                  "Year.w2013", "Largeoaks", "April.lay.date", "Degree", 
                                   "N.avg.male.bs")[-c(1,4:5)]),
                   labels = rev(c("Intercept", "Age (numeric)", "Age (juv vs. adult)", "Year2012",  
                                  "Year2013", "Habitat quality", "Lay date", "Degree", 
                                  "Male N bond strength")[-c(1,4:5)])
  ) +
  scale_color_brewer(palette="Set2") + 
  guides(color = guide_legend(reverse = T)) +
  
  ggtitle("Controlling for lay date")



#### looking at male neighbor bond strength alone#### 
Resps <- "Num.fledglings"

Covar <- c("Age_num", "Age_cat", "Year.w", "Largeoaks", "N.avg.male.bs")

r <- 1

IMList <- list()

Resps %<>% sort


for(r in r:length(Resps)){
  
  print(Resps[r])
  
  if(0){
    
    TestDF <- DF_all %>% 
      filter(Focal.sex == "M") %>% 
      dplyr::select(all_of(Covar), Focal.ring, April.lay.date, Resps[r], X, Y) %>% na.omit
    
    
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
  
  TestDF <- DF_all %>% 
    filter(Focal.sex == "M") %>% 
    dplyr::select(all_of(Covar), 
                  Focal.ring, Resps[r], X, Y) %>%
    na.omit %>% droplevels %>% 
    mutate_at("Year.w", as.factor)
  
  TestDF %<>% mutate_at(c(Resps[r]), ~c(scale(.x)))
  
  IM2 <- INLAModelAdd(Data = TestDF, 
                      Response = Resps[r], 
                      Explanatory = Covar, 
                      AllModels = T,
                      Base = T,
                      # Rounds = 1,
                      Random = "Focal.ring", RandomModel = "iid",
                      AddSpatial = T)
  
  # IM2$FinalModel %>% Efxplot()
  
  IMList[[Resps[r]]] <- list()
  IMList[[Resps[r]]]$Model1 <- IM2
  
}

N.male.bsLIST <- IMList 


Efxplot(N.male.bsLIST[["Num.fledglings"]][["Model1"]][["FinalModel"]])

N.male.bsLIST[["Num.fledglings"]][["Model1"]][["FinalModel"]]%>% 
  Efxplot(Intercept = F, Size = 3, PointOutline = T, 
          ModelNames = Resps %>%
            str_replace_all(c("Num.fledglings" = "Number of fledglings"))#,
  ) +
  scale_x_discrete(limits = rev(c("Intercept", "Age_num", "Age_catjuvenile", "Year.w2012", 
                                  "Year.w2013", "Largeoaks", 
                                  "N.avg.male.bs")[-c(1,4:5)]),
                   labels = rev(c("Intercept", "Age (numeric)", "Age (juv vs. adult)", "Year2012",  
                                  "Year2013", "Habitat quality", 
                                  "Male N bond strength")[-c(1,4:5)])
  ) +
  scale_color_brewer(palette="Set2") + 
  guides(color = guide_legend(reverse = T)) +
  
  ggtitle("Male neighbor strength only")











# Sociality as a response ####

SpocialList <- list()

# Resps %<>% sort

r <- 1

TestDF <- DF_all %>% 
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

SpocialList %>% map("FinalModel") %>% Efxplot(ModelNames = SocialCovar) +
  SpocialList %>% map(c("Spatial", "Model")) %>% Efxplot(ModelNames = SocialCovar) +
  plot_layout(guides = "collect")


###with previous year familiarity #### 
DF.fam <- readRDS("Data/fn2.data.withfam.Rds")
DF.fam$Age_cat <- as.factor(DF.fam$Age_cat)


Resps <- c("April.lay.date",
           "Binary.succ",
           "Clutch.size",
           "Mean.chick.weight",
           "Num.fledglings")

SocialCovar <- c("Strength_mean", "Degree", #social 
                 "Bondstrength", #pair bond
                 "N.num.ind.familiar", "N.num.FEMALEind.familiar", "N.num.MALEind.familiar", #neighbors
                 "Spatial.assoc") #spatial 

DensityCovar <- c("LifetimeDensity", "AnnualDensity")

# DF %>% mutate_at(SocialCovar, ~as.numeric(as.character(.x))) %>% 
# dplyr::select(all_of(SocialCovar)) %>% ggpairs()

# DF %>% mutate_at(Resps, ~as.numeric(as.character(.x))) %>%
#   dplyr::select(all_of(Resps)) %>%
#   ggpairs()

ClashList <- list(
  c("Strength_mean", "Degree"),
  c("AnnualDensity", "Spatial.assoc", "LifetimeDensity"),
  c(SocialCovar[c(4:6)])
)

Covar <- c("Age_num", "Age_cat", "Year.w", "Largeoaks")

r <- 1

IMList <- list()

Resps %<>% sort

####FEMALE####

# DF <- DF_all[which(DF_all$Focal.sex == "F"),]

for(r in r:length(Resps)){
  
  print(Resps[r])
  
  if(0){
    
    TestDF <- DF.fam %>% 
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
  
  TestDF <- DF.fam %>% 
    filter(Focal.sex == "F") %>% 
    dplyr::select(all_of(Covar), all_of(SocialCovar), all_of(DensityCovar), 
                  Focal.ring, Resps[r], X, Y) %>% 
    mutate_at(SocialCovar, ~as.numeric(as.character(.x))) %>%
    mutate_at(SocialCovar[3], ~log(.x+1)) %>% mutate_at(SocialCovar[4], ~kader:::cuberoot(.x)) %>% 
    na.omit %>% droplevels %>% 
    mutate_at("Year.w", as.factor)
  
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

IMListF.fam <- IMList

female <- IMListF.fam %>% map(c("Model1", "FinalModel")) %>% 
  Efxplot(ModelNames = Resps, PointOutline = T, Intercept = F, Size = 3) +
  scale_colour_brewer(palette = "Set1") +
  guides(color = guide_legend(reverse = T)) +
  IMListF.fam %>% map(c("Model1", "Spatial", "Model")) %>% 
  Efxplot(ModelNames = Resps, PointOutline = T, Intercept = F, Size = 3) +
  scale_colour_brewer(palette = "Set1") +
  guides(color = guide_legend(reverse = T)) +
  plot_layout(guides = "collect")

###MALE####

# DF <- DF_all[which(DF_all$Focal.sex == "M"),]

for(r in r:length(Resps)){
  
  print(Resps[r])
  
  if(0){
    
    TestDF <- DF.fam %>% 
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
  
  TestDF <- DF.fam %>% 
    filter(Focal.sex == "M") %>% 
    dplyr::select(all_of(Covar), all_of(SocialCovar), all_of(DensityCovar), 
                  Focal.ring, Resps[r], X, Y) %>%
    mutate_at(SocialCovar, ~as.numeric(as.character(.x))) %>%
    mutate_at(SocialCovar[3], ~log(.x+1)) %>% mutate_at(SocialCovar[4], ~kader:::cuberoot(.x)) %>% 
    na.omit %>% droplevels %>% 
    mutate_at("Year.w", as.factor)
  
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

IMListM.fam <- IMList
