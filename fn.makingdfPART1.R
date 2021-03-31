setwd("~/Documents/2/Familiar_neighbors/DATA")
library(sf)

#load in data
raw.breeding.data <- read.csv("BREEDINGDATA.csv")
nestbox.data <- read.csv("Nestboxes.csv")
wood.outline <- sf::st_read("perimeter poly with clearings_region.shp")
wood.outline <- wood.outline[1,] #keep first polygon

#add box locations to breeding data
box.locations <- nestbox.data[,c(2,3,4)]
breeding.data <- raw.breeding.data[which(raw.breeding.data$year > 1964),] 

breeding.data$Pnum <- as.character(breeding.data$Pnum)
breeding.data$temp <- gsub("^.{0,4}", "", breeding.data$Pnum)  
breeding.data$attempt <- substr(breeding.data$"temp",1,1) #label attempt number
breeding.data <- breeding.data[which(breeding.data$attempt==1),] #remove the ones that are second attempts
breeding.data$Box <- gsub("^.{1,1}", "", breeding.data$temp) #get box number in right format 
breeding.data <- dplyr::left_join(breeding.data, box.locations, by="Box") #add box locations 

#get a base with great tits only to work with 
xdata <- breeding.data[which(breeding.data$Species=="g"),]
  
  
#remove records where a parent is not identified 
xdata$nest.id.type <- "NA" #make new column 

#fill in empty spaces with NA
xdata$Mother <- as.character(xdata$Mother)
xdata$mID <- with(xdata, ifelse(Mother=="", NA, 
                                         xdata$Mother)) #label one ID
xdata$mID <- !is.na(xdata$mID)

xdata$Father <- as.character(xdata$Father)
xdata$fID <- with(xdata, ifelse(Father=="", NA, 
                                xdata$Father)) #label one ID
xdata$fID <- !is.na(xdata$fID)

summary(as.factor(xdata$mID))

xdata$nest.id.type <- "neither"

xdata$nest.id.type <- with(xdata, ifelse(mID == TRUE & fID == FALSE, "one", 
                                     xdata$nest.id.type)) #label one ID

xdata$nest.id.type <- with(xdata, ifelse(mID == FALSE & fID == TRUE, "one", 
                                         xdata$nest.id.type)) #label one ID

xdata$nest.id.type <- with(xdata, ifelse(mID == TRUE & fID == TRUE, "both", 
                                         xdata$nest.id.type)) #label both ID

xdata$fID <- NULL
xdata$mID <- NULL

#remove neithers and ones 
xdata <- xdata[which(xdata$nest.id.type == "both"),] 

#make a row for the mom and the dad of each nest 
x <- data.table::setDT(xdata)[, .(parent = c("mother","father")),
                              .(year, Box, x, y, Section, 
                                Species, April.lay.date, Laying.rate, Incubation.started, 
                                April.hatch.date, Incubation.duration, Total.egg.weight, Num.eggs.weighed,
                                Clutch.size, Num.chicks, Num.dead.chicks, Num.fledglings, Mean.chick.weight,
                                Father, Mother)]

x <-data.frame(x,"box.year.parentid"=paste(x$Box, x$year, x$parent,sep="_")) 

xdata <- x 

rm(x)

#get the focal ring 
xdata$focal.ring <- NA

xdata$focal.ring <- with(xdata, ifelse(parent == "mother", xdata$Mother, 
                                         xdata$focal.ring)) #label one ID

xdata$focal.ring <- with(xdata, ifelse(parent == "father", xdata$Father, 
                                       xdata$focal.ring)) #label one ID


#add sex

xdata$focal.sex <- NA

xdata$focal.sex <- with(xdata, ifelse(parent == "mother", "F", 
                                   xdata$focal.sex)) 
xdata$focal.sex <- with(xdata, ifelse(parent == "father", "M", 
                                      xdata$focal.sex)) 

#add binary success fitness variable 
xdata$Binary.succ <- xdata$Num.fledglings
xdata$Binary.succ <- with(xdata, ifelse(Binary.succ == 0, "0", 
                                        xdata$Binary.succ)) 
xdata$Binary.succ <- with(xdata, ifelse(Binary.succ > 0, "1", 
                                        xdata$Binary.succ)) 

xdata$Binary.succ <- as.numeric(xdata$Binary.succ)

base.fn.data <- xdata


###get a version of the breeding data for finding neighbors 
zdata <- raw.breeding.data

#make breeding data to use to get neighbors 
zdata$Pnum <- as.character(zdata$Pnum)
zdata$temp <- gsub("^.{0,4}", "", zdata$Pnum)  
zdata$attempt <- substr(zdata$"temp",1,1) #label attempt number
zdata <- zdata[which(zdata$attempt==1),] #remove the ones that are second attempts
zdata$Box <- gsub("^.{1,1}", "", zdata$temp) #get box number in right format 
zdata <- dplyr::left_join(zdata, box.locations, by="Box") #add box locations 

#get a base with great tits only to work with 
zdata <- zdata[which(zdata$Species=="g"),]

breeding.data.neighbors <- zdata


####get neighbors#### 

#for 1964, only for familiarity info
xdata <- breeding.data.neighbors[which(breeding.data.neighbors$Species=="g"),]

#1964
breeding.data.1964 <- xdata[which(xdata$year == 1964),] 
breeding.data.1964 <- breeding.data.1964[!is.na(breeding.data.1964$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1964 <- sf::st_as_sf(breeding.data.1964, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons

bbox_polygon <- function(x) {
  bb <- sf::st_bbox(x)
  
  p <- matrix(
    c(bb["xmin"], bb["ymin"], 
      bb["xmin"], bb["ymax"],
      bb["xmax"], bb["ymax"], 
      bb["xmax"], bb["ymin"], 
      bb["xmin"], bb["ymin"]),
    ncol = 2, byrow = T
  )
  
  sf::st_polygon(list(p))
}

box <- sf::st_sfc(bbox_polygon(breeding.data.1964))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1964), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1964))
breeding.ids.1964 <- breeding.data.1964[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1964)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1964 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1964$Focal.male <- with(neighbors.1964, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1964$Focal.female <- with(neighbors.1964, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1964$Neighboring.male <- with(neighbors.1964, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1964$Neighboring.female <- with(neighbors.1964, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1964.b <- as.data.frame(with(neighbors.1964, paste(Focal.male, Neighboring.male, sep="_")))
names(N1964.b)[1] <- "ring_ring"
N1964.c <- as.data.frame(with(neighbors.1964, paste(Focal.male, Neighboring.female, sep="_")))
names(N1964.c)[1] <- "ring_ring"
N1964.d <- as.data.frame(with(neighbors.1964, paste(Focal.female, Neighboring.female, sep="_")))
names(N1964.d)[1] <- "ring_ring"
N1964.e <- as.data.frame(with(neighbors.1964, paste(Focal.female, Neighboring.male, sep="_")))
names(N1964.e)[1] <- "ring_ring"

N1964 <- rbind(N1964.b, N1964.c, N1964.d, N1964.e)
rm(N1964.b, N1964.c, N1964.d, N1964.e)
N1964$neighbors <- TRUE
N1964$Year.s <- 1965

N1964 <- N1964[!grepl("UNKNOWN", N1964$ring_ring),]

N_reference <- N1964



#one year at a time, for now..
xdata <- breeding.data.neighbors[which(breeding.data.neighbors$Species=="g"),]

#1965 ####
breeding.data.1965 <- xdata[which(xdata$year == 1965),] 
breeding.data.1965 <- breeding.data.1965[!is.na(breeding.data.1965$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1965 <- sf::st_as_sf(breeding.data.1965, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1965))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1965), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1965))
breeding.ids.1965 <- breeding.data.1965[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1965)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                            territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                            territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                            territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                            territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1965 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1965$Focal.male <- with(neighbors.1965, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1965$Focal.female <- with(neighbors.1965, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1965$Neighboring.male <- with(neighbors.1965, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1965$Neighboring.female <- with(neighbors.1965, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1965.b <- as.data.frame(with(neighbors.1965, paste(Focal.male, Neighboring.male, sep="_")))
names(N1965.b)[1] <- "ring_ring"
N1965.c <- as.data.frame(with(neighbors.1965, paste(Focal.male, Neighboring.female, sep="_")))
names(N1965.c)[1] <- "ring_ring"
N1965.d <- as.data.frame(with(neighbors.1965, paste(Focal.female, Neighboring.female, sep="_")))
names(N1965.d)[1] <- "ring_ring"
N1965.e <- as.data.frame(with(neighbors.1965, paste(Focal.female, Neighboring.male, sep="_")))
names(N1965.e)[1] <- "ring_ring"

N1965 <- rbind(N1965.b, N1965.c, N1965.d, N1965.e)
rm(N1965.b, N1965.c, N1965.d, N1965.e)
N1965$neighbors <- TRUE
N1965$Year.s <- 1966

N1965 <- N1965[!grepl("UNKNOWN", N1965$ring_ring),]

N_reference <- rbind(N_reference, N1965)


ydata <- base.fn.data[which(base.fn.data$year==1965),]


library(dplyr)

nei1965 <- neighbors.1965[,c(1,4)]

nei1965 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1965


nei1965 <- tidyr::pivot_wider(nei1965, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1965) <- paste0('N', colnames(nei1965))
names(nei1965)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1965
as.data.frame(colnames(ydata))
par1965 <- ydata[,c(2,19,20)]
par1965 <- distinct(par1965, Box, .keep_all = TRUE)


####add neighbor ids
####1965
#N1
names(par1965)[names(par1965) == "Box"] <- "N1"
names(par1965)[names(par1965) == "Mother"] <- "N1.mother"
names(par1965)[names(par1965) == "Father"] <- "N1.father"
nei1965 <- merge(nei1965, par1965, by="N1", all.x=TRUE)
#N2
names(par1965)[names(par1965) == "N1"] <- "N2"
names(par1965)[names(par1965) == "N1.mother"] <- "N2.mother"
names(par1965)[names(par1965) == "N1.father"] <- "N2.father"
nei1965 <- merge(nei1965, par1965, by="N2", all.x=TRUE)
#N3
names(par1965)[names(par1965) == "N2"] <- "N3"
names(par1965)[names(par1965) == "N2.mother"] <- "N3.mother"
names(par1965)[names(par1965) == "N2.father"] <- "N3.father"
nei1965 <- merge(nei1965, par1965, by="N3", all.x=TRUE)
#N4
names(par1965)[names(par1965) == "N3"] <- "N4"
names(par1965)[names(par1965) == "N3.mother"] <- "N4.mother"
names(par1965)[names(par1965) == "N3.father"] <- "N4.father"
nei1965 <- merge(nei1965, par1965, by="N4", all.x=TRUE)
#N5
names(par1965)[names(par1965) == "N4"] <- "N5"
names(par1965)[names(par1965) == "N4.mother"] <- "N5.mother"
names(par1965)[names(par1965) == "N4.father"] <- "N5.father"
nei1965 <- merge(nei1965, par1965, by="N5", all.x=TRUE)
#N6
names(par1965)[names(par1965) == "N5"] <- "N6"
names(par1965)[names(par1965) == "N5.mother"] <- "N6.mother"
names(par1965)[names(par1965) == "N5.father"] <- "N6.father"
nei1965 <- merge(nei1965, par1965, by="N6", all.x=TRUE)
#N7
names(par1965)[names(par1965) == "N6"] <- "N7"
names(par1965)[names(par1965) == "N6.mother"] <- "N7.mother"
names(par1965)[names(par1965) == "N6.father"] <- "N7.father"
nei1965 <- merge(nei1965, par1965, by="N7", all.x=TRUE)
#N8
names(par1965)[names(par1965) == "N7"] <- "N8"
names(par1965)[names(par1965) == "N7.mother"] <- "N8.mother"
names(par1965)[names(par1965) == "N7.father"] <- "N8.father"
nei1965 <- merge(nei1965, par1965, by="N8", all.x=TRUE)
#N9
names(par1965)[names(par1965) == "N8"] <- "N9"
names(par1965)[names(par1965) == "N8.mother"] <- "N9.mother"
names(par1965)[names(par1965) == "N8.father"] <- "N9.father"
nei1965 <- merge(nei1965, par1965, by="N9", all.x=TRUE)
#N10
names(par1965)[names(par1965) == "N9"] <- "N10"
names(par1965)[names(par1965) == "N9.mother"] <- "N10.mother"
names(par1965)[names(par1965) == "N9.father"] <- "N10.father"
nei1965 <- merge(nei1965, par1965, by="N10", all.x=TRUE)

#and identifying column 
nei1965$year <- 1965
nei1965a  <-data.frame(nei1965,"box.year.parentid"=paste(nei1965$Focal.box, nei1965$year, "mother",sep="_")) 
nei1965b  <-data.frame(nei1965,"box.year.parentid"=paste(nei1965$Focal.box, nei1965$year, "father",sep="_")) 

nei1965 <- rbind(nei1965a, nei1965b)
rm(nei1965a, nei1965b)

nei1965$N9 <- NA 
nei1965$N10 <- NA

nei1965$N9.mother <- NA
nei1965$N9.father <- NA
nei1965$N10.mother <- NA
nei1965$N10.father <- NA

nei1965 <- nei1965[,order(colnames(nei1965))]

nei_output <- nei1965

#1966 ####
breeding.data.1966 <- xdata[which(xdata$year == 1966),] 
breeding.data.1966 <- breeding.data.1966[!is.na(breeding.data.1966$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1966 <- sf::st_as_sf(breeding.data.1966, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1966))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1966), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1966))
breeding.ids.1966 <- breeding.data.1966[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1966)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1966 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1966$Focal.male <- with(neighbors.1966, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1966$Focal.female <- with(neighbors.1966, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1966$Neighboring.male <- with(neighbors.1966, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1966$Neighboring.female <- with(neighbors.1966, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1966.b <- as.data.frame(with(neighbors.1966, paste(Focal.male, Neighboring.male, sep="_")))
names(N1966.b)[1] <- "ring_ring"
N1966.c <- as.data.frame(with(neighbors.1966, paste(Focal.male, Neighboring.female, sep="_")))
names(N1966.c)[1] <- "ring_ring"
N1966.d <- as.data.frame(with(neighbors.1966, paste(Focal.female, Neighboring.female, sep="_")))
names(N1966.d)[1] <- "ring_ring"
N1966.e <- as.data.frame(with(neighbors.1966, paste(Focal.female, Neighboring.male, sep="_")))
names(N1966.e)[1] <- "ring_ring"

N1966 <- rbind(N1966.b, N1966.c, N1966.d, N1966.e)
rm(N1966.b, N1966.c, N1966.d, N1966.e)
N1966$neighbors <- TRUE
N1966$Year.s <- 1967

N1966 <- N1966[!grepl("UNKNOWN", N1966$ring_ring),]

N_reference <- rbind(N_reference, N1966)


ydata <- base.fn.data[which(base.fn.data$year==1966),]


library(dplyr)

nei1966 <- neighbors.1966[,c(1,4)]

nei1966 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1966


nei1966 <- tidyr::pivot_wider(nei1966, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1966) <- paste0('N', colnames(nei1966))
names(nei1966)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1966
as.data.frame(colnames(ydata))
par1966 <- ydata[,c(2,19,20)]
par1966 <- distinct(par1966, Box, .keep_all = TRUE)


####add neighbor ids
####1966
#N1
names(par1966)[names(par1966) == "Box"] <- "N1"
names(par1966)[names(par1966) == "Mother"] <- "N1.mother"
names(par1966)[names(par1966) == "Father"] <- "N1.father"
nei1966 <- merge(nei1966, par1966, by="N1", all.x=TRUE)
#N2
names(par1966)[names(par1966) == "N1"] <- "N2"
names(par1966)[names(par1966) == "N1.mother"] <- "N2.mother"
names(par1966)[names(par1966) == "N1.father"] <- "N2.father"
nei1966 <- merge(nei1966, par1966, by="N2", all.x=TRUE)
#N3
names(par1966)[names(par1966) == "N2"] <- "N3"
names(par1966)[names(par1966) == "N2.mother"] <- "N3.mother"
names(par1966)[names(par1966) == "N2.father"] <- "N3.father"
nei1966 <- merge(nei1966, par1966, by="N3", all.x=TRUE)
#N4
names(par1966)[names(par1966) == "N3"] <- "N4"
names(par1966)[names(par1966) == "N3.mother"] <- "N4.mother"
names(par1966)[names(par1966) == "N3.father"] <- "N4.father"
nei1966 <- merge(nei1966, par1966, by="N4", all.x=TRUE)
#N5
names(par1966)[names(par1966) == "N4"] <- "N5"
names(par1966)[names(par1966) == "N4.mother"] <- "N5.mother"
names(par1966)[names(par1966) == "N4.father"] <- "N5.father"
nei1966 <- merge(nei1966, par1966, by="N5", all.x=TRUE)
#N6
names(par1966)[names(par1966) == "N5"] <- "N6"
names(par1966)[names(par1966) == "N5.mother"] <- "N6.mother"
names(par1966)[names(par1966) == "N5.father"] <- "N6.father"
nei1966 <- merge(nei1966, par1966, by="N6", all.x=TRUE)
#N7
names(par1966)[names(par1966) == "N6"] <- "N7"
names(par1966)[names(par1966) == "N6.mother"] <- "N7.mother"
names(par1966)[names(par1966) == "N6.father"] <- "N7.father"
nei1966 <- merge(nei1966, par1966, by="N7", all.x=TRUE)
#N8
names(par1966)[names(par1966) == "N7"] <- "N8"
names(par1966)[names(par1966) == "N7.mother"] <- "N8.mother"
names(par1966)[names(par1966) == "N7.father"] <- "N8.father"
nei1966 <- merge(nei1966, par1966, by="N8", all.x=TRUE)
#N9
names(par1966)[names(par1966) == "N8"] <- "N9"
names(par1966)[names(par1966) == "N8.mother"] <- "N9.mother"
names(par1966)[names(par1966) == "N8.father"] <- "N9.father"
nei1966 <- merge(nei1966, par1966, by="N9", all.x=TRUE)
#N10
names(par1966)[names(par1966) == "N9"] <- "N10"
names(par1966)[names(par1966) == "N9.mother"] <- "N10.mother"
names(par1966)[names(par1966) == "N9.father"] <- "N10.father"
nei1966 <- merge(nei1966, par1966, by="N10", all.x=TRUE)

#and identifying column 
nei1966$year <- 1966
nei1966a  <-data.frame(nei1966,"box.year.parentid"=paste(nei1966$Focal.box, nei1966$year, "mother",sep="_")) 
nei1966b  <-data.frame(nei1966,"box.year.parentid"=paste(nei1966$Focal.box, nei1966$year, "father",sep="_")) 

nei1966 <- rbind(nei1966a, nei1966b)
rm(nei1966a, nei1966b)

nei1966$N9 <- NA 
nei1966$N10 <- NA

nei1966$N9.mother <- NA
nei1966$N9.father <- NA
nei1966$N10.mother <- NA
nei1966$N10.father <- NA

nei1966 <- nei1966[,order(colnames(nei1966))]

nei_output <- rbind(nei_output, nei1966)

#1967 ####
breeding.data.1967 <- xdata[which(xdata$year == 1967),] 
breeding.data.1967 <- breeding.data.1967[!is.na(breeding.data.1967$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1967 <- sf::st_as_sf(breeding.data.1967, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1967))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1967), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1967))
breeding.ids.1967 <- breeding.data.1967[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1967)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1967 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1967$Focal.male <- with(neighbors.1967, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1967$Focal.female <- with(neighbors.1967, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1967$Neighboring.male <- with(neighbors.1967, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1967$Neighboring.female <- with(neighbors.1967, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1967.b <- as.data.frame(with(neighbors.1967, paste(Focal.male, Neighboring.male, sep="_")))
names(N1967.b)[1] <- "ring_ring"
N1967.c <- as.data.frame(with(neighbors.1967, paste(Focal.male, Neighboring.female, sep="_")))
names(N1967.c)[1] <- "ring_ring"
N1967.d <- as.data.frame(with(neighbors.1967, paste(Focal.female, Neighboring.female, sep="_")))
names(N1967.d)[1] <- "ring_ring"
N1967.e <- as.data.frame(with(neighbors.1967, paste(Focal.female, Neighboring.male, sep="_")))
names(N1967.e)[1] <- "ring_ring"

N1967 <- rbind(N1967.b, N1967.c, N1967.d, N1967.e)
rm(N1967.b, N1967.c, N1967.d, N1967.e)
N1967$neighbors <- TRUE
N1967$Year.s <- 1968

N1967 <- N1967[!grepl("UNKNOWN", N1967$ring_ring),]

N_reference <- rbind(N_reference, N1967)


ydata <- base.fn.data[which(base.fn.data$year==1967),]


library(dplyr)

nei1967 <- neighbors.1967[,c(1,4)]

nei1967 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1967


nei1967 <- tidyr::pivot_wider(nei1967, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1967) <- paste0('N', colnames(nei1967))
names(nei1967)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1967
as.data.frame(colnames(ydata))
par1967 <- ydata[,c(2,19,20)]
par1967 <- distinct(par1967, Box, .keep_all = TRUE)


####add neighbor ids
####1967
#N1
names(par1967)[names(par1967) == "Box"] <- "N1"
names(par1967)[names(par1967) == "Mother"] <- "N1.mother"
names(par1967)[names(par1967) == "Father"] <- "N1.father"
nei1967 <- merge(nei1967, par1967, by="N1", all.x=TRUE)
#N2
names(par1967)[names(par1967) == "N1"] <- "N2"
names(par1967)[names(par1967) == "N1.mother"] <- "N2.mother"
names(par1967)[names(par1967) == "N1.father"] <- "N2.father"
nei1967 <- merge(nei1967, par1967, by="N2", all.x=TRUE)
#N3
names(par1967)[names(par1967) == "N2"] <- "N3"
names(par1967)[names(par1967) == "N2.mother"] <- "N3.mother"
names(par1967)[names(par1967) == "N2.father"] <- "N3.father"
nei1967 <- merge(nei1967, par1967, by="N3", all.x=TRUE)
#N4
names(par1967)[names(par1967) == "N3"] <- "N4"
names(par1967)[names(par1967) == "N3.mother"] <- "N4.mother"
names(par1967)[names(par1967) == "N3.father"] <- "N4.father"
nei1967 <- merge(nei1967, par1967, by="N4", all.x=TRUE)
#N5
names(par1967)[names(par1967) == "N4"] <- "N5"
names(par1967)[names(par1967) == "N4.mother"] <- "N5.mother"
names(par1967)[names(par1967) == "N4.father"] <- "N5.father"
nei1967 <- merge(nei1967, par1967, by="N5", all.x=TRUE)
#N6
names(par1967)[names(par1967) == "N5"] <- "N6"
names(par1967)[names(par1967) == "N5.mother"] <- "N6.mother"
names(par1967)[names(par1967) == "N5.father"] <- "N6.father"
nei1967 <- merge(nei1967, par1967, by="N6", all.x=TRUE)
#N7
names(par1967)[names(par1967) == "N6"] <- "N7"
names(par1967)[names(par1967) == "N6.mother"] <- "N7.mother"
names(par1967)[names(par1967) == "N6.father"] <- "N7.father"
nei1967 <- merge(nei1967, par1967, by="N7", all.x=TRUE)
#N8
names(par1967)[names(par1967) == "N7"] <- "N8"
names(par1967)[names(par1967) == "N7.mother"] <- "N8.mother"
names(par1967)[names(par1967) == "N7.father"] <- "N8.father"
nei1967 <- merge(nei1967, par1967, by="N8", all.x=TRUE)
#N9
names(par1967)[names(par1967) == "N8"] <- "N9"
names(par1967)[names(par1967) == "N8.mother"] <- "N9.mother"
names(par1967)[names(par1967) == "N8.father"] <- "N9.father"
nei1967 <- merge(nei1967, par1967, by="N9", all.x=TRUE)
#N10
names(par1967)[names(par1967) == "N9"] <- "N10"
names(par1967)[names(par1967) == "N9.mother"] <- "N10.mother"
names(par1967)[names(par1967) == "N9.father"] <- "N10.father"
nei1967 <- merge(nei1967, par1967, by="N10", all.x=TRUE)

#and identifying column 
nei1967$year <- 1967
nei1967a  <-data.frame(nei1967,"box.year.parentid"=paste(nei1967$Focal.box, nei1967$year, "mother",sep="_")) 
nei1967b  <-data.frame(nei1967,"box.year.parentid"=paste(nei1967$Focal.box, nei1967$year, "father",sep="_")) 

nei1967 <- rbind(nei1967a, nei1967b)
rm(nei1967a, nei1967b)

nei1967 <- nei1967[,order(colnames(nei1967))]

nei_output <- rbind(nei_output, nei1967)


#1968 ####
breeding.data.1968 <- xdata[which(xdata$year == 1968),] 
breeding.data.1968 <- breeding.data.1968[!is.na(breeding.data.1968$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1968 <- sf::st_as_sf(breeding.data.1968, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1968))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1968), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1968))
breeding.ids.1968 <- breeding.data.1968[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1968)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1968 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1968$Focal.male <- with(neighbors.1968, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1968$Focal.female <- with(neighbors.1968, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1968$Neighboring.male <- with(neighbors.1968, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1968$Neighboring.female <- with(neighbors.1968, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1968.b <- as.data.frame(with(neighbors.1968, paste(Focal.male, Neighboring.male, sep="_")))
names(N1968.b)[1] <- "ring_ring"
N1968.c <- as.data.frame(with(neighbors.1968, paste(Focal.male, Neighboring.female, sep="_")))
names(N1968.c)[1] <- "ring_ring"
N1968.d <- as.data.frame(with(neighbors.1968, paste(Focal.female, Neighboring.female, sep="_")))
names(N1968.d)[1] <- "ring_ring"
N1968.e <- as.data.frame(with(neighbors.1968, paste(Focal.female, Neighboring.male, sep="_")))
names(N1968.e)[1] <- "ring_ring"

N1968 <- rbind(N1968.b, N1968.c, N1968.d, N1968.e)
rm(N1968.b, N1968.c, N1968.d, N1968.e)
N1968$neighbors <- TRUE
N1968$Year.s <- 1969

N1968 <- N1968[!grepl("UNKNOWN", N1968$ring_ring),]

N_reference <- rbind(N_reference, N1968)


ydata <- base.fn.data[which(base.fn.data$year==1968),]


library(dplyr)

nei1968 <- neighbors.1968[,c(1,4)]

nei1968 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1968


nei1968 <- tidyr::pivot_wider(nei1968, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1968) <- paste0('N', colnames(nei1968))
names(nei1968)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1968
as.data.frame(colnames(ydata))
par1968 <- ydata[,c(2,19,20)]
par1968 <- distinct(par1968, Box, .keep_all = TRUE)


####add neighbor ids
####1968
#N1
names(par1968)[names(par1968) == "Box"] <- "N1"
names(par1968)[names(par1968) == "Mother"] <- "N1.mother"
names(par1968)[names(par1968) == "Father"] <- "N1.father"
nei1968 <- merge(nei1968, par1968, by="N1", all.x=TRUE)
#N2
names(par1968)[names(par1968) == "N1"] <- "N2"
names(par1968)[names(par1968) == "N1.mother"] <- "N2.mother"
names(par1968)[names(par1968) == "N1.father"] <- "N2.father"
nei1968 <- merge(nei1968, par1968, by="N2", all.x=TRUE)
#N3
names(par1968)[names(par1968) == "N2"] <- "N3"
names(par1968)[names(par1968) == "N2.mother"] <- "N3.mother"
names(par1968)[names(par1968) == "N2.father"] <- "N3.father"
nei1968 <- merge(nei1968, par1968, by="N3", all.x=TRUE)
#N4
names(par1968)[names(par1968) == "N3"] <- "N4"
names(par1968)[names(par1968) == "N3.mother"] <- "N4.mother"
names(par1968)[names(par1968) == "N3.father"] <- "N4.father"
nei1968 <- merge(nei1968, par1968, by="N4", all.x=TRUE)
#N5
names(par1968)[names(par1968) == "N4"] <- "N5"
names(par1968)[names(par1968) == "N4.mother"] <- "N5.mother"
names(par1968)[names(par1968) == "N4.father"] <- "N5.father"
nei1968 <- merge(nei1968, par1968, by="N5", all.x=TRUE)
#N6
names(par1968)[names(par1968) == "N5"] <- "N6"
names(par1968)[names(par1968) == "N5.mother"] <- "N6.mother"
names(par1968)[names(par1968) == "N5.father"] <- "N6.father"
nei1968 <- merge(nei1968, par1968, by="N6", all.x=TRUE)
#N7
names(par1968)[names(par1968) == "N6"] <- "N7"
names(par1968)[names(par1968) == "N6.mother"] <- "N7.mother"
names(par1968)[names(par1968) == "N6.father"] <- "N7.father"
nei1968 <- merge(nei1968, par1968, by="N7", all.x=TRUE)
#N8
names(par1968)[names(par1968) == "N7"] <- "N8"
names(par1968)[names(par1968) == "N7.mother"] <- "N8.mother"
names(par1968)[names(par1968) == "N7.father"] <- "N8.father"
nei1968 <- merge(nei1968, par1968, by="N8", all.x=TRUE)
#N9
names(par1968)[names(par1968) == "N8"] <- "N9"
names(par1968)[names(par1968) == "N8.mother"] <- "N9.mother"
names(par1968)[names(par1968) == "N8.father"] <- "N9.father"
nei1968 <- merge(nei1968, par1968, by="N9", all.x=TRUE)
#N10
names(par1968)[names(par1968) == "N9"] <- "N10"
names(par1968)[names(par1968) == "N9.mother"] <- "N10.mother"
names(par1968)[names(par1968) == "N9.father"] <- "N10.father"
nei1968 <- merge(nei1968, par1968, by="N10", all.x=TRUE)

#and identifying column 
nei1968$year <- 1968
nei1968a  <-data.frame(nei1968,"box.year.parentid"=paste(nei1968$Focal.box, nei1968$year, "mother",sep="_")) 
nei1968b  <-data.frame(nei1968,"box.year.parentid"=paste(nei1968$Focal.box, nei1968$year, "father",sep="_")) 

nei1968 <- rbind(nei1968a, nei1968b)
rm(nei1968a, nei1968b)

nei1968$N8 <- NA 
nei1968$N9 <- NA 
nei1968$N10 <- NA

nei1968$N8.mother <- NA 
nei1968$N8.father <- NA 
nei1968$N9.mother <- NA
nei1968$N9.father <- NA
nei1968$N10.mother <- NA
nei1968$N10.father <- NA

nei1968 <- nei1968[,order(colnames(nei1968))]

nei_output <- rbind(nei_output, nei1968)

#1969 ####
breeding.data.1969 <- xdata[which(xdata$year == 1969),] 
breeding.data.1969 <- breeding.data.1969[!is.na(breeding.data.1969$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1969 <- sf::st_as_sf(breeding.data.1969, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1969))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1969), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1969))
breeding.ids.1969 <- breeding.data.1969[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1969)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1969 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1969$Focal.male <- with(neighbors.1969, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1969$Focal.female <- with(neighbors.1969, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1969$Neighboring.male <- with(neighbors.1969, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1969$Neighboring.female <- with(neighbors.1969, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1969.b <- as.data.frame(with(neighbors.1969, paste(Focal.male, Neighboring.male, sep="_")))
names(N1969.b)[1] <- "ring_ring"
N1969.c <- as.data.frame(with(neighbors.1969, paste(Focal.male, Neighboring.female, sep="_")))
names(N1969.c)[1] <- "ring_ring"
N1969.d <- as.data.frame(with(neighbors.1969, paste(Focal.female, Neighboring.female, sep="_")))
names(N1969.d)[1] <- "ring_ring"
N1969.e <- as.data.frame(with(neighbors.1969, paste(Focal.female, Neighboring.male, sep="_")))
names(N1969.e)[1] <- "ring_ring"

N1969 <- rbind(N1969.b, N1969.c, N1969.d, N1969.e)
rm(N1969.b, N1969.c, N1969.d, N1969.e)
N1969$neighbors <- TRUE
N1969$Year.s <- 1970

N1969 <- N1969[!grepl("UNKNOWN", N1969$ring_ring),]

N_reference <- rbind(N_reference, N1969)


ydata <- base.fn.data[which(base.fn.data$year==1969),]


library(dplyr)

nei1969 <- neighbors.1969[,c(1,4)]

nei1969 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1969


nei1969 <- tidyr::pivot_wider(nei1969, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1969) <- paste0('N', colnames(nei1969))
names(nei1969)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1969
as.data.frame(colnames(ydata))
par1969 <- ydata[,c(2,19,20)]
par1969 <- distinct(par1969, Box, .keep_all = TRUE)


####add neighbor ids
####1969
#N1
names(par1969)[names(par1969) == "Box"] <- "N1"
names(par1969)[names(par1969) == "Mother"] <- "N1.mother"
names(par1969)[names(par1969) == "Father"] <- "N1.father"
nei1969 <- merge(nei1969, par1969, by="N1", all.x=TRUE)
#N2
names(par1969)[names(par1969) == "N1"] <- "N2"
names(par1969)[names(par1969) == "N1.mother"] <- "N2.mother"
names(par1969)[names(par1969) == "N1.father"] <- "N2.father"
nei1969 <- merge(nei1969, par1969, by="N2", all.x=TRUE)
#N3
names(par1969)[names(par1969) == "N2"] <- "N3"
names(par1969)[names(par1969) == "N2.mother"] <- "N3.mother"
names(par1969)[names(par1969) == "N2.father"] <- "N3.father"
nei1969 <- merge(nei1969, par1969, by="N3", all.x=TRUE)
#N4
names(par1969)[names(par1969) == "N3"] <- "N4"
names(par1969)[names(par1969) == "N3.mother"] <- "N4.mother"
names(par1969)[names(par1969) == "N3.father"] <- "N4.father"
nei1969 <- merge(nei1969, par1969, by="N4", all.x=TRUE)
#N5
names(par1969)[names(par1969) == "N4"] <- "N5"
names(par1969)[names(par1969) == "N4.mother"] <- "N5.mother"
names(par1969)[names(par1969) == "N4.father"] <- "N5.father"
nei1969 <- merge(nei1969, par1969, by="N5", all.x=TRUE)
#N6
names(par1969)[names(par1969) == "N5"] <- "N6"
names(par1969)[names(par1969) == "N5.mother"] <- "N6.mother"
names(par1969)[names(par1969) == "N5.father"] <- "N6.father"
nei1969 <- merge(nei1969, par1969, by="N6", all.x=TRUE)
#N7
names(par1969)[names(par1969) == "N6"] <- "N7"
names(par1969)[names(par1969) == "N6.mother"] <- "N7.mother"
names(par1969)[names(par1969) == "N6.father"] <- "N7.father"
nei1969 <- merge(nei1969, par1969, by="N7", all.x=TRUE)
#N8
names(par1969)[names(par1969) == "N7"] <- "N8"
names(par1969)[names(par1969) == "N7.mother"] <- "N8.mother"
names(par1969)[names(par1969) == "N7.father"] <- "N8.father"
nei1969 <- merge(nei1969, par1969, by="N8", all.x=TRUE)
#N9
names(par1969)[names(par1969) == "N8"] <- "N9"
names(par1969)[names(par1969) == "N8.mother"] <- "N9.mother"
names(par1969)[names(par1969) == "N8.father"] <- "N9.father"
nei1969 <- merge(nei1969, par1969, by="N9", all.x=TRUE)
#N10
names(par1969)[names(par1969) == "N9"] <- "N10"
names(par1969)[names(par1969) == "N9.mother"] <- "N10.mother"
names(par1969)[names(par1969) == "N9.father"] <- "N10.father"
nei1969 <- merge(nei1969, par1969, by="N10", all.x=TRUE)

#and identifying column 
nei1969$year <- 1969
nei1969a  <-data.frame(nei1969,"box.year.parentid"=paste(nei1969$Focal.box, nei1969$year, "mother",sep="_")) 
nei1969b  <-data.frame(nei1969,"box.year.parentid"=paste(nei1969$Focal.box, nei1969$year, "father",sep="_")) 

nei1969 <- rbind(nei1969a, nei1969b)
rm(nei1969a, nei1969b)

nei1969$N9 <- NA 
nei1969$N10 <- NA

nei1969$N9.mother <- NA
nei1969$N9.father <- NA
nei1969$N10.mother <- NA
nei1969$N10.father <- NA

nei1969 <- nei1969[,order(colnames(nei1969))]

nei_output <- rbind(nei_output, nei1969)


#1970 ####
breeding.data.1970 <- xdata[which(xdata$year == 1970),] 
breeding.data.1970 <- breeding.data.1970[!is.na(breeding.data.1970$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1970 <- sf::st_as_sf(breeding.data.1970, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1970))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1970), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1970))
breeding.ids.1970 <- breeding.data.1970[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1970)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1970 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1970$Focal.male <- with(neighbors.1970, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1970$Focal.female <- with(neighbors.1970, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1970$Neighboring.male <- with(neighbors.1970, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1970$Neighboring.female <- with(neighbors.1970, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1970.b <- as.data.frame(with(neighbors.1970, paste(Focal.male, Neighboring.male, sep="_")))
names(N1970.b)[1] <- "ring_ring"
N1970.c <- as.data.frame(with(neighbors.1970, paste(Focal.male, Neighboring.female, sep="_")))
names(N1970.c)[1] <- "ring_ring"
N1970.d <- as.data.frame(with(neighbors.1970, paste(Focal.female, Neighboring.female, sep="_")))
names(N1970.d)[1] <- "ring_ring"
N1970.e <- as.data.frame(with(neighbors.1970, paste(Focal.female, Neighboring.male, sep="_")))
names(N1970.e)[1] <- "ring_ring"

N1970 <- rbind(N1970.b, N1970.c, N1970.d, N1970.e)
rm(N1970.b, N1970.c, N1970.d, N1970.e)
N1970$neighbors <- TRUE
N1970$Year.s <- 1971

N1970 <- N1970[!grepl("UNKNOWN", N1970$ring_ring),]

N_reference <- rbind(N_reference, N1970)


ydata <- base.fn.data[which(base.fn.data$year==1970),]


library(dplyr)

nei1970 <- neighbors.1970[,c(1,4)]

nei1970 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1970


nei1970 <- tidyr::pivot_wider(nei1970, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1970) <- paste0('N', colnames(nei1970))
names(nei1970)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1970
as.data.frame(colnames(ydata))
par1970 <- ydata[,c(2,19,20)]
par1970 <- distinct(par1970, Box, .keep_all = TRUE)


####add neighbor ids
####1970
#N1
names(par1970)[names(par1970) == "Box"] <- "N1"
names(par1970)[names(par1970) == "Mother"] <- "N1.mother"
names(par1970)[names(par1970) == "Father"] <- "N1.father"
nei1970 <- merge(nei1970, par1970, by="N1", all.x=TRUE)
#N2
names(par1970)[names(par1970) == "N1"] <- "N2"
names(par1970)[names(par1970) == "N1.mother"] <- "N2.mother"
names(par1970)[names(par1970) == "N1.father"] <- "N2.father"
nei1970 <- merge(nei1970, par1970, by="N2", all.x=TRUE)
#N3
names(par1970)[names(par1970) == "N2"] <- "N3"
names(par1970)[names(par1970) == "N2.mother"] <- "N3.mother"
names(par1970)[names(par1970) == "N2.father"] <- "N3.father"
nei1970 <- merge(nei1970, par1970, by="N3", all.x=TRUE)
#N4
names(par1970)[names(par1970) == "N3"] <- "N4"
names(par1970)[names(par1970) == "N3.mother"] <- "N4.mother"
names(par1970)[names(par1970) == "N3.father"] <- "N4.father"
nei1970 <- merge(nei1970, par1970, by="N4", all.x=TRUE)
#N5
names(par1970)[names(par1970) == "N4"] <- "N5"
names(par1970)[names(par1970) == "N4.mother"] <- "N5.mother"
names(par1970)[names(par1970) == "N4.father"] <- "N5.father"
nei1970 <- merge(nei1970, par1970, by="N5", all.x=TRUE)
#N6
names(par1970)[names(par1970) == "N5"] <- "N6"
names(par1970)[names(par1970) == "N5.mother"] <- "N6.mother"
names(par1970)[names(par1970) == "N5.father"] <- "N6.father"
nei1970 <- merge(nei1970, par1970, by="N6", all.x=TRUE)
#N7
names(par1970)[names(par1970) == "N6"] <- "N7"
names(par1970)[names(par1970) == "N6.mother"] <- "N7.mother"
names(par1970)[names(par1970) == "N6.father"] <- "N7.father"
nei1970 <- merge(nei1970, par1970, by="N7", all.x=TRUE)
#N8
names(par1970)[names(par1970) == "N7"] <- "N8"
names(par1970)[names(par1970) == "N7.mother"] <- "N8.mother"
names(par1970)[names(par1970) == "N7.father"] <- "N8.father"
nei1970 <- merge(nei1970, par1970, by="N8", all.x=TRUE)
#N9
names(par1970)[names(par1970) == "N8"] <- "N9"
names(par1970)[names(par1970) == "N8.mother"] <- "N9.mother"
names(par1970)[names(par1970) == "N8.father"] <- "N9.father"
nei1970 <- merge(nei1970, par1970, by="N9", all.x=TRUE)
#N10
names(par1970)[names(par1970) == "N9"] <- "N10"
names(par1970)[names(par1970) == "N9.mother"] <- "N10.mother"
names(par1970)[names(par1970) == "N9.father"] <- "N10.father"
nei1970 <- merge(nei1970, par1970, by="N10", all.x=TRUE)

#and identifying column 
nei1970$year <- 1970
nei1970a  <-data.frame(nei1970,"box.year.parentid"=paste(nei1970$Focal.box, nei1970$year, "mother",sep="_")) 
nei1970b  <-data.frame(nei1970,"box.year.parentid"=paste(nei1970$Focal.box, nei1970$year, "father",sep="_")) 

nei1970 <- rbind(nei1970a, nei1970b)
rm(nei1970a, nei1970b)

nei1970$N8 <- NA 
nei1970$N9 <- NA 
nei1970$N10 <- NA

nei1970$N8.mother <- NA 
nei1970$N8.father <- NA 
nei1970$N9.mother <- NA
nei1970$N9.father <- NA
nei1970$N10.mother <- NA
nei1970$N10.father <- NA

nei1970 <- nei1970[,order(colnames(nei1970))]

nei_output <- rbind(nei_output, nei1970)

#1971 ####
breeding.data.1971 <- xdata[which(xdata$year == 1971),] 
breeding.data.1971 <- breeding.data.1971[!is.na(breeding.data.1971$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1971 <- sf::st_as_sf(breeding.data.1971, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1971))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1971), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1971))
breeding.ids.1971 <- breeding.data.1971[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1971)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1971 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1971$Focal.male <- with(neighbors.1971, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1971$Focal.female <- with(neighbors.1971, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1971$Neighboring.male <- with(neighbors.1971, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1971$Neighboring.female <- with(neighbors.1971, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1971.b <- as.data.frame(with(neighbors.1971, paste(Focal.male, Neighboring.male, sep="_")))
names(N1971.b)[1] <- "ring_ring"
N1971.c <- as.data.frame(with(neighbors.1971, paste(Focal.male, Neighboring.female, sep="_")))
names(N1971.c)[1] <- "ring_ring"
N1971.d <- as.data.frame(with(neighbors.1971, paste(Focal.female, Neighboring.female, sep="_")))
names(N1971.d)[1] <- "ring_ring"
N1971.e <- as.data.frame(with(neighbors.1971, paste(Focal.female, Neighboring.male, sep="_")))
names(N1971.e)[1] <- "ring_ring"

N1971 <- rbind(N1971.b, N1971.c, N1971.d, N1971.e)
rm(N1971.b, N1971.c, N1971.d, N1971.e)
N1971$neighbors <- TRUE
N1971$Year.s <- 1972

N1971 <- N1971[!grepl("UNKNOWN", N1971$ring_ring),]

N_reference <- rbind(N_reference, N1971)


ydata <- base.fn.data[which(base.fn.data$year==1971),]


library(dplyr)

nei1971 <- neighbors.1971[,c(1,4)]

nei1971 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1971


nei1971 <- tidyr::pivot_wider(nei1971, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1971) <- paste0('N', colnames(nei1971))
names(nei1971)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1971
as.data.frame(colnames(ydata))
par1971 <- ydata[,c(2,19,20)]
par1971 <- distinct(par1971, Box, .keep_all = TRUE)


####add neighbor ids
####1971
#N1
names(par1971)[names(par1971) == "Box"] <- "N1"
names(par1971)[names(par1971) == "Mother"] <- "N1.mother"
names(par1971)[names(par1971) == "Father"] <- "N1.father"
nei1971 <- merge(nei1971, par1971, by="N1", all.x=TRUE)
#N2
names(par1971)[names(par1971) == "N1"] <- "N2"
names(par1971)[names(par1971) == "N1.mother"] <- "N2.mother"
names(par1971)[names(par1971) == "N1.father"] <- "N2.father"
nei1971 <- merge(nei1971, par1971, by="N2", all.x=TRUE)
#N3
names(par1971)[names(par1971) == "N2"] <- "N3"
names(par1971)[names(par1971) == "N2.mother"] <- "N3.mother"
names(par1971)[names(par1971) == "N2.father"] <- "N3.father"
nei1971 <- merge(nei1971, par1971, by="N3", all.x=TRUE)
#N4
names(par1971)[names(par1971) == "N3"] <- "N4"
names(par1971)[names(par1971) == "N3.mother"] <- "N4.mother"
names(par1971)[names(par1971) == "N3.father"] <- "N4.father"
nei1971 <- merge(nei1971, par1971, by="N4", all.x=TRUE)
#N5
names(par1971)[names(par1971) == "N4"] <- "N5"
names(par1971)[names(par1971) == "N4.mother"] <- "N5.mother"
names(par1971)[names(par1971) == "N4.father"] <- "N5.father"
nei1971 <- merge(nei1971, par1971, by="N5", all.x=TRUE)
#N6
names(par1971)[names(par1971) == "N5"] <- "N6"
names(par1971)[names(par1971) == "N5.mother"] <- "N6.mother"
names(par1971)[names(par1971) == "N5.father"] <- "N6.father"
nei1971 <- merge(nei1971, par1971, by="N6", all.x=TRUE)
#N7
names(par1971)[names(par1971) == "N6"] <- "N7"
names(par1971)[names(par1971) == "N6.mother"] <- "N7.mother"
names(par1971)[names(par1971) == "N6.father"] <- "N7.father"
nei1971 <- merge(nei1971, par1971, by="N7", all.x=TRUE)
#N8
names(par1971)[names(par1971) == "N7"] <- "N8"
names(par1971)[names(par1971) == "N7.mother"] <- "N8.mother"
names(par1971)[names(par1971) == "N7.father"] <- "N8.father"
nei1971 <- merge(nei1971, par1971, by="N8", all.x=TRUE)
#N9
names(par1971)[names(par1971) == "N8"] <- "N9"
names(par1971)[names(par1971) == "N8.mother"] <- "N9.mother"
names(par1971)[names(par1971) == "N8.father"] <- "N9.father"
nei1971 <- merge(nei1971, par1971, by="N9", all.x=TRUE)
#N10
names(par1971)[names(par1971) == "N9"] <- "N10"
names(par1971)[names(par1971) == "N9.mother"] <- "N10.mother"
names(par1971)[names(par1971) == "N9.father"] <- "N10.father"
nei1971 <- merge(nei1971, par1971, by="N10", all.x=TRUE)

#and identifying column 
nei1971$year <- 1971
nei1971a  <-data.frame(nei1971,"box.year.parentid"=paste(nei1971$Focal.box, nei1971$year, "mother",sep="_")) 
nei1971b  <-data.frame(nei1971,"box.year.parentid"=paste(nei1971$Focal.box, nei1971$year, "father",sep="_")) 

nei1971 <- rbind(nei1971a, nei1971b)
rm(nei1971a, nei1971b)

nei1971 <- nei1971[,order(colnames(nei1971))]

nei_output <- rbind(nei_output, nei1971)

#1972 ####
breeding.data.1972 <- xdata[which(xdata$year == 1972),] 
breeding.data.1972 <- breeding.data.1972[!is.na(breeding.data.1972$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1972 <- sf::st_as_sf(breeding.data.1972, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1972))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1972), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1972))
breeding.ids.1972 <- breeding.data.1972[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1972)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1972 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1972$Focal.male <- with(neighbors.1972, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1972$Focal.female <- with(neighbors.1972, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1972$Neighboring.male <- with(neighbors.1972, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1972$Neighboring.female <- with(neighbors.1972, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1972.b <- as.data.frame(with(neighbors.1972, paste(Focal.male, Neighboring.male, sep="_")))
names(N1972.b)[1] <- "ring_ring"
N1972.c <- as.data.frame(with(neighbors.1972, paste(Focal.male, Neighboring.female, sep="_")))
names(N1972.c)[1] <- "ring_ring"
N1972.d <- as.data.frame(with(neighbors.1972, paste(Focal.female, Neighboring.female, sep="_")))
names(N1972.d)[1] <- "ring_ring"
N1972.e <- as.data.frame(with(neighbors.1972, paste(Focal.female, Neighboring.male, sep="_")))
names(N1972.e)[1] <- "ring_ring"

N1972 <- rbind(N1972.b, N1972.c, N1972.d, N1972.e)
rm(N1972.b, N1972.c, N1972.d, N1972.e)
N1972$neighbors <- TRUE
N1972$Year.s <- 1973

N1972 <- N1972[!grepl("UNKNOWN", N1972$ring_ring),]

N_reference <- rbind(N_reference, N1972)


ydata <- base.fn.data[which(base.fn.data$year==1972),]


library(dplyr)

nei1972 <- neighbors.1972[,c(1,4)]

nei1972 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1972


nei1972 <- tidyr::pivot_wider(nei1972, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1972) <- paste0('N', colnames(nei1972))
names(nei1972)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1972
as.data.frame(colnames(ydata))
par1972 <- ydata[,c(2,19,20)]
par1972 <- distinct(par1972, Box, .keep_all = TRUE)


####add neighbor ids
####1972
#N1
names(par1972)[names(par1972) == "Box"] <- "N1"
names(par1972)[names(par1972) == "Mother"] <- "N1.mother"
names(par1972)[names(par1972) == "Father"] <- "N1.father"
nei1972 <- merge(nei1972, par1972, by="N1", all.x=TRUE)
#N2
names(par1972)[names(par1972) == "N1"] <- "N2"
names(par1972)[names(par1972) == "N1.mother"] <- "N2.mother"
names(par1972)[names(par1972) == "N1.father"] <- "N2.father"
nei1972 <- merge(nei1972, par1972, by="N2", all.x=TRUE)
#N3
names(par1972)[names(par1972) == "N2"] <- "N3"
names(par1972)[names(par1972) == "N2.mother"] <- "N3.mother"
names(par1972)[names(par1972) == "N2.father"] <- "N3.father"
nei1972 <- merge(nei1972, par1972, by="N3", all.x=TRUE)
#N4
names(par1972)[names(par1972) == "N3"] <- "N4"
names(par1972)[names(par1972) == "N3.mother"] <- "N4.mother"
names(par1972)[names(par1972) == "N3.father"] <- "N4.father"
nei1972 <- merge(nei1972, par1972, by="N4", all.x=TRUE)
#N5
names(par1972)[names(par1972) == "N4"] <- "N5"
names(par1972)[names(par1972) == "N4.mother"] <- "N5.mother"
names(par1972)[names(par1972) == "N4.father"] <- "N5.father"
nei1972 <- merge(nei1972, par1972, by="N5", all.x=TRUE)
#N6
names(par1972)[names(par1972) == "N5"] <- "N6"
names(par1972)[names(par1972) == "N5.mother"] <- "N6.mother"
names(par1972)[names(par1972) == "N5.father"] <- "N6.father"
nei1972 <- merge(nei1972, par1972, by="N6", all.x=TRUE)
#N7
names(par1972)[names(par1972) == "N6"] <- "N7"
names(par1972)[names(par1972) == "N6.mother"] <- "N7.mother"
names(par1972)[names(par1972) == "N6.father"] <- "N7.father"
nei1972 <- merge(nei1972, par1972, by="N7", all.x=TRUE)
#N8
names(par1972)[names(par1972) == "N7"] <- "N8"
names(par1972)[names(par1972) == "N7.mother"] <- "N8.mother"
names(par1972)[names(par1972) == "N7.father"] <- "N8.father"
nei1972 <- merge(nei1972, par1972, by="N8", all.x=TRUE)
#N9
names(par1972)[names(par1972) == "N8"] <- "N9"
names(par1972)[names(par1972) == "N8.mother"] <- "N9.mother"
names(par1972)[names(par1972) == "N8.father"] <- "N9.father"
nei1972 <- merge(nei1972, par1972, by="N9", all.x=TRUE)
#N10
names(par1972)[names(par1972) == "N9"] <- "N10"
names(par1972)[names(par1972) == "N9.mother"] <- "N10.mother"
names(par1972)[names(par1972) == "N9.father"] <- "N10.father"
nei1972 <- merge(nei1972, par1972, by="N10", all.x=TRUE)

#and identifying column 
nei1972$year <- 1972
nei1972a  <-data.frame(nei1972,"box.year.parentid"=paste(nei1972$Focal.box, nei1972$year, "mother",sep="_")) 
nei1972b  <-data.frame(nei1972,"box.year.parentid"=paste(nei1972$Focal.box, nei1972$year, "father",sep="_")) 

nei1972 <- rbind(nei1972a, nei1972b)
rm(nei1972a, nei1972b)

nei1972$N9 <- NA 
nei1972$N10 <- NA

nei1972$N9.mother <- NA
nei1972$N9.father <- NA
nei1972$N10.mother <- NA
nei1972$N10.father <- NA

nei1972 <- nei1972[,order(colnames(nei1972))]

nei_output <- rbind(nei_output, nei1972)

#1973 ####
breeding.data.1973 <- xdata[which(xdata$year == 1973),] 
breeding.data.1973 <- breeding.data.1973[!is.na(breeding.data.1973$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1973 <- sf::st_as_sf(breeding.data.1973, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1973))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1973), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1973))
breeding.ids.1973 <- breeding.data.1973[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1973)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1973 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1973$Focal.male <- with(neighbors.1973, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1973$Focal.female <- with(neighbors.1973, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1973$Neighboring.male <- with(neighbors.1973, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1973$Neighboring.female <- with(neighbors.1973, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1973.b <- as.data.frame(with(neighbors.1973, paste(Focal.male, Neighboring.male, sep="_")))
names(N1973.b)[1] <- "ring_ring"
N1973.c <- as.data.frame(with(neighbors.1973, paste(Focal.male, Neighboring.female, sep="_")))
names(N1973.c)[1] <- "ring_ring"
N1973.d <- as.data.frame(with(neighbors.1973, paste(Focal.female, Neighboring.female, sep="_")))
names(N1973.d)[1] <- "ring_ring"
N1973.e <- as.data.frame(with(neighbors.1973, paste(Focal.female, Neighboring.male, sep="_")))
names(N1973.e)[1] <- "ring_ring"

N1973 <- rbind(N1973.b, N1973.c, N1973.d, N1973.e)
rm(N1973.b, N1973.c, N1973.d, N1973.e)
N1973$neighbors <- TRUE
N1973$Year.s <- 1974

N1973 <- N1973[!grepl("UNKNOWN", N1973$ring_ring),]

N_reference <- rbind(N_reference, N1973)


ydata <- base.fn.data[which(base.fn.data$year==1973),]


library(dplyr)

nei1973 <- neighbors.1973[,c(1,4)]

nei1973 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1973


nei1973 <- tidyr::pivot_wider(nei1973, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1973) <- paste0('N', colnames(nei1973))
names(nei1973)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1973
as.data.frame(colnames(ydata))
par1973 <- ydata[,c(2,19,20)]
par1973 <- distinct(par1973, Box, .keep_all = TRUE)


####add neighbor ids
####1973
#N1
names(par1973)[names(par1973) == "Box"] <- "N1"
names(par1973)[names(par1973) == "Mother"] <- "N1.mother"
names(par1973)[names(par1973) == "Father"] <- "N1.father"
nei1973 <- merge(nei1973, par1973, by="N1", all.x=TRUE)
#N2
names(par1973)[names(par1973) == "N1"] <- "N2"
names(par1973)[names(par1973) == "N1.mother"] <- "N2.mother"
names(par1973)[names(par1973) == "N1.father"] <- "N2.father"
nei1973 <- merge(nei1973, par1973, by="N2", all.x=TRUE)
#N3
names(par1973)[names(par1973) == "N2"] <- "N3"
names(par1973)[names(par1973) == "N2.mother"] <- "N3.mother"
names(par1973)[names(par1973) == "N2.father"] <- "N3.father"
nei1973 <- merge(nei1973, par1973, by="N3", all.x=TRUE)
#N4
names(par1973)[names(par1973) == "N3"] <- "N4"
names(par1973)[names(par1973) == "N3.mother"] <- "N4.mother"
names(par1973)[names(par1973) == "N3.father"] <- "N4.father"
nei1973 <- merge(nei1973, par1973, by="N4", all.x=TRUE)
#N5
names(par1973)[names(par1973) == "N4"] <- "N5"
names(par1973)[names(par1973) == "N4.mother"] <- "N5.mother"
names(par1973)[names(par1973) == "N4.father"] <- "N5.father"
nei1973 <- merge(nei1973, par1973, by="N5", all.x=TRUE)
#N6
names(par1973)[names(par1973) == "N5"] <- "N6"
names(par1973)[names(par1973) == "N5.mother"] <- "N6.mother"
names(par1973)[names(par1973) == "N5.father"] <- "N6.father"
nei1973 <- merge(nei1973, par1973, by="N6", all.x=TRUE)
#N7
names(par1973)[names(par1973) == "N6"] <- "N7"
names(par1973)[names(par1973) == "N6.mother"] <- "N7.mother"
names(par1973)[names(par1973) == "N6.father"] <- "N7.father"
nei1973 <- merge(nei1973, par1973, by="N7", all.x=TRUE)
#N8
names(par1973)[names(par1973) == "N7"] <- "N8"
names(par1973)[names(par1973) == "N7.mother"] <- "N8.mother"
names(par1973)[names(par1973) == "N7.father"] <- "N8.father"
nei1973 <- merge(nei1973, par1973, by="N8", all.x=TRUE)
#N9
names(par1973)[names(par1973) == "N8"] <- "N9"
names(par1973)[names(par1973) == "N8.mother"] <- "N9.mother"
names(par1973)[names(par1973) == "N8.father"] <- "N9.father"
nei1973 <- merge(nei1973, par1973, by="N9", all.x=TRUE)
#N10
names(par1973)[names(par1973) == "N9"] <- "N10"
names(par1973)[names(par1973) == "N9.mother"] <- "N10.mother"
names(par1973)[names(par1973) == "N9.father"] <- "N10.father"
nei1973 <- merge(nei1973, par1973, by="N10", all.x=TRUE)

#and identifying column 
nei1973$year <- 1973
nei1973a  <-data.frame(nei1973,"box.year.parentid"=paste(nei1973$Focal.box, nei1973$year, "mother",sep="_")) 
nei1973b  <-data.frame(nei1973,"box.year.parentid"=paste(nei1973$Focal.box, nei1973$year, "father",sep="_")) 

nei1973 <- rbind(nei1973a, nei1973b)
rm(nei1973a, nei1973b)

nei1973$N9 <- NA 
nei1973$N10 <- NA

nei1973$N9.mother <- NA
nei1973$N9.father <- NA
nei1973$N10.mother <- NA
nei1973$N10.father <- NA

nei1973 <- nei1973[,order(colnames(nei1973))]

nei_output <- rbind(nei_output, nei1973)

#1974 ####
breeding.data.1974 <- xdata[which(xdata$year == 1974),] 
breeding.data.1974 <- breeding.data.1974[!is.na(breeding.data.1974$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1974 <- sf::st_as_sf(breeding.data.1974, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1974))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1974), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1974))
breeding.ids.1974 <- breeding.data.1974[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1974)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1974 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1974$Focal.male <- with(neighbors.1974, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1974$Focal.female <- with(neighbors.1974, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1974$Neighboring.male <- with(neighbors.1974, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1974$Neighboring.female <- with(neighbors.1974, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1974.b <- as.data.frame(with(neighbors.1974, paste(Focal.male, Neighboring.male, sep="_")))
names(N1974.b)[1] <- "ring_ring"
N1974.c <- as.data.frame(with(neighbors.1974, paste(Focal.male, Neighboring.female, sep="_")))
names(N1974.c)[1] <- "ring_ring"
N1974.d <- as.data.frame(with(neighbors.1974, paste(Focal.female, Neighboring.female, sep="_")))
names(N1974.d)[1] <- "ring_ring"
N1974.e <- as.data.frame(with(neighbors.1974, paste(Focal.female, Neighboring.male, sep="_")))
names(N1974.e)[1] <- "ring_ring"

N1974 <- rbind(N1974.b, N1974.c, N1974.d, N1974.e)
rm(N1974.b, N1974.c, N1974.d, N1974.e)
N1974$neighbors <- TRUE
N1974$Year.s <- 1975

N1974 <- N1974[!grepl("UNKNOWN", N1974$ring_ring),]

N_reference <- rbind(N_reference, N1974)


ydata <- base.fn.data[which(base.fn.data$year==1974),]


library(dplyr)

nei1974 <- neighbors.1974[,c(1,4)]

nei1974 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1974


nei1974 <- tidyr::pivot_wider(nei1974, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1974) <- paste0('N', colnames(nei1974))
names(nei1974)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1974
as.data.frame(colnames(ydata))
par1974 <- ydata[,c(2,19,20)]
par1974 <- distinct(par1974, Box, .keep_all = TRUE)


####add neighbor ids
####1974
#N1
names(par1974)[names(par1974) == "Box"] <- "N1"
names(par1974)[names(par1974) == "Mother"] <- "N1.mother"
names(par1974)[names(par1974) == "Father"] <- "N1.father"
nei1974 <- merge(nei1974, par1974, by="N1", all.x=TRUE)
#N2
names(par1974)[names(par1974) == "N1"] <- "N2"
names(par1974)[names(par1974) == "N1.mother"] <- "N2.mother"
names(par1974)[names(par1974) == "N1.father"] <- "N2.father"
nei1974 <- merge(nei1974, par1974, by="N2", all.x=TRUE)
#N3
names(par1974)[names(par1974) == "N2"] <- "N3"
names(par1974)[names(par1974) == "N2.mother"] <- "N3.mother"
names(par1974)[names(par1974) == "N2.father"] <- "N3.father"
nei1974 <- merge(nei1974, par1974, by="N3", all.x=TRUE)
#N4
names(par1974)[names(par1974) == "N3"] <- "N4"
names(par1974)[names(par1974) == "N3.mother"] <- "N4.mother"
names(par1974)[names(par1974) == "N3.father"] <- "N4.father"
nei1974 <- merge(nei1974, par1974, by="N4", all.x=TRUE)
#N5
names(par1974)[names(par1974) == "N4"] <- "N5"
names(par1974)[names(par1974) == "N4.mother"] <- "N5.mother"
names(par1974)[names(par1974) == "N4.father"] <- "N5.father"
nei1974 <- merge(nei1974, par1974, by="N5", all.x=TRUE)
#N6
names(par1974)[names(par1974) == "N5"] <- "N6"
names(par1974)[names(par1974) == "N5.mother"] <- "N6.mother"
names(par1974)[names(par1974) == "N5.father"] <- "N6.father"
nei1974 <- merge(nei1974, par1974, by="N6", all.x=TRUE)
#N7
names(par1974)[names(par1974) == "N6"] <- "N7"
names(par1974)[names(par1974) == "N6.mother"] <- "N7.mother"
names(par1974)[names(par1974) == "N6.father"] <- "N7.father"
nei1974 <- merge(nei1974, par1974, by="N7", all.x=TRUE)
#N8
names(par1974)[names(par1974) == "N7"] <- "N8"
names(par1974)[names(par1974) == "N7.mother"] <- "N8.mother"
names(par1974)[names(par1974) == "N7.father"] <- "N8.father"
nei1974 <- merge(nei1974, par1974, by="N8", all.x=TRUE)
#N9
names(par1974)[names(par1974) == "N8"] <- "N9"
names(par1974)[names(par1974) == "N8.mother"] <- "N9.mother"
names(par1974)[names(par1974) == "N8.father"] <- "N9.father"
nei1974 <- merge(nei1974, par1974, by="N9", all.x=TRUE)
#N10
names(par1974)[names(par1974) == "N9"] <- "N10"
names(par1974)[names(par1974) == "N9.mother"] <- "N10.mother"
names(par1974)[names(par1974) == "N9.father"] <- "N10.father"
nei1974 <- merge(nei1974, par1974, by="N10", all.x=TRUE)

#and identifying column 
nei1974$year <- 1974
nei1974a  <-data.frame(nei1974,"box.year.parentid"=paste(nei1974$Focal.box, nei1974$year, "mother",sep="_")) 
nei1974b  <-data.frame(nei1974,"box.year.parentid"=paste(nei1974$Focal.box, nei1974$year, "father",sep="_")) 

nei1974 <- rbind(nei1974a, nei1974b)
rm(nei1974a, nei1974b)

nei1974$N8 <- NA 
nei1974$N9 <- NA 
nei1974$N10 <- NA

nei1974$N8.mother <- NA 
nei1974$N8.father <- NA 
nei1974$N9.mother <- NA
nei1974$N9.father <- NA
nei1974$N10.mother <- NA
nei1974$N10.father <- NA

nei1974 <- nei1974[,order(colnames(nei1974))]

nei_output <- rbind(nei_output, nei1974)


#1975 ####
breeding.data.1975 <- xdata[which(xdata$year == 1975),] 
breeding.data.1975 <- breeding.data.1975[!is.na(breeding.data.1975$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1975 <- sf::st_as_sf(breeding.data.1975, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1975))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1975), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1975))
breeding.ids.1975 <- breeding.data.1975[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1975)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1975 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1975$Focal.male <- with(neighbors.1975, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1975$Focal.female <- with(neighbors.1975, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1975$Neighboring.male <- with(neighbors.1975, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1975$Neighboring.female <- with(neighbors.1975, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1975.b <- as.data.frame(with(neighbors.1975, paste(Focal.male, Neighboring.male, sep="_")))
names(N1975.b)[1] <- "ring_ring"
N1975.c <- as.data.frame(with(neighbors.1975, paste(Focal.male, Neighboring.female, sep="_")))
names(N1975.c)[1] <- "ring_ring"
N1975.d <- as.data.frame(with(neighbors.1975, paste(Focal.female, Neighboring.female, sep="_")))
names(N1975.d)[1] <- "ring_ring"
N1975.e <- as.data.frame(with(neighbors.1975, paste(Focal.female, Neighboring.male, sep="_")))
names(N1975.e)[1] <- "ring_ring"

N1975 <- rbind(N1975.b, N1975.c, N1975.d, N1975.e)
rm(N1975.b, N1975.c, N1975.d, N1975.e)
N1975$neighbors <- TRUE
N1975$Year.s <- 1976

N1975 <- N1975[!grepl("UNKNOWN", N1975$ring_ring),]

N_reference <- rbind(N_reference, N1975)


ydata <- base.fn.data[which(base.fn.data$year==1975),]


library(dplyr)

nei1975 <- neighbors.1975[,c(1,4)]

nei1975 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1975


nei1975 <- tidyr::pivot_wider(nei1975, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1975) <- paste0('N', colnames(nei1975))
names(nei1975)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1975
as.data.frame(colnames(ydata))
par1975 <- ydata[,c(2,19,20)]
par1975 <- distinct(par1975, Box, .keep_all = TRUE)


####add neighbor ids
####1975
#N1
names(par1975)[names(par1975) == "Box"] <- "N1"
names(par1975)[names(par1975) == "Mother"] <- "N1.mother"
names(par1975)[names(par1975) == "Father"] <- "N1.father"
nei1975 <- merge(nei1975, par1975, by="N1", all.x=TRUE)
#N2
names(par1975)[names(par1975) == "N1"] <- "N2"
names(par1975)[names(par1975) == "N1.mother"] <- "N2.mother"
names(par1975)[names(par1975) == "N1.father"] <- "N2.father"
nei1975 <- merge(nei1975, par1975, by="N2", all.x=TRUE)
#N3
names(par1975)[names(par1975) == "N2"] <- "N3"
names(par1975)[names(par1975) == "N2.mother"] <- "N3.mother"
names(par1975)[names(par1975) == "N2.father"] <- "N3.father"
nei1975 <- merge(nei1975, par1975, by="N3", all.x=TRUE)
#N4
names(par1975)[names(par1975) == "N3"] <- "N4"
names(par1975)[names(par1975) == "N3.mother"] <- "N4.mother"
names(par1975)[names(par1975) == "N3.father"] <- "N4.father"
nei1975 <- merge(nei1975, par1975, by="N4", all.x=TRUE)
#N5
names(par1975)[names(par1975) == "N4"] <- "N5"
names(par1975)[names(par1975) == "N4.mother"] <- "N5.mother"
names(par1975)[names(par1975) == "N4.father"] <- "N5.father"
nei1975 <- merge(nei1975, par1975, by="N5", all.x=TRUE)
#N6
names(par1975)[names(par1975) == "N5"] <- "N6"
names(par1975)[names(par1975) == "N5.mother"] <- "N6.mother"
names(par1975)[names(par1975) == "N5.father"] <- "N6.father"
nei1975 <- merge(nei1975, par1975, by="N6", all.x=TRUE)
#N7
names(par1975)[names(par1975) == "N6"] <- "N7"
names(par1975)[names(par1975) == "N6.mother"] <- "N7.mother"
names(par1975)[names(par1975) == "N6.father"] <- "N7.father"
nei1975 <- merge(nei1975, par1975, by="N7", all.x=TRUE)
#N8
names(par1975)[names(par1975) == "N7"] <- "N8"
names(par1975)[names(par1975) == "N7.mother"] <- "N8.mother"
names(par1975)[names(par1975) == "N7.father"] <- "N8.father"
nei1975 <- merge(nei1975, par1975, by="N8", all.x=TRUE)
#N9
names(par1975)[names(par1975) == "N8"] <- "N9"
names(par1975)[names(par1975) == "N8.mother"] <- "N9.mother"
names(par1975)[names(par1975) == "N8.father"] <- "N9.father"
nei1975 <- merge(nei1975, par1975, by="N9", all.x=TRUE)
#N10
names(par1975)[names(par1975) == "N9"] <- "N10"
names(par1975)[names(par1975) == "N9.mother"] <- "N10.mother"
names(par1975)[names(par1975) == "N9.father"] <- "N10.father"
nei1975 <- merge(nei1975, par1975, by="N10", all.x=TRUE)

#and identifying column 
nei1975$year <- 1975
nei1975a  <-data.frame(nei1975,"box.year.parentid"=paste(nei1975$Focal.box, nei1975$year, "mother",sep="_")) 
nei1975b  <-data.frame(nei1975,"box.year.parentid"=paste(nei1975$Focal.box, nei1975$year, "father",sep="_")) 

nei1975 <- rbind(nei1975a, nei1975b)
rm(nei1975a, nei1975b)

nei1975$N9 <- NA 
nei1975$N10 <- NA

nei1975$N9.mother <- NA
nei1975$N9.father <- NA
nei1975$N10.mother <- NA
nei1975$N10.father <- NA

nei1975 <- nei1975[,order(colnames(nei1975))]

nei_output <- rbind(nei_output, nei1975)

#1976 ####
breeding.data.1976 <- xdata[which(xdata$year == 1976),] 
breeding.data.1976 <- breeding.data.1976[!is.na(breeding.data.1976$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1976 <- sf::st_as_sf(breeding.data.1976, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1976))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1976), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1976))
breeding.ids.1976 <- breeding.data.1976[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1976)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1976 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1976$Focal.male <- with(neighbors.1976, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1976$Focal.female <- with(neighbors.1976, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1976$Neighboring.male <- with(neighbors.1976, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1976$Neighboring.female <- with(neighbors.1976, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1976.b <- as.data.frame(with(neighbors.1976, paste(Focal.male, Neighboring.male, sep="_")))
names(N1976.b)[1] <- "ring_ring"
N1976.c <- as.data.frame(with(neighbors.1976, paste(Focal.male, Neighboring.female, sep="_")))
names(N1976.c)[1] <- "ring_ring"
N1976.d <- as.data.frame(with(neighbors.1976, paste(Focal.female, Neighboring.female, sep="_")))
names(N1976.d)[1] <- "ring_ring"
N1976.e <- as.data.frame(with(neighbors.1976, paste(Focal.female, Neighboring.male, sep="_")))
names(N1976.e)[1] <- "ring_ring"

N1976 <- rbind(N1976.b, N1976.c, N1976.d, N1976.e)
rm(N1976.b, N1976.c, N1976.d, N1976.e)
N1976$neighbors <- TRUE
N1976$Year.s <- 1977

N1976 <- N1976[!grepl("UNKNOWN", N1976$ring_ring),]

N_reference <- rbind(N_reference, N1976)


ydata <- base.fn.data[which(base.fn.data$year==1976),]


library(dplyr)

nei1976 <- neighbors.1976[,c(1,4)]

nei1976 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1976


nei1976 <- tidyr::pivot_wider(nei1976, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1976) <- paste0('N', colnames(nei1976))
names(nei1976)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1976
as.data.frame(colnames(ydata))
par1976 <- ydata[,c(2,19,20)]
par1976 <- distinct(par1976, Box, .keep_all = TRUE)


####add neighbor ids
####1976
#N1
names(par1976)[names(par1976) == "Box"] <- "N1"
names(par1976)[names(par1976) == "Mother"] <- "N1.mother"
names(par1976)[names(par1976) == "Father"] <- "N1.father"
nei1976 <- merge(nei1976, par1976, by="N1", all.x=TRUE)
#N2
names(par1976)[names(par1976) == "N1"] <- "N2"
names(par1976)[names(par1976) == "N1.mother"] <- "N2.mother"
names(par1976)[names(par1976) == "N1.father"] <- "N2.father"
nei1976 <- merge(nei1976, par1976, by="N2", all.x=TRUE)
#N3
names(par1976)[names(par1976) == "N2"] <- "N3"
names(par1976)[names(par1976) == "N2.mother"] <- "N3.mother"
names(par1976)[names(par1976) == "N2.father"] <- "N3.father"
nei1976 <- merge(nei1976, par1976, by="N3", all.x=TRUE)
#N4
names(par1976)[names(par1976) == "N3"] <- "N4"
names(par1976)[names(par1976) == "N3.mother"] <- "N4.mother"
names(par1976)[names(par1976) == "N3.father"] <- "N4.father"
nei1976 <- merge(nei1976, par1976, by="N4", all.x=TRUE)
#N5
names(par1976)[names(par1976) == "N4"] <- "N5"
names(par1976)[names(par1976) == "N4.mother"] <- "N5.mother"
names(par1976)[names(par1976) == "N4.father"] <- "N5.father"
nei1976 <- merge(nei1976, par1976, by="N5", all.x=TRUE)
#N6
names(par1976)[names(par1976) == "N5"] <- "N6"
names(par1976)[names(par1976) == "N5.mother"] <- "N6.mother"
names(par1976)[names(par1976) == "N5.father"] <- "N6.father"
nei1976 <- merge(nei1976, par1976, by="N6", all.x=TRUE)
#N7
names(par1976)[names(par1976) == "N6"] <- "N7"
names(par1976)[names(par1976) == "N6.mother"] <- "N7.mother"
names(par1976)[names(par1976) == "N6.father"] <- "N7.father"
nei1976 <- merge(nei1976, par1976, by="N7", all.x=TRUE)
#N8
names(par1976)[names(par1976) == "N7"] <- "N8"
names(par1976)[names(par1976) == "N7.mother"] <- "N8.mother"
names(par1976)[names(par1976) == "N7.father"] <- "N8.father"
nei1976 <- merge(nei1976, par1976, by="N8", all.x=TRUE)
#N9
names(par1976)[names(par1976) == "N8"] <- "N9"
names(par1976)[names(par1976) == "N8.mother"] <- "N9.mother"
names(par1976)[names(par1976) == "N8.father"] <- "N9.father"
nei1976 <- merge(nei1976, par1976, by="N9", all.x=TRUE)
#N10
names(par1976)[names(par1976) == "N9"] <- "N10"
names(par1976)[names(par1976) == "N9.mother"] <- "N10.mother"
names(par1976)[names(par1976) == "N9.father"] <- "N10.father"
nei1976 <- merge(nei1976, par1976, by="N10", all.x=TRUE)

#and identifying column 
nei1976$year <- 1976
nei1976a  <-data.frame(nei1976,"box.year.parentid"=paste(nei1976$Focal.box, nei1976$year, "mother",sep="_")) 
nei1976b  <-data.frame(nei1976,"box.year.parentid"=paste(nei1976$Focal.box, nei1976$year, "father",sep="_")) 

nei1976 <- rbind(nei1976a, nei1976b)
rm(nei1976a, nei1976b)

nei1976$N10 <- NA

nei1976$N10.mother <- NA
nei1976$N10.father <- NA

nei1976 <- nei1976[,order(colnames(nei1976))]

nei_output <- rbind(nei_output, nei1976)

#1977 ####
breeding.data.1977 <- xdata[which(xdata$year == 1977),] 
breeding.data.1977 <- breeding.data.1977[!is.na(breeding.data.1977$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1977 <- sf::st_as_sf(breeding.data.1977, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1977))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1977), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1977))
breeding.ids.1977 <- breeding.data.1977[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1977)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1977 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1977$Focal.male <- with(neighbors.1977, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1977$Focal.female <- with(neighbors.1977, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1977$Neighboring.male <- with(neighbors.1977, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1977$Neighboring.female <- with(neighbors.1977, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1977.b <- as.data.frame(with(neighbors.1977, paste(Focal.male, Neighboring.male, sep="_")))
names(N1977.b)[1] <- "ring_ring"
N1977.c <- as.data.frame(with(neighbors.1977, paste(Focal.male, Neighboring.female, sep="_")))
names(N1977.c)[1] <- "ring_ring"
N1977.d <- as.data.frame(with(neighbors.1977, paste(Focal.female, Neighboring.female, sep="_")))
names(N1977.d)[1] <- "ring_ring"
N1977.e <- as.data.frame(with(neighbors.1977, paste(Focal.female, Neighboring.male, sep="_")))
names(N1977.e)[1] <- "ring_ring"

N1977 <- rbind(N1977.b, N1977.c, N1977.d, N1977.e)
rm(N1977.b, N1977.c, N1977.d, N1977.e)
N1977$neighbors <- TRUE
N1977$Year.s <- 1978

N1977 <- N1977[!grepl("UNKNOWN", N1977$ring_ring),]

N_reference <- rbind(N_reference, N1977)


ydata <- base.fn.data[which(base.fn.data$year==1977),]


library(dplyr)

nei1977 <- neighbors.1977[,c(1,4)]

nei1977 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1977


nei1977 <- tidyr::pivot_wider(nei1977, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1977) <- paste0('N', colnames(nei1977))
names(nei1977)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1977
as.data.frame(colnames(ydata))
par1977 <- ydata[,c(2,19,20)]
par1977 <- distinct(par1977, Box, .keep_all = TRUE)


####add neighbor ids
####1977
#N1
names(par1977)[names(par1977) == "Box"] <- "N1"
names(par1977)[names(par1977) == "Mother"] <- "N1.mother"
names(par1977)[names(par1977) == "Father"] <- "N1.father"
nei1977 <- merge(nei1977, par1977, by="N1", all.x=TRUE)
#N2
names(par1977)[names(par1977) == "N1"] <- "N2"
names(par1977)[names(par1977) == "N1.mother"] <- "N2.mother"
names(par1977)[names(par1977) == "N1.father"] <- "N2.father"
nei1977 <- merge(nei1977, par1977, by="N2", all.x=TRUE)
#N3
names(par1977)[names(par1977) == "N2"] <- "N3"
names(par1977)[names(par1977) == "N2.mother"] <- "N3.mother"
names(par1977)[names(par1977) == "N2.father"] <- "N3.father"
nei1977 <- merge(nei1977, par1977, by="N3", all.x=TRUE)
#N4
names(par1977)[names(par1977) == "N3"] <- "N4"
names(par1977)[names(par1977) == "N3.mother"] <- "N4.mother"
names(par1977)[names(par1977) == "N3.father"] <- "N4.father"
nei1977 <- merge(nei1977, par1977, by="N4", all.x=TRUE)
#N5
names(par1977)[names(par1977) == "N4"] <- "N5"
names(par1977)[names(par1977) == "N4.mother"] <- "N5.mother"
names(par1977)[names(par1977) == "N4.father"] <- "N5.father"
nei1977 <- merge(nei1977, par1977, by="N5", all.x=TRUE)
#N6
names(par1977)[names(par1977) == "N5"] <- "N6"
names(par1977)[names(par1977) == "N5.mother"] <- "N6.mother"
names(par1977)[names(par1977) == "N5.father"] <- "N6.father"
nei1977 <- merge(nei1977, par1977, by="N6", all.x=TRUE)
#N7
names(par1977)[names(par1977) == "N6"] <- "N7"
names(par1977)[names(par1977) == "N6.mother"] <- "N7.mother"
names(par1977)[names(par1977) == "N6.father"] <- "N7.father"
nei1977 <- merge(nei1977, par1977, by="N7", all.x=TRUE)
#N8
names(par1977)[names(par1977) == "N7"] <- "N8"
names(par1977)[names(par1977) == "N7.mother"] <- "N8.mother"
names(par1977)[names(par1977) == "N7.father"] <- "N8.father"
nei1977 <- merge(nei1977, par1977, by="N8", all.x=TRUE)
#N9
names(par1977)[names(par1977) == "N8"] <- "N9"
names(par1977)[names(par1977) == "N8.mother"] <- "N9.mother"
names(par1977)[names(par1977) == "N8.father"] <- "N9.father"
nei1977 <- merge(nei1977, par1977, by="N9", all.x=TRUE)
#N10
names(par1977)[names(par1977) == "N9"] <- "N10"
names(par1977)[names(par1977) == "N9.mother"] <- "N10.mother"
names(par1977)[names(par1977) == "N9.father"] <- "N10.father"
nei1977 <- merge(nei1977, par1977, by="N10", all.x=TRUE)

#and identifying column 
nei1977$year <- 1977
nei1977a  <-data.frame(nei1977,"box.year.parentid"=paste(nei1977$Focal.box, nei1977$year, "mother",sep="_")) 
nei1977b  <-data.frame(nei1977,"box.year.parentid"=paste(nei1977$Focal.box, nei1977$year, "father",sep="_")) 

nei1977 <- rbind(nei1977a, nei1977b)
rm(nei1977a, nei1977b)

nei1977 <- nei1977[,order(colnames(nei1977))]

nei_output <- rbind(nei_output, nei1977)

#1978 ####
breeding.data.1978 <- xdata[which(xdata$year == 1978),] 
breeding.data.1978 <- breeding.data.1978[!is.na(breeding.data.1978$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1978 <- sf::st_as_sf(breeding.data.1978, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1978))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1978), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1978))
breeding.ids.1978 <- breeding.data.1978[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1978)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1978 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1978$Focal.male <- with(neighbors.1978, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1978$Focal.female <- with(neighbors.1978, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1978$Neighboring.male <- with(neighbors.1978, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1978$Neighboring.female <- with(neighbors.1978, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1978.b <- as.data.frame(with(neighbors.1978, paste(Focal.male, Neighboring.male, sep="_")))
names(N1978.b)[1] <- "ring_ring"
N1978.c <- as.data.frame(with(neighbors.1978, paste(Focal.male, Neighboring.female, sep="_")))
names(N1978.c)[1] <- "ring_ring"
N1978.d <- as.data.frame(with(neighbors.1978, paste(Focal.female, Neighboring.female, sep="_")))
names(N1978.d)[1] <- "ring_ring"
N1978.e <- as.data.frame(with(neighbors.1978, paste(Focal.female, Neighboring.male, sep="_")))
names(N1978.e)[1] <- "ring_ring"

N1978 <- rbind(N1978.b, N1978.c, N1978.d, N1978.e)
rm(N1978.b, N1978.c, N1978.d, N1978.e)
N1978$neighbors <- TRUE
N1978$Year.s <- 1979

N1978 <- N1978[!grepl("UNKNOWN", N1978$ring_ring),]

N_reference <- rbind(N_reference, N1978)


ydata <- base.fn.data[which(base.fn.data$year==1978),]


library(dplyr)

nei1978 <- neighbors.1978[,c(1,4)]

nei1978 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1978


nei1978 <- tidyr::pivot_wider(nei1978, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1978) <- paste0('N', colnames(nei1978))
names(nei1978)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1978
as.data.frame(colnames(ydata))
par1978 <- ydata[,c(2,19,20)]
par1978 <- distinct(par1978, Box, .keep_all = TRUE)


####add neighbor ids
####1978
#N1
names(par1978)[names(par1978) == "Box"] <- "N1"
names(par1978)[names(par1978) == "Mother"] <- "N1.mother"
names(par1978)[names(par1978) == "Father"] <- "N1.father"
nei1978 <- merge(nei1978, par1978, by="N1", all.x=TRUE)
#N2
names(par1978)[names(par1978) == "N1"] <- "N2"
names(par1978)[names(par1978) == "N1.mother"] <- "N2.mother"
names(par1978)[names(par1978) == "N1.father"] <- "N2.father"
nei1978 <- merge(nei1978, par1978, by="N2", all.x=TRUE)
#N3
names(par1978)[names(par1978) == "N2"] <- "N3"
names(par1978)[names(par1978) == "N2.mother"] <- "N3.mother"
names(par1978)[names(par1978) == "N2.father"] <- "N3.father"
nei1978 <- merge(nei1978, par1978, by="N3", all.x=TRUE)
#N4
names(par1978)[names(par1978) == "N3"] <- "N4"
names(par1978)[names(par1978) == "N3.mother"] <- "N4.mother"
names(par1978)[names(par1978) == "N3.father"] <- "N4.father"
nei1978 <- merge(nei1978, par1978, by="N4", all.x=TRUE)
#N5
names(par1978)[names(par1978) == "N4"] <- "N5"
names(par1978)[names(par1978) == "N4.mother"] <- "N5.mother"
names(par1978)[names(par1978) == "N4.father"] <- "N5.father"
nei1978 <- merge(nei1978, par1978, by="N5", all.x=TRUE)
#N6
names(par1978)[names(par1978) == "N5"] <- "N6"
names(par1978)[names(par1978) == "N5.mother"] <- "N6.mother"
names(par1978)[names(par1978) == "N5.father"] <- "N6.father"
nei1978 <- merge(nei1978, par1978, by="N6", all.x=TRUE)
#N7
names(par1978)[names(par1978) == "N6"] <- "N7"
names(par1978)[names(par1978) == "N6.mother"] <- "N7.mother"
names(par1978)[names(par1978) == "N6.father"] <- "N7.father"
nei1978 <- merge(nei1978, par1978, by="N7", all.x=TRUE)
#N8
names(par1978)[names(par1978) == "N7"] <- "N8"
names(par1978)[names(par1978) == "N7.mother"] <- "N8.mother"
names(par1978)[names(par1978) == "N7.father"] <- "N8.father"
nei1978 <- merge(nei1978, par1978, by="N8", all.x=TRUE)
#N9
names(par1978)[names(par1978) == "N8"] <- "N9"
names(par1978)[names(par1978) == "N8.mother"] <- "N9.mother"
names(par1978)[names(par1978) == "N8.father"] <- "N9.father"
nei1978 <- merge(nei1978, par1978, by="N9", all.x=TRUE)
#N10
names(par1978)[names(par1978) == "N9"] <- "N10"
names(par1978)[names(par1978) == "N9.mother"] <- "N10.mother"
names(par1978)[names(par1978) == "N9.father"] <- "N10.father"
nei1978 <- merge(nei1978, par1978, by="N10", all.x=TRUE)

#and identifying column 
nei1978$year <- 1978
nei1978a  <-data.frame(nei1978,"box.year.parentid"=paste(nei1978$Focal.box, nei1978$year, "mother",sep="_")) 
nei1978b  <-data.frame(nei1978,"box.year.parentid"=paste(nei1978$Focal.box, nei1978$year, "father",sep="_")) 

nei1978 <- rbind(nei1978a, nei1978b)
rm(nei1978a, nei1978b)

nei1978$N9 <- NA 
nei1978$N10 <- NA

nei1978$N9.mother <- NA
nei1978$N9.father <- NA
nei1978$N10.mother <- NA
nei1978$N10.father <- NA

nei1978 <- nei1978[,order(colnames(nei1978))]

nei_output <- rbind(nei_output, nei1978)


#1979 ####
breeding.data.1979 <- xdata[which(xdata$year == 1979),] 
breeding.data.1979 <- breeding.data.1979[!is.na(breeding.data.1979$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1979 <- sf::st_as_sf(breeding.data.1979, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1979))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1979), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1979))
breeding.ids.1979 <- breeding.data.1979[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1979)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1979 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1979$Focal.male <- with(neighbors.1979, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1979$Focal.female <- with(neighbors.1979, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1979$Neighboring.male <- with(neighbors.1979, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1979$Neighboring.female <- with(neighbors.1979, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1979.b <- as.data.frame(with(neighbors.1979, paste(Focal.male, Neighboring.male, sep="_")))
names(N1979.b)[1] <- "ring_ring"
N1979.c <- as.data.frame(with(neighbors.1979, paste(Focal.male, Neighboring.female, sep="_")))
names(N1979.c)[1] <- "ring_ring"
N1979.d <- as.data.frame(with(neighbors.1979, paste(Focal.female, Neighboring.female, sep="_")))
names(N1979.d)[1] <- "ring_ring"
N1979.e <- as.data.frame(with(neighbors.1979, paste(Focal.female, Neighboring.male, sep="_")))
names(N1979.e)[1] <- "ring_ring"

N1979 <- rbind(N1979.b, N1979.c, N1979.d, N1979.e)
rm(N1979.b, N1979.c, N1979.d, N1979.e)
N1979$neighbors <- TRUE
N1979$Year.s <- 1980

N1979 <- N1979[!grepl("UNKNOWN", N1979$ring_ring),]

N_reference <- rbind(N_reference, N1979)


ydata <- base.fn.data[which(base.fn.data$year==1979),]


library(dplyr)

nei1979 <- neighbors.1979[,c(1,4)]

nei1979 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1979


nei1979 <- tidyr::pivot_wider(nei1979, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1979) <- paste0('N', colnames(nei1979))
names(nei1979)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1979
as.data.frame(colnames(ydata))
par1979 <- ydata[,c(2,19,20)]
par1979 <- distinct(par1979, Box, .keep_all = TRUE)


####add neighbor ids
####1979
#N1
names(par1979)[names(par1979) == "Box"] <- "N1"
names(par1979)[names(par1979) == "Mother"] <- "N1.mother"
names(par1979)[names(par1979) == "Father"] <- "N1.father"
nei1979 <- merge(nei1979, par1979, by="N1", all.x=TRUE)
#N2
names(par1979)[names(par1979) == "N1"] <- "N2"
names(par1979)[names(par1979) == "N1.mother"] <- "N2.mother"
names(par1979)[names(par1979) == "N1.father"] <- "N2.father"
nei1979 <- merge(nei1979, par1979, by="N2", all.x=TRUE)
#N3
names(par1979)[names(par1979) == "N2"] <- "N3"
names(par1979)[names(par1979) == "N2.mother"] <- "N3.mother"
names(par1979)[names(par1979) == "N2.father"] <- "N3.father"
nei1979 <- merge(nei1979, par1979, by="N3", all.x=TRUE)
#N4
names(par1979)[names(par1979) == "N3"] <- "N4"
names(par1979)[names(par1979) == "N3.mother"] <- "N4.mother"
names(par1979)[names(par1979) == "N3.father"] <- "N4.father"
nei1979 <- merge(nei1979, par1979, by="N4", all.x=TRUE)
#N5
names(par1979)[names(par1979) == "N4"] <- "N5"
names(par1979)[names(par1979) == "N4.mother"] <- "N5.mother"
names(par1979)[names(par1979) == "N4.father"] <- "N5.father"
nei1979 <- merge(nei1979, par1979, by="N5", all.x=TRUE)
#N6
names(par1979)[names(par1979) == "N5"] <- "N6"
names(par1979)[names(par1979) == "N5.mother"] <- "N6.mother"
names(par1979)[names(par1979) == "N5.father"] <- "N6.father"
nei1979 <- merge(nei1979, par1979, by="N6", all.x=TRUE)
#N7
names(par1979)[names(par1979) == "N6"] <- "N7"
names(par1979)[names(par1979) == "N6.mother"] <- "N7.mother"
names(par1979)[names(par1979) == "N6.father"] <- "N7.father"
nei1979 <- merge(nei1979, par1979, by="N7", all.x=TRUE)
#N8
names(par1979)[names(par1979) == "N7"] <- "N8"
names(par1979)[names(par1979) == "N7.mother"] <- "N8.mother"
names(par1979)[names(par1979) == "N7.father"] <- "N8.father"
nei1979 <- merge(nei1979, par1979, by="N8", all.x=TRUE)
#N9
names(par1979)[names(par1979) == "N8"] <- "N9"
names(par1979)[names(par1979) == "N8.mother"] <- "N9.mother"
names(par1979)[names(par1979) == "N8.father"] <- "N9.father"
nei1979 <- merge(nei1979, par1979, by="N9", all.x=TRUE)
#N10
names(par1979)[names(par1979) == "N9"] <- "N10"
names(par1979)[names(par1979) == "N9.mother"] <- "N10.mother"
names(par1979)[names(par1979) == "N9.father"] <- "N10.father"
nei1979 <- merge(nei1979, par1979, by="N10", all.x=TRUE)

#and identifying column 
nei1979$year <- 1979
nei1979a  <-data.frame(nei1979,"box.year.parentid"=paste(nei1979$Focal.box, nei1979$year, "mother",sep="_")) 
nei1979b  <-data.frame(nei1979,"box.year.parentid"=paste(nei1979$Focal.box, nei1979$year, "father",sep="_")) 

nei1979 <- rbind(nei1979a, nei1979b)
rm(nei1979a, nei1979b)

nei1979$N9 <- NA 
nei1979$N10 <- NA

nei1979$N9.mother <- NA
nei1979$N9.father <- NA
nei1979$N10.mother <- NA
nei1979$N10.father <- NA

nei1979 <- nei1979[,order(colnames(nei1979))]

nei_output <- rbind(nei_output, nei1979)


#1980 ####
breeding.data.1980 <- xdata[which(xdata$year == 1980),] 
breeding.data.1980 <- breeding.data.1980[!is.na(breeding.data.1980$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1980 <- sf::st_as_sf(breeding.data.1980, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1980))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1980), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1980))
breeding.ids.1980 <- breeding.data.1980[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1980)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1980 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1980$Focal.male <- with(neighbors.1980, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1980$Focal.female <- with(neighbors.1980, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1980$Neighboring.male <- with(neighbors.1980, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1980$Neighboring.female <- with(neighbors.1980, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1980.b <- as.data.frame(with(neighbors.1980, paste(Focal.male, Neighboring.male, sep="_")))
names(N1980.b)[1] <- "ring_ring"
N1980.c <- as.data.frame(with(neighbors.1980, paste(Focal.male, Neighboring.female, sep="_")))
names(N1980.c)[1] <- "ring_ring"
N1980.d <- as.data.frame(with(neighbors.1980, paste(Focal.female, Neighboring.female, sep="_")))
names(N1980.d)[1] <- "ring_ring"
N1980.e <- as.data.frame(with(neighbors.1980, paste(Focal.female, Neighboring.male, sep="_")))
names(N1980.e)[1] <- "ring_ring"

N1980 <- rbind(N1980.b, N1980.c, N1980.d, N1980.e)
rm(N1980.b, N1980.c, N1980.d, N1980.e)
N1980$neighbors <- TRUE
N1980$Year.s <- 1981

N1980 <- N1980[!grepl("UNKNOWN", N1980$ring_ring),]

N_reference <- rbind(N_reference, N1980)


ydata <- base.fn.data[which(base.fn.data$year==1980),]


library(dplyr)

nei1980 <- neighbors.1980[,c(1,4)]

nei1980 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1980


nei1980 <- tidyr::pivot_wider(nei1980, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1980) <- paste0('N', colnames(nei1980))
names(nei1980)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1980
as.data.frame(colnames(ydata))
par1980 <- ydata[,c(2,19,20)]
par1980 <- distinct(par1980, Box, .keep_all = TRUE)


####add neighbor ids
####1980
#N1
names(par1980)[names(par1980) == "Box"] <- "N1"
names(par1980)[names(par1980) == "Mother"] <- "N1.mother"
names(par1980)[names(par1980) == "Father"] <- "N1.father"
nei1980 <- merge(nei1980, par1980, by="N1", all.x=TRUE)
#N2
names(par1980)[names(par1980) == "N1"] <- "N2"
names(par1980)[names(par1980) == "N1.mother"] <- "N2.mother"
names(par1980)[names(par1980) == "N1.father"] <- "N2.father"
nei1980 <- merge(nei1980, par1980, by="N2", all.x=TRUE)
#N3
names(par1980)[names(par1980) == "N2"] <- "N3"
names(par1980)[names(par1980) == "N2.mother"] <- "N3.mother"
names(par1980)[names(par1980) == "N2.father"] <- "N3.father"
nei1980 <- merge(nei1980, par1980, by="N3", all.x=TRUE)
#N4
names(par1980)[names(par1980) == "N3"] <- "N4"
names(par1980)[names(par1980) == "N3.mother"] <- "N4.mother"
names(par1980)[names(par1980) == "N3.father"] <- "N4.father"
nei1980 <- merge(nei1980, par1980, by="N4", all.x=TRUE)
#N5
names(par1980)[names(par1980) == "N4"] <- "N5"
names(par1980)[names(par1980) == "N4.mother"] <- "N5.mother"
names(par1980)[names(par1980) == "N4.father"] <- "N5.father"
nei1980 <- merge(nei1980, par1980, by="N5", all.x=TRUE)
#N6
names(par1980)[names(par1980) == "N5"] <- "N6"
names(par1980)[names(par1980) == "N5.mother"] <- "N6.mother"
names(par1980)[names(par1980) == "N5.father"] <- "N6.father"
nei1980 <- merge(nei1980, par1980, by="N6", all.x=TRUE)
#N7
names(par1980)[names(par1980) == "N6"] <- "N7"
names(par1980)[names(par1980) == "N6.mother"] <- "N7.mother"
names(par1980)[names(par1980) == "N6.father"] <- "N7.father"
nei1980 <- merge(nei1980, par1980, by="N7", all.x=TRUE)
#N8
names(par1980)[names(par1980) == "N7"] <- "N8"
names(par1980)[names(par1980) == "N7.mother"] <- "N8.mother"
names(par1980)[names(par1980) == "N7.father"] <- "N8.father"
nei1980 <- merge(nei1980, par1980, by="N8", all.x=TRUE)
#N9
names(par1980)[names(par1980) == "N8"] <- "N9"
names(par1980)[names(par1980) == "N8.mother"] <- "N9.mother"
names(par1980)[names(par1980) == "N8.father"] <- "N9.father"
nei1980 <- merge(nei1980, par1980, by="N9", all.x=TRUE)
#N10
names(par1980)[names(par1980) == "N9"] <- "N10"
names(par1980)[names(par1980) == "N9.mother"] <- "N10.mother"
names(par1980)[names(par1980) == "N9.father"] <- "N10.father"
nei1980 <- merge(nei1980, par1980, by="N10", all.x=TRUE)

#and identifying column 
nei1980$year <- 1980
nei1980a  <-data.frame(nei1980,"box.year.parentid"=paste(nei1980$Focal.box, nei1980$year, "mother",sep="_")) 
nei1980b  <-data.frame(nei1980,"box.year.parentid"=paste(nei1980$Focal.box, nei1980$year, "father",sep="_")) 

nei1980 <- rbind(nei1980a, nei1980b)
rm(nei1980a, nei1980b)

nei1980$N9 <- NA 
nei1980$N10 <- NA

nei1980$N9.mother <- NA
nei1980$N9.father <- NA
nei1980$N10.mother <- NA
nei1980$N10.father <- NA

nei1980 <- nei1980[,order(colnames(nei1980))]

nei_output <- rbind(nei_output, nei1980)


#1981 ####
breeding.data.1981 <- xdata[which(xdata$year == 1981),] 
breeding.data.1981 <- breeding.data.1981[!is.na(breeding.data.1981$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1981 <- sf::st_as_sf(breeding.data.1981, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1981))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1981), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1981))
breeding.ids.1981 <- breeding.data.1981[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1981)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1981 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1981$Focal.male <- with(neighbors.1981, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1981$Focal.female <- with(neighbors.1981, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1981$Neighboring.male <- with(neighbors.1981, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1981$Neighboring.female <- with(neighbors.1981, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1981.b <- as.data.frame(with(neighbors.1981, paste(Focal.male, Neighboring.male, sep="_")))
names(N1981.b)[1] <- "ring_ring"
N1981.c <- as.data.frame(with(neighbors.1981, paste(Focal.male, Neighboring.female, sep="_")))
names(N1981.c)[1] <- "ring_ring"
N1981.d <- as.data.frame(with(neighbors.1981, paste(Focal.female, Neighboring.female, sep="_")))
names(N1981.d)[1] <- "ring_ring"
N1981.e <- as.data.frame(with(neighbors.1981, paste(Focal.female, Neighboring.male, sep="_")))
names(N1981.e)[1] <- "ring_ring"

N1981 <- rbind(N1981.b, N1981.c, N1981.d, N1981.e)
rm(N1981.b, N1981.c, N1981.d, N1981.e)
N1981$neighbors <- TRUE
N1981$Year.s <- 1982

N1981 <- N1981[!grepl("UNKNOWN", N1981$ring_ring),]

N_reference <- rbind(N_reference, N1981)


ydata <- base.fn.data[which(base.fn.data$year==1981),]


library(dplyr)

nei1981 <- neighbors.1981[,c(1,4)]

nei1981 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1981


nei1981 <- tidyr::pivot_wider(nei1981, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1981) <- paste0('N', colnames(nei1981))
names(nei1981)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1981
as.data.frame(colnames(ydata))
par1981 <- ydata[,c(2,19,20)]
par1981 <- distinct(par1981, Box, .keep_all = TRUE)


####add neighbor ids
####1981
#N1
names(par1981)[names(par1981) == "Box"] <- "N1"
names(par1981)[names(par1981) == "Mother"] <- "N1.mother"
names(par1981)[names(par1981) == "Father"] <- "N1.father"
nei1981 <- merge(nei1981, par1981, by="N1", all.x=TRUE)
#N2
names(par1981)[names(par1981) == "N1"] <- "N2"
names(par1981)[names(par1981) == "N1.mother"] <- "N2.mother"
names(par1981)[names(par1981) == "N1.father"] <- "N2.father"
nei1981 <- merge(nei1981, par1981, by="N2", all.x=TRUE)
#N3
names(par1981)[names(par1981) == "N2"] <- "N3"
names(par1981)[names(par1981) == "N2.mother"] <- "N3.mother"
names(par1981)[names(par1981) == "N2.father"] <- "N3.father"
nei1981 <- merge(nei1981, par1981, by="N3", all.x=TRUE)
#N4
names(par1981)[names(par1981) == "N3"] <- "N4"
names(par1981)[names(par1981) == "N3.mother"] <- "N4.mother"
names(par1981)[names(par1981) == "N3.father"] <- "N4.father"
nei1981 <- merge(nei1981, par1981, by="N4", all.x=TRUE)
#N5
names(par1981)[names(par1981) == "N4"] <- "N5"
names(par1981)[names(par1981) == "N4.mother"] <- "N5.mother"
names(par1981)[names(par1981) == "N4.father"] <- "N5.father"
nei1981 <- merge(nei1981, par1981, by="N5", all.x=TRUE)
#N6
names(par1981)[names(par1981) == "N5"] <- "N6"
names(par1981)[names(par1981) == "N5.mother"] <- "N6.mother"
names(par1981)[names(par1981) == "N5.father"] <- "N6.father"
nei1981 <- merge(nei1981, par1981, by="N6", all.x=TRUE)
#N7
names(par1981)[names(par1981) == "N6"] <- "N7"
names(par1981)[names(par1981) == "N6.mother"] <- "N7.mother"
names(par1981)[names(par1981) == "N6.father"] <- "N7.father"
nei1981 <- merge(nei1981, par1981, by="N7", all.x=TRUE)
#N8
names(par1981)[names(par1981) == "N7"] <- "N8"
names(par1981)[names(par1981) == "N7.mother"] <- "N8.mother"
names(par1981)[names(par1981) == "N7.father"] <- "N8.father"
nei1981 <- merge(nei1981, par1981, by="N8", all.x=TRUE)
#N9
names(par1981)[names(par1981) == "N8"] <- "N9"
names(par1981)[names(par1981) == "N8.mother"] <- "N9.mother"
names(par1981)[names(par1981) == "N8.father"] <- "N9.father"
nei1981 <- merge(nei1981, par1981, by="N9", all.x=TRUE)
#N10
names(par1981)[names(par1981) == "N9"] <- "N10"
names(par1981)[names(par1981) == "N9.mother"] <- "N10.mother"
names(par1981)[names(par1981) == "N9.father"] <- "N10.father"
nei1981 <- merge(nei1981, par1981, by="N10", all.x=TRUE)

#and identifying column 
nei1981$year <- 1981
nei1981a  <-data.frame(nei1981,"box.year.parentid"=paste(nei1981$Focal.box, nei1981$year, "mother",sep="_")) 
nei1981b  <-data.frame(nei1981,"box.year.parentid"=paste(nei1981$Focal.box, nei1981$year, "father",sep="_")) 

nei1981 <- rbind(nei1981a, nei1981b)
rm(nei1981a, nei1981b)

nei1981$N10 <- NA

nei1981$N10.mother <- NA
nei1981$N10.father <- NA

nei1981 <- nei1981[,order(colnames(nei1981))]

nei_output <- rbind(nei_output, nei1981)

#1982 ####
breeding.data.1982 <- xdata[which(xdata$year == 1982),] 
breeding.data.1982 <- breeding.data.1982[!is.na(breeding.data.1982$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1982 <- sf::st_as_sf(breeding.data.1982, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1982))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1982), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1982))
breeding.ids.1982 <- breeding.data.1982[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1982)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1982 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1982$Focal.male <- with(neighbors.1982, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1982$Focal.female <- with(neighbors.1982, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1982$Neighboring.male <- with(neighbors.1982, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1982$Neighboring.female <- with(neighbors.1982, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1982.b <- as.data.frame(with(neighbors.1982, paste(Focal.male, Neighboring.male, sep="_")))
names(N1982.b)[1] <- "ring_ring"
N1982.c <- as.data.frame(with(neighbors.1982, paste(Focal.male, Neighboring.female, sep="_")))
names(N1982.c)[1] <- "ring_ring"
N1982.d <- as.data.frame(with(neighbors.1982, paste(Focal.female, Neighboring.female, sep="_")))
names(N1982.d)[1] <- "ring_ring"
N1982.e <- as.data.frame(with(neighbors.1982, paste(Focal.female, Neighboring.male, sep="_")))
names(N1982.e)[1] <- "ring_ring"

N1982 <- rbind(N1982.b, N1982.c, N1982.d, N1982.e)
rm(N1982.b, N1982.c, N1982.d, N1982.e)
N1982$neighbors <- TRUE
N1982$Year.s <- 1983

N1982 <- N1982[!grepl("UNKNOWN", N1982$ring_ring),]

N_reference <- rbind(N_reference, N1982)


ydata <- base.fn.data[which(base.fn.data$year==1982),]


library(dplyr)

nei1982 <- neighbors.1982[,c(1,4)]

nei1982 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1982


nei1982 <- tidyr::pivot_wider(nei1982, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1982) <- paste0('N', colnames(nei1982))
names(nei1982)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1982
as.data.frame(colnames(ydata))
par1982 <- ydata[,c(2,19,20)]
par1982 <- distinct(par1982, Box, .keep_all = TRUE)


####add neighbor ids
####1982
#N1
names(par1982)[names(par1982) == "Box"] <- "N1"
names(par1982)[names(par1982) == "Mother"] <- "N1.mother"
names(par1982)[names(par1982) == "Father"] <- "N1.father"
nei1982 <- merge(nei1982, par1982, by="N1", all.x=TRUE)
#N2
names(par1982)[names(par1982) == "N1"] <- "N2"
names(par1982)[names(par1982) == "N1.mother"] <- "N2.mother"
names(par1982)[names(par1982) == "N1.father"] <- "N2.father"
nei1982 <- merge(nei1982, par1982, by="N2", all.x=TRUE)
#N3
names(par1982)[names(par1982) == "N2"] <- "N3"
names(par1982)[names(par1982) == "N2.mother"] <- "N3.mother"
names(par1982)[names(par1982) == "N2.father"] <- "N3.father"
nei1982 <- merge(nei1982, par1982, by="N3", all.x=TRUE)
#N4
names(par1982)[names(par1982) == "N3"] <- "N4"
names(par1982)[names(par1982) == "N3.mother"] <- "N4.mother"
names(par1982)[names(par1982) == "N3.father"] <- "N4.father"
nei1982 <- merge(nei1982, par1982, by="N4", all.x=TRUE)
#N5
names(par1982)[names(par1982) == "N4"] <- "N5"
names(par1982)[names(par1982) == "N4.mother"] <- "N5.mother"
names(par1982)[names(par1982) == "N4.father"] <- "N5.father"
nei1982 <- merge(nei1982, par1982, by="N5", all.x=TRUE)
#N6
names(par1982)[names(par1982) == "N5"] <- "N6"
names(par1982)[names(par1982) == "N5.mother"] <- "N6.mother"
names(par1982)[names(par1982) == "N5.father"] <- "N6.father"
nei1982 <- merge(nei1982, par1982, by="N6", all.x=TRUE)
#N7
names(par1982)[names(par1982) == "N6"] <- "N7"
names(par1982)[names(par1982) == "N6.mother"] <- "N7.mother"
names(par1982)[names(par1982) == "N6.father"] <- "N7.father"
nei1982 <- merge(nei1982, par1982, by="N7", all.x=TRUE)
#N8
names(par1982)[names(par1982) == "N7"] <- "N8"
names(par1982)[names(par1982) == "N7.mother"] <- "N8.mother"
names(par1982)[names(par1982) == "N7.father"] <- "N8.father"
nei1982 <- merge(nei1982, par1982, by="N8", all.x=TRUE)
#N9
names(par1982)[names(par1982) == "N8"] <- "N9"
names(par1982)[names(par1982) == "N8.mother"] <- "N9.mother"
names(par1982)[names(par1982) == "N8.father"] <- "N9.father"
nei1982 <- merge(nei1982, par1982, by="N9", all.x=TRUE)
#N10
names(par1982)[names(par1982) == "N9"] <- "N10"
names(par1982)[names(par1982) == "N9.mother"] <- "N10.mother"
names(par1982)[names(par1982) == "N9.father"] <- "N10.father"
nei1982 <- merge(nei1982, par1982, by="N10", all.x=TRUE)

#and identifying column 
nei1982$year <- 1982
nei1982a  <-data.frame(nei1982,"box.year.parentid"=paste(nei1982$Focal.box, nei1982$year, "mother",sep="_")) 
nei1982b  <-data.frame(nei1982,"box.year.parentid"=paste(nei1982$Focal.box, nei1982$year, "father",sep="_")) 

nei1982 <- rbind(nei1982a, nei1982b)
rm(nei1982a, nei1982b)

nei1982 <- nei1982[,order(colnames(nei1982))]

nei_output <- rbind(nei_output, nei1982)

#1983 ####
breeding.data.1983 <- xdata[which(xdata$year == 1983),] 
breeding.data.1983 <- breeding.data.1983[!is.na(breeding.data.1983$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1983 <- sf::st_as_sf(breeding.data.1983, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1983))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1983), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1983))
breeding.ids.1983 <- breeding.data.1983[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1983)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1983 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1983$Focal.male <- with(neighbors.1983, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1983$Focal.female <- with(neighbors.1983, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1983$Neighboring.male <- with(neighbors.1983, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1983$Neighboring.female <- with(neighbors.1983, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1983.b <- as.data.frame(with(neighbors.1983, paste(Focal.male, Neighboring.male, sep="_")))
names(N1983.b)[1] <- "ring_ring"
N1983.c <- as.data.frame(with(neighbors.1983, paste(Focal.male, Neighboring.female, sep="_")))
names(N1983.c)[1] <- "ring_ring"
N1983.d <- as.data.frame(with(neighbors.1983, paste(Focal.female, Neighboring.female, sep="_")))
names(N1983.d)[1] <- "ring_ring"
N1983.e <- as.data.frame(with(neighbors.1983, paste(Focal.female, Neighboring.male, sep="_")))
names(N1983.e)[1] <- "ring_ring"

N1983 <- rbind(N1983.b, N1983.c, N1983.d, N1983.e)
rm(N1983.b, N1983.c, N1983.d, N1983.e)
N1983$neighbors <- TRUE
N1983$Year.s <- 1984

N1983 <- N1983[!grepl("UNKNOWN", N1983$ring_ring),]

N_reference <- rbind(N_reference, N1983)


ydata <- base.fn.data[which(base.fn.data$year==1983),]


library(dplyr)

nei1983 <- neighbors.1983[,c(1,4)]

nei1983 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1983


nei1983 <- tidyr::pivot_wider(nei1983, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1983) <- paste0('N', colnames(nei1983))
names(nei1983)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1983
as.data.frame(colnames(ydata))
par1983 <- ydata[,c(2,19,20)]
par1983 <- distinct(par1983, Box, .keep_all = TRUE)


####add neighbor ids
####1983
#N1
names(par1983)[names(par1983) == "Box"] <- "N1"
names(par1983)[names(par1983) == "Mother"] <- "N1.mother"
names(par1983)[names(par1983) == "Father"] <- "N1.father"
nei1983 <- merge(nei1983, par1983, by="N1", all.x=TRUE)
#N2
names(par1983)[names(par1983) == "N1"] <- "N2"
names(par1983)[names(par1983) == "N1.mother"] <- "N2.mother"
names(par1983)[names(par1983) == "N1.father"] <- "N2.father"
nei1983 <- merge(nei1983, par1983, by="N2", all.x=TRUE)
#N3
names(par1983)[names(par1983) == "N2"] <- "N3"
names(par1983)[names(par1983) == "N2.mother"] <- "N3.mother"
names(par1983)[names(par1983) == "N2.father"] <- "N3.father"
nei1983 <- merge(nei1983, par1983, by="N3", all.x=TRUE)
#N4
names(par1983)[names(par1983) == "N3"] <- "N4"
names(par1983)[names(par1983) == "N3.mother"] <- "N4.mother"
names(par1983)[names(par1983) == "N3.father"] <- "N4.father"
nei1983 <- merge(nei1983, par1983, by="N4", all.x=TRUE)
#N5
names(par1983)[names(par1983) == "N4"] <- "N5"
names(par1983)[names(par1983) == "N4.mother"] <- "N5.mother"
names(par1983)[names(par1983) == "N4.father"] <- "N5.father"
nei1983 <- merge(nei1983, par1983, by="N5", all.x=TRUE)
#N6
names(par1983)[names(par1983) == "N5"] <- "N6"
names(par1983)[names(par1983) == "N5.mother"] <- "N6.mother"
names(par1983)[names(par1983) == "N5.father"] <- "N6.father"
nei1983 <- merge(nei1983, par1983, by="N6", all.x=TRUE)
#N7
names(par1983)[names(par1983) == "N6"] <- "N7"
names(par1983)[names(par1983) == "N6.mother"] <- "N7.mother"
names(par1983)[names(par1983) == "N6.father"] <- "N7.father"
nei1983 <- merge(nei1983, par1983, by="N7", all.x=TRUE)
#N8
names(par1983)[names(par1983) == "N7"] <- "N8"
names(par1983)[names(par1983) == "N7.mother"] <- "N8.mother"
names(par1983)[names(par1983) == "N7.father"] <- "N8.father"
nei1983 <- merge(nei1983, par1983, by="N8", all.x=TRUE)
#N9
names(par1983)[names(par1983) == "N8"] <- "N9"
names(par1983)[names(par1983) == "N8.mother"] <- "N9.mother"
names(par1983)[names(par1983) == "N8.father"] <- "N9.father"
nei1983 <- merge(nei1983, par1983, by="N9", all.x=TRUE)
#N10
names(par1983)[names(par1983) == "N9"] <- "N10"
names(par1983)[names(par1983) == "N9.mother"] <- "N10.mother"
names(par1983)[names(par1983) == "N9.father"] <- "N10.father"
nei1983 <- merge(nei1983, par1983, by="N10", all.x=TRUE)

#and identifying column 
nei1983$year <- 1983
nei1983a  <-data.frame(nei1983,"box.year.parentid"=paste(nei1983$Focal.box, nei1983$year, "mother",sep="_")) 
nei1983b  <-data.frame(nei1983,"box.year.parentid"=paste(nei1983$Focal.box, nei1983$year, "father",sep="_")) 

nei1983 <- rbind(nei1983a, nei1983b)
rm(nei1983a, nei1983b)

nei1983$N10 <- NA

nei1983$N10.mother <- NA
nei1983$N10.father <- NA

nei1983 <- nei1983[,order(colnames(nei1983))]

nei_output <- rbind(nei_output, nei1983)

#1984 ####
breeding.data.1984 <- xdata[which(xdata$year == 1984),] 
breeding.data.1984 <- breeding.data.1984[!is.na(breeding.data.1984$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1984 <- sf::st_as_sf(breeding.data.1984, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1984))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1984), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1984))
breeding.ids.1984 <- breeding.data.1984[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1984)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1984 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1984$Focal.male <- with(neighbors.1984, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1984$Focal.female <- with(neighbors.1984, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1984$Neighboring.male <- with(neighbors.1984, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1984$Neighboring.female <- with(neighbors.1984, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1984.b <- as.data.frame(with(neighbors.1984, paste(Focal.male, Neighboring.male, sep="_")))
names(N1984.b)[1] <- "ring_ring"
N1984.c <- as.data.frame(with(neighbors.1984, paste(Focal.male, Neighboring.female, sep="_")))
names(N1984.c)[1] <- "ring_ring"
N1984.d <- as.data.frame(with(neighbors.1984, paste(Focal.female, Neighboring.female, sep="_")))
names(N1984.d)[1] <- "ring_ring"
N1984.e <- as.data.frame(with(neighbors.1984, paste(Focal.female, Neighboring.male, sep="_")))
names(N1984.e)[1] <- "ring_ring"

N1984 <- rbind(N1984.b, N1984.c, N1984.d, N1984.e)
rm(N1984.b, N1984.c, N1984.d, N1984.e)
N1984$neighbors <- TRUE
N1984$Year.s <- 1985

N1984 <- N1984[!grepl("UNKNOWN", N1984$ring_ring),]

N_reference <- rbind(N_reference, N1984)


ydata <- base.fn.data[which(base.fn.data$year==1984),]


library(dplyr)

nei1984 <- neighbors.1984[,c(1,4)]

nei1984 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1984


nei1984 <- tidyr::pivot_wider(nei1984, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1984) <- paste0('N', colnames(nei1984))
names(nei1984)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1984
as.data.frame(colnames(ydata))
par1984 <- ydata[,c(2,19,20)]
par1984 <- distinct(par1984, Box, .keep_all = TRUE)


####add neighbor ids
####1984
#N1
names(par1984)[names(par1984) == "Box"] <- "N1"
names(par1984)[names(par1984) == "Mother"] <- "N1.mother"
names(par1984)[names(par1984) == "Father"] <- "N1.father"
nei1984 <- merge(nei1984, par1984, by="N1", all.x=TRUE)
#N2
names(par1984)[names(par1984) == "N1"] <- "N2"
names(par1984)[names(par1984) == "N1.mother"] <- "N2.mother"
names(par1984)[names(par1984) == "N1.father"] <- "N2.father"
nei1984 <- merge(nei1984, par1984, by="N2", all.x=TRUE)
#N3
names(par1984)[names(par1984) == "N2"] <- "N3"
names(par1984)[names(par1984) == "N2.mother"] <- "N3.mother"
names(par1984)[names(par1984) == "N2.father"] <- "N3.father"
nei1984 <- merge(nei1984, par1984, by="N3", all.x=TRUE)
#N4
names(par1984)[names(par1984) == "N3"] <- "N4"
names(par1984)[names(par1984) == "N3.mother"] <- "N4.mother"
names(par1984)[names(par1984) == "N3.father"] <- "N4.father"
nei1984 <- merge(nei1984, par1984, by="N4", all.x=TRUE)
#N5
names(par1984)[names(par1984) == "N4"] <- "N5"
names(par1984)[names(par1984) == "N4.mother"] <- "N5.mother"
names(par1984)[names(par1984) == "N4.father"] <- "N5.father"
nei1984 <- merge(nei1984, par1984, by="N5", all.x=TRUE)
#N6
names(par1984)[names(par1984) == "N5"] <- "N6"
names(par1984)[names(par1984) == "N5.mother"] <- "N6.mother"
names(par1984)[names(par1984) == "N5.father"] <- "N6.father"
nei1984 <- merge(nei1984, par1984, by="N6", all.x=TRUE)
#N7
names(par1984)[names(par1984) == "N6"] <- "N7"
names(par1984)[names(par1984) == "N6.mother"] <- "N7.mother"
names(par1984)[names(par1984) == "N6.father"] <- "N7.father"
nei1984 <- merge(nei1984, par1984, by="N7", all.x=TRUE)
#N8
names(par1984)[names(par1984) == "N7"] <- "N8"
names(par1984)[names(par1984) == "N7.mother"] <- "N8.mother"
names(par1984)[names(par1984) == "N7.father"] <- "N8.father"
nei1984 <- merge(nei1984, par1984, by="N8", all.x=TRUE)
#N9
names(par1984)[names(par1984) == "N8"] <- "N9"
names(par1984)[names(par1984) == "N8.mother"] <- "N9.mother"
names(par1984)[names(par1984) == "N8.father"] <- "N9.father"
nei1984 <- merge(nei1984, par1984, by="N9", all.x=TRUE)
#N10
names(par1984)[names(par1984) == "N9"] <- "N10"
names(par1984)[names(par1984) == "N9.mother"] <- "N10.mother"
names(par1984)[names(par1984) == "N9.father"] <- "N10.father"
nei1984 <- merge(nei1984, par1984, by="N10", all.x=TRUE)

#and identifying column 
nei1984$year <- 1984
nei1984a  <-data.frame(nei1984,"box.year.parentid"=paste(nei1984$Focal.box, nei1984$year, "mother",sep="_")) 
nei1984b  <-data.frame(nei1984,"box.year.parentid"=paste(nei1984$Focal.box, nei1984$year, "father",sep="_")) 

nei1984 <- rbind(nei1984a, nei1984b)
rm(nei1984a, nei1984b)

nei1984$N10 <- NA

nei1984$N10.mother <- NA
nei1984$N10.father <- NA

nei1984 <- nei1984[,order(colnames(nei1984))]

nei_output <- rbind(nei_output, nei1984)

#1985 ####
breeding.data.1985 <- xdata[which(xdata$year == 1985),] 
breeding.data.1985 <- breeding.data.1985[!is.na(breeding.data.1985$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1985 <- sf::st_as_sf(breeding.data.1985, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1985))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1985), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1985))
breeding.ids.1985 <- breeding.data.1985[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1985)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1985 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1985$Focal.male <- with(neighbors.1985, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1985$Focal.female <- with(neighbors.1985, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1985$Neighboring.male <- with(neighbors.1985, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1985$Neighboring.female <- with(neighbors.1985, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1985.b <- as.data.frame(with(neighbors.1985, paste(Focal.male, Neighboring.male, sep="_")))
names(N1985.b)[1] <- "ring_ring"
N1985.c <- as.data.frame(with(neighbors.1985, paste(Focal.male, Neighboring.female, sep="_")))
names(N1985.c)[1] <- "ring_ring"
N1985.d <- as.data.frame(with(neighbors.1985, paste(Focal.female, Neighboring.female, sep="_")))
names(N1985.d)[1] <- "ring_ring"
N1985.e <- as.data.frame(with(neighbors.1985, paste(Focal.female, Neighboring.male, sep="_")))
names(N1985.e)[1] <- "ring_ring"

N1985 <- rbind(N1985.b, N1985.c, N1985.d, N1985.e)
rm(N1985.b, N1985.c, N1985.d, N1985.e)
N1985$neighbors <- TRUE
N1985$Year.s <- 1986

N1985 <- N1985[!grepl("UNKNOWN", N1985$ring_ring),]

N_reference <- rbind(N_reference, N1985)


ydata <- base.fn.data[which(base.fn.data$year==1985),]


library(dplyr)

nei1985 <- neighbors.1985[,c(1,4)]

nei1985 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1985


nei1985 <- tidyr::pivot_wider(nei1985, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1985) <- paste0('N', colnames(nei1985))
names(nei1985)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1985
as.data.frame(colnames(ydata))
par1985 <- ydata[,c(2,19,20)]
par1985 <- distinct(par1985, Box, .keep_all = TRUE)


####add neighbor ids
####1985
#N1
names(par1985)[names(par1985) == "Box"] <- "N1"
names(par1985)[names(par1985) == "Mother"] <- "N1.mother"
names(par1985)[names(par1985) == "Father"] <- "N1.father"
nei1985 <- merge(nei1985, par1985, by="N1", all.x=TRUE)
#N2
names(par1985)[names(par1985) == "N1"] <- "N2"
names(par1985)[names(par1985) == "N1.mother"] <- "N2.mother"
names(par1985)[names(par1985) == "N1.father"] <- "N2.father"
nei1985 <- merge(nei1985, par1985, by="N2", all.x=TRUE)
#N3
names(par1985)[names(par1985) == "N2"] <- "N3"
names(par1985)[names(par1985) == "N2.mother"] <- "N3.mother"
names(par1985)[names(par1985) == "N2.father"] <- "N3.father"
nei1985 <- merge(nei1985, par1985, by="N3", all.x=TRUE)
#N4
names(par1985)[names(par1985) == "N3"] <- "N4"
names(par1985)[names(par1985) == "N3.mother"] <- "N4.mother"
names(par1985)[names(par1985) == "N3.father"] <- "N4.father"
nei1985 <- merge(nei1985, par1985, by="N4", all.x=TRUE)
#N5
names(par1985)[names(par1985) == "N4"] <- "N5"
names(par1985)[names(par1985) == "N4.mother"] <- "N5.mother"
names(par1985)[names(par1985) == "N4.father"] <- "N5.father"
nei1985 <- merge(nei1985, par1985, by="N5", all.x=TRUE)
#N6
names(par1985)[names(par1985) == "N5"] <- "N6"
names(par1985)[names(par1985) == "N5.mother"] <- "N6.mother"
names(par1985)[names(par1985) == "N5.father"] <- "N6.father"
nei1985 <- merge(nei1985, par1985, by="N6", all.x=TRUE)
#N7
names(par1985)[names(par1985) == "N6"] <- "N7"
names(par1985)[names(par1985) == "N6.mother"] <- "N7.mother"
names(par1985)[names(par1985) == "N6.father"] <- "N7.father"
nei1985 <- merge(nei1985, par1985, by="N7", all.x=TRUE)
#N8
names(par1985)[names(par1985) == "N7"] <- "N8"
names(par1985)[names(par1985) == "N7.mother"] <- "N8.mother"
names(par1985)[names(par1985) == "N7.father"] <- "N8.father"
nei1985 <- merge(nei1985, par1985, by="N8", all.x=TRUE)
#N9
names(par1985)[names(par1985) == "N8"] <- "N9"
names(par1985)[names(par1985) == "N8.mother"] <- "N9.mother"
names(par1985)[names(par1985) == "N8.father"] <- "N9.father"
nei1985 <- merge(nei1985, par1985, by="N9", all.x=TRUE)
#N10
names(par1985)[names(par1985) == "N9"] <- "N10"
names(par1985)[names(par1985) == "N9.mother"] <- "N10.mother"
names(par1985)[names(par1985) == "N9.father"] <- "N10.father"
nei1985 <- merge(nei1985, par1985, by="N10", all.x=TRUE)

#and identifying column 
nei1985$year <- 1985
nei1985a  <-data.frame(nei1985,"box.year.parentid"=paste(nei1985$Focal.box, nei1985$year, "mother",sep="_")) 
nei1985b  <-data.frame(nei1985,"box.year.parentid"=paste(nei1985$Focal.box, nei1985$year, "father",sep="_")) 

nei1985 <- rbind(nei1985a, nei1985b)
rm(nei1985a, nei1985b)

nei1985$N10 <- NA

nei1985$N10.mother <- NA
nei1985$N10.father <- NA

nei1985 <- nei1985[,order(colnames(nei1985))]

nei_output <- rbind(nei_output, nei1985)


#1986 ####
breeding.data.1986 <- xdata[which(xdata$year == 1986),] 
breeding.data.1986 <- breeding.data.1986[!is.na(breeding.data.1986$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1986 <- sf::st_as_sf(breeding.data.1986, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1986))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1986), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1986))
breeding.ids.1986 <- breeding.data.1986[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1986)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1986 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1986$Focal.male <- with(neighbors.1986, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1986$Focal.female <- with(neighbors.1986, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1986$Neighboring.male <- with(neighbors.1986, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1986$Neighboring.female <- with(neighbors.1986, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1986.b <- as.data.frame(with(neighbors.1986, paste(Focal.male, Neighboring.male, sep="_")))
names(N1986.b)[1] <- "ring_ring"
N1986.c <- as.data.frame(with(neighbors.1986, paste(Focal.male, Neighboring.female, sep="_")))
names(N1986.c)[1] <- "ring_ring"
N1986.d <- as.data.frame(with(neighbors.1986, paste(Focal.female, Neighboring.female, sep="_")))
names(N1986.d)[1] <- "ring_ring"
N1986.e <- as.data.frame(with(neighbors.1986, paste(Focal.female, Neighboring.male, sep="_")))
names(N1986.e)[1] <- "ring_ring"

N1986 <- rbind(N1986.b, N1986.c, N1986.d, N1986.e)
rm(N1986.b, N1986.c, N1986.d, N1986.e)
N1986$neighbors <- TRUE
N1986$Year.s <- 1987

N1986 <- N1986[!grepl("UNKNOWN", N1986$ring_ring),]

N_reference <- rbind(N_reference, N1986)


ydata <- base.fn.data[which(base.fn.data$year==1986),]


library(dplyr)

nei1986 <- neighbors.1986[,c(1,4)]

nei1986 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1986


nei1986 <- tidyr::pivot_wider(nei1986, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1986) <- paste0('N', colnames(nei1986))
names(nei1986)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1986
as.data.frame(colnames(ydata))
par1986 <- ydata[,c(2,19,20)]
par1986 <- distinct(par1986, Box, .keep_all = TRUE)


####add neighbor ids
####1986
#N1
names(par1986)[names(par1986) == "Box"] <- "N1"
names(par1986)[names(par1986) == "Mother"] <- "N1.mother"
names(par1986)[names(par1986) == "Father"] <- "N1.father"
nei1986 <- merge(nei1986, par1986, by="N1", all.x=TRUE)
#N2
names(par1986)[names(par1986) == "N1"] <- "N2"
names(par1986)[names(par1986) == "N1.mother"] <- "N2.mother"
names(par1986)[names(par1986) == "N1.father"] <- "N2.father"
nei1986 <- merge(nei1986, par1986, by="N2", all.x=TRUE)
#N3
names(par1986)[names(par1986) == "N2"] <- "N3"
names(par1986)[names(par1986) == "N2.mother"] <- "N3.mother"
names(par1986)[names(par1986) == "N2.father"] <- "N3.father"
nei1986 <- merge(nei1986, par1986, by="N3", all.x=TRUE)
#N4
names(par1986)[names(par1986) == "N3"] <- "N4"
names(par1986)[names(par1986) == "N3.mother"] <- "N4.mother"
names(par1986)[names(par1986) == "N3.father"] <- "N4.father"
nei1986 <- merge(nei1986, par1986, by="N4", all.x=TRUE)
#N5
names(par1986)[names(par1986) == "N4"] <- "N5"
names(par1986)[names(par1986) == "N4.mother"] <- "N5.mother"
names(par1986)[names(par1986) == "N4.father"] <- "N5.father"
nei1986 <- merge(nei1986, par1986, by="N5", all.x=TRUE)
#N6
names(par1986)[names(par1986) == "N5"] <- "N6"
names(par1986)[names(par1986) == "N5.mother"] <- "N6.mother"
names(par1986)[names(par1986) == "N5.father"] <- "N6.father"
nei1986 <- merge(nei1986, par1986, by="N6", all.x=TRUE)
#N7
names(par1986)[names(par1986) == "N6"] <- "N7"
names(par1986)[names(par1986) == "N6.mother"] <- "N7.mother"
names(par1986)[names(par1986) == "N6.father"] <- "N7.father"
nei1986 <- merge(nei1986, par1986, by="N7", all.x=TRUE)
#N8
names(par1986)[names(par1986) == "N7"] <- "N8"
names(par1986)[names(par1986) == "N7.mother"] <- "N8.mother"
names(par1986)[names(par1986) == "N7.father"] <- "N8.father"
nei1986 <- merge(nei1986, par1986, by="N8", all.x=TRUE)
#N9
names(par1986)[names(par1986) == "N8"] <- "N9"
names(par1986)[names(par1986) == "N8.mother"] <- "N9.mother"
names(par1986)[names(par1986) == "N8.father"] <- "N9.father"
nei1986 <- merge(nei1986, par1986, by="N9", all.x=TRUE)
#N10
names(par1986)[names(par1986) == "N9"] <- "N10"
names(par1986)[names(par1986) == "N9.mother"] <- "N10.mother"
names(par1986)[names(par1986) == "N9.father"] <- "N10.father"
nei1986 <- merge(nei1986, par1986, by="N10", all.x=TRUE)

#and identifying column 
nei1986$year <- 1986
nei1986a  <-data.frame(nei1986,"box.year.parentid"=paste(nei1986$Focal.box, nei1986$year, "mother",sep="_")) 
nei1986b  <-data.frame(nei1986,"box.year.parentid"=paste(nei1986$Focal.box, nei1986$year, "father",sep="_")) 

nei1986 <- rbind(nei1986a, nei1986b)
rm(nei1986a, nei1986b)

nei1986$N9 <- NA 
nei1986$N10 <- NA

nei1986$N9.mother <- NA
nei1986$N9.father <- NA
nei1986$N10.mother <- NA
nei1986$N10.father <- NA

nei1986 <- nei1986[,order(colnames(nei1986))]

nei_output <- rbind(nei_output, nei1986)


#1987 ####
breeding.data.1987 <- xdata[which(xdata$year == 1987),] 
breeding.data.1987 <- breeding.data.1987[!is.na(breeding.data.1987$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1987 <- sf::st_as_sf(breeding.data.1987, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1987))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1987), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1987))
breeding.ids.1987 <- breeding.data.1987[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1987)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1987 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1987$Focal.male <- with(neighbors.1987, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1987$Focal.female <- with(neighbors.1987, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1987$Neighboring.male <- with(neighbors.1987, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1987$Neighboring.female <- with(neighbors.1987, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1987.b <- as.data.frame(with(neighbors.1987, paste(Focal.male, Neighboring.male, sep="_")))
names(N1987.b)[1] <- "ring_ring"
N1987.c <- as.data.frame(with(neighbors.1987, paste(Focal.male, Neighboring.female, sep="_")))
names(N1987.c)[1] <- "ring_ring"
N1987.d <- as.data.frame(with(neighbors.1987, paste(Focal.female, Neighboring.female, sep="_")))
names(N1987.d)[1] <- "ring_ring"
N1987.e <- as.data.frame(with(neighbors.1987, paste(Focal.female, Neighboring.male, sep="_")))
names(N1987.e)[1] <- "ring_ring"

N1987 <- rbind(N1987.b, N1987.c, N1987.d, N1987.e)
rm(N1987.b, N1987.c, N1987.d, N1987.e)
N1987$neighbors <- TRUE
N1987$Year.s <- 1988

N1987 <- N1987[!grepl("UNKNOWN", N1987$ring_ring),]

N_reference <- rbind(N_reference, N1987)


ydata <- base.fn.data[which(base.fn.data$year==1987),]


library(dplyr)

nei1987 <- neighbors.1987[,c(1,4)]

nei1987 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1987


nei1987 <- tidyr::pivot_wider(nei1987, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1987) <- paste0('N', colnames(nei1987))
names(nei1987)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1987
as.data.frame(colnames(ydata))
par1987 <- ydata[,c(2,19,20)]
par1987 <- distinct(par1987, Box, .keep_all = TRUE)


####add neighbor ids
####1987
#N1
names(par1987)[names(par1987) == "Box"] <- "N1"
names(par1987)[names(par1987) == "Mother"] <- "N1.mother"
names(par1987)[names(par1987) == "Father"] <- "N1.father"
nei1987 <- merge(nei1987, par1987, by="N1", all.x=TRUE)
#N2
names(par1987)[names(par1987) == "N1"] <- "N2"
names(par1987)[names(par1987) == "N1.mother"] <- "N2.mother"
names(par1987)[names(par1987) == "N1.father"] <- "N2.father"
nei1987 <- merge(nei1987, par1987, by="N2", all.x=TRUE)
#N3
names(par1987)[names(par1987) == "N2"] <- "N3"
names(par1987)[names(par1987) == "N2.mother"] <- "N3.mother"
names(par1987)[names(par1987) == "N2.father"] <- "N3.father"
nei1987 <- merge(nei1987, par1987, by="N3", all.x=TRUE)
#N4
names(par1987)[names(par1987) == "N3"] <- "N4"
names(par1987)[names(par1987) == "N3.mother"] <- "N4.mother"
names(par1987)[names(par1987) == "N3.father"] <- "N4.father"
nei1987 <- merge(nei1987, par1987, by="N4", all.x=TRUE)
#N5
names(par1987)[names(par1987) == "N4"] <- "N5"
names(par1987)[names(par1987) == "N4.mother"] <- "N5.mother"
names(par1987)[names(par1987) == "N4.father"] <- "N5.father"
nei1987 <- merge(nei1987, par1987, by="N5", all.x=TRUE)
#N6
names(par1987)[names(par1987) == "N5"] <- "N6"
names(par1987)[names(par1987) == "N5.mother"] <- "N6.mother"
names(par1987)[names(par1987) == "N5.father"] <- "N6.father"
nei1987 <- merge(nei1987, par1987, by="N6", all.x=TRUE)
#N7
names(par1987)[names(par1987) == "N6"] <- "N7"
names(par1987)[names(par1987) == "N6.mother"] <- "N7.mother"
names(par1987)[names(par1987) == "N6.father"] <- "N7.father"
nei1987 <- merge(nei1987, par1987, by="N7", all.x=TRUE)
#N8
names(par1987)[names(par1987) == "N7"] <- "N8"
names(par1987)[names(par1987) == "N7.mother"] <- "N8.mother"
names(par1987)[names(par1987) == "N7.father"] <- "N8.father"
nei1987 <- merge(nei1987, par1987, by="N8", all.x=TRUE)
#N9
names(par1987)[names(par1987) == "N8"] <- "N9"
names(par1987)[names(par1987) == "N8.mother"] <- "N9.mother"
names(par1987)[names(par1987) == "N8.father"] <- "N9.father"
nei1987 <- merge(nei1987, par1987, by="N9", all.x=TRUE)
#N10
names(par1987)[names(par1987) == "N9"] <- "N10"
names(par1987)[names(par1987) == "N9.mother"] <- "N10.mother"
names(par1987)[names(par1987) == "N9.father"] <- "N10.father"
nei1987 <- merge(nei1987, par1987, by="N10", all.x=TRUE)

#and identifying column 
nei1987$year <- 1987
nei1987a  <-data.frame(nei1987,"box.year.parentid"=paste(nei1987$Focal.box, nei1987$year, "mother",sep="_")) 
nei1987b  <-data.frame(nei1987,"box.year.parentid"=paste(nei1987$Focal.box, nei1987$year, "father",sep="_")) 

nei1987 <- rbind(nei1987a, nei1987b)
rm(nei1987a, nei1987b)

nei1987 <- nei1987[,order(colnames(nei1987))]

nei_output <- rbind(nei_output, nei1987)


#1988 ####
breeding.data.1988 <- xdata[which(xdata$year == 1988),] 
breeding.data.1988 <- breeding.data.1988[!is.na(breeding.data.1988$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1988 <- sf::st_as_sf(breeding.data.1988, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1988))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1988), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1988))
breeding.ids.1988 <- breeding.data.1988[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1988)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1988 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1988$Focal.male <- with(neighbors.1988, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1988$Focal.female <- with(neighbors.1988, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1988$Neighboring.male <- with(neighbors.1988, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1988$Neighboring.female <- with(neighbors.1988, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1988.b <- as.data.frame(with(neighbors.1988, paste(Focal.male, Neighboring.male, sep="_")))
names(N1988.b)[1] <- "ring_ring"
N1988.c <- as.data.frame(with(neighbors.1988, paste(Focal.male, Neighboring.female, sep="_")))
names(N1988.c)[1] <- "ring_ring"
N1988.d <- as.data.frame(with(neighbors.1988, paste(Focal.female, Neighboring.female, sep="_")))
names(N1988.d)[1] <- "ring_ring"
N1988.e <- as.data.frame(with(neighbors.1988, paste(Focal.female, Neighboring.male, sep="_")))
names(N1988.e)[1] <- "ring_ring"

N1988 <- rbind(N1988.b, N1988.c, N1988.d, N1988.e)
rm(N1988.b, N1988.c, N1988.d, N1988.e)
N1988$neighbors <- TRUE
N1988$Year.s <- 1989

N1988 <- N1988[!grepl("UNKNOWN", N1988$ring_ring),]

N_reference <- rbind(N_reference, N1988)


ydata <- base.fn.data[which(base.fn.data$year==1988),]


library(dplyr)

nei1988 <- neighbors.1988[,c(1,4)]

nei1988 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1988


nei1988 <- tidyr::pivot_wider(nei1988, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1988) <- paste0('N', colnames(nei1988))
names(nei1988)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1988
as.data.frame(colnames(ydata))
par1988 <- ydata[,c(2,19,20)]
par1988 <- distinct(par1988, Box, .keep_all = TRUE)


####add neighbor ids
####1988
#N1
names(par1988)[names(par1988) == "Box"] <- "N1"
names(par1988)[names(par1988) == "Mother"] <- "N1.mother"
names(par1988)[names(par1988) == "Father"] <- "N1.father"
nei1988 <- merge(nei1988, par1988, by="N1", all.x=TRUE)
#N2
names(par1988)[names(par1988) == "N1"] <- "N2"
names(par1988)[names(par1988) == "N1.mother"] <- "N2.mother"
names(par1988)[names(par1988) == "N1.father"] <- "N2.father"
nei1988 <- merge(nei1988, par1988, by="N2", all.x=TRUE)
#N3
names(par1988)[names(par1988) == "N2"] <- "N3"
names(par1988)[names(par1988) == "N2.mother"] <- "N3.mother"
names(par1988)[names(par1988) == "N2.father"] <- "N3.father"
nei1988 <- merge(nei1988, par1988, by="N3", all.x=TRUE)
#N4
names(par1988)[names(par1988) == "N3"] <- "N4"
names(par1988)[names(par1988) == "N3.mother"] <- "N4.mother"
names(par1988)[names(par1988) == "N3.father"] <- "N4.father"
nei1988 <- merge(nei1988, par1988, by="N4", all.x=TRUE)
#N5
names(par1988)[names(par1988) == "N4"] <- "N5"
names(par1988)[names(par1988) == "N4.mother"] <- "N5.mother"
names(par1988)[names(par1988) == "N4.father"] <- "N5.father"
nei1988 <- merge(nei1988, par1988, by="N5", all.x=TRUE)
#N6
names(par1988)[names(par1988) == "N5"] <- "N6"
names(par1988)[names(par1988) == "N5.mother"] <- "N6.mother"
names(par1988)[names(par1988) == "N5.father"] <- "N6.father"
nei1988 <- merge(nei1988, par1988, by="N6", all.x=TRUE)
#N7
names(par1988)[names(par1988) == "N6"] <- "N7"
names(par1988)[names(par1988) == "N6.mother"] <- "N7.mother"
names(par1988)[names(par1988) == "N6.father"] <- "N7.father"
nei1988 <- merge(nei1988, par1988, by="N7", all.x=TRUE)
#N8
names(par1988)[names(par1988) == "N7"] <- "N8"
names(par1988)[names(par1988) == "N7.mother"] <- "N8.mother"
names(par1988)[names(par1988) == "N7.father"] <- "N8.father"
nei1988 <- merge(nei1988, par1988, by="N8", all.x=TRUE)
#N9
names(par1988)[names(par1988) == "N8"] <- "N9"
names(par1988)[names(par1988) == "N8.mother"] <- "N9.mother"
names(par1988)[names(par1988) == "N8.father"] <- "N9.father"
nei1988 <- merge(nei1988, par1988, by="N9", all.x=TRUE)
#N10
names(par1988)[names(par1988) == "N9"] <- "N10"
names(par1988)[names(par1988) == "N9.mother"] <- "N10.mother"
names(par1988)[names(par1988) == "N9.father"] <- "N10.father"
nei1988 <- merge(nei1988, par1988, by="N10", all.x=TRUE)

#and identifying column 
nei1988$year <- 1988
nei1988a  <-data.frame(nei1988,"box.year.parentid"=paste(nei1988$Focal.box, nei1988$year, "mother",sep="_")) 
nei1988b  <-data.frame(nei1988,"box.year.parentid"=paste(nei1988$Focal.box, nei1988$year, "father",sep="_")) 

nei1988 <- rbind(nei1988a, nei1988b)
rm(nei1988a, nei1988b)

nei1988 <- nei1988[,order(colnames(nei1988))]

nei_output <- rbind(nei_output, nei1988)

#1989 ####
breeding.data.1989 <- xdata[which(xdata$year == 1989),] 
breeding.data.1989 <- breeding.data.1989[!is.na(breeding.data.1989$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1989 <- sf::st_as_sf(breeding.data.1989, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1989))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1989), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1989))
breeding.ids.1989 <- breeding.data.1989[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1989)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1989 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1989$Focal.male <- with(neighbors.1989, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1989$Focal.female <- with(neighbors.1989, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1989$Neighboring.male <- with(neighbors.1989, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1989$Neighboring.female <- with(neighbors.1989, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1989.b <- as.data.frame(with(neighbors.1989, paste(Focal.male, Neighboring.male, sep="_")))
names(N1989.b)[1] <- "ring_ring"
N1989.c <- as.data.frame(with(neighbors.1989, paste(Focal.male, Neighboring.female, sep="_")))
names(N1989.c)[1] <- "ring_ring"
N1989.d <- as.data.frame(with(neighbors.1989, paste(Focal.female, Neighboring.female, sep="_")))
names(N1989.d)[1] <- "ring_ring"
N1989.e <- as.data.frame(with(neighbors.1989, paste(Focal.female, Neighboring.male, sep="_")))
names(N1989.e)[1] <- "ring_ring"

N1989 <- rbind(N1989.b, N1989.c, N1989.d, N1989.e)
rm(N1989.b, N1989.c, N1989.d, N1989.e)
N1989$neighbors <- TRUE
N1989$Year.s <- 1990

N1989 <- N1989[!grepl("UNKNOWN", N1989$ring_ring),]

N_reference <- rbind(N_reference, N1989)


ydata <- base.fn.data[which(base.fn.data$year==1989),]


library(dplyr)

nei1989 <- neighbors.1989[,c(1,4)]

nei1989 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1989


nei1989 <- tidyr::pivot_wider(nei1989, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1989) <- paste0('N', colnames(nei1989))
names(nei1989)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1989
as.data.frame(colnames(ydata))
par1989 <- ydata[,c(2,19,20)]
par1989 <- distinct(par1989, Box, .keep_all = TRUE)


####add neighbor ids
####1989
#N1
names(par1989)[names(par1989) == "Box"] <- "N1"
names(par1989)[names(par1989) == "Mother"] <- "N1.mother"
names(par1989)[names(par1989) == "Father"] <- "N1.father"
nei1989 <- merge(nei1989, par1989, by="N1", all.x=TRUE)
#N2
names(par1989)[names(par1989) == "N1"] <- "N2"
names(par1989)[names(par1989) == "N1.mother"] <- "N2.mother"
names(par1989)[names(par1989) == "N1.father"] <- "N2.father"
nei1989 <- merge(nei1989, par1989, by="N2", all.x=TRUE)
#N3
names(par1989)[names(par1989) == "N2"] <- "N3"
names(par1989)[names(par1989) == "N2.mother"] <- "N3.mother"
names(par1989)[names(par1989) == "N2.father"] <- "N3.father"
nei1989 <- merge(nei1989, par1989, by="N3", all.x=TRUE)
#N4
names(par1989)[names(par1989) == "N3"] <- "N4"
names(par1989)[names(par1989) == "N3.mother"] <- "N4.mother"
names(par1989)[names(par1989) == "N3.father"] <- "N4.father"
nei1989 <- merge(nei1989, par1989, by="N4", all.x=TRUE)
#N5
names(par1989)[names(par1989) == "N4"] <- "N5"
names(par1989)[names(par1989) == "N4.mother"] <- "N5.mother"
names(par1989)[names(par1989) == "N4.father"] <- "N5.father"
nei1989 <- merge(nei1989, par1989, by="N5", all.x=TRUE)
#N6
names(par1989)[names(par1989) == "N5"] <- "N6"
names(par1989)[names(par1989) == "N5.mother"] <- "N6.mother"
names(par1989)[names(par1989) == "N5.father"] <- "N6.father"
nei1989 <- merge(nei1989, par1989, by="N6", all.x=TRUE)
#N7
names(par1989)[names(par1989) == "N6"] <- "N7"
names(par1989)[names(par1989) == "N6.mother"] <- "N7.mother"
names(par1989)[names(par1989) == "N6.father"] <- "N7.father"
nei1989 <- merge(nei1989, par1989, by="N7", all.x=TRUE)
#N8
names(par1989)[names(par1989) == "N7"] <- "N8"
names(par1989)[names(par1989) == "N7.mother"] <- "N8.mother"
names(par1989)[names(par1989) == "N7.father"] <- "N8.father"
nei1989 <- merge(nei1989, par1989, by="N8", all.x=TRUE)
#N9
names(par1989)[names(par1989) == "N8"] <- "N9"
names(par1989)[names(par1989) == "N8.mother"] <- "N9.mother"
names(par1989)[names(par1989) == "N8.father"] <- "N9.father"
nei1989 <- merge(nei1989, par1989, by="N9", all.x=TRUE)
#N10
names(par1989)[names(par1989) == "N9"] <- "N10"
names(par1989)[names(par1989) == "N9.mother"] <- "N10.mother"
names(par1989)[names(par1989) == "N9.father"] <- "N10.father"
nei1989 <- merge(nei1989, par1989, by="N10", all.x=TRUE)

#and identifying column 
nei1989$year <- 1989
nei1989a  <-data.frame(nei1989,"box.year.parentid"=paste(nei1989$Focal.box, nei1989$year, "mother",sep="_")) 
nei1989b  <-data.frame(nei1989,"box.year.parentid"=paste(nei1989$Focal.box, nei1989$year, "father",sep="_")) 

nei1989 <- rbind(nei1989a, nei1989b)
rm(nei1989a, nei1989b)

nei1989$N9 <- NA 
nei1989$N10 <- NA

nei1989$N9.mother <- NA
nei1989$N9.father <- NA
nei1989$N10.mother <- NA
nei1989$N10.father <- NA

nei1989 <- nei1989[,order(colnames(nei1989))]

nei_output <- rbind(nei_output, nei1989)


#1990 ####
breeding.data.1990 <- xdata[which(xdata$year == 1990),] 
breeding.data.1990 <- breeding.data.1990[!is.na(breeding.data.1990$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1990 <- sf::st_as_sf(breeding.data.1990, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1990))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1990), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1990))
breeding.ids.1990 <- breeding.data.1990[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1990)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1990 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1990$Focal.male <- with(neighbors.1990, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1990$Focal.female <- with(neighbors.1990, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1990$Neighboring.male <- with(neighbors.1990, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1990$Neighboring.female <- with(neighbors.1990, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1990.b <- as.data.frame(with(neighbors.1990, paste(Focal.male, Neighboring.male, sep="_")))
names(N1990.b)[1] <- "ring_ring"
N1990.c <- as.data.frame(with(neighbors.1990, paste(Focal.male, Neighboring.female, sep="_")))
names(N1990.c)[1] <- "ring_ring"
N1990.d <- as.data.frame(with(neighbors.1990, paste(Focal.female, Neighboring.female, sep="_")))
names(N1990.d)[1] <- "ring_ring"
N1990.e <- as.data.frame(with(neighbors.1990, paste(Focal.female, Neighboring.male, sep="_")))
names(N1990.e)[1] <- "ring_ring"

N1990 <- rbind(N1990.b, N1990.c, N1990.d, N1990.e)
rm(N1990.b, N1990.c, N1990.d, N1990.e)
N1990$neighbors <- TRUE
N1990$Year.s <- 1991

N1990 <- N1990[!grepl("UNKNOWN", N1990$ring_ring),]

N_reference <- rbind(N_reference, N1990)


ydata <- base.fn.data[which(base.fn.data$year==1990),]


library(dplyr)

nei1990 <- neighbors.1990[,c(1,4)]

nei1990 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1990


nei1990 <- tidyr::pivot_wider(nei1990, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1990) <- paste0('N', colnames(nei1990))
names(nei1990)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1990
as.data.frame(colnames(ydata))
par1990 <- ydata[,c(2,19,20)]
par1990 <- distinct(par1990, Box, .keep_all = TRUE)


####add neighbor ids
####1990
#N1
names(par1990)[names(par1990) == "Box"] <- "N1"
names(par1990)[names(par1990) == "Mother"] <- "N1.mother"
names(par1990)[names(par1990) == "Father"] <- "N1.father"
nei1990 <- merge(nei1990, par1990, by="N1", all.x=TRUE)
#N2
names(par1990)[names(par1990) == "N1"] <- "N2"
names(par1990)[names(par1990) == "N1.mother"] <- "N2.mother"
names(par1990)[names(par1990) == "N1.father"] <- "N2.father"
nei1990 <- merge(nei1990, par1990, by="N2", all.x=TRUE)
#N3
names(par1990)[names(par1990) == "N2"] <- "N3"
names(par1990)[names(par1990) == "N2.mother"] <- "N3.mother"
names(par1990)[names(par1990) == "N2.father"] <- "N3.father"
nei1990 <- merge(nei1990, par1990, by="N3", all.x=TRUE)
#N4
names(par1990)[names(par1990) == "N3"] <- "N4"
names(par1990)[names(par1990) == "N3.mother"] <- "N4.mother"
names(par1990)[names(par1990) == "N3.father"] <- "N4.father"
nei1990 <- merge(nei1990, par1990, by="N4", all.x=TRUE)
#N5
names(par1990)[names(par1990) == "N4"] <- "N5"
names(par1990)[names(par1990) == "N4.mother"] <- "N5.mother"
names(par1990)[names(par1990) == "N4.father"] <- "N5.father"
nei1990 <- merge(nei1990, par1990, by="N5", all.x=TRUE)
#N6
names(par1990)[names(par1990) == "N5"] <- "N6"
names(par1990)[names(par1990) == "N5.mother"] <- "N6.mother"
names(par1990)[names(par1990) == "N5.father"] <- "N6.father"
nei1990 <- merge(nei1990, par1990, by="N6", all.x=TRUE)
#N7
names(par1990)[names(par1990) == "N6"] <- "N7"
names(par1990)[names(par1990) == "N6.mother"] <- "N7.mother"
names(par1990)[names(par1990) == "N6.father"] <- "N7.father"
nei1990 <- merge(nei1990, par1990, by="N7", all.x=TRUE)
#N8
names(par1990)[names(par1990) == "N7"] <- "N8"
names(par1990)[names(par1990) == "N7.mother"] <- "N8.mother"
names(par1990)[names(par1990) == "N7.father"] <- "N8.father"
nei1990 <- merge(nei1990, par1990, by="N8", all.x=TRUE)
#N9
names(par1990)[names(par1990) == "N8"] <- "N9"
names(par1990)[names(par1990) == "N8.mother"] <- "N9.mother"
names(par1990)[names(par1990) == "N8.father"] <- "N9.father"
nei1990 <- merge(nei1990, par1990, by="N9", all.x=TRUE)
#N10
names(par1990)[names(par1990) == "N9"] <- "N10"
names(par1990)[names(par1990) == "N9.mother"] <- "N10.mother"
names(par1990)[names(par1990) == "N9.father"] <- "N10.father"
nei1990 <- merge(nei1990, par1990, by="N10", all.x=TRUE)

#and identifying column 
nei1990$year <- 1990
nei1990a  <-data.frame(nei1990,"box.year.parentid"=paste(nei1990$Focal.box, nei1990$year, "mother",sep="_")) 
nei1990b  <-data.frame(nei1990,"box.year.parentid"=paste(nei1990$Focal.box, nei1990$year, "father",sep="_")) 

nei1990 <- rbind(nei1990a, nei1990b)
rm(nei1990a, nei1990b)

nei1990$N9 <- NA 
nei1990$N10 <- NA

nei1990$N9.mother <- NA
nei1990$N9.father <- NA
nei1990$N10.mother <- NA
nei1990$N10.father <- NA

nei1990 <- nei1990[,order(colnames(nei1990))]

nei_output <- rbind(nei_output, nei1990)

#1991 ####
breeding.data.1991 <- xdata[which(xdata$year == 1991),] 
breeding.data.1991 <- breeding.data.1991[!is.na(breeding.data.1991$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1991 <- sf::st_as_sf(breeding.data.1991, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1991))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1991), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1991))
breeding.ids.1991 <- breeding.data.1991[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1991)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1991 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1991$Focal.male <- with(neighbors.1991, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1991$Focal.female <- with(neighbors.1991, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1991$Neighboring.male <- with(neighbors.1991, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1991$Neighboring.female <- with(neighbors.1991, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1991.b <- as.data.frame(with(neighbors.1991, paste(Focal.male, Neighboring.male, sep="_")))
names(N1991.b)[1] <- "ring_ring"
N1991.c <- as.data.frame(with(neighbors.1991, paste(Focal.male, Neighboring.female, sep="_")))
names(N1991.c)[1] <- "ring_ring"
N1991.d <- as.data.frame(with(neighbors.1991, paste(Focal.female, Neighboring.female, sep="_")))
names(N1991.d)[1] <- "ring_ring"
N1991.e <- as.data.frame(with(neighbors.1991, paste(Focal.female, Neighboring.male, sep="_")))
names(N1991.e)[1] <- "ring_ring"

N1991 <- rbind(N1991.b, N1991.c, N1991.d, N1991.e)
rm(N1991.b, N1991.c, N1991.d, N1991.e)
N1991$neighbors <- TRUE
N1991$Year.s <- 1992

N1991 <- N1991[!grepl("UNKNOWN", N1991$ring_ring),]

N_reference <- rbind(N_reference, N1991)


ydata <- base.fn.data[which(base.fn.data$year==1991),]


library(dplyr)

nei1991 <- neighbors.1991[,c(1,4)]

nei1991 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1991


nei1991 <- tidyr::pivot_wider(nei1991, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1991) <- paste0('N', colnames(nei1991))
names(nei1991)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1991
as.data.frame(colnames(ydata))
par1991 <- ydata[,c(2,19,20)]
par1991 <- distinct(par1991, Box, .keep_all = TRUE)


####add neighbor ids
####1991
#N1
names(par1991)[names(par1991) == "Box"] <- "N1"
names(par1991)[names(par1991) == "Mother"] <- "N1.mother"
names(par1991)[names(par1991) == "Father"] <- "N1.father"
nei1991 <- merge(nei1991, par1991, by="N1", all.x=TRUE)
#N2
names(par1991)[names(par1991) == "N1"] <- "N2"
names(par1991)[names(par1991) == "N1.mother"] <- "N2.mother"
names(par1991)[names(par1991) == "N1.father"] <- "N2.father"
nei1991 <- merge(nei1991, par1991, by="N2", all.x=TRUE)
#N3
names(par1991)[names(par1991) == "N2"] <- "N3"
names(par1991)[names(par1991) == "N2.mother"] <- "N3.mother"
names(par1991)[names(par1991) == "N2.father"] <- "N3.father"
nei1991 <- merge(nei1991, par1991, by="N3", all.x=TRUE)
#N4
names(par1991)[names(par1991) == "N3"] <- "N4"
names(par1991)[names(par1991) == "N3.mother"] <- "N4.mother"
names(par1991)[names(par1991) == "N3.father"] <- "N4.father"
nei1991 <- merge(nei1991, par1991, by="N4", all.x=TRUE)
#N5
names(par1991)[names(par1991) == "N4"] <- "N5"
names(par1991)[names(par1991) == "N4.mother"] <- "N5.mother"
names(par1991)[names(par1991) == "N4.father"] <- "N5.father"
nei1991 <- merge(nei1991, par1991, by="N5", all.x=TRUE)
#N6
names(par1991)[names(par1991) == "N5"] <- "N6"
names(par1991)[names(par1991) == "N5.mother"] <- "N6.mother"
names(par1991)[names(par1991) == "N5.father"] <- "N6.father"
nei1991 <- merge(nei1991, par1991, by="N6", all.x=TRUE)
#N7
names(par1991)[names(par1991) == "N6"] <- "N7"
names(par1991)[names(par1991) == "N6.mother"] <- "N7.mother"
names(par1991)[names(par1991) == "N6.father"] <- "N7.father"
nei1991 <- merge(nei1991, par1991, by="N7", all.x=TRUE)
#N8
names(par1991)[names(par1991) == "N7"] <- "N8"
names(par1991)[names(par1991) == "N7.mother"] <- "N8.mother"
names(par1991)[names(par1991) == "N7.father"] <- "N8.father"
nei1991 <- merge(nei1991, par1991, by="N8", all.x=TRUE)
#N9
names(par1991)[names(par1991) == "N8"] <- "N9"
names(par1991)[names(par1991) == "N8.mother"] <- "N9.mother"
names(par1991)[names(par1991) == "N8.father"] <- "N9.father"
nei1991 <- merge(nei1991, par1991, by="N9", all.x=TRUE)
#N10
names(par1991)[names(par1991) == "N9"] <- "N10"
names(par1991)[names(par1991) == "N9.mother"] <- "N10.mother"
names(par1991)[names(par1991) == "N9.father"] <- "N10.father"
nei1991 <- merge(nei1991, par1991, by="N10", all.x=TRUE)

#and identifying column 
nei1991$year <- 1991
nei1991a  <-data.frame(nei1991,"box.year.parentid"=paste(nei1991$Focal.box, nei1991$year, "mother",sep="_")) 
nei1991b  <-data.frame(nei1991,"box.year.parentid"=paste(nei1991$Focal.box, nei1991$year, "father",sep="_")) 

nei1991 <- rbind(nei1991a, nei1991b)
rm(nei1991a, nei1991b)

nei1991$N9 <- NA 
nei1991$N10 <- NA

nei1991$N9.mother <- NA
nei1991$N9.father <- NA
nei1991$N10.mother <- NA
nei1991$N10.father <- NA

nei1991 <- nei1991[,order(colnames(nei1991))]

nei_output <- rbind(nei_output, nei1991)

#1992 ####
breeding.data.1992 <- xdata[which(xdata$year == 1992),] 
breeding.data.1992 <- breeding.data.1992[!is.na(breeding.data.1992$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1992 <- sf::st_as_sf(breeding.data.1992, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1992))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1992), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1992))
breeding.ids.1992 <- breeding.data.1992[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1992)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1992 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1992$Focal.male <- with(neighbors.1992, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1992$Focal.female <- with(neighbors.1992, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1992$Neighboring.male <- with(neighbors.1992, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1992$Neighboring.female <- with(neighbors.1992, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1992.b <- as.data.frame(with(neighbors.1992, paste(Focal.male, Neighboring.male, sep="_")))
names(N1992.b)[1] <- "ring_ring"
N1992.c <- as.data.frame(with(neighbors.1992, paste(Focal.male, Neighboring.female, sep="_")))
names(N1992.c)[1] <- "ring_ring"
N1992.d <- as.data.frame(with(neighbors.1992, paste(Focal.female, Neighboring.female, sep="_")))
names(N1992.d)[1] <- "ring_ring"
N1992.e <- as.data.frame(with(neighbors.1992, paste(Focal.female, Neighboring.male, sep="_")))
names(N1992.e)[1] <- "ring_ring"

N1992 <- rbind(N1992.b, N1992.c, N1992.d, N1992.e)
rm(N1992.b, N1992.c, N1992.d, N1992.e)
N1992$neighbors <- TRUE
N1992$Year.s <- 1993

N1992 <- N1992[!grepl("UNKNOWN", N1992$ring_ring),]

N_reference <- rbind(N_reference, N1992)


ydata <- base.fn.data[which(base.fn.data$year==1992),]


library(dplyr)

nei1992 <- neighbors.1992[,c(1,4)]

nei1992 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1992


nei1992 <- tidyr::pivot_wider(nei1992, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1992) <- paste0('N', colnames(nei1992))
names(nei1992)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1992
as.data.frame(colnames(ydata))
par1992 <- ydata[,c(2,19,20)]
par1992 <- distinct(par1992, Box, .keep_all = TRUE)


####add neighbor ids
####1992
#N1
names(par1992)[names(par1992) == "Box"] <- "N1"
names(par1992)[names(par1992) == "Mother"] <- "N1.mother"
names(par1992)[names(par1992) == "Father"] <- "N1.father"
nei1992 <- merge(nei1992, par1992, by="N1", all.x=TRUE)
#N2
names(par1992)[names(par1992) == "N1"] <- "N2"
names(par1992)[names(par1992) == "N1.mother"] <- "N2.mother"
names(par1992)[names(par1992) == "N1.father"] <- "N2.father"
nei1992 <- merge(nei1992, par1992, by="N2", all.x=TRUE)
#N3
names(par1992)[names(par1992) == "N2"] <- "N3"
names(par1992)[names(par1992) == "N2.mother"] <- "N3.mother"
names(par1992)[names(par1992) == "N2.father"] <- "N3.father"
nei1992 <- merge(nei1992, par1992, by="N3", all.x=TRUE)
#N4
names(par1992)[names(par1992) == "N3"] <- "N4"
names(par1992)[names(par1992) == "N3.mother"] <- "N4.mother"
names(par1992)[names(par1992) == "N3.father"] <- "N4.father"
nei1992 <- merge(nei1992, par1992, by="N4", all.x=TRUE)
#N5
names(par1992)[names(par1992) == "N4"] <- "N5"
names(par1992)[names(par1992) == "N4.mother"] <- "N5.mother"
names(par1992)[names(par1992) == "N4.father"] <- "N5.father"
nei1992 <- merge(nei1992, par1992, by="N5", all.x=TRUE)
#N6
names(par1992)[names(par1992) == "N5"] <- "N6"
names(par1992)[names(par1992) == "N5.mother"] <- "N6.mother"
names(par1992)[names(par1992) == "N5.father"] <- "N6.father"
nei1992 <- merge(nei1992, par1992, by="N6", all.x=TRUE)
#N7
names(par1992)[names(par1992) == "N6"] <- "N7"
names(par1992)[names(par1992) == "N6.mother"] <- "N7.mother"
names(par1992)[names(par1992) == "N6.father"] <- "N7.father"
nei1992 <- merge(nei1992, par1992, by="N7", all.x=TRUE)
#N8
names(par1992)[names(par1992) == "N7"] <- "N8"
names(par1992)[names(par1992) == "N7.mother"] <- "N8.mother"
names(par1992)[names(par1992) == "N7.father"] <- "N8.father"
nei1992 <- merge(nei1992, par1992, by="N8", all.x=TRUE)
#N9
names(par1992)[names(par1992) == "N8"] <- "N9"
names(par1992)[names(par1992) == "N8.mother"] <- "N9.mother"
names(par1992)[names(par1992) == "N8.father"] <- "N9.father"
nei1992 <- merge(nei1992, par1992, by="N9", all.x=TRUE)
#N10
names(par1992)[names(par1992) == "N9"] <- "N10"
names(par1992)[names(par1992) == "N9.mother"] <- "N10.mother"
names(par1992)[names(par1992) == "N9.father"] <- "N10.father"
nei1992 <- merge(nei1992, par1992, by="N10", all.x=TRUE)

#and identifying column 
nei1992$year <- 1992
nei1992a  <-data.frame(nei1992,"box.year.parentid"=paste(nei1992$Focal.box, nei1992$year, "mother",sep="_")) 
nei1992b  <-data.frame(nei1992,"box.year.parentid"=paste(nei1992$Focal.box, nei1992$year, "father",sep="_")) 

nei1992 <- rbind(nei1992a, nei1992b)
rm(nei1992a, nei1992b)

nei1992 <- nei1992[,order(colnames(nei1992))]

nei_output <- rbind(nei_output, nei1992)

#1993 ####
breeding.data.1993 <- xdata[which(xdata$year == 1993),] 
breeding.data.1993 <- breeding.data.1993[!is.na(breeding.data.1993$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1993 <- sf::st_as_sf(breeding.data.1993, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1993))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1993), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1993))
breeding.ids.1993 <- breeding.data.1993[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1993)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1993 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1993$Focal.male <- with(neighbors.1993, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1993$Focal.female <- with(neighbors.1993, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1993$Neighboring.male <- with(neighbors.1993, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1993$Neighboring.female <- with(neighbors.1993, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1993.b <- as.data.frame(with(neighbors.1993, paste(Focal.male, Neighboring.male, sep="_")))
names(N1993.b)[1] <- "ring_ring"
N1993.c <- as.data.frame(with(neighbors.1993, paste(Focal.male, Neighboring.female, sep="_")))
names(N1993.c)[1] <- "ring_ring"
N1993.d <- as.data.frame(with(neighbors.1993, paste(Focal.female, Neighboring.female, sep="_")))
names(N1993.d)[1] <- "ring_ring"
N1993.e <- as.data.frame(with(neighbors.1993, paste(Focal.female, Neighboring.male, sep="_")))
names(N1993.e)[1] <- "ring_ring"

N1993 <- rbind(N1993.b, N1993.c, N1993.d, N1993.e)
rm(N1993.b, N1993.c, N1993.d, N1993.e)
N1993$neighbors <- TRUE
N1993$Year.s <- 1994

N1993 <- N1993[!grepl("UNKNOWN", N1993$ring_ring),]

N_reference <- rbind(N_reference, N1993)


ydata <- base.fn.data[which(base.fn.data$year==1993),]


library(dplyr)

nei1993 <- neighbors.1993[,c(1,4)]

nei1993 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1993


nei1993 <- tidyr::pivot_wider(nei1993, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1993) <- paste0('N', colnames(nei1993))
names(nei1993)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1993
as.data.frame(colnames(ydata))
par1993 <- ydata[,c(2,19,20)]
par1993 <- distinct(par1993, Box, .keep_all = TRUE)

nei1993$N11 <- NULL 
####add neighbor ids
####1993
#N1
names(par1993)[names(par1993) == "Box"] <- "N1"
names(par1993)[names(par1993) == "Mother"] <- "N1.mother"
names(par1993)[names(par1993) == "Father"] <- "N1.father"
nei1993 <- merge(nei1993, par1993, by="N1", all.x=TRUE)
#N2
names(par1993)[names(par1993) == "N1"] <- "N2"
names(par1993)[names(par1993) == "N1.mother"] <- "N2.mother"
names(par1993)[names(par1993) == "N1.father"] <- "N2.father"
nei1993 <- merge(nei1993, par1993, by="N2", all.x=TRUE)
#N3
names(par1993)[names(par1993) == "N2"] <- "N3"
names(par1993)[names(par1993) == "N2.mother"] <- "N3.mother"
names(par1993)[names(par1993) == "N2.father"] <- "N3.father"
nei1993 <- merge(nei1993, par1993, by="N3", all.x=TRUE)
#N4
names(par1993)[names(par1993) == "N3"] <- "N4"
names(par1993)[names(par1993) == "N3.mother"] <- "N4.mother"
names(par1993)[names(par1993) == "N3.father"] <- "N4.father"
nei1993 <- merge(nei1993, par1993, by="N4", all.x=TRUE)
#N5
names(par1993)[names(par1993) == "N4"] <- "N5"
names(par1993)[names(par1993) == "N4.mother"] <- "N5.mother"
names(par1993)[names(par1993) == "N4.father"] <- "N5.father"
nei1993 <- merge(nei1993, par1993, by="N5", all.x=TRUE)
#N6
names(par1993)[names(par1993) == "N5"] <- "N6"
names(par1993)[names(par1993) == "N5.mother"] <- "N6.mother"
names(par1993)[names(par1993) == "N5.father"] <- "N6.father"
nei1993 <- merge(nei1993, par1993, by="N6", all.x=TRUE)
#N7
names(par1993)[names(par1993) == "N6"] <- "N7"
names(par1993)[names(par1993) == "N6.mother"] <- "N7.mother"
names(par1993)[names(par1993) == "N6.father"] <- "N7.father"
nei1993 <- merge(nei1993, par1993, by="N7", all.x=TRUE)
#N8
names(par1993)[names(par1993) == "N7"] <- "N8"
names(par1993)[names(par1993) == "N7.mother"] <- "N8.mother"
names(par1993)[names(par1993) == "N7.father"] <- "N8.father"
nei1993 <- merge(nei1993, par1993, by="N8", all.x=TRUE)
#N9
names(par1993)[names(par1993) == "N8"] <- "N9"
names(par1993)[names(par1993) == "N8.mother"] <- "N9.mother"
names(par1993)[names(par1993) == "N8.father"] <- "N9.father"
nei1993 <- merge(nei1993, par1993, by="N9", all.x=TRUE)
#N10
names(par1993)[names(par1993) == "N9"] <- "N10"
names(par1993)[names(par1993) == "N9.mother"] <- "N10.mother"
names(par1993)[names(par1993) == "N9.father"] <- "N10.father"
nei1993 <- merge(nei1993, par1993, by="N10", all.x=TRUE)

#and identifying column 
nei1993$year <- 1993
nei1993a  <-data.frame(nei1993,"box.year.parentid"=paste(nei1993$Focal.box, nei1993$year, "mother",sep="_")) 
nei1993b  <-data.frame(nei1993,"box.year.parentid"=paste(nei1993$Focal.box, nei1993$year, "father",sep="_")) 

nei1993 <- rbind(nei1993a, nei1993b)
rm(nei1993a, nei1993b)

nei1993 <- nei1993[,order(colnames(nei1993))]

nei_output <- rbind(nei_output, nei1993)


#1994 ####
breeding.data.1994 <- xdata[which(xdata$year == 1994),] 
breeding.data.1994 <- breeding.data.1994[!is.na(breeding.data.1994$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1994 <- sf::st_as_sf(breeding.data.1994, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1994))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1994), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1994))
breeding.ids.1994 <- breeding.data.1994[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1994)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1994 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1994$Focal.male <- with(neighbors.1994, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1994$Focal.female <- with(neighbors.1994, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1994$Neighboring.male <- with(neighbors.1994, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1994$Neighboring.female <- with(neighbors.1994, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1994.b <- as.data.frame(with(neighbors.1994, paste(Focal.male, Neighboring.male, sep="_")))
names(N1994.b)[1] <- "ring_ring"
N1994.c <- as.data.frame(with(neighbors.1994, paste(Focal.male, Neighboring.female, sep="_")))
names(N1994.c)[1] <- "ring_ring"
N1994.d <- as.data.frame(with(neighbors.1994, paste(Focal.female, Neighboring.female, sep="_")))
names(N1994.d)[1] <- "ring_ring"
N1994.e <- as.data.frame(with(neighbors.1994, paste(Focal.female, Neighboring.male, sep="_")))
names(N1994.e)[1] <- "ring_ring"

N1994 <- rbind(N1994.b, N1994.c, N1994.d, N1994.e)
rm(N1994.b, N1994.c, N1994.d, N1994.e)
N1994$neighbors <- TRUE
N1994$Year.s <- 1995

N1994 <- N1994[!grepl("UNKNOWN", N1994$ring_ring),]

N_reference <- rbind(N_reference, N1994)


ydata <- base.fn.data[which(base.fn.data$year==1994),]


library(dplyr)

nei1994 <- neighbors.1994[,c(1,4)]

nei1994 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1994


nei1994 <- tidyr::pivot_wider(nei1994, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1994) <- paste0('N', colnames(nei1994))
names(nei1994)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1994
as.data.frame(colnames(ydata))
par1994 <- ydata[,c(2,19,20)]
par1994 <- distinct(par1994, Box, .keep_all = TRUE)


####add neighbor ids
####1994
#N1
names(par1994)[names(par1994) == "Box"] <- "N1"
names(par1994)[names(par1994) == "Mother"] <- "N1.mother"
names(par1994)[names(par1994) == "Father"] <- "N1.father"
nei1994 <- merge(nei1994, par1994, by="N1", all.x=TRUE)
#N2
names(par1994)[names(par1994) == "N1"] <- "N2"
names(par1994)[names(par1994) == "N1.mother"] <- "N2.mother"
names(par1994)[names(par1994) == "N1.father"] <- "N2.father"
nei1994 <- merge(nei1994, par1994, by="N2", all.x=TRUE)
#N3
names(par1994)[names(par1994) == "N2"] <- "N3"
names(par1994)[names(par1994) == "N2.mother"] <- "N3.mother"
names(par1994)[names(par1994) == "N2.father"] <- "N3.father"
nei1994 <- merge(nei1994, par1994, by="N3", all.x=TRUE)
#N4
names(par1994)[names(par1994) == "N3"] <- "N4"
names(par1994)[names(par1994) == "N3.mother"] <- "N4.mother"
names(par1994)[names(par1994) == "N3.father"] <- "N4.father"
nei1994 <- merge(nei1994, par1994, by="N4", all.x=TRUE)
#N5
names(par1994)[names(par1994) == "N4"] <- "N5"
names(par1994)[names(par1994) == "N4.mother"] <- "N5.mother"
names(par1994)[names(par1994) == "N4.father"] <- "N5.father"
nei1994 <- merge(nei1994, par1994, by="N5", all.x=TRUE)
#N6
names(par1994)[names(par1994) == "N5"] <- "N6"
names(par1994)[names(par1994) == "N5.mother"] <- "N6.mother"
names(par1994)[names(par1994) == "N5.father"] <- "N6.father"
nei1994 <- merge(nei1994, par1994, by="N6", all.x=TRUE)
#N7
names(par1994)[names(par1994) == "N6"] <- "N7"
names(par1994)[names(par1994) == "N6.mother"] <- "N7.mother"
names(par1994)[names(par1994) == "N6.father"] <- "N7.father"
nei1994 <- merge(nei1994, par1994, by="N7", all.x=TRUE)
#N8
names(par1994)[names(par1994) == "N7"] <- "N8"
names(par1994)[names(par1994) == "N7.mother"] <- "N8.mother"
names(par1994)[names(par1994) == "N7.father"] <- "N8.father"
nei1994 <- merge(nei1994, par1994, by="N8", all.x=TRUE)
#N9
names(par1994)[names(par1994) == "N8"] <- "N9"
names(par1994)[names(par1994) == "N8.mother"] <- "N9.mother"
names(par1994)[names(par1994) == "N8.father"] <- "N9.father"
nei1994 <- merge(nei1994, par1994, by="N9", all.x=TRUE)
#N10
names(par1994)[names(par1994) == "N9"] <- "N10"
names(par1994)[names(par1994) == "N9.mother"] <- "N10.mother"
names(par1994)[names(par1994) == "N9.father"] <- "N10.father"
nei1994 <- merge(nei1994, par1994, by="N10", all.x=TRUE)

#and identifying column 
nei1994$year <- 1994
nei1994a  <-data.frame(nei1994,"box.year.parentid"=paste(nei1994$Focal.box, nei1994$year, "mother",sep="_")) 
nei1994b  <-data.frame(nei1994,"box.year.parentid"=paste(nei1994$Focal.box, nei1994$year, "father",sep="_")) 

nei1994 <- rbind(nei1994a, nei1994b)
rm(nei1994a, nei1994b)

nei1994 <- nei1994[,order(colnames(nei1994))]

nei_output <- rbind(nei_output, nei1994)

#1995 ####
breeding.data.1995 <- xdata[which(xdata$year == 1995),] 
breeding.data.1995 <- breeding.data.1995[!is.na(breeding.data.1995$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1995 <- sf::st_as_sf(breeding.data.1995, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1995))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1995), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1995))
breeding.ids.1995 <- breeding.data.1995[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1995)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1995 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1995$Focal.male <- with(neighbors.1995, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1995$Focal.female <- with(neighbors.1995, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1995$Neighboring.male <- with(neighbors.1995, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1995$Neighboring.female <- with(neighbors.1995, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1995.b <- as.data.frame(with(neighbors.1995, paste(Focal.male, Neighboring.male, sep="_")))
names(N1995.b)[1] <- "ring_ring"
N1995.c <- as.data.frame(with(neighbors.1995, paste(Focal.male, Neighboring.female, sep="_")))
names(N1995.c)[1] <- "ring_ring"
N1995.d <- as.data.frame(with(neighbors.1995, paste(Focal.female, Neighboring.female, sep="_")))
names(N1995.d)[1] <- "ring_ring"
N1995.e <- as.data.frame(with(neighbors.1995, paste(Focal.female, Neighboring.male, sep="_")))
names(N1995.e)[1] <- "ring_ring"

N1995 <- rbind(N1995.b, N1995.c, N1995.d, N1995.e)
rm(N1995.b, N1995.c, N1995.d, N1995.e)
N1995$neighbors <- TRUE
N1995$Year.s <- 1996

N1995 <- N1995[!grepl("UNKNOWN", N1995$ring_ring),]

N_reference <- rbind(N_reference, N1995)


ydata <- base.fn.data[which(base.fn.data$year==1995),]


library(dplyr)

nei1995 <- neighbors.1995[,c(1,4)]

nei1995 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1995


nei1995 <- tidyr::pivot_wider(nei1995, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1995) <- paste0('N', colnames(nei1995))
names(nei1995)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1995
as.data.frame(colnames(ydata))
par1995 <- ydata[,c(2,19,20)]
par1995 <- distinct(par1995, Box, .keep_all = TRUE)


####add neighbor ids
####1995
#N1
names(par1995)[names(par1995) == "Box"] <- "N1"
names(par1995)[names(par1995) == "Mother"] <- "N1.mother"
names(par1995)[names(par1995) == "Father"] <- "N1.father"
nei1995 <- merge(nei1995, par1995, by="N1", all.x=TRUE)
#N2
names(par1995)[names(par1995) == "N1"] <- "N2"
names(par1995)[names(par1995) == "N1.mother"] <- "N2.mother"
names(par1995)[names(par1995) == "N1.father"] <- "N2.father"
nei1995 <- merge(nei1995, par1995, by="N2", all.x=TRUE)
#N3
names(par1995)[names(par1995) == "N2"] <- "N3"
names(par1995)[names(par1995) == "N2.mother"] <- "N3.mother"
names(par1995)[names(par1995) == "N2.father"] <- "N3.father"
nei1995 <- merge(nei1995, par1995, by="N3", all.x=TRUE)
#N4
names(par1995)[names(par1995) == "N3"] <- "N4"
names(par1995)[names(par1995) == "N3.mother"] <- "N4.mother"
names(par1995)[names(par1995) == "N3.father"] <- "N4.father"
nei1995 <- merge(nei1995, par1995, by="N4", all.x=TRUE)
#N5
names(par1995)[names(par1995) == "N4"] <- "N5"
names(par1995)[names(par1995) == "N4.mother"] <- "N5.mother"
names(par1995)[names(par1995) == "N4.father"] <- "N5.father"
nei1995 <- merge(nei1995, par1995, by="N5", all.x=TRUE)
#N6
names(par1995)[names(par1995) == "N5"] <- "N6"
names(par1995)[names(par1995) == "N5.mother"] <- "N6.mother"
names(par1995)[names(par1995) == "N5.father"] <- "N6.father"
nei1995 <- merge(nei1995, par1995, by="N6", all.x=TRUE)
#N7
names(par1995)[names(par1995) == "N6"] <- "N7"
names(par1995)[names(par1995) == "N6.mother"] <- "N7.mother"
names(par1995)[names(par1995) == "N6.father"] <- "N7.father"
nei1995 <- merge(nei1995, par1995, by="N7", all.x=TRUE)
#N8
names(par1995)[names(par1995) == "N7"] <- "N8"
names(par1995)[names(par1995) == "N7.mother"] <- "N8.mother"
names(par1995)[names(par1995) == "N7.father"] <- "N8.father"
nei1995 <- merge(nei1995, par1995, by="N8", all.x=TRUE)
#N9
names(par1995)[names(par1995) == "N8"] <- "N9"
names(par1995)[names(par1995) == "N8.mother"] <- "N9.mother"
names(par1995)[names(par1995) == "N8.father"] <- "N9.father"
nei1995 <- merge(nei1995, par1995, by="N9", all.x=TRUE)
#N10
names(par1995)[names(par1995) == "N9"] <- "N10"
names(par1995)[names(par1995) == "N9.mother"] <- "N10.mother"
names(par1995)[names(par1995) == "N9.father"] <- "N10.father"
nei1995 <- merge(nei1995, par1995, by="N10", all.x=TRUE)

#and identifying column 
nei1995$year <- 1995
nei1995a  <-data.frame(nei1995,"box.year.parentid"=paste(nei1995$Focal.box, nei1995$year, "mother",sep="_")) 
nei1995b  <-data.frame(nei1995,"box.year.parentid"=paste(nei1995$Focal.box, nei1995$year, "father",sep="_")) 

nei1995 <- rbind(nei1995a, nei1995b)
rm(nei1995a, nei1995b)

nei1995$N9 <- NA 
nei1995$N10 <- NA

nei1995$N9.mother <- NA
nei1995$N9.father <- NA
nei1995$N10.mother <- NA
nei1995$N10.father <- NA

nei1995 <- nei1995[,order(colnames(nei1995))]

nei_output <- rbind(nei_output, nei1995)


#1996 ####
breeding.data.1996 <- xdata[which(xdata$year == 1996),] 
breeding.data.1996 <- breeding.data.1996[!is.na(breeding.data.1996$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1996 <- sf::st_as_sf(breeding.data.1996, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1996))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1996), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1996))
breeding.ids.1996 <- breeding.data.1996[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1996)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1996 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1996$Focal.male <- with(neighbors.1996, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1996$Focal.female <- with(neighbors.1996, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1996$Neighboring.male <- with(neighbors.1996, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1996$Neighboring.female <- with(neighbors.1996, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1996.b <- as.data.frame(with(neighbors.1996, paste(Focal.male, Neighboring.male, sep="_")))
names(N1996.b)[1] <- "ring_ring"
N1996.c <- as.data.frame(with(neighbors.1996, paste(Focal.male, Neighboring.female, sep="_")))
names(N1996.c)[1] <- "ring_ring"
N1996.d <- as.data.frame(with(neighbors.1996, paste(Focal.female, Neighboring.female, sep="_")))
names(N1996.d)[1] <- "ring_ring"
N1996.e <- as.data.frame(with(neighbors.1996, paste(Focal.female, Neighboring.male, sep="_")))
names(N1996.e)[1] <- "ring_ring"

N1996 <- rbind(N1996.b, N1996.c, N1996.d, N1996.e)
rm(N1996.b, N1996.c, N1996.d, N1996.e)
N1996$neighbors <- TRUE
N1996$Year.s <- 1997

N1996 <- N1996[!grepl("UNKNOWN", N1996$ring_ring),]

N_reference <- rbind(N_reference, N1996)


ydata <- base.fn.data[which(base.fn.data$year==1996),]


library(dplyr)

nei1996 <- neighbors.1996[,c(1,4)]

nei1996 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1996


nei1996 <- tidyr::pivot_wider(nei1996, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1996) <- paste0('N', colnames(nei1996))
names(nei1996)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1996
as.data.frame(colnames(ydata))
par1996 <- ydata[,c(2,19,20)]
par1996 <- distinct(par1996, Box, .keep_all = TRUE)


####add neighbor ids
####1996
#N1
names(par1996)[names(par1996) == "Box"] <- "N1"
names(par1996)[names(par1996) == "Mother"] <- "N1.mother"
names(par1996)[names(par1996) == "Father"] <- "N1.father"
nei1996 <- merge(nei1996, par1996, by="N1", all.x=TRUE)
#N2
names(par1996)[names(par1996) == "N1"] <- "N2"
names(par1996)[names(par1996) == "N1.mother"] <- "N2.mother"
names(par1996)[names(par1996) == "N1.father"] <- "N2.father"
nei1996 <- merge(nei1996, par1996, by="N2", all.x=TRUE)
#N3
names(par1996)[names(par1996) == "N2"] <- "N3"
names(par1996)[names(par1996) == "N2.mother"] <- "N3.mother"
names(par1996)[names(par1996) == "N2.father"] <- "N3.father"
nei1996 <- merge(nei1996, par1996, by="N3", all.x=TRUE)
#N4
names(par1996)[names(par1996) == "N3"] <- "N4"
names(par1996)[names(par1996) == "N3.mother"] <- "N4.mother"
names(par1996)[names(par1996) == "N3.father"] <- "N4.father"
nei1996 <- merge(nei1996, par1996, by="N4", all.x=TRUE)
#N5
names(par1996)[names(par1996) == "N4"] <- "N5"
names(par1996)[names(par1996) == "N4.mother"] <- "N5.mother"
names(par1996)[names(par1996) == "N4.father"] <- "N5.father"
nei1996 <- merge(nei1996, par1996, by="N5", all.x=TRUE)
#N6
names(par1996)[names(par1996) == "N5"] <- "N6"
names(par1996)[names(par1996) == "N5.mother"] <- "N6.mother"
names(par1996)[names(par1996) == "N5.father"] <- "N6.father"
nei1996 <- merge(nei1996, par1996, by="N6", all.x=TRUE)
#N7
names(par1996)[names(par1996) == "N6"] <- "N7"
names(par1996)[names(par1996) == "N6.mother"] <- "N7.mother"
names(par1996)[names(par1996) == "N6.father"] <- "N7.father"
nei1996 <- merge(nei1996, par1996, by="N7", all.x=TRUE)
#N8
names(par1996)[names(par1996) == "N7"] <- "N8"
names(par1996)[names(par1996) == "N7.mother"] <- "N8.mother"
names(par1996)[names(par1996) == "N7.father"] <- "N8.father"
nei1996 <- merge(nei1996, par1996, by="N8", all.x=TRUE)
#N9
names(par1996)[names(par1996) == "N8"] <- "N9"
names(par1996)[names(par1996) == "N8.mother"] <- "N9.mother"
names(par1996)[names(par1996) == "N8.father"] <- "N9.father"
nei1996 <- merge(nei1996, par1996, by="N9", all.x=TRUE)
#N10
names(par1996)[names(par1996) == "N9"] <- "N10"
names(par1996)[names(par1996) == "N9.mother"] <- "N10.mother"
names(par1996)[names(par1996) == "N9.father"] <- "N10.father"
nei1996 <- merge(nei1996, par1996, by="N10", all.x=TRUE)

#and identifying column 
nei1996$year <- 1996
nei1996a  <-data.frame(nei1996,"box.year.parentid"=paste(nei1996$Focal.box, nei1996$year, "mother",sep="_")) 
nei1996b  <-data.frame(nei1996,"box.year.parentid"=paste(nei1996$Focal.box, nei1996$year, "father",sep="_")) 

nei1996 <- rbind(nei1996a, nei1996b)
rm(nei1996a, nei1996b)

nei1996$N10 <- NA

nei1996$N10.mother <- NA
nei1996$N10.father <- NA

nei1996 <- nei1996[,order(colnames(nei1996))]

nei_output <- rbind(nei_output, nei1996)

#1997 ####
breeding.data.1997 <- xdata[which(xdata$year == 1997),] 
breeding.data.1997 <- breeding.data.1997[!is.na(breeding.data.1997$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1997 <- sf::st_as_sf(breeding.data.1997, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1997))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1997), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1997))
breeding.ids.1997 <- breeding.data.1997[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1997)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1997 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1997$Focal.male <- with(neighbors.1997, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1997$Focal.female <- with(neighbors.1997, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1997$Neighboring.male <- with(neighbors.1997, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1997$Neighboring.female <- with(neighbors.1997, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1997.b <- as.data.frame(with(neighbors.1997, paste(Focal.male, Neighboring.male, sep="_")))
names(N1997.b)[1] <- "ring_ring"
N1997.c <- as.data.frame(with(neighbors.1997, paste(Focal.male, Neighboring.female, sep="_")))
names(N1997.c)[1] <- "ring_ring"
N1997.d <- as.data.frame(with(neighbors.1997, paste(Focal.female, Neighboring.female, sep="_")))
names(N1997.d)[1] <- "ring_ring"
N1997.e <- as.data.frame(with(neighbors.1997, paste(Focal.female, Neighboring.male, sep="_")))
names(N1997.e)[1] <- "ring_ring"

N1997 <- rbind(N1997.b, N1997.c, N1997.d, N1997.e)
rm(N1997.b, N1997.c, N1997.d, N1997.e)
N1997$neighbors <- TRUE
N1997$Year.s <- 1998

N1997 <- N1997[!grepl("UNKNOWN", N1997$ring_ring),]

N_reference <- rbind(N_reference, N1997)


ydata <- base.fn.data[which(base.fn.data$year==1997),]


library(dplyr)

nei1997 <- neighbors.1997[,c(1,4)]

nei1997 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1997


nei1997 <- tidyr::pivot_wider(nei1997, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1997) <- paste0('N', colnames(nei1997))
names(nei1997)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1997
as.data.frame(colnames(ydata))
par1997 <- ydata[,c(2,19,20)]
par1997 <- distinct(par1997, Box, .keep_all = TRUE)


####add neighbor ids
####1997
#N1
names(par1997)[names(par1997) == "Box"] <- "N1"
names(par1997)[names(par1997) == "Mother"] <- "N1.mother"
names(par1997)[names(par1997) == "Father"] <- "N1.father"
nei1997 <- merge(nei1997, par1997, by="N1", all.x=TRUE)
#N2
names(par1997)[names(par1997) == "N1"] <- "N2"
names(par1997)[names(par1997) == "N1.mother"] <- "N2.mother"
names(par1997)[names(par1997) == "N1.father"] <- "N2.father"
nei1997 <- merge(nei1997, par1997, by="N2", all.x=TRUE)
#N3
names(par1997)[names(par1997) == "N2"] <- "N3"
names(par1997)[names(par1997) == "N2.mother"] <- "N3.mother"
names(par1997)[names(par1997) == "N2.father"] <- "N3.father"
nei1997 <- merge(nei1997, par1997, by="N3", all.x=TRUE)
#N4
names(par1997)[names(par1997) == "N3"] <- "N4"
names(par1997)[names(par1997) == "N3.mother"] <- "N4.mother"
names(par1997)[names(par1997) == "N3.father"] <- "N4.father"
nei1997 <- merge(nei1997, par1997, by="N4", all.x=TRUE)
#N5
names(par1997)[names(par1997) == "N4"] <- "N5"
names(par1997)[names(par1997) == "N4.mother"] <- "N5.mother"
names(par1997)[names(par1997) == "N4.father"] <- "N5.father"
nei1997 <- merge(nei1997, par1997, by="N5", all.x=TRUE)
#N6
names(par1997)[names(par1997) == "N5"] <- "N6"
names(par1997)[names(par1997) == "N5.mother"] <- "N6.mother"
names(par1997)[names(par1997) == "N5.father"] <- "N6.father"
nei1997 <- merge(nei1997, par1997, by="N6", all.x=TRUE)
#N7
names(par1997)[names(par1997) == "N6"] <- "N7"
names(par1997)[names(par1997) == "N6.mother"] <- "N7.mother"
names(par1997)[names(par1997) == "N6.father"] <- "N7.father"
nei1997 <- merge(nei1997, par1997, by="N7", all.x=TRUE)
#N8
names(par1997)[names(par1997) == "N7"] <- "N8"
names(par1997)[names(par1997) == "N7.mother"] <- "N8.mother"
names(par1997)[names(par1997) == "N7.father"] <- "N8.father"
nei1997 <- merge(nei1997, par1997, by="N8", all.x=TRUE)
#N9
names(par1997)[names(par1997) == "N8"] <- "N9"
names(par1997)[names(par1997) == "N8.mother"] <- "N9.mother"
names(par1997)[names(par1997) == "N8.father"] <- "N9.father"
nei1997 <- merge(nei1997, par1997, by="N9", all.x=TRUE)
#N10
names(par1997)[names(par1997) == "N9"] <- "N10"
names(par1997)[names(par1997) == "N9.mother"] <- "N10.mother"
names(par1997)[names(par1997) == "N9.father"] <- "N10.father"
nei1997 <- merge(nei1997, par1997, by="N10", all.x=TRUE)

#and identifying column 
nei1997$year <- 1997
nei1997a  <-data.frame(nei1997,"box.year.parentid"=paste(nei1997$Focal.box, nei1997$year, "mother",sep="_")) 
nei1997b  <-data.frame(nei1997,"box.year.parentid"=paste(nei1997$Focal.box, nei1997$year, "father",sep="_")) 

nei1997 <- rbind(nei1997a, nei1997b)
rm(nei1997a, nei1997b)

nei1997$N10 <- NA

nei1997$N10.mother <- NA
nei1997$N10.father <- NA

nei1997 <- nei1997[,order(colnames(nei1997))]

nei_output <- rbind(nei_output, nei1997)

#1998 ####
breeding.data.1998 <- xdata[which(xdata$year == 1998),] 
breeding.data.1998 <- breeding.data.1998[!is.na(breeding.data.1998$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1998 <- sf::st_as_sf(breeding.data.1998, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1998))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1998), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1998))
breeding.ids.1998 <- breeding.data.1998[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1998)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1998 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1998$Focal.male <- with(neighbors.1998, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1998$Focal.female <- with(neighbors.1998, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1998$Neighboring.male <- with(neighbors.1998, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1998$Neighboring.female <- with(neighbors.1998, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1998.b <- as.data.frame(with(neighbors.1998, paste(Focal.male, Neighboring.male, sep="_")))
names(N1998.b)[1] <- "ring_ring"
N1998.c <- as.data.frame(with(neighbors.1998, paste(Focal.male, Neighboring.female, sep="_")))
names(N1998.c)[1] <- "ring_ring"
N1998.d <- as.data.frame(with(neighbors.1998, paste(Focal.female, Neighboring.female, sep="_")))
names(N1998.d)[1] <- "ring_ring"
N1998.e <- as.data.frame(with(neighbors.1998, paste(Focal.female, Neighboring.male, sep="_")))
names(N1998.e)[1] <- "ring_ring"

N1998 <- rbind(N1998.b, N1998.c, N1998.d, N1998.e)
rm(N1998.b, N1998.c, N1998.d, N1998.e)
N1998$neighbors <- TRUE
N1998$Year.s <- 1999

N1998 <- N1998[!grepl("UNKNOWN", N1998$ring_ring),]

N_reference <- rbind(N_reference, N1998)


ydata <- base.fn.data[which(base.fn.data$year==1998),]


library(dplyr)

nei1998 <- neighbors.1998[,c(1,4)]

nei1998 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1998


nei1998 <- tidyr::pivot_wider(nei1998, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1998) <- paste0('N', colnames(nei1998))
names(nei1998)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1998
as.data.frame(colnames(ydata))
par1998 <- ydata[,c(2,19,20)]
par1998 <- distinct(par1998, Box, .keep_all = TRUE)


####add neighbor ids
####1998
#N1
names(par1998)[names(par1998) == "Box"] <- "N1"
names(par1998)[names(par1998) == "Mother"] <- "N1.mother"
names(par1998)[names(par1998) == "Father"] <- "N1.father"
nei1998 <- merge(nei1998, par1998, by="N1", all.x=TRUE)
#N2
names(par1998)[names(par1998) == "N1"] <- "N2"
names(par1998)[names(par1998) == "N1.mother"] <- "N2.mother"
names(par1998)[names(par1998) == "N1.father"] <- "N2.father"
nei1998 <- merge(nei1998, par1998, by="N2", all.x=TRUE)
#N3
names(par1998)[names(par1998) == "N2"] <- "N3"
names(par1998)[names(par1998) == "N2.mother"] <- "N3.mother"
names(par1998)[names(par1998) == "N2.father"] <- "N3.father"
nei1998 <- merge(nei1998, par1998, by="N3", all.x=TRUE)
#N4
names(par1998)[names(par1998) == "N3"] <- "N4"
names(par1998)[names(par1998) == "N3.mother"] <- "N4.mother"
names(par1998)[names(par1998) == "N3.father"] <- "N4.father"
nei1998 <- merge(nei1998, par1998, by="N4", all.x=TRUE)
#N5
names(par1998)[names(par1998) == "N4"] <- "N5"
names(par1998)[names(par1998) == "N4.mother"] <- "N5.mother"
names(par1998)[names(par1998) == "N4.father"] <- "N5.father"
nei1998 <- merge(nei1998, par1998, by="N5", all.x=TRUE)
#N6
names(par1998)[names(par1998) == "N5"] <- "N6"
names(par1998)[names(par1998) == "N5.mother"] <- "N6.mother"
names(par1998)[names(par1998) == "N5.father"] <- "N6.father"
nei1998 <- merge(nei1998, par1998, by="N6", all.x=TRUE)
#N7
names(par1998)[names(par1998) == "N6"] <- "N7"
names(par1998)[names(par1998) == "N6.mother"] <- "N7.mother"
names(par1998)[names(par1998) == "N6.father"] <- "N7.father"
nei1998 <- merge(nei1998, par1998, by="N7", all.x=TRUE)
#N8
names(par1998)[names(par1998) == "N7"] <- "N8"
names(par1998)[names(par1998) == "N7.mother"] <- "N8.mother"
names(par1998)[names(par1998) == "N7.father"] <- "N8.father"
nei1998 <- merge(nei1998, par1998, by="N8", all.x=TRUE)
#N9
names(par1998)[names(par1998) == "N8"] <- "N9"
names(par1998)[names(par1998) == "N8.mother"] <- "N9.mother"
names(par1998)[names(par1998) == "N8.father"] <- "N9.father"
nei1998 <- merge(nei1998, par1998, by="N9", all.x=TRUE)
#N10
names(par1998)[names(par1998) == "N9"] <- "N10"
names(par1998)[names(par1998) == "N9.mother"] <- "N10.mother"
names(par1998)[names(par1998) == "N9.father"] <- "N10.father"
nei1998 <- merge(nei1998, par1998, by="N10", all.x=TRUE)

#and identifying column 
nei1998$year <- 1998
nei1998a  <-data.frame(nei1998,"box.year.parentid"=paste(nei1998$Focal.box, nei1998$year, "mother",sep="_")) 
nei1998b  <-data.frame(nei1998,"box.year.parentid"=paste(nei1998$Focal.box, nei1998$year, "father",sep="_")) 

nei1998 <- rbind(nei1998a, nei1998b)
rm(nei1998a, nei1998b)

nei1998$N9 <- NA 
nei1998$N10 <- NA

nei1998$N9.mother <- NA
nei1998$N9.father <- NA
nei1998$N10.mother <- NA
nei1998$N10.father <- NA

nei1998 <- nei1998[,order(colnames(nei1998))]

nei_output <- rbind(nei_output, nei1998)


#1999 ####
breeding.data.1999 <- xdata[which(xdata$year == 1999),] 
breeding.data.1999 <- breeding.data.1999[!is.na(breeding.data.1999$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.1999 <- sf::st_as_sf(breeding.data.1999, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.1999))

territories <- sf::st_voronoi(sf::st_union(breeding.data.1999), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.1999))
breeding.ids.1999 <- breeding.data.1999[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.1999)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.1999 <- territories.list

#change NA ids to UNKNOWN 
neighbors.1999$Focal.male <- with(neighbors.1999, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.1999$Focal.female <- with(neighbors.1999, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.1999$Neighboring.male <- with(neighbors.1999, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.1999$Neighboring.female <- with(neighbors.1999, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N1999.b <- as.data.frame(with(neighbors.1999, paste(Focal.male, Neighboring.male, sep="_")))
names(N1999.b)[1] <- "ring_ring"
N1999.c <- as.data.frame(with(neighbors.1999, paste(Focal.male, Neighboring.female, sep="_")))
names(N1999.c)[1] <- "ring_ring"
N1999.d <- as.data.frame(with(neighbors.1999, paste(Focal.female, Neighboring.female, sep="_")))
names(N1999.d)[1] <- "ring_ring"
N1999.e <- as.data.frame(with(neighbors.1999, paste(Focal.female, Neighboring.male, sep="_")))
names(N1999.e)[1] <- "ring_ring"

N1999 <- rbind(N1999.b, N1999.c, N1999.d, N1999.e)
rm(N1999.b, N1999.c, N1999.d, N1999.e)
N1999$neighbors <- TRUE
N1999$Year.s <- 2000

N1999 <- N1999[!grepl("UNKNOWN", N1999$ring_ring),]

N_reference <- rbind(N_reference, N1999)


ydata <- base.fn.data[which(base.fn.data$year==1999),]


library(dplyr)

nei1999 <- neighbors.1999[,c(1,4)]

nei1999 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei1999


nei1999 <- tidyr::pivot_wider(nei1999, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei1999) <- paste0('N', colnames(nei1999))
names(nei1999)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 1999
as.data.frame(colnames(ydata))
par1999 <- ydata[,c(2,19,20)]
par1999 <- distinct(par1999, Box, .keep_all = TRUE)


####add neighbor ids
####1999
#N1
names(par1999)[names(par1999) == "Box"] <- "N1"
names(par1999)[names(par1999) == "Mother"] <- "N1.mother"
names(par1999)[names(par1999) == "Father"] <- "N1.father"
nei1999 <- merge(nei1999, par1999, by="N1", all.x=TRUE)
#N2
names(par1999)[names(par1999) == "N1"] <- "N2"
names(par1999)[names(par1999) == "N1.mother"] <- "N2.mother"
names(par1999)[names(par1999) == "N1.father"] <- "N2.father"
nei1999 <- merge(nei1999, par1999, by="N2", all.x=TRUE)
#N3
names(par1999)[names(par1999) == "N2"] <- "N3"
names(par1999)[names(par1999) == "N2.mother"] <- "N3.mother"
names(par1999)[names(par1999) == "N2.father"] <- "N3.father"
nei1999 <- merge(nei1999, par1999, by="N3", all.x=TRUE)
#N4
names(par1999)[names(par1999) == "N3"] <- "N4"
names(par1999)[names(par1999) == "N3.mother"] <- "N4.mother"
names(par1999)[names(par1999) == "N3.father"] <- "N4.father"
nei1999 <- merge(nei1999, par1999, by="N4", all.x=TRUE)
#N5
names(par1999)[names(par1999) == "N4"] <- "N5"
names(par1999)[names(par1999) == "N4.mother"] <- "N5.mother"
names(par1999)[names(par1999) == "N4.father"] <- "N5.father"
nei1999 <- merge(nei1999, par1999, by="N5", all.x=TRUE)
#N6
names(par1999)[names(par1999) == "N5"] <- "N6"
names(par1999)[names(par1999) == "N5.mother"] <- "N6.mother"
names(par1999)[names(par1999) == "N5.father"] <- "N6.father"
nei1999 <- merge(nei1999, par1999, by="N6", all.x=TRUE)
#N7
names(par1999)[names(par1999) == "N6"] <- "N7"
names(par1999)[names(par1999) == "N6.mother"] <- "N7.mother"
names(par1999)[names(par1999) == "N6.father"] <- "N7.father"
nei1999 <- merge(nei1999, par1999, by="N7", all.x=TRUE)
#N8
names(par1999)[names(par1999) == "N7"] <- "N8"
names(par1999)[names(par1999) == "N7.mother"] <- "N8.mother"
names(par1999)[names(par1999) == "N7.father"] <- "N8.father"
nei1999 <- merge(nei1999, par1999, by="N8", all.x=TRUE)
#N9
names(par1999)[names(par1999) == "N8"] <- "N9"
names(par1999)[names(par1999) == "N8.mother"] <- "N9.mother"
names(par1999)[names(par1999) == "N8.father"] <- "N9.father"
nei1999 <- merge(nei1999, par1999, by="N9", all.x=TRUE)
#N10
names(par1999)[names(par1999) == "N9"] <- "N10"
names(par1999)[names(par1999) == "N9.mother"] <- "N10.mother"
names(par1999)[names(par1999) == "N9.father"] <- "N10.father"
nei1999 <- merge(nei1999, par1999, by="N10", all.x=TRUE)

#and identifying column 
nei1999$year <- 1999
nei1999a  <-data.frame(nei1999,"box.year.parentid"=paste(nei1999$Focal.box, nei1999$year, "mother",sep="_")) 
nei1999b  <-data.frame(nei1999,"box.year.parentid"=paste(nei1999$Focal.box, nei1999$year, "father",sep="_")) 

nei1999 <- rbind(nei1999a, nei1999b)
rm(nei1999a, nei1999b)

nei1999$N9 <- NA 
nei1999$N10 <- NA

nei1999$N9.mother <- NA
nei1999$N9.father <- NA
nei1999$N10.mother <- NA
nei1999$N10.father <- NA

nei1999 <- nei1999[,order(colnames(nei1999))]

nei_output <- rbind(nei_output, nei1999)


#2000 ####
breeding.data.2000 <- xdata[which(xdata$year == 2000),] 
breeding.data.2000 <- breeding.data.2000[!is.na(breeding.data.2000$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.2000 <- sf::st_as_sf(breeding.data.2000, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.2000))

territories <- sf::st_voronoi(sf::st_union(breeding.data.2000), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.2000))
breeding.ids.2000 <- breeding.data.2000[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.2000)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.2000 <- territories.list

#change NA ids to UNKNOWN 
neighbors.2000$Focal.male <- with(neighbors.2000, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.2000$Focal.female <- with(neighbors.2000, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.2000$Neighboring.male <- with(neighbors.2000, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.2000$Neighboring.female <- with(neighbors.2000, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N2000.b <- as.data.frame(with(neighbors.2000, paste(Focal.male, Neighboring.male, sep="_")))
names(N2000.b)[1] <- "ring_ring"
N2000.c <- as.data.frame(with(neighbors.2000, paste(Focal.male, Neighboring.female, sep="_")))
names(N2000.c)[1] <- "ring_ring"
N2000.d <- as.data.frame(with(neighbors.2000, paste(Focal.female, Neighboring.female, sep="_")))
names(N2000.d)[1] <- "ring_ring"
N2000.e <- as.data.frame(with(neighbors.2000, paste(Focal.female, Neighboring.male, sep="_")))
names(N2000.e)[1] <- "ring_ring"

N2000 <- rbind(N2000.b, N2000.c, N2000.d, N2000.e)
rm(N2000.b, N2000.c, N2000.d, N2000.e)
N2000$neighbors <- TRUE
N2000$Year.s <- 2001

N2000 <- N2000[!grepl("UNKNOWN", N2000$ring_ring),]

N_reference <- rbind(N_reference, N2000)


ydata <- base.fn.data[which(base.fn.data$year==2000),]


library(dplyr)

nei2000 <- neighbors.2000[,c(1,4)]

nei2000 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei2000


nei2000 <- tidyr::pivot_wider(nei2000, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei2000) <- paste0('N', colnames(nei2000))
names(nei2000)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 2000
as.data.frame(colnames(ydata))
par2000 <- ydata[,c(2,19,20)]
par2000 <- distinct(par2000, Box, .keep_all = TRUE)


####add neighbor ids
####2000
#N1
names(par2000)[names(par2000) == "Box"] <- "N1"
names(par2000)[names(par2000) == "Mother"] <- "N1.mother"
names(par2000)[names(par2000) == "Father"] <- "N1.father"
nei2000 <- merge(nei2000, par2000, by="N1", all.x=TRUE)
#N2
names(par2000)[names(par2000) == "N1"] <- "N2"
names(par2000)[names(par2000) == "N1.mother"] <- "N2.mother"
names(par2000)[names(par2000) == "N1.father"] <- "N2.father"
nei2000 <- merge(nei2000, par2000, by="N2", all.x=TRUE)
#N3
names(par2000)[names(par2000) == "N2"] <- "N3"
names(par2000)[names(par2000) == "N2.mother"] <- "N3.mother"
names(par2000)[names(par2000) == "N2.father"] <- "N3.father"
nei2000 <- merge(nei2000, par2000, by="N3", all.x=TRUE)
#N4
names(par2000)[names(par2000) == "N3"] <- "N4"
names(par2000)[names(par2000) == "N3.mother"] <- "N4.mother"
names(par2000)[names(par2000) == "N3.father"] <- "N4.father"
nei2000 <- merge(nei2000, par2000, by="N4", all.x=TRUE)
#N5
names(par2000)[names(par2000) == "N4"] <- "N5"
names(par2000)[names(par2000) == "N4.mother"] <- "N5.mother"
names(par2000)[names(par2000) == "N4.father"] <- "N5.father"
nei2000 <- merge(nei2000, par2000, by="N5", all.x=TRUE)
#N6
names(par2000)[names(par2000) == "N5"] <- "N6"
names(par2000)[names(par2000) == "N5.mother"] <- "N6.mother"
names(par2000)[names(par2000) == "N5.father"] <- "N6.father"
nei2000 <- merge(nei2000, par2000, by="N6", all.x=TRUE)
#N7
names(par2000)[names(par2000) == "N6"] <- "N7"
names(par2000)[names(par2000) == "N6.mother"] <- "N7.mother"
names(par2000)[names(par2000) == "N6.father"] <- "N7.father"
nei2000 <- merge(nei2000, par2000, by="N7", all.x=TRUE)
#N8
names(par2000)[names(par2000) == "N7"] <- "N8"
names(par2000)[names(par2000) == "N7.mother"] <- "N8.mother"
names(par2000)[names(par2000) == "N7.father"] <- "N8.father"
nei2000 <- merge(nei2000, par2000, by="N8", all.x=TRUE)
#N9
names(par2000)[names(par2000) == "N8"] <- "N9"
names(par2000)[names(par2000) == "N8.mother"] <- "N9.mother"
names(par2000)[names(par2000) == "N8.father"] <- "N9.father"
nei2000 <- merge(nei2000, par2000, by="N9", all.x=TRUE)
#N10
names(par2000)[names(par2000) == "N9"] <- "N10"
names(par2000)[names(par2000) == "N9.mother"] <- "N10.mother"
names(par2000)[names(par2000) == "N9.father"] <- "N10.father"
nei2000 <- merge(nei2000, par2000, by="N10", all.x=TRUE)

#and identifying column 
nei2000$year <- 2000
nei2000a  <-data.frame(nei2000,"box.year.parentid"=paste(nei2000$Focal.box, nei2000$year, "mother",sep="_")) 
nei2000b  <-data.frame(nei2000,"box.year.parentid"=paste(nei2000$Focal.box, nei2000$year, "father",sep="_")) 

nei2000 <- rbind(nei2000a, nei2000b)
rm(nei2000a, nei2000b)

nei2000$N10 <- NA

nei2000$N10.mother <- NA
nei2000$N10.father <- NA

nei2000 <- nei2000[,order(colnames(nei2000))]

nei_output <- rbind(nei_output, nei2000)

#2001 ####
breeding.data.2001 <- xdata[which(xdata$year == 2001),] 
breeding.data.2001 <- breeding.data.2001[!is.na(breeding.data.2001$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.2001 <- sf::st_as_sf(breeding.data.2001, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.2001))

territories <- sf::st_voronoi(sf::st_union(breeding.data.2001), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.2001))
breeding.ids.2001 <- breeding.data.2001[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.2001)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.2001 <- territories.list

#change NA ids to UNKNOWN 
neighbors.2001$Focal.male <- with(neighbors.2001, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.2001$Focal.female <- with(neighbors.2001, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.2001$Neighboring.male <- with(neighbors.2001, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.2001$Neighboring.female <- with(neighbors.2001, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N2001.b <- as.data.frame(with(neighbors.2001, paste(Focal.male, Neighboring.male, sep="_")))
names(N2001.b)[1] <- "ring_ring"
N2001.c <- as.data.frame(with(neighbors.2001, paste(Focal.male, Neighboring.female, sep="_")))
names(N2001.c)[1] <- "ring_ring"
N2001.d <- as.data.frame(with(neighbors.2001, paste(Focal.female, Neighboring.female, sep="_")))
names(N2001.d)[1] <- "ring_ring"
N2001.e <- as.data.frame(with(neighbors.2001, paste(Focal.female, Neighboring.male, sep="_")))
names(N2001.e)[1] <- "ring_ring"

N2001 <- rbind(N2001.b, N2001.c, N2001.d, N2001.e)
rm(N2001.b, N2001.c, N2001.d, N2001.e)
N2001$neighbors <- TRUE
N2001$Year.s <- 2002

N2001 <- N2001[!grepl("UNKNOWN", N2001$ring_ring),]

N_reference <- rbind(N_reference, N2001)


ydata <- base.fn.data[which(base.fn.data$year==2001),]


library(dplyr)

nei2001 <- neighbors.2001[,c(1,4)]

nei2001 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei2001


nei2001 <- tidyr::pivot_wider(nei2001, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei2001) <- paste0('N', colnames(nei2001))
names(nei2001)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 2001
as.data.frame(colnames(ydata))
par2001 <- ydata[,c(2,19,20)]
par2001 <- distinct(par2001, Box, .keep_all = TRUE)


####add neighbor ids
####2001
#N1
names(par2001)[names(par2001) == "Box"] <- "N1"
names(par2001)[names(par2001) == "Mother"] <- "N1.mother"
names(par2001)[names(par2001) == "Father"] <- "N1.father"
nei2001 <- merge(nei2001, par2001, by="N1", all.x=TRUE)
#N2
names(par2001)[names(par2001) == "N1"] <- "N2"
names(par2001)[names(par2001) == "N1.mother"] <- "N2.mother"
names(par2001)[names(par2001) == "N1.father"] <- "N2.father"
nei2001 <- merge(nei2001, par2001, by="N2", all.x=TRUE)
#N3
names(par2001)[names(par2001) == "N2"] <- "N3"
names(par2001)[names(par2001) == "N2.mother"] <- "N3.mother"
names(par2001)[names(par2001) == "N2.father"] <- "N3.father"
nei2001 <- merge(nei2001, par2001, by="N3", all.x=TRUE)
#N4
names(par2001)[names(par2001) == "N3"] <- "N4"
names(par2001)[names(par2001) == "N3.mother"] <- "N4.mother"
names(par2001)[names(par2001) == "N3.father"] <- "N4.father"
nei2001 <- merge(nei2001, par2001, by="N4", all.x=TRUE)
#N5
names(par2001)[names(par2001) == "N4"] <- "N5"
names(par2001)[names(par2001) == "N4.mother"] <- "N5.mother"
names(par2001)[names(par2001) == "N4.father"] <- "N5.father"
nei2001 <- merge(nei2001, par2001, by="N5", all.x=TRUE)
#N6
names(par2001)[names(par2001) == "N5"] <- "N6"
names(par2001)[names(par2001) == "N5.mother"] <- "N6.mother"
names(par2001)[names(par2001) == "N5.father"] <- "N6.father"
nei2001 <- merge(nei2001, par2001, by="N6", all.x=TRUE)
#N7
names(par2001)[names(par2001) == "N6"] <- "N7"
names(par2001)[names(par2001) == "N6.mother"] <- "N7.mother"
names(par2001)[names(par2001) == "N6.father"] <- "N7.father"
nei2001 <- merge(nei2001, par2001, by="N7", all.x=TRUE)
#N8
names(par2001)[names(par2001) == "N7"] <- "N8"
names(par2001)[names(par2001) == "N7.mother"] <- "N8.mother"
names(par2001)[names(par2001) == "N7.father"] <- "N8.father"
nei2001 <- merge(nei2001, par2001, by="N8", all.x=TRUE)
#N9
names(par2001)[names(par2001) == "N8"] <- "N9"
names(par2001)[names(par2001) == "N8.mother"] <- "N9.mother"
names(par2001)[names(par2001) == "N8.father"] <- "N9.father"
nei2001 <- merge(nei2001, par2001, by="N9", all.x=TRUE)
#N10
names(par2001)[names(par2001) == "N9"] <- "N10"
names(par2001)[names(par2001) == "N9.mother"] <- "N10.mother"
names(par2001)[names(par2001) == "N9.father"] <- "N10.father"
nei2001 <- merge(nei2001, par2001, by="N10", all.x=TRUE)

#and identifying column 
nei2001$year <- 2001
nei2001a  <-data.frame(nei2001,"box.year.parentid"=paste(nei2001$Focal.box, nei2001$year, "mother",sep="_")) 
nei2001b  <-data.frame(nei2001,"box.year.parentid"=paste(nei2001$Focal.box, nei2001$year, "father",sep="_")) 

nei2001 <- rbind(nei2001a, nei2001b)
rm(nei2001a, nei2001b)

nei2001$N10 <- NA

nei2001$N10.mother <- NA
nei2001$N10.father <- NA

nei2001 <- nei2001[,order(colnames(nei2001))]

nei_output <- rbind(nei_output, nei2001)

#2002 ####
breeding.data.2002 <- xdata[which(xdata$year == 2002),] 
breeding.data.2002 <- breeding.data.2002[!is.na(breeding.data.2002$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.2002 <- sf::st_as_sf(breeding.data.2002, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.2002))

territories <- sf::st_voronoi(sf::st_union(breeding.data.2002), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.2002))
breeding.ids.2002 <- breeding.data.2002[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.2002)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.2002 <- territories.list

#change NA ids to UNKNOWN 
neighbors.2002$Focal.male <- with(neighbors.2002, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.2002$Focal.female <- with(neighbors.2002, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.2002$Neighboring.male <- with(neighbors.2002, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.2002$Neighboring.female <- with(neighbors.2002, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N2002.b <- as.data.frame(with(neighbors.2002, paste(Focal.male, Neighboring.male, sep="_")))
names(N2002.b)[1] <- "ring_ring"
N2002.c <- as.data.frame(with(neighbors.2002, paste(Focal.male, Neighboring.female, sep="_")))
names(N2002.c)[1] <- "ring_ring"
N2002.d <- as.data.frame(with(neighbors.2002, paste(Focal.female, Neighboring.female, sep="_")))
names(N2002.d)[1] <- "ring_ring"
N2002.e <- as.data.frame(with(neighbors.2002, paste(Focal.female, Neighboring.male, sep="_")))
names(N2002.e)[1] <- "ring_ring"

N2002 <- rbind(N2002.b, N2002.c, N2002.d, N2002.e)
rm(N2002.b, N2002.c, N2002.d, N2002.e)
N2002$neighbors <- TRUE
N2002$Year.s <- 2003

N2002 <- N2002[!grepl("UNKNOWN", N2002$ring_ring),]

N_reference <- rbind(N_reference, N2002)


ydata <- base.fn.data[which(base.fn.data$year==2002),]


library(dplyr)

nei2002 <- neighbors.2002[,c(1,4)]

nei2002 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei2002


nei2002 <- tidyr::pivot_wider(nei2002, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei2002) <- paste0('N', colnames(nei2002))
names(nei2002)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 2002
as.data.frame(colnames(ydata))
par2002 <- ydata[,c(2,19,20)]
par2002 <- distinct(par2002, Box, .keep_all = TRUE)


####add neighbor ids
####2002
#N1
names(par2002)[names(par2002) == "Box"] <- "N1"
names(par2002)[names(par2002) == "Mother"] <- "N1.mother"
names(par2002)[names(par2002) == "Father"] <- "N1.father"
nei2002 <- merge(nei2002, par2002, by="N1", all.x=TRUE)
#N2
names(par2002)[names(par2002) == "N1"] <- "N2"
names(par2002)[names(par2002) == "N1.mother"] <- "N2.mother"
names(par2002)[names(par2002) == "N1.father"] <- "N2.father"
nei2002 <- merge(nei2002, par2002, by="N2", all.x=TRUE)
#N3
names(par2002)[names(par2002) == "N2"] <- "N3"
names(par2002)[names(par2002) == "N2.mother"] <- "N3.mother"
names(par2002)[names(par2002) == "N2.father"] <- "N3.father"
nei2002 <- merge(nei2002, par2002, by="N3", all.x=TRUE)
#N4
names(par2002)[names(par2002) == "N3"] <- "N4"
names(par2002)[names(par2002) == "N3.mother"] <- "N4.mother"
names(par2002)[names(par2002) == "N3.father"] <- "N4.father"
nei2002 <- merge(nei2002, par2002, by="N4", all.x=TRUE)
#N5
names(par2002)[names(par2002) == "N4"] <- "N5"
names(par2002)[names(par2002) == "N4.mother"] <- "N5.mother"
names(par2002)[names(par2002) == "N4.father"] <- "N5.father"
nei2002 <- merge(nei2002, par2002, by="N5", all.x=TRUE)
#N6
names(par2002)[names(par2002) == "N5"] <- "N6"
names(par2002)[names(par2002) == "N5.mother"] <- "N6.mother"
names(par2002)[names(par2002) == "N5.father"] <- "N6.father"
nei2002 <- merge(nei2002, par2002, by="N6", all.x=TRUE)
#N7
names(par2002)[names(par2002) == "N6"] <- "N7"
names(par2002)[names(par2002) == "N6.mother"] <- "N7.mother"
names(par2002)[names(par2002) == "N6.father"] <- "N7.father"
nei2002 <- merge(nei2002, par2002, by="N7", all.x=TRUE)
#N8
names(par2002)[names(par2002) == "N7"] <- "N8"
names(par2002)[names(par2002) == "N7.mother"] <- "N8.mother"
names(par2002)[names(par2002) == "N7.father"] <- "N8.father"
nei2002 <- merge(nei2002, par2002, by="N8", all.x=TRUE)
#N9
names(par2002)[names(par2002) == "N8"] <- "N9"
names(par2002)[names(par2002) == "N8.mother"] <- "N9.mother"
names(par2002)[names(par2002) == "N8.father"] <- "N9.father"
nei2002 <- merge(nei2002, par2002, by="N9", all.x=TRUE)
#N10
names(par2002)[names(par2002) == "N9"] <- "N10"
names(par2002)[names(par2002) == "N9.mother"] <- "N10.mother"
names(par2002)[names(par2002) == "N9.father"] <- "N10.father"
nei2002 <- merge(nei2002, par2002, by="N10", all.x=TRUE)

#and identifying column 
nei2002$year <- 2002
nei2002a  <-data.frame(nei2002,"box.year.parentid"=paste(nei2002$Focal.box, nei2002$year, "mother",sep="_")) 
nei2002b  <-data.frame(nei2002,"box.year.parentid"=paste(nei2002$Focal.box, nei2002$year, "father",sep="_")) 

nei2002 <- rbind(nei2002a, nei2002b)
rm(nei2002a, nei2002b)

nei2002 <- nei2002[,order(colnames(nei2002))]

nei_output <- rbind(nei_output, nei2002)

#2003 ####
breeding.data.2003 <- xdata[which(xdata$year == 2003),] 
breeding.data.2003 <- breeding.data.2003[!is.na(breeding.data.2003$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.2003 <- sf::st_as_sf(breeding.data.2003, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.2003))

territories <- sf::st_voronoi(sf::st_union(breeding.data.2003), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.2003))
breeding.ids.2003 <- breeding.data.2003[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.2003)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.2003 <- territories.list

#change NA ids to UNKNOWN 
neighbors.2003$Focal.male <- with(neighbors.2003, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.2003$Focal.female <- with(neighbors.2003, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.2003$Neighboring.male <- with(neighbors.2003, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.2003$Neighboring.female <- with(neighbors.2003, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N2003.b <- as.data.frame(with(neighbors.2003, paste(Focal.male, Neighboring.male, sep="_")))
names(N2003.b)[1] <- "ring_ring"
N2003.c <- as.data.frame(with(neighbors.2003, paste(Focal.male, Neighboring.female, sep="_")))
names(N2003.c)[1] <- "ring_ring"
N2003.d <- as.data.frame(with(neighbors.2003, paste(Focal.female, Neighboring.female, sep="_")))
names(N2003.d)[1] <- "ring_ring"
N2003.e <- as.data.frame(with(neighbors.2003, paste(Focal.female, Neighboring.male, sep="_")))
names(N2003.e)[1] <- "ring_ring"

N2003 <- rbind(N2003.b, N2003.c, N2003.d, N2003.e)
rm(N2003.b, N2003.c, N2003.d, N2003.e)
N2003$neighbors <- TRUE
N2003$Year.s <- 2004

N2003 <- N2003[!grepl("UNKNOWN", N2003$ring_ring),]

N_reference <- rbind(N_reference, N2003)


ydata <- base.fn.data[which(base.fn.data$year==2003),]


library(dplyr)

nei2003 <- neighbors.2003[,c(1,4)]

nei2003 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei2003


nei2003 <- tidyr::pivot_wider(nei2003, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei2003) <- paste0('N', colnames(nei2003))
names(nei2003)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 2003
as.data.frame(colnames(ydata))
par2003 <- ydata[,c(2,19,20)]
par2003 <- distinct(par2003, Box, .keep_all = TRUE)


####add neighbor ids
####2003
#N1
names(par2003)[names(par2003) == "Box"] <- "N1"
names(par2003)[names(par2003) == "Mother"] <- "N1.mother"
names(par2003)[names(par2003) == "Father"] <- "N1.father"
nei2003 <- merge(nei2003, par2003, by="N1", all.x=TRUE)
#N2
names(par2003)[names(par2003) == "N1"] <- "N2"
names(par2003)[names(par2003) == "N1.mother"] <- "N2.mother"
names(par2003)[names(par2003) == "N1.father"] <- "N2.father"
nei2003 <- merge(nei2003, par2003, by="N2", all.x=TRUE)
#N3
names(par2003)[names(par2003) == "N2"] <- "N3"
names(par2003)[names(par2003) == "N2.mother"] <- "N3.mother"
names(par2003)[names(par2003) == "N2.father"] <- "N3.father"
nei2003 <- merge(nei2003, par2003, by="N3", all.x=TRUE)
#N4
names(par2003)[names(par2003) == "N3"] <- "N4"
names(par2003)[names(par2003) == "N3.mother"] <- "N4.mother"
names(par2003)[names(par2003) == "N3.father"] <- "N4.father"
nei2003 <- merge(nei2003, par2003, by="N4", all.x=TRUE)
#N5
names(par2003)[names(par2003) == "N4"] <- "N5"
names(par2003)[names(par2003) == "N4.mother"] <- "N5.mother"
names(par2003)[names(par2003) == "N4.father"] <- "N5.father"
nei2003 <- merge(nei2003, par2003, by="N5", all.x=TRUE)
#N6
names(par2003)[names(par2003) == "N5"] <- "N6"
names(par2003)[names(par2003) == "N5.mother"] <- "N6.mother"
names(par2003)[names(par2003) == "N5.father"] <- "N6.father"
nei2003 <- merge(nei2003, par2003, by="N6", all.x=TRUE)
#N7
names(par2003)[names(par2003) == "N6"] <- "N7"
names(par2003)[names(par2003) == "N6.mother"] <- "N7.mother"
names(par2003)[names(par2003) == "N6.father"] <- "N7.father"
nei2003 <- merge(nei2003, par2003, by="N7", all.x=TRUE)
#N8
names(par2003)[names(par2003) == "N7"] <- "N8"
names(par2003)[names(par2003) == "N7.mother"] <- "N8.mother"
names(par2003)[names(par2003) == "N7.father"] <- "N8.father"
nei2003 <- merge(nei2003, par2003, by="N8", all.x=TRUE)
#N9
names(par2003)[names(par2003) == "N8"] <- "N9"
names(par2003)[names(par2003) == "N8.mother"] <- "N9.mother"
names(par2003)[names(par2003) == "N8.father"] <- "N9.father"
nei2003 <- merge(nei2003, par2003, by="N9", all.x=TRUE)
#N10
names(par2003)[names(par2003) == "N9"] <- "N10"
names(par2003)[names(par2003) == "N9.mother"] <- "N10.mother"
names(par2003)[names(par2003) == "N9.father"] <- "N10.father"
nei2003 <- merge(nei2003, par2003, by="N10", all.x=TRUE)

#and identifying column 
nei2003$year <- 2003
nei2003a  <-data.frame(nei2003,"box.year.parentid"=paste(nei2003$Focal.box, nei2003$year, "mother",sep="_")) 
nei2003b  <-data.frame(nei2003,"box.year.parentid"=paste(nei2003$Focal.box, nei2003$year, "father",sep="_")) 

nei2003 <- rbind(nei2003a, nei2003b)
rm(nei2003a, nei2003b)

nei2003$N8 <- NA 
nei2003$N9 <- NA 
nei2003$N10 <- NA

nei2003$N8.mother <- NA 
nei2003$N8.father <- NA 
nei2003$N9.mother <- NA
nei2003$N9.father <- NA
nei2003$N10.mother <- NA
nei2003$N10.father <- NA

nei2003 <- nei2003[,order(colnames(nei2003))]

nei_output <- rbind(nei_output, nei2003)

#2004 ####
breeding.data.2004 <- xdata[which(xdata$year == 2004),] 
breeding.data.2004 <- breeding.data.2004[!is.na(breeding.data.2004$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.2004 <- sf::st_as_sf(breeding.data.2004, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.2004))

territories <- sf::st_voronoi(sf::st_union(breeding.data.2004), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.2004))
breeding.ids.2004 <- breeding.data.2004[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.2004)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.2004 <- territories.list

#change NA ids to UNKNOWN 
neighbors.2004$Focal.male <- with(neighbors.2004, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.2004$Focal.female <- with(neighbors.2004, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.2004$Neighboring.male <- with(neighbors.2004, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.2004$Neighboring.female <- with(neighbors.2004, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N2004.b <- as.data.frame(with(neighbors.2004, paste(Focal.male, Neighboring.male, sep="_")))
names(N2004.b)[1] <- "ring_ring"
N2004.c <- as.data.frame(with(neighbors.2004, paste(Focal.male, Neighboring.female, sep="_")))
names(N2004.c)[1] <- "ring_ring"
N2004.d <- as.data.frame(with(neighbors.2004, paste(Focal.female, Neighboring.female, sep="_")))
names(N2004.d)[1] <- "ring_ring"
N2004.e <- as.data.frame(with(neighbors.2004, paste(Focal.female, Neighboring.male, sep="_")))
names(N2004.e)[1] <- "ring_ring"

N2004 <- rbind(N2004.b, N2004.c, N2004.d, N2004.e)
rm(N2004.b, N2004.c, N2004.d, N2004.e)
N2004$neighbors <- TRUE
N2004$Year.s <- 2005

N2004 <- N2004[!grepl("UNKNOWN", N2004$ring_ring),]

N_reference <- rbind(N_reference, N2004)


ydata <- base.fn.data[which(base.fn.data$year==2004),]


library(dplyr)

nei2004 <- neighbors.2004[,c(1,4)]

nei2004 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei2004


nei2004 <- tidyr::pivot_wider(nei2004, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei2004) <- paste0('N', colnames(nei2004))
names(nei2004)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 2004
as.data.frame(colnames(ydata))
par2004 <- ydata[,c(2,19,20)]
par2004 <- distinct(par2004, Box, .keep_all = TRUE)


####add neighbor ids
####2004
#N1
names(par2004)[names(par2004) == "Box"] <- "N1"
names(par2004)[names(par2004) == "Mother"] <- "N1.mother"
names(par2004)[names(par2004) == "Father"] <- "N1.father"
nei2004 <- merge(nei2004, par2004, by="N1", all.x=TRUE)
#N2
names(par2004)[names(par2004) == "N1"] <- "N2"
names(par2004)[names(par2004) == "N1.mother"] <- "N2.mother"
names(par2004)[names(par2004) == "N1.father"] <- "N2.father"
nei2004 <- merge(nei2004, par2004, by="N2", all.x=TRUE)
#N3
names(par2004)[names(par2004) == "N2"] <- "N3"
names(par2004)[names(par2004) == "N2.mother"] <- "N3.mother"
names(par2004)[names(par2004) == "N2.father"] <- "N3.father"
nei2004 <- merge(nei2004, par2004, by="N3", all.x=TRUE)
#N4
names(par2004)[names(par2004) == "N3"] <- "N4"
names(par2004)[names(par2004) == "N3.mother"] <- "N4.mother"
names(par2004)[names(par2004) == "N3.father"] <- "N4.father"
nei2004 <- merge(nei2004, par2004, by="N4", all.x=TRUE)
#N5
names(par2004)[names(par2004) == "N4"] <- "N5"
names(par2004)[names(par2004) == "N4.mother"] <- "N5.mother"
names(par2004)[names(par2004) == "N4.father"] <- "N5.father"
nei2004 <- merge(nei2004, par2004, by="N5", all.x=TRUE)
#N6
names(par2004)[names(par2004) == "N5"] <- "N6"
names(par2004)[names(par2004) == "N5.mother"] <- "N6.mother"
names(par2004)[names(par2004) == "N5.father"] <- "N6.father"
nei2004 <- merge(nei2004, par2004, by="N6", all.x=TRUE)
#N7
names(par2004)[names(par2004) == "N6"] <- "N7"
names(par2004)[names(par2004) == "N6.mother"] <- "N7.mother"
names(par2004)[names(par2004) == "N6.father"] <- "N7.father"
nei2004 <- merge(nei2004, par2004, by="N7", all.x=TRUE)
#N8
names(par2004)[names(par2004) == "N7"] <- "N8"
names(par2004)[names(par2004) == "N7.mother"] <- "N8.mother"
names(par2004)[names(par2004) == "N7.father"] <- "N8.father"
nei2004 <- merge(nei2004, par2004, by="N8", all.x=TRUE)
#N9
names(par2004)[names(par2004) == "N8"] <- "N9"
names(par2004)[names(par2004) == "N8.mother"] <- "N9.mother"
names(par2004)[names(par2004) == "N8.father"] <- "N9.father"
nei2004 <- merge(nei2004, par2004, by="N9", all.x=TRUE)
#N10
names(par2004)[names(par2004) == "N9"] <- "N10"
names(par2004)[names(par2004) == "N9.mother"] <- "N10.mother"
names(par2004)[names(par2004) == "N9.father"] <- "N10.father"
nei2004 <- merge(nei2004, par2004, by="N10", all.x=TRUE)

#and identifying column 
nei2004$year <- 2004
nei2004a  <-data.frame(nei2004,"box.year.parentid"=paste(nei2004$Focal.box, nei2004$year, "mother",sep="_")) 
nei2004b  <-data.frame(nei2004,"box.year.parentid"=paste(nei2004$Focal.box, nei2004$year, "father",sep="_")) 

nei2004 <- rbind(nei2004a, nei2004b)
rm(nei2004a, nei2004b)

nei2004$N10 <- NA

nei2004$N10.mother <- NA
nei2004$N10.father <- NA

nei2004 <- nei2004[,order(colnames(nei2004))]

nei_output <- rbind(nei_output, nei2004)


#2005 ####
breeding.data.2005 <- xdata[which(xdata$year == 2005),] 
breeding.data.2005 <- breeding.data.2005[!is.na(breeding.data.2005$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.2005 <- sf::st_as_sf(breeding.data.2005, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.2005))

territories <- sf::st_voronoi(sf::st_union(breeding.data.2005), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.2005))
breeding.ids.2005 <- breeding.data.2005[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.2005)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.2005 <- territories.list

#change NA ids to UNKNOWN 
neighbors.2005$Focal.male <- with(neighbors.2005, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.2005$Focal.female <- with(neighbors.2005, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.2005$Neighboring.male <- with(neighbors.2005, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.2005$Neighboring.female <- with(neighbors.2005, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N2005.b <- as.data.frame(with(neighbors.2005, paste(Focal.male, Neighboring.male, sep="_")))
names(N2005.b)[1] <- "ring_ring"
N2005.c <- as.data.frame(with(neighbors.2005, paste(Focal.male, Neighboring.female, sep="_")))
names(N2005.c)[1] <- "ring_ring"
N2005.d <- as.data.frame(with(neighbors.2005, paste(Focal.female, Neighboring.female, sep="_")))
names(N2005.d)[1] <- "ring_ring"
N2005.e <- as.data.frame(with(neighbors.2005, paste(Focal.female, Neighboring.male, sep="_")))
names(N2005.e)[1] <- "ring_ring"

N2005 <- rbind(N2005.b, N2005.c, N2005.d, N2005.e)
rm(N2005.b, N2005.c, N2005.d, N2005.e)
N2005$neighbors <- TRUE
N2005$Year.s <- 2006

N2005 <- N2005[!grepl("UNKNOWN", N2005$ring_ring),]

N_reference <- rbind(N_reference, N2005)


ydata <- base.fn.data[which(base.fn.data$year==2005),]


library(dplyr)

nei2005 <- neighbors.2005[,c(1,4)]

nei2005 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei2005


nei2005 <- tidyr::pivot_wider(nei2005, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei2005) <- paste0('N', colnames(nei2005))
names(nei2005)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 2005
as.data.frame(colnames(ydata))
par2005 <- ydata[,c(2,19,20)]
par2005 <- distinct(par2005, Box, .keep_all = TRUE)


####add neighbor ids
####2005
#N1
names(par2005)[names(par2005) == "Box"] <- "N1"
names(par2005)[names(par2005) == "Mother"] <- "N1.mother"
names(par2005)[names(par2005) == "Father"] <- "N1.father"
nei2005 <- merge(nei2005, par2005, by="N1", all.x=TRUE)
#N2
names(par2005)[names(par2005) == "N1"] <- "N2"
names(par2005)[names(par2005) == "N1.mother"] <- "N2.mother"
names(par2005)[names(par2005) == "N1.father"] <- "N2.father"
nei2005 <- merge(nei2005, par2005, by="N2", all.x=TRUE)
#N3
names(par2005)[names(par2005) == "N2"] <- "N3"
names(par2005)[names(par2005) == "N2.mother"] <- "N3.mother"
names(par2005)[names(par2005) == "N2.father"] <- "N3.father"
nei2005 <- merge(nei2005, par2005, by="N3", all.x=TRUE)
#N4
names(par2005)[names(par2005) == "N3"] <- "N4"
names(par2005)[names(par2005) == "N3.mother"] <- "N4.mother"
names(par2005)[names(par2005) == "N3.father"] <- "N4.father"
nei2005 <- merge(nei2005, par2005, by="N4", all.x=TRUE)
#N5
names(par2005)[names(par2005) == "N4"] <- "N5"
names(par2005)[names(par2005) == "N4.mother"] <- "N5.mother"
names(par2005)[names(par2005) == "N4.father"] <- "N5.father"
nei2005 <- merge(nei2005, par2005, by="N5", all.x=TRUE)
#N6
names(par2005)[names(par2005) == "N5"] <- "N6"
names(par2005)[names(par2005) == "N5.mother"] <- "N6.mother"
names(par2005)[names(par2005) == "N5.father"] <- "N6.father"
nei2005 <- merge(nei2005, par2005, by="N6", all.x=TRUE)
#N7
names(par2005)[names(par2005) == "N6"] <- "N7"
names(par2005)[names(par2005) == "N6.mother"] <- "N7.mother"
names(par2005)[names(par2005) == "N6.father"] <- "N7.father"
nei2005 <- merge(nei2005, par2005, by="N7", all.x=TRUE)
#N8
names(par2005)[names(par2005) == "N7"] <- "N8"
names(par2005)[names(par2005) == "N7.mother"] <- "N8.mother"
names(par2005)[names(par2005) == "N7.father"] <- "N8.father"
nei2005 <- merge(nei2005, par2005, by="N8", all.x=TRUE)
#N9
names(par2005)[names(par2005) == "N8"] <- "N9"
names(par2005)[names(par2005) == "N8.mother"] <- "N9.mother"
names(par2005)[names(par2005) == "N8.father"] <- "N9.father"
nei2005 <- merge(nei2005, par2005, by="N9", all.x=TRUE)
#N10
names(par2005)[names(par2005) == "N9"] <- "N10"
names(par2005)[names(par2005) == "N9.mother"] <- "N10.mother"
names(par2005)[names(par2005) == "N9.father"] <- "N10.father"
nei2005 <- merge(nei2005, par2005, by="N10", all.x=TRUE)

#and identifying column 
nei2005$year <- 2005
nei2005a  <-data.frame(nei2005,"box.year.parentid"=paste(nei2005$Focal.box, nei2005$year, "mother",sep="_")) 
nei2005b  <-data.frame(nei2005,"box.year.parentid"=paste(nei2005$Focal.box, nei2005$year, "father",sep="_")) 

nei2005 <- rbind(nei2005a, nei2005b)
rm(nei2005a, nei2005b)

nei2005 <- nei2005[,order(colnames(nei2005))]

nei_output <- rbind(nei_output, nei2005)


#2006 ####
breeding.data.2006 <- xdata[which(xdata$year == 2006),] 
breeding.data.2006 <- breeding.data.2006[!is.na(breeding.data.2006$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.2006 <- sf::st_as_sf(breeding.data.2006, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.2006))

territories <- sf::st_voronoi(sf::st_union(breeding.data.2006), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.2006))
breeding.ids.2006 <- breeding.data.2006[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.2006)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.2006 <- territories.list

#change NA ids to UNKNOWN 
neighbors.2006$Focal.male <- with(neighbors.2006, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.2006$Focal.female <- with(neighbors.2006, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.2006$Neighboring.male <- with(neighbors.2006, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.2006$Neighboring.female <- with(neighbors.2006, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N2006.b <- as.data.frame(with(neighbors.2006, paste(Focal.male, Neighboring.male, sep="_")))
names(N2006.b)[1] <- "ring_ring"
N2006.c <- as.data.frame(with(neighbors.2006, paste(Focal.male, Neighboring.female, sep="_")))
names(N2006.c)[1] <- "ring_ring"
N2006.d <- as.data.frame(with(neighbors.2006, paste(Focal.female, Neighboring.female, sep="_")))
names(N2006.d)[1] <- "ring_ring"
N2006.e <- as.data.frame(with(neighbors.2006, paste(Focal.female, Neighboring.male, sep="_")))
names(N2006.e)[1] <- "ring_ring"

N2006 <- rbind(N2006.b, N2006.c, N2006.d, N2006.e)
rm(N2006.b, N2006.c, N2006.d, N2006.e)
N2006$neighbors <- TRUE
N2006$Year.s <- 2007

N2006 <- N2006[!grepl("UNKNOWN", N2006$ring_ring),]

N_reference <- rbind(N_reference, N2006)


ydata <- base.fn.data[which(base.fn.data$year==2006),]


library(dplyr)

nei2006 <- neighbors.2006[,c(1,4)]

nei2006 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei2006


nei2006 <- tidyr::pivot_wider(nei2006, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei2006) <- paste0('N', colnames(nei2006))
names(nei2006)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 2006
as.data.frame(colnames(ydata))
par2006 <- ydata[,c(2,19,20)]
par2006 <- distinct(par2006, Box, .keep_all = TRUE)


####add neighbor ids
####2006
#N1
names(par2006)[names(par2006) == "Box"] <- "N1"
names(par2006)[names(par2006) == "Mother"] <- "N1.mother"
names(par2006)[names(par2006) == "Father"] <- "N1.father"
nei2006 <- merge(nei2006, par2006, by="N1", all.x=TRUE)
#N2
names(par2006)[names(par2006) == "N1"] <- "N2"
names(par2006)[names(par2006) == "N1.mother"] <- "N2.mother"
names(par2006)[names(par2006) == "N1.father"] <- "N2.father"
nei2006 <- merge(nei2006, par2006, by="N2", all.x=TRUE)
#N3
names(par2006)[names(par2006) == "N2"] <- "N3"
names(par2006)[names(par2006) == "N2.mother"] <- "N3.mother"
names(par2006)[names(par2006) == "N2.father"] <- "N3.father"
nei2006 <- merge(nei2006, par2006, by="N3", all.x=TRUE)
#N4
names(par2006)[names(par2006) == "N3"] <- "N4"
names(par2006)[names(par2006) == "N3.mother"] <- "N4.mother"
names(par2006)[names(par2006) == "N3.father"] <- "N4.father"
nei2006 <- merge(nei2006, par2006, by="N4", all.x=TRUE)
#N5
names(par2006)[names(par2006) == "N4"] <- "N5"
names(par2006)[names(par2006) == "N4.mother"] <- "N5.mother"
names(par2006)[names(par2006) == "N4.father"] <- "N5.father"
nei2006 <- merge(nei2006, par2006, by="N5", all.x=TRUE)
#N6
names(par2006)[names(par2006) == "N5"] <- "N6"
names(par2006)[names(par2006) == "N5.mother"] <- "N6.mother"
names(par2006)[names(par2006) == "N5.father"] <- "N6.father"
nei2006 <- merge(nei2006, par2006, by="N6", all.x=TRUE)
#N7
names(par2006)[names(par2006) == "N6"] <- "N7"
names(par2006)[names(par2006) == "N6.mother"] <- "N7.mother"
names(par2006)[names(par2006) == "N6.father"] <- "N7.father"
nei2006 <- merge(nei2006, par2006, by="N7", all.x=TRUE)
#N8
names(par2006)[names(par2006) == "N7"] <- "N8"
names(par2006)[names(par2006) == "N7.mother"] <- "N8.mother"
names(par2006)[names(par2006) == "N7.father"] <- "N8.father"
nei2006 <- merge(nei2006, par2006, by="N8", all.x=TRUE)
#N9
names(par2006)[names(par2006) == "N8"] <- "N9"
names(par2006)[names(par2006) == "N8.mother"] <- "N9.mother"
names(par2006)[names(par2006) == "N8.father"] <- "N9.father"
nei2006 <- merge(nei2006, par2006, by="N9", all.x=TRUE)
#N10
names(par2006)[names(par2006) == "N9"] <- "N10"
names(par2006)[names(par2006) == "N9.mother"] <- "N10.mother"
names(par2006)[names(par2006) == "N9.father"] <- "N10.father"
nei2006 <- merge(nei2006, par2006, by="N10", all.x=TRUE)

#and identifying column 
nei2006$year <- 2006
nei2006a  <-data.frame(nei2006,"box.year.parentid"=paste(nei2006$Focal.box, nei2006$year, "mother",sep="_")) 
nei2006b  <-data.frame(nei2006,"box.year.parentid"=paste(nei2006$Focal.box, nei2006$year, "father",sep="_")) 

nei2006 <- rbind(nei2006a, nei2006b)
rm(nei2006a, nei2006b)

nei2006$N10 <- NA

nei2006$N10.mother <- NA
nei2006$N10.father <- NA

nei2006 <- nei2006[,order(colnames(nei2006))]

nei_output <- rbind(nei_output, nei2006)


#2007 ####
breeding.data.2007 <- xdata[which(xdata$year == 2007),] 
breeding.data.2007 <- breeding.data.2007[!is.na(breeding.data.2007$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.2007 <- sf::st_as_sf(breeding.data.2007, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.2007))

territories <- sf::st_voronoi(sf::st_union(breeding.data.2007), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.2007))
breeding.ids.2007 <- breeding.data.2007[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.2007)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.2007 <- territories.list

#change NA ids to UNKNOWN 
neighbors.2007$Focal.male <- with(neighbors.2007, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.2007$Focal.female <- with(neighbors.2007, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.2007$Neighboring.male <- with(neighbors.2007, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.2007$Neighboring.female <- with(neighbors.2007, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N2007.b <- as.data.frame(with(neighbors.2007, paste(Focal.male, Neighboring.male, sep="_")))
names(N2007.b)[1] <- "ring_ring"
N2007.c <- as.data.frame(with(neighbors.2007, paste(Focal.male, Neighboring.female, sep="_")))
names(N2007.c)[1] <- "ring_ring"
N2007.d <- as.data.frame(with(neighbors.2007, paste(Focal.female, Neighboring.female, sep="_")))
names(N2007.d)[1] <- "ring_ring"
N2007.e <- as.data.frame(with(neighbors.2007, paste(Focal.female, Neighboring.male, sep="_")))
names(N2007.e)[1] <- "ring_ring"

N2007 <- rbind(N2007.b, N2007.c, N2007.d, N2007.e)
rm(N2007.b, N2007.c, N2007.d, N2007.e)
N2007$neighbors <- TRUE
N2007$Year.s <- 2008

N2007 <- N2007[!grepl("UNKNOWN", N2007$ring_ring),]

N_reference <- rbind(N_reference, N2007)


ydata <- base.fn.data[which(base.fn.data$year==2007),]


library(dplyr)

nei2007 <- neighbors.2007[,c(1,4)]

nei2007 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei2007


nei2007 <- tidyr::pivot_wider(nei2007, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei2007) <- paste0('N', colnames(nei2007))
names(nei2007)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 2007
as.data.frame(colnames(ydata))
par2007 <- ydata[,c(2,19,20)]
par2007 <- distinct(par2007, Box, .keep_all = TRUE)


####add neighbor ids
####2007
#N1
names(par2007)[names(par2007) == "Box"] <- "N1"
names(par2007)[names(par2007) == "Mother"] <- "N1.mother"
names(par2007)[names(par2007) == "Father"] <- "N1.father"
nei2007 <- merge(nei2007, par2007, by="N1", all.x=TRUE)
#N2
names(par2007)[names(par2007) == "N1"] <- "N2"
names(par2007)[names(par2007) == "N1.mother"] <- "N2.mother"
names(par2007)[names(par2007) == "N1.father"] <- "N2.father"
nei2007 <- merge(nei2007, par2007, by="N2", all.x=TRUE)
#N3
names(par2007)[names(par2007) == "N2"] <- "N3"
names(par2007)[names(par2007) == "N2.mother"] <- "N3.mother"
names(par2007)[names(par2007) == "N2.father"] <- "N3.father"
nei2007 <- merge(nei2007, par2007, by="N3", all.x=TRUE)
#N4
names(par2007)[names(par2007) == "N3"] <- "N4"
names(par2007)[names(par2007) == "N3.mother"] <- "N4.mother"
names(par2007)[names(par2007) == "N3.father"] <- "N4.father"
nei2007 <- merge(nei2007, par2007, by="N4", all.x=TRUE)
#N5
names(par2007)[names(par2007) == "N4"] <- "N5"
names(par2007)[names(par2007) == "N4.mother"] <- "N5.mother"
names(par2007)[names(par2007) == "N4.father"] <- "N5.father"
nei2007 <- merge(nei2007, par2007, by="N5", all.x=TRUE)
#N6
names(par2007)[names(par2007) == "N5"] <- "N6"
names(par2007)[names(par2007) == "N5.mother"] <- "N6.mother"
names(par2007)[names(par2007) == "N5.father"] <- "N6.father"
nei2007 <- merge(nei2007, par2007, by="N6", all.x=TRUE)
#N7
names(par2007)[names(par2007) == "N6"] <- "N7"
names(par2007)[names(par2007) == "N6.mother"] <- "N7.mother"
names(par2007)[names(par2007) == "N6.father"] <- "N7.father"
nei2007 <- merge(nei2007, par2007, by="N7", all.x=TRUE)
#N8
names(par2007)[names(par2007) == "N7"] <- "N8"
names(par2007)[names(par2007) == "N7.mother"] <- "N8.mother"
names(par2007)[names(par2007) == "N7.father"] <- "N8.father"
nei2007 <- merge(nei2007, par2007, by="N8", all.x=TRUE)
#N9
names(par2007)[names(par2007) == "N8"] <- "N9"
names(par2007)[names(par2007) == "N8.mother"] <- "N9.mother"
names(par2007)[names(par2007) == "N8.father"] <- "N9.father"
nei2007 <- merge(nei2007, par2007, by="N9", all.x=TRUE)
#N10
names(par2007)[names(par2007) == "N9"] <- "N10"
names(par2007)[names(par2007) == "N9.mother"] <- "N10.mother"
names(par2007)[names(par2007) == "N9.father"] <- "N10.father"
nei2007 <- merge(nei2007, par2007, by="N10", all.x=TRUE)

#and identifying column 
nei2007$year <- 2007
nei2007a  <-data.frame(nei2007,"box.year.parentid"=paste(nei2007$Focal.box, nei2007$year, "mother",sep="_")) 
nei2007b  <-data.frame(nei2007,"box.year.parentid"=paste(nei2007$Focal.box, nei2007$year, "father",sep="_")) 

nei2007 <- rbind(nei2007a, nei2007b)
rm(nei2007a, nei2007b)

nei2007$N10 <- NA

nei2007$N10.mother <- NA
nei2007$N10.father <- NA

nei2007 <- nei2007[,order(colnames(nei2007))]

nei_output <- rbind(nei_output, nei2007)

#2008 ####
breeding.data.2008 <- xdata[which(xdata$year == 2008),] 
breeding.data.2008 <- breeding.data.2008[!is.na(breeding.data.2008$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.2008 <- sf::st_as_sf(breeding.data.2008, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.2008))

territories <- sf::st_voronoi(sf::st_union(breeding.data.2008), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.2008))
breeding.ids.2008 <- breeding.data.2008[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.2008)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.2008 <- territories.list

#change NA ids to UNKNOWN 
neighbors.2008$Focal.male <- with(neighbors.2008, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.2008$Focal.female <- with(neighbors.2008, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.2008$Neighboring.male <- with(neighbors.2008, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.2008$Neighboring.female <- with(neighbors.2008, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N2008.b <- as.data.frame(with(neighbors.2008, paste(Focal.male, Neighboring.male, sep="_")))
names(N2008.b)[1] <- "ring_ring"
N2008.c <- as.data.frame(with(neighbors.2008, paste(Focal.male, Neighboring.female, sep="_")))
names(N2008.c)[1] <- "ring_ring"
N2008.d <- as.data.frame(with(neighbors.2008, paste(Focal.female, Neighboring.female, sep="_")))
names(N2008.d)[1] <- "ring_ring"
N2008.e <- as.data.frame(with(neighbors.2008, paste(Focal.female, Neighboring.male, sep="_")))
names(N2008.e)[1] <- "ring_ring"

N2008 <- rbind(N2008.b, N2008.c, N2008.d, N2008.e)
rm(N2008.b, N2008.c, N2008.d, N2008.e)
N2008$neighbors <- TRUE
N2008$Year.s <- 2009

N2008 <- N2008[!grepl("UNKNOWN", N2008$ring_ring),]

N_reference <- rbind(N_reference, N2008)


ydata <- base.fn.data[which(base.fn.data$year==2008),]


library(dplyr)

nei2008 <- neighbors.2008[,c(1,4)]

nei2008 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei2008


nei2008 <- tidyr::pivot_wider(nei2008, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei2008) <- paste0('N', colnames(nei2008))
names(nei2008)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 2008
as.data.frame(colnames(ydata))
par2008 <- ydata[,c(2,19,20)]
par2008 <- distinct(par2008, Box, .keep_all = TRUE)


####add neighbor ids
####2008
#N1
names(par2008)[names(par2008) == "Box"] <- "N1"
names(par2008)[names(par2008) == "Mother"] <- "N1.mother"
names(par2008)[names(par2008) == "Father"] <- "N1.father"
nei2008 <- merge(nei2008, par2008, by="N1", all.x=TRUE)
#N2
names(par2008)[names(par2008) == "N1"] <- "N2"
names(par2008)[names(par2008) == "N1.mother"] <- "N2.mother"
names(par2008)[names(par2008) == "N1.father"] <- "N2.father"
nei2008 <- merge(nei2008, par2008, by="N2", all.x=TRUE)
#N3
names(par2008)[names(par2008) == "N2"] <- "N3"
names(par2008)[names(par2008) == "N2.mother"] <- "N3.mother"
names(par2008)[names(par2008) == "N2.father"] <- "N3.father"
nei2008 <- merge(nei2008, par2008, by="N3", all.x=TRUE)
#N4
names(par2008)[names(par2008) == "N3"] <- "N4"
names(par2008)[names(par2008) == "N3.mother"] <- "N4.mother"
names(par2008)[names(par2008) == "N3.father"] <- "N4.father"
nei2008 <- merge(nei2008, par2008, by="N4", all.x=TRUE)
#N5
names(par2008)[names(par2008) == "N4"] <- "N5"
names(par2008)[names(par2008) == "N4.mother"] <- "N5.mother"
names(par2008)[names(par2008) == "N4.father"] <- "N5.father"
nei2008 <- merge(nei2008, par2008, by="N5", all.x=TRUE)
#N6
names(par2008)[names(par2008) == "N5"] <- "N6"
names(par2008)[names(par2008) == "N5.mother"] <- "N6.mother"
names(par2008)[names(par2008) == "N5.father"] <- "N6.father"
nei2008 <- merge(nei2008, par2008, by="N6", all.x=TRUE)
#N7
names(par2008)[names(par2008) == "N6"] <- "N7"
names(par2008)[names(par2008) == "N6.mother"] <- "N7.mother"
names(par2008)[names(par2008) == "N6.father"] <- "N7.father"
nei2008 <- merge(nei2008, par2008, by="N7", all.x=TRUE)
#N8
names(par2008)[names(par2008) == "N7"] <- "N8"
names(par2008)[names(par2008) == "N7.mother"] <- "N8.mother"
names(par2008)[names(par2008) == "N7.father"] <- "N8.father"
nei2008 <- merge(nei2008, par2008, by="N8", all.x=TRUE)
#N9
names(par2008)[names(par2008) == "N8"] <- "N9"
names(par2008)[names(par2008) == "N8.mother"] <- "N9.mother"
names(par2008)[names(par2008) == "N8.father"] <- "N9.father"
nei2008 <- merge(nei2008, par2008, by="N9", all.x=TRUE)
#N10
names(par2008)[names(par2008) == "N9"] <- "N10"
names(par2008)[names(par2008) == "N9.mother"] <- "N10.mother"
names(par2008)[names(par2008) == "N9.father"] <- "N10.father"
nei2008 <- merge(nei2008, par2008, by="N10", all.x=TRUE)

#and identifying column 
nei2008$year <- 2008
nei2008a  <-data.frame(nei2008,"box.year.parentid"=paste(nei2008$Focal.box, nei2008$year, "mother",sep="_")) 
nei2008b  <-data.frame(nei2008,"box.year.parentid"=paste(nei2008$Focal.box, nei2008$year, "father",sep="_")) 

nei2008 <- rbind(nei2008a, nei2008b)
rm(nei2008a, nei2008b)

nei2008$N10 <- NA

nei2008$N10.mother <- NA
nei2008$N10.father <- NA

nei2008 <- nei2008[,order(colnames(nei2008))]

nei_output <- rbind(nei_output, nei2008)


#2009 ####
breeding.data.2009 <- xdata[which(xdata$year == 2009),] 
breeding.data.2009 <- breeding.data.2009[!is.na(breeding.data.2009$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.2009 <- sf::st_as_sf(breeding.data.2009, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.2009))

territories <- sf::st_voronoi(sf::st_union(breeding.data.2009), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.2009))
breeding.ids.2009 <- breeding.data.2009[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.2009)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.2009 <- territories.list

#change NA ids to UNKNOWN 
neighbors.2009$Focal.male <- with(neighbors.2009, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.2009$Focal.female <- with(neighbors.2009, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.2009$Neighboring.male <- with(neighbors.2009, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.2009$Neighboring.female <- with(neighbors.2009, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N2009.b <- as.data.frame(with(neighbors.2009, paste(Focal.male, Neighboring.male, sep="_")))
names(N2009.b)[1] <- "ring_ring"
N2009.c <- as.data.frame(with(neighbors.2009, paste(Focal.male, Neighboring.female, sep="_")))
names(N2009.c)[1] <- "ring_ring"
N2009.d <- as.data.frame(with(neighbors.2009, paste(Focal.female, Neighboring.female, sep="_")))
names(N2009.d)[1] <- "ring_ring"
N2009.e <- as.data.frame(with(neighbors.2009, paste(Focal.female, Neighboring.male, sep="_")))
names(N2009.e)[1] <- "ring_ring"

N2009 <- rbind(N2009.b, N2009.c, N2009.d, N2009.e)
rm(N2009.b, N2009.c, N2009.d, N2009.e)
N2009$neighbors <- TRUE
N2009$Year.s <- 2010

N2009 <- N2009[!grepl("UNKNOWN", N2009$ring_ring),]

N_reference <- rbind(N_reference, N2009)


ydata <- base.fn.data[which(base.fn.data$year==2009),]


library(dplyr)

nei2009 <- neighbors.2009[,c(1,4)]

nei2009 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei2009


nei2009 <- tidyr::pivot_wider(nei2009, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei2009) <- paste0('N', colnames(nei2009))
names(nei2009)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 2009
as.data.frame(colnames(ydata))
par2009 <- ydata[,c(2,19,20)]
par2009 <- distinct(par2009, Box, .keep_all = TRUE)


####add neighbor ids
####2009
#N1
names(par2009)[names(par2009) == "Box"] <- "N1"
names(par2009)[names(par2009) == "Mother"] <- "N1.mother"
names(par2009)[names(par2009) == "Father"] <- "N1.father"
nei2009 <- merge(nei2009, par2009, by="N1", all.x=TRUE)
#N2
names(par2009)[names(par2009) == "N1"] <- "N2"
names(par2009)[names(par2009) == "N1.mother"] <- "N2.mother"
names(par2009)[names(par2009) == "N1.father"] <- "N2.father"
nei2009 <- merge(nei2009, par2009, by="N2", all.x=TRUE)
#N3
names(par2009)[names(par2009) == "N2"] <- "N3"
names(par2009)[names(par2009) == "N2.mother"] <- "N3.mother"
names(par2009)[names(par2009) == "N2.father"] <- "N3.father"
nei2009 <- merge(nei2009, par2009, by="N3", all.x=TRUE)
#N4
names(par2009)[names(par2009) == "N3"] <- "N4"
names(par2009)[names(par2009) == "N3.mother"] <- "N4.mother"
names(par2009)[names(par2009) == "N3.father"] <- "N4.father"
nei2009 <- merge(nei2009, par2009, by="N4", all.x=TRUE)
#N5
names(par2009)[names(par2009) == "N4"] <- "N5"
names(par2009)[names(par2009) == "N4.mother"] <- "N5.mother"
names(par2009)[names(par2009) == "N4.father"] <- "N5.father"
nei2009 <- merge(nei2009, par2009, by="N5", all.x=TRUE)
#N6
names(par2009)[names(par2009) == "N5"] <- "N6"
names(par2009)[names(par2009) == "N5.mother"] <- "N6.mother"
names(par2009)[names(par2009) == "N5.father"] <- "N6.father"
nei2009 <- merge(nei2009, par2009, by="N6", all.x=TRUE)
#N7
names(par2009)[names(par2009) == "N6"] <- "N7"
names(par2009)[names(par2009) == "N6.mother"] <- "N7.mother"
names(par2009)[names(par2009) == "N6.father"] <- "N7.father"
nei2009 <- merge(nei2009, par2009, by="N7", all.x=TRUE)
#N8
names(par2009)[names(par2009) == "N7"] <- "N8"
names(par2009)[names(par2009) == "N7.mother"] <- "N8.mother"
names(par2009)[names(par2009) == "N7.father"] <- "N8.father"
nei2009 <- merge(nei2009, par2009, by="N8", all.x=TRUE)
#N9
names(par2009)[names(par2009) == "N8"] <- "N9"
names(par2009)[names(par2009) == "N8.mother"] <- "N9.mother"
names(par2009)[names(par2009) == "N8.father"] <- "N9.father"
nei2009 <- merge(nei2009, par2009, by="N9", all.x=TRUE)
#N10
names(par2009)[names(par2009) == "N9"] <- "N10"
names(par2009)[names(par2009) == "N9.mother"] <- "N10.mother"
names(par2009)[names(par2009) == "N9.father"] <- "N10.father"
nei2009 <- merge(nei2009, par2009, by="N10", all.x=TRUE)

#and identifying column 
nei2009$year <- 2009
nei2009a  <-data.frame(nei2009,"box.year.parentid"=paste(nei2009$Focal.box, nei2009$year, "mother",sep="_")) 
nei2009b  <-data.frame(nei2009,"box.year.parentid"=paste(nei2009$Focal.box, nei2009$year, "father",sep="_")) 

nei2009 <- rbind(nei2009a, nei2009b)
rm(nei2009a, nei2009b)

nei2009 <- nei2009[,order(colnames(nei2009))]

nei_output <- rbind(nei_output, nei2009)


#2010 ####
breeding.data.2010 <- xdata[which(xdata$year == 2010),] 
breeding.data.2010 <- breeding.data.2010[!is.na(breeding.data.2010$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.2010 <- sf::st_as_sf(breeding.data.2010, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.2010))

territories <- sf::st_voronoi(sf::st_union(breeding.data.2010), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.2010))
breeding.ids.2010 <- breeding.data.2010[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.2010)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.2010 <- territories.list

#change NA ids to UNKNOWN 
neighbors.2010$Focal.male <- with(neighbors.2010, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.2010$Focal.female <- with(neighbors.2010, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.2010$Neighboring.male <- with(neighbors.2010, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.2010$Neighboring.female <- with(neighbors.2010, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N2010.b <- as.data.frame(with(neighbors.2010, paste(Focal.male, Neighboring.male, sep="_")))
names(N2010.b)[1] <- "ring_ring"
N2010.c <- as.data.frame(with(neighbors.2010, paste(Focal.male, Neighboring.female, sep="_")))
names(N2010.c)[1] <- "ring_ring"
N2010.d <- as.data.frame(with(neighbors.2010, paste(Focal.female, Neighboring.female, sep="_")))
names(N2010.d)[1] <- "ring_ring"
N2010.e <- as.data.frame(with(neighbors.2010, paste(Focal.female, Neighboring.male, sep="_")))
names(N2010.e)[1] <- "ring_ring"

N2010 <- rbind(N2010.b, N2010.c, N2010.d, N2010.e)
rm(N2010.b, N2010.c, N2010.d, N2010.e)
N2010$neighbors <- TRUE
N2010$Year.s <- 2011

N2010 <- N2010[!grepl("UNKNOWN", N2010$ring_ring),]

N_reference <- rbind(N_reference, N2010)


ydata <- base.fn.data[which(base.fn.data$year==2010),]


library(dplyr)

nei2010 <- neighbors.2010[,c(1,4)]

nei2010 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei2010


nei2010 <- tidyr::pivot_wider(nei2010, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei2010) <- paste0('N', colnames(nei2010))
names(nei2010)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 2010
as.data.frame(colnames(ydata))
par2010 <- ydata[,c(2,19,20)]
par2010 <- distinct(par2010, Box, .keep_all = TRUE)


####add neighbor ids
####2010
#N1
names(par2010)[names(par2010) == "Box"] <- "N1"
names(par2010)[names(par2010) == "Mother"] <- "N1.mother"
names(par2010)[names(par2010) == "Father"] <- "N1.father"
nei2010 <- merge(nei2010, par2010, by="N1", all.x=TRUE)
#N2
names(par2010)[names(par2010) == "N1"] <- "N2"
names(par2010)[names(par2010) == "N1.mother"] <- "N2.mother"
names(par2010)[names(par2010) == "N1.father"] <- "N2.father"
nei2010 <- merge(nei2010, par2010, by="N2", all.x=TRUE)
#N3
names(par2010)[names(par2010) == "N2"] <- "N3"
names(par2010)[names(par2010) == "N2.mother"] <- "N3.mother"
names(par2010)[names(par2010) == "N2.father"] <- "N3.father"
nei2010 <- merge(nei2010, par2010, by="N3", all.x=TRUE)
#N4
names(par2010)[names(par2010) == "N3"] <- "N4"
names(par2010)[names(par2010) == "N3.mother"] <- "N4.mother"
names(par2010)[names(par2010) == "N3.father"] <- "N4.father"
nei2010 <- merge(nei2010, par2010, by="N4", all.x=TRUE)
#N5
names(par2010)[names(par2010) == "N4"] <- "N5"
names(par2010)[names(par2010) == "N4.mother"] <- "N5.mother"
names(par2010)[names(par2010) == "N4.father"] <- "N5.father"
nei2010 <- merge(nei2010, par2010, by="N5", all.x=TRUE)
#N6
names(par2010)[names(par2010) == "N5"] <- "N6"
names(par2010)[names(par2010) == "N5.mother"] <- "N6.mother"
names(par2010)[names(par2010) == "N5.father"] <- "N6.father"
nei2010 <- merge(nei2010, par2010, by="N6", all.x=TRUE)
#N7
names(par2010)[names(par2010) == "N6"] <- "N7"
names(par2010)[names(par2010) == "N6.mother"] <- "N7.mother"
names(par2010)[names(par2010) == "N6.father"] <- "N7.father"
nei2010 <- merge(nei2010, par2010, by="N7", all.x=TRUE)
#N8
names(par2010)[names(par2010) == "N7"] <- "N8"
names(par2010)[names(par2010) == "N7.mother"] <- "N8.mother"
names(par2010)[names(par2010) == "N7.father"] <- "N8.father"
nei2010 <- merge(nei2010, par2010, by="N8", all.x=TRUE)
#N9
names(par2010)[names(par2010) == "N8"] <- "N9"
names(par2010)[names(par2010) == "N8.mother"] <- "N9.mother"
names(par2010)[names(par2010) == "N8.father"] <- "N9.father"
nei2010 <- merge(nei2010, par2010, by="N9", all.x=TRUE)
#N10
names(par2010)[names(par2010) == "N9"] <- "N10"
names(par2010)[names(par2010) == "N9.mother"] <- "N10.mother"
names(par2010)[names(par2010) == "N9.father"] <- "N10.father"
nei2010 <- merge(nei2010, par2010, by="N10", all.x=TRUE)

#and identifying column 
nei2010$year <- 2010
nei2010a  <-data.frame(nei2010,"box.year.parentid"=paste(nei2010$Focal.box, nei2010$year, "mother",sep="_")) 
nei2010b  <-data.frame(nei2010,"box.year.parentid"=paste(nei2010$Focal.box, nei2010$year, "father",sep="_")) 

nei2010 <- rbind(nei2010a, nei2010b)
rm(nei2010a, nei2010b)

nei2010$N10 <- NA

nei2010$N10.mother <- NA
nei2010$N10.father <- NA

nei2010 <- nei2010[,order(colnames(nei2010))]

nei_output <- rbind(nei_output, nei2010)


#2011 ####
breeding.data.2011 <- xdata[which(xdata$year == 2011),] 
breeding.data.2011 <- breeding.data.2011[!is.na(breeding.data.2011$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.2011 <- sf::st_as_sf(breeding.data.2011, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.2011))

territories <- sf::st_voronoi(sf::st_union(breeding.data.2011), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.2011))
breeding.ids.2011 <- breeding.data.2011[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.2011)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.2011 <- territories.list

#change NA ids to UNKNOWN 
neighbors.2011$Focal.male <- with(neighbors.2011, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.2011$Focal.female <- with(neighbors.2011, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.2011$Neighboring.male <- with(neighbors.2011, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.2011$Neighboring.female <- with(neighbors.2011, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N2011.b <- as.data.frame(with(neighbors.2011, paste(Focal.male, Neighboring.male, sep="_")))
names(N2011.b)[1] <- "ring_ring"
N2011.c <- as.data.frame(with(neighbors.2011, paste(Focal.male, Neighboring.female, sep="_")))
names(N2011.c)[1] <- "ring_ring"
N2011.d <- as.data.frame(with(neighbors.2011, paste(Focal.female, Neighboring.female, sep="_")))
names(N2011.d)[1] <- "ring_ring"
N2011.e <- as.data.frame(with(neighbors.2011, paste(Focal.female, Neighboring.male, sep="_")))
names(N2011.e)[1] <- "ring_ring"

N2011 <- rbind(N2011.b, N2011.c, N2011.d, N2011.e)
rm(N2011.b, N2011.c, N2011.d, N2011.e)
N2011$neighbors <- TRUE
N2011$Year.s <- 2012

N2011 <- N2011[!grepl("UNKNOWN", N2011$ring_ring),]

N_reference <- rbind(N_reference, N2011)


ydata <- base.fn.data[which(base.fn.data$year==2011),]


library(dplyr)

nei2011 <- neighbors.2011[,c(1,4)]

nei2011 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei2011


nei2011 <- tidyr::pivot_wider(nei2011, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei2011) <- paste0('N', colnames(nei2011))
names(nei2011)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 2011
as.data.frame(colnames(ydata))
par2011 <- ydata[,c(2,19,20)]
par2011 <- distinct(par2011, Box, .keep_all = TRUE)


####add neighbor ids
####2011
#N1
names(par2011)[names(par2011) == "Box"] <- "N1"
names(par2011)[names(par2011) == "Mother"] <- "N1.mother"
names(par2011)[names(par2011) == "Father"] <- "N1.father"
nei2011 <- merge(nei2011, par2011, by="N1", all.x=TRUE)
#N2
names(par2011)[names(par2011) == "N1"] <- "N2"
names(par2011)[names(par2011) == "N1.mother"] <- "N2.mother"
names(par2011)[names(par2011) == "N1.father"] <- "N2.father"
nei2011 <- merge(nei2011, par2011, by="N2", all.x=TRUE)
#N3
names(par2011)[names(par2011) == "N2"] <- "N3"
names(par2011)[names(par2011) == "N2.mother"] <- "N3.mother"
names(par2011)[names(par2011) == "N2.father"] <- "N3.father"
nei2011 <- merge(nei2011, par2011, by="N3", all.x=TRUE)
#N4
names(par2011)[names(par2011) == "N3"] <- "N4"
names(par2011)[names(par2011) == "N3.mother"] <- "N4.mother"
names(par2011)[names(par2011) == "N3.father"] <- "N4.father"
nei2011 <- merge(nei2011, par2011, by="N4", all.x=TRUE)
#N5
names(par2011)[names(par2011) == "N4"] <- "N5"
names(par2011)[names(par2011) == "N4.mother"] <- "N5.mother"
names(par2011)[names(par2011) == "N4.father"] <- "N5.father"
nei2011 <- merge(nei2011, par2011, by="N5", all.x=TRUE)
#N6
names(par2011)[names(par2011) == "N5"] <- "N6"
names(par2011)[names(par2011) == "N5.mother"] <- "N6.mother"
names(par2011)[names(par2011) == "N5.father"] <- "N6.father"
nei2011 <- merge(nei2011, par2011, by="N6", all.x=TRUE)
#N7
names(par2011)[names(par2011) == "N6"] <- "N7"
names(par2011)[names(par2011) == "N6.mother"] <- "N7.mother"
names(par2011)[names(par2011) == "N6.father"] <- "N7.father"
nei2011 <- merge(nei2011, par2011, by="N7", all.x=TRUE)
#N8
names(par2011)[names(par2011) == "N7"] <- "N8"
names(par2011)[names(par2011) == "N7.mother"] <- "N8.mother"
names(par2011)[names(par2011) == "N7.father"] <- "N8.father"
nei2011 <- merge(nei2011, par2011, by="N8", all.x=TRUE)
#N9
names(par2011)[names(par2011) == "N8"] <- "N9"
names(par2011)[names(par2011) == "N8.mother"] <- "N9.mother"
names(par2011)[names(par2011) == "N8.father"] <- "N9.father"
nei2011 <- merge(nei2011, par2011, by="N9", all.x=TRUE)
#N10
names(par2011)[names(par2011) == "N9"] <- "N10"
names(par2011)[names(par2011) == "N9.mother"] <- "N10.mother"
names(par2011)[names(par2011) == "N9.father"] <- "N10.father"
nei2011 <- merge(nei2011, par2011, by="N10", all.x=TRUE)

#and identifying column 
nei2011$year <- 2011
nei2011a  <-data.frame(nei2011,"box.year.parentid"=paste(nei2011$Focal.box, nei2011$year, "mother",sep="_")) 
nei2011b  <-data.frame(nei2011,"box.year.parentid"=paste(nei2011$Focal.box, nei2011$year, "father",sep="_")) 

nei2011 <- rbind(nei2011a, nei2011b)
rm(nei2011a, nei2011b)

nei2011$N10 <- NA

nei2011$N10.mother <- NA
nei2011$N10.father <- NA

nei2011 <- nei2011[,order(colnames(nei2011))]

nei_output <- rbind(nei_output, nei2011)

#2012 ####
breeding.data.2012 <- xdata[which(xdata$year == 2012),] 
breeding.data.2012 <- breeding.data.2012[!is.na(breeding.data.2012$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.2012 <- sf::st_as_sf(breeding.data.2012, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.2012))

territories <- sf::st_voronoi(sf::st_union(breeding.data.2012), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.2012))
breeding.ids.2012 <- breeding.data.2012[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.2012)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.2012 <- territories.list

#change NA ids to UNKNOWN 
neighbors.2012$Focal.male <- with(neighbors.2012, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.2012$Focal.female <- with(neighbors.2012, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.2012$Neighboring.male <- with(neighbors.2012, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.2012$Neighboring.female <- with(neighbors.2012, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N2012.b <- as.data.frame(with(neighbors.2012, paste(Focal.male, Neighboring.male, sep="_")))
names(N2012.b)[1] <- "ring_ring"
N2012.c <- as.data.frame(with(neighbors.2012, paste(Focal.male, Neighboring.female, sep="_")))
names(N2012.c)[1] <- "ring_ring"
N2012.d <- as.data.frame(with(neighbors.2012, paste(Focal.female, Neighboring.female, sep="_")))
names(N2012.d)[1] <- "ring_ring"
N2012.e <- as.data.frame(with(neighbors.2012, paste(Focal.female, Neighboring.male, sep="_")))
names(N2012.e)[1] <- "ring_ring"

N2012 <- rbind(N2012.b, N2012.c, N2012.d, N2012.e)
rm(N2012.b, N2012.c, N2012.d, N2012.e)
N2012$neighbors <- TRUE
N2012$Year.s <- 2013

N2012 <- N2012[!grepl("UNKNOWN", N2012$ring_ring),]

N_reference <- rbind(N_reference, N2012)


ydata <- base.fn.data[which(base.fn.data$year==2012),]


library(dplyr)

nei2012 <- neighbors.2012[,c(1,4)]

nei2012 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei2012


nei2012 <- tidyr::pivot_wider(nei2012, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei2012) <- paste0('N', colnames(nei2012))
names(nei2012)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 2012
as.data.frame(colnames(ydata))
par2012 <- ydata[,c(2,19,20)]
par2012 <- distinct(par2012, Box, .keep_all = TRUE)


####add neighbor ids
####2012
#N1
names(par2012)[names(par2012) == "Box"] <- "N1"
names(par2012)[names(par2012) == "Mother"] <- "N1.mother"
names(par2012)[names(par2012) == "Father"] <- "N1.father"
nei2012 <- merge(nei2012, par2012, by="N1", all.x=TRUE)
#N2
names(par2012)[names(par2012) == "N1"] <- "N2"
names(par2012)[names(par2012) == "N1.mother"] <- "N2.mother"
names(par2012)[names(par2012) == "N1.father"] <- "N2.father"
nei2012 <- merge(nei2012, par2012, by="N2", all.x=TRUE)
#N3
names(par2012)[names(par2012) == "N2"] <- "N3"
names(par2012)[names(par2012) == "N2.mother"] <- "N3.mother"
names(par2012)[names(par2012) == "N2.father"] <- "N3.father"
nei2012 <- merge(nei2012, par2012, by="N3", all.x=TRUE)
#N4
names(par2012)[names(par2012) == "N3"] <- "N4"
names(par2012)[names(par2012) == "N3.mother"] <- "N4.mother"
names(par2012)[names(par2012) == "N3.father"] <- "N4.father"
nei2012 <- merge(nei2012, par2012, by="N4", all.x=TRUE)
#N5
names(par2012)[names(par2012) == "N4"] <- "N5"
names(par2012)[names(par2012) == "N4.mother"] <- "N5.mother"
names(par2012)[names(par2012) == "N4.father"] <- "N5.father"
nei2012 <- merge(nei2012, par2012, by="N5", all.x=TRUE)
#N6
names(par2012)[names(par2012) == "N5"] <- "N6"
names(par2012)[names(par2012) == "N5.mother"] <- "N6.mother"
names(par2012)[names(par2012) == "N5.father"] <- "N6.father"
nei2012 <- merge(nei2012, par2012, by="N6", all.x=TRUE)
#N7
names(par2012)[names(par2012) == "N6"] <- "N7"
names(par2012)[names(par2012) == "N6.mother"] <- "N7.mother"
names(par2012)[names(par2012) == "N6.father"] <- "N7.father"
nei2012 <- merge(nei2012, par2012, by="N7", all.x=TRUE)
#N8
names(par2012)[names(par2012) == "N7"] <- "N8"
names(par2012)[names(par2012) == "N7.mother"] <- "N8.mother"
names(par2012)[names(par2012) == "N7.father"] <- "N8.father"
nei2012 <- merge(nei2012, par2012, by="N8", all.x=TRUE)
#N9
names(par2012)[names(par2012) == "N8"] <- "N9"
names(par2012)[names(par2012) == "N8.mother"] <- "N9.mother"
names(par2012)[names(par2012) == "N8.father"] <- "N9.father"
nei2012 <- merge(nei2012, par2012, by="N9", all.x=TRUE)
#N10
names(par2012)[names(par2012) == "N9"] <- "N10"
names(par2012)[names(par2012) == "N9.mother"] <- "N10.mother"
names(par2012)[names(par2012) == "N9.father"] <- "N10.father"
nei2012 <- merge(nei2012, par2012, by="N10", all.x=TRUE)

#and identifying column 
nei2012$year <- 2012
nei2012a  <-data.frame(nei2012,"box.year.parentid"=paste(nei2012$Focal.box, nei2012$year, "mother",sep="_")) 
nei2012b  <-data.frame(nei2012,"box.year.parentid"=paste(nei2012$Focal.box, nei2012$year, "father",sep="_")) 

nei2012 <- rbind(nei2012a, nei2012b)
rm(nei2012a, nei2012b)

nei2012$N9 <- NA 
nei2012$N10 <- NA

nei2012$N9.mother <- NA
nei2012$N9.father <- NA
nei2012$N10.mother <- NA
nei2012$N10.father <- NA

nei2012 <- nei2012[,order(colnames(nei2012))]

nei_output <- rbind(nei_output, nei2012)

#2013 ####
breeding.data.2013 <- xdata[which(xdata$year == 2013),] 
breeding.data.2013 <- breeding.data.2013[!is.na(breeding.data.2013$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.2013 <- sf::st_as_sf(breeding.data.2013, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.2013))

territories <- sf::st_voronoi(sf::st_union(breeding.data.2013), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.2013))
breeding.ids.2013 <- breeding.data.2013[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.2013)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.2013 <- territories.list

#change NA ids to UNKNOWN 
neighbors.2013$Focal.male <- with(neighbors.2013, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.2013$Focal.female <- with(neighbors.2013, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.2013$Neighboring.male <- with(neighbors.2013, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.2013$Neighboring.female <- with(neighbors.2013, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N2013.b <- as.data.frame(with(neighbors.2013, paste(Focal.male, Neighboring.male, sep="_")))
names(N2013.b)[1] <- "ring_ring"
N2013.c <- as.data.frame(with(neighbors.2013, paste(Focal.male, Neighboring.female, sep="_")))
names(N2013.c)[1] <- "ring_ring"
N2013.d <- as.data.frame(with(neighbors.2013, paste(Focal.female, Neighboring.female, sep="_")))
names(N2013.d)[1] <- "ring_ring"
N2013.e <- as.data.frame(with(neighbors.2013, paste(Focal.female, Neighboring.male, sep="_")))
names(N2013.e)[1] <- "ring_ring"

N2013 <- rbind(N2013.b, N2013.c, N2013.d, N2013.e)
rm(N2013.b, N2013.c, N2013.d, N2013.e)
N2013$neighbors <- TRUE
N2013$Year.s <- 2014

N2013 <- N2013[!grepl("UNKNOWN", N2013$ring_ring),]

N_reference <- rbind(N_reference, N2013)


ydata <- base.fn.data[which(base.fn.data$year==2013),]


library(dplyr)

nei2013 <- neighbors.2013[,c(1,4)]

nei2013 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei2013


nei2013 <- tidyr::pivot_wider(nei2013, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei2013) <- paste0('N', colnames(nei2013))
names(nei2013)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 2013
as.data.frame(colnames(ydata))
par2013 <- ydata[,c(2,19,20)]
par2013 <- distinct(par2013, Box, .keep_all = TRUE)


####add neighbor ids
####2013
#N1
names(par2013)[names(par2013) == "Box"] <- "N1"
names(par2013)[names(par2013) == "Mother"] <- "N1.mother"
names(par2013)[names(par2013) == "Father"] <- "N1.father"
nei2013 <- merge(nei2013, par2013, by="N1", all.x=TRUE)
#N2
names(par2013)[names(par2013) == "N1"] <- "N2"
names(par2013)[names(par2013) == "N1.mother"] <- "N2.mother"
names(par2013)[names(par2013) == "N1.father"] <- "N2.father"
nei2013 <- merge(nei2013, par2013, by="N2", all.x=TRUE)
#N3
names(par2013)[names(par2013) == "N2"] <- "N3"
names(par2013)[names(par2013) == "N2.mother"] <- "N3.mother"
names(par2013)[names(par2013) == "N2.father"] <- "N3.father"
nei2013 <- merge(nei2013, par2013, by="N3", all.x=TRUE)
#N4
names(par2013)[names(par2013) == "N3"] <- "N4"
names(par2013)[names(par2013) == "N3.mother"] <- "N4.mother"
names(par2013)[names(par2013) == "N3.father"] <- "N4.father"
nei2013 <- merge(nei2013, par2013, by="N4", all.x=TRUE)
#N5
names(par2013)[names(par2013) == "N4"] <- "N5"
names(par2013)[names(par2013) == "N4.mother"] <- "N5.mother"
names(par2013)[names(par2013) == "N4.father"] <- "N5.father"
nei2013 <- merge(nei2013, par2013, by="N5", all.x=TRUE)
#N6
names(par2013)[names(par2013) == "N5"] <- "N6"
names(par2013)[names(par2013) == "N5.mother"] <- "N6.mother"
names(par2013)[names(par2013) == "N5.father"] <- "N6.father"
nei2013 <- merge(nei2013, par2013, by="N6", all.x=TRUE)
#N7
names(par2013)[names(par2013) == "N6"] <- "N7"
names(par2013)[names(par2013) == "N6.mother"] <- "N7.mother"
names(par2013)[names(par2013) == "N6.father"] <- "N7.father"
nei2013 <- merge(nei2013, par2013, by="N7", all.x=TRUE)
#N8
names(par2013)[names(par2013) == "N7"] <- "N8"
names(par2013)[names(par2013) == "N7.mother"] <- "N8.mother"
names(par2013)[names(par2013) == "N7.father"] <- "N8.father"
nei2013 <- merge(nei2013, par2013, by="N8", all.x=TRUE)
#N9
names(par2013)[names(par2013) == "N8"] <- "N9"
names(par2013)[names(par2013) == "N8.mother"] <- "N9.mother"
names(par2013)[names(par2013) == "N8.father"] <- "N9.father"
nei2013 <- merge(nei2013, par2013, by="N9", all.x=TRUE)
#N10
names(par2013)[names(par2013) == "N9"] <- "N10"
names(par2013)[names(par2013) == "N9.mother"] <- "N10.mother"
names(par2013)[names(par2013) == "N9.father"] <- "N10.father"
nei2013 <- merge(nei2013, par2013, by="N10", all.x=TRUE)

#and identifying column 
nei2013$year <- 2013
nei2013a  <-data.frame(nei2013,"box.year.parentid"=paste(nei2013$Focal.box, nei2013$year, "mother",sep="_")) 
nei2013b  <-data.frame(nei2013,"box.year.parentid"=paste(nei2013$Focal.box, nei2013$year, "father",sep="_")) 

nei2013 <- rbind(nei2013a, nei2013b)
rm(nei2013a, nei2013b)

nei2013$N9 <- NA 
nei2013$N10 <- NA

nei2013$N9.mother <- NA
nei2013$N9.father <- NA
nei2013$N10.mother <- NA
nei2013$N10.father <- NA

nei2013 <- nei2013[,order(colnames(nei2013))]

nei_output <- rbind(nei_output, nei2013)

#2014 ####
breeding.data.2014 <- xdata[which(xdata$year == 2014),] 
breeding.data.2014 <- breeding.data.2014[!is.na(breeding.data.2014$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.2014 <- sf::st_as_sf(breeding.data.2014, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.2014))

territories <- sf::st_voronoi(sf::st_union(breeding.data.2014), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.2014))
breeding.ids.2014 <- breeding.data.2014[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.2014)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.2014 <- territories.list

#change NA ids to UNKNOWN 
neighbors.2014$Focal.male <- with(neighbors.2014, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.2014$Focal.female <- with(neighbors.2014, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.2014$Neighboring.male <- with(neighbors.2014, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.2014$Neighboring.female <- with(neighbors.2014, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N2014.b <- as.data.frame(with(neighbors.2014, paste(Focal.male, Neighboring.male, sep="_")))
names(N2014.b)[1] <- "ring_ring"
N2014.c <- as.data.frame(with(neighbors.2014, paste(Focal.male, Neighboring.female, sep="_")))
names(N2014.c)[1] <- "ring_ring"
N2014.d <- as.data.frame(with(neighbors.2014, paste(Focal.female, Neighboring.female, sep="_")))
names(N2014.d)[1] <- "ring_ring"
N2014.e <- as.data.frame(with(neighbors.2014, paste(Focal.female, Neighboring.male, sep="_")))
names(N2014.e)[1] <- "ring_ring"

N2014 <- rbind(N2014.b, N2014.c, N2014.d, N2014.e)
rm(N2014.b, N2014.c, N2014.d, N2014.e)
N2014$neighbors <- TRUE
N2014$Year.s <- 2015

N2014 <- N2014[!grepl("UNKNOWN", N2014$ring_ring),]

N_reference <- rbind(N_reference, N2014)


ydata <- base.fn.data[which(base.fn.data$year==2014),]


library(dplyr)

nei2014 <- neighbors.2014[,c(1,4)]

nei2014 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei2014


nei2014 <- tidyr::pivot_wider(nei2014, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei2014) <- paste0('N', colnames(nei2014))
names(nei2014)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 2014
as.data.frame(colnames(ydata))
par2014 <- ydata[,c(2,19,20)]
par2014 <- distinct(par2014, Box, .keep_all = TRUE)


####add neighbor ids
####2014
#N1
names(par2014)[names(par2014) == "Box"] <- "N1"
names(par2014)[names(par2014) == "Mother"] <- "N1.mother"
names(par2014)[names(par2014) == "Father"] <- "N1.father"
nei2014 <- merge(nei2014, par2014, by="N1", all.x=TRUE)
#N2
names(par2014)[names(par2014) == "N1"] <- "N2"
names(par2014)[names(par2014) == "N1.mother"] <- "N2.mother"
names(par2014)[names(par2014) == "N1.father"] <- "N2.father"
nei2014 <- merge(nei2014, par2014, by="N2", all.x=TRUE)
#N3
names(par2014)[names(par2014) == "N2"] <- "N3"
names(par2014)[names(par2014) == "N2.mother"] <- "N3.mother"
names(par2014)[names(par2014) == "N2.father"] <- "N3.father"
nei2014 <- merge(nei2014, par2014, by="N3", all.x=TRUE)
#N4
names(par2014)[names(par2014) == "N3"] <- "N4"
names(par2014)[names(par2014) == "N3.mother"] <- "N4.mother"
names(par2014)[names(par2014) == "N3.father"] <- "N4.father"
nei2014 <- merge(nei2014, par2014, by="N4", all.x=TRUE)
#N5
names(par2014)[names(par2014) == "N4"] <- "N5"
names(par2014)[names(par2014) == "N4.mother"] <- "N5.mother"
names(par2014)[names(par2014) == "N4.father"] <- "N5.father"
nei2014 <- merge(nei2014, par2014, by="N5", all.x=TRUE)
#N6
names(par2014)[names(par2014) == "N5"] <- "N6"
names(par2014)[names(par2014) == "N5.mother"] <- "N6.mother"
names(par2014)[names(par2014) == "N5.father"] <- "N6.father"
nei2014 <- merge(nei2014, par2014, by="N6", all.x=TRUE)
#N7
names(par2014)[names(par2014) == "N6"] <- "N7"
names(par2014)[names(par2014) == "N6.mother"] <- "N7.mother"
names(par2014)[names(par2014) == "N6.father"] <- "N7.father"
nei2014 <- merge(nei2014, par2014, by="N7", all.x=TRUE)
#N8
names(par2014)[names(par2014) == "N7"] <- "N8"
names(par2014)[names(par2014) == "N7.mother"] <- "N8.mother"
names(par2014)[names(par2014) == "N7.father"] <- "N8.father"
nei2014 <- merge(nei2014, par2014, by="N8", all.x=TRUE)
#N9
names(par2014)[names(par2014) == "N8"] <- "N9"
names(par2014)[names(par2014) == "N8.mother"] <- "N9.mother"
names(par2014)[names(par2014) == "N8.father"] <- "N9.father"
nei2014 <- merge(nei2014, par2014, by="N9", all.x=TRUE)
#N10
names(par2014)[names(par2014) == "N9"] <- "N10"
names(par2014)[names(par2014) == "N9.mother"] <- "N10.mother"
names(par2014)[names(par2014) == "N9.father"] <- "N10.father"
nei2014 <- merge(nei2014, par2014, by="N10", all.x=TRUE)

#and identifying column 
nei2014$year <- 2014
nei2014a  <-data.frame(nei2014,"box.year.parentid"=paste(nei2014$Focal.box, nei2014$year, "mother",sep="_")) 
nei2014b  <-data.frame(nei2014,"box.year.parentid"=paste(nei2014$Focal.box, nei2014$year, "father",sep="_")) 

nei2014 <- rbind(nei2014a, nei2014b)
rm(nei2014a, nei2014b)

nei2014$N9 <- NA 
nei2014$N10 <- NA

nei2014$N9.mother <- NA
nei2014$N9.father <- NA
nei2014$N10.mother <- NA
nei2014$N10.father <- NA

nei2014 <- nei2014[,order(colnames(nei2014))]

nei_output <- rbind(nei_output, nei2014)

#2015 ####
breeding.data.2015 <- xdata[which(xdata$year == 2015),] 
breeding.data.2015 <- breeding.data.2015[!is.na(breeding.data.2015$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.2015 <- sf::st_as_sf(breeding.data.2015, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.2015))

territories <- sf::st_voronoi(sf::st_union(breeding.data.2015), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.2015))
breeding.ids.2015 <- breeding.data.2015[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.2015)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.2015 <- territories.list

#change NA ids to UNKNOWN 
neighbors.2015$Focal.male <- with(neighbors.2015, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.2015$Focal.female <- with(neighbors.2015, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.2015$Neighboring.male <- with(neighbors.2015, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.2015$Neighboring.female <- with(neighbors.2015, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N2015.b <- as.data.frame(with(neighbors.2015, paste(Focal.male, Neighboring.male, sep="_")))
names(N2015.b)[1] <- "ring_ring"
N2015.c <- as.data.frame(with(neighbors.2015, paste(Focal.male, Neighboring.female, sep="_")))
names(N2015.c)[1] <- "ring_ring"
N2015.d <- as.data.frame(with(neighbors.2015, paste(Focal.female, Neighboring.female, sep="_")))
names(N2015.d)[1] <- "ring_ring"
N2015.e <- as.data.frame(with(neighbors.2015, paste(Focal.female, Neighboring.male, sep="_")))
names(N2015.e)[1] <- "ring_ring"

N2015 <- rbind(N2015.b, N2015.c, N2015.d, N2015.e)
rm(N2015.b, N2015.c, N2015.d, N2015.e)
N2015$neighbors <- TRUE
N2015$Year.s <- 2016

N2015 <- N2015[!grepl("UNKNOWN", N2015$ring_ring),]

N_reference <- rbind(N_reference, N2015)


ydata <- base.fn.data[which(base.fn.data$year==2015),]


library(dplyr)

nei2015 <- neighbors.2015[,c(1,4)]

nei2015 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei2015


nei2015 <- tidyr::pivot_wider(nei2015, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei2015) <- paste0('N', colnames(nei2015))
names(nei2015)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 2015
as.data.frame(colnames(ydata))
par2015 <- ydata[,c(2,19,20)]
par2015 <- distinct(par2015, Box, .keep_all = TRUE)


####add neighbor ids
####2015
#N1
names(par2015)[names(par2015) == "Box"] <- "N1"
names(par2015)[names(par2015) == "Mother"] <- "N1.mother"
names(par2015)[names(par2015) == "Father"] <- "N1.father"
nei2015 <- merge(nei2015, par2015, by="N1", all.x=TRUE)
#N2
names(par2015)[names(par2015) == "N1"] <- "N2"
names(par2015)[names(par2015) == "N1.mother"] <- "N2.mother"
names(par2015)[names(par2015) == "N1.father"] <- "N2.father"
nei2015 <- merge(nei2015, par2015, by="N2", all.x=TRUE)
#N3
names(par2015)[names(par2015) == "N2"] <- "N3"
names(par2015)[names(par2015) == "N2.mother"] <- "N3.mother"
names(par2015)[names(par2015) == "N2.father"] <- "N3.father"
nei2015 <- merge(nei2015, par2015, by="N3", all.x=TRUE)
#N4
names(par2015)[names(par2015) == "N3"] <- "N4"
names(par2015)[names(par2015) == "N3.mother"] <- "N4.mother"
names(par2015)[names(par2015) == "N3.father"] <- "N4.father"
nei2015 <- merge(nei2015, par2015, by="N4", all.x=TRUE)
#N5
names(par2015)[names(par2015) == "N4"] <- "N5"
names(par2015)[names(par2015) == "N4.mother"] <- "N5.mother"
names(par2015)[names(par2015) == "N4.father"] <- "N5.father"
nei2015 <- merge(nei2015, par2015, by="N5", all.x=TRUE)
#N6
names(par2015)[names(par2015) == "N5"] <- "N6"
names(par2015)[names(par2015) == "N5.mother"] <- "N6.mother"
names(par2015)[names(par2015) == "N5.father"] <- "N6.father"
nei2015 <- merge(nei2015, par2015, by="N6", all.x=TRUE)
#N7
names(par2015)[names(par2015) == "N6"] <- "N7"
names(par2015)[names(par2015) == "N6.mother"] <- "N7.mother"
names(par2015)[names(par2015) == "N6.father"] <- "N7.father"
nei2015 <- merge(nei2015, par2015, by="N7", all.x=TRUE)
#N8
names(par2015)[names(par2015) == "N7"] <- "N8"
names(par2015)[names(par2015) == "N7.mother"] <- "N8.mother"
names(par2015)[names(par2015) == "N7.father"] <- "N8.father"
nei2015 <- merge(nei2015, par2015, by="N8", all.x=TRUE)
#N9
names(par2015)[names(par2015) == "N8"] <- "N9"
names(par2015)[names(par2015) == "N8.mother"] <- "N9.mother"
names(par2015)[names(par2015) == "N8.father"] <- "N9.father"
nei2015 <- merge(nei2015, par2015, by="N9", all.x=TRUE)
#N10
names(par2015)[names(par2015) == "N9"] <- "N10"
names(par2015)[names(par2015) == "N9.mother"] <- "N10.mother"
names(par2015)[names(par2015) == "N9.father"] <- "N10.father"
nei2015 <- merge(nei2015, par2015, by="N10", all.x=TRUE)

nei2015$year <- 2015
nei2015a  <-data.frame(nei2015,"box.year.parentid"=paste(nei2015$Focal.box, nei2015$year, "mother",sep="_")) 
nei2015b  <-data.frame(nei2015,"box.year.parentid"=paste(nei2015$Focal.box, nei2015$year, "father",sep="_")) 

nei2015 <- rbind(nei2015a, nei2015b)
rm(nei2015a, nei2015b)

nei2015$N10 <- NA

nei2015$N10.mother <- NA
nei2015$N10.father <- NA

nei2015 <- nei2015[,order(colnames(nei2015))]

nei_output <- rbind(nei_output, nei2015)


#2016 ####
breeding.data.2016 <- xdata[which(xdata$year == 2016),] 
breeding.data.2016 <- breeding.data.2016[!is.na(breeding.data.2016$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.2016 <- sf::st_as_sf(breeding.data.2016, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.2016))

territories <- sf::st_voronoi(sf::st_union(breeding.data.2016), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.2016))
breeding.ids.2016 <- breeding.data.2016[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.2016)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.2016 <- territories.list

#change NA ids to UNKNOWN 
neighbors.2016$Focal.male <- with(neighbors.2016, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.2016$Focal.female <- with(neighbors.2016, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.2016$Neighboring.male <- with(neighbors.2016, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.2016$Neighboring.female <- with(neighbors.2016, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N2016.b <- as.data.frame(with(neighbors.2016, paste(Focal.male, Neighboring.male, sep="_")))
names(N2016.b)[1] <- "ring_ring"
N2016.c <- as.data.frame(with(neighbors.2016, paste(Focal.male, Neighboring.female, sep="_")))
names(N2016.c)[1] <- "ring_ring"
N2016.d <- as.data.frame(with(neighbors.2016, paste(Focal.female, Neighboring.female, sep="_")))
names(N2016.d)[1] <- "ring_ring"
N2016.e <- as.data.frame(with(neighbors.2016, paste(Focal.female, Neighboring.male, sep="_")))
names(N2016.e)[1] <- "ring_ring"

N2016 <- rbind(N2016.b, N2016.c, N2016.d, N2016.e)
rm(N2016.b, N2016.c, N2016.d, N2016.e)
N2016$neighbors <- TRUE
N2016$Year.s <- 2017

N2016 <- N2016[!grepl("UNKNOWN", N2016$ring_ring),]

N_reference <- rbind(N_reference, N2016)


ydata <- base.fn.data[which(base.fn.data$year==2016),]


library(dplyr)

nei2016 <- neighbors.2016[,c(1,4)]

nei2016 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei2016


nei2016 <- tidyr::pivot_wider(nei2016, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei2016) <- paste0('N', colnames(nei2016))
names(nei2016)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 2016
as.data.frame(colnames(ydata))
par2016 <- ydata[,c(2,19,20)]
par2016 <- distinct(par2016, Box, .keep_all = TRUE)


####add neighbor ids
####2016
#N1
names(par2016)[names(par2016) == "Box"] <- "N1"
names(par2016)[names(par2016) == "Mother"] <- "N1.mother"
names(par2016)[names(par2016) == "Father"] <- "N1.father"
nei2016 <- merge(nei2016, par2016, by="N1", all.x=TRUE)
#N2
names(par2016)[names(par2016) == "N1"] <- "N2"
names(par2016)[names(par2016) == "N1.mother"] <- "N2.mother"
names(par2016)[names(par2016) == "N1.father"] <- "N2.father"
nei2016 <- merge(nei2016, par2016, by="N2", all.x=TRUE)
#N3
names(par2016)[names(par2016) == "N2"] <- "N3"
names(par2016)[names(par2016) == "N2.mother"] <- "N3.mother"
names(par2016)[names(par2016) == "N2.father"] <- "N3.father"
nei2016 <- merge(nei2016, par2016, by="N3", all.x=TRUE)
#N4
names(par2016)[names(par2016) == "N3"] <- "N4"
names(par2016)[names(par2016) == "N3.mother"] <- "N4.mother"
names(par2016)[names(par2016) == "N3.father"] <- "N4.father"
nei2016 <- merge(nei2016, par2016, by="N4", all.x=TRUE)
#N5
names(par2016)[names(par2016) == "N4"] <- "N5"
names(par2016)[names(par2016) == "N4.mother"] <- "N5.mother"
names(par2016)[names(par2016) == "N4.father"] <- "N5.father"
nei2016 <- merge(nei2016, par2016, by="N5", all.x=TRUE)
#N6
names(par2016)[names(par2016) == "N5"] <- "N6"
names(par2016)[names(par2016) == "N5.mother"] <- "N6.mother"
names(par2016)[names(par2016) == "N5.father"] <- "N6.father"
nei2016 <- merge(nei2016, par2016, by="N6", all.x=TRUE)
#N7
names(par2016)[names(par2016) == "N6"] <- "N7"
names(par2016)[names(par2016) == "N6.mother"] <- "N7.mother"
names(par2016)[names(par2016) == "N6.father"] <- "N7.father"
nei2016 <- merge(nei2016, par2016, by="N7", all.x=TRUE)
#N8
names(par2016)[names(par2016) == "N7"] <- "N8"
names(par2016)[names(par2016) == "N7.mother"] <- "N8.mother"
names(par2016)[names(par2016) == "N7.father"] <- "N8.father"
nei2016 <- merge(nei2016, par2016, by="N8", all.x=TRUE)
#N9
names(par2016)[names(par2016) == "N8"] <- "N9"
names(par2016)[names(par2016) == "N8.mother"] <- "N9.mother"
names(par2016)[names(par2016) == "N8.father"] <- "N9.father"
nei2016 <- merge(nei2016, par2016, by="N9", all.x=TRUE)
#N10
names(par2016)[names(par2016) == "N9"] <- "N10"
names(par2016)[names(par2016) == "N9.mother"] <- "N10.mother"
names(par2016)[names(par2016) == "N9.father"] <- "N10.father"
nei2016 <- merge(nei2016, par2016, by="N10", all.x=TRUE)

#and identifying column 
nei2016$year <- 2016
nei2016a  <-data.frame(nei2016,"box.year.parentid"=paste(nei2016$Focal.box, nei2016$year, "mother",sep="_")) 
nei2016b  <-data.frame(nei2016,"box.year.parentid"=paste(nei2016$Focal.box, nei2016$year, "father",sep="_")) 

nei2016 <- rbind(nei2016a, nei2016b)
rm(nei2016a, nei2016b)

nei2016$N9 <- NA 
nei2016$N10 <- NA

nei2016$N9.mother <- NA
nei2016$N9.father <- NA
nei2016$N10.mother <- NA
nei2016$N10.father <- NA

nei2016 <- nei2016[,order(colnames(nei2016))]

nei_output <- rbind(nei_output, nei2016)

#2017 ####
breeding.data.2017 <- xdata[which(xdata$year == 2017),] 
breeding.data.2017 <- breeding.data.2017[!is.na(breeding.data.2017$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.2017 <- sf::st_as_sf(breeding.data.2017, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.2017))

territories <- sf::st_voronoi(sf::st_union(breeding.data.2017), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.2017))
breeding.ids.2017 <- breeding.data.2017[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.2017)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.2017 <- territories.list

#change NA ids to UNKNOWN 
neighbors.2017$Focal.male <- with(neighbors.2017, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.2017$Focal.female <- with(neighbors.2017, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.2017$Neighboring.male <- with(neighbors.2017, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.2017$Neighboring.female <- with(neighbors.2017, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N2017.b <- as.data.frame(with(neighbors.2017, paste(Focal.male, Neighboring.male, sep="_")))
names(N2017.b)[1] <- "ring_ring"
N2017.c <- as.data.frame(with(neighbors.2017, paste(Focal.male, Neighboring.female, sep="_")))
names(N2017.c)[1] <- "ring_ring"
N2017.d <- as.data.frame(with(neighbors.2017, paste(Focal.female, Neighboring.female, sep="_")))
names(N2017.d)[1] <- "ring_ring"
N2017.e <- as.data.frame(with(neighbors.2017, paste(Focal.female, Neighboring.male, sep="_")))
names(N2017.e)[1] <- "ring_ring"

N2017 <- rbind(N2017.b, N2017.c, N2017.d, N2017.e)
rm(N2017.b, N2017.c, N2017.d, N2017.e)
N2017$neighbors <- TRUE
N2017$Year.s <- 2018

N2017 <- N2017[!grepl("UNKNOWN", N2017$ring_ring),]

N_reference <- rbind(N_reference, N2017)


ydata <- base.fn.data[which(base.fn.data$year==2017),]


library(dplyr)

nei2017 <- neighbors.2017[,c(1,4)]

nei2017 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei2017


nei2017 <- tidyr::pivot_wider(nei2017, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei2017) <- paste0('N', colnames(nei2017))
names(nei2017)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 2017
as.data.frame(colnames(ydata))
par2017 <- ydata[,c(2,19,20)]
par2017 <- distinct(par2017, Box, .keep_all = TRUE)


####add neighbor ids
####2017
#N1
names(par2017)[names(par2017) == "Box"] <- "N1"
names(par2017)[names(par2017) == "Mother"] <- "N1.mother"
names(par2017)[names(par2017) == "Father"] <- "N1.father"
nei2017 <- merge(nei2017, par2017, by="N1", all.x=TRUE)
#N2
names(par2017)[names(par2017) == "N1"] <- "N2"
names(par2017)[names(par2017) == "N1.mother"] <- "N2.mother"
names(par2017)[names(par2017) == "N1.father"] <- "N2.father"
nei2017 <- merge(nei2017, par2017, by="N2", all.x=TRUE)
#N3
names(par2017)[names(par2017) == "N2"] <- "N3"
names(par2017)[names(par2017) == "N2.mother"] <- "N3.mother"
names(par2017)[names(par2017) == "N2.father"] <- "N3.father"
nei2017 <- merge(nei2017, par2017, by="N3", all.x=TRUE)
#N4
names(par2017)[names(par2017) == "N3"] <- "N4"
names(par2017)[names(par2017) == "N3.mother"] <- "N4.mother"
names(par2017)[names(par2017) == "N3.father"] <- "N4.father"
nei2017 <- merge(nei2017, par2017, by="N4", all.x=TRUE)
#N5
names(par2017)[names(par2017) == "N4"] <- "N5"
names(par2017)[names(par2017) == "N4.mother"] <- "N5.mother"
names(par2017)[names(par2017) == "N4.father"] <- "N5.father"
nei2017 <- merge(nei2017, par2017, by="N5", all.x=TRUE)
#N6
names(par2017)[names(par2017) == "N5"] <- "N6"
names(par2017)[names(par2017) == "N5.mother"] <- "N6.mother"
names(par2017)[names(par2017) == "N5.father"] <- "N6.father"
nei2017 <- merge(nei2017, par2017, by="N6", all.x=TRUE)
#N7
names(par2017)[names(par2017) == "N6"] <- "N7"
names(par2017)[names(par2017) == "N6.mother"] <- "N7.mother"
names(par2017)[names(par2017) == "N6.father"] <- "N7.father"
nei2017 <- merge(nei2017, par2017, by="N7", all.x=TRUE)
#N8
names(par2017)[names(par2017) == "N7"] <- "N8"
names(par2017)[names(par2017) == "N7.mother"] <- "N8.mother"
names(par2017)[names(par2017) == "N7.father"] <- "N8.father"
nei2017 <- merge(nei2017, par2017, by="N8", all.x=TRUE)
#N9
names(par2017)[names(par2017) == "N8"] <- "N9"
names(par2017)[names(par2017) == "N8.mother"] <- "N9.mother"
names(par2017)[names(par2017) == "N8.father"] <- "N9.father"
nei2017 <- merge(nei2017, par2017, by="N9", all.x=TRUE)
#N10
names(par2017)[names(par2017) == "N9"] <- "N10"
names(par2017)[names(par2017) == "N9.mother"] <- "N10.mother"
names(par2017)[names(par2017) == "N9.father"] <- "N10.father"
nei2017 <- merge(nei2017, par2017, by="N10", all.x=TRUE)

#and identifying column 
nei2017$year <- 2017
nei2017a  <-data.frame(nei2017,"box.year.parentid"=paste(nei2017$Focal.box, nei2017$year, "mother",sep="_")) 
nei2017b  <-data.frame(nei2017,"box.year.parentid"=paste(nei2017$Focal.box, nei2017$year, "father",sep="_")) 

nei2017 <- rbind(nei2017a, nei2017b)
rm(nei2017a, nei2017b)

nei2017$N9 <- NA 
nei2017$N10 <- NA

nei2017$N9.mother <- NA
nei2017$N9.father <- NA
nei2017$N10.mother <- NA
nei2017$N10.father <- NA

nei2017 <- nei2017[,order(colnames(nei2017))]

nei_output <- rbind(nei_output, nei2017)

#2018 ####
breeding.data.2018 <- xdata[which(xdata$year == 2018),] 
breeding.data.2018 <- breeding.data.2018[!is.na(breeding.data.2018$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.2018 <- sf::st_as_sf(breeding.data.2018, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.2018))

territories <- sf::st_voronoi(sf::st_union(breeding.data.2018), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.2018))
breeding.ids.2018 <- breeding.data.2018[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.2018)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.2018 <- territories.list

#change NA ids to UNKNOWN 
neighbors.2018$Focal.male <- with(neighbors.2018, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.2018$Focal.female <- with(neighbors.2018, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.2018$Neighboring.male <- with(neighbors.2018, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.2018$Neighboring.female <- with(neighbors.2018, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N2018.b <- as.data.frame(with(neighbors.2018, paste(Focal.male, Neighboring.male, sep="_")))
names(N2018.b)[1] <- "ring_ring"
N2018.c <- as.data.frame(with(neighbors.2018, paste(Focal.male, Neighboring.female, sep="_")))
names(N2018.c)[1] <- "ring_ring"
N2018.d <- as.data.frame(with(neighbors.2018, paste(Focal.female, Neighboring.female, sep="_")))
names(N2018.d)[1] <- "ring_ring"
N2018.e <- as.data.frame(with(neighbors.2018, paste(Focal.female, Neighboring.male, sep="_")))
names(N2018.e)[1] <- "ring_ring"

N2018 <- rbind(N2018.b, N2018.c, N2018.d, N2018.e)
rm(N2018.b, N2018.c, N2018.d, N2018.e)
N2018$neighbors <- TRUE
N2018$Year.s <- 2019

N2018 <- N2018[!grepl("UNKNOWN", N2018$ring_ring),]

N_reference <- rbind(N_reference, N2018)


ydata <- base.fn.data[which(base.fn.data$year==2018),]


library(dplyr)

nei2018 <- neighbors.2018[,c(1,4)]

nei2018 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei2018


nei2018 <- tidyr::pivot_wider(nei2018, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei2018) <- paste0('N', colnames(nei2018))
names(nei2018)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 2018
as.data.frame(colnames(ydata))
par2018 <- ydata[,c(2,19,20)]
par2018 <- distinct(par2018, Box, .keep_all = TRUE)


####add neighbor ids
####2018
#N1
names(par2018)[names(par2018) == "Box"] <- "N1"
names(par2018)[names(par2018) == "Mother"] <- "N1.mother"
names(par2018)[names(par2018) == "Father"] <- "N1.father"
nei2018 <- merge(nei2018, par2018, by="N1", all.x=TRUE)
#N2
names(par2018)[names(par2018) == "N1"] <- "N2"
names(par2018)[names(par2018) == "N1.mother"] <- "N2.mother"
names(par2018)[names(par2018) == "N1.father"] <- "N2.father"
nei2018 <- merge(nei2018, par2018, by="N2", all.x=TRUE)
#N3
names(par2018)[names(par2018) == "N2"] <- "N3"
names(par2018)[names(par2018) == "N2.mother"] <- "N3.mother"
names(par2018)[names(par2018) == "N2.father"] <- "N3.father"
nei2018 <- merge(nei2018, par2018, by="N3", all.x=TRUE)
#N4
names(par2018)[names(par2018) == "N3"] <- "N4"
names(par2018)[names(par2018) == "N3.mother"] <- "N4.mother"
names(par2018)[names(par2018) == "N3.father"] <- "N4.father"
nei2018 <- merge(nei2018, par2018, by="N4", all.x=TRUE)
#N5
names(par2018)[names(par2018) == "N4"] <- "N5"
names(par2018)[names(par2018) == "N4.mother"] <- "N5.mother"
names(par2018)[names(par2018) == "N4.father"] <- "N5.father"
nei2018 <- merge(nei2018, par2018, by="N5", all.x=TRUE)
#N6
names(par2018)[names(par2018) == "N5"] <- "N6"
names(par2018)[names(par2018) == "N5.mother"] <- "N6.mother"
names(par2018)[names(par2018) == "N5.father"] <- "N6.father"
nei2018 <- merge(nei2018, par2018, by="N6", all.x=TRUE)
#N7
names(par2018)[names(par2018) == "N6"] <- "N7"
names(par2018)[names(par2018) == "N6.mother"] <- "N7.mother"
names(par2018)[names(par2018) == "N6.father"] <- "N7.father"
nei2018 <- merge(nei2018, par2018, by="N7", all.x=TRUE)
#N8
names(par2018)[names(par2018) == "N7"] <- "N8"
names(par2018)[names(par2018) == "N7.mother"] <- "N8.mother"
names(par2018)[names(par2018) == "N7.father"] <- "N8.father"
nei2018 <- merge(nei2018, par2018, by="N8", all.x=TRUE)
#N9
names(par2018)[names(par2018) == "N8"] <- "N9"
names(par2018)[names(par2018) == "N8.mother"] <- "N9.mother"
names(par2018)[names(par2018) == "N8.father"] <- "N9.father"
nei2018 <- merge(nei2018, par2018, by="N9", all.x=TRUE)
#N10
names(par2018)[names(par2018) == "N9"] <- "N10"
names(par2018)[names(par2018) == "N9.mother"] <- "N10.mother"
names(par2018)[names(par2018) == "N9.father"] <- "N10.father"
nei2018 <- merge(nei2018, par2018, by="N10", all.x=TRUE)

#and identifying column 
nei2018$year <- 2018
nei2018a  <-data.frame(nei2018,"box.year.parentid"=paste(nei2018$Focal.box, nei2018$year, "mother",sep="_")) 
nei2018b  <-data.frame(nei2018,"box.year.parentid"=paste(nei2018$Focal.box, nei2018$year, "father",sep="_")) 

nei2018 <- rbind(nei2018a, nei2018b)
rm(nei2018a, nei2018b)

nei2018$N10 <- NA

nei2018$N10.mother <- NA
nei2018$N10.father <- NA

nei2018 <- nei2018[,order(colnames(nei2018))]

nei_output <- rbind(nei_output, nei2018)

#2019 ####
breeding.data.2019 <- xdata[which(xdata$year == 2019),] 
breeding.data.2019 <- breeding.data.2019[!is.na(breeding.data.2019$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.2019 <- sf::st_as_sf(breeding.data.2019, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.2019))

territories <- sf::st_voronoi(sf::st_union(breeding.data.2019), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.2019))
breeding.ids.2019 <- breeding.data.2019[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.2019)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.2019 <- territories.list

#change NA ids to UNKNOWN 
neighbors.2019$Focal.male <- with(neighbors.2019, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.2019$Focal.female <- with(neighbors.2019, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.2019$Neighboring.male <- with(neighbors.2019, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.2019$Neighboring.female <- with(neighbors.2019, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N2019.b <- as.data.frame(with(neighbors.2019, paste(Focal.male, Neighboring.male, sep="_")))
names(N2019.b)[1] <- "ring_ring"
N2019.c <- as.data.frame(with(neighbors.2019, paste(Focal.male, Neighboring.female, sep="_")))
names(N2019.c)[1] <- "ring_ring"
N2019.d <- as.data.frame(with(neighbors.2019, paste(Focal.female, Neighboring.female, sep="_")))
names(N2019.d)[1] <- "ring_ring"
N2019.e <- as.data.frame(with(neighbors.2019, paste(Focal.female, Neighboring.male, sep="_")))
names(N2019.e)[1] <- "ring_ring"

N2019 <- rbind(N2019.b, N2019.c, N2019.d, N2019.e)
rm(N2019.b, N2019.c, N2019.d, N2019.e)
N2019$neighbors <- TRUE
N2019$Year.s <- 2020

N2019 <- N2019[!grepl("UNKNOWN", N2019$ring_ring),]

N_reference <- rbind(N_reference, N2019)


ydata <- base.fn.data[which(base.fn.data$year==2019),]


library(dplyr)

nei2019 <- neighbors.2019[,c(1,4)]

nei2019 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei2019


nei2019 <- tidyr::pivot_wider(nei2019, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei2019) <- paste0('N', colnames(nei2019))
names(nei2019)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 2019
as.data.frame(colnames(ydata))
par2019 <- ydata[,c(2,19,20)]
par2019 <- distinct(par2019, Box, .keep_all = TRUE)


####add neighbor ids
####2019
#N1
names(par2019)[names(par2019) == "Box"] <- "N1"
names(par2019)[names(par2019) == "Mother"] <- "N1.mother"
names(par2019)[names(par2019) == "Father"] <- "N1.father"
nei2019 <- merge(nei2019, par2019, by="N1", all.x=TRUE)
#N2
names(par2019)[names(par2019) == "N1"] <- "N2"
names(par2019)[names(par2019) == "N1.mother"] <- "N2.mother"
names(par2019)[names(par2019) == "N1.father"] <- "N2.father"
nei2019 <- merge(nei2019, par2019, by="N2", all.x=TRUE)
#N3
names(par2019)[names(par2019) == "N2"] <- "N3"
names(par2019)[names(par2019) == "N2.mother"] <- "N3.mother"
names(par2019)[names(par2019) == "N2.father"] <- "N3.father"
nei2019 <- merge(nei2019, par2019, by="N3", all.x=TRUE)
#N4
names(par2019)[names(par2019) == "N3"] <- "N4"
names(par2019)[names(par2019) == "N3.mother"] <- "N4.mother"
names(par2019)[names(par2019) == "N3.father"] <- "N4.father"
nei2019 <- merge(nei2019, par2019, by="N4", all.x=TRUE)
#N5
names(par2019)[names(par2019) == "N4"] <- "N5"
names(par2019)[names(par2019) == "N4.mother"] <- "N5.mother"
names(par2019)[names(par2019) == "N4.father"] <- "N5.father"
nei2019 <- merge(nei2019, par2019, by="N5", all.x=TRUE)
#N6
names(par2019)[names(par2019) == "N5"] <- "N6"
names(par2019)[names(par2019) == "N5.mother"] <- "N6.mother"
names(par2019)[names(par2019) == "N5.father"] <- "N6.father"
nei2019 <- merge(nei2019, par2019, by="N6", all.x=TRUE)
#N7
names(par2019)[names(par2019) == "N6"] <- "N7"
names(par2019)[names(par2019) == "N6.mother"] <- "N7.mother"
names(par2019)[names(par2019) == "N6.father"] <- "N7.father"
nei2019 <- merge(nei2019, par2019, by="N7", all.x=TRUE)
#N8
names(par2019)[names(par2019) == "N7"] <- "N8"
names(par2019)[names(par2019) == "N7.mother"] <- "N8.mother"
names(par2019)[names(par2019) == "N7.father"] <- "N8.father"
nei2019 <- merge(nei2019, par2019, by="N8", all.x=TRUE)
#N9
names(par2019)[names(par2019) == "N8"] <- "N9"
names(par2019)[names(par2019) == "N8.mother"] <- "N9.mother"
names(par2019)[names(par2019) == "N8.father"] <- "N9.father"
nei2019 <- merge(nei2019, par2019, by="N9", all.x=TRUE)
#N10
names(par2019)[names(par2019) == "N9"] <- "N10"
names(par2019)[names(par2019) == "N9.mother"] <- "N10.mother"
names(par2019)[names(par2019) == "N9.father"] <- "N10.father"
nei2019 <- merge(nei2019, par2019, by="N10", all.x=TRUE)

#and identifying column 
nei2019$year <- 2019
nei2019a  <-data.frame(nei2019,"box.year.parentid"=paste(nei2019$Focal.box, nei2019$year, "mother",sep="_")) 
nei2019b  <-data.frame(nei2019,"box.year.parentid"=paste(nei2019$Focal.box, nei2019$year, "father",sep="_")) 

nei2019 <- rbind(nei2019a, nei2019b)
rm(nei2019a, nei2019b)

nei2019$N9 <- NA 
nei2019$N10 <- NA

nei2019$N9.mother <- NA
nei2019$N9.father <- NA
nei2019$N10.mother <- NA
nei2019$N10.father <- NA

nei2019 <- nei2019[,order(colnames(nei2019))]

nei_output <- rbind(nei_output, nei2019)


#2020 ####
breeding.data.2020 <- xdata[which(xdata$year == 2020),] 
breeding.data.2020 <- breeding.data.2020[!is.na(breeding.data.2020$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
breeding.data.2020 <- sf::st_as_sf(breeding.data.2020, coords=c("x","y"), remove=F, crs=27700)

#calculating a bounding box for the function that calculates the territory polygons
box <- sf::st_sfc(bbox_polygon(breeding.data.2020))

territories <- sf::st_voronoi(sf::st_union(breeding.data.2020), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(breeding.data.2020))
breeding.ids.2020 <- breeding.data.2020[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, breeding.ids.2020)

#now we want to figure out who was in the neighboring territories for each box
territories.list <- st_intersection(territories, territories)

#this includes the box itself when making the comparisons so we'll remove those
territories.list <- subset(territories.list, Box.1 != Box)

#removing the geometry column as we don't need that anymore
st_geometry(territories.list) <- NULL

#we'll remove cases where the identities of neighbors were unknown 
#(presumably because they weren't caught or it failed before they were)


#fill in empty spaces with NA
territories.list$Mother <- as.character(territories.list$Mother)
territories.list$Mother <- with(territories.list, ifelse(Mother=="", NA, 
                                                         territories.list$Mother)) #label one ID

territories.list$Father <- as.character(territories.list$Father)
territories.list$Father <- with(territories.list, ifelse(Father=="", NA, 
                                                         territories.list$Father)) #label one ID

territories.list$Mother.1 <- as.character(territories.list$Mother.1)
territories.list$Mother.1 <- with(territories.list, ifelse(Mother.1=="", NA, 
                                                           territories.list$Mother.1)) #label one ID

territories.list$Father.1 <- as.character(territories.list$Father.1)
territories.list$Father.1 <- with(territories.list, ifelse(Father.1=="", NA, 
                                                           territories.list$Father.1)) #label one ID

territories.list <- subset(territories.list, !is.na(Father) | !is.na(Mother))

#we also want to do the same if the focal nest box had no ID information
territories.list <- subset(territories.list, !is.na(Father.1) | !is.na(Mother.1))

#now just getting the dataframe into a nice order with informative column names

territories.list <- territories.list[,c(4,5,6,1,2,3)]

colnames(territories.list) <- c("Focal.box", "Focal.male", "Focal.female", "Box.N", "Neighboring.male", "Neighboring.female")

neighbors.2020 <- territories.list

#change NA ids to UNKNOWN 
neighbors.2020$Focal.male <- with(neighbors.2020, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.2020$Focal.female <- with(neighbors.2020, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.2020$Neighboring.male <- with(neighbors.2020, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.2020$Neighboring.female <- with(neighbors.2020, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#remove unknowns so we just have a full list 

N2020.b <- as.data.frame(with(neighbors.2020, paste(Focal.male, Neighboring.male, sep="_")))
names(N2020.b)[1] <- "ring_ring"
N2020.c <- as.data.frame(with(neighbors.2020, paste(Focal.male, Neighboring.female, sep="_")))
names(N2020.c)[1] <- "ring_ring"
N2020.d <- as.data.frame(with(neighbors.2020, paste(Focal.female, Neighboring.female, sep="_")))
names(N2020.d)[1] <- "ring_ring"
N2020.e <- as.data.frame(with(neighbors.2020, paste(Focal.female, Neighboring.male, sep="_")))
names(N2020.e)[1] <- "ring_ring"

N2020 <- rbind(N2020.b, N2020.c, N2020.d, N2020.e)
rm(N2020.b, N2020.c, N2020.d, N2020.e)
N2020$neighbors <- TRUE
N2020$Year.s <- 2021

N2020 <- N2020[!grepl("UNKNOWN", N2020$ring_ring),]

N_reference <- rbind(N_reference, N2020)


ydata <- base.fn.data[which(base.fn.data$year==2020),]


library(dplyr)

nei2020 <- neighbors.2020[,c(1,4)]

nei2020 %>% group_by(Focal.box) %>% mutate(id = seq_along(Focal.box)) -> nei2020


nei2020 <- tidyr::pivot_wider(nei2020, names_from= "id", values_from="Box.N")

#add "N" to column names 
colnames(nei2020) <- paste0('N', colnames(nei2020))
names(nei2020)[1] <- "Focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 2020
as.data.frame(colnames(ydata))
par2020 <- ydata[,c(2,19,20)]
par2020 <- distinct(par2020, Box, .keep_all = TRUE)


####add neighbor ids
####2020
#N1
names(par2020)[names(par2020) == "Box"] <- "N1"
names(par2020)[names(par2020) == "Mother"] <- "N1.mother"
names(par2020)[names(par2020) == "Father"] <- "N1.father"
nei2020 <- merge(nei2020, par2020, by="N1", all.x=TRUE)
#N2
names(par2020)[names(par2020) == "N1"] <- "N2"
names(par2020)[names(par2020) == "N1.mother"] <- "N2.mother"
names(par2020)[names(par2020) == "N1.father"] <- "N2.father"
nei2020 <- merge(nei2020, par2020, by="N2", all.x=TRUE)
#N3
names(par2020)[names(par2020) == "N2"] <- "N3"
names(par2020)[names(par2020) == "N2.mother"] <- "N3.mother"
names(par2020)[names(par2020) == "N2.father"] <- "N3.father"
nei2020 <- merge(nei2020, par2020, by="N3", all.x=TRUE)
#N4
names(par2020)[names(par2020) == "N3"] <- "N4"
names(par2020)[names(par2020) == "N3.mother"] <- "N4.mother"
names(par2020)[names(par2020) == "N3.father"] <- "N4.father"
nei2020 <- merge(nei2020, par2020, by="N4", all.x=TRUE)
#N5
names(par2020)[names(par2020) == "N4"] <- "N5"
names(par2020)[names(par2020) == "N4.mother"] <- "N5.mother"
names(par2020)[names(par2020) == "N4.father"] <- "N5.father"
nei2020 <- merge(nei2020, par2020, by="N5", all.x=TRUE)
#N6
names(par2020)[names(par2020) == "N5"] <- "N6"
names(par2020)[names(par2020) == "N5.mother"] <- "N6.mother"
names(par2020)[names(par2020) == "N5.father"] <- "N6.father"
nei2020 <- merge(nei2020, par2020, by="N6", all.x=TRUE)
#N7
names(par2020)[names(par2020) == "N6"] <- "N7"
names(par2020)[names(par2020) == "N6.mother"] <- "N7.mother"
names(par2020)[names(par2020) == "N6.father"] <- "N7.father"
nei2020 <- merge(nei2020, par2020, by="N7", all.x=TRUE)
#N8
names(par2020)[names(par2020) == "N7"] <- "N8"
names(par2020)[names(par2020) == "N7.mother"] <- "N8.mother"
names(par2020)[names(par2020) == "N7.father"] <- "N8.father"
nei2020 <- merge(nei2020, par2020, by="N8", all.x=TRUE)
#N9
names(par2020)[names(par2020) == "N8"] <- "N9"
names(par2020)[names(par2020) == "N8.mother"] <- "N9.mother"
names(par2020)[names(par2020) == "N8.father"] <- "N9.father"
nei2020 <- merge(nei2020, par2020, by="N9", all.x=TRUE)
#N10
names(par2020)[names(par2020) == "N9"] <- "N10"
names(par2020)[names(par2020) == "N9.mother"] <- "N10.mother"
names(par2020)[names(par2020) == "N9.father"] <- "N10.father"
nei2020 <- merge(nei2020, par2020, by="N10", all.x=TRUE)

#and identifying column 
nei2020$year <- 2020
nei2020a  <-data.frame(nei2020,"box.year.parentid"=paste(nei2020$Focal.box, nei2020$year, "mother",sep="_")) 
nei2020b  <-data.frame(nei2020,"box.year.parentid"=paste(nei2020$Focal.box, nei2020$year, "father",sep="_")) 

nei2020 <- rbind(nei2020a, nei2020b)
rm(nei2020a, nei2020b)

nei2020$N10 <- NA

nei2020$N10.mother <- NA
nei2020$N10.father <- NA

nei2020 <- nei2020[,order(colnames(nei2020))]

nei_output <- rbind(nei_output, nei2020)

## add number of neighbors 

temp <- as.data.frame((is.na(nei_output[,c("N1","N2","N3","N4","N5","N6","N7","N8","N9","N10")])))
library(dplyr)
temp %>% mutate_if(is.logical,as.numeric) -> temp
temp$sumna <- rowSums(temp)
temp$N.num <- 10 - (temp$sumna)
nei_output$N.num <- temp$N.num

nei_output$year <- NULL
nei_output$Focal.box <- NULL

#add it to the base

base.fn.data.temp <- merge(base.fn.data, nei_output, by="box.year.parentid", all.x=TRUE)

#remove boxes without locations/that don't exist anymore
base.fn.data.temp <- base.fn.data.temp[which(!is.na(base.fn.data.temp$x)),]


#add familiarity based on previous years  ####

DF <- base.fn.data.temp
names(N_reference)[3] <- "year"

#each neighbor at a time

#N1 
DF$ring_ring <-(with(DF, paste(focal.ring, N1.mother, sep="_")))
names(N_reference)[2] <- "N1.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN1.mother <- DF.temp[,c(3,58)]
DF$ring_ring <-(with(DF, paste(focal.ring, N1.father, sep="_")))
names(N_reference)[2] <- "N1.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN1.father<- DF.temp[,c(3,58)]


#N2 
DF$ring_ring <-(with(DF, paste(focal.ring, N2.mother, sep="_")))
names(N_reference)[2] <- "N2.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN2.mother <- DF.temp[,c(3,58)]
DF$ring_ring <-(with(DF, paste(focal.ring, N2.father, sep="_")))
names(N_reference)[2] <- "N2.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN2.father<- DF.temp[,c(3,58)]

#N3
DF$ring_ring <-(with(DF, paste(focal.ring, N3.mother, sep="_")))
names(N_reference)[2] <- "N3.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN3.mother <- DF.temp[,c(3,58)]
DF$ring_ring <-(with(DF, paste(focal.ring, N3.father, sep="_")))
names(N_reference)[2] <- "N3.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN3.father<- DF.temp[,c(3,58)]

#N4 
DF$ring_ring <-(with(DF, paste(focal.ring, N4.mother, sep="_")))
names(N_reference)[2] <- "N4.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN4.mother <- DF.temp[,c(3,58)]
DF$ring_ring <-(with(DF, paste(focal.ring, N4.father, sep="_")))
names(N_reference)[2] <- "N4.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN4.father<- DF.temp[,c(3,58)]

#N5 
DF$ring_ring <-(with(DF, paste(focal.ring, N5.mother, sep="_")))
names(N_reference)[2] <- "N5.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN5.mother <- DF.temp[,c(3,58)]
DF$ring_ring <-(with(DF, paste(focal.ring, N5.father, sep="_")))
names(N_reference)[2] <- "N5.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN5.father<- DF.temp[,c(3,58)]

#N6 
DF$ring_ring <-(with(DF, paste(focal.ring, N6.mother, sep="_")))
names(N_reference)[2] <- "N6.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN6.mother <- DF.temp[,c(3,58)]
DF$ring_ring <-(with(DF, paste(focal.ring, N6.father, sep="_")))
names(N_reference)[2] <- "N6.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN6.father<- DF.temp[,c(3,58)]

#N7 
DF$ring_ring <-(with(DF, paste(focal.ring, N7.mother, sep="_")))
names(N_reference)[2] <- "N7.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN7.mother <- DF.temp[,c(3,58)]
DF$ring_ring <-(with(DF, paste(focal.ring, N7.father, sep="_")))
names(N_reference)[2] <- "N7.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN7.father<- DF.temp[,c(3,58)]

#N8 
DF$ring_ring <-(with(DF, paste(focal.ring, N8.mother, sep="_")))
names(N_reference)[2] <- "N8.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN8.mother <- DF.temp[,c(3,58)]
DF$ring_ring <-(with(DF, paste(focal.ring, N8.father, sep="_")))
names(N_reference)[2] <- "N8.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN8.father<- DF.temp[,c(3,58)]

#N9 
DF$ring_ring <-(with(DF, paste(focal.ring, N9.mother, sep="_")))
names(N_reference)[2] <- "N9.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN9.mother <- DF.temp[,c(3,58)]
DF$ring_ring <-(with(DF, paste(focal.ring, N9.father, sep="_")))
names(N_reference)[2] <- "N9.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN9.father<- DF.temp[,c(3,58)]

#N10 
DF$ring_ring <-(with(DF, paste(focal.ring, N10.mother, sep="_")))
names(N_reference)[2] <- "N10.MOTHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN10.mother <- DF.temp[,c(3,58)]
DF$ring_ring <-(with(DF, paste(focal.ring, N10.father, sep="_")))
names(N_reference)[2] <- "N10.FATHERfp"
DF.temp <- merge(DF, N_reference, by=c("ring_ring", "year"), all.x=TRUE)
CN10.father<- DF.temp[,c(3,58)]


#ok let's try to put them together?

CN1 <- merge(CN1.mother, CN1.father, by="box.year.parentid")
CN2 <- merge(CN2.mother, CN2.father, by="box.year.parentid")
CN3 <- merge(CN3.mother, CN3.father, by="box.year.parentid")
CN4 <- merge(CN4.mother, CN4.father, by="box.year.parentid")
CN5 <- merge(CN5.mother, CN5.father, by="box.year.parentid")
CN6 <- merge(CN6.mother, CN6.father, by="box.year.parentid")
CN7 <- merge(CN7.mother, CN7.father, by="box.year.parentid")
CN8 <- merge(CN8.mother, CN8.father, by="box.year.parentid")
CN9 <- merge(CN9.mother, CN9.father, by="box.year.parentid")
CN10 <- merge(CN10.mother, CN10.father, by="box.year.parentid")

x <- merge(CN1, CN2, by="box.year.parentid")
x <- merge(x, CN3, by="box.year.parentid")
x <- merge(x, CN4, by="box.year.parentid")
x <- merge(x, CN5, by="box.year.parentid")
x <- merge(x, CN6, by="box.year.parentid")
x <- merge(x, CN7, by="box.year.parentid")
x <- merge(x, CN8, by="box.year.parentid")
x <- merge(x, CN9, by="box.year.parentid")
x <- merge(x, CN10, by="box.year.parentid")

DF$ring_ring <- NULL
DF.temp <- merge(DF, x, by="box.year.parentid", all.x=TRUE)


test1 <- as.data.frame(order(DF$box.year.parentid))
test2 <- as.data.frame(order(DF.temp$box.year.parentid))
summary(arsenal::comparedf(test1, test2))
DF.temp <- DF.temp[c(1:20586),]
summary(arsenal::comparedf(DF, DF.temp))

#label number of familiar neighbors (individuals )
temp <- as.data.frame((is.na(DF.temp[,c("N1.MOTHERfp","N1.FATHERfp","N2.MOTHERfp", "N2.FATHERfp", "N3.MOTHERfp","N3.FATHERfp","N4.MOTHERfp", "N4.FATHERfp", "N5.MOTHERfp", "N5.FATHERfp", "N6.MOTHERfp", "N6.FATHERfp", "N7.MOTHERfp","N7.FATHERfp","N8.MOTHERfp", "N8.FATHERfp", "N9.MOTHERfp" ,"N9.FATHERfp","N10.MOTHERfp", "N10.FATHERfp")])))
library(dplyr)
temp %>% mutate_if(is.logical,as.numeric) -> temp
temp$sumna <- rowSums(temp)
temp$N.num.familiar <- 20 - temp$sumna
hist(temp$N.num.familiar)
DF.temp$N.num.ind.familiar <- temp$N.num.familiar

#familiarity to mothers 
temp <- as.data.frame((is.na(DF.temp[,c("N1.MOTHERfp","N2.MOTHERfp", "N3.MOTHERfp","N4.MOTHERfp",  "N5.MOTHERfp",  "N6.MOTHERfp",  "N7.MOTHERfp","N8.MOTHERfp", "N9.MOTHERfp" ,"N10.MOTHERfp")])))
library(dplyr)
temp %>% mutate_if(is.logical,as.numeric) -> temp
temp$sumna <- rowSums(temp)
temp$N.num.familiar <- 10 - (temp$sumna)
hist(temp$N.num.familiar)
DF.temp$N.num.FEMALEind.familiar <- temp$N.num.familiar

#familiarity to father 
temp <- as.data.frame((is.na(DF.temp[,c("N1.FATHERfp","N2.FATHERfp", "N3.FATHERfp","N4.FATHERfp",  "N5.FATHERfp",  "N6.FATHERfp",  "N7.FATHERfp","N8.FATHERfp", "N9.FATHERfp" ,"N10.FATHERfp")])))
library(dplyr)
temp %>% mutate_if(is.logical,as.numeric) -> temp
temp$sumna <- rowSums(temp)
temp$N.num.familiar <- 10 - (temp$sumna)
hist(temp$N.num.familiar)
DF.temp$N.num.MALEind.familiar <- temp$N.num.familiar


#change NA to false
DF.temp$N1.MOTHERfp <- with(DF.temp, ifelse(is.na(N1.MOTHERfp), FALSE, N1.MOTHERfp))
DF.temp$N2.MOTHERfp <- with(DF.temp, ifelse(is.na(N2.MOTHERfp), FALSE, N2.MOTHERfp)) 
DF.temp$N3.MOTHERfp <- with(DF.temp, ifelse(is.na(N3.MOTHERfp), FALSE, N3.MOTHERfp)) 
DF.temp$N4.MOTHERfp <- with(DF.temp, ifelse(is.na(N4.MOTHERfp), FALSE, N4.MOTHERfp)) 
DF.temp$N5.MOTHERfp <- with(DF.temp, ifelse(is.na(N5.MOTHERfp), FALSE, N5.MOTHERfp)) 
DF.temp$N6.MOTHERfp <- with(DF.temp, ifelse(is.na(N6.MOTHERfp), FALSE, N6.MOTHERfp)) 
DF.temp$N7.MOTHERfp <- with(DF.temp, ifelse(is.na(N7.MOTHERfp), FALSE, N7.MOTHERfp)) 
DF.temp$N8.MOTHERfp <- with(DF.temp, ifelse(is.na(N8.MOTHERfp), FALSE, N8.MOTHERfp)) 
DF.temp$N9.MOTHERfp <- with(DF.temp, ifelse(is.na(N9.MOTHERfp), FALSE, N9.MOTHERfp)) 
DF.temp$N10.MOTHERfp <- with(DF.temp, ifelse(is.na(N10.MOTHERfp), FALSE, N10.MOTHERfp)) 

DF.temp$N1.FATHERfp <- with(DF.temp, ifelse(is.na(N1.FATHERfp), FALSE, N1.FATHERfp))
DF.temp$N2.FATHERfp <- with(DF.temp, ifelse(is.na(N2.FATHERfp), FALSE, N2.FATHERfp)) 
DF.temp$N3.FATHERfp <- with(DF.temp, ifelse(is.na(N3.FATHERfp), FALSE, N3.FATHERfp)) 
DF.temp$N4.FATHERfp <- with(DF.temp, ifelse(is.na(N4.FATHERfp), FALSE, N4.FATHERfp)) 
DF.temp$N5.FATHERfp <- with(DF.temp, ifelse(is.na(N5.FATHERfp), FALSE, N5.FATHERfp)) 
DF.temp$N6.FATHERfp <- with(DF.temp, ifelse(is.na(N6.FATHERfp), FALSE, N6.FATHERfp)) 
DF.temp$N7.FATHERfp <- with(DF.temp, ifelse(is.na(N7.FATHERfp), FALSE, N7.FATHERfp)) 
DF.temp$N8.FATHERfp <- with(DF.temp, ifelse(is.na(N8.FATHERfp), FALSE, N8.FATHERfp)) 
DF.temp$N9.FATHERfp <- with(DF.temp, ifelse(is.na(N9.FATHERfp), FALSE, N9.FATHERfp)) 
DF.temp$N10.FATHERfp <- with(DF.temp, ifelse(is.na(N10.FATHERfp), FALSE, N10.FATHERfp)) 


fn.data <- DF.temp

setwd("~/Documents/2/Familiar_neighbors/DATA")
saveRDS(fn.data, "fn.data.Rds")



