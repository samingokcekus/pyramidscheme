setwd("~/Documents/2/Familiar_neighbors/DATA")

#load data 
load("Data Package A - Social Network Data For Samin(1).RData")
load("Data Package B - Spatial Data For Samin(corrected).RData")


####make the base####
#a.nests as base...
xdata <- a.nests
xdata[xdata==""]<-NA #make empty cells NA

#remove nests where a parent is not identified 
xdata$nest.id.type <- "NA" #make new column 

xdata$mID <- !is.na(xdata$mother)
xdata$fID <- !is.na(xdata$father)

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
                              .(year, nestbox, x, y, breeding.attempt, section, 
                                species, april.lay.date, laying.rate, incubation.started, 
                                april.hatch.date, incubation.duration, total.egg.weight, num.eggs.weighed,
                                clutch.size, num.chicks, num.dead.chicks, num.fledglings, mean.chick.weight,
                                father, mother, area, box, box.yr)]

x<-data.frame(x,"box.year.parentid"=paste(x$nestbox, x$year, x$parent,sep="_")) 

xdata <- x 

#get the focal ring 
xdata$focal.ring <- NA

xdata$focal.ring <- with(xdata, ifelse(parent == "mother", xdata$mother, 
                                         xdata$focal.ring)) #label one ID

xdata$focal.ring <- with(xdata, ifelse(parent == "father", xdata$father, 
                                       xdata$focal.ring)) #label one ID


#####add individual information####

x <- id.info

#set years to reflect breeding season year 
x$year.s <- NA

x$year.s <- with(x, ifelse(year == "2013","2014", 
                         x$year.s)) 

x$year.s <- with(x, ifelse(year == "2012", "2013", 
                         x$year.s)) 

x$year.s <- with(x, ifelse(year == "2011", "2012", 
                                       x$year.s)) 


#add individal info to xdata 
names(x)[names(x) == "ring"] <- "focal.ring"

names(xdata)[names(xdata) == "year"] <- "year.s"
y <- merge(xdata, x, by=c("year.s","focal.ring"), all.x=TRUE)

xdata <- y

#rename 
names(xdata)[names(xdata) == "year"] <- "year.w"
names(xdata)[names(xdata) == "sex"] <- "focal.sex"
names(xdata)[names(xdata) == "adjuv"] <- "focal.age"
names(xdata)[names(xdata) == "born"] <- "focal.wborn"

#remove data from 2015 and 2016
xdata <- xdata[which(xdata$year.s != 2015),] 
xdata <- xdata[which(xdata$year.s != 2016),] 

#fill in NAs

#years
xdata$year.w <- with(xdata, ifelse(year.s == "2012", "2011", 
                           xdata$year.w)) 
xdata$year.w <- with(xdata, ifelse(year.s == "2013", "2012", 
                                   xdata$year.w)) 
xdata$year.w <- with(xdata, ifelse(year.s == "2014", "2013", 
                                   xdata$year.w)) 

#sex
xdata$focal.sex <- with(xdata, ifelse(parent == "mother", "F", 
                                   xdata$focal.sex)) 
xdata$focal.sex <- with(xdata, ifelse(parent == "father", "M", 
                                      xdata$focal.sex)) 

####extract network metrics for each year for each individual####

#
####get the yearly network metrics####
#2011 
net2011 <- l.am.t[[1]]
Clustering <- tnet::clustering_local_w(net2011, measure = "am")
Strength <- colSums(net2011)
Strength_Mean <- colMeans(net2011) 
Betweenness <- sna::betweenness(net2011)
Eigenvector <- sna::evcent(net2011)
Degree <- sna::degree(net2011)
ring <- row.names(net2011)


x <- as.data.frame(cbind(ring, Clustering, Strength, Strength_Mean, Betweenness, Eigenvector, Degree))
names(x)[names(x) == "am"] <- "Clustering"
x$node <- NULL
x$year.w <- 2011

met2011.w <- x 

#2012 
net2012 <- l.am.t[[2]]
Clustering <- tnet::clustering_local_w(net2012, measure = "am")
Strength <- colSums(net2012)
Strength_Mean <- colMeans(net2012) 
Betweenness <- sna::betweenness(net2012)
Eigenvector <- sna::evcent(net2012)
Degree <- sna::degree(net2012)
ring <- row.names(net2012)

x <- as.data.frame(cbind(ring, Clustering, Strength, Strength_Mean, Betweenness, Eigenvector, Degree))
names(x)[names(x) == "am"] <- "Clustering"
x$node <- NULL
x$year.w <- 2012

met2012.w <- x

#2013 
net2013 <- l.am.t[[3]]
Clustering <- tnet::clustering_local_w(net2013, measure = "am")
Strength <- colSums(net2013)
Strength_Mean <- colMeans(net2013) 
Betweenness <- sna::betweenness(net2013)
Eigenvector <- sna::evcent(net2013)
Degree <- sna::degree(net2013)
ring <- row.names(net2013)

x <- as.data.frame(cbind(ring, Clustering, Strength, Strength_Mean, Betweenness, Eigenvector, Degree))
names(x)[names(x) == "am"] <- "Clustering"
x$node <- NULL
x$year.w <- 2013

met2013.w <- x

#put all three years together
x <- rbind(met2011.w, met2012.w, met2013.w)
names(x)[names(x) == "ring"] <- "focal.ring"

#add it to xdata
y <- merge(xdata, x, by=c("year.w","focal.ring"), all.x=TRUE)

xdata <- y

#who has social data 
temp <- id.info[,1:2]
names(temp)[names(temp) == "year"] <- "year.w"
names(temp)[names(temp) == "ring"] <- "mother"
temp$msdata <- "yes"

z <- merge(xdata, temp, by=c("mother", "year.w"), all.x=TRUE) #label mothers that have social data 


names(temp)[names(temp) == "mother"] <- "father"
temp$msdata <- NULL
temp$fsdata <- "yes"

x <- merge(z, temp, by=c("father", "year.w"), all.x=TRUE) #label fathers that have social data 

#retain the ones with both 
x$sdata <- NA
x$sdata <- with(x, ifelse(msdata == "yes" & fsdata == "yes", "retain", x$sdata)) #label the ones to keep

x <- x[which(x$sdata == "retain"),] 

rm(temp)
x$sdata <- NULL
x$fsdata <- NULL
x$msdata <- NULL

xdata <- x 

####pair bond strength####
#need to pull cell from association matrix 
library(tidyverse)
library(reshape2)

xdata2011 <- xdata[which(xdata$year.w == 2011),] 
xdata2012 <- xdata[which(xdata$year.w == 2012),] 
xdata2013 <- xdata[which(xdata$year.w == 2013),] 

bond2011 <- net2011 %>% reshape2::melt() %>% 
  rename(mother = Var1, father = Var2, BondStrength = value)%>% 
  left_join(xdata2011, ., by = c("mother", "father")) 

bond2012 <- net2012 %>% reshape2::melt() %>% 
  rename(mother = Var1, father = Var2, BondStrength = value)%>% 
  left_join(xdata2012, ., by = c("mother", "father")) 

bond2013 <- net2013 %>% reshape2::melt() %>% 
  rename(mother = Var1, father = Var2, BondStrength = value)%>% 
  left_join(xdata2013, ., by = c("mother", "father")) 

xdata <- rbind(bond2011, bond2012, bond2013)

xdata2011 <- xdata[which(xdata$year.w == 2011),] 
xdata2012 <- xdata[which(xdata$year.w == 2012),] 
xdata2013 <- xdata[which(xdata$year.w == 2013),] 

####familiarity to neighbors#### 

##first get list of neighbors ####
#remove individuals that are not first-order neighbors 
dat.info <- dat.info[which(dat.info$vor == 1),] 

#get just box combinations and year 
xx <- dat.info[,c(1,5,6,7)]

#rlooking one year at a time 
xx2012 <- xx[which(xx$year == 2012),]
xx2012$year <- NULL

#order by box then distance 
xx2012 <- xx2012[order(xx2012$box2, xx2012$dis, sample(1:nrow(xx2012))),]
xx2012$dis <- NULL

#library(dplyr) #to remove the "duplicates" so only one set of each pair of boxes, don't need to do this 
#xx2012 %>% 
#  mutate_all(as.character) %>%
#  transmute(x1 = pmin(box1, box2), y1 = pmax(box1, box2)) %>%
#  rename_all(~ names(xx2012)) %>%
#  distinct -> xxTEMP

library(dplyr)
#xx2012 %>% group_by(box2) %>% mutate(id = row_number()) -> xx2012
#xx2012 %>% group_by(box2) %>% mutate(id = 1:n()) -> xx2012
#xx2012 %>% group_by(box2) %>% mutate(id = seq_len(n())) -> xx2012
xx2012 %>% group_by(box2) %>% mutate(id = seq_along(box2)) -> xx2012

nei2012 <- pivot_wider(xx2012, names_from= "id", values_from="box1")

#### 2013
xx2013 <- xx[which(xx$year == 2013),]
xx2013$year <- NULL

#order by box then distance 
xx2013 <- xx2013[order(xx2013$box2, xx2013$dis, sample(1:nrow(xx2013))),]
xx2013$dis <- NULL

library(dplyr)
xx2013 %>% group_by(box2) %>% mutate(id = seq_along(box2)) -> xx2013

nei2013 <- pivot_wider(xx2013, names_from= "id", values_from="box1")

#### 2014
xx2014 <- xx[which(xx$year == 2014),]
xx2014$year <- NULL

#order by box then distance 
xx2014 <- xx2014[order(xx2014$box2, xx2014$dis, sample(1:nrow(xx2014))),]
xx2014$dis <- NULL

library(dplyr)
xx2014 %>% group_by(box2) %>% mutate(id = seq_along(box2)) -> xx2014

nei2014 <- pivot_wider(xx2014, names_from= "id", values_from="box1")

#add "N" to column names 
colnames(nei2012) <- paste0('N', colnames(nei2012))
names(nei2012)[1] <- "focal.box"
colnames(nei2013) <- paste0('N', colnames(nei2013))
names(nei2013)[1] <- "focal.box"
colnames(nei2014) <- paste0('N', colnames(nei2014))
names(nei2014)[1] <- "focal.box"

##duplicate the columns so there is a spot for the mother and father at each box 
#2012
#x1 <- setNames(nei2012, paste0(names(nei2012), '.', 'male'))
#x2 <- setNames(nei2012, paste0(names(nei2012), '.', 'female'))
#nei2012 <- cbind(x1, x2)
#nei2012$focal.box.female <- NULL
#names(nei2012)[names(nei2012) == "focal.box.male"] <- "focal.box"
##2013
#x1 <- setNames(nei2013, paste0(names(nei2013), '.', 'male'))
#x2 <- setNames(nei2013, paste0(names(nei2013), '.', 'female'))
#nei2013 <- cbind(x1, x2)
#nei2013$focal.box.female <- NULL
#names(nei2013)[names(nei2013) == "focal.box.male"] <- "focal.box"
##2014
#x1 <- setNames(nei2014, paste0(names(nei2014), '.', 'male'))
#x2 <- setNames(nei2014, paste0(names(nei2014), '.', 'female'))
#nei2014 <- cbind(x1, x2)
#nei2014$focal.box.female <- NULL
#names(nei2014)[names(nei2014) == "focal.box.male"] <- "focal.box"

##so now we need to add the parent information for each neighbor
#get a list of all parents in 2012
a.nests$nestbox <- toupper(a.nests$nestbox)
xx <- a.nests[,c(2,3,21,22)]


par2012 <- xx[which(xx$year == 2012),]
par2012$year <- NULL
par2013 <- xx[which(xx$year == 2013),]
par2013$year <- NULL
par2014 <- xx[which(xx$year == 2014),]
par2014$year <- NULL

####adding neighbor IDs ####
####for 2012 
#N1
names(par2012)[names(par2012) == "nestbox"] <- "N1"
names(par2012)[names(par2012) == "mother"] <- "N1.mother"
names(par2012)[names(par2012) == "father"] <- "N1.father"
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

####for 2013 
#N1
names(par2013)[names(par2013) == "nestbox"] <- "N1"
names(par2013)[names(par2013) == "mother"] <- "N1.mother"
names(par2013)[names(par2013) == "father"] <- "N1.father"
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

nei2013$N10 <- NA
nei2013$N10.mother <- NA 
nei2013$N10.father <- NA


####for 2014 
#N1
names(par2014)[names(par2014) == "nestbox"] <- "N1"
names(par2014)[names(par2014) == "mother"] <- "N1.mother"
names(par2014)[names(par2014) == "father"] <- "N1.father"
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

#add the year column for merging 

nei2012$year <- "2012"
nei2013$year <- "2013"
nei2014$year <- "2014"

nei2012 <- unite(nei2012, "box.yr", focal.box, year, sep = " ", remove = TRUE, na.rm = FALSE)
nei2013 <- unite(nei2013, "box.yr", focal.box, year, sep = " ", remove = TRUE, na.rm = FALSE)
nei2014 <- unite(nei2014, "box.yr", focal.box, year, sep = " ", remove = TRUE, na.rm = FALSE)

#merge with xdata 
yy2012 <- merge(xdata2011, nei2012, by="box.yr", all.x=TRUE)
yy2013 <- merge(xdata2012, nei2013, by="box.yr", all.x=TRUE)
yy2014 <- merge(xdata2013, nei2014, by="box.yr", all.x=TRUE)

#adding bond strength for each focal and neighbor ####

##2012####
#N1
zz2012 <- net2011 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N1.father = Var2, N1.fbs = value)%>% 
  left_join(yy2012, ., by = c("focal.ring", "N1.father")) 

zz2012 <- net2011 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N1.mother = Var2, N1.mbs = value)%>% 
  left_join(zz2012, ., by = c("focal.ring", "N1.mother")) 

#N2
zz2012 <- net2011 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N2.father = Var2, N2.fbs = value)%>% 
  left_join(zz2012, ., by = c("focal.ring", "N2.father")) 

zz2012 <- net2011 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N2.mother = Var2, N2.mbs = value)%>% 
  left_join(zz2012, ., by = c("focal.ring", "N2.mother")) 

#N3
zz2012 <- net2011 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N3.father = Var2, N3.fbs = value)%>% 
  left_join(zz2012, ., by = c("focal.ring", "N3.father")) 

zz2012 <- net2011 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N3.mother = Var2, N3.mbs = value)%>% 
  left_join(zz2012, ., by = c("focal.ring", "N3.mother")) 


#N4
zz2012 <- net2011 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N4.father = Var2, N4.fbs = value)%>% 
  left_join(zz2012, ., by = c("focal.ring", "N4.father")) 

zz2012 <- net2011 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N4.mother = Var2, N4.mbs = value)%>% 
  left_join(zz2012, ., by = c("focal.ring", "N4.mother")) 

#N5
zz2012 <- net2011 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N5.father = Var2, N5.fbs = value)%>% 
  left_join(zz2012, ., by = c("focal.ring", "N5.father")) 

zz2012 <- net2011 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N5.mother = Var2, N5.mbs = value)%>% 
  left_join(zz2012, ., by = c("focal.ring", "N5.mother")) 


#N6
zz2012 <- net2011 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N6.father = Var2, N6.fbs = value)%>% 
  left_join(zz2012, ., by = c("focal.ring", "N6.father")) 

zz2012 <- net2011 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N6.mother = Var2, N6.mbs = value)%>% 
  left_join(zz2012, ., by = c("focal.ring", "N6.mother")) 

#N7
zz2012 <- net2011 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N7.father = Var2, N7.fbs = value)%>% 
  left_join(zz2012, ., by = c("focal.ring", "N7.father")) 

zz2012 <- net2011 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N7.mother = Var2, N7.mbs = value)%>% 
  left_join(zz2012, ., by = c("focal.ring", "N7.mother")) 


#N8
zz2012 <- net2011 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N8.father = Var2, N8.fbs = value)%>% 
  left_join(zz2012, ., by = c("focal.ring", "N8.father")) 

zz2012 <- net2011 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N8.mother = Var2, N8.mbs = value)%>% 
  left_join(zz2012, ., by = c("focal.ring", "N8.mother")) 


#N9
zz2012 <- net2011 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N9.father = Var2, N9.fbs = value)%>% 
  left_join(zz2012, ., by = c("focal.ring", "N9.father")) 

zz2012 <- net2011 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N9.mother = Var2, N9.mbs = value)%>% 
  left_join(zz2012, ., by = c("focal.ring", "N9.mother")) 

#N10
zz2012 <- net2011 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N10.father = Var2, N10.fbs = value)%>% 
  left_join(zz2012, ., by = c("focal.ring", "N10.father")) 

zz2012 <- net2011 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N10.mother = Var2, N10.mbs = value)%>% 
  left_join(zz2012, ., by = c("focal.ring", "N10.mother")) 

#2013####
#N1
zz2013 <- net2012 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N1.father = Var2, N1.fbs = value)%>% 
  left_join(yy2013, ., by = c("focal.ring", "N1.father")) 

zz2013 <- net2012 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N1.mother = Var2, N1.mbs = value)%>% 
  left_join(zz2013, ., by = c("focal.ring", "N1.mother")) 

#N2
zz2013 <- net2012 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N2.father = Var2, N2.fbs = value)%>% 
  left_join(zz2013, ., by = c("focal.ring", "N2.father")) 

zz2013 <- net2012 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N2.mother = Var2, N2.mbs = value)%>% 
  left_join(zz2013, ., by = c("focal.ring", "N2.mother")) 

#N3
zz2013 <- net2012 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N3.father = Var2, N3.fbs = value)%>% 
  left_join(zz2013, ., by = c("focal.ring", "N3.father")) 

zz2013 <- net2012 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N3.mother = Var2, N3.mbs = value)%>% 
  left_join(zz2013, ., by = c("focal.ring", "N3.mother")) 


#N4
zz2013 <- net2012 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N4.father = Var2, N4.fbs = value)%>% 
  left_join(zz2013, ., by = c("focal.ring", "N4.father")) 

zz2013 <- net2012 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N4.mother = Var2, N4.mbs = value)%>% 
  left_join(zz2013, ., by = c("focal.ring", "N4.mother")) 

#N5
zz2013 <- net2012 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N5.father = Var2, N5.fbs = value)%>% 
  left_join(zz2013, ., by = c("focal.ring", "N5.father")) 

zz2013 <- net2012 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N5.mother = Var2, N5.mbs = value)%>% 
  left_join(zz2013, ., by = c("focal.ring", "N5.mother")) 


#N6
zz2013 <- net2012 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N6.father = Var2, N6.fbs = value)%>% 
  left_join(zz2013, ., by = c("focal.ring", "N6.father")) 

zz2013 <- net2012 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N6.mother = Var2, N6.mbs = value)%>% 
  left_join(zz2013, ., by = c("focal.ring", "N6.mother")) 

#N7
zz2013 <- net2012 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N7.father = Var2, N7.fbs = value)%>% 
  left_join(zz2013, ., by = c("focal.ring", "N7.father")) 

zz2013 <- net2012 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N7.mother = Var2, N7.mbs = value)%>% 
  left_join(zz2013, ., by = c("focal.ring", "N7.mother")) 


#N8
zz2013 <- net2012 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N8.father = Var2, N8.fbs = value)%>% 
  left_join(zz2013, ., by = c("focal.ring", "N8.father")) 

zz2013 <- net2012 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N8.mother = Var2, N8.mbs = value)%>% 
  left_join(zz2013, ., by = c("focal.ring", "N8.mother")) 


#N9
zz2013 <- net2012 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N9.father = Var2, N9.fbs = value)%>% 
  left_join(zz2013, ., by = c("focal.ring", "N9.father")) 

zz2013 <- net2012 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N9.mother = Var2, N9.mbs = value)%>% 
  left_join(zz2013, ., by = c("focal.ring", "N9.mother")) 

#N10
zz2013 <- net2012 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N10.father = Var2, N10.fbs = value)%>% 
  left_join(zz2013, ., by = c("focal.ring", "N10.father")) 

zz2013 <- net2012 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N10.mother = Var2, N10.mbs = value)%>% 
  left_join(zz2013, ., by = c("focal.ring", "N10.mother")) 


#2014####
#N1
zz2014 <- net2013 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N1.father = Var2, N1.fbs = value)%>% 
  left_join(yy2014, ., by = c("focal.ring", "N1.father")) 

zz2014 <- net2013 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N1.mother = Var2, N1.mbs = value)%>% 
  left_join(zz2014, ., by = c("focal.ring", "N1.mother")) 

#N2
zz2014 <- net2013 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N2.father = Var2, N2.fbs = value)%>% 
  left_join(zz2014, ., by = c("focal.ring", "N2.father")) 

zz2014 <- net2013 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N2.mother = Var2, N2.mbs = value)%>% 
  left_join(zz2014, ., by = c("focal.ring", "N2.mother")) 

#N3
zz2014 <- net2013 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N3.father = Var2, N3.fbs = value)%>% 
  left_join(zz2014, ., by = c("focal.ring", "N3.father")) 

zz2014 <- net2013 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N3.mother = Var2, N3.mbs = value)%>% 
  left_join(zz2014, ., by = c("focal.ring", "N3.mother")) 


#N4
zz2014 <- net2013 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N4.father = Var2, N4.fbs = value)%>% 
  left_join(zz2014, ., by = c("focal.ring", "N4.father")) 

zz2014 <- net2013 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N4.mother = Var2, N4.mbs = value)%>% 
  left_join(zz2014, ., by = c("focal.ring", "N4.mother")) 

#N5
zz2014 <- net2013 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N5.father = Var2, N5.fbs = value)%>% 
  left_join(zz2014, ., by = c("focal.ring", "N5.father")) 

zz2014 <- net2013 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N5.mother = Var2, N5.mbs = value)%>% 
  left_join(zz2014, ., by = c("focal.ring", "N5.mother")) 


#N6
zz2014 <- net2013 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N6.father = Var2, N6.fbs = value)%>% 
  left_join(zz2014, ., by = c("focal.ring", "N6.father")) 

zz2014 <- net2013 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N6.mother = Var2, N6.mbs = value)%>% 
  left_join(zz2014, ., by = c("focal.ring", "N6.mother")) 

#N7
zz2014 <- net2013 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N7.father = Var2, N7.fbs = value)%>% 
  left_join(zz2014, ., by = c("focal.ring", "N7.father")) 

zz2014 <- net2013 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N7.mother = Var2, N7.mbs = value)%>% 
  left_join(zz2014, ., by = c("focal.ring", "N7.mother")) 


#N8
zz2014 <- net2013 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N8.father = Var2, N8.fbs = value)%>% 
  left_join(zz2014, ., by = c("focal.ring", "N8.father")) 

zz2014 <- net2013 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N8.mother = Var2, N8.mbs = value)%>% 
  left_join(zz2014, ., by = c("focal.ring", "N8.mother")) 


#N9
zz2014 <- net2013 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N9.father = Var2, N9.fbs = value)%>% 
  left_join(zz2014, ., by = c("focal.ring", "N9.father")) 

zz2014 <- net2013 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N9.mother = Var2, N9.mbs = value)%>% 
  left_join(zz2014, ., by = c("focal.ring", "N9.mother")) 

#N10
zz2014 <- net2013 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N10.father = Var2, N10.fbs = value)%>% 
  left_join(zz2014, ., by = c("focal.ring", "N10.father")) 

zz2014 <- net2013 %>% reshape2::melt() %>% 
  rename(focal.ring = Var1, N10.mother = Var2, N10.mbs = value)%>% 
  left_join(zz2014, ., by = c("focal.ring", "N10.mother")) 

###okay now putting them back together ?
zz2012 <- zz2012[,order(colnames(zz2012))]
zz2013 <- zz2013[,order(colnames(zz2013))]
zz2014 <- zz2014[,order(colnames(zz2014))]

xdata <- rbind(zz2012, zz2013, zz2014)


#save these 

saveRDS(xdata, "fnbasedata_full.Rda")
saveRDS(zz2012, "fnbasedata_2012.Rda")
saveRDS(zz2013, "fnbasedata_2013.Rda")
saveRDS(zz2014, "fnbasedata_2014.Rda")

