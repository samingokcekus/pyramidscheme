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


#####using clean data from greg as base
####adding familiarity based on previous years #### 
rm(list=ls())

setwd("~/Documents/2/Familiar_neighbors/DATA")
library(sf)

#load data 
load("Data Package A - Social Network Data For Samin(1).RData")
load("Data Package B - Spatial Data For Samin(corrected).RData")

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

#get the neighbors from 2011 ####
xdata2011 <- xdata[which(xdata$year == 2011),]



xdata2011 <- xdata2011[!is.na(xdata2011$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
xdata2011 <- sf::st_as_sf(xdata2011, coords=c("x","y"), remove=F, crs=27700)

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

box <- sf::st_sfc(bbox_polygon(xdata2011))

territories <- sf::st_voronoi(sf::st_union(xdata2011), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(xdata2011))
xdata2011 <- xdata2011[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, xdata2011)

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

#get the neighbors from 2012 ####
xdata2012 <- xdata[which(xdata$year == 2012),]



xdata2012 <- xdata2012[!is.na(xdata2012$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
xdata2012 <- sf::st_as_sf(xdata2012, coords=c("x","y"), remove=F, crs=27700)

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

box <- sf::st_sfc(bbox_polygon(xdata2012))

territories <- sf::st_voronoi(sf::st_union(xdata2012), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(xdata2012))
xdata2012 <- xdata2012[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, xdata2012)

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

#get the neighbors from 2013 ####
xdata2013 <- xdata[which(xdata$year == 2013),]



xdata2013 <- xdata2013[!is.na(xdata2013$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
xdata2013 <- sf::st_as_sf(xdata2013, coords=c("x","y"), remove=F, crs=27700)

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

box <- sf::st_sfc(bbox_polygon(xdata2013))

territories <- sf::st_voronoi(sf::st_union(xdata2013), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(xdata2013))
xdata2013 <- xdata2013[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, xdata2013)

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

#get the neighbors from 2014 ####
xdata2014 <- xdata[which(xdata$year == 2014),]



xdata2014 <- xdata2014[!is.na(xdata2014$x), ] #can get this info somehow? for now removing the 10 without coords

#converting it into a spatial object
xdata2014 <- sf::st_as_sf(xdata2014, coords=c("x","y"), remove=F, crs=27700)

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

box <- sf::st_sfc(bbox_polygon(xdata2014))

territories <- sf::st_voronoi(sf::st_union(xdata2014), box)
territories <- sf::st_intersection(sf::st_cast(territories), sf::st_union(wood.outline))

plot(territories)

#joining the territory polygons back up with the individuals that bred in them
data.frame(colnames(xdata2014))
xdata2014 <- xdata2014[,c(57,37,38)]

territories <- sf::st_sf(geom = territories)
territories <- sf::st_join(territories, xdata2014)

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


#### get data to merge with  ####
setwd("~/Documents/2/Familiar_neighbors/familiarneighbor/Data")
DF <- readRDS("CleanData.rds")
DF$Binary.succ <- DF$Num.fledglings
DF$Binary.succ <- with(DF, ifelse(Binary.succ == 0, "0", 
                                  DF$Binary.succ)) 
DF$Binary.succ <- with(DF, ifelse(Binary.succ > 0, "1", 
                                  DF$Binary.succ)) 

DF$Binary.succ <- as.numeric(DF$Binary.succ)

#neighbor stuff 
DF$N.full.avgbs <- rowMeans(DF[,c("N1.fbs", "N1.mbs","N2.fbs", "N2.mbs","N3.fbs", "N3.mbs","N4.fbs", "N4.mbs", 
                                  "N5.fbs", "N5.mbs", "N6.fbs", "N6.mbs", "N7.fbs", "N7.mbs", "N8.fbs", "N8.mbs", 
                                  "N9.fbs", "N9.mbs", "N10.fbs", "N10.mbs")], na.rm=TRUE)

DF$N.female.avgbs <- rowMeans(DF[,c("N1.mbs","N2.mbs","N3.mbs", "N4.mbs", 
                                    "N5.mbs", "N6.mbs","N7.mbs", "N8.mbs", 
                                    "N9.mbs", "N10.mbs")], na.rm=TRUE)

DF$N.male.avgbs <- rowMeans(DF[,c("N1.fbs","N2.fbs","N3.fbs", "N4.fbs", 
                                  "N5.fbs", "N6.fbs","N7.fbs", "N8.fbs", 
                                  "N9.fbs", "N10.fbs")], na.rm=TRUE)

temp <- as.data.frame((is.na(DF[,c("N1","N2","N3","N4","N5","N6","N7","N8","N9","N10")])))
library(dplyr)
temp %>% mutate_if(is.logical,as.numeric) -> temp
temp$sumna <- rowSums(temp)
temp$N.num <- 10 - (temp$sumna)

DF$N.num <- temp$N.num

#### make dataframe for identifying neighbors from previous years ####

#change NA ids to UNKNOWN 
neighbors.2011$Focal.male <- with(neighbors.2011, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.2011$Focal.female <- with(neighbors.2011, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.2011$Neighboring.male <- with(neighbors.2011, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.2011$Neighboring.female <- with(neighbors.2011, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

neighbors.2012$Focal.male <- with(neighbors.2012, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.2012$Focal.female <- with(neighbors.2012, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.2012$Neighboring.male <- with(neighbors.2012, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.2012$Neighboring.female <- with(neighbors.2012, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

neighbors.2013$Focal.male <- with(neighbors.2013, ifelse(is.na(Focal.male), "UNKNOWN", Focal.male)) 
neighbors.2013$Focal.female <- with(neighbors.2013, ifelse(is.na(Focal.female), "UNKNOWN", Focal.female)) 
neighbors.2013$Neighboring.male <- with(neighbors.2013, ifelse(is.na(Neighboring.male), "UNKNOWN", Neighboring.male)) 
neighbors.2013$Neighboring.female <- with(neighbors.2013, ifelse(is.na(Neighboring.female), "UNKNOWN", Neighboring.female)) 

#make a column with neighbor pairs, year, and "true" 
#2011
N2011.a <- as.data.frame(with(neighbors.2011, paste(Focal.box, Box.N, sep="_")))
names(N2011.a)[1] <- "boxes"
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


#2012
N2012.a <- as.data.frame(with(neighbors.2012, paste(Focal.box, Box.N, sep="_")))
names(N2012.a)[1] <- "boxes"
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


#2013
N2013.a <- as.data.frame(with(neighbors.2013, paste(Focal.box, Box.N, sep="_")))
names(N2013.a)[1] <- "boxes"
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

N_123 <- rbind(N2011, N2012, N2013)

N_123_full <- N_123[!grepl("UNKNOWN", N_123$ring_ring),]


#each neighbor at a time

#N1 
#DF$ring_ring <-(with(DF, paste(Focal.ring, N1.mother, sep="_")))
#names(N_123_full)[2] <- "N1.MOTHERfp"
#DF.temp <- merge(DF, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
#DF.temp$ring_ring <-(with(DF.temp, paste(Focal.ring, N1.father, sep="_")))
#names(N_123_full)[2] <- "N1.FATHERfp"
#DF.temp <- merge(DF.temp, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)

DF$ring_ring <-(with(DF, paste(Focal.ring, N1.mother, sep="_")))
names(N_123_full)[2] <- "N1.MOTHERfp"
DF.temp <- merge(DF, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
CN1.mother <- DF.temp[,c(9,97)]
DF$ring_ring <-(with(DF, paste(Focal.ring, N1.father, sep="_")))
names(N_123_full)[2] <- "N1.FATHERfp"
DF.temp <- merge(DF, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
CN1.father<- DF.temp[,c(9,97)]


#N2 
#DF.temp$ring_ring <-(with(DF.temp, paste(Focal.ring, N2.mother, sep="_")))
#names(N_123_full)[2] <- "N2.MOTHERfp"
#DF.temp <- merge(DF.temp, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
#DF.temp$ring_ring <-(with(DF.temp, paste(Focal.ring, N2.father, sep="_")))
#names(N_123_full)[2] <- "N2.FATHERfp"
#DF.temp <- merge(DF.temp, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)

DF$ring_ring <-(with(DF, paste(Focal.ring, N2.mother, sep="_")))
names(N_123_full)[2] <- "N2.MOTHERfp"
DF.temp <- merge(DF, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
CN2.mother <- DF.temp[,c(9,97)]
DF$ring_ring <-(with(DF, paste(Focal.ring, N2.father, sep="_")))
names(N_123_full)[2] <- "N2.FATHERfp"
DF.temp <- merge(DF, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
CN2.father<- DF.temp[,c(9,97)]

#N3
#DF.temp$ring_ring <-(with(DF.temp, paste(Focal.ring, N3.mother, sep="_")))
#names(N_123_full)[2] <- "N3.MOTHERfp"
#DF.temp <- merge(DF.temp, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
#DF.temp$ring_ring <-(with(DF.temp, paste(Focal.ring, N3.father, sep="_")))
#names(N_123_full)[2] <- "N3.FATHERfp"
#DF.temp <- merge(DF.temp, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)

DF$ring_ring <-(with(DF, paste(Focal.ring, N3.mother, sep="_")))
names(N_123_full)[2] <- "N3.MOTHERfp"
DF.temp <- merge(DF, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
CN3.mother <- DF.temp[,c(9,97)]
DF$ring_ring <-(with(DF, paste(Focal.ring, N3.father, sep="_")))
names(N_123_full)[2] <- "N3.FATHERfp"
DF.temp <- merge(DF, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
CN3.father<- DF.temp[,c(9,97)]

#N4 
#DF.temp$ring_ring <-(with(DF.temp, paste(Focal.ring, N4.mother, sep="_")))
#names(N_123_full)[2] <- "N4.MOTHERfp"
#DF.temp <- merge(DF.temp, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
#DF.temp$ring_ring <-(with(DF.temp, paste(Focal.ring, N4.father, sep="_")))
#names(N_123_full)[2] <- "N4.FATHERfp"
#DF.temp <- merge(DF.temp, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)

DF$ring_ring <-(with(DF, paste(Focal.ring, N4.mother, sep="_")))
names(N_123_full)[2] <- "N4.MOTHERfp"
DF.temp <- merge(DF, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
CN4.mother <- DF.temp[,c(9,97)]
DF$ring_ring <-(with(DF, paste(Focal.ring, N4.father, sep="_")))
names(N_123_full)[2] <- "N4.FATHERfp"
DF.temp <- merge(DF, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
CN4.father<- DF.temp[,c(9,97)]

#N5 
#DF.temp$ring_ring <-(with(DF.temp, paste(Focal.ring, N5.mother, sep="_")))
#names(N_123_full)[2] <- "N5.MOTHERfp"
#DF.temp <- merge(DF.temp, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
#DF.temp$ring_ring <-(with(DF.temp, paste(Focal.ring, N5.father, sep="_")))
#names(N_123_full)[2] <- "N5.FATHERfp"
#DF.temp <- merge(DF.temp, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)

DF$ring_ring <-(with(DF, paste(Focal.ring, N5.mother, sep="_")))
names(N_123_full)[2] <- "N5.MOTHERfp"
DF.temp <- merge(DF, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
CN5.mother <- DF.temp[,c(9,97)]
DF$ring_ring <-(with(DF, paste(Focal.ring, N5.father, sep="_")))
names(N_123_full)[2] <- "N5.FATHERfp"
DF.temp <- merge(DF, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
CN5.father<- DF.temp[,c(9,97)]

#N6 
#DF.temp$ring_ring <-(with(DF.temp, paste(Focal.ring, N6.mother, sep="_")))
#names(N_123_full)[2] <- "N6.MOTHERfp"
#DF.temp <- merge(DF.temp, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
#DF.temp$ring_ring <-(with(DF.temp, paste(Focal.ring, N6.father, sep="_")))
#names(N_123_full)[2] <- "N6.FATHERfp"
#DF.temp <- merge(DF.temp, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)

DF$ring_ring <-(with(DF, paste(Focal.ring, N6.mother, sep="_")))
names(N_123_full)[2] <- "N6.MOTHERfp"
DF.temp <- merge(DF, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
CN6.mother <- DF.temp[,c(9,97)]
DF$ring_ring <-(with(DF, paste(Focal.ring, N6.father, sep="_")))
names(N_123_full)[2] <- "N6.FATHERfp"
DF.temp <- merge(DF, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
CN6.father<- DF.temp[,c(9,97)]

#N7 
#DF.temp$ring_ring <-(with(DF.temp, paste(Focal.ring, N7.mother, sep="_")))
#names(N_123_full)[2] <- "N7.MOTHERfp"
#DF.temp <- merge(DF.temp, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
#DF.temp$ring_ring <-(with(DF.temp, paste(Focal.ring, N7.father, sep="_")))
#names(N_123_full)[2] <- "N7.FATHERfp"
#DF.temp <- merge(DF.temp, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)

DF$ring_ring <-(with(DF, paste(Focal.ring, N7.mother, sep="_")))
names(N_123_full)[2] <- "N7.MOTHERfp"
DF.temp <- merge(DF, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
CN7.mother <- DF.temp[,c(9,97)]
DF$ring_ring <-(with(DF, paste(Focal.ring, N7.father, sep="_")))
names(N_123_full)[2] <- "N7.FATHERfp"
DF.temp <- merge(DF, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
CN7.father<- DF.temp[,c(9,97)]

#N8 
#DF.temp$ring_ring <-(with(DF.temp, paste(Focal.ring, N8.mother, sep="_")))
#names(N_123_full)[2] <- "N8.MOTHERfp"
#DF.temp <- merge(DF.temp, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
#DF.temp$ring_ring <-(with(DF.temp, paste(Focal.ring, N8.father, sep="_")))
#names(N_123_full)[2] <- "N8.FATHERfp"
#DF.temp <- merge(DF.temp, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)

DF$ring_ring <-(with(DF, paste(Focal.ring, N8.mother, sep="_")))
names(N_123_full)[2] <- "N8.MOTHERfp"
DF.temp <- merge(DF, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
CN8.mother <- DF.temp[,c(9,97)]
DF$ring_ring <-(with(DF, paste(Focal.ring, N8.father, sep="_")))
names(N_123_full)[2] <- "N8.FATHERfp"
DF.temp <- merge(DF, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
CN8.father<- DF.temp[,c(9,97)]

#N9 
#DF.temp$ring_ring <-(with(DF.temp, paste(Focal.ring, N9.mother, sep="_")))
#names(N_123_full)[2] <- "N9.MOTHERfp"
#DF.temp <- merge(DF.temp, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
#DF.temp$ring_ring <-(with(DF.temp, paste(Focal.ring, N9.father, sep="_")))
#names(N_123_full)[2] <- "N9.FATHERfp"
#DF.temp <- merge(DF.temp, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)

DF$ring_ring <-(with(DF, paste(Focal.ring, N9.mother, sep="_")))
names(N_123_full)[2] <- "N9.MOTHERfp"
DF.temp <- merge(DF, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
CN9.mother <- DF.temp[,c(9,97)]
DF$ring_ring <-(with(DF, paste(Focal.ring, N9.father, sep="_")))
names(N_123_full)[2] <- "N9.FATHERfp"
DF.temp <- merge(DF, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
CN9.father<- DF.temp[,c(9,97)]

#N10 
#DF.temp$ring_ring <-(with(DF.temp, paste(Focal.ring, N10.mother, sep="_")))
#names(N_123_full)[2] <- "N10.MOTHERfp"
#DF.temp <- merge(DF.temp, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
#DF.temp$ring_ring <-(with(DF.temp, paste(Focal.ring, N10.father, sep="_")))
#names(N_123_full)[2] <- "N10.FATHERfp"
#DF.temp <- merge(DF.temp, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)

DF$ring_ring <-(with(DF, paste(Focal.ring, N10.mother, sep="_")))
names(N_123_full)[2] <- "N10.MOTHERfp"
DF.temp <- merge(DF, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
CN10.mother <- DF.temp[,c(9,97)]
DF$ring_ring <-(with(DF, paste(Focal.ring, N10.father, sep="_")))
names(N_123_full)[2] <- "N10.FATHERfp"
DF.temp <- merge(DF, N_123_full, by=c("ring_ring", "Year.s"), all.x=TRUE)
CN10.father<- DF.temp[,c(9,97)]


#ok let's try to put them together?

CN1 <- merge(CN1.mother, CN1.father, by="Box.year.parentid")
CN2 <- merge(CN2.mother, CN2.father, by="Box.year.parentid")
CN3 <- merge(CN3.mother, CN3.father, by="Box.year.parentid")
CN4 <- merge(CN4.mother, CN4.father, by="Box.year.parentid")
CN5 <- merge(CN5.mother, CN5.father, by="Box.year.parentid")
CN6 <- merge(CN6.mother, CN6.father, by="Box.year.parentid")
CN7 <- merge(CN7.mother, CN7.father, by="Box.year.parentid")
CN8 <- merge(CN8.mother, CN8.father, by="Box.year.parentid")
CN9 <- merge(CN9.mother, CN9.father, by="Box.year.parentid")
CN10 <- merge(CN10.mother, CN10.father, by="Box.year.parentid")

x <- merge(CN1, CN2, by="Box.year.parentid")
x <- merge(x, CN3, by="Box.year.parentid")
x <- merge(x, CN4, by="Box.year.parentid")
x <- merge(x, CN5, by="Box.year.parentid")
x <- merge(x, CN6, by="Box.year.parentid")
x <- merge(x, CN7, by="Box.year.parentid")
x <- merge(x, CN8, by="Box.year.parentid")
x <- merge(x, CN9, by="Box.year.parentid")
x <- merge(x, CN10, by="Box.year.parentid")


DF.temp <- merge(DF, x, by="Box.year.parentid", all.x=TRUE)


DF.temp <- DF.temp[c(1:770),]
test1 <- as.data.frame(order(DF.temp$Box.year.parentid))
test2 <- as.data.frame(order(DF$Box.year.parentid))
summary(arsenal::comparedf(test1, test2))

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

DF.temp$boxes <- NULL

DF <- DF.temp

library(tidyverse)
DF <- DF %>% rename_with(str_to_title)

setwd("~/Documents/2/Familiar_neighbors/familiarneighbor/Data")
saveRDS(DF, "CleanData2.rds")







