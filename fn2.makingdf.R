setwd("~/Documents/2/Familiar_neighbors/DATA/fn2forgit")

library(tidyr)
library(dplyr)
library(magrittr)
library(reshape2)

#load data 
#load("Data Package A - Social Network Data For Samin(1).RData")
#load("Data Package B - Spatial Data For Samin(corrected).RData")
#agedata <- read.csv("GRETI_Age_Data.csv")

a.nests <- readRDS("Data/a.nests.Rds")
l.am.t <- readRDS("Data/l.am.t.Rds")
dat.info <- readRDS("Data/dat.info.Rds")
l.gbi <- readRDS("Data/l.gbi.Rds")
l.gd <- readRDS("Data/l.gd.Rds")
agedata <- readRDS("Data/agedata.Rds")



#make a base ####
xdata <- a.nests
xdata[xdata==""]<-NA #make empty cells NA

#uppercaseboxes 
xdata$nestbox <- toupper(xdata$nestbox)

#remove data from 2015 and 2016
xdata <- xdata[which(xdata$year!= 2015),] 
xdata <- xdata[which(xdata$year!= 2016),] 

#keep nests where both parents are known 
xdata <-
  xdata %>% 
  mutate_at(c("mother", "father"), as.character) %>% 
  mutate(mID = mother != "") %>% 
  mutate(fID = father != "")

xdata <-
  xdata %>% 
  mutate_at(c("mID", "fID"), as.numeric) %>% 
  mutate(nest.id.type = mID + fID + 1) %>% 
  mutate(nest.id.type = c("neither", "one", "both")[nest.id.type])

xdata %<>% filter(nest.id.type == "both")


##make a row for each individual in each pair 
xdata %<>% as.data.frame()

x <- data.table::setDT(xdata)[, .(parent = c("mother","father")),
                              .(year, nestbox, x, y, breeding.attempt,
                                section, species, april.lay.date, laying.rate, incubation.started,
                                april.hatch.date, incubation.duration, total.egg.weight, num.eggs.weighed,
                                clutch.size, num.chicks, num.dead.chicks, num.fledglings, mean.chick.weight,
                                father, mother, box, box.yr)]

xdata <- x

#get the focal ring 
xdata$focal.ring <- NA

xdata$focal.ring <- with(xdata, ifelse(parent == "mother", xdata$mother, 
                                         xdata$focal.ring)) #label one ID

xdata$focal.ring <- with(xdata, ifelse(parent == "father", xdata$father, 
                                         xdata$focal.ring)) #label one ID

#add binary success variable  
xdata %<>% mutate(binary.succ = as.numeric(num.fledglings > 0))

#add individual information 

#sex 
xdata$focal.sex <- NA

xdata$focal.sex <- with(xdata, ifelse(parent == "mother", "F", 
                                      xdata$focal.sex)) 
xdata$focal.sex <- with(xdata, ifelse(parent == "father", "M", 
                                      xdata$focal.sex)) 

#age
xdata$focal.ring <- toupper(xdata$focal.ring)
agedata <- agedata[,c(2,3,4,10)]
agedata$Season <- gsub("^.{0,5}", "", agedata$Season)  

agedata$Age <- agedata$Estimate_Age
agedata$Age <- with(agedata, ifelse(Age > 1, "adult", agedata$Age)) 
agedata$Age <- with(agedata, ifelse(Age < 2, "juvenile", agedata$Age)) 

agedata$Season <- NULL
names(agedata)[1] <- "focal.ring"
names(agedata)[2] <- "year"
names(agedata)[3] <- "age_num"
names(agedata)[4] <- "age_cat"

agedata <- data.frame(agedata,"ring.year"=paste(agedata$focal.ring, agedata$year,sep="_")) 
agedata <- agedata[!duplicated(agedata[,"ring.year"]),]
agedata$ring.year <- NULL

xdata <- merge(xdata, agedata, by=c("focal.ring", "year"), all.x=TRUE)

#filling in from id.info and agedata
xdata[6, 29] = "juvenile" #D471250
xdata[6, 28] = 1 #D471250
xdata[41, 29] = "adult" #D472794
xdata[41, 28] = 2 #D472794
xdata[322, 29] = "adult" #TP25494
xdata[322, 28] = 5 #TP25494
xdata[396, 29] = "adult" #TP42153
xdata[396, 28] = 3 #TP42153
xdata[684, 29] = "adult" #TS46406
xdata[684, 28] = 3 #TS46406
xdata[795, 29] = "adult" #X235629
xdata[795, 28] = 5 #X235629
xdata[1074, 29] = "juvenile"#Y163141
xdata[1074, 28] = 1 #Y163141
xdata[1110, 29] = "adult" #Y837301
xdata[1110, 28] = 2 #Y837301
xdata[1163, 29] = "adult" #Y838931
xdata[1163, 28] = 3 #Y838931

#unknown age?? 
#V279628
#Y161026
#L809317

#add winter year 

xdata$year.w <- NA

names(xdata)[names(xdata) == "year"] <- "year.s"


xdata$year.w <- with(xdata, ifelse(year.s == "2012", "2011", 
                                   xdata$year.w)) 
xdata$year.w <- with(xdata, ifelse(year.s == "2013", "2012", 
                                   xdata$year.w)) 
xdata$year.w <- with(xdata, ifelse(year.s == "2014", "2013", 
                                   xdata$year.w)) 

#rename 
names(xdata)[names(xdata) == "sex"] <- "focal.sex"
names(xdata)[names(xdata) == "age"] <- "focal.age"


####yearly network metrics - "social flocking associations" ####
#2011 
net2011 <- l.am.t[[1]]
Strength <- colSums(net2011)
Strength_Mean <- colMeans(net2011) 
Degree <- sna::degree(net2011)
ring <- row.names(net2011)


x <- as.data.frame(cbind(ring, Strength, Strength_Mean, Degree))
x$node <- NULL
x$year.w <- 2011

met2011.w <- x 

#2012 
net2012 <- l.am.t[[2]]
Strength <- colSums(net2012)
Strength_Mean <- colMeans(net2012) 
Degree <- sna::degree(net2012)
ring <- row.names(net2012)

x <- as.data.frame(cbind(ring, Strength, Strength_Mean, Degree))
x$node <- NULL
x$year.w <- 2012

met2012.w <- x

#2013 
net2013 <- l.am.t[[3]]
Strength <- colSums(net2013)
Strength_Mean <- colMeans(net2013) 
Degree <- sna::degree(net2013)
ring <- row.names(net2013)

x <- as.data.frame(cbind(ring, Strength, Strength_Mean, Degree))
x$node <- NULL
x$year.w <- 2013

met2013.w <- x

#put all three years together
x <- rbind(met2011.w, met2012.w, met2013.w)
names(x)[names(x) == "ring"] <- "focal.ring"
xdata$focal.ring <- tolower(xdata$focal.ring)

#add it to xdata
xdata$year.w <- as.numeric(xdata$year.w)
y <- merge(xdata, x, by=c("year.w","focal.ring"), all.x=TRUE)

xdata <- y

rm(x)
rm(y)

#keep nests where both parents have social data 
xdata <-
  xdata %>% 
  mutate_at(c("Degree"), as.character) %>% 
  mutate(socialinfo = !is.na(Degree))


xdata %<>% filter(socialinfo == "TRUE")


xdata$Strength <- as.character(xdata$Strength)
xdata$Strength <- as.numeric(xdata$Strength)
xdata$Strength_Mean <- as.character(xdata$Strength_Mean)
xdata$Strength_Mean <- as.numeric(xdata$Strength_Mean)
xdata$Degree <- as.numeric(xdata$Degree)

#### "social pair bond" ####
#need to pull cell from association matrix 

xdata2011 <- xdata[which(xdata$year.w == 2011),] 
xdata2012 <- xdata[which(xdata$year.w == 2012),] 
xdata2013 <- xdata[which(xdata$year.w == 2013),] 

bond2011 <- net2011 %>% reshape2::melt() %>% 
  rename(mother = Var1, father = Var2, bondstrength = value)%>% 
  left_join(xdata2011, ., by = c("mother", "father")) 

bond2012 <- net2012 %>% reshape2::melt() %>% 
  rename(mother = Var1, father = Var2, bondstrength = value)%>% 
  left_join(xdata2012, ., by = c("mother", "father")) 

bond2013 <- net2013 %>% reshape2::melt() %>% 
  rename(mother = Var1, father = Var2, bondstrength = value)%>% 
  left_join(xdata2013, ., by = c("mother", "father")) 

xdata <- rbind(bond2011, bond2012, bond2013)

#keep the ones that have pair bond strength info 
xdata <-
  xdata %>% 
  mutate_at(c("bondstrength"), as.character) %>% 
  mutate(socialinfo = !is.na(bondstrength))


xdata %<>% filter(socialinfo == "TRUE")


BASE <- xdata

####familiarity to neighbors "social territorial bonds"##### 
#get parent info ####
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

xx2012 %>% group_by(box2) %>% mutate(id = seq_along(box2)) -> xx2012

nei2012 <- pivot_wider(xx2012, names_from= "id", values_from="box1")

#### 2013
xx2013 <- xx[which(xx$year == 2013),]
xx2013$year <- NULL

#order by box then distance 
xx2013 <- xx2013[order(xx2013$box2, xx2013$dis, sample(1:nrow(xx2013))),]
xx2013$dis <- NULL

xx2013 %>% group_by(box2) %>% mutate(id = seq_along(box2)) -> xx2013

nei2013 <- pivot_wider(xx2013, names_from= "id", values_from="box1")

#### 2014
xx2014 <- xx[which(xx$year == 2014),]
xx2014$year <- NULL

#order by box then distance 
xx2014 <- xx2014[order(xx2014$box2, xx2014$dis, sample(1:nrow(xx2014))),]
xx2014$dis <- NULL

xx2014 %>% group_by(box2) %>% mutate(id = seq_along(box2)) -> xx2014

nei2014 <- pivot_wider(xx2014, names_from= "id", values_from="box1")

#add "N" to column names 
colnames(nei2012) <- paste0('N', colnames(nei2012))
names(nei2012)[1] <- "focal.box"
colnames(nei2013) <- paste0('N', colnames(nei2013))
names(nei2013)[1] <- "focal.box"
colnames(nei2014) <- paste0('N', colnames(nei2014))
names(nei2014)[1] <- "focal.box"


#only keep the ones that we need for the base data 

#2012
xdata2011 <- xdata[which(xdata$year.w==2011),]
box2012 <- xdata2011[,4]
box2012$occupied <- "yes"
names(box2012)[names(box2012) == "nestbox"] <- "focal.box"
box2012 <- box2012[!duplicated(box2012[ , "focal.box"]), ]

nei2012$focal.box <- as.character(nei2012$focal.box)
nei2012 <- merge(nei2012, box2012, by="focal.box", all.x = TRUE)
nei2012 <- nei2012[which(nei2012$occupied=="yes"),]

nei2012$occupied <- NULL


#2013
xdata2012 <- xdata[which(xdata$year.w==2012),]
box2013 <- xdata2012[,4]
box2013$occupied <- "yes"
names(box2013)[names(box2013) == "nestbox"] <- "focal.box"
box2013 <- box2013[!duplicated(box2013[ , "focal.box"]), ]

nei2013$focal.box <- as.character(nei2013$focal.box)
nei2013 <- merge(nei2013, box2013, by="focal.box", all.x = TRUE)
nei2013 <- nei2013[which(nei2013$occupied=="yes"),]

nei2013$occupied <- NULL


#2014
xdata2013 <- xdata[which(xdata$year.w==2013),]
box2014 <- xdata2013[,4]
box2014$occupied <- "yes"
names(box2014)[names(box2014) == "nestbox"] <- "focal.box"
box2014 <- box2014[!duplicated(box2014[ , "focal.box"]), ]

nei2014$focal.box <- as.character(nei2014$focal.box)
nei2014 <- merge(nei2014, box2014, by="focal.box", all.x = TRUE)
nei2014 <- nei2014[which(nei2014$occupied=="yes"),]

nei2014$occupied <- NULL

##so now we need to add the parent information for each neighbor
#get a list of all parents in 2012
xx <- a.nests[,c(2,3,21,22)]
xx$nestbox <- toupper(xx$nestbox)

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




#adding bond strength for each focal and neighbor
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



#### number of neighbors and average bond strength ####

#2012
## add number of neighbors 
temp <- as.data.frame((is.na(zz2012[,c("N1","N2","N3","N4","N5","N6","N7","N8","N9","N10")])))
temp %>% mutate_if(is.logical,as.numeric) -> temp
temp$sumna <- rowSums(temp)
temp$N.num <- 10 - (temp$sumna)
zz2012$N.num <- temp$N.num

#add number of female neighbors with network metrics
temp <- as.data.frame((!is.na(zz2012[,c("N1.mbs","N2.mbs","N3.mbs","N4.mbs","N5.mbs","N6.mbs","N7.mbs","N8.mbs","N9.mbs","N10.mbs")])))
temp %>% mutate_if(is.logical,as.numeric) -> temp
temp$sumIDf <- rowSums(temp)
zz2012$N.num.female.sm <- temp$sumIDf

#get average strength to female neighbors
temp <- as.data.frame((zz2012[,c("N1.mbs","N2.mbs","N3.mbs","N4.mbs","N5.mbs","N6.mbs","N7.mbs","N8.mbs","N9.mbs","N10.mbs")]))
temp %<>% 
  mutate_at(c("N1.mbs","N2.mbs","N3.mbs","N4.mbs","N5.mbs","N6.mbs","N7.mbs","N8.mbs","N9.mbs","N10.mbs"), 
            ~ifelse(is.na(.x), 0, .x))    %>% 
  mutate_at(c("N1.mbs","N2.mbs","N3.mbs","N4.mbs","N5.mbs","N6.mbs","N7.mbs","N8.mbs","N9.mbs","N10.mbs"), as.numeric)
temp$sumMBS <- rowSums(temp)
zz2012$N.sum.female.sm <- temp$sumMBS

zz2012$N.avg.female.bs <- zz2012$N.sum.female.sm / zz2012$N.num.female.sm



#add number of male neighbors with network metrics
temp <- as.data.frame((!is.na(zz2012[,c("N1.fbs","N2.fbs","N3.fbs","N4.fbs","N5.fbs","N6.fbs","N7.fbs","N8.fbs","N9.fbs","N10.fbs")])))
temp %>% mutate_if(is.logical,as.numeric) -> temp
temp$sumIDm <- rowSums(temp)
zz2012$N.num.male.sm <- temp$sumIDm

#get average strength to male neighbors
temp <- as.data.frame((zz2012[,c("N1.fbs","N2.fbs","N3.fbs","N4.fbs","N5.fbs","N6.fbs","N7.fbs","N8.fbs","N9.fbs","N10.fbs")]))
temp %<>% 
  mutate_at(c("N1.fbs","N2.fbs","N3.fbs","N4.fbs","N5.fbs","N6.fbs","N7.fbs","N8.fbs","N9.fbs","N10.fbs"), 
            ~ifelse(is.na(.x), 0, .x))    %>% 
  mutate_at(c("N1.fbs","N2.fbs","N3.fbs","N4.fbs","N5.fbs","N6.fbs","N7.fbs","N8.fbs","N9.fbs","N10.fbs"), as.numeric)
temp$sumMBS <- rowSums(temp)
zz2012$N.sum.male.sm <- temp$sumMBS

zz2012$N.avg.male.bs <- zz2012$N.sum.male.sm / zz2012$N.num.male.sm

zz2012$N.avg.bs <- (zz2012$N.sum.male.sm + zz2012$N.sum.female.sm) / (zz2012$N.num.male.sm + zz2012$N.num.female.sm) 

zz2012 %<>% 
  mutate_at("N.avg.male.bs", 
            ~ifelse(.x == "NaN", 0, .x))
#2013
## add number of neighbors 
temp <- as.data.frame((is.na(zz2013[,c("N1","N2","N3","N4","N5","N6","N7","N8","N9","N10")])))
temp %>% mutate_if(is.logical,as.numeric) -> temp
temp$sumna <- rowSums(temp)
temp$N.num <- 10 - (temp$sumna)
zz2013$N.num <- temp$N.num

#add number of female neighbors with network metrics
temp <- as.data.frame((!is.na(zz2013[,c("N1.mbs","N2.mbs","N3.mbs","N4.mbs","N5.mbs","N6.mbs","N7.mbs","N8.mbs","N9.mbs","N10.mbs")])))
temp %>% mutate_if(is.logical,as.numeric) -> temp
temp$sumIDf <- rowSums(temp)
zz2013$N.num.female.sm <- temp$sumIDf

#get average strength to female neighbors
temp <- as.data.frame((zz2013[,c("N1.mbs","N2.mbs","N3.mbs","N4.mbs","N5.mbs","N6.mbs","N7.mbs","N8.mbs","N9.mbs","N10.mbs")]))
temp %<>% 
  mutate_at(c("N1.mbs","N2.mbs","N3.mbs","N4.mbs","N5.mbs","N6.mbs","N7.mbs","N8.mbs","N9.mbs","N10.mbs"), 
            ~ifelse(is.na(.x), 0, .x))    %>% 
  mutate_at(c("N1.mbs","N2.mbs","N3.mbs","N4.mbs","N5.mbs","N6.mbs","N7.mbs","N8.mbs","N9.mbs","N10.mbs"), as.numeric)
temp$sumMBS <- rowSums(temp)
zz2013$N.sum.female.sm <- temp$sumMBS

zz2013$N.avg.female.bs <- zz2013$N.sum.female.sm / zz2013$N.num.female.sm


#add number of male neighbors with network metrics
temp <- as.data.frame((!is.na(zz2013[,c("N1.fbs","N2.fbs","N3.fbs","N4.fbs","N5.fbs","N6.fbs","N7.fbs","N8.fbs","N9.fbs","N10.fbs")])))
temp %>% mutate_if(is.logical,as.numeric) -> temp
temp$sumIDm <- rowSums(temp)
zz2013$N.num.male.sm <- temp$sumIDm

#get average strength to male neighbors
temp <- as.data.frame((zz2013[,c("N1.fbs","N2.fbs","N3.fbs","N4.fbs","N5.fbs","N6.fbs","N7.fbs","N8.fbs","N9.fbs","N10.fbs")]))
temp %<>% 
  mutate_at(c("N1.fbs","N2.fbs","N3.fbs","N4.fbs","N5.fbs","N6.fbs","N7.fbs","N8.fbs","N9.fbs","N10.fbs"), 
            ~ifelse(is.na(.x), 0, .x))    %>% 
  mutate_at(c("N1.fbs","N2.fbs","N3.fbs","N4.fbs","N5.fbs","N6.fbs","N7.fbs","N8.fbs","N9.fbs","N10.fbs"), as.numeric)
temp$sumMBS <- rowSums(temp)
zz2013$N.sum.male.sm <- temp$sumMBS

zz2013$N.avg.male.bs <- zz2013$N.sum.male.sm / zz2013$N.num.male.sm

zz2013$N.avg.bs <- (zz2013$N.sum.male.sm + zz2013$N.sum.female.sm) / (zz2013$N.num.male.sm + zz2013$N.num.female.sm) 

zz2013 %<>% 
  mutate_at("N.avg.male.bs", 
            ~ifelse(.x == "NaN", 0, .x))


#2014
## add number of neighbors 
temp <- as.data.frame((is.na(zz2014[,c("N1","N2","N3","N4","N5","N6","N7","N8","N9","N10")])))
temp %>% mutate_if(is.logical,as.numeric) -> temp
temp$sumna <- rowSums(temp)
temp$N.num <- 10 - (temp$sumna)
zz2014$N.num <- temp$N.num

#add number of female neighbors with network metrics
temp <- as.data.frame((!is.na(zz2014[,c("N1.mbs","N2.mbs","N3.mbs","N4.mbs","N5.mbs","N6.mbs","N7.mbs","N8.mbs","N9.mbs","N10.mbs")])))
temp %>% mutate_if(is.logical,as.numeric) -> temp
temp$sumIDf <- rowSums(temp)
zz2014$N.num.female.sm <- temp$sumIDf

#get average strength to female neighbors
temp <- as.data.frame((zz2014[,c("N1.mbs","N2.mbs","N3.mbs","N4.mbs","N5.mbs","N6.mbs","N7.mbs","N8.mbs","N9.mbs","N10.mbs")]))
temp %<>% 
  mutate_at(c("N1.mbs","N2.mbs","N3.mbs","N4.mbs","N5.mbs","N6.mbs","N7.mbs","N8.mbs","N9.mbs","N10.mbs"), 
            ~ifelse(is.na(.x), 0, .x))    %>% 
  mutate_at(c("N1.mbs","N2.mbs","N3.mbs","N4.mbs","N5.mbs","N6.mbs","N7.mbs","N8.mbs","N9.mbs","N10.mbs"), as.numeric)
temp$sumMBS <- rowSums(temp)
zz2014$N.sum.female.sm <- temp$sumMBS

zz2014$N.avg.female.bs <- zz2014$N.sum.female.sm / zz2014$N.num.female.sm



#add number of male neighbors with network metrics
temp <- as.data.frame((!is.na(zz2014[,c("N1.fbs","N2.fbs","N3.fbs","N4.fbs","N5.fbs","N6.fbs","N7.fbs","N8.fbs","N9.fbs","N10.fbs")])))
temp %>% mutate_if(is.logical,as.numeric) -> temp
temp$sumIDm <- rowSums(temp)
zz2014$N.num.male.sm <- temp$sumIDm

#get average strength to male neighbors
temp <- as.data.frame((zz2014[,c("N1.fbs","N2.fbs","N3.fbs","N4.fbs","N5.fbs","N6.fbs","N7.fbs","N8.fbs","N9.fbs","N10.fbs")]))
temp %<>% 
  mutate_at(c("N1.fbs","N2.fbs","N3.fbs","N4.fbs","N5.fbs","N6.fbs","N7.fbs","N8.fbs","N9.fbs","N10.fbs"), 
            ~ifelse(is.na(.x), 0, .x))    %>% 
  mutate_at(c("N1.fbs","N2.fbs","N3.fbs","N4.fbs","N5.fbs","N6.fbs","N7.fbs","N8.fbs","N9.fbs","N10.fbs"), as.numeric)
temp$sumMBS <- rowSums(temp)
zz2014$N.sum.male.sm <- temp$sumMBS

zz2014$N.avg.male.bs <- zz2014$N.sum.male.sm / zz2014$N.num.male.sm

zz2014$N.avg.bs <- (zz2014$N.sum.male.sm + zz2014$N.sum.female.sm) / (zz2014$N.num.male.sm + zz2014$N.num.female.sm) 

zz2014 %<>% 
  mutate_at("N.avg.male.bs", 
            ~ifelse(.x == "NaN", 0, .x))

#spatial associations..####

###failed attempt ####
#temp <- l.gbi[[1]] 
#temp.df <- as.data.frame(temp)
#temp2 <- (l.gd[[1]])
#temp.df$logger <- temp2$logger
#
#Loggers <- unique(temp.df$logger)
#Logger <- Loggers[1]
#LoggerSums <- list()
#
##beginning of loop 
#
#for(Logger in Loggers){
#  
#  print(Logger)
#  
#  data <- temp.df %>% filter(logger == Logger)
#  
#  sums <- colSums(Filter(is.numeric, data))
#  
#  LoggerSums[[which(Loggers == Logger)]] <- sums
#  
#}
#
##end of loop ####
#
#zz <- as.data.frame(LoggerSums)
#colnames(zz) <- Loggers
#
##zz[zz == 0] <- "no"
##zz[zz != "no"] <- "yes"
#zz$focal.ring <- rownames(zz)


###code from josh for this #### 
#2012
inds.group<-apply(l.gbi[[1]] ,2,function(a)which(a>0))

inds.locs<-sapply(inds.group,function(a)table(l.gd[[1]]$logger[a]))

locs<-sort(unique(l.gd[[1]]$logger));ids.loc<-names(inds.locs)

lbi<-matrix(0,length(locs),length(ids.loc),dimnames=list(locs,ids.loc))

for(ll in 1:length(inds.locs)){
  
  vals.in<-inds.locs[[ll]]
  
  lbi[names(vals.in),ll]<-as.numeric(vals.in)}

lbi<-apply(lbi,2,function(a)a/sum(a))

get.lam<-function(lbi){
  
  xab<-matrix(NA,ncol(lbi),ncol(lbi))
  
  for(i1 in 1:ncol(lbi)){
    
    #for each dyad, find the spatial overlap between them. So, if they spent equal proportions of times in the smae places, they get 1.
    
    focal.occur<-lbi[,i1]
    
    xab[,i1]<-apply(lbi,2,function(a)sum(apply(cbind(a,focal.occur),1,min)))
    
  }
  
  am<-xab
  
  diag(am)<-0;am[is.nan(am)]<-0;rownames(am)<-colnames(am)<-colnames(lbi)
  
  am}

lam2012 <-get.lam(lbi) #checked and works well

#2013
inds.group<-apply(l.gbi[[2]] ,2,function(a)which(a>0))

inds.locs<-sapply(inds.group,function(a)table(l.gd[[2]]$logger[a]))

locs<-sort(unique(l.gd[[2]]$logger));ids.loc<-names(inds.locs)

lbi<-matrix(0,length(locs),length(ids.loc),dimnames=list(locs,ids.loc))

for(ll in 1:length(inds.locs)){
  
  vals.in<-inds.locs[[ll]]
  
  lbi[names(vals.in),ll]<-as.numeric(vals.in)}

lbi<-apply(lbi,2,function(a)a/sum(a))

lam2013 <-get.lam(lbi) #checked and works well

#2014
inds.group<-apply(l.gbi[[3]] ,2,function(a)which(a>0))

inds.locs<-sapply(inds.group,function(a)table(l.gd[[3]]$logger[a]))

locs<-sort(unique(l.gd[[3]]$logger));ids.loc<-names(inds.locs)

lbi<-matrix(0,length(locs),length(ids.loc),dimnames=list(locs,ids.loc))

for(ll in 1:length(inds.locs)){
  
  vals.in<-inds.locs[[ll]]
  
  lbi[names(vals.in),ll]<-as.numeric(vals.in)}

lbi<-apply(lbi,2,function(a)a/sum(a))

lam2014 <-get.lam(lbi) #checked and works well


#get the info for each individual 
spas2012 <- as.data.frame(colSums(lam2012))
spas2012$focal.ring <- rownames(spas2012)
spas2012$year.s <- 2012
names(spas2012)[1] <- "spatial.assoc"

spas2013 <- as.data.frame(colSums(lam2013))
spas2013$focal.ring <- rownames(spas2013)
spas2013$year.s <- 2013
names(spas2013)[1] <- "spatial.assoc"

spas2014 <- as.data.frame(colSums(lam2014))
spas2014$focal.ring <- rownames(spas2014)
spas2014$year.s <- 2014
names(spas2014)[1] <- "spatial.assoc"

zz2012 <- merge(zz2012, spas2012, by=c("focal.ring","year.s"))
zz2013 <- merge(zz2013, spas2013, by=c("focal.ring","year.s"))
zz2014 <- merge(zz2014, spas2014, by=c("focal.ring","year.s"))


###putting them back together ?####
xdata <- rbind(zz2012, zz2013, zz2014)

#clean up
xdata$bondstrength <- as.numeric(xdata$bondstrength)
colnames(xdata) <- stringr::str_to_title(colnames(xdata))

###adding oak health info#### 
HQ <- readRDS("Data/habitatquality.Rds")
fn.data <- merge(xdata, HQ, by="Box", all.x=TRUE)

#data checks - remove duplicates (from merging?)

fn <- fn.data

fn2<-unique(fn)
fn2$Ring.yr<-paste(fn2$Focal.ring,fn2$Year.s)
table(fn2$Ring.yr)[table(fn2$Ring.yr)>1] 
fn2.dups<-fn2[fn2$Ring.yr%in%fn2$Ring.yr[duplicated(fn2$Ring.yr)],]
fn2.dups[1:4,]
min.dates<-tapply(fn2$April.lay.date,fn2$Ring.yr,function(a)min(a))
fn3<-fn2[paste(fn2$Ring.yr,fn2$April.lay.date) %in% paste(names(min.dates),min.dates),]
fn3$Box.yr<-paste(fn3$Box,fn3$Year.s)
table(fn3$Box.yr)[table(fn3$Box.yr)>2] #after removing the above individual-within-year duplicates, we don't appear to have any issues with box-within-year  duplicates either
table(fn3$Box.yr[fn3$Parent%in%"mother"])[table(fn3$Box.yr[fn3$Parent%in%"mother"])>1] #true for just mothers
table(fn3$Box.yr[fn3$Parent%in%"father"])[table(fn3$Box.yr[fn3$Parent%in%"father"])>1] #and true for just fathers
fath.t<-fn3[fn3$Parent=="father",c("Box.yr","Father","Mother")]
moth.t<-fn3[fn3$Parent=="mother",c("Box.yr","Father","Mother")]
mean(unique(fn3$Box.yr) %in% fath.t$Box.yr  & unique(fn3$Box.yr) %in% moth.t$Box.yr) #only minority of the box-year have both the mother and father known?
both.parents<-unique(fn3$Box.yr[fn3$Box.yr %in% fath.t$Box.yr  & fn3$Box.yr %in% moth.t$Box.yr])
fath.b<-fath.t[match(both.parents,fath.t$Box.yr),]
moth.b<-moth.t[match(both.parents,moth.t$Box.yr),]
mean(fath.b$Father ==moth.b$Father)
mean(fath.b$Mother ==moth.b$Mother)

#DONE

fn2.data.full <- fn3

saveRDS(fn2.data.full, "fn2.data.full.Rds")



