#INTERIM SCRIPT FOR LFA 41 BYCATCH
require (chron)
require (lattice)
require (MASS)
require (doBy)
require (reshape)
library (maptools)     # loads sp library too
library (RColorBrewer) # creates nice color schemes
library (xlsx)
library(ggplot2)
library(dplyr)
library(bio.base)
library(bio.lobster)
library(bio.utilities)
library(PBSmapping)

#Observer Data:
lobster.db('observer41.redo')
lobster.db('observer41')
str(observer41) # SEPT_2016 7,372 RECORDS
summary(observer41)

#Commercial Logs:
#Lobster
lobster.db("logs41.redo")
lobster.db("logs41")
logs41$YEAR<-year(logs41$FV_FISHED_DATETIME) #Data from 2002-to Present
names(logs41)[names(logs41)=="FV_FISHED_DATETIME"]<-"DATE_FISHED"
logs41$LANDINGS_KG<-logs41$ADJCATCH*0.4536
logs41$LANDINGS_T<-logs41$ADJCATCH*0.0004536
logs41$SYEAR<-sapply(logs41$DATE_FISHED,offFishingYear)
str(logs41) #SEPT_2016 37,955 RECORDS
summary(logs41)

#Landings Table
ann.41<-logs41%>%group_by(YEAR)%>%summarise(TOT_LAND_T=sum(LANDINGS_T,na.rm=TRUE), TOT_LAND_KG=sum(LANDINGS_KG, na.rm=TRUE))%>%ungroup()%>%data.frame()

#At-Sea Sampling Data:
lobster.db("atSea.redo")
lobster.db("atSea")
atSea$YEAR<-year(atSea$STARTDATE)
str(atSea)#Data from 1972-to Present 2,754,832 RECORDS OCT_2016
atSea41<-subset(atSea, LFA=="41")
str(atSea41) # 330,793 RECORDS
summary(atSea41)

#CREATE DATE FORMATS FOR 2 DATA SOURCES:
#OBSERVER
observer41$LONGITUDE<-observer41$LONGITUDE*-1
observer41$YEAR<- as.numeric(format(observer41$BOARD_DATE,"%Y"))
str(observer41)
##Add an estimated total weight column:
df<-cbind(observer41$EST_KEPT_WT,observer41$EST_DISCARD_WT)
a<-apply(df,1,sum,na.rm=T)
observer41$EST_TOT_WT<-a
head(observer41)
##Remove records that have SOURCE=1, these are logged accounts of catch not observed:
table(observer41$SOURCE)
   0    1 
6563  898 
LFA41.obsv.filt<-subset(observer41,SOURCE==0)
str(LFA41.obsv.filt) #6,563 RECORDS

#AT-SEA DATA
atSea41<-addwt(atSea41)
species.atsea<-aggregate(CALC_WT ~ SPECIES + SPECIESCODE, data = atSea41, sum, na.rm=T)
species.atsea<-species.atsea[order(species.atsea$CALC_WT,decreasing=T),]
head(atSea41)
#Create list of all species recorded based on trip within 2002-Present
species<-aggregate(EST_DISCARD_WT ~ COMMON + SPECCD_ID, data = observer41, sum, na.rm=T)
species<-species[order(species$EST_DISCARD_WT,decreasing=T),]

#################################################### BY-CATCH DESCRIPTIVE STATS ##############################################################
#How many observer trips per year:
str(LFA41.obsv.filt)
length(unique(paste(LFA41.obsv.filt$BOARD_DATE,LFA41.obsv.filt$TRIP_ID))) # 77 SAMPLES
ann.trip<-LFA41.obsv.filt%>%group_by(YEAR)%>%summarise(TOT_SAMP=length(unique(TRIP_ID)))%>%ungroup()%>%data.frame()

YEAR TOT_SAMP
1  2002        5
2  2003        7
3  2004        3
4  2005        9
5  2006        8
6  2007        5
7  2008        4
8  2009        4
9  2010        3
10 2011        3
11 2012        5
12 2013        6
13 2014        6
14 2015        4
15 2016        5

#There are 77 observer trips from 2002 to 2016. Catch/Discards can be from multiple subareas within a trip
length(unique(paste(LFA41.obsv.filt$BOARD_DATE,LFA41.obsv.filt$TRIP_ID,LFA41.obsv.filt$OFFAREA))) # 149 SAMPLES

#Remove select fishing year
LFA41.obs.0615<-subset(LFA41.obsv.filt,YEAR>2005 & YEAR<2016)
LFA41.logs.0615<-subset(logs41,YEAR>2005 & YEAR<2016)
LFA41.atsea.0615<-subset(atSea41,YEAR>2005 & YEAR<2016)
#OBSERVER
#Calculate total lobster by 3-year groupings:
LFA41.obs.0615$yr3.grp<-NA
G1=which(LFA41.obs.0615$YEAR %in% c(2006:2008))
LFA41.obs.0615[G1,"yr3.grp"]="2006-08"
G2=which(LFA41.obs.0615$YEAR %in% c(2009:2011))
LFA41.obs.0615[G2,"yr3.grp"]="2009-11"
G3=which(LFA41.obs.0615$YEAR %in% c(2012:2015))
LFA41.obs.0615[G3,"yr3.grp"]="2012-15"
#5 YEAR IDENTIFIER:
LFA41.obs.0615$yr5.grp<-NA
H1=which(LFA41.obs.0615$YEAR %in% c(2006:2010))
LFA41.obs.0615[H1,"yr5.grp"]="2006-10"
H2=which(LFA41.obs.0615$YEAR %in% c(2011:2015))
LFA41.obs.0615[H2,"yr5.grp"]="2011-15"

#LOGS
#Calculate total lobster by 3-year groupings:
LFA41.logs.0615$yr3.grp<-NA
G1=which(LFA41.logs.0615$YEAR %in% c(2006:2008))
LFA41.logs.0615[G1,"yr3.grp"]="2006-08"
G2=which(LFA41.logs.0615$YEAR %in% c(2009:2011))
LFA41.logs.0615[G2,"yr3.grp"]="2009-11"
G3=which(LFA41.logs.0615$YEAR %in% c(2012:2015))
LFA41.logs.0615[G3,"yr3.grp"]="2012-15"
#5 YEAR IDENTIFIER:
LFA41.logs.0615$yr5.grp<-NA
H1=which(LFA41.logs.0615$YEAR %in% c(2006:2010))
LFA41.logs.0615[H1,"yr5.grp"]="2006-10"
H2=which(LFA41.logs.0615$YEAR %in% c(2011:2015))
LFA41.logs.0615[H2,"yr5.grp"]="2011-15"

#AT_SEA
#Calculate total lobster by 3-year groupings:
LFA41.atsea.0615$yr3.grp<-NA
G1=which(LFA41.atsea.0615$YEAR %in% c(2006:2008))
LFA41.atsea.0615[G1,"yr3.grp"]="2006-08"
G2=which(LFA41.atsea.0615$YEAR %in% c(2009:2011))
LFA41.atsea.0615[G2,"yr3.grp"]="2009-11"
G3=which(LFA41.atsea.0615$YEAR %in% c(2012:2015))
LFA41.atsea.0615[G3,"yr3.grp"]="2012-15"
#5 YEAR IDENTIFIER:
LFA41.atsea.0615$yr5.grp<-NA
H1=which(LFA41.atsea.0615$YEAR %in% c(2006:2010))
LFA41.atsea.0615[H1,"yr5.grp"]="2006-10"
H2=which(LFA41.atsea.0615$YEAR %in% c(2011:2015))
LFA41.atsea.0615[H2,"yr5.grp"]="2011-15"

#OBSERVER COVERAGE
#BY TRIP
length(unique(paste(LFA41.obsv.filt$BOARD_DATE,LFA41.obsv.filt$TRIP_ID))) # 77 Trips, 2002-2016
length(unique(paste(logs41$DATE_FISHED,logs41$MON_DOC_ID))) # 5295 Trips,2002-2016

logs41$MONTH = as.character(logs41$DATE_FISHED, format="%b")
LFA41.obsv.filt$MONTH<- as.character(LFA41.obsv.filt$BOARD_DATE,"%b")

ann.trip.obsv<-LFA41.obsv.filt%>%group_by(YEAR)%>%summarise(TOT_OBSV_TRIP=length(unique(TRIP_ID)))%>%ungroup()%>%data.frame()
ann.trip.land<-logs41%>%group_by(YEAR)%>%summarise(TOT_LAND_TRIP=length(unique(MON_DOC_ID)))%>%ungroup()%>%data.frame()
trip.sum<-merge(ann.trip.obsv,ann.trip.land,all=T)
obsv.cov<-trip.sum%>%group_by(YEAR)%>%summarise(PERC_COVERAGE=TOT_OBSV_TRIP/TOT_LAND_TRIP,na.rm=T)%>%ungroup()%>%data.frame()

data<-melt(trip.sum, id=c("YEAR","MONTH"))

data$MONTH<- factor(data$MONTH, levels = c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
data$MONTH <- factor(data$MONTH, levels = data$MONTH[order(x$val)])
Palette <- c("gray87", "gray77", "gray67", "gray47","gray27","lightskyblue1", "skyblue1", "steelblue2", "royalblue1","navy")

ggplot(data,aes(MONTH,value, group=variable)) + geom_bar(data=subset(data,variable=="TOT_OBSV_TRIP"),stat='identity', position=position_dodge(),width=.8) +
  facet_wrap(~YEAR) + geom_line(data=subset(data, variable=="TOT_LAND_TRIP"), linetype=1, size=0.5, color="blue") + xlab('') + scale_y_continuous(limits=c(0,30),breaks=seq(0, 30, 5)) + 
  ggtitle("LFA41 - At-Sea Sampling Coverage") + ylab("Number of Trips") +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=0.5,size=8, colour='black'),panel.background=element_rect(fill="white", colour='black'),
        panel.border=element_rect(,fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),
        axis.title.y= element_text(size=10), title=element_text(size=12),legend.justification=c(1,1), legend.position=c(1,1),
        legend.key.size = unit(0.5,"cm")) +  
  scale_fill_manual(name="Year",values=Palette)

#PERCENT LANDINGS
obs.lob.catch<-LFA41.obsv.filt[LFA41.obsv.filt$SPECCD_ID %in% c(2550),]

ann.lob.obsv<-obs.lob.catch%>%group_by(YEAR,MONTH)%>%summarise(TOT_OBSLOB_KG=sum(EST_KEPT_WT,na.rm=T))%>%ungroup()%>%data.frame()
ann.lob.land<-logs41%>%group_by(YEAR,MONTH)%>%summarise(TOT_LAND_KG=sum(LANDINGS_KG))%>%ungroup()%>%data.frame()
perc.land<-merge(ann.lob.obsv,ann.lob.land,all=T)
month.sum<-perc.land%>%group_by(YEAR,MONTH)%>%summarise(OBS_KG=sum(TOT_OBSLOB_KG,na.rm=T),LAND_KG=sum(TOT_LAND_KG))%>%ungroup()%>%data.frame()
test.perc<-test.sum%>%group_by(YEAR)%>%summarise(OBS_KG=sum(OBS_KG,na.rm=T), LAND_KG=sum(LAND_KG))%>%ungroup()%>%data.frame()
test.sum.2<-perc.land%>%group_by(YEAR)%>%summarise(OBS_KG=sum(TOT_OBSLOB_KG,na.rm=T),LAND_KG=sum(TOT_LAND_KG))%>%ungroup()%>%data.frame()

test.sum$PERC_COVERAGE<-test.sum$OBS_KG/test.sum$LAND_KG

obsv.cov.w<-perc.land%>%group_by(YEAR)%>%summarise(PERC_COVERAGE=sum((TOT_OBSLOB_KG/TOT_LAND_KG),na.rm=T))%>%ungroup()%>%data.frame()

data<-melt(perc.land, id=c("YEAR","MONTH"))

data$MONTH<- factor(data$MONTH, levels = c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
data$MONTH <- factor(data$MONTH, levels = data$MONTH[order(x$val)])
Palette <- c("gray87", "gray77", "gray67", "gray47","gray27","lightskyblue1", "skyblue1", "steelblue2", "royalblue1","navy")

ggplot(data,aes(MONTH,value, group=variable)) + geom_bar(data=subset(data,variable=="TOT_OBSLOB_KG"),stat='identity', position=position_dodge(),width=.8) +
  facet_wrap(~YEAR) + geom_line(data=subset(data, variable=="TOT_LAND_KG"), linetype=1, size=0.5, color="blue") + xlab('') + scale_y_continuous(limits=c(0,200000),breaks=seq(0, 200000, 40000)) + 
  ggtitle("LFA41 - At-Sea Sampling Coverage") + ylab("Lobster Kg") +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=0.5,size=8, colour='black'),panel.background=element_rect(fill="white", colour='black'),
        panel.border=element_rect(,fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),
        axis.title.y= element_text(size=10), title=element_text(size=12),legend.justification=c(1,1), legend.position=c(1,1),
        legend.key.size = unit(0.5,"cm")) +  
  scale_fill_manual(name="Year",values=Palette)

#write.xlsx(LFA41.obsv.filt, paste("ObserverrawdataManon",Sys.Date(),".xlsx"))

####################################################### Annual Catch/Discard Summary ###############################################
##Observer Data
#Calculate total lobster by year:
obs.lob<-LFA41.obs.0615[LFA41.obs.0615$SPECCD_ID %in% c(2550),]
obs.kept.sp<-LFA41.obs.0615%>%group_by(YEAR,COMMON)%>%summarize(TOT_KEPT_SP_WT=sum(EST_KEPT_WT,na.rm=T))%>%data.frame()
#calculate total jonah crab by year
obs.jonah<-LFA41.obs.0615[LFA41.obs.0615$SPECCD_ID %in% c(2511),]
obs.kept.jonah<-obs.jonah%>%group_by(YEAR)%>%summarize(TOT_KEPT_OBSJONAH_WT=sum(EST_KEPT_WT,na.rm=T))%>%data.frame()
#Calculate annual catch and discard
obs.kept.lob<-obs.lob%>%group_by(YEAR)%>%summarize(TOT_KEPT_OBSLOB_WT=sum(EST_KEPT_WT,na.rm=T))%>%data.frame()
obs.disc.sp<-LFA41.obs.0615%>%group_by(YEAR,SPECCD_ID)%>%summarize(TOT_DISCARD_OBS_WT=sum(EST_DISCARD_WT,na.rm=T))%>%data.frame()
#Calculate kept lobster and incidental catch for every 3 years
#SUM
obs.kept.lob3<-obs.lob%>%group_by(yr3.grp)%>%summarize(TOT_KEPT_OBSLOB_WT=sum(EST_KEPT_WT,na.rm=T))%>%data.frame()
obs.disc.sp3<-LFA41.obs.0615%>%group_by(yr3.grp,SPECCD_ID)%>%summarize(TOT_DISCARD_OBS_WT=sum(EST_DISCARD_WT,na.rm=T))%>%data.frame()
#AVERAGE
obs.kept.lob3<-obs.lob%>%group_by(YEAR,yr3.grp)%>%summarize(TOT_KEPT_OBSLOB_WT=sum(EST_KEPT_WT,na.rm=T))%>%data.frame()
obs.kept.lob3<-obs.kept.lob3%>%group_by(yr3.grp)%>%summarize(TOT_KEPT_OBSLOB_WT=mean(TOT_KEPT_OBSLOB_WT))%>%data.frame()
obs.disc.sp3<-LFA41.obs.0615%>%group_by(YEAR,yr3.grp,SPECCD_ID)%>%summarize(TOT_DISCARD_OBS_WT=sum(EST_DISCARD_WT,na.rm=T))%>%data.frame()
obs.disc.sp3<-obs.disc.sp3%>%group_by(yr3.grp,SPECCD_ID)%>%summarize(TOT_DISCARD_OBS_WT=mean(TOT_DISCARD_OBS_WT))%>%data.frame()

#Calculate kept lobster and incidental catch for every 5 years
#SUM
obs.kept.lob5<-obs.lob%>%group_by(yr5.grp)%>%summarize(TOT_KEPT_OBSLOB_WT=sum(EST_KEPT_WT,na.rm=T))%>%data.frame()
obs.disc.sp5<-LFA41.obs.0615%>%group_by(yr5.grp,SPECCD_ID)%>%summarize(TOT_DISCARD_OBS_WT=sum(EST_DISCARD_WT,na.rm=T))%>%data.frame()
#AVERAGE
obs.kept.lob5<-obs.lob%>%group_by(YEAR,yr5.grp)%>%summarize(TOT_KEPT_OBSLOB_WT=sum(EST_KEPT_WT,na.rm=T))%>%data.frame()
obs.kept.lob5<-obs.kept.lob5%>%group_by(yr5.grp)%>%summarize(TOT_KEPT_OBSLOB_WT=mean(TOT_KEPT_OBSLOB_WT))%>%data.frame()
obs.disc.sp5<-LFA41.obs.0615%>%group_by(YEAR,yr5.grp,SPECCD_ID)%>%summarize(TOT_DISCARD_OBS_WT=sum(EST_DISCARD_WT,na.rm=T))%>%data.frame()
obs.disc.sp5<-obs.disc.sp5%>%group_by(yr5.grp,SPECCD_ID)%>%summarize(TOT_DISCARD_OBS_WT=mean(TOT_DISCARD_OBS_WT))%>%data.frame()

#Add zeros for species not observed in a particular set:
obs.dis_allsp<-merge(obs.disc.sp[!duplicated(paste(obs.disc.sp$YEAR)),1],species[,1:2])
names(obs.dis_allsp)[names(obs.dis_allsp)=="x"]<-"YEAR"
obs.disc.all<-merge(obs.disc.sp,obs.dis_allsp,all=T)
obs.catch<-merge(obs.kept.lob,obs.disc.all,all=T)
obs.catch$TOT_DISCARD_OBS_WT[is.na(obs.catch$TOT_DISCARD_OBS_WT)]<-0
#Add zeros for species not observed in a particular set for every 3 years:
obs.dis_allsp3<-merge(obs.disc.sp3[!duplicated(paste(obs.disc.sp3$yr3.grp)),1],species[,1:2])
names(obs.dis_allsp3)[names(obs.dis_allsp3)=="x"]<-"yr3.grp"
obs.disc.all3<-merge(obs.disc.sp3,obs.dis_allsp3,all=T)
obs.catch.3<-merge(obs.kept.lob3,obs.disc.all3,all=T)
obs.catch.3$TOT_DISCARD_OBS_WT[is.na(obs.catch.3$TOT_DISCARD_OBS_WT)]<-0
#Add zeros for species not observed in a particular set for every 5 years:
obs.dis_allsp5<-merge(obs.disc.sp5[!duplicated(paste(obs.disc.sp5$yr5.grp)),1],species[,1:2])
names(obs.dis_allsp5)[names(obs.dis_allsp5)=="x"]<-"yr5.grp"
obs.disc.all5<-merge(obs.disc.sp5,obs.dis_allsp5,all=T)
obs.catch.5<-merge(obs.kept.lob5,obs.disc.all5,all=T)
obs.catch.5$TOT_DISCARD_OBS_WT[is.na(obs.catch.5$TOT_DISCARD_OBS_WT)]<-0

#Calculate discard rate
obs.catch$DISC_RATE<-obs.catch$TOT_DISCARD_OBS_WT/obs.catch$TOT_KEPT_OBSLOB_WT
obs.catch.3$DISC_RATE<-obs.catch.3$TOT_DISCARD_OBS_WT/obs.catch.3$TOT_KEPT_OBSLOB_WT
obs.catch.5$DISC_RATE<-obs.catch.5$TOT_DISCARD_OBS_WT/obs.catch.5$TOT_KEPT_OBSLOB_WT

##ATSEA Data
#Calculate total lobster by year:
atsea.lob<-LFA41.atsea.0615[LFA41.atsea.0615$SPECIESCODE %in% c(2550),]
atsea.all.lob<-atsea.lob%>%group_by(YEAR)%>%summarize(TOT_KG=(sum(CALC_WT,na.rm=T))/1000)%>%data.frame()
#Portion of Legals:
atsea.lob.leg<-subset(atsea.lob,CARLENGTH>81 & SEX %in% c(1,2))
atsea.leg.lob<-atsea.lob.leg%>%group_by(YEAR)%>%summarize(TOT_KG=(sum(CALC_WT,na.rm=T))/1000)%>%data.frame()
#Portion of sublegals:
atsea.lob.sub<-subset(atsea.lob,CARLENGTH<82 & SEX %in% c(1,2))
atsea.sub.lob<-atsea.lob.sub%>%group_by(YEAR)%>%summarize(TOT_KG=(sum(CALC_WT,na.rm=T))/1000)%>%data.frame()
#Remove data errors:
atsea.lob.err<-subset(atsea.lob,CARLENGTH>9 & CARLENGTH<265) #drops 10 lobsters
data.flag<-atsea.lob.err
str(data.flag)
#RETURNS
data.flag$FLAG1<-0
data.flag$FLAG1[which(data.flag$SEX==3| data.flag$CARLENGTH<=81 | data.flag$CARLENGTH>=140| data.flag$VNOTCH != 0 | 
                      !is.na(data.flag$CULL) | data.flag$SHELL %in% c(1,2,3,7))]<-1                  
sum(data.flag$CALC_WT*data.flag$FLAG1,na.rm=TRUE)
#FLAG BERRIED
data.flag$FLAG2<-0
data.flag$FLAG2[which(data.flag$SEX==3)]<-1                 
sum(data.flag$CALC_WT*data.flag$FLAG2,na.rm=TRUE)
#FLAG VNOTCH
data.flag$FLAG3<-0
data.flag$FLAG3[which(data.flag$VNOTCH != 0 & data.flag$SEX %in% c(2,3))]<-1
sum(data.flag$CALC_WT*data.flag$FLAG3, na.rm=TRUE)
#CULL
data.flag$FLAG4<-0
data.flag$FLAG4[which(!is.na(data.flag$CULL))]<-1
sum(data.flag$CALC_WT*data.flag$FLAG4, na.rm=TRUE)
#SOFT SHELL
data.flag$FLAG5<-0
data.flag$FLAG5[which(data.flag$SHELL %in% c(1,2,3,7))]<-1
sum(data.flag$CALC_WT*data.flag$FLAG5, na.rm=TRUE)
#FLAG UNDERSIZE
data.flag$FLAG6<-0
data.flag$FLAG6[which(data.flag$CARLENGTH<=82)]<-1
sum(data.flag$CALC_WT*data.flag$FLAG6, na.rm=TRUE)
#FLAG JUMBO
data.flag$FLAG7<-0
data.flag$FLAG7[which(data.flag$CARLENGTH>=140)]<-1
sum(data.flag$CALC_WT*data.flag$FLAG7, na.rm=TRUE)

#PERCENT
with(data.flag,tapply(CARLENGTH,YEAR,length))#CATCH
with(subset(data.flag,FLAG1==1),tapply(FLAG1,YEAR,length))#RETURNS
with(subset(data.flag,FLAG2==1),tapply(FLAG2,YEAR,length))#BERRIED
with(subset(data.flag,FLAG3==1),tapply(FLAG3,YEAR,length))#VNOTCH
with(subset(data.flag,FLAG4==1),tapply(FLAG4,YEAR,length))#CULL
with(subset(data.flag,FLAG5==1),tapply(FLAG5,YEAR,length))#SOFT SHELL
with(subset(data.flag,FLAG6==1),tapply(FLAG6,YEAR,length))#UNDERSIZE
with(subset(data.flag,FLAG7==1),tapply(FLAG7,YEAR,length))#JUMBO

with(subset(data.flag,FLAG1==1),tapply(FLAG1,YEAR,length))/with(data.flag,tapply(CARLENGTH,YEAR,length))
with(subset(data.flag,FLAG2==1),tapply(FLAG2,YEAR,length))/with(subset(data.flag,FLAG1==1),tapply(FLAG1,YEAR,length))
with(subset(data.flag,FLAG3==1),tapply(FLAG3,YEAR,length))/with(subset(data.flag,FLAG1==1),tapply(FLAG1,YEAR,length))
with(subset(data.flag,FLAG4==1),tapply(FLAG4,YEAR,length))/with(subset(data.flag,FLAG1==1),tapply(FLAG1,YEAR,length))
with(subset(data.flag,FLAG5==1),tapply(FLAG5,YEAR,length))/with(subset(data.flag,FLAG1==1),tapply(FLAG1,YEAR,length))
with(subset(data.flag,FLAG6==1),tapply(FLAG6,YEAR,length))/with(subset(data.flag,FLAG1==1),tapply(FLAG1,YEAR,length))
with(subset(data.flag,FLAG7==1),tapply(FLAG7,YEAR,length))/with(subset(data.flag,FLAG1==1),tapply(FLAG1,YEAR,length))

##Commercial log data
#Lobster Landings by year
lob.landings<-LFA41.logs.0615%>%group_by(YEAR)%>%summarize(LAND_KG=round(sum(LANDINGS_KG,na.rm=T),2))%>%data.frame()
landings<-merge(lob.landings,species[,1:2]) #ALL SPECIES INCLUDED
#Lobster Landings by 3-year intervals
lob.landings3<-LFA41.logs.0615%>%group_by(yr3.grp)%>%summarize(LAND_KG=round(sum(LANDINGS_KG,na.rm=T),2))%>%data.frame()
landings.y3<-merge(lob.landings3,species[,1:2]) #ALL SPECIES INCLUDED
#Lobster Landings by 5-year intervals
lob.landings5<-LFA41.logs.0615%>%group_by(yr5.grp)%>%summarize(LAND_KG=round(sum(LANDINGS_KG,na.rm=T),2))%>%data.frame()
landings.y5<-merge(lob.landings5,species[,1:2]) #ALL SPECIES INCLUDED

#Combine observer catch with commercial data
obsv.log.merge<-merge(obs.catch,landings,all=T) 
notSampled<-subset(obsv.log.merge,is.na(DISC_RATE))
Sampled<-subset(obsv.log.merge,!is.na(DISC_RATE))                                                                                            

obsv.log.merge.y3<-merge(obs.catch.3,landings.y3,all=T) 
notSampled.y3<-subset(obsv.log.merge.y3,is.na(DISC_RATE))
Sampled.y3<-subset(obsv.log.merge.y3,!is.na(DISC_RATE))                                                                                            

obsv.log.merge.y5<-merge(obs.catch.5,landings.y5,all=T) 
notSampled.y5<-subset(obsv.log.merge.y5,is.na(DISC_RATE))
Sampled.y5<-subset(obsv.log.merge.y5,!is.na(DISC_RATE))                                                                                            

#Combine sampled and non sampled 
finaldata<-obsv.log.merge
finaldata.y3<-obsv.log.merge.y3
finaldata.y5<-obsv.log.merge.y5

#Calculate total discards
finaldata$TotalDiscards<-finaldata$LAND_KG*finaldata$DISC_RATE
finaldata.y3$TotalDiscards<-finaldata.y3$LAND_KG*finaldata.y3$DISC_RATE
finaldata.y5$TotalDiscards<-finaldata.y5$LAND_KG*finaldata.y5$DISC_RATE

write.xlsx(finaldata, file=paste("Finaldata.new",Sys.Date(),".xlsx"))

#INCIDENTAL CATCH TABLES
yrs<-sort(unique(finaldata$YEAR))
table.catch<-sapply(yrs,function(y){with(subset(finaldata,YEAR==y),tapply(TOT_DISCARD_OBS_WT,COMMON,sum,na.rm=T))})
table<-data.frame(round(table.catch[order(rowSums(table.catch),decreasing=T),]))[,]
names(table)<-yrs
table

table.obs.catch<-sapply(yrs,function(y){with(subset(finaldata,YEAR==y),tapply(TotalDiscards,COMMON,sum,na.rm=T))})
table.obs<-data.frame(round(table.obs.catch[order(rowSums(table.obs.catch),decreasing=T),]))[,]
names(table.obs)<-yrs
table.obs

#Without lobster
no.lob.inc<-subset(finaldata,COMMON!="AMERICAN LOBSTER")
tot.inc.catch.0615<-no.lob.inc%>%group_by(YEAR)%>%summarise(INC_CATCH_KG=sum(TOT_DISCARD_OBS_WT,na.rm=TRUE),DISCARD_RATIO_WT=sum(TotalDiscards,na.rm=TRUE))%>%ungroup()%>%data.frame()

no.lob.inc.y3<-subset(finaldata.y3,COMMON!="AMERICAN LOBSTER")
tot.inc.catch.y3<-no.lob.inc.y3%>%group_by(yr3.grp)%>%summarise(INC_CATCH_KG=sum(TOT_DISCARD_OBS_WT,na.rm=TRUE),DISCARD_RATIO_WT=sum(TotalDiscards,na.rm=TRUE))%>%ungroup()%>%data.frame()

no.lob.inc.y5<-subset(finaldata.y5,COMMON!="AMERICAN LOBSTER")
tot.inc.catch.y5<-no.lob.inc.y5%>%group_by(yr5.grp)%>%summarise(INC_CATCH_KG=sum(TOT_DISCARD_OBS_WT,na.rm=TRUE),DISCARD_RATIO_WT=sum(TotalDiscards,na.rm=TRUE))%>%ungroup()%>%data.frame()

#without lobster and crab
no.lob.jon.inc<-subset(no.lob.inc,COMMON!="JONAH CRAB")
no.lob.jon.inc.y3<-subset(no.lob.inc.y3,COMMON!="JONAH CRAB")
no.lob.jon.inc.y5<-subset(no.lob.inc.y5,COMMON!="JONAH CRAB")

tot.nolj.catch.0615<-no.lob.jon.inc%>%group_by(YEAR,COMMON)%>%summarise(INC_CATCH_KG=sum(TOT_DISCARD_OBS_WT,na.rm=TRUE),DISCARD_RATIO_WT=sum(TotalDiscards,na.rm=TRUE))%>%ungroup()%>%data.frame()
tot.nolj.catch.y3<-no.lob.jon.inc.y3%>%group_by(yr3.grp,COMMON)%>%summarise(INC_CATCH_KG=sum(TOT_DISCARD_OBS_WT,na.rm=TRUE),DISCARD_RATIO_WT=sum(TotalDiscards,na.rm=TRUE))%>%ungroup()%>%data.frame()
tot.nolj.catch.y5<-no.lob.jon.inc.y5%>%group_by(yr5.grp,COMMON)%>%summarise(INC_CATCH_KG=sum(TOT_DISCARD_OBS_WT,na.rm=TRUE),DISCARD_RATIO_WT=sum(TotalDiscards,na.rm=TRUE))%>%ungroup()%>%data.frame()

write.xlsx(tot.nolj.catch.0615, file=paste("Incidental catch",Sys.Date(),".xlsx"))

#LFA 41 Incidental catch 
inc.catch.0615<-read.csv(file.path(project.datadirectory("bio.lobster"),"data","LFA41.Inc.catch.edit.csv"))

table.nolj<-subset(tot.nolj.catch.0615, COMMON %in% c('CUSK','COD(ATLANTIC)','WHITE HAKE','SQUIRREL OR RED HAKE',
                                                  'HAKE (NS)','HADDOCK','MONKFISH,GOOSEFISH,ANGLER',
                                                  'REDFISH UNSEPERATED','SEA RAVEN',
                                                  'ROSEFISH(BLACK BELLY)',
                                                  'STRIPED ATLANTIC WOLFFISH','SEAROBINS','POLLOCK',
                                                  'LONGHORN SCULPIN','SCULPINS (NS)'))

table.nolj.y3<-subset(tot.nolj.catch.y3, COMMON %in% c('CUSK','COD(ATLANTIC)','WHITE HAKE','SQUIRREL OR RED HAKE',
                                                      'HAKE (NS)','HADDOCK','MONKFISH,GOOSEFISH,ANGLER',
                                                      'REDFISH UNSEPERATED','SEA RAVEN',
                                                      'ROSEFISH(BLACK BELLY)',
                                                      'STRIPED ATLANTIC WOLFFISH','SEAROBINS','POLLOCK',
                                                      'LONGHORN SCULPIN','SCULPINS (NS)'))

table.nolj.y5<-subset(tot.nolj.catch.y5, COMMON %in% c('CUSK','COD(ATLANTIC)','WHITE HAKE','SQUIRREL OR RED HAKE',
                                                      'HAKE (NS)','HADDOCK','MONKFISH,GOOSEFISH,ANGLER',
                                                      'REDFISH UNSEPERATED','SEA RAVEN',
                                                      'ROSEFISH(BLACK BELLY)',
                                                      'STRIPED ATLANTIC WOLFFISH','SEAROBINS','POLLOCK',
                                                      'LONGHORN SCULPIN','SCULPINS (NS)'))

Palette <- c("gray87", "gray77", "gray67", "gray47","gray27","lightskyblue1", "skyblue1", "steelblue2", "royalblue1","navy")

ggplot(table.nolj,aes(COMMON,DISCARD_RATIO_WT,fill=factor(YEAR))) + geom_bar(stat='identity', position=position_dodge(),width=.8) +
  xlab('') + scale_y_continuous(limits=c(0,50000),breaks=seq(0, 50000, 5000)) + 
  ggtitle("LFA41 - Incidental Catch (Kg)") + ylab("Catch (Kg)") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5,size=08, colour='black'),panel.background=element_rect(fill="white", colour='black'),
        panel.border=element_rect(,fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),
        axis.title.y= element_text(size=10), title=element_text(size=12),legend.justification=c(1,1.025), legend.position=c(1,1),
        legend.key.size = unit(0.5,"cm")) +  
  scale_fill_manual(name="Year",values=Palette)

Palette <- c("gray27", "skyblue1", "navy")

ggplot(table.nolj.y3,aes(COMMON,DISCARD_RATIO_WT,fill=factor(yr3.grp))) + geom_bar(stat='identity', position=position_dodge(),width=.8) +
  xlab('') + scale_y_continuous(limits=c(0,90000),breaks=seq(0, 90000, 10000),labels = scales::comma) + 
  ggtitle("LFA41 - Incidental Catch (Kg) in 3-year Groupings") + ylab("Catch (Kg)") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5,size=08, colour='black'),panel.background=element_rect(fill="white", colour='black'),
        panel.border=element_rect(,fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),
        axis.title.y= element_text(size=10), title=element_text(size=12),legend.justification=c(1,1), legend.position=c(1,1),
        legend.key.size = unit(0.5,"cm")) +  
  scale_fill_manual(name="Year Group",values=Palette)

Palette <- c("gray27", "royalblue1")

ggplot(table.nolj.y5,aes(COMMON,DISCARD_RATIO_WT,fill=factor(yr5.grp))) + geom_bar(stat='identity', position=position_dodge(),width=.8) +
  xlab('') + scale_y_continuous(limits=c(0,125000),breaks=seq(0, 125000, 25000),labels = scales::comma) + 
  ggtitle("LFA41 - Incidental Catch (Kg) in 5-year Groupings") + ylab("Catch (Kg)") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5,size=08, colour='black'),panel.background=element_rect(fill="white", colour='black'),
        panel.border=element_rect(,fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),
        axis.title.y= element_text(size=10), title=element_text(size=12),legend.justification=c(1,1), legend.position=c(1,1),
        legend.key.size = unit(0.5,"cm")) +  
  scale_fill_manual(name="Year Group",values=Palette)

#Incidental catch rate per Kg of lobster landed:
#Run ImportData.R to acquire ann.41

land.41<-subset(ann.41,YEAR>2005 & YEAR<2016)
#Calculate total lobster by 3-year groupings:
land.41$yr3.grp<-NA
G1=which(land.41$YEAR %in% c(2006:2008))
land.41[G1,"yr3.grp"]="2006-08"
G2=which(land.41$YEAR %in% c(2009:2011))
land.41[G2,"yr3.grp"]="2009-11"
G3=which(land.41$YEAR %in% c(2012:2015))
land.41[G3,"yr3.grp"]="2012-15"
#5 YEAR IDENTIFIER:
land.41$yr5.grp<-NA
H1=which(land.41$YEAR %in% c(2006:2010))
land.41[H1,"yr5.grp"]="2006-10"
H2=which(land.41$YEAR %in% c(2011:2015))
land.41[H2,"yr5.grp"]="2011-15"


land.41<-land.41[,-c(3,4)]
obs.land.data<-merge(finaldata,land.41,by="YEAR")
obs.land.data$catch.rate<-obs.land.data$TotalDiscards/obs.land.data$LAND_T

catch.rate<-subset(obs.land.data,COMMON!="JONAH CRAB" & COMMON!="AMERICAN LOBSTER")
tot.catch.rate.0615<-catch.rate%>%group_by(YEAR,COMMON)%>%summarise(CR=sum(catch.rate,na.rm=TRUE))%>%ungroup()%>%data.frame()
tot.catch.rate.3<-catch.rate%>%group_by(yr3.grp,COMMON)%>%summarise(CR=sum(catch.rate,na.rm=TRUE))%>%ungroup()%>%data.frame()
tot.catch.rate.5<-catch.rate%>%group_by(yr5.grp,COMMON)%>%summarise(CR=sum(catch.rate,na.rm=TRUE))%>%ungroup()%>%data.frame()

table.cr<-subset(tot.catch.rate.0615, COMMON %in% c('CUSK','COD(ATLANTIC)','WHITE HAKE','SQUIRREL OR RED HAKE',
                                                    'HAKE (NS)','HADDOCK','MONKFISH,GOOSEFISH,ANGLER',
                                                    'REDFISH UNSEPERATED','SEA RAVEN',
                                                    'ROSEFISH(BLACK BELLY)',
                                                    'STRIPED ATLANTIC WOLFFISH','SEAROBINS','POLLOCK',
                                                    'LONGHORN SCULPIN','SCULPINS (NS)'))

table.cr.3<-subset(tot.catch.rate.3, COMMON %in% c('CUSK','COD(ATLANTIC)','WHITE HAKE','SQUIRREL OR RED HAKE',
                                                    'HAKE (NS)','HADDOCK','MONKFISH,GOOSEFISH,ANGLER',
                                                    'REDFISH UNSEPERATED','SEA RAVEN',
                                                    'ROSEFISH(BLACK BELLY)',
                                                    'STRIPED ATLANTIC WOLFFISH','SEAROBINS','POLLOCK',
                                                    'LONGHORN SCULPIN','SCULPINS (NS)'))


table.cr.5<-subset(tot.catch.rate.5, COMMON %in% c('CUSK','COD(ATLANTIC)','WHITE HAKE','SQUIRREL OR RED HAKE',
                                                    'HAKE (NS)','HADDOCK','MONKFISH,GOOSEFISH,ANGLER',
                                                    'REDFISH UNSEPERATED','SEA RAVEN',
                                                    'ROSEFISH(BLACK BELLY)',
                                                    'STRIPED ATLANTIC WOLFFISH','SEAROBINS','POLLOCK',
                                                    'LONGHORN SCULPIN','SCULPINS (NS)'))


Palette <- c("gray87", "gray77", "gray67", "gray47","gray27","lightskyblue1", "skyblue1", "steelblue2", "royalblue1","navy")
ggplot(table.cr,aes(COMMON,CR,fill=factor(YEAR))) + geom_bar(stat='identity', position=position_dodge(),width=.8) +
  xlab('') + scale_y_continuous(limits=c(0,60),breaks=seq(0, 60, 5)) + 
  ggtitle("LFA41 - Incidental Catch Rate (Kg/Metric tonne lobster landed)") + ylab("Catch Rate(Kg/metric tonne lobster landed)") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5,size=08, colour='black'),panel.background=element_rect(fill="white", colour='black'),
        panel.border=element_rect(,fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),
        axis.title.y= element_text(size=10), title=element_text(size=12),legend.justification=c(1,1.01), legend.position=c(1,1),
        legend.key.size = unit(0.5,"cm")) +  
  scale_fill_manual(name="Year",values=Palette)

Palette <- c("gray27", "skyblue1", "navy")
ggplot(table.cr.3,aes(COMMON,CR,fill=factor(yr3.grp))) + geom_bar(stat='identity', position=position_dodge(),width=.8) +
  xlab('') + scale_y_continuous(limits=c(0,150),breaks=seq(0, 150, 25)) + 
  ggtitle("LFA41 - Incidental Catch Rate in 3-year Groupings") + ylab("Catch Rate(Kg/Metric tonne lobster landed)") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5,size=08, colour='black'),panel.background=element_rect(fill="white", colour='black'),
        panel.border=element_rect(,fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),
        axis.title.y= element_text(size=10), title=element_text(size=12),legend.justification=c(1,1), legend.position=c(1,1),
        legend.key.size = unit(0.5,"cm")) +  
  scale_fill_manual(name="Year Group",values=Palette)

Palette <- c("gray27", "royalblue1")
ggplot(table.cr.5,aes(COMMON,CR,fill=factor(yr5.grp))) + geom_bar(stat='identity', position=position_dodge(),width=.8) +
  xlab('') + scale_y_continuous(limits=c(0,200),breaks=seq(0, 200, 25)) + 
  ggtitle("LFA41 - Incidental Catch Rate in 5-year Groupings") + ylab("Catch Rate(Kg/Metric tonne lobster landed)") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5,size=08, colour='black'),panel.background=element_rect(fill="white", colour='black'),
        panel.border=element_rect(,fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),
        axis.title.y= element_text(size=10), title=element_text(size=12),legend.justification=c(1,1), legend.position=c(1,1),
        legend.key.size = unit(0.5,"cm")) +  
  scale_fill_manual(name="Year Group",values=Palette)

#Proportion of total lobster catch
prop.disc.lob<-obs.land.data[,-c(2,3,5,6)]

#LOBSTER KEPT:
lob.kept<-subset(finaldata,COMMON=="AMERICAN LOBSTER" | COMMON=="JONAH CRAB")
lob.kept2<-lob.kept[,-c(2,4:6)]
write.xlsx(lob.kept2, file=paste("Total lobster and Jonah catch",Sys.Date(),".xlsx"))
inc.lobs.0615<-read.csv(file.path(project.datadirectory("bio.lobster"),"Total.lobster.and.Jonah.catch.2016-11-10 .csv"))
inc.lobs.0615.1<-melt(inc.lobs.0615,id="YEAR")
Palette <- c("gray87", "gray77", "gray67", "gray47","gray27","lightskyblue1", "skyblue1", "steelblue2", "royalblue1","navy")

ggplot(inc.lobs.0615.1,aes(variable,value,fill=factor(YEAR))) + geom_bar(stat='identity', position=position_dodge(),width=.8) +
  xlab('') + scale_y_continuous(limits=c(0,900000),breaks=seq(0, 900000, 100000),labels = scales::comma) + 
  ggtitle("LFA41 - Lobster and Jonah Crab Catch (Kg)") + ylab("") +
  theme(axis.text.x=element_text(angle=0, hjust=0.5, vjust=0.5,size=10, colour='black'),panel.background=element_rect(fill="white", colour='black'),
        panel.border=element_rect(,fill=NA,colour='black'),axis.text.y=element_text(size=8, colour='black'),
        axis.title.y= element_text(size=10), title=element_text(size=12),legend.justification=c(1,1), legend.position=c(1,1),
        legend.key.size = unit(0.5,"cm")) + 
  scale_fill_manual(name="Year",values=Palette)

#Lobster and Jonah Catch Rate
#Run ImportData.R to acquire ann.41

land.41<-subset(ann.41,YEAR>2005 & YEAR<2016)
land.41<-land.41[,-c(3,4)]
obs.land.data<-merge(inc.lobs.0615,land.41,by="YEAR")
obs.land.data$Lob.Disc.CR<-obs.land.data$Lobster.Discard/obs.land.data$LAND_T
obs.land.data$Jon.Disc.CR<-obs.land.data$Jonah.Crab.Discard/obs.land.data$LAND_T
cr.41<-obs.land.data[,-c(2,3,5,6)]
names(cr.41)[names(cr.41)=="Lob.Kept.CR"] <- "Lobster.Discard"
names(cr.41)[names(cr.41)=="Jon.Disc.CR"] <- "Jonah.Crab.Discard"
write.xlsx(cr.41, file=paste("Incidental catch lobster and Jonah",Sys.Date(),".xlsx"))
cr.0615<-read.csv(file.path(project.datadirectory("bio.lobster"),"data","Incidental catch lobster and Jonah 2016-11-14 .csv"))

inc.lobs.0615.1<-melt(cr.0615,id="YEAR")

Palette <- c("gray87", "gray77", "gray67", "gray47","gray27","lightskyblue1", "skyblue1", "steelblue2", "royalblue1","navy")

ggplot(inc.lobs.0615.1,aes(variable,value,fill=factor(YEAR))) + geom_bar(stat='identity', position=position_dodge(),width=.8) +
  xlab('') + scale_y_continuous(limits=c(0,450),breaks=seq(0, 450, 50)) + 
  ggtitle("LFA41 - Lobster and Jonah Crab Catch Rate (Kg/Metric tonne lobser landed)") + ylab("Catch Rate(Kg/metric tonne lobster landed)") +
  theme(axis.text.x=element_text(angle=0, hjust=0.5, vjust=0.5,size=10, colour='black'),panel.background=element_rect(fill="white", colour='black'),
        panel.border=element_rect(,fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),
        axis.title.y= element_text(size=10), title=element_text(size=12),legend.justification=c(1,1), legend.position=c(1,1),
        legend.key.size = unit(0.5,"cm")) +  
  scale_fill_manual(name="Year",values=Palette)


  
####################################################CATCH COMPOSITION OF DISCARDS################################################
#Compare non-retained lobster with LFA 41 atsea sampling:
lobster.db("atSea.redo")
lobster.db("atSea")
atSea$YEAR<-year(atSea$STARTDATE) #Data from 1972-to Present
atsea2012<-subset(atSea,YEAR==2012)
sub.2012<-subset(atsea2012,CARLENGTH<83)
jum.2012<-subset(atsea2012,CARLENGTH>=150 & SPECIESCODE==2550 & SEX>1)
table(atsea2012$SEX)
table(atsea2012$VNOTCH)
table(atsea2012$EGG)
table(atsea2012$CARLENGTH<83)



table.obsv.sp.yr<-subset(finaldata, COMMON %in% c('CUSK','COD(ATLANTIC)','WHITE HAKE','SQUIRREL OR RED HAKE', 'SILVER HAKE',
                                                     'HAKE (NS)','HADDOCK','MONKFISH,GOOSEFISH,ANGLER',
                                                     'REDFISH UNSEPERATED','SEA RAVEN','ATLANTIC ROCK CRAB', 'SNOW CRAB (QUEEN)',
                                                     'ROSEFISH(BLACK BELLY)','RED DEEPSEA CRAB',
                                                     'STRIPED ATLANTIC WOLFFISH','SEAROBINS','POLLOCK',
                                                     'LONGHORN SCULPIN','SMOOTH SKATE','WINTER SKATE',
                                                     'SPINY CRAB','AMERICAN PLAICE','SCULPINS (NS)') & YEAR>2005)

Palette <- c("gray77", "orange", "green", "blue","gray27","lawngreen", "pink", "darkseagreen3", "yellow","gray57")

ggplot(table.obsv.sp.yr,aes(factor(YEAR),TotalDiscards/1000,,fill=factor(COMMON))) + geom_bar(stat='identity', position=position_dodge(),width=.8) +
  facet_wrap(~COMMON) + xlab('') + scale_y_continuous(limits=c(0,50),breaks=seq(0, 50, 10)) + 
  ggtitle("LFA41 - Incidental Catch (t)") + ylab("Catch (t)") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5,size=08, colour='black'),panel.background=element_rect(fill="white", colour='black'),
        panel.border=element_rect(,fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),
        axis.title.y= element_text(size=10), title=element_text(size=12),legend.justification=c(1,1), legend.position=c(1,1),
        legend.key.size = unit(0.5,"cm")) +  
  scale_fill_manual(name="Year",values=Palette)

ggplot(table.obsv.sp.yr,aes(factor(YEAR),log(TotalDiscards),,fill=factor(COMMON))) + geom_bar(stat='identity', position=position_dodge(),width=.8) +
  facet_wrap(~COMMON) + xlab('') + scale_y_continuous(limits=c(0,50),breaks=seq(0, 50, 10)) + 
  ggtitle("LFA41 - Incidental Catch (t)") + ylab("Catch (t)") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5,size=08, colour='black'),panel.background=element_rect(fill="white", colour='black'),
        panel.border=element_rect(,fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),
        axis.title.y= element_text(size=10),legend.position="none")


atsea0615<-subset(atSea,YEAR>=2005)

atsea2015<-subset(atSea,YEAR==2015)

sub.2015<-subset(atsea2015,CARLENGTH<83)
table(atsea2015$SEX)
table(atsea2015$VNOTCH)
table(atsea2015$EGG)
table(atsea2015$CARLENGTH<83)

logs41$LANDINGS_T<-logs41$ADJCATCH*0.0004536
logs41$SYEAR<-sapply(logs41$DATE_FISHED,offFishingYear)
str(atSea) #SEPT_2016 37,955 RECORDS
summary(atSea)

logs for 2015
head(logs41)

##################################################### GRAPHICS ##########################################################
#BYCATCH TABLES
yrs<-sort(unique(finaldata$YEAR))

table4X<-sapply(yrs,function(y){with(subset(finaldata,NAFOAREA=="4X"& YEAR==y),tapply(TotalDiscards,COMMON,sum,na.rm=T))})
table4X<-data.frame(round(table4X[order(rowSums(table4X),decreasing=T),]))
names(table4X)<-yrs
table4X

table5Z.new<-sapply(yrs.new,function(y){with(subset(finaldata.new,NAFOAREA=="5Z"&SYEAR==y),tapply(TotalDiscards,COMMON,sum,na.rm=T))})
table5Z.new<-data.frame(round(table5Z.new[order(rowSums(table5Z.new),decreasing=T),]))
names(table5Z.new)<-yrs.new
table5Z.new

table<-sapply(yrs,function(y){with(subset(finaldata,YEAR==y),tapply(TotalDiscards,COMMON,sum,na.rm=T))})
table<-data.frame(round(table[order(rowSums(table),decreasing=T),]))[-1,]
names(table)<-yrs
table
write.xlsx(table, file=paste("est.incid.catch",Sys.Date(),".xlsx"))

#Table of annual observed incidental catch composition for LFA 41, lobster included
table.inc<-sapply(yrs,function(y){with(subset(finaldata,YEAR==y),tapply(TOT_DISCARD_OBS_WT,COMMON,sum,na.rm=T))})
table.inc<-data.frame(round(table.inc[order(rowSums(table.inc),decreasing=T),]))
names(table.inc)<-yrs
table.inc
write.xlsx(table.inc, file=paste("obs.incid.catch",Sys.Date(),".xlsx"))

#Table of total annual estimated incidental catch composition for LFA 41, lobster included
table<-sapply(yrs,function(y){with(subset(finaldata,YEAR==y),tapply(TotalDiscards,COMMON,sum,na.rm=T))})
table<-data.frame(round(table[order(rowSums(table),decreasing=T),]))
names(table)<-yrs
table
write.xlsx(table, file=paste("est.incid.catch",Sys.Date(),".xlsx"))

#BYCATCH PLOTS:
str(finaldata)
#INCIDENTAL CATCH:
est.incid.catch<-finaldata%>%group_by(YEAR,COMMON)%>%summarize(TOT_EST_CATCH=round(sum(TotalDiscards,na.rm=T),0))%>%data.frame()

ggplot(est.incid.catch,aes(COMMON,TOT_EST_CATCH), fill=YEAR,group=YEAR)  + geom_bar(stat="identity", alpha=0.5) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5,size=7, colour='black'),
        panel.background=element_rect(fill="white", colour='black'),panel.border=element_rect(,fill=NA,colour='black'),
        axis.text.y=element_text(size=10, colour='black'))

inc.catch<-subset(finaldata,!is.na(COMMON))
inc.catch<-subset(inc.catch,!is.na(TotalDiscards))

Palette <- c("gray77", "orange", "green", "indigo","gray27","lawngreen", "pink", "darkseagreen3", "yellow", "darkolivegreen4","light blue","red", "bluegrey", "deeppurple")

log.obs.lob<-finaldata%>%group_by(YEAR)%>%summarize(LOB_LOG_ADJ_CATCH.KG=round(mean(LOB_LOG_ADJ_CATCH.KG,na.rm=T),2),
                                                    TOT_KEPT_OBSLOB_WT=round(mean(TOT_KEPT_OBSLOB_WT, na.rm=T),2))%>%data.frame()

lob.catchsum<-merge(log.obs.lob,ann.trip)
lob.catchsum<-melt(lob.catchsum, id=c("YEAR"))

ggplot(lob.catchsum,aes(factor(YEAR),value, group=variable)) + geom_point(data=subset(lob.catchsum,variable="LOB_LOG_ADJ_CATCH.KG")) + geom_line() + 
  geom_bar(data=subset(lob.catchsum, variable=TOT_SAMP), stat="identity", alpha=0.5) + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5,size=7, colour='black'),
        panel.background=element_rect(fill="white", colour='black'),panel.border=element_rect(,fill=NA,colour='black'),
        axis.text.y=element_text(size=10, colour='black'))

ggplot(log.obs.lob,aes(factor(YEAR),TOT_KEPT_OBSLOB_WT, group=YEAR)) + geom_point() + geom_line() + 
  geom_bar(data=ann.trip, aes(y=TOT_SAMP)) + facet_wrap(~OFFAREA) + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5,size=7, colour='black'),
        panel.background=element_rect(fill="white", colour='black'),panel.border=element_rect(,fill=NA,colour='black'),
        axis.text.y=element_text(size=10, colour='black'))



xlab('') + scale_y_continuous(limits=c(0,1000000),breaks=seq(0, 10000000, 100000)) + 
  ggtitle("LFA41 - Incidental Catch") + ylab("Incidental Catch (kg)") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5,size=8, colour='black'),panel.background=element_rect(fill="white", colour='black'),
        panel.border=element_rect(,fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),
        axis.title.y= element_text(size=10), title=element_text(size=12),legend.justification=c(1,1), legend.position=c(1,1),
        legend.key.size = unit(0.5,"cm")) +  scale_fill_manual(scale_fill_manual(name="Year",values=Palette))

#INCIDENTAL CATCH KG/MT LOBSTER LANDED:
ggplot(bycatch.MT,aes(SPECIES,KG.MT,fill=factor(YEAR))) + geom_bar(stat='identity', position=position_dodge(),width=.8) +  facet_wrap(~AREA) +
  xlab('') + scale_y_continuous(limits=c(0,80),breaks=seq(0, 80, 10)) + 
  ggtitle("LFA41 - 4X Incidental Catch") + ylab("Incidental Catch (kg/MT lobster landed)") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5,size=08, colour='black'),panel.background=element_rect(fill="white", colour='black'),
        panel.border=element_rect(,fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),
        axis.title.y= element_text(size=10), title=element_text(size=12),legend.justification=c(1,1), legend.position=c(1,1),
        legend.key.size = unit(0.5,"cm")) +  
  scale_fill_manual(name="Year",values=Palette)






obsv.log.merge$Time<-obsv.log.merge$YEAR+(obsv.log.merge$QUARTER-1)*0.25
par(mfrow=c(2,2))
for(i in 1:4){
  plot(DISC_RATE~Time,subset(obsv.log.merge,COMMON=="JONAH CRAB"&OFFAREA==areas[i]))
}

obsv.log.merge$logEST_KEPT_WT<-log(obsv.log.merge$EST_KEPT_WT)

jonah<-subset(obsv.log.merge,SPECCD_ID==species$SPECCD_ID[2])


fit<-gam(EST_DISCARD_WT ~ offset(logEST_KEPT_WT) + Time  + OFFAREA, data=jonah, family=poisson(link="log"))
exp(predict.glm(fit))->jonah$predicted










##Jonah crab commercial log data
ann.landings.jc<-LFA41.logsjonah.0215%>%group_by(YEAR,NAFOAREA,OFFAREA,QUARTER)%>%summarize(TOT_KEPT_WT.LBS=round(sum(EST_WEIGHT_LOG_LBS,na.rm=T),2),
                                                                                            TOT_ADJ_CATCH.KG=round(sum(WEIGHT_KG,na.rm=T),2))%>%data.frame()
##Calculate total effort

ann.obs.effort<-LFA41.obs.0215%>%group_by(YEAR,NAFOAREA,OFFAREA,QUARTER,COMMON)%>%summarize(TOT_KEPT_WT=round(sum(EST_KEPT_WT,na.rm=T),2),
                                                                                            TOT_DISCARD_WT=round(sum(EST_DISCARD_WT,na.rm=T),2),TOT_CATCH_WT=round(sum(EST_TOT_WT),2))%>%data.frame()



##Add in zeros for species not observed in a particular set
LFA41.obs.kept<-aggregate(EST_KEPT_WT ~ YEAR + QUARTER + OFFAREA, data = LFA41.obs.0215, sum, na.rm=T)
LFA41.obs.discard<-aggregate(EST_DISCARD_WT ~ YEAR + QUARTER  + OFFAREA + SPECCD_ID, data = LFA41.obs.0215, sum, na.rm=T)

LFA41.obs.discard_allsp<-merge(LFA41.obs.discard[!duplicated(paste(LFA41.obs.discard$YEAR, LFA41.obs.discard$QUARTER, LFA41.obs.discard$OFFAREA)),1:3],species[,1:2])
discard<-merge(LFA41.obs.discard,LFA41.obs.discard_allsp,all=T)
LFA41.obs.discard.table<-merge(kept,discard,all=T)
LFA41.obs.discard.table$EST_DISCARD_WT[is.na(LFA41.obs.discard.table$EST_DISCARD_WT)]<-0

##Calculate observed data discard rate
LFA41.obs.discard.table$discardRate<-round(LFA41.obs.discard.table$EST_DISCARD_WT/LFA41.obs.discard.table$EST_KEPT_WT,4)
LFA41.obs.discard.table


species<-aggregate(EST_DISCARD_WT ~ COMMON + SPECCD_ID, data = LFA41.obs.0215, sum, na.rm=T)
species<-species[order(species$EST_DISCARD_WT,decreasing=T),]


ann.comlog.bysp<-LFA41.logs.0215%>%group_by(YEAR,NAFOAREA,OFFAREA,QUARTER)%>%summarize(TOT_KEPT_WT.LBS=round(sum(EST_WEIGHT_LOG_LBS,na.rm=T),2),
                                                                                       TOT_ADJ_CATCH.KG=round(sum(WEIGHT_KG,na.rm=T),2))%>%data.frame()


total.land.bysp<-merge(ann.landings.lob,species[1:3])

## calculate total discarded and kept weights from observed sets 
kept<-aggregate(EST_KEPT_WT ~ YEAR + QUARTER + OFFAREA, data = bycatch41, sum, na.rm=T)
Kept.obs.0216<-LFA41.obs.0215%>%group_by(YEAR,OFFAREA,QUARTER,COMMON)%>%summarize(TOT_KEPT_WT=round(sum(EST_KEPT_WT,na.rm=T),2),
                                                                                  TOT_DISCARD_WT=round(sum(EST_DISCARD_WT,na.rm=T),2),TOT_CATCH_WT=round(sum(EST_TOT_WT),2))%>%data.frame()
discard<-aggregate(EST_DISCARD_WT ~ YEAR + QUARTER  + OFFAREA + SPECCD_ID, data = bycatch41, sum, na.rm=T)






#DATA MANIPULATION IN EXCEL TO CALCULATE DISCARD RATES FOR DISCARDS
#2006-2013: FILE CONTAINS 2 AGGREGATES 2006-2008 AND 2009-2013

#LOBSTER AND JONAH:
ann.catch.jonah<-LFA41.logs.jonah%>%group_by(YEAR,NAFOAREA)%>%summarize(CATCH_LBS=round(sum(EST_WEIGHT_LOG_LBS,na.rm=T,2)),
                                                                        ADJ_CATCH_LBS=round(sum(ADJCATCH_LBS,na.rm=T),2))%>%data.frame()

LFA41.lob.catch.0613<-read.delim("C:/LOBSTER/LFA 27-41/Data Sources/Observer Data/raw data files/LFA41.total.catch.lobster.jonah.2006-2013.Nov.2014 .txt",  header = TRUE, sep="\t")
str(LFA41.lob.catch.0613) # NOV_2014
summary(LFA41.lob.catch.0613)

#BYCATCH:
LFA41.bycatch.filter<-read.delim("C:/LOBSTER/LFA 27-41/Data Sources/Observer Data/raw data files/LFA41.total.catch.bycatch.2006-2013.species.filtered.Nov.2014 .txt",  header = TRUE, sep="\t")
str(LFA41.bycatch.filter)

a<-subset(LFA41.lob.catch.0613,TYPE=="Lobster_Kept")
bycatch.MT<-merge(LFA41.bycatch.filter,a,by=c('YEAR','AREA'))
bycatch.MT$KG.MT<-bycatch.MT$DISC_KG/bycatch.MT$LOB_KEPT_MT



#ESTMATED TOTAL CATCH OF LOBSTER AND JONAH CRAB:
str(LFA41.lob.catch.0613) # NOV_2014

LFA41.lob.catch.0613$TYPE<-ordered(LFA41.lob.catch.0613$TYPE, levels=c("Lobster_Kept","Lobster_Discard","Jonah_Kept","Jonah_Discard"))

ggplot(LFA41.lob.catch.0613,aes(TYPE,CATCH_KG/1000,,fill=factor(YEAR))) + geom_bar(stat='identity', position=position_dodge(),width=.8) +  facet_wrap(~AREA) +
  xlab('') + scale_y_continuous(limits=c(0,800),breaks=seq(0, 800, 100)) + 
  ggtitle("LFA41 - Lobster and Jonah Catch") + ylab("Catch (*1000 kg)") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5,size=08, colour='black'),panel.background=element_rect(fill="white", colour='black'),
        panel.border=element_rect(,fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),
        axis.title.y= element_text(size=10), title=element_text(size=12),legend.justification=c(1,1), legend.position=c(1,1),
        legend.key.size = unit(0.5,"cm")) +  
  scale_fill_manual(name="Year",values=Palette)

#ESTMATED TOTAL CATCH OF LOBSTER AND JONAH CRAB AS KG/MT:
str(LFA41.lob.catch.0613)
b<-subset(LFA41.lob.catch.0613,TYPE!="Lobster_Kept")
b$TYPE<-ordered(b$TYPE, levels=c("Lobster_Discard","Jonah_Kept","Jonah_Discard"))
b$KG.MT<-b$CATCH_KG/b$LOB_KEPT_MT

ggplot(b,aes(TYPE,KG.MT,,fill=factor(YEAR))) + geom_bar(stat='identity', position=position_dodge(),width=.8) +  facet_wrap(~AREA) +
  xlab('') + scale_y_continuous(limits=c(0,700),breaks=seq(0, 700, 100)) + 
  ggtitle("LFA41 - Lobster and Jonah Catch") + ylab("Catch (kg/MT lobster landed)") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5,size=08, colour='black'),panel.background=element_rect(fill="white", colour='black'),
        panel.border=element_rect(,fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),
        axis.title.y= element_text(size=10), title=element_text(size=12),legend.justification=c(0,1), legend.position=c(0,1),
        legend.key.size = unit(0.5,"cm")) +  
  scale_fill_manual(name="Year",values=Palette)


