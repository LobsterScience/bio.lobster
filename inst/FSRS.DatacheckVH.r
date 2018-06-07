require (chron)
require (lattice)
require (MASS)
require (doBy)
require (RODBC)
##Checks overwritten for 2017 data - uploaded in Jan 2018##

lobster.db("fsrs.redo")
lobster.db("fsrs")
str(fsrs)    #1365712 Records for 2017
FSRS<-fsrs
##COLUMN ADDITIONS TO DATA FRAME ALLOWING DIFFERENT DATE FORMATS
fsrs$julian = as.integer(julian(fsrs$HAUL_DATE))
summary(fsrs)

##TIDY UP THE LFA SPECIFICATIONS
sort(unique(fsrs$LFA))

#CHECK SOAK DAYS BY COMPARING SAME VESSEL, SAME DAY THAT HAS DIFFERENT DEPTHS:
fsrs$Unique<-paste(fsrs$VESSEL_CD,as.character(fsrs$HAUL_DATE),sep = " ")
Depth.dif<-sapply(split(fsrs$DEPTH,fsrs$Unique),function(x){length(unique(x))}) #Should only be one record: LFA 34 - 1230 2001-11-30 (0.5 SD) 
table(Depth.dif)
a<-names(Depth.dif)[which(Depth.dif==2)] #Should only be one record: LFA 34 - 1230 2001-11-30 (0.5 SD)
dups<-fsrs[which(fsrs$Unique %in% a),]
unique(dups$Unique)

library(xlsx)
write.xlsx(dups, file=paste("Duplicates",Sys.Date(),".xlsx"))

fsrs<-fsrs[,-29]
#----------------------------------------------------------------------------------------------------------------------------------------------------------------

##                                                           CREATION OF DATA FILES FOR EACH LFA:

#---------------------------------------------------------------------------LFA27--------------------------------------------------------------------------------
LFA27.9917=which(fsrs$LFA==27 & fsrs$HAUL_YEAR)
length(LFA27.9917)  # 238,973 RECORDS 2016   259,917 for 2017
LFA27.9917=fsrs[LFA27.9917,]
unique(LFA27.9917$HAUL_YEAR)
summary(LFA27.9917)

##EMPTY TRAPS
e=which(LFA27.9917$LOBSTER_NO == 0)
length(e)  #7917 records for 2016 and  8029 records for 2017

##VALIDATE SOAK DAYS:
table(LFA27.9917$SOAK_DAYS,LFA27.9917$HAUL_YEAR)


#   1999  2000  2001  2002  2003  2004  2005  2006  2007  2008  2009  2010  2011  2012  2013  2014  2015  2016  2017
#1  5878  5205  5353  4834  6369  7641  7661 10960  8494  9878  8010  8288  8421  7206  8230  8900  9732  9790 12952
#2  2844  2539  2944  2547  4066  3591  3689  5108  4577  5384  4854  5372  4336  3569  5009  5476  6917  6906  5844
#3   285   375   370   430  1004   734   843   687   483   789  1082  1082   635   475   863  1185  1936  1441  1615
#4    33   101   129   103   179    59   155    53    18   413   196   143    97    34   160   180   246   233   332
#5    84     9    32    68    27    29     0    82     0     0    92     0   112    89   106    75    31    90   116
#6     0    21     0     0     0     0     0     8     0    55    30     0    12    46     0     0     0     0     0
#7     0    30     0     0     0     0     4     0     0     0     0     0     0    15     0     0     0     0    85
#9     0     0     0     0     0     6     0     0     0     0    11     0     0     0     0     0     0     0     0

	
write.table(LFA27.9917, file=paste('rec.LFA27 9917.Noremovals.from.datacheck.9917.',Sys.Date(),'.txt'), row.names=FALSE, col.names=TRUE, sep=" ")

#SOAK DAY REMOVALS (FOR MODELING):
#LFA 27 SOAK DAYS ARE MOSTLY WITHN 1 TO 5 DAYS
sd=which(LFA27.9917$SOAK_DAYS > 5)
length(sd)  #289 RECORDS ( I'm getting 238 records for 2016 and 323 for 2017)
LFA27.9917.sd=LFA27.9917[-sd,]

##CALCULATE TRAP # PER DAY
LFA27.9917.traps<-aggregate(TRAP_NO~HAUL_YEAR+VESSEL_CD+HAUL_DATE, data=LFA27.9917.sd, FUN=function(x) length(unique(x)))
#LFA27.9917.traps<-ddply(LFA27.9917.sd,c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE"),summarize,TOT_TRAPS=length(unique(TRAP_NO)))
str(LFA27.9917.traps) # 19446 for 2017

##SIZE ADJUSTMENTS for MLS CHANGE IN 1999-2002
table(LFA27.9917$SIZE_CD,LFA27.9917$SHORT)
a1= which (LFA27.9917$SIZE_CD==8 & LFA27.9917$SHORT==1)
LFA27.9917[a1,"SIZE_CD"]=8.1
summary(LFA27.9917[a1,])
a2= which (LFA27.9917$SIZE_CD==8 & LFA27.9917$SHORT==0)
LFA27.9917[a2,"SIZE_CD"]=8.2
summary(LFA27.9917[a2,])
summary(LFA27.9917)
str(LFA27.9917)
a3= which (LFA27.9917$SIZE_CD==9 & LFA27.9917$SHORT==1)
LFA27.9917[a3,"SIZE_CD"]=9.1
summary(LFA27.9917[a3,])
a4= which (LFA27.9917$SIZE_CD==9 & LFA27.9917$SHORT==0)
LFA27.9917[a4,"SIZE_CD"]=9.2
summary(LFA27.9917[a4,])
summary(LFA27.9917)
str(LFA27.9917)
a5= which (LFA27.9917$SIZE_CD==10 & LFA27.9917$SHORT==1)
LFA27.9917[a5,"SIZE_CD"]=10.1
summary(LFA27.9917[a5,])
a6= which (LFA27.9917$SIZE_CD==10 & LFA27.9917$SHORT==0)
LFA27.9917[a6,"SIZE_CD"]=10.2
summary(LFA27.9917[a6,])
summary(LFA27.9917)
str(LFA27.9917)

table(LFA27.9917$SIZE_CD,LFA27.9917$SHORT)

##CALCULATE TOTAL COUNT OF BERRIED PER DAY PER SIZE
b= which (LFA27.9917$SEX==3)
length (b) #24,765 RECORDS JAN.2016 SQL EXPORT
rec.LFA27.ber.fem <- LFA27.9917[b,]
rec.LFA27.ber.fem =aggregate(rec.LFA27.ber.fem[,1], by=as.list(rec.LFA27.ber.fem[c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE","SOAK_DAYS", "SIZE_CD")]), length, simplify = TRUE)
rec.LFA27.ber.fem = na.omit (rec.LFA27.ber.fem)
library(gdata)
rec.LFA27.ber.fem <- rename.vars(rec.LFA27.ber.fem, from="x", to="Freq")
summary(rec.LFA27.ber.fem)
str(rec.LFA27.ber.fem) #17,790 RECORDS

##MERGE TOTAL TRAPS INFO WITH BERRIED FEMALE SIZE COUNTS PER DAY
rec.LFA27.berfem.cpue.size = merge(LFA27.9917.traps,rec.LFA27.ber.fem, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE"),all.x=T,all.y=F, sort=F)
rec.LFA27.berfem.cpue.size = na.omit (rec.LFA27.berfem.cpue.size) 
str(rec.LFA27.berfem.cpue.size)  #17,770 RECORDS JAN_2016
summary(rec.LFA27.berfem.cpue.size)
write.table(rec.LFA27.berfem.cpue.size, file="rec.LFA27.berfem.9916.JAN.2016_1to5_1.txt", row.names=FALSE, col.names=TRUE, sep=" ")

##REMOVE RECORDS FOR BERRIED BY INCLUDING IN SEX COLUMN - FOR CPUE ANALYSIS
LFA27.9917=LFA27.9917[-b,]
str(LFA27.9917)

##CALCULATE TOTAL COUNT PER SIZE CLASS
LFA27.9917.counts.at.size =aggregate(LFA27.9917[,1], by=as.list(LFA27.9917[c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "SIZE_CD")]), length, simplify = TRUE)
LFA27.9917.counts.at.size = na.omit (LFA27.9917.counts.at.size)
library(gdata)
LFA27.9917.counts.at.size <- rename.vars(LFA27.9917.counts.at.size, from="x", to="Freq")
summary(LFA27.9917.counts.at.size)
str(LFA27.9917.counts.at.size)   #76,121 RECORDS
unique(LFA27.9917.counts.at.size$SIZE_CD)  #SIZE 0-16; NO NA VALUES

##SIZE CLASS FREQUENCY TABLE
LFA27.cpue.size = merge(LFA27.9917.traps,LFA27.9917.counts.at.size, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE"),all.x=T,all.y=F, sort=F)
LFA27.0=which(LFA27.cpue.size$SIZE_CD=='0')
LFA27.size0=LFA27.cpue.size[LFA27.0,]
LFA27.size0 = rename.df(LFA27.size0,"Freq", "size0")
LFA27.size0$SIZE_CD=NULL
LFA27.2=which(LFA27.cpue.size$SIZE_CD=='2')
LFA27.size2=LFA27.cpue.size[LFA27.2,]
LFA27.size2 = rename.df(LFA27.size2, "Freq", "size2")
LFA27.size2$SIZE_CD=NULL
LFA27.0.2 = merge(LFA27.size0,LFA27.size2, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA27.3=which(LFA27.cpue.size$SIZE_CD=='3')
LFA27.size3=LFA27.cpue.size[LFA27.3,]
LFA27.size3 = rename.df(LFA27.size3, "Freq", "size3")
LFA27.size3$SIZE_CD=NULL
LFA27.0.3 = merge(LFA27.0.2,LFA27.size3, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA27.4=which(LFA27.cpue.size$SIZE_CD=='4')
LFA27.size4=LFA27.cpue.size[LFA27.4,]
LFA27.size4 = rename.df(LFA27.size4, "Freq", "size4")
LFA27.size4$SIZE_CD=NULL
LFA27.0.4 = merge(LFA27.0.3,LFA27.size4, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA27.5=which(LFA27.cpue.size$SIZE_CD=='5')
LFA27.size5=LFA27.cpue.size[LFA27.5,]
LFA27.size5 = rename.df(LFA27.size5, "Freq", "size5")
LFA27.size5$SIZE_CD=NULL
LFA27.0.5 = merge(LFA27.0.4,LFA27.size5, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA27.6=which(LFA27.cpue.size$SIZE_CD=='6')
LFA27.size6=LFA27.cpue.size[LFA27.6,]
LFA27.size6 = rename.df(LFA27.size6, "Freq", "size6")
LFA27.size6$SIZE_CD=NULL
LFA27.0.6 = merge(LFA27.0.5,LFA27.size6, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA27.7=which(LFA27.cpue.size$SIZE_CD=='7')
LFA27.size7=LFA27.cpue.size[LFA27.7,]
LFA27.size7 = rename.df(LFA27.size7, "Freq", "size7")
LFA27.size7$SIZE_CD=NULL
LFA27.0.7 = merge(LFA27.0.6,LFA27.size7, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA27.8.1=which(LFA27.cpue.size$SIZE_CD=='8.1')
LFA27.size8.1=LFA27.cpue.size[LFA27.8.1,]
LFA27.size8.1 = rename.df(LFA27.size8.1, "Freq", "size8.1")
LFA27.size8.1$SIZE_CD=NULL
LFA27.0.8.1 = merge(LFA27.0.7,LFA27.size8.1, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA27.8.2=which(LFA27.cpue.size$SIZE_CD=='8.2')
LFA27.size8.2=LFA27.cpue.size[LFA27.8.2,]
LFA27.size8.2 = rename.df(LFA27.size8.2, "Freq", "size8.2")
LFA27.size8.2$SIZE_CD=NULL
LFA27.0.8.2 = merge(LFA27.0.8.1,LFA27.size8.2, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA27.9.1=which(LFA27.cpue.size$SIZE_CD=='9.1')
LFA27.size9.1=LFA27.cpue.size[LFA27.9.1,]
LFA27.size9.1 = rename.df(LFA27.size9.1, "Freq", "size9.1")
LFA27.size9.1$SIZE_CD=NULL
LFA27.0.9.1 = merge(LFA27.0.8.2,LFA27.size9.1, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA27.9.2=which(LFA27.cpue.size$SIZE_CD=='9.2')
LFA27.size9.2=LFA27.cpue.size[LFA27.9.2,]
LFA27.size9.2 = rename.df(LFA27.size9.2, "Freq", "size9.2")
LFA27.size9.2$SIZE_CD=NULL
LFA27.0.9.2 = merge(LFA27.0.9.1,LFA27.size9.2, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA27.10.1=which(LFA27.cpue.size$SIZE_CD=='10.1')
LFA27.size10.1=LFA27.cpue.size[LFA27.10.1,]
LFA27.size10.1 = rename.df(LFA27.size10.1, "Freq", "size10.1")
LFA27.size10.1$SIZE_CD=NULL
LFA27.0.10.1 = merge(LFA27.0.9.2,LFA27.size10.1, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA27.10.2=which(LFA27.cpue.size$SIZE_CD=='10.2')
LFA27.size10.2=LFA27.cpue.size[LFA27.10.2,]
LFA27.size10.2 = rename.df(LFA27.size10.2, "Freq", "size10.2")
LFA27.size10.2$SIZE_CD=NULL
LFA27.0.10.2 = merge(LFA27.0.10.1,LFA27.size10.2, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA27.11=which(LFA27.cpue.size$SIZE_CD=='11')
LFA27.size11=LFA27.cpue.size[LFA27.11,]
LFA27.size11 = rename.df(LFA27.size11, "Freq", "size11")
LFA27.size11$SIZE_CD=NULL
LFA27.0.11 = merge(LFA27.0.10.2,LFA27.size11, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA27.12=which(LFA27.cpue.size$SIZE_CD=='12')
LFA27.size12=LFA27.cpue.size[LFA27.12,]
LFA27.size12 = rename.df(LFA27.size12, "Freq", "size12")
LFA27.size12$SIZE_CD=NULL
LFA27.0.12 = merge(LFA27.0.11,LFA27.size12, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA27.13=which(LFA27.cpue.size$SIZE_CD=='13')
LFA27.size13=LFA27.cpue.size[LFA27.13,]
LFA27.size13 = rename.df(LFA27.size13, "Freq", "size13")
LFA27.size13$SIZE_CD=NULL
LFA27.0.13 = merge(LFA27.0.12,LFA27.size13, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA27.14=which(LFA27.cpue.size$SIZE_CD=='14')
LFA27.size14=LFA27.cpue.size[LFA27.14,]
LFA27.size14 = rename.df(LFA27.size14, "Freq", "size14")
LFA27.size14$SIZE_CD=NULL
LFA27.0.14 = merge(LFA27.0.13,LFA27.size14, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA27.15=which(LFA27.cpue.size$SIZE_CD=='15')
LFA27.size15=LFA27.cpue.size[LFA27.15,]
LFA27.size15 = rename.df(LFA27.size15, "Freq", "size15")
LFA27.size15$SIZE_CD=NULL
LFA27.0.15 = merge(LFA27.0.14,LFA27.size15, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA27.16=which(LFA27.cpue.size$SIZE_CD=='16')
LFA27.size16=LFA27.cpue.size[LFA27.16,]
LFA27.size16 = rename.df(LFA27.size16, "Freq", "size16")
LFA27.size16$SIZE_CD=NULL
LFA27.0.16 = merge(LFA27.0.15,LFA27.size16, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA27.0.16[is.na(LFA27.0.16)]=0
summary(LFA27.0.16)

##SIZE CLASS SPECS FOR all LOBSTERS FROM 1999-2014
LFA27.0.16$shorts=NA
LFA27.0.16$shorts=LFA27.0.16$size0+LFA27.0.16$size2+LFA27.0.16$size3+LFA27.0.16$size4+LFA27.0.16$size5+LFA27.0.16$size6+
  LFA27.0.16$size7+LFA27.0.16$size8.1+LFA27.0.16$size9.1 + LFA27.0.16$size10.1

LFA27.0.16$legals=NA
LFA27.0.16$legals=LFA27.0.16$size8.2+LFA27.0.16$size9.2+LFA27.0.16$size10.2+LFA27.0.16$size11+LFA27.0.16$size12+LFA27.0.16$size13+
  LFA27.0.16$size14+LFA27.0.16$size15+LFA27.0.16$size16

summary(LFA27.0.16)

#Max shorts = 78; Max legals = 29
write.table(LFA27.0.16, file=paste("rec.LFA27.9917.SD1to5.nober.",Sys.Date(),".txt"), row.names=TRUE, col.names=TRUE, sep=" ")

#--------------------------------LFA28---------------------------------------
##LFA 28  - NO PARTICIPATION SINCE 2013

LFA28.9913=which(fsrs$LFA==28)
length(LFA28.9913) #7198 JAN_2018 
LFA28.9913=fsrs[LFA28.9913,]
unique(LFA28.9913$HAUL_YEAR)
summary(LFA28.9913)

##EMPTY TRAPS
e=which(LFA28.9913$LOBSTER_NO==0)
length(e)  #558 Jan 2018

##VALIDATE SOAK DAYS:
table(LFA28.9913$SOAK_DAYS, LFA28.9913$HAUL_YEAR)

 # 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013
#1   64  208   22  209  125   73    0   30  499   65  433  379   47
#2  298  503  459  289  413  492  127  202  299  153   70  136  279
#3  115  108   96   93   96  181  117   96    0  123   71    0    5
#4   60    0   46   13   15    0    0   26    0    0   20    0    0
#5    5    0    0    0    0    0   18    0    0   20    0    0    0
>
  
write.table(LFA28.9913, file=paste("rec.LFA28 9913.Noremovals.from.datacheck.9913",Sys.Date(),".txt"), row.names=FALSE, col.names=TRUE, sep=" ")

#CPUE PREP FILE:
##DAILY AGGREGATE FOR CPUE ANALYSIS:
#SOAK DAY CONSTRAINT:

#FOR THIS LFA SOAK DAYS ARE ALL BETWEEN 1 AND 5

##CALCULATE TRAP # PER DAY
LFA28.9913.traps = compute.unique.lengths(x=LFA28.9913, var="TRAP_NO", index=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE"))
LFA28.9913.traps = na.omit (LFA28.9913.traps)
LFA28.9913.traps = rename.df(LFA28.9913.traps,"Freq", "TOT_TRAPS")
summary(LFA28.9913.traps)
str(LFA28.9913.traps) #573 MAY_2014

#SHOWS NUMBER OF RECORDS WITH TRAP NUMBER DISTRIBUTION PER VESSEL
table(LFA28.9913.traps$TOT_TRAPS,LFA28.9913.traps$VESSEL_CD)

##SIZE CLASS ADJUSTMENT TO SPLIT THE SHORTS AND LEGAL SIZES IN CLASS 10
table(LFA28.9917$SIZE_CD, LFA28.9916$SHORT)
b1= which (LFA28.9913$SIZE_CD==10 & LFA28.9913$SHORT==1)
LFA28.9913[b1,"SIZE_CD"]=10.1
summary(LFA28.9913[b1,])
b2= which (LFA28.9913$SIZE_CD==10 & LFA28.9913$SHORT==0)
LFA28.9913[b2,"SIZE_CD"]=10.2
summary(LFA28.9913[b2,])
table(LFA28.9916$SIZE_CD, LFA28.9916$SHORT)

##REMOVE RECORDS FOR BERRIED BY INCLUDING IN SEX COLUMN - FOR CPUE ANALYSIS
b= which (LFA28.9913$SEX==3)
length (b) #2319 MAY_2014 
LFA28.9913.sd.noberried=LFA28.9913[-b,]
str(LFA28.9913.sd.noberried)

##CALCULATE TOTAL COUNT PER SIZE CLASS
LFA28.9913.sd.noberried$VESSEL_CD <- as.factor(LFA28.9913.sd.noberried$VESSEL_CD)
LFA28.9913.sd.noberried$SIZE_CD <- as.factor(LFA28.9913.sd.noberried$SIZE_CD)

LFA28.9913.counts.at.size =aggregate(LFA28.9913.sd.noberried[,1], by=as.list(LFA28.9913.sd.noberried[c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "SIZE_CD")]), length, simplify = TRUE)
LFA28.9913.counts.at.size = na.omit (LFA28.9913.counts.at.size)
library(gdata)
LFA28.9913.counts.at.size <- rename.vars(LFA28.9913.counts.at.size, from="x", to="Freq")
summary(LFA28.9913.counts.at.size)
str(LFA28.9913.counts.at.size)   #MAY_2014 2403 RECORDS
unique(LFA28.9913.counts.at.size$SIZE_CD)  #SIZE 0-16; NO NA VALUES

##SIZE CLASS FREQUENCY TABLE
LFA28.cpue.size = merge(LFA28.9913.traps,LFA28.9913.counts.at.size, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE"),all.x=T,all.y=F, sort=F)
LFA28.0=which(LFA28.cpue.size$SIZE_CD=='0')
LFA28.size0=LFA28.cpue.size[LFA28.0,]
LFA28.size0 = rename.df(LFA28.size0,"Freq", "size0")
LFA28.size0$SIZE_CD=NULL
LFA28.3=which(LFA28.cpue.size$SIZE_CD=='3')
LFA28.size3=LFA28.cpue.size[LFA28.3,]
LFA28.size3 = rename.df(LFA28.size3, "Freq", "size3")
LFA28.size3$SIZE_CD=NULL
LFA28.0.3 = merge(LFA28.size0,LFA28.size3, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA28.4=which(LFA28.cpue.size$SIZE_CD=='4')
LFA28.size4=LFA28.cpue.size[LFA28.4,]
LFA28.size4 = rename.df(LFA28.size4, "Freq", "size4")
LFA28.size4$SIZE_CD=NULL
LFA28.0.4 = merge(LFA28.0.3,LFA28.size4, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA28.5=which(LFA28.cpue.size$SIZE_CD=='5')
LFA28.size5=LFA28.cpue.size[LFA28.5,]
LFA28.size5 = rename.df(LFA28.size5, "Freq", "size5")
LFA28.size5$SIZE_CD=NULL
LFA28.0.5 = merge(LFA28.0.4,LFA28.size5, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA28.6=which(LFA28.cpue.size$SIZE_CD=='6')
LFA28.size6=LFA28.cpue.size[LFA28.6,]
LFA28.size6 = rename.df(LFA28.size6, "Freq", "size6")
LFA28.size6$SIZE_CD=NULL
LFA28.0.6 = merge(LFA28.0.5,LFA28.size6, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA28.7=which(LFA28.cpue.size$SIZE_CD=='7')
LFA28.size7=LFA28.cpue.size[LFA28.7,]
LFA28.size7 = rename.df(LFA28.size7, "Freq", "size7")
LFA28.size7$SIZE_CD=NULL
LFA28.0.7 = merge(LFA28.0.6,LFA28.size7, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA28.8=which(LFA28.cpue.size$SIZE_CD=='8')
LFA28.size8=LFA28.cpue.size[LFA28.8,]
LFA28.size8 = rename.df(LFA28.size8, "Freq", "size8")
LFA28.size8$SIZE_CD=NULL
LFA28.0.8 = merge(LFA28.0.7,LFA28.size8, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA28.9=which(LFA28.cpue.size$SIZE_CD=='9')
LFA28.size9=LFA28.cpue.size[LFA28.9,]
LFA28.size9 = rename.df(LFA28.size9, "Freq", "size9")
LFA28.size9$SIZE_CD=NULL
LFA28.0.9 = merge(LFA28.0.8,LFA28.size9, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA28.10.1=which(LFA28.cpue.size$SIZE_CD=='10.1')
LFA28.size10.1=LFA28.cpue.size[LFA28.10.1,]
LFA28.size10.1 = rename.df(LFA28.size10.1, "Freq", "size10.1")
LFA28.size10.1$SIZE_CD=NULL
LFA28.0.10.1 = merge(LFA28.0.9,LFA28.size10.1, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA28.10.2=which(LFA28.cpue.size$SIZE_CD=='10.2')
LFA28.size10.2=LFA28.cpue.size[LFA28.10.2,]
LFA28.size10.2 = rename.df(LFA28.size10.2, "Freq", "size10.2")
LFA28.size10.2$SIZE_CD=NULL
LFA28.0.10.2 = merge(LFA28.0.10.1,LFA28.size10.2, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA28.11=which(LFA28.cpue.size$SIZE_CD=='11')
LFA28.size11=LFA28.cpue.size[LFA28.11,]
LFA28.size11 = rename.df(LFA28.size11, "Freq", "size11")
LFA28.size11$SIZE_CD=NULL
LFA28.0.11 = merge(LFA28.0.10.2,LFA28.size11, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA28.12=which(LFA28.cpue.size$SIZE_CD=='12')
LFA28.size12=LFA28.cpue.size[LFA28.12,]
LFA28.size12 = rename.df(LFA28.size12, "Freq", "size12")
LFA28.size12$SIZE_CD=NULL
LFA28.0.12 = merge(LFA28.0.11,LFA28.size12, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA28.13=which(LFA28.cpue.size$SIZE_CD=='13')
LFA28.size13=LFA28.cpue.size[LFA28.13,]
LFA28.size13 = rename.df(LFA28.size13, "Freq", "size13")
LFA28.size13$SIZE_CD=NULL
LFA28.0.13 = merge(LFA28.0.12,LFA28.size13, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA28.14=which(LFA28.cpue.size$SIZE_CD=='14')
LFA28.size14=LFA28.cpue.size[LFA28.14,]
LFA28.size14 = rename.df(LFA28.size14, "Freq", "size14")
LFA28.size14$SIZE_CD=NULL
LFA28.0.14 = merge(LFA28.0.13,LFA28.size14, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA28.15=which(LFA28.cpue.size$SIZE_CD=='15')
LFA28.size15=LFA28.cpue.size[LFA28.15,]
LFA28.size15 = rename.df(LFA28.size15, "Freq", "size15")
LFA28.size15$SIZE_CD=NULL
LFA28.0.15 = merge(LFA28.0.14,LFA28.size15, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA28.16=which(LFA28.cpue.size$SIZE_CD=='16')
LFA28.size16=LFA28.cpue.size[LFA28.16,]
LFA28.size16 = rename.df(LFA28.size16, "Freq", "size16")
LFA28.size16$SIZE_CD=NULL
LFA28.0.16 = merge(LFA28.0.15,LFA28.size16, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA28.0.16[is.na(LFA28.0.16)]=0
summary(LFA28.0.16)

##SIZE CLASS SPECS FOR UNDERSIZED LOBSTERS FROM 1999-2013
LFA28.0.16$shorts=NA
LFA28.0.16$shorts=LFA28.0.16$size0+LFA28.0.16$size3+LFA28.0.16$size4+LFA28.0.16$size5+LFA28.0.16$size6+
  LFA28.0.16$size7+LFA28.0.16$size8+LFA28.0.16$size9+LFA28.0.16$size10.1
##SIZE CLASS SPECS FOR LEGAL LOBSTERS FROM 2004-2011
LFA28.0.16$legals=NA
LFA28.0.16$legals=LFA28.0.16$size10.2+LFA28.0.16$size11+LFA28.0.16$size12+LFA28.0.16$size13+
  LFA28.0.16$size14+LFA28.0.16$size15+LFA28.0.16$size16
summary(LFA28.0.16)

#Max shorts = 28; Max legals = 14
write.table(LFA28.0.16, file=paste("rec.LFA28.9913.SD1to5.nober.",Sys.Date(),".txt"), row.names=TRUE, col.names=TRUE, sep=" ")

#--------------------------------LFA29---------------------------------------
LFA29.9917=which(fsrs$LFA==29)
length(LFA29.9917)  # 110608 RECORDS FOR 2017
LFA29.9917=fsrs[LFA29.9917,]
unique(LFA29.9917$HAUL_YEAR)
summary(LFA29.9917)

##EMPTY TRAPS
e=which(LFA29.9917$LOBSTER_NO==0)
length(e)  #2178 RECORDS FOR 2017

##VALIDATE SOAK DAYS:
table(LFA29.9917$SOAK_DAYS,LFA29.9917$HAUL_YEAR)

#  1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017
#1   88  414  483  461  902 1770 2911 7665 7851 8892 8232 6566 5892 6233 5838 6041 8342 8121 8956
#2  111  246  371  388  378  784  475  721  933  589  547  894  851  531  927  889 1146  924 1090
#3   12   50   20   50  158   81   91    8   76  148  195  125   60    0   94  105   44   36  260
#4    0   16    5    0   12    0   13   17   40   98   30    0    0    0    0   98   21    0   17
#5    0    0    0    0    0    0    0    0   76   15    0    0    0    0    0    0    0    0   60
#6    0    0    0   12    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
#7    0   12    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0

  
write.table(LFA29.9917, file=paste("rec.LFA29 9917.Noremovals.from.datacheck.9917",Sys.Date(),".txt"), row.names=FALSE, col.names=TRUE, sep=" ")

#CPUE PREP FILE:
#SOAK DAY CONSTRAINT:  
sd=which(LFA29.9917$SOAK_DAYS > 5)
length(sd)  #24 RECORDS
LFA29.9917.sd=LFA29.9917[-sd,]

##CALCULATE TRAP # PER DAY
#LFA29.9917.traps = compute.unique.lengths( x=LFA29.9917.sd, var="TRAP_NO", index=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE")  )
#LFA29.9917.traps = na.omit (LFA29.9917.traps)
#LFA29.9917.traps = rename.df(LFA29.9917.traps,"Freq", "TOT_TRAPS")


LFA29.9917.traps<-aggregate(TRAP_NO~HAUL_YEAR+VESSEL_CD+HAUL_DATE, data=LFA29.9917.sd, FUN=function(x) length(unique(x)))
str(LFA29.9917.traps)  # 5436 Records for 2017
summary(LFA29.9917.traps)



#SHOWS NUMBER OF RECORDS WITH TRAP NUMBER DISTRIBUTION PER VESSEL
table(LFA29.9917.traps$TOT_TRAPS,LFA29.9917.traps$VESSEL_CD)

##SIZE ADJUSTMENTS for MLS to 84 mm IN 1999-2013
table(LFA29.9916.sd$SIZE_CD,LFA29.9916.sd$SHORT)

c1= which (LFA29.9916.sd$SIZE_CD==10 & LFA29.9916.sd$SHORT==1)
LFA29.9916.sd[c1,"SIZE_CD"]=10.1
c2= which (LFA29.9916.sd$SIZE_CD==10 & LFA29.9916.sd$SHORT==0)
LFA29.9916.sd[c2,"SIZE_CD"]=10.2 
summary(LFA29.9916.sd[c1,])
summary(LFA29.9916.sd[c2,])

##REMOVE RECORDS FOR BERRIED BY INCLUDING IN SEX COLUMN - FOR CPUE ANALYSIS
b= which (LFA29.9916.sd$SEX==3)
length (b) #12,563 RECORDS
LFA29.9916.sd.noberried=LFA29.9916.sd[-b,]
str(LFA29.9916.sd.noberried)

##FILE EDITS BEFORE FURTHER COMPUTING
LFA29.9913.sd.noberried$VESSEL_CD <- as.factor(LFA29.9913.sd.noberried$VESSEL_CD)
LFA29.9913.sd.noberried$SIZE_CD <- as.factor(LFA29.9913.sd.noberried$SIZE_CD)

##CALCULATE TOTAL COUNT PER SIZE CLASS
LFA29.9913.counts.at.size =aggregate(LFA29.9913.sd.noberried[,1], by=as.list(LFA29.9913.sd.noberried[c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "SIZE_CD")]), length, simplify = TRUE)
LFA29.9913.counts.at.size = na.omit (LFA29.9913.counts.at.size)
library(gdata)
LFA29.9913.counts.at.size <- rename.vars(LFA29.9913.counts.at.size, from="x", to="Freq")
summary(LFA29.9913.counts.at.size)
str(LFA29.9913.counts.at.size)   #21,435 RECORDS MAY_2014
unique(LFA29.9913.counts.at.size$SIZE_CD)  #SIZE 0-16; NO NA VALUES

##SIZE CLASS FREQUENCY TABLE
LFA29.cpue.size = merge(LFA29.9913.traps,LFA29.9913.counts.at.size, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE"),all.x=T,all.y=F, sort=F)
LFA29.0=which(LFA29.cpue.size$SIZE_CD=='0')
LFA29.size0=LFA29.cpue.size[LFA29.0,]
LFA29.size0 = rename.df(LFA29.size0,"Freq", "size0")
LFA29.size0$SIZE_CD=NULL
LFA29.2=which(LFA29.cpue.size$SIZE_CD=='2')
LFA29.size2=LFA29.cpue.size[LFA29.2,]
LFA29.size2 = rename.df(LFA29.size2, "Freq", "size2")
LFA29.size2$SIZE_CD=NULL
LFA29.0.2 = merge(LFA29.size0,LFA29.size2, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA29.3=which(LFA29.cpue.size$SIZE_CD=='3')
LFA29.size3=LFA29.cpue.size[LFA29.3,]
LFA29.size3 = rename.df(LFA29.size3, "Freq", "size3")
LFA29.size3$SIZE_CD=NULL
LFA29.0.3 = merge(LFA29.0.2,LFA29.size3, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA29.4=which(LFA29.cpue.size$SIZE_CD=='4')
LFA29.size4=LFA29.cpue.size[LFA29.4,]
LFA29.size4 = rename.df(LFA29.size4, "Freq", "size4")
LFA29.size4$SIZE_CD=NULL
LFA29.0.4 = merge(LFA29.0.3,LFA29.size4, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA29.5=which(LFA29.cpue.size$SIZE_CD=='5')
LFA29.size5=LFA29.cpue.size[LFA29.5,]
LFA29.size5 = rename.df(LFA29.size5, "Freq", "size5")
LFA29.size5$SIZE_CD=NULL
LFA29.0.5 = merge(LFA29.0.4,LFA29.size5, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA29.6=which(LFA29.cpue.size$SIZE_CD=='6')
LFA29.size6=LFA29.cpue.size[LFA29.6,]
LFA29.size6 = rename.df(LFA29.size6, "Freq", "size6")
LFA29.size6$SIZE_CD=NULL
LFA29.0.6 = merge(LFA29.0.5,LFA29.size6, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA29.7=which(LFA29.cpue.size$SIZE_CD=='7')
LFA29.size7=LFA29.cpue.size[LFA29.7,]
LFA29.size7 = rename.df(LFA29.size7, "Freq", "size7")
LFA29.size7$SIZE_CD=NULL
LFA29.0.7 = merge(LFA29.0.6,LFA29.size7, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA29.8=which(LFA29.cpue.size$SIZE_CD=='8')
LFA29.size8=LFA29.cpue.size[LFA29.8,]
LFA29.size8 = rename.df(LFA29.size8, "Freq", "size8")
LFA29.size8$SIZE_CD=NULL
LFA29.0.8 = merge(LFA29.0.7,LFA29.size8, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA29.9=which(LFA29.cpue.size$SIZE_CD=='9')
LFA29.size9=LFA29.cpue.size[LFA29.9,]
LFA29.size9 = rename.df(LFA29.size9, "Freq", "size9")
LFA29.size9$SIZE_CD=NULL
LFA29.0.9 = merge(LFA29.0.8,LFA29.size9, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA29.10.1=which(LFA29.cpue.size$SIZE_CD=='10.1')
LFA29.size10.1=LFA29.cpue.size[LFA29.10.1,]
LFA29.size10.1 = rename.df(LFA29.size10.1, "Freq", "size10.1")
LFA29.size10.1$SIZE_CD=NULL
LFA29.0.10.1 = merge(LFA29.0.9,LFA29.size10.1, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA29.10.2=which(LFA29.cpue.size$SIZE_CD=='10.2')
LFA29.size10.2=LFA29.cpue.size[LFA29.10.2,]
LFA29.size10.2 = rename.df(LFA29.size10.2, "Freq", "size10.2")
LFA29.size10.2$SIZE_CD=NULL
LFA29.0.10.2 = merge(LFA29.0.10.1,LFA29.size10.2, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA29.11=which(LFA29.cpue.size$SIZE_CD=='11')
LFA29.size11=LFA29.cpue.size[LFA29.11,]
LFA29.size11 = rename.df(LFA29.size11, "Freq", "size11")
LFA29.size11$SIZE_CD=NULL
LFA29.0.11 = merge(LFA29.0.10.2,LFA29.size11, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA29.12=which(LFA29.cpue.size$SIZE_CD=='12')
LFA29.size12=LFA29.cpue.size[LFA29.12,]
LFA29.size12 = rename.df(LFA29.size12, "Freq", "size12")
LFA29.size12$SIZE_CD=NULL
LFA29.0.12 = merge(LFA29.0.11,LFA29.size12, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA29.13=which(LFA29.cpue.size$SIZE_CD=='13')
LFA29.size13=LFA29.cpue.size[LFA29.13,]
LFA29.size13 = rename.df(LFA29.size13, "Freq", "size13")
LFA29.size13$SIZE_CD=NULL
LFA29.0.13 = merge(LFA29.0.12,LFA29.size13, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA29.14=which(LFA29.cpue.size$SIZE_CD=='14')
LFA29.size14=LFA29.cpue.size[LFA29.14,]
LFA29.size14 = rename.df(LFA29.size14, "Freq", "size14")
LFA29.size14$SIZE_CD=NULL
LFA29.0.14 = merge(LFA29.0.13,LFA29.size14, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA29.15=which(LFA29.cpue.size$SIZE_CD=='15')
LFA29.size15=LFA29.cpue.size[LFA29.15,]
LFA29.size15 = rename.df(LFA29.size15, "Freq", "size15")
LFA29.size15$SIZE_CD=NULL
LFA29.0.15 = merge(LFA29.0.14,LFA29.size15, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA29.16=which(LFA29.cpue.size$SIZE_CD=='16')
LFA29.size16=LFA29.cpue.size[LFA29.16,]
LFA29.size16 = rename.df(LFA29.size16, "Freq", "size16")
LFA29.size16$SIZE_CD=NULL
LFA29.0.16 = merge(LFA29.0.15,LFA29.size16, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA29.0.16[is.na(LFA29.0.16)]=0
summary(LFA29.0.16)

##SIZE CLASS SPECS FOR UNDERSIZED LOBSTERS FROM 1999-2013
LFA29.0.16$shorts=NA
LFA29.0.16$shorts=LFA29.0.16$size0+LFA29.0.16$size2+LFA29.0.16$size3+LFA29.0.16$size4+LFA29.0.16$size5+LFA29.0.16$size6+
  LFA29.0.16$size7+LFA29.0.16$size8+LFA29.0.16$size9+LFA29.0.16$size10.1
##SIZE CLASS SPECS FOR LEGAL LOBSTERS FROM 1999-2013
LFA29.0.16$legals=NA
LFA29.0.16$legals=LFA29.0.16$size10.2+LFA29.0.16$size11+LFA29.0.16$size12+LFA29.0.16$size13+
  LFA29.0.16$size14+LFA29.0.16$size15+LFA29.0.16$size16
summary(LFA29.0.16)

#Max shorts = 58; Max legals = 34
write.table(LFA29.0.16, file=paste("rec.LFA29.9913.SD1to5.nober.",Sys.Date(),".txt"), row.names=TRUE, col.names=TRUE, sep=" ")

#----------------------------------LFA30------------------------------------
#LFA 30
LFA30.9916=which(fsrs$LFA==30)
length(LFA30.9916) #70,574 JAN_2016
LFA30.9916=fsrs[LFA30.9916,]
unique(LFA30.9916$HAUL_YEAR)
summary(LFA30.9916)

##EMPTY TRAPS
e=which(LFA30.9916$LOBSTER_NO==0)
length(e)  #1,310 RECORDS

##VALIDATE SOAK DAYS:
> table(LFA30.9916$SOAK_DAYS, LFA30.9916$HAUL_YEAR)

  2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015
1  352  374  364  354 1030 1320 4538 5080 4444 5191 4839 4423 5029 4811 4869 5425
2   88  116  113  151  335  623 1345 1469 1426 1225 1309 1585 1161 1118 1258 1627
3    0   43   40   38   22  165  278   43  234  280  322  191  196  134  188  319
4    9   19   13   16   18   17   56    0   73   12   99   71    9   58   60    0
5    0    0    0    0    0    0    0    0   81    0    0    0    0    0   46    0
6    0    0    0   12    0    0    0    0    0    0    0    0    0    0    0    0
7    0    0    0    0    0    0    0    0   20    0    0    0    0    0    0    0
>
  
write.table(LFA30.9916, file=paste("rec.LFA30 9916.Noremovals.from.datacheck.9916",Sys.Date(),".txt"), row.names=FALSE, col.names=TRUE, sep=" ")

#CPUE PREP FILE:
##DAILY AGGREGATE FOR CPUE ANALYSIS
#SOAK DAY CONSTRAINT:
sd=which(LFA30.9916$SOAK_DAYS > 5)
length(sd)  #32 RECORDS
LFA30.9916.sd=LFA30.9916[-sd,]

##CALCULATE TRAP # PER DAY
LFA30.9916.traps = ddply(LFA30.9916.sd,c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE"),summarize,TOT_TRAPS=length(unique(TRAP_NO)))
summary(LFA30.9916.traps)
str(LFA30.9916.traps) #3,721 RECORDS

#SHOWS NUMBER OF RECORDS WITH TRAP NUMBER DISTRIBUTION PER VESSEL
table(LFA30.9916.traps$TOT_TRAPS,LFA30.9916.traps$VESSEL_CD)
table(LFA30.9916.traps$HAUL_YEAR,LFA30.9916.traps$VESSEL_CD)

##SIZE CLASS ADJUSTMENT TO SPLIT THE SHORTS AND LEGAL SIZES IN CLASS 10
table(LFA30.9916.sd$SIZE_CD, LFA30.9916.sd$SHORT)

d1= which (LFA30.9916.sd$SIZE_CD==10 & LFA30.9916.sd$SHORT==1)
LFA30.9916.sd[d1,"SIZE_CD"]=10.1
summary(LFA30.9916.sd[d1,])
d2= which (LFA30.9916.sd$SIZE_CD==10 & LFA30.9916.sd$SHORT==0)
LFA30.9916.sd[d2,"SIZE_CD"]=10.2
summary(LFA30.9916.sd[d2,])

##REMOVE RECORDS FOR BERRIED BY INCLUDING IN SEX COLUMN - FOR CPUE ANALYSIS
b= which (LFA30.9916.sd$SEX==3)
length (b) #10,446 RECORDS
LFA30.9916.sd.noberried=LFA30.9916.sd[-b,]
str(LFA30.9916.sd.noberried)

##CALCULATE TOTAL COUNT PER SIZE CLASS
LFA30.9916.counts.at.size =aggregate(LFA30.9916.sd.noberried[,1], by=as.list(LFA30.9916.sd.noberried[c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "SIZE_CD")]), length, simplify = TRUE)
LFA30.9916.counts.at.size = na.omit (LFA30.9916.counts.at.size)
library(gdata)
LFA30.9916.counts.at.size <- rename.vars(LFA30.9916.counts.at.size, from="x", to="Freq")
summary(LFA30.9916.counts.at.size)
str(LFA30.9916.counts.at.size)   #17,686 RECORDS
unique(LFA30.9916.counts.at.size$SIZE_CD)  #SIZE 0-16; NO NA VALUES

##SIZE CLASS FREQUENCY TABLE
LFA30.cpue.size = merge(LFA30.9916.traps,LFA30.9916.counts.at.size, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE"),all.x=T,all.y=F, sort=F)
LFA30.0=which(LFA30.cpue.size$SIZE_CD=='0')
LFA30.size0=LFA30.cpue.size[LFA30.0,]
LFA30.size0 = rename.df(LFA30.size0,"Freq", "size0")
LFA30.size0$SIZE_CD=NULL
LFA30.2=which(LFA30.cpue.size$SIZE_CD=='2')
LFA30.size2=LFA30.cpue.size[LFA30.2,]
LFA30.size2 = rename.df(LFA30.size2,"Freq", "size2")
LFA30.size2$SIZE_CD=NULL
LFA30.0.2 = merge(LFA30.size0,LFA30.size2, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA30.3=which(LFA30.cpue.size$SIZE_CD=='3')
LFA30.size3=LFA30.cpue.size[LFA30.3,]
LFA30.size3 = rename.df(LFA30.size3, "Freq", "size3")
LFA30.size3$SIZE_CD=NULL
LFA30.0.3 = merge(LFA30.0.2,LFA30.size3, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA30.4=which(LFA30.cpue.size$SIZE_CD=='4')
LFA30.size4=LFA30.cpue.size[LFA30.4,]
LFA30.size4 = rename.df(LFA30.size4, "Freq", "size4")
LFA30.size4$SIZE_CD=NULL
LFA30.0.4 = merge(LFA30.0.3,LFA30.size4, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA30.5=which(LFA30.cpue.size$SIZE_CD=='5')
LFA30.size5=LFA30.cpue.size[LFA30.5,]
LFA30.size5 = rename.df(LFA30.size5, "Freq", "size5")
LFA30.size5$SIZE_CD=NULL
LFA30.0.5 = merge(LFA30.0.4,LFA30.size5, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA30.6=which(LFA30.cpue.size$SIZE_CD=='6')
LFA30.size6=LFA30.cpue.size[LFA30.6,]
LFA30.size6 = rename.df(LFA30.size6, "Freq", "size6")
LFA30.size6$SIZE_CD=NULL
LFA30.0.6 = merge(LFA30.0.5,LFA30.size6, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA30.7=which(LFA30.cpue.size$SIZE_CD=='7')
LFA30.size7=LFA30.cpue.size[LFA30.7,]
LFA30.size7 = rename.df(LFA30.size7, "Freq", "size7")
LFA30.size7$SIZE_CD=NULL
LFA30.0.7 = merge(LFA30.0.6,LFA30.size7, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA30.8=which(LFA30.cpue.size$SIZE_CD=='8')
LFA30.size8=LFA30.cpue.size[LFA30.8,]
LFA30.size8 = rename.df(LFA30.size8, "Freq", "size8")
LFA30.size8$SIZE_CD=NULL
LFA30.0.8 = merge(LFA30.0.7,LFA30.size8, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA30.9=which(LFA30.cpue.size$SIZE_CD=='9')
LFA30.size9=LFA30.cpue.size[LFA30.9,]
LFA30.size9 = rename.df(LFA30.size9, "Freq", "size9")
LFA30.size9$SIZE_CD=NULL
LFA30.0.9 = merge(LFA30.0.8,LFA30.size9, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA30.10.1=which(LFA30.cpue.size$SIZE_CD=='10.1')
LFA30.size10.1=LFA30.cpue.size[LFA30.10.1,]
LFA30.size10.1 = rename.df(LFA30.size10.1, "Freq", "size10.1")
LFA30.size10.1$SIZE_CD=NULL
LFA30.0.10.1 = merge(LFA30.0.9,LFA30.size10.1, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA30.10.2=which(LFA30.cpue.size$SIZE_CD=='10.2')
LFA30.size10.2=LFA30.cpue.size[LFA30.10.2,]
LFA30.size10.2 = rename.df(LFA30.size10.2, "Freq", "size10.2")
LFA30.size10.2$SIZE_CD=NULL
LFA30.0.10.2 = merge(LFA30.0.10.1,LFA30.size10.2, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA30.11=which(LFA30.cpue.size$SIZE_CD=='11')
LFA30.size11=LFA30.cpue.size[LFA30.11,]
LFA30.size11 = rename.df(LFA30.size11, "Freq", "size11")
LFA30.size11$SIZE_CD=NULL
LFA30.0.11 = merge(LFA30.0.10.2,LFA30.size11, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA30.12=which(LFA30.cpue.size$SIZE_CD=='12')
LFA30.size12=LFA30.cpue.size[LFA30.12,]
LFA30.size12 = rename.df(LFA30.size12, "Freq", "size12")
LFA30.size12$SIZE_CD=NULL
LFA30.0.12 = merge(LFA30.0.11,LFA30.size12, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA30.13=which(LFA30.cpue.size$SIZE_CD=='13')
LFA30.size13=LFA30.cpue.size[LFA30.13,]
LFA30.size13 = rename.df(LFA30.size13, "Freq", "size13")
LFA30.size13$SIZE_CD=NULL
LFA30.0.13 = merge(LFA30.0.12,LFA30.size13, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA30.14=which(LFA30.cpue.size$SIZE_CD=='14')
LFA30.size14=LFA30.cpue.size[LFA30.14,]
LFA30.size14 = rename.df(LFA30.size14, "Freq", "size14")
LFA30.size14$SIZE_CD=NULL
LFA30.0.14 = merge(LFA30.0.13,LFA30.size14, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA30.15=which(LFA30.cpue.size$SIZE_CD=='15')
LFA30.size15=LFA30.cpue.size[LFA30.15,]
LFA30.size15 = rename.df(LFA30.size15, "Freq", "size15")
LFA30.size15$SIZE_CD=NULL
LFA30.0.15 = merge(LFA30.0.14,LFA30.size15, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA30.16=which(LFA30.cpue.size$SIZE_CD=='16')
LFA30.size16=LFA30.cpue.size[LFA30.16,]
LFA30.size16 = rename.df(LFA30.size16, "Freq", "size16")
LFA30.size16$SIZE_CD=NULL
LFA30.0.16 = merge(LFA30.0.15,LFA30.size16, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA30.0.16[is.na(LFA30.0.16)]=0
summary(LFA30.0.16)

##SIZE CLASS SPECS FOR UNDERSIZED LOBSTERS
LFA30.0.16$shorts=NA
LFA30.0.16$shorts=LFA30.0.16$size0+LFA30.0.16$size2+LFA30.0.16$size3+LFA30.0.16$size4+LFA30.0.16$size5+LFA30.0.16$size6+
  LFA30.0.16$size7+LFA30.0.16$size8+LFA30.0.16$size9+LFA30.0.16$size10.1
##SIZE CLASS SPECS FOR LEGAL LOBSTERS
LFA30.0.16$legals=NA
LFA30.0.16$legals=LFA30.0.16$size10.2+LFA30.0.16$size11+LFA30.0.16$size12+LFA30.0.16$size13+
  LFA30.0.16$size14+LFA30.0.16$size15+LFA30.0.16$size16
summary(LFA30.0.16)

#Max shorts = 44; Max legals = 62
write.table(LFA30.0.16, file=paste("rec.LFA30.9916.SD1to5.nober.",Sys.Date(),".txt"), row.names=TRUE, col.names=TRUE, sep=" ")

#----------------------------------LFA31A--------------------------------------
##LFA 31A
LFA31A.9916=which(fsrs$LFA==31.1)
length(LFA31A.9916) #34,688 RECORDS JAN_2016 
LFA31A.9916=fsrs[LFA31A.9916,]
unique(LFA31A.9916$HAUL_YEAR)
summary(LFA31A.9916)

##EMPTY TRAPS
e=which(LFA31A.9916$LOBSTER_NO==0)
length(e)  #1,263 RECORDS

##VALIDATE SOAK DAYS:
> table(LFA31A.9916$SOAK_DAYS, LFA31A.9916$HAUL_YEAR)

  1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015
1  207  254  287  452  718  560  744 1692 1052 2318 3231 2492 2377 2364 2055 2681 2525
2  137  185  115  184  359  539  720  890  741  331  444  430  369  255  452  543  849
3   30    8   35   39  115   82   35   36    0    0  104   59   79   34   45   45   12
4    2    5    4    2   37   24   28   10   45   13    0    0   37   15   52    0    0
5    0    2    6    0    0    0    0    0    0    2    0    0    9    2    0    0    0
6    0    0    9    0    0    0   17    0    0   13    0    0    0    0    0    0    0
7    0    3    4    0    8    0   10    0    0    0    0    0    0    0    0    0    0
8    0    0    0    0    0   14    0    0    0    0    0    0    0    0    0    0    0
9    0    0    0    0    0    0    5    0    0    0    0    0    0    0    0    0    0
>
  
write.table(LFA31A.9916, file=paste("rec.LFA31A 9916.Noremovals.from.datacheck.9916",Sys.Date(),".txt"), row.names=FALSE, col.names=TRUE, sep=" ")

#CPUE PREP FILE
##DAILY AGGREGATE FOR CPUE ANALYSIS
#SOAK DAY CONSTRAINT:
sd=which(LFA31A.9916$SOAK_DAYS > 5)
length(sd)  #83 RECORDS
LFA31A.9916.sd=LFA31A.9916[-sd,]

##CALCULATE TRAP # PER DAY
LFA31A.9916.traps = ddply(LFA31A.9916.sd,c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE"),summarize,TOT_TRAPS=length(unique(TRAP_NO)))
summary(LFA31A.9916.traps)
str(LFA31A.9916.traps) #5,165 RECORDS

#SHOWS NUMBER OF RECORDS WITH TRAP NUMBER DISTRIBUTION PER VESSEL
table(LFA31A.9916.traps$TOT_TRAPS,LFA31A.9916.traps$VESSEL_CD)
table(LFA31A.9916.traps$HAUL_YEAR,LFA31A.9916.traps$VESSEL_CD)

##SIZE CLASS ADJUSTMENT TO SPLIT THE SHORTS AND LEGAL SIZES IN CLASS 10
table(LFA31A.9916.sd$SIZE_CD,LFA31A.9916.sd$SHORT)

e1= which (LFA31A.9916.sd$SIZE_CD==10 & LFA31A.9916.sd$SHORT==1)
LFA31A.9916.sd[e1,"SIZE_CD"]=10.1
summary(LFA31A.9916.sd[e1,])
e2= which (LFA31A.9916.sd$SIZE==10 & LFA31A.9916.sd$SHORT==0)
LFA31A.9916.sd[e2,"SIZE_CD"]=10.2
summary(LFA31A.9916.sd[e2,])

##REMOVE RECORDS FOR BERRIED BY INCLUDING IN SEX COLUMN - FOR CPUE ANALYSIS
b= which (LFA31A.9916.sd$SEX==3)
length (b) #4,305 RECORDS
LFA31A.9916.sd.noberried=LFA31A.9916.sd[-b,]
str(LFA31A.9916.sd.noberried)

##CALCULATE TOTAL COUNT PER SIZE CLASS
LFA31A.9916.sd.noberried$VESSEL_CD <- as.factor(LFA31A.9916.sd.noberried$VESSEL_CD)
LFA31A.9916.sd.noberried$SIZE_CD <- as.factor(LFA31A.9916.sd.noberried$SIZE_CD)

LFA31A.9916.counts.at.size =aggregate(LFA31A.9916.sd.noberried[,1], by=as.list(LFA31A.9916.sd.noberried[c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "SIZE_CD")]), length, simplify = TRUE)
LFA31A.9916.counts.at.size = na.omit (LFA31A.9916.counts.at.size)
library(gdata)
LFA31A.9916.counts.at.size <- rename.vars(LFA31A.9916.counts.at.size, from="x", to="Freq")
summary(LFA31A.9916.counts.at.size)
str(LFA31A.9916.counts.at.size)   #15,546 RECORDS
unique(LFA31A.9916.counts.at.size$SIZE_CD)  #SIZE 0-16; NO NA VALUES

##SIZE CLASS FREQUENCY TABLE
LFA31A.cpue.size = merge(LFA31A.9916.traps,LFA31A.9916.counts.at.size, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE"),all.x=T,all.y=F, sort=F)
LFA31A.0=which(LFA31A.cpue.size$SIZE_CD=='0')
LFA31A.size0=LFA31A.cpue.size[LFA31A.0,]
LFA31A.size0 = rename.df(LFA31A.size0,"Freq", "size0")
LFA31A.size0$SIZE_CD=NULL
LFA31A.2=which(LFA31A.cpue.size$SIZE_CD=='2')
LFA31A.size2=LFA31A.cpue.size[LFA31A.2,]
LFA31A.size2 = rename.df(LFA31A.size2,"Freq", "size2")
LFA31A.size2$SIZE_CD=NULL
LFA31A.0.2 = merge(LFA31A.size0,LFA31A.size2, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31A.3=which(LFA31A.cpue.size$SIZE_CD=='3')
LFA31A.size3=LFA31A.cpue.size[LFA31A.3,]
LFA31A.size3 = rename.df(LFA31A.size3, "Freq", "size3")
LFA31A.size3$SIZE_CD=NULL
LFA31A.0.3 = merge(LFA31A.0.2,LFA31A.size3, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31A.4=which(LFA31A.cpue.size$SIZE_CD=='4')
LFA31A.size4=LFA31A.cpue.size[LFA31A.4,]
LFA31A.size4 = rename.df(LFA31A.size4, "Freq", "size4")
LFA31A.size4$SIZE_CD=NULL
LFA31A.0.4 = merge(LFA31A.0.3,LFA31A.size4, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31A.5=which(LFA31A.cpue.size$SIZE_CD=='5')
LFA31A.size5=LFA31A.cpue.size[LFA31A.5,]
LFA31A.size5 = rename.df(LFA31A.size5, "Freq", "size5")
LFA31A.size5$SIZE_CD=NULL
LFA31A.0.5 = merge(LFA31A.0.4,LFA31A.size5, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31A.6=which(LFA31A.cpue.size$SIZE_CD=='6')
LFA31A.size6=LFA31A.cpue.size[LFA31A.6,]
LFA31A.size6 = rename.df(LFA31A.size6, "Freq", "size6")
LFA31A.size6$SIZE_CD=NULL
LFA31A.0.6 = merge(LFA31A.0.5,LFA31A.size6, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31A.7=which(LFA31A.cpue.size$SIZE_CD=='7')
LFA31A.size7=LFA31A.cpue.size[LFA31A.7,]
LFA31A.size7 = rename.df(LFA31A.size7, "Freq", "size7")
LFA31A.size7$SIZE_CD=NULL
LFA31A.0.7 = merge(LFA31A.0.6,LFA31A.size7, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31A.8=which(LFA31A.cpue.size$SIZE_CD=='8')
LFA31A.size8=LFA31A.cpue.size[LFA31A.8,]
LFA31A.size8 = rename.df(LFA31A.size8, "Freq", "size8")
LFA31A.size8$SIZE_CD=NULL
LFA31A.0.8 = merge(LFA31A.0.7,LFA31A.size8, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31A.9=which(LFA31A.cpue.size$SIZE_CD=='9')
LFA31A.size9=LFA31A.cpue.size[LFA31A.9,]
LFA31A.size9 = rename.df(LFA31A.size9, "Freq", "size9")
LFA31A.size9$SIZE_CD=NULL
LFA31A.0.9 = merge(LFA31A.0.8,LFA31A.size9, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31A.10.1=which(LFA31A.cpue.size$SIZE_CD=='10.1')
LFA31A.size10.1=LFA31A.cpue.size[LFA31A.10.1,]
LFA31A.size10.1 = rename.df(LFA31A.size10.1, "Freq", "size10.1")
LFA31A.size10.1$SIZE_CD=NULL
LFA31A.0.10.1 = merge(LFA31A.0.9,LFA31A.size10.1, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31A.10.2=which(LFA31A.cpue.size$SIZE_CD=='10.2')
LFA31A.size10.2=LFA31A.cpue.size[LFA31A.10.2,]
LFA31A.size10.2 = rename.df(LFA31A.size10.2, "Freq", "size10.2")
LFA31A.size10.2$SIZE_CD=NULL
LFA31A.0.10.2 = merge(LFA31A.0.10.1,LFA31A.size10.2, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31A.11=which(LFA31A.cpue.size$SIZE_CD=='11')
LFA31A.size11=LFA31A.cpue.size[LFA31A.11,]
LFA31A.size11 = rename.df(LFA31A.size11, "Freq", "size11")
LFA31A.size11$SIZE_CD=NULL
LFA31A.0.11 = merge(LFA31A.0.10.2,LFA31A.size11, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31A.12=which(LFA31A.cpue.size$SIZE_CD=='12')
LFA31A.size12=LFA31A.cpue.size[LFA31A.12,]
LFA31A.size12 = rename.df(LFA31A.size12, "Freq", "size12")
LFA31A.size12$SIZE_CD=NULL
LFA31A.0.12 = merge(LFA31A.0.11,LFA31A.size12, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31A.13=which(LFA31A.cpue.size$SIZE_CD=='13')
LFA31A.size13=LFA31A.cpue.size[LFA31A.13,]
LFA31A.size13 = rename.df(LFA31A.size13, "Freq", "size13")
LFA31A.size13$SIZE_CD=NULL
LFA31A.0.13 = merge(LFA31A.0.12,LFA31A.size13, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31A.14=which(LFA31A.cpue.size$SIZE_CD=='14')
LFA31A.size14=LFA31A.cpue.size[LFA31A.14,]
LFA31A.size14 = rename.df(LFA31A.size14, "Freq", "size14")
LFA31A.size14$SIZE_CD=NULL
LFA31A.0.14 = merge(LFA31A.0.13,LFA31A.size14, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31A.15=which(LFA31A.cpue.size$SIZE_CD=='15')
LFA31A.size15=LFA31A.cpue.size[LFA31A.15,]
LFA31A.size15 = rename.df(LFA31A.size15, "Freq", "size15")
LFA31A.size15$SIZE_CD=NULL
LFA31A.0.15 = merge(LFA31A.0.14,LFA31A.size15, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31A.16=which(LFA31A.cpue.size$SIZE_CD=='16')
LFA31A.size16=LFA31A.cpue.size[LFA31A.16,]
LFA31A.size16 = rename.df(LFA31A.size16, "Freq", "size16")
LFA31A.size16$SIZE_CD=NULL
LFA31A.0.16 = merge(LFA31A.0.15,LFA31A.size16, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31A.0.16[is.na(LFA31A.0.16)]=0
summary(LFA31A.0.16)

##SIZE CLASS SPECS FOR UNDERSIZED LOBSTERS
LFA31A.0.16$shorts=NA
LFA31A.0.16$shorts=LFA31A.0.16$size0+LFA31A.0.16$size2+LFA31A.0.16$size3+LFA31A.0.16$size4+LFA31A.0.16$size5+LFA31A.0.16$size6+
  LFA31A.0.16$size7+LFA31A.0.16$size8+LFA31A.0.16$size9+LFA31A.0.16$size10.1
##SIZE CLASS SPECS FOR LEGAL LOBSTERS
LFA31A.0.16$legals=NA
LFA31A.0.16$legals=LFA31A.0.16$size10.2+LFA31A.0.16$size11+LFA31A.0.16$size12+LFA31A.0.16$size13+
  LFA31A.0.16$size14+LFA31A.0.16$size15+LFA31A.0.16$size16
summary(LFA31A.0.16)

#Max shorts = 18; Max legals = 20
write.table(LFA31A.0.16, file=paste("rec.LFA31A.9916.SD1to5.nober.",Sys.Date(),".txt"), row.names=TRUE, col.names=TRUE, sep=" ")

#---------------------------------------LFA31B---------------------------------
#LFA 31B
LFA31B.9916=which(fsrs$LFA==31.2)
length(LFA31B.9916) #46,125 RECORDS JAN_2016
LFA31B.9916=fsrs[LFA31B.9916,]
unique(LFA31B.9916$HAUL_YEAR)
summary(LFA31B.9916)

##EMPTY TRAPS
e=which(LFA31B.9916$LOBSTER_NO==0)
length(e)  #2,410 RECORDS

##VALIDATE SOAK DAYS:
>table(LFA31B.9916$SOAK_DAYS, LFA31B.9916$HAUL_YEAR)

  1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015
1  282  889  856 1446 2590 2235 3035 4118 2700 3283 3741 2132 1925 2908 2310 2930 3709
2  106  237  188  300  392  155  290  629  174  275  193  172  116  121  302  219  222
3   32   36   37   48   40    7  108   57   24   32   44    0   69   86   81   80   15
4    0    6    5   20    0    0    0    0    0    0    0    0    6    0    0   30    0
5    0    0    0   14    0    0   13    0    0    0    0    0    8    0    0    0    0
6    0    0    0    0    3    0    0    0    0    0    0    0   12    0    0    0    0
7    0    6    0    0    0    0    6    0    0    0    0    0    0    0    0    0    6
8    0    0    0    0    0    0    0    0    0    7    0    0    0    0    0    7    0
>
  
write.table(LFA31B.9916, file=paste("rec.LFA31B 9916.Noremovals.from.datacheck.9916",Sys.Date(),".txt"), row.names=FALSE, col.names=TRUE, sep=" ")

#CPUE PREP FILE
##DAILY AGGREGATE FOR CPUE ANALYSIS
#SOAK DAY CONSTRAINT:
sd=which(LFA31B.9916$SOAK_DAYS > 5)
length(sd)  #47 RECORDS
LFA31B.9916.sd=LFA31B.9916[-sd,]

##CALCULATE TRAP # PER DAY
LFA31B.9916.traps = compute.unique.lengths( x=LFA31B.9916.sd, var="TRAP_NO", index=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE"))
LFA31B.9916.traps = na.omit (LFA31B.9916.traps)
LFA31B.9916.traps = rename.df(LFA31B.9916.traps,"Freq", "TOT_TRAPS")
summary(LFA31B.9916.traps)
str(LFA31B.9916.traps)  #8,597 RECORDS

#SHOWS NUMBER OF RECORDS WITH TRAP NUMBER DISTRIBUTION PER VESSEL
table(LFA31B.9916.traps$TOT_TRAPS,LFA31B.9916.traps$VESSEL_CD)
table(LFA31B.9916.traps$HAUL_YEAR,LFA31B.9916.traps$VESSEL_CD)

##SIZE CLASS ADJUSTMENT TO SPLIT THE SHORTS AND LEGAL SIZES IN CLASS 10
table(LFA31B.9916.sd$SIZE_CD,LFA31B.9916.sd$SHORT)

f1= which (LFA31B.9913.sd$SIZE_CD==10 & LFA31B.9913.sd$SHORT==1)
LFA31B.9913.sd[f1,"SIZE_CD"]=10.1
summary(LFA31B.9913.sd[f1,])
f2= which (LFA31B.9913.sd$SIZE_CD==10 & LFA31B.9913.sd$SHORT==0)
LFA31B.9913.sd[f2,"SIZE_CD"]=10.2
summary(LFA31B.9913.sd[f2,])

##REMOVE RECORDS FOR BERRIED BY INCLUDING IN SEX COLUMN - FOR CPUE ANALYSIS
b= which (LFA31B.9913.sd$SEX==3)
length (b) #2798 RECORDS
LFA31B.9913.sd.noberried=LFA31B.9913.sd[-b,]
str(LFA31B.9913.sd.noberried)

##CALCULATE TOTAL COUNT PER SIZE CLASS
LFA31B.9913.sd.noberried$VESSEL_CD <- as.factor(LFA31B.9913.sd.noberried$VESSEL_CD)
LFA31B.9913.sd.noberried$SIZE_CD <- as.factor(LFA31B.9913.sd.noberried$SIZE_CD)

LFA31B.9913.counts.at.size =aggregate(LFA31B.9913.sd.noberried[,1], by=as.list(LFA31B.9913.sd.noberried[c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "SIZE_CD")]), length, simplify = TRUE)
LFA31B.9913.counts.at.size = na.omit (LFA31B.9913.counts.at.size)
library(gdata)
LFA31B.9913.counts.at.size <- rename.vars(LFA31B.9913.counts.at.size, from="x", to="Freq")
summary(LFA31B.9913.counts.at.size)
str(LFA31B.9913.counts.at.size)   #22,001 RECORDS
unique(LFA31B.9913.counts.at.size$SIZE_CD)  #SIZE 0-16; NO NA VALUES

##SIZE CLASS FREQUENCY TABLE
LFA31B.cpue.size = merge(LFA31B.9913.traps,LFA31B.9913.counts.at.size, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE"),all.x=T,all.y=F, sort=F)
LFA31B.0=which(LFA31B.cpue.size$SIZE_CD=='0')
LFA31B.size0=LFA31B.cpue.size[LFA31B.0,]
LFA31B.size0 = rename.df(LFA31B.size0,"Freq", "size0")
LFA31B.size0$SIZE_CD=NULL
LFA31B.2=which(LFA31B.cpue.size$SIZE_CD=='2')
LFA31B.size2=LFA31B.cpue.size[LFA31B.2,]
LFA31B.size2 = rename.df(LFA31B.size2, "Freq", "size2")
LFA31B.size2$SIZE_CD=NULL
LFA31B.0.2 = merge(LFA31B.size0,LFA31B.size2, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31B.3=which(LFA31B.cpue.size$SIZE_CD=='3')
LFA31B.size3=LFA31B.cpue.size[LFA31B.3,]
LFA31B.size3 = rename.df(LFA31B.size3, "Freq", "size3")
LFA31B.size3$SIZE_CD=NULL
LFA31B.0.3 = merge(LFA31B.0.2,LFA31B.size3, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31B.4=which(LFA31B.cpue.size$SIZE_CD=='4')
LFA31B.size4=LFA31B.cpue.size[LFA31B.4,]
LFA31B.size4 = rename.df(LFA31B.size4, "Freq", "size4")
LFA31B.size4$SIZE_CD=NULL
LFA31B.0.4 = merge(LFA31B.0.3,LFA31B.size4, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31B.5=which(LFA31B.cpue.size$SIZE_CD=='5')
LFA31B.size5=LFA31B.cpue.size[LFA31B.5,]
LFA31B.size5 = rename.df(LFA31B.size5, "Freq", "size5")
LFA31B.size5$SIZE_CD=NULL
LFA31B.0.5 = merge(LFA31B.0.4,LFA31B.size5, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31B.6=which(LFA31B.cpue.size$SIZE_CD=='6')
LFA31B.size6=LFA31B.cpue.size[LFA31B.6,]
LFA31B.size6 = rename.df(LFA31B.size6, "Freq", "size6")
LFA31B.size6$SIZE_CD=NULL
LFA31B.0.6 = merge(LFA31B.0.5,LFA31B.size6, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31B.7=which(LFA31B.cpue.size$SIZE_CD=='7')
LFA31B.size7=LFA31B.cpue.size[LFA31B.7,]
LFA31B.size7 = rename.df(LFA31B.size7, "Freq", "size7")
LFA31B.size7$SIZE_CD=NULL
LFA31B.0.7 = merge(LFA31B.0.6,LFA31B.size7, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31B.8=which(LFA31B.cpue.size$SIZE_CD=='8')
LFA31B.size8=LFA31B.cpue.size[LFA31B.8,]
LFA31B.size8 = rename.df(LFA31B.size8, "Freq", "size8")
LFA31B.size8$SIZE_CD=NULL
LFA31B.0.8 = merge(LFA31B.0.7,LFA31B.size8, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31B.9=which(LFA31B.cpue.size$SIZE_CD=='9')
LFA31B.size9=LFA31B.cpue.size[LFA31B.9,]
LFA31B.size9 = rename.df(LFA31B.size9, "Freq", "size9")
LFA31B.size9$SIZE_CD=NULL
LFA31B.0.9 = merge(LFA31B.0.8,LFA31B.size9, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31B.10.1=which(LFA31B.cpue.size$SIZE_CD=='10.1')
LFA31B.size10.1=LFA31B.cpue.size[LFA31B.10.1,]
LFA31B.size10.1 = rename.df(LFA31B.size10.1, "Freq", "size10.1")
LFA31B.size10.1$SIZE_CD=NULL
LFA31B.0.10.1 = merge(LFA31B.0.9,LFA31B.size10.1, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31B.10.2=which(LFA31B.cpue.size$SIZE_CD=='10.2')
LFA31B.size10.2=LFA31B.cpue.size[LFA31B.10.2,]
LFA31B.size10.2 = rename.df(LFA31B.size10.2, "Freq", "size10.2")
LFA31B.size10.2$SIZE_CD=NULL
LFA31B.0.10.2 = merge(LFA31B.0.10.1,LFA31B.size10.2, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31B.11=which(LFA31B.cpue.size$SIZE_CD=='11')
LFA31B.size11=LFA31B.cpue.size[LFA31B.11,]
LFA31B.size11 = rename.df(LFA31B.size11, "Freq", "size11")
LFA31B.size11$SIZE_CD=NULL
LFA31B.0.11 = merge(LFA31B.0.10.2,LFA31B.size11, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31B.12=which(LFA31B.cpue.size$SIZE_CD=='12')
LFA31B.size12=LFA31B.cpue.size[LFA31B.12,]
LFA31B.size12 = rename.df(LFA31B.size12, "Freq", "size12")
LFA31B.size12$SIZE_CD=NULL
LFA31B.0.12 = merge(LFA31B.0.11,LFA31B.size12, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31B.13=which(LFA31B.cpue.size$SIZE_CD=='13')
LFA31B.size13=LFA31B.cpue.size[LFA31B.13,]
LFA31B.size13 = rename.df(LFA31B.size13, "Freq", "size13")
LFA31B.size13$SIZE_CD=NULL
LFA31B.0.13 = merge(LFA31B.0.12,LFA31B.size13, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31B.14=which(LFA31B.cpue.size$SIZE_CD=='14')
LFA31B.size14=LFA31B.cpue.size[LFA31B.14,]
LFA31B.size14 = rename.df(LFA31B.size14, "Freq", "size14")
LFA31B.size14$SIZE_CD=NULL
LFA31B.0.14 = merge(LFA31B.0.13,LFA31B.size14, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31B.15=which(LFA31B.cpue.size$SIZE_CD=='15')
LFA31B.size15=LFA31B.cpue.size[LFA31B.15,]
LFA31B.size15 = rename.df(LFA31B.size15, "Freq", "size15")
LFA31B.size15$SIZE_CD=NULL
LFA31B.0.15 = merge(LFA31B.0.14,LFA31B.size15, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31B.16=which(LFA31B.cpue.size$SIZE_CD=='16')
LFA31B.size16=LFA31B.cpue.size[LFA31B.16,]
LFA31B.size16 = rename.df(LFA31B.size16, "Freq", "size16")
LFA31B.size16$SIZE_CD=NULL
LFA31B.0.16 = merge(LFA31B.0.15,LFA31B.size16, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA31B.0.16[is.na(LFA31B.0.16)]=0
summary(LFA31B.0.16)

##SIZE CLASS SPECS FOR UNDERSIZED LOBSTERS FROM 1999-2013
LFA31B.0.16$shorts=NA
LFA31B.0.16$shorts=LFA31B.0.16$size0+LFA31B.0.16$size2+LFA31B.0.16$size3+LFA31B.0.16$size4+LFA31B.0.16$size5+LFA31B.0.16$size6+
  LFA31B.0.16$size7+LFA31B.0.16$size8+LFA31B.0.16$size9+LFA31B.0.16$size10.1
##SIZE CLASS SPECS FOR LEGAL LOBSTERS FROM 1999-2013
LFA31B.0.16$legals=NA
LFA31B.0.16$legals=LFA31B.0.16$size10.2+LFA31B.0.16$size11+LFA31B.0.16$size12+LFA31B.0.16$size13+
  LFA31B.0.16$size14+LFA31B.0.16$size15+LFA31B.0.16$size16
summary(LFA31B.0.16)

#Max shorts = 15; Max legals = 18
write.table(LFA31B.0.16, file=paste("rec.LFA31B.9913.SD1to5.nober.",Sys.Date(),".txt"), row.names=TRUE, col.names=TRUE, sep=" ")

#-------------------------------------LFA32-------------------------------------
#LFA 32
LFA32.9916=which(fsrs$LFA==32)
length(LFA32.9916) #45,019 RECORDS JAN_2016
LFA32.9916=fsrs[LFA32.9916,]
unique(LFA32.9916$HAUL_YEAR)
summary(LFA32.9916)

##EMPTY TRAPS
e=which(LFA32.9916$LOBSTER_NO==0)
length(e)  #6,253 RECORDS

##VALIDATE SOAK DAYS:
>table(LFA32.9916$SOAK_DAYS, LFA32.9916$HAUL_YEAR)

   1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015
1   854 1238 1158 1276 1715 1707 1945 2950 1594 2605 2885 2432 2522 3184 1972 4051 3152
2   222  337  211  295  213  325  290  376  188  321  431  337  559  518  272  431  492
3    39   36   46   46   46    9   57  102   56   71  121  102   90  146  173  157   49
4    17    1   25   18    2    2   11    4   23   36   37   37   52   37   36   18    2
5     2    1    8    2    2    0   24    4    0    2    0    2   39    7   14    0    0
6     0    0    4    1    2    0   30    2    5   18    0    0    0    0    0   14    4
7     0    0    1    0    2    0   10    4    6    0    0    2   13    0    0    0    0
8     0    0    0    0    2    0    0    2    0    2    0    0   10    0    0    0    0
9     0    2    0    0    2    0    6    0    0    0    0    0    0    0    0    0    0
10    0    0    0    0    3    0    3    0    0    0    0    0    0    0    0    0    0
>
  
write.table(LFA32.9916, file=paste("rec.LFA32 9916.Noremovals.from.datacheck.9916",Sys.Date(),".txt"), row.names=FALSE, col.names=TRUE, sep=" ")

#CPUE PREP FILE
##DAILY AGGREGATE FOR CPUE ANALYSIS
#SOAK DAY CONSTRAINT:
sd=which(LFA32.9916$SOAK_DAYS> 5)
length(sd)  #150 RECORDS
LFA32.9916.sd=LFA32.9916[-sd,]

##CALCULATE TRAP # PER DAY
LFA32.9916.traps = ddply(LFA32.9916.sd,c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE"),summarize,TOT_TRAPS=length(unique(TRAP_NO)))
summary(LFA32.9916.traps)
str(LFA32.9916.traps) #11,772 RECORDS

#SHOWS NUMBER OF RECORDS WITH TRAP NUMBER DISTRIBUTION PER VESSEL
table(LFA32.9916.traps$TOT_TRAPS,LFA32.9916.traps$VESSEL_CD)
table(LFA32.9916.traps$HAUL_YEAR,LFA32.9916.traps$VESSEL_CD)

##SIZE CLASS ADJUSTMENT TO SPLIT THE SHORTS AND LEGAL SIZES IN CLASS 10
table(LFA32.9916.sd$SIZE_CD,LFA32.9916.sd$SHORT)

g1= which (LFA32.9916.sd$SIZE_CD==10 & LFA32.9916.sd$SHORT==1)
LFA32.9916.sd[g1,"SIZE_CD"]=10.1
summary(LFA32.9916.sd[g1,])
g2= which (LFA32.9916.sd$SIZE_CD==10 & LFA32.9916.sd$SHORT==0)
LFA32.9916.sd[g2,"SIZE_CD"]=10.2
summary(LFA32.9916.sd[g2,])

##REMOVE RECORDS FOR BERRIED BY INCLUDING IN SEX COLUMN - FOR CPUE ANALYSIS
b= which (LFA32.9916.sd$SEX==3)
length (b) #1,783 RECORDS
LFA32.9916.sd.noberried=LFA32.9916.sd[-b,]
str(LFA32.9916.sd.noberried) 

##CALCULATE TOTAL COUNT PER SIZE CLASS
LFA32.9916.counts.at.size =aggregate(LFA32.9916.sd.noberried[,1], by=as.list(LFA32.9916.sd.noberried[c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "SIZE_CD")]), length, simplify = TRUE)
LFA32.9916.counts.at.size = na.omit (LFA32.9916.counts.at.size)
library(gdata)
LFA32.9916.counts.at.size <- rename.vars(LFA32.9916.counts.at.size, from="x", to="Freq")
summary(LFA32.9916.counts.at.size)
str(LFA32.9916.counts.at.size)   #24,701 RECORDS
unique(LFA32.9916.counts.at.size$SIZE_CD)  #SIZE 0-16; NO NA VALUES

##SIZE CLASS FREQUENCY TABLE
LFA32.cpue.size = merge(LFA32.9916.traps,LFA32.9916.counts.at.size, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE"),all.x=T,all.y=F, sort=F)
LFA32.0=which(LFA32.cpue.size$SIZE_CD=='0')
LFA32.size0=LFA32.cpue.size[LFA32.0,]
LFA32.size0 = rename.df(LFA32.size0,"Freq", "size0")
LFA32.size0$SIZE_CD=NULL
LFA32.2=which(LFA32.cpue.size$SIZE_CD=='2')
LFA32.size2=LFA32.cpue.size[LFA32.2,]
LFA32.size2 = rename.df(LFA32.size2, "Freq", "size2")
LFA32.size2$SIZE_CD=NULL
LFA32.0.2 = merge(LFA32.size0,LFA32.size2, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA32.3=which(LFA32.cpue.size$SIZE_CD=='3')
LFA32.size3=LFA32.cpue.size[LFA32.3,]
LFA32.size3 = rename.df(LFA32.size3, "Freq", "size3")
LFA32.size3$SIZE_CD=NULL
LFA32.0.3 = merge(LFA32.0.2,LFA32.size3, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA32.4=which(LFA32.cpue.size$SIZE_CD=='4')
LFA32.size4=LFA32.cpue.size[LFA32.4,]
LFA32.size4 = rename.df(LFA32.size4, "Freq", "size4")
LFA32.size4$SIZE_CD=NULL
LFA32.0.4 = merge(LFA32.0.3,LFA32.size4, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA32.5=which(LFA32.cpue.size$SIZE_CD=='5')
LFA32.size5=LFA32.cpue.size[LFA32.5,]
LFA32.size5 = rename.df(LFA32.size5, "Freq", "size5")
LFA32.size5$SIZE_CD=NULL
LFA32.0.5 = merge(LFA32.0.4,LFA32.size5, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA32.6=which(LFA32.cpue.size$SIZE_CD=='6')
LFA32.size6=LFA32.cpue.size[LFA32.6,]
LFA32.size6 = rename.df(LFA32.size6, "Freq", "size6")
LFA32.size6$SIZE_CD=NULL
LFA32.0.6 = merge(LFA32.0.5,LFA32.size6, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA32.7=which(LFA32.cpue.size$SIZE_CD=='7')
LFA32.size7=LFA32.cpue.size[LFA32.7,]
LFA32.size7 = rename.df(LFA32.size7, "Freq", "size7")
LFA32.size7$SIZE_CD=NULL
LFA32.0.7 = merge(LFA32.0.6,LFA32.size7, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA32.8=which(LFA32.cpue.size$SIZE_CD=='8')
LFA32.size8=LFA32.cpue.size[LFA32.8,]
LFA32.size8 = rename.df(LFA32.size8, "Freq", "size8")
LFA32.size8$SIZE_CD=NULL
LFA32.0.8 = merge(LFA32.0.7,LFA32.size8, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA32.9=which(LFA32.cpue.size$SIZE_CD=='9')
LFA32.size9=LFA32.cpue.size[LFA32.9,]
LFA32.size9 = rename.df(LFA32.size9, "Freq", "size9")
LFA32.size9$SIZE_CD=NULL
LFA32.0.9 = merge(LFA32.0.8,LFA32.size9, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA32.10.1=which(LFA32.cpue.size$SIZE_CD=='10.1')
LFA32.size10.1=LFA32.cpue.size[LFA32.10.1,]
LFA32.size10.1 = rename.df(LFA32.size10.1, "Freq", "size10.1")
LFA32.size10.1$SIZE_CD=NULL
LFA32.0.10.1 = merge(LFA32.0.9,LFA32.size10.1, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA32.10.2=which(LFA32.cpue.size$SIZE_CD=='10.2')
LFA32.size10.2=LFA32.cpue.size[LFA32.10.2,]
LFA32.size10.2 = rename.df(LFA32.size10.2, "Freq", "size10.2")
LFA32.size10.2$SIZE_CD=NULL
LFA32.0.10.2 = merge(LFA32.0.10.1,LFA32.size10.2, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA32.11=which(LFA32.cpue.size$SIZE_CD=='11')
LFA32.size11=LFA32.cpue.size[LFA32.11,]
LFA32.size11 = rename.df(LFA32.size11, "Freq", "size11")
LFA32.size11$SIZE_CD=NULL
LFA32.0.11 = merge(LFA32.0.10.2,LFA32.size11, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA32.12=which(LFA32.cpue.size$SIZE_CD=='12')
LFA32.size12=LFA32.cpue.size[LFA32.12,]
LFA32.size12 = rename.df(LFA32.size12, "Freq", "size12")
LFA32.size12$SIZE_CD=NULL
LFA32.0.12 = merge(LFA32.0.11,LFA32.size12, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA32.13=which(LFA32.cpue.size$SIZE_CD=='13')
LFA32.size13=LFA32.cpue.size[LFA32.13,]
LFA32.size13 = rename.df(LFA32.size13, "Freq", "size13")
LFA32.size13$SIZE_CD=NULL
LFA32.0.13 = merge(LFA32.0.12,LFA32.size13, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA32.14=which(LFA32.cpue.size$SIZE_CD=='14')
LFA32.size14=LFA32.cpue.size[LFA32.14,]
LFA32.size14 = rename.df(LFA32.size14, "Freq", "size14")
LFA32.size14$SIZE_CD=NULL
LFA32.0.14 = merge(LFA32.0.13,LFA32.size14, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA32.15=which(LFA32.cpue.size$SIZE_CD=='15')
LFA32.size15=LFA32.cpue.size[LFA32.15,]
LFA32.size15 = rename.df(LFA32.size15, "Freq", "size15")
LFA32.size15$SIZE_CD=NULL
LFA32.0.15 = merge(LFA32.0.14,LFA32.size15, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA32.16=which(LFA32.cpue.size$SIZE_CD=='16')
LFA32.size16=LFA32.cpue.size[LFA32.16,]
LFA32.size16 = rename.df(LFA32.size16, "Freq", "size16")
LFA32.size16$SIZE_CD=NULL
LFA32.0.16 = merge(LFA32.0.15,LFA32.size16, by=c("HAUL_YEAR", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA32.0.16[is.na(LFA32.0.16)]=0
summary(LFA32.0.16)

##SIZE CLASS SPECS FOR UNDERSIZED LOBSTERS FROM 1999-2013
LFA32.0.16$shorts=NA
LFA32.0.16$shorts=LFA32.0.16$size0+LFA32.0.16$size2+LFA32.0.16$size3+LFA32.0.16$size4+LFA32.0.16$size5+LFA32.0.16$size6+
  LFA32.0.16$size7+LFA32.0.16$size8+LFA32.0.16$size9+LFA32.0.16$size10.1
##SIZE CLASS SPECS FOR LEGAL LOBSTERS FROM 1999-2013
LFA32.0.16$legals=NA
LFA32.0.16$legals=LFA32.0.16$size10.2+LFA32.0.16$size11+LFA32.0.16$size12+LFA32.0.16$size13+
  LFA32.0.16$size14+LFA32.0.16$size15+LFA32.0.16$size16
summary(LFA32.0.16)

#Max shorts = 18; Max legals = 13
write.table(LFA32.0.16, file=paste("rec.LFA32.9916.SD1to5.nober.",Sys.Date(),".txt"), row.names=TRUE, col.names=TRUE, sep=" ")

#------------------------------------------LFA33-------------------------------
##LFA 33
LFA33.9916=which(fsrs$LFA==33)
length(LFA33.9916) #279,629 RECORDS JAN_2016
LFA33.9916=fsrs[LFA33.9916,]
summary(LFA33.9916)

##EMPTY TRAPS
e=which(LFA33.9916$LOBSTER_NO==0)
length(e)  #15,085 RECORDS

##VALIDATE SOAK DAYS:
> table(LFA33.9916$SOAK_DAYS,LFA33.9916$S_LABEL) # A PORTION OF RECORDS HAVE TO HAVE LABEL ADDED - THESE ARE PAST NORMAL SEASON DATES
#LFA33.9916$S_LABEL<-recode(LFA33.9916$S_LABEL,"is.na='2004-2005'")

    1998-1999 1999-2000 2000-2001 2001-2002 2002-2003 2003-2004 2004-2005 2005-2006 2006-2007 2007-2008 2008-2009 2009-2010 2010-2011 2011-2012 2012-2013 2013-2014
0.5         0         0         0        38         0         0         0        27        61         0         0         0         0         0         0         0
1        2583      5257      6864      8635      8108      8509     10908     11185     11008      8631     10414     13106     12111     10008     10598     11626
2         978      2682      2376      3144      3283      3190      4023      3875      4075      3200      5275      4503      3838      5452      4663      4108
3         240      1143       961      1308      1064      1349      1407      1667      1323      1329      1767      1861      2059      2083      3004      1637
4          25       461       396       351       347       622       598       811       596       325       681      1421       995      1073       879       609
5          12        92       176       251       270       362       277       425       379       339       489       511      1079       575       420       643
6           2       117        54       176       144       120       233       405       141       180       167       295       279       291       321       206
7           6        29        54        27        50       120       212       218        75        84        68       202       293       197       179       214
8           0        38        14        41        13        43       104       120        86         6       114       104       200        43       144       154
9           0        10        16        16        19       128        64        88        56        18        68        96        84       185       149       146
10          0        10        11        25        10        28        49        21        26        35        67        24        33        41       138        55
11          0         0        12        30         0         0         8        69         4        37        40        52        78        62        51        15
12          0         8         0         0         0         8         0       113        33        20        21        18        43         9        31        16
13          0         0         3         0         0         3         0         1        15         3         7        26         7        27        65        57
14          0         6         0         0         9         9         0        10        10         0        13        36        15         0        13        11
15          0         0         3         5        21        23         7         0         7         7         0        23        44        41        25         0
16          0         0         3         0         0         0         5        12        14        12         0         4         9        18         0        11
17          0         0         0         0         2        10         0         3         0         0         6        11        14        16        55         0
18          0         0         0         0         0         9         0         0         0         0         0        17        21         5        25        12
19          0         0         3         0        22        45         0        27         0         0         0         0         7         0         7         0
20          0         0         0         0         0         0         0         1        13         5        14         0        21         0        12        22
21          0         0         3        14         0         0         0         0         0         0         0         0         0         0         0        27
22          0         0         0         0         0         4         4         0         0         0         0         5         7         5         0        31
23          0         0         0         0         0         0        13         3         0         0         0         0         0         0        20         0
24          0         0         5         0         0         0         0         0         0         0         0         0         0         0         0         0
25          0         0         0         0         0         3         0         0         0         0        10         3         0         0         6         0
26          0         0         0         0         0         0         0         3         0         0        33         0         0         0         0        19
27          0         0         0         0         0         0         0         0         0         0         0        12         0         0         0         0
28          0         0         0         0         0         0         0         0         0         0         0         0        11         9         0         0
29          0         0         0         0         0         0         0         0         0         0         0         5         0         0         0         5
30          0         3         0         0         5         4         0         0         0         0         0         0         0         0         0         0
31          0         0         0         0         0         0         0         0         3         0         0         0         0         0         0         0
32          0         0         0         0         0         0         0         0         0         0         0         0         0         0         7         0
33          0         0         0         0         0         0         0         0         3         0         0         0         0         0         0         0
34          0         0         0         0         0         0         0         0         0         0         3         0         0         0         0         0
35          0         0         0         0         0         0         0         0         9         0         0         0         0         0         0         0
36          0         0         0         0         0         0         0         0         0         0         0         0         0        21         0         0
37          0         0         0         0         0         0         0         0        11         0         0         0         0         0         0         0
38          0         0         0         0         0         0         0         3         0         6         0         0         0         0         0         0
40          0         0         0         0         0         0         0         4         0         0         0         0         0         0         8        12
42          0         0         0         0         0         0         0         0         0         0         0         0         8         0         0         0
43          0         0         0         0         0         5         4         0         0         0         0         0         3         0         0         0
48          0         0         0         0         0         0         0         0         0         0         0         0         0         8         0         0
50          0         0         0         0         0         0         0         0         0         0         0         0        11         0         0         0
57          0         0         0         0         0        20         0         0         0         0         0         0         0         0         0         0
59          0         0         0         0         0         0         0         0         5         0         0         0         0         0         0         0
60          0         0         0         0         0         4         0         0         0         0         0         0         0         0         0         0
64          0         0         0         0         0         3         0         0         0         0         0         0         0         0         0         0
>
write.table(LFA33.9916, file=paste("rec.LFA33 9916.Noremovals.from.datacheck.9916",Sys.Date(),".txt"), row.names=FALSE, col.names=TRUE, sep=" ")

#CPUE PREP FILE:
##DAILY AGGREGATE FOR CPUE ANALYSIS
#SOAK DAY CONSTRAINT:
sd=which(LFA33.9916$SOAK_DAYS > 5)
length(sd)  #10,355 RECORDS
LFA33.9916.sd=LFA33.9916[-sd,]
short.soak.days=which(LFA33.9916.sd$SOAK_DAYS < 1)
length(short.soak.days)  # 126 RECORDS
LFA33.9916.sd=LFA33.9916.sd[-short.soak.days,]

##CALCULATE TRAP # PER DAY
LFA33.9916.traps<-ddply(LFA33.9916.sd,c("S_LABEL", "VESSEL_CD", "HAUL_DATE"),summarize, TOT_TRAPS=length(unique(TRAP_NO))) #27,647 RECORDS
str(LFA33.9916.traps)   #27,647 RECORDS (SD 1 to 5 SELECTION) DEC.2014 SQL EXPORT

#SHOWS NUMBER OF RECORDS WITH TRAP NUMBER DISTRIBUTION PER VESSEL
table(LFA33.9916.traps$S_LABEL,LFA33.9916.traps$TOT_TRAPS)
table(LFA33.9916.traps$S_LABEL,LFA33.9916.traps$VESSEL_CD)

##SIZE CLASS ADJUSTMENT TO SPLIT THE SHORTS AND LEGAL SIZES IN CLASS 10
table(LFA33.9916.sd$SIZE_CD, LFA33.9916.sd$SHORT)

h1= which (LFA33.9916.sd$SIZE_CD==10 & LFA33.9916.sd$SHORT==1)
LFA33.9916.sd[h1,"SIZE_CD"]=10.1
summary(LFA33.9916.sd[h1,])
h2= which (LFA33.9916.sd$SIZE_CD==10 & LFA33.9916.sd$SHORT==0)
LFA33.9916.sd[h2,"SIZE_CD"]=10.2
summary(LFA33.9916.sd[h2,])
str(LFA33.9916.sd)

##REMOVE RECORDS FOR BERRIED BY INCLUDING IN SEX COLUMN - FOR CPUE ANALYSIS
b= which (LFA33.9916.sd$SEX==3)
length (b) #3861 RECORDS
LFA33.9916.sd.noberried=LFA33.9916.sd[-b,]
str(LFA33.9916.sd.noberried)

##CALCULATE TOTAL COUNT PER SIZE CLASS
LFA33.9916.counts.at.size =aggregate(LFA33.9916.sd.noberried[,1], by=as.list(LFA33.9916.sd.noberried[c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "SIZE_CD")]), length, simplify = TRUE)
LFA33.9916.counts.at.size = na.omit (LFA33.9916.counts.at.size)
library(gdata)
LFA33.9916.counts.at.size <- rename.vars(LFA33.9916.counts.at.size, from="x", to="Freq")
summary(LFA33.9916.counts.at.size)
str(LFA33.9916.counts.at.size)   #100,527 RECORDS
unique(LFA33.9916.counts.at.size$SIZE_CD)  #SIZE 0-16; NO NA VALUES

##SIZE CLASS FREQUENCY TABLE
LFA33.cpue.size = merge(LFA33.9916.traps,LFA33.9916.counts.at.size, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE"),all.x=T,all.y=F, sort=F)
LFA33.0=which(LFA33.cpue.size$SIZE_CD=='0')
LFA33.size0=LFA33.cpue.size[LFA33.0,]
LFA33.size0 = rename.df(LFA33.size0,"Freq", "size0")
LFA33.size0$SIZE_CD=NULL
LFA33.2=which(LFA33.cpue.size$SIZE_CD=='2')
LFA33.size2=LFA33.cpue.size[LFA33.2,]
LFA33.size2 = rename.df(LFA33.size2, "Freq", "size2")
LFA33.size2$SIZE_CD=NULL
LFA33.0.2 = merge(LFA33.size0,LFA33.size2, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA33.3=which(LFA33.cpue.size$SIZE_CD=='3')
LFA33.size3=LFA33.cpue.size[LFA33.3,]
LFA33.size3 = rename.df(LFA33.size3, "Freq", "size3")
LFA33.size3$SIZE_CD=NULL
LFA33.0.3 = merge(LFA33.0.2,LFA33.size3, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA33.4=which(LFA33.cpue.size$SIZE_CD=='4')
LFA33.size4=LFA33.cpue.size[LFA33.4,]
LFA33.size4 = rename.df(LFA33.size4, "Freq", "size4")
LFA33.size4$SIZE_CD=NULL
LFA33.0.4 = merge(LFA33.0.3,LFA33.size4, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA33.5=which(LFA33.cpue.size$SIZE_CD=='5')
LFA33.size5=LFA33.cpue.size[LFA33.5,]
LFA33.size5 = rename.df(LFA33.size5, "Freq", "size5")
LFA33.size5$SIZE_CD=NULL
LFA33.0.5 = merge(LFA33.0.4,LFA33.size5, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA33.6=which(LFA33.cpue.size$SIZE_CD=='6')
LFA33.size6=LFA33.cpue.size[LFA33.6,]
LFA33.size6 = rename.df(LFA33.size6, "Freq", "size6")
LFA33.size6$SIZE_CD=NULL
LFA33.0.6 = merge(LFA33.0.5,LFA33.size6, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA33.7=which(LFA33.cpue.size$SIZE_CD=='7')
LFA33.size7=LFA33.cpue.size[LFA33.7,]
LFA33.size7 = rename.df(LFA33.size7, "Freq", "size7")
LFA33.size7$SIZE_CD=NULL
LFA33.0.7 = merge(LFA33.0.6,LFA33.size7, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA33.8=which(LFA33.cpue.size$SIZE_CD=='8')
LFA33.size8=LFA33.cpue.size[LFA33.8,]
LFA33.size8 = rename.df(LFA33.size8, "Freq", "size8")
LFA33.size8$SIZE_CD=NULL
LFA33.0.8 = merge(LFA33.0.7,LFA33.size8, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA33.9=which(LFA33.cpue.size$SIZE_CD=='9')
LFA33.size9=LFA33.cpue.size[LFA33.9,]
LFA33.size9 = rename.df(LFA33.size9, "Freq", "size9")
LFA33.size9$SIZE_CD=NULL
LFA33.0.9 = merge(LFA33.0.8,LFA33.size9, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA33.10.1=which(LFA33.cpue.size$SIZE_CD=='10.1')
LFA33.size10.1=LFA33.cpue.size[LFA33.10.1,]
LFA33.size10.1 = rename.df(LFA33.size10.1, "Freq", "size10.1")
LFA33.size10.1$SIZE_CD=NULL
LFA33.0.10.1 = merge(LFA33.0.9,LFA33.size10.1, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA33.10.2=which(LFA33.cpue.size$SIZE_CD=='10.2')
LFA33.size10.2=LFA33.cpue.size[LFA33.10.2,]
LFA33.size10.2 = rename.df(LFA33.size10.2, "Freq", "size10.2")
LFA33.size10.2$SIZE_CD=NULL
LFA33.0.10.2 = merge(LFA33.0.10.1,LFA33.size10.2, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA33.11=which(LFA33.cpue.size$SIZE_CD=='11')
LFA33.size11=LFA33.cpue.size[LFA33.11,]
LFA33.size11 = rename.df(LFA33.size11, "Freq", "size11")
LFA33.size11$SIZE_CD=NULL
LFA33.0.11 = merge(LFA33.0.10.2,LFA33.size11, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA33.12=which(LFA33.cpue.size$SIZE_CD=='12')
LFA33.size12=LFA33.cpue.size[LFA33.12,]
LFA33.size12 = rename.df(LFA33.size12, "Freq", "size12")
LFA33.size12$SIZE_CD=NULL
LFA33.0.12 = merge(LFA33.0.11,LFA33.size12, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA33.13=which(LFA33.cpue.size$SIZE_CD=='13')
LFA33.size13=LFA33.cpue.size[LFA33.13,]
LFA33.size13 = rename.df(LFA33.size13, "Freq", "size13")
LFA33.size13$SIZE_CD=NULL
LFA33.0.13 = merge(LFA33.0.12,LFA33.size13, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA33.14=which(LFA33.cpue.size$SIZE_CD=='14')
LFA33.size14=LFA33.cpue.size[LFA33.14,]
LFA33.size14 = rename.df(LFA33.size14, "Freq", "size14")
LFA33.size14$SIZE_CD=NULL
LFA33.0.14 = merge(LFA33.0.13,LFA33.size14, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA33.15=which(LFA33.cpue.size$SIZE_CD=='15')
LFA33.size15=LFA33.cpue.size[LFA33.15,]
LFA33.size15 = rename.df(LFA33.size15, "Freq", "size15")
LFA33.size15$SIZE_CD=NULL
LFA33.0.15 = merge(LFA33.0.14,LFA33.size15, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA33.16=which(LFA33.cpue.size$SIZE_CD=='16')
LFA33.size16=LFA33.cpue.size[LFA33.16,]
LFA33.size16 = rename.df(LFA33.size16, "Freq", "size16")
LFA33.size16$SIZE_CD=NULL
LFA33.0.16 = merge(LFA33.0.15,LFA33.size16, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA33.0.16[is.na(LFA33.0.16)]=0
summary(LFA33.0.16)

##SIZE CLASS SPECS FOR UNDERSIZED LOBSTERS FROM 1999-2013
LFA33.0.16$shorts=NA
LFA33.0.16$shorts=LFA33.0.16$size0+LFA33.0.16$size2+LFA33.0.16$size3+LFA33.0.16$size4+LFA33.0.16$size5+LFA33.0.16$size6+
  LFA33.0.16$size7+LFA33.0.16$size8+LFA33.0.16$size9+LFA33.0.16$size10.1
##SIZE CLASS SPECS FOR LEGAL LOBSTERS FROM 1999-2013
LFA33.0.16$legals=NA
LFA33.0.16$legals=LFA33.0.16$size10.2+LFA33.0.16$size11+LFA33.0.16$size12+LFA33.0.16$size13+
  LFA33.0.16$size14+LFA33.0.16$size15+LFA33.0.16$size16
summary(LFA33.0.16)

#Max shorts = 95; Max legals = 33
write.table(LFA33.0.16, file=paste("rec.LFA33.9916.SD1to5.nober.",Sys.Date(),".txt"), row.names=TRUE, col.names=TRUE, sep=" ")

#------------------------------------------LFA34-------------------------------
##LFA 34
LFA34.9916=which(fsrs$LFA==34)
length(LFA34.9916) #308,726 RECORDS DEC_2014
LFA34.9916=fsrs[LFA34.9916,]
summary(LFA34.9916)

##EMPTY TRAPS
e=which(LFA34.9916$LOBSTER_NO==0)
length(e)  #5129 RECORDS

##VALIDATE SOAK DAYS:
LFA34.9916$S_LABEL<-as.character(LFA34.9916$S_LABEL)
#out.of.season<-subset(LFA34.9916,S_LABEL=="")
#LFA34.9916$S_LABEL<-recode(LFA34.9916$S_LABEL,"''='2004-2005'")

> table(LFA34.9916$SOAK_DAYS,LFA34.9916$S_LABEL)# A PORTION OF RECORDS HAVE TO HAVE LABEL ADDED - THESE ARE PAST NORMAL SEASON DATES


    1998-1999 1999-2000 2000-2001 2001-2002 2002-2003 2003-2004 2004-2005 2005-2006 2006-2007 2007-2008 2008-2009 2009-2010 2010-2011 2011-2012 2012-2013 2013-2014
0.5         0         0         0       129         0         0         0        27        78        13         0         0         0         0         0         0
1         197      3697      8964     12911     11168      7130     10171     10786     10222      6303      6117      7413      6611      8077      5305      4974
2         202      3541      7537      8088      7853      5069      8490     11545      6187      6168      6020      6331      4143      4774      5177      2536
3          37      2191      2481      3856      3683      3177      2833      4807      1727      1957      2030      1953      2105      2369      3242      1630
4           7       705      1237      1776      1803      1464      1128      2431       993      1053      1134      1559      1308      1136      1010       481
5           9       227       698       809       859       559       598      1348       914       861       454       637       749       682       864       525
6           0       192       443       571       533       532       635       717       300       200       361       273       398       379       631       222
7           0        69       462       271       108       616       641       785       142       294       284       204       331       299       319       169
8           0        52       194       374       216       236       268       210        97       121       169       112       247       201       228       148
9           0       105       123        33       167       158        90       230        61        31        32        31        82       280       323        41
10          0        39        75       267        57        62        53       340        40        55        79        10        94       167       287        76
11          0        53        78        42       172        17        99        97        57        30        12        47        40        83       191        18
12          0        42        41        98        39        62        10       110        47        59         4        85        34       131       105        68
13          0        36        55        59         3        52        73        77         4        26        13        27        14        35        34        21
14          0        14        15        13        34       104        21       116        66        13        28        26         5        49        51         4
15          0         2        31        17        66        88        57        28        34        18        12         2        20        11         2        33
16          0        37        43        19         3        34        46        10        15        11         2         0         4        33         7        24
17          0        11        10         0        34         6         0         3        52        13         9         2         5        18        53        31
18          0         0        20        12        33         0         2         8        15        17         4         9        29        38        11         9
19          0         7        17         7        43         6        22         8        15         0         9        30         4         9        14        13
20          0         0        11         8        35         9        14         5        13         2         8        13         0         0         0        34
21          0         3        15         0         4         4         9         0         7        11         7         0         4         0         0         8
22          0         3        13         5         7         0         3         6         3         0         0         0         5         0         0         0
23          0         0         9         4         2        21         5         0         0         3         0         0        23         0         0         6
24          0         4         0         0        12        34        10         0         2         2         2         0         0         2         0         0
25          0         4         0         0         8        16         4         0         0         0         2         5         2         0         0         0
26          0         0         0         0         3         0         0         0        24         0         0         0         8         3         0         0
27          0         8         0         7        19         0         0         0         0         4         0        15         0         4        19         0
28          0         5         7         0        11        38         0         0         0         3         0         0         0         0        15         7
29          0         0         0         0         0        12         0         0        17         0         0         0         0         0         0         0
30          0         0         4         0        23         9        32        24         2         0         2         6         2         0        26         0
31          0         0         0         0         8         0         2         0         2         0         2         0         0         0         0         0
32          0         0         0         0         0         0         0         0         0         7        13         0         7         0         0         0
33          0         0         0         8         0         0         0         0         2         0         0         0         0         0         0         0
34          0         0         0         0         8         0         0         3         0         0         0         0         0         0         0         0
35          0         0         2         3         7        12         0         0        17         0         2         0         3         0         8         0
36          0         0         0         0         0        10         0         0         4         0         2         0         0         0         0         0
37          0         0         0         0         0         2         9         0         0         0         0         0         0         0         0         0
38          0         0         0         0        23         3        16         0         0         0         0         0        12         0         0         0
39          0         0         0        39         0        11         0         0         0         0         0         0         0         0         0         0
40          0         0         0         0         0        16         2         0         3         0         0         0         0         0        11        33
41          0         0         0         0         0        11         0         0         0         0         0         0         0         0         0         0
42          0         0         0         0         0         0         0         0         0         0         6         0         0         0         0         0
43          0         0         0         0         0         0         0         2         0         0         0         0         0         0         0         0
44          0         0         8         0         7         0         0         0         0         0         2         0         0         0         0         0
45          0         0         0         0         0         0         3         0         0         0         0         3         0         0         0         0
46          0        15         0         0         0         0         0         0         0         0         0         0         0         0         0         0
47          0         0         0         0         0         0         4         0         0         0         0         0         0         0         0         0
48          0         0         5         0         0         5         0         0         0         0         0         0         0         0         0         0
50          0         4         0         0         0         0         0         0         0         0         0        12         9         0         0         0
52          0         0         0         0         3         0         0         0         0         0         0         0         0         0         0         0
53          0         0         0         0         0         2         0         0         0         0         0         0         0         0         0         0
54          0         0         0        12         0         0         0         0         0         0         0         0         0         0         0         0
55          0         0         0         0         0         0         0        14         0         0         0         0         0         0         0         0
56          0         0         0         2         0         0         0         0         0         0         0         0         0         0         0         0
57          0         0         0         0         0         0         0         0         0         0         5         0         0         0         0         0
59          0         0        16         4         0         0         0         0         0         0         0         0         0         0         0         0
60          0         0         0         0         0         0         0         2         0         0         0         0         0         0         0         0
61          0         0         0         0         8         0         0         0         0         0         0         0         0         0         0         0
62          0         0         0         0         0         0         0         0         0         0         2         0         0         0         0         0
65          0         0         0         0         2         0         0         0         0         0         7         0         0         0         0         0
68          0         0         0         0        13         0         0         7         0         0         0         0         0         0         0         0
70          0         0         0         0        10         0         0        14         0         0        19         0         0         0         0         0
72          0         0         0         0         2         0         0         0         0         0         0         0         0         0         0         0
79          0         0         0         0         0         0         0         0        21         0         0         0         0         0         0         0
80          0         0         0         0         0         2         0         0         0         0         0         0         0         0         0         0
83          0         0         0         0        18         0         0         0         0         0         0         0         0         0         0         0
84          0         0         0         0         2         0         0         0         0         0         0         0         0         0         0         0
90          0         0         0         0         0         9         0         0         0         0         0         0         0         0         0         0
92          0         0         0         0         7         0         0         0         0         0         0         0         0         0         0         0
>  
write.table(LFA34.9916, file=paste("rec.LFA34 9916.Noremovals.from.datacheck.9916",Sys.Date(),".txt"), row.names=FALSE, col.names=TRUE, sep=" ")

#CPUE PREP FILE
##DAILY AGGREGATE FOR CPUE ANALYSIS:
#SOAK DAY CONSTRAINT:
sd=which(LFA34.9916$SOAK_DAYS > 5)
length(sd)  #22,598 RECORDS
LFA34.9916.sd=LFA34.9916[-sd,]
short.soak.days=which(LFA34.9916.sd$SOAK_DAYS < 1)
length(short.soak.days)  #247 RECORDS
LFA34.9916.sd=LFA34.9916.sd[-short.soak.days,]

##CALCULATE TRAP # PER DAY
LFA34.9913.traps = compute.unique.lengths( x=LFA34.9913.sd, var="TRAP_NO", index=c("S_LABEL", "VESSEL_CD", "HAUL_DATE")  )
LFA34.9913.traps = na.omit (LFA34.9913.traps)
LFA34.9913.traps = rename.df(LFA34.9913.traps,"Freq", "TOT_TRAPS")
summary(LFA34.9913.traps)
str(LFA34.9913.traps)    #24,819 RECORDS

#SHOWS NUMBER OF RECORDS WITH TRAP NUMBER DISTRIBUTION PER VESSEL
table(LFA34.9913.traps$TOT_TRAPS,LFA34.9913.traps$VESSEL_CD)

##SIZE CLASS ADJUSTMENT TO SPLIT THE SHORTS AND LEGAL SIZES IN CLASS 10
table(LFA34.9916.sd$SIZE_CD,LFA34.9916.sd$SHORT)

i1= which (LFA34.9913.sd$SIZE_CD==10 & LFA34.9913.sd$SHORT==1)
LFA34.9913.sd[i1,"SIZE_CD"]=10.1
summary(LFA34.9913.sd[i1,])
i2= which (LFA34.9913.sd$SIZE_CD==10 & LFA34.9913.sd$SHORT==0)
LFA34.9913.sd[i2,"SIZE_CD"]=10.2
summary(LFA34.9913.sd[i2,])
str(LFA34.9913.sd)

##REMOVE RECORDS FOR BERRIED BY INCLUDING IN SEX COLUMN - FOR CPUE ANALYSIS
b= which (LFA34.9913.sd$SEX==3)
length (b) #1631 RECORDS
LFA34.9913.sd.noberried=LFA34.9913.sd[-b,]
str(LFA34.9913.sd.noberried)

##CALCULATE TOTAL COUNT PER SIZE CLASS
LFA34.9913.sd.noberried$VESSEL_CD <- as.factor(LFA34.9913.sd.noberried$VESSEL_CD)
LFA34.9913.sd.noberried$SIZE_CD <- as.factor(LFA34.9913.sd.noberried$SIZE_CD)

LFA34.9913.counts.at.size =aggregate(LFA34.9913.sd.noberried[,1], by=as.list(LFA34.9913.sd.noberried[c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "SIZE_CD")]), length, simplify = TRUE)
LFA34.9913.counts.at.size = na.omit (LFA34.9913.counts.at.size)
library(gdata)
LFA34.9913.counts.at.size <- rename.vars(LFA34.9913.counts.at.size, from="x", to="Freq")
summary(LFA34.9913.counts.at.size)
str(LFA34.9913.counts.at.size)   #104,705 RECORDS
unique(LFA34.9913.counts.at.size$SIZE_CD)  #SIZE 0-16; NO NA VALUES


##SIZE CLASS FREQUENCY TABLE
LFA34.cpue.size = merge(LFA34.9913.traps,LFA34.9913.counts.at.size, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE"),all.x=T,all.y=F, sort=F)
LFA34.0=which(LFA34.cpue.size$SIZE_CD=='0')
LFA34.size0=LFA34.cpue.size[LFA34.0,]
LFA34.size0 = rename.df(LFA34.size0,"Freq", "size0")
LFA34.size0$SIZE_CD=NULL
LFA34.2=which(LFA34.cpue.size$SIZE_CD=='2')
LFA34.size2=LFA34.cpue.size[LFA34.2,]
LFA34.size2 = rename.df(LFA34.size2, "Freq", "size2")
LFA34.size2$SIZE_CD=NULL
LFA34.0.2 = merge(LFA34.size0,LFA34.size2, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA34.3=which(LFA34.cpue.size$SIZE_CD=='3')
LFA34.size3=LFA34.cpue.size[LFA34.3,]
LFA34.size3 = rename.df(LFA34.size3, "Freq", "size3")
LFA34.size3$SIZE_CD=NULL
LFA34.0.3 = merge(LFA34.0.2,LFA34.size3, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA34.4=which(LFA34.cpue.size$SIZE_CD=='4')
LFA34.size4=LFA34.cpue.size[LFA34.4,]
LFA34.size4 = rename.df(LFA34.size4, "Freq", "size4")
LFA34.size4$SIZE_CD=NULL
LFA34.0.4 = merge(LFA34.0.3,LFA34.size4, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA34.5=which(LFA34.cpue.size$SIZE_CD=='5')
LFA34.size5=LFA34.cpue.size[LFA34.5,]
LFA34.size5 = rename.df(LFA34.size5, "Freq", "size5")
LFA34.size5$SIZE_CD=NULL
LFA34.0.5 = merge(LFA34.0.4,LFA34.size5, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA34.6=which(LFA34.cpue.size$SIZE_CD=='6')
LFA34.size6=LFA34.cpue.size[LFA34.6,]
LFA34.size6 = rename.df(LFA34.size6, "Freq", "size6")
LFA34.size6$SIZE_CD=NULL
LFA34.0.6 = merge(LFA34.0.5,LFA34.size6, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA34.7=which(LFA34.cpue.size$SIZE_CD=='7')
LFA34.size7=LFA34.cpue.size[LFA34.7,]
LFA34.size7 = rename.df(LFA34.size7, "Freq", "size7")
LFA34.size7$SIZE_CD=NULL
LFA34.0.7 = merge(LFA34.0.6,LFA34.size7, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA34.8=which(LFA34.cpue.size$SIZE_CD=='8')
LFA34.size8=LFA34.cpue.size[LFA34.8,]
LFA34.size8 = rename.df(LFA34.size8, "Freq", "size8")
LFA34.size8$SIZE_CD=NULL
LFA34.0.8 = merge(LFA34.0.7,LFA34.size8, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA34.9=which(LFA34.cpue.size$SIZE_CD=='9')
LFA34.size9=LFA34.cpue.size[LFA34.9,]
LFA34.size9 = rename.df(LFA34.size9, "Freq", "size9")
LFA34.size9$SIZE_CD=NULL
LFA34.0.9 = merge(LFA34.0.8,LFA34.size9, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA34.10.1=which(LFA34.cpue.size$SIZE_CD=='10.1')
LFA34.size10.1=LFA34.cpue.size[LFA34.10.1,]
LFA34.size10.1 = rename.df(LFA34.size10.1, "Freq", "size10.1")
LFA34.size10.1$SIZE_CD=NULL
LFA34.0.10.1 = merge(LFA34.0.9,LFA34.size10.1, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA34.10.2=which(LFA34.cpue.size$SIZE_CD=='10.2')
LFA34.size10.2=LFA34.cpue.size[LFA34.10.2,]
LFA34.size10.2 = rename.df(LFA34.size10.2, "Freq", "size10.2")
LFA34.size10.2$SIZE_CD=NULL
LFA34.0.10.2 = merge(LFA34.0.10.1,LFA34.size10.2, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA34.11=which(LFA34.cpue.size$SIZE_CD=='11')
LFA34.size11=LFA34.cpue.size[LFA34.11,]
LFA34.size11 = rename.df(LFA34.size11, "Freq", "size11")
LFA34.size11$SIZE_CD=NULL
LFA34.0.11 = merge(LFA34.0.10.2,LFA34.size11, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA34.12=which(LFA34.cpue.size$SIZE_CD=='12')
LFA34.size12=LFA34.cpue.size[LFA34.12,]
LFA34.size12 = rename.df(LFA34.size12, "Freq", "size12")
LFA34.size12$SIZE_CD=NULL
LFA34.0.12 = merge(LFA34.0.11,LFA34.size12, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA34.13=which(LFA34.cpue.size$SIZE_CD=='13')
LFA34.size13=LFA34.cpue.size[LFA34.13,]
LFA34.size13 = rename.df(LFA34.size13, "Freq", "size13")
LFA34.size13$SIZE_CD=NULL
LFA34.0.13 = merge(LFA34.0.12,LFA34.size13, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA34.14=which(LFA34.cpue.size$SIZE_CD=='14')
LFA34.size14=LFA34.cpue.size[LFA34.14,]
LFA34.size14 = rename.df(LFA34.size14, "Freq", "size14")
LFA34.size14$SIZE_CD=NULL
LFA34.0.14 = merge(LFA34.0.13,LFA34.size14, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA34.15=which(LFA34.cpue.size$SIZE_CD=='15')
LFA34.size15=LFA34.cpue.size[LFA34.15,]
LFA34.size15 = rename.df(LFA34.size15, "Freq", "size15")
LFA34.size15$SIZE_CD=NULL
LFA34.0.15 = merge(LFA34.0.14,LFA34.size15, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA34.16=which(LFA34.cpue.size$SIZE_CD=='16')
LFA34.size16=LFA34.cpue.size[LFA34.16,]
LFA34.size16 = rename.df(LFA34.size16, "Freq", "size16")
LFA34.size16$SIZE_CD=NULL
LFA34.0.16 = merge(LFA34.0.15,LFA34.size16, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA34.0.16[is.na(LFA34.0.16)]=0
summary(LFA34.0.16)

##SIZE CLASS SPECS FOR UNDERSIZED LOBSTERS FROM 1999-2013
LFA34.0.16$shorts=NA
LFA34.0.16$shorts=LFA34.0.16$size0+LFA34.0.16$size2+LFA34.0.16$size3+LFA34.0.16$size4+LFA34.0.16$size5+LFA34.0.16$size6+
  LFA34.0.16$size7+LFA34.0.16$size8+LFA34.0.16$size9+LFA34.0.16$size10.1
##SIZE CLASS SPECS FOR LEGAL LOBSTERS FROM 1999-2013
LFA34.0.16$legals=NA
LFA34.0.16$legals=LFA34.0.16$size10.2+LFA34.0.16$size11+LFA34.0.16$size12+LFA34.0.16$size13+
  LFA34.0.16$size14+LFA34.0.16$size15+LFA34.0.16$size16
summary(LFA34.0.16)

#Max shorts = 146; Max legals = 35
write.table(LFA34.0.16, file=paste("rec.LFA34.9913.SD1to5.nober.",Sys.Date(),".txt"), row.names=TRUE, col.names=TRUE, sep=" ")

#------------------------------------------LFA35-------------------------------
##LFA 35
LFA35.9916=which(fsrs$LFA==35)
length(LFA35.9916) #58,406 RECORDS MAY_2014
LFA35.9916=fsrs[LFA35.9916,]
summary(LFA35.9916)

##EMPTY TRAPS
e=which(LFA35.9913$LOBSTER_NO==0)
length(e)  #505 RECORDS

##VALIDATE SOAK DAYS:
LFA35.9916$S_LABEL<-as.character(LFA35.9916$S_LABEL)
> table(LFA35.9916$SOAK_DAYS,LFA35.9916$S_LABEL)

   2007 2008 2009 2010 2011 2012 2013
1  1262  413 1438  886 1061 1916 1974
2  1480 1021 2390 1691 3237 2749 1862
3   661  355 3117 2878 2267 2804 1527
4   420   76 1435 1874 1290 1872  853
5   250   57 1233  974 1361  789  832
6   139   71  496  509  569  467  284
7     9    0  696  444  436  416  270
8     0    0   78  132  234  189  228
9    22    0   29  160  256  124  165
10   80    6  168   49   91  119   21
11    7    0    0   51   12   18    8
12    0    0    0   36   58   25    0
13    0    0    0    0    0   29    0
14    0   23   79    0   23    0    7
15    0    0   25   22    0    0   20
16    0    0    0    0   29   38    0
17   12    0    0    0    0    0    0
18    0    0    0    0    9    0    0
19    0    0    0   36    0    0    0
30    0    0    0    0    0    0   13
36    0    0   16    0    0    0    0
> 
  
write.table(LFA35.9913f, file=paste("rec.LFA35 9913.Noremovals.from.datacheck.9913",Sys.Date(),".txt"), row.names=FALSE, col.names=TRUE, sep=" ")

#CPUE PREP FILE
#DAILY AGGREGATE FOR CPUE ANALYSIS
#SOAK DAY CONSTRAINT:
sd=which(LFA35.9913$SOAK_DAYS > 5)
length(sd)  # 7584 RECORDS
LFA35.9913.sd=LFA35.9913[-sd,]

##CALCULATE TRAP # PER DAY
LFA35.9913.traps = compute.unique.lengths( x=LFA35.9913.sd, var="TRAP_NO", index=c("S_LABEL", "VESSEL_CD", "HAUL_DATE")  )
LFA35.9913.traps = na.omit (LFA35.9913.traps)
LFA35.9913.traps = rename.df(LFA35.9913.traps,"Freq", "TOT_TRAPS")
summary(LFA35.9913.traps)
str(LFA35.9913.traps)    # 1960 RECORDS

#SHOWS NUMBER OF RECORDS WITH TRAP NUMBER DISTRIBUTION PER VESSEL
table(LFA35.9913.traps$TOT_TRAPS,LFA35.9913.traps$VESSEL_CD)

##SIZE CLASS ADJUSTMENT TO SPLIT THE SHORTS AND LEGAL SIZES IN CLASS 10
table(LFA35.9913.sd$SIZE_CD,LFA35.9913.sd$SHORT)

j1= which (LFA35.9913.sd$SIZE_CD==10 & LFA35.9913.sd$SHORT==1)
LFA35.9913.sd[j1,"SIZE_CD"]=10.1
summary(LFA35.9913.sd[j1,])
j2= which (LFA35.9913.sd$SIZE_CD==10 & LFA35.9913.sd$SHORT==0)
LFA35.9913.sd[j2,"SIZE_CD"]=10.2
summary(LFA35.9913.sd[j2,])
str(LFA35.9913.sd)

##REMOVE RECORDS FOR BERRIED BY INCLUDING IN SEX COLUMN - FOR CPUE ANALYSIS
b= which (LFA35.9913.sd$SEX==3)
length (b) #521 RECORDS
LFA35.9913.sd.noberried=LFA35.9913.sd[-b,]
str(LFA35.9913.sd.noberried)

##CALCULATE TOTAL COUNT PER SIZE CLASS
LFA35.9913.sd.noberried$VESSEL_CD <- as.factor(LFA35.9913.sd.noberried$VESSEL_CD)
LFA35.9913.sd.noberried$SIZE_CD <- as.factor(LFA35.9913.sd.noberried$SIZE_CD)

LFA35.9913.counts.at.size =aggregate(LFA35.9913.sd.noberried[,1], by=as.list(LFA35.9913.sd.noberried[c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "SIZE_CD")]), length, simplify = TRUE)
LFA35.9913.counts.at.size = na.omit (LFA35.9913.counts.at.size)
library(gdata)
LFA35.9913.counts.at.size <- rename.vars(LFA35.9913.counts.at.size, from="x", to="Freq")
summary(LFA35.9913.counts.at.size)
str(LFA35.9913.counts.at.size)   #12,982 RECORDS
unique(LFA35.9913.counts.at.size$SIZE_CD)  #SIZE 2-16; NO NA VALUES

##SIZE CLASS FREQUENCY TABLE
LFA35.cpue.size = merge(LFA35.9913.traps,LFA35.9913.counts.at.size, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE"),all.x=T,all.y=F, sort=F)
LFA35.2=which(LFA35.cpue.size$SIZE_CD=='2')
LFA35.size2=LFA35.cpue.size[LFA35.2,]
LFA35.size2 = rename.df(LFA35.size2, "Freq", "size2")
LFA35.size2$SIZE_CD=NULL
LFA35.3=which(LFA35.cpue.size$SIZE_CD=='3')
LFA35.size3=LFA35.cpue.size[LFA35.3,]
LFA35.size3 = rename.df(LFA35.size3, "Freq", "size3")
LFA35.size3$SIZE_CD=NULL
LFA35.2.3 = merge(LFA35.2,LFA35.size3, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA35.4=which(LFA35.cpue.size$SIZE_CD=='4')
LFA35.size4=LFA35.cpue.size[LFA35.4,]
LFA35.size4 = rename.df(LFA35.size4, "Freq", "size4")
LFA35.size4$SIZE_CD=NULL
LFA35.2.4 = merge(LFA35.2.3,LFA35.size4, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA35.5=which(LFA35.cpue.size$SIZE_CD=='5')
LFA35.size5=LFA35.cpue.size[LFA35.5,]
LFA35.size5 = rename.df(LFA35.size5, "Freq", "size5")
LFA35.size5$SIZE_CD=NULL
LFA35.2.5 = merge(LFA35.2.4,LFA35.size5, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA35.6=which(LFA35.cpue.size$SIZE_CD=='6')
LFA35.size6=LFA35.cpue.size[LFA35.6,]
LFA35.size6 = rename.df(LFA35.size6, "Freq", "size6")
LFA35.size6$SIZE_CD=NULL
LFA35.2.6 = merge(LFA35.2.5,LFA35.size6, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA35.7=which(LFA35.cpue.size$SIZE_CD=='7')
LFA35.size7=LFA35.cpue.size[LFA35.7,]
LFA35.size7 = rename.df(LFA35.size7, "Freq", "size7")
LFA35.size7$SIZE_CD=NULL
LFA35.2.7 = merge(LFA35.2.6,LFA35.size7, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA35.8=which(LFA35.cpue.size$SIZE_CD=='8')
LFA35.size8=LFA35.cpue.size[LFA35.8,]
LFA35.size8 = rename.df(LFA35.size8, "Freq", "size8")
LFA35.size8$SIZE_CD=NULL
LFA35.2.8 = merge(LFA35.2.7,LFA35.size8, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA35.9=which(LFA35.cpue.size$SIZE_CD=='9')
LFA35.size9=LFA35.cpue.size[LFA35.9,]
LFA35.size9 = rename.df(LFA35.size9, "Freq", "size9")
LFA35.size9$SIZE_CD=NULL
LFA35.2.9 = merge(LFA35.2.8,LFA35.size9, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA35.10.1=which(LFA35.cpue.size$SIZE_CD=='10.1')
LFA35.size10.1=LFA35.cpue.size[LFA35.10.1,]
LFA35.size10.1 = rename.df(LFA35.size10.1, "Freq", "size10.1")
LFA35.size10.1$SIZE_CD=NULL
LFA35.2.10.1 = merge(LFA35.2.9,LFA35.size10.1, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA35.10.2=which(LFA35.cpue.size$SIZE_CD=='10.2')
LFA35.size10.2=LFA35.cpue.size[LFA35.10.2,]
LFA35.size10.2 = rename.df(LFA35.size10.2, "Freq", "size10.2")
LFA35.size10.2$SIZE_CD=NULL
LFA35.2.10.2 = merge(LFA35.2.10.1,LFA35.size10.2, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA35.11=which(LFA35.cpue.size$SIZE_CD=='11')
LFA35.size11=LFA35.cpue.size[LFA35.11,]
LFA35.size11 = rename.df(LFA35.size11, "Freq", "size11")
LFA35.size11$SIZE_CD=NULL
LFA35.2.11 = merge(LFA35.2.10.2,LFA35.size11, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA35.12=which(LFA35.cpue.size$SIZE_CD=='12')
LFA35.size12=LFA35.cpue.size[LFA35.12,]
LFA35.size12 = rename.df(LFA35.size12, "Freq", "size12")
LFA35.size12$SIZE_CD=NULL
LFA35.2.12 = merge(LFA35.2.11,LFA35.size12, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA35.13=which(LFA35.cpue.size$SIZE_CD=='13')
LFA35.size13=LFA35.cpue.size[LFA35.13,]
LFA35.size13 = rename.df(LFA35.size13, "Freq", "size13")
LFA35.size13$SIZE_CD=NULL
LFA35.2.13 = merge(LFA35.2.12,LFA35.size13, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA35.14=which(LFA35.cpue.size$SIZE_CD=='14')
LFA35.size14=LFA35.cpue.size[LFA35.14,]
LFA35.size14 = rename.df(LFA35.size14, "Freq", "size14")
LFA35.size14$SIZE_CD=NULL
LFA35.2.14 = merge(LFA35.2.13,LFA35.size14, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA35.15=which(LFA35.cpue.size$SIZE_CD=='15')
LFA35.size15=LFA35.cpue.size[LFA35.15,]
LFA35.size15 = rename.df(LFA35.size15, "Freq", "size15")
LFA35.size15$SIZE_CD=NULL
LFA35.2.15 = merge(LFA35.2.14,LFA35.size15, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA35.16=which(LFA35.cpue.size$SIZE_CD=='16')
LFA35.size16=LFA35.cpue.size[LFA35.16,]
LFA35.size16 = rename.df(LFA35.size16, "Freq", "size16")
LFA35.size16$SIZE_CD=NULL
LFA35.2.16 = merge(LFA35.2.15,LFA35.size16, by=c("S_LABEL", "VESSEL_CD", "HAUL_DATE", "TOT_TRAPS"),all.x=T,all.y=T, sort=F)
LFA35.2.16[is.na(LFA35.2.16)]=0
summary(LFA35.2.16)

##SIZE CLASS SPECS FOR UNDERSIZED LOBSTERS FROM 1999-2013
LFA35.2.16$shorts=NA
LFA35.2.16$shorts=LFA35.2.16$size0+LFA35.2.16$size2+LFA35.2.16$size3+LFA35.2.16$size4+LFA35.2.16$size5+LFA35.2.16$size6+
  LFA35.2.16$size7+LFA35.2.16$size8+LFA35.2.16$size9+LFA35.2.16$size10.1
##SIZE CLASS SPECS FOR LEGAL LOBSTERS FROM 1999-2013
LFA35.2.16$legals=NA
LFA35.2.16$legals=LFA35.2.16$size10.2+LFA35.2.16$size11+LFA35.2.16$size12+LFA35.2.16$size13+
  LFA35.2.16$size14+LFA35.2.16$size15+LFA35.2.16$size16
summary(LFA35.2.16)

#Max shorts = 146; Max legals = 35
write.table(LFA35.2.16, file=paste("rec.LFA35.9913.SD1to5.nober.",Sys.Date(),".txt"), row.names=TRUE, col.names=TRUE, sep=" ")
