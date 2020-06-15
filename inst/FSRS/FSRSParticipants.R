require (chron)
require (lattice)
require (MASS)
require (doBy)
require (RODBC)

lobster.db("fsrs.redo")
lobster.db("fsrs")
str(fsrs) #1,273,010 RECORDS JAN2017 SQL EXPORT
FSRS<-fsrs


dat27<-subset(FSRS, HAUL_YEAR == 2017 & LFA == 27)
dat28<-subset(FSRS, HAUL_YEAR == 2017 & LFA == 28)
dat29<-subset(FSRS, HAUL_YEAR == 2017 & LFA == 29)
dat30<-subset(FSRS, HAUL_YEAR == 2017 & LFA == 30)
dat31.1<-subset(FSRS, HAUL_YEAR == 2017 & LFA == 31.1)
dat31.2<-subset(FSRS, HAUL_YEAR == 2017 & LFA == 31.2)
dat32<-subset(FSRS, HAUL_YEAR == 2017 & LFA == 32)

dat33<-subset(FSRS, HAUL_YEAR == 2016 & 2017 & LFA == 33)

dat34<-subset(FSRS, HAUL_YEAR == 2016 & 2017 & LFA == 34)
dat35<-subset(FSRS, HAUL_YEAR == 2016 & 2017 & LFA == 35)

check27<-unique(dat27$VESSEL_CD)
length(check27)

check28<-unique(dat28$VESSEL_CD)
length(check28)

check29<-unique(dat29$VESSEL_CD)
length(check29)

check30<-unique(dat30$VESSEL_CD)
length(check30)

check31.1<-unique(dat31.1$VESSEL_CD)
length(check31.1)


check31.2<-unique(dat31.2$VESSEL_CD)
length(check31.2)


check32<-unique(dat32$VESSEL_CD)
length(check32)

check33<-unique(dat33$VESSEL_CD)
length(check33)

check34<-unique(dat34$VESSEL_CD)
length(check34)

check35<-unique(dat35$VESSEL_CD)
length(check35)

