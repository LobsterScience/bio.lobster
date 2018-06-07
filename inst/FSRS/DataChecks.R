require (chron)
require (lattice)
require (MASS)
require (doBy)
require (RODBC)
require(PBSmapping)
require(bio.lobster)


lobster.db("fsrs.redo")
lobster.db("fsrs")
str(fsrs)    #1365712 Records for 2017
FSRS<-fsrs

##COLUMN ADDITIONS TO DATA FRAME ALLOWING DIFFERENT DATE FORMATS
FSRS$julian = as.integer(julian(FSRS$HAUL_DATE))
summary(FSRS)

##TIDY UP THE LFA SPECIFICATIONS
sort(unique(FSRS$LFA))

## Check that LAT and LONG are in the correct LFAs
FSRS$X = FSRS$LONG_DD
FSRS$Y = FSRS$LAT_DD
FSRS$EID = 1:nrow(FSRS)

LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))

locCheck<-findPolys(FSRS, LFAs, maxRows = nrow(FSRS))
dim(locCheck)
dim(FSRS)

#Find missing locations - or locations that don't pair with corresponding LFAs
missingLoc<-merge(FSRS, locCheck, all.x = T, by = "EID")
missedLoc<-subset(missingLoc, is.na(PID))

LobsterMap("27")
addPoints(subset(missedLoc,LFA=="27",select=c("X","Y","EID")),pch=16, cex=0.5, col="red")

missingLoc$PID<-ifelse(missingLoc$PID %in% c(311, 312),missingLoc$PID/10, missingLoc$PID)

h<-subset(missingLoc, LFA!= PID)
unique(missingLoc$PID)

#Check unique licence fishes in consistent LFA (all licence should only have one LFA)
w<-aggregate(LFA~VESSEL_CD, data=FSRS, FUN= function(x)length(unique(x)))

any(w$LFA>1)

##NOTE: SOME POINTS ARE FALLING ON LAND _ NOT ASSOCIATED WITH AN LFA ##

mls2fsrs()
require(bio.utilities)
mlsCheck<-mls2fsrs()

mlsCheck<-rename.df(mlsCheck,"Year", "HAUL_YEAR")
FSRS<-merge(FSRS, mlsCheck, all.x=T, by=c("HAUL_YEAR","LFA"))

FSRS$ID<-NA
FSRS$ID<-ifelse(FSRS$SIZE_CD<FSRS$FSRS & FSRS$SHORT == 0, 1,FSRS$ID)
any(FSRS$ID == 1,na.rm = T)

FSRS$ID<-NA
FSRS$ID<-ifelse(FSRS$SIZE_CD>FSRS$FSRS & FSRS$SHORT == 1,1,FSRS$ID)
any(FSRS$ID == 1,na.rm = T)

FSRS$ID <- NULL

#Check males aren't berried or notched

    # Males = 1
    # Females = 2 
    # Berried = 3
    # V-notch Column 1 = Y, 0 = N


FSRS$SEXCHECK<-NA
FSRS$SEXCHECK<- ifelse(FSRS$SEX == 1 & FSRS$V_NOTCHED == 1,1, FSRS$SEXCHECK)
any(FSRS$SEXCHECK == 1, na.rm = T)
which(FSRS$SEXCHECK == 1)
FSRS$SEXCHECK <- NULL


#Check the temperature column is fully populated
hist(FSRS$TEMP)

any(FSRS$TEMP == -99)
length(which(FSRS$TEMP == -99))
## 75347 "-99" for TEMP


any(is.na(FSRS$TEMP))
length(which(is.na(FSRS$TEMP)))

## 16666 NAs for TEMP


# Check what years/entries are missing the temperature data

missingTemp <- subset(FSRS, TEMP == -99)
str(missingTemp)
summary(missingTemp)
hist(missingTemp$HAUL_YEAR)
## Temp data at -99 consistently from 2003-2017

missingTemp<- subset(FSRS, is.na(TEMP))
str(missingTemp)
summary(missingTemp)
hist(missingTemp$HAUL_YEAR)
##Temp Data as NAs between 1999-2010 Mostly in 99-03


#Aggregate of haul date and vessel id and number of lobsters
counts<-aggregate(LOBSTER_NO~HAUL_YEAR + VESSEL_CD + HAUL_DATE, data= FSRS, FUN =max)

#Check the maxium numbers makes sense
range(counts$LOBSTER_NO)
highCounts<-subset(counts, LOBSTER_NO > 40)

#Normal looking distribution of max lobsters in traps - no massive outliers
#Number of lobsters in traps range from 0 - 91

#Check ranges and factors for rest of the categories

range(FSRS$SOAK_DAYS)
hist(FSRS$SOAK_DAYS)

soaking<-subset(FSRS, SOAK_DAYS>50)
hist(soaking$SOAK_DAYs)
soak<-as.numeric(soaking$SOAK_DAYS)
range(soaking$LFA)




