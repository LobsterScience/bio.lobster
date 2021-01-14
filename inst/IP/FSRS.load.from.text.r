#Provides Ability to load data from FSRS that comes in three files (lines 18-20)
#if provided as .txt files, convert to .csv
#Saves a file which is then read into the fsrs data through the lobster.db('fsrs.redo') function 
require(bio.lobster)
require(lubridate)
require(bio.utilities)
options(stringsAsFactors=F)

p = bio.lobster::load.environment()

setwd(file.path(project.datadirectory('bio.lobster'),'data','fsrs.text.dump',p$current.assessment.year))

#lobster.db('fsrs') #imports databased FSRS data 

#-------------------------------------
#import "new" frsr data from csv files from Shannon, etc

xy = read.csv('Position.csv')
ca = read.csv('Catch.csv')
tr = read.csv('Traps.csv')

#Check location of data points. Choose LFA of choice below
LobsterMap('27-32')

		xy$X =convert.dd.dddd(xy$Longitude)*-1
		xy$Y =convert.dd.dddd(xy$Latitude)
		xy = makePBS(xy,polygon=F) #,coords=c('Longitude','Latitude'))
		addPoints(xy,col='red',pch=16,cex=.5)

LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
attr(LFAgrid,'projection') <- "LL"
g = findPolys(xy,LFAgrid)
xy = merge(xy,g[,c('EID','SID')])

xy$Date = as.Date(xy$Date,format='%d-%b-%y')

Fu = merge(tr,merge(xy,ca,by=c('Record.Number')),by=c('Record.Number')) #lobster 0 is an empty trap


Fu$LID = paste(Fu$Lobster.Number,Fu$Trap.Number,Fu$Record.Number,sep="-")
Fu$TID = paste(Fu$Trap.Number,Fu$Record.Number,sep="-")

#rename columns with dplyr package to facilitate merge with data from database

names(Fu)[names(Fu) == "Sepal.Length"] <- "sepal_length"

names(Fu)[names(Fu) == "Record.Number"]="RECORD_NUMBER"
names(Fu)[names(Fu) == "ID"]="ID"
names(Fu)[names(Fu) == "Trap.Number"]="TRAP_NO"
names(Fu)[names(Fu) == "Lobster.Number"]="LOBSTER_NO"
names(Fu)[names(Fu) == "Sex"]="SEX"           
names(Fu)[names(Fu) == "Size"]="SIZE_CD"
names(Fu)[names(Fu) == "Short"]="SHORT"
names(Fu)[names(Fu) == "Berried"]="BERRIED"
names(Fu)[names(Fu) == "V.Notched"]="V_NOTCHED"
names(Fu)[names(Fu) == "Recaptured"]="RECAPTURED"
names(Fu)[names(Fu) == "EID"]="EID"
names(Fu)[names(Fu) == "ID.x"]="ID.X"
names(Fu)[names(Fu) == "Vessel.Code"]="VESSEL_CD"
names(Fu)[names(Fu) == "Soak.Days"]="SOAK_DAYS"
names(Fu)[names(Fu) == "Date"]="HAUL_DATE"
names(Fu)[names(Fu) == "Depth"]="DEPTH"
names(Fu)[names(Fu) == "Gauge"]="GAUGE"
names(Fu)[names(Fu) == "Latitude"]="LATITUDE"
names(Fu)[names(Fu) == "Longitude"]="LONGITUDE"
names(Fu)[names(Fu) == "Temperature"]="TEMP"
names(Fu)[names(Fu) == "Bait"]="BAIT"
names(Fu)[names(Fu) == "Wind.Direction"]="WIND_DIRECTION"
names(Fu)[names(Fu) == "Wind.Speed"]="WIND_SPEED"
names(Fu)[names(Fu) == "Time"]="HAUL_TIME"
names(Fu)[names(Fu) == "LFA"]="LFA"
names(Fu)[names(Fu) == "X"]="LONG_DD"
names(Fu)[names(Fu) == "Y"]="LAT_DD"
names(Fu)[names(Fu) == "SID"]="LFA_GRID"
names(Fu)[names(Fu) == "ID.y"]="ID.Y"
names(Fu)[names(Fu) == "Number.of.Traps.Hauled"]="NUMBER.OF.TRAPS.HAULED"
names(Fu)[names(Fu) == "Lbs.Canners"]="LBS.CANNERS"
names(Fu)[names(Fu) == "Lbs.Markets"]="LBS.MARKETS"
names(Fu)[names(Fu) == "V.notch.w.o.eggs"]="V.NOTCH.W.O.EGGS"
names(Fu)[names(Fu) == "V.notch.with.eggs"]="V.NOTCH.WITH.EGGS"
names(Fu)[names(Fu) == "Berried..Lobsters"]="BERRIED..LOBSTERS"
names(Fu)[names(Fu) == "Tagged.v.notch.with.eggs"]="TAGGED.V.NOTCH.WITH.EGGS"
names(Fu)[names(Fu) == "Tagged.v.notch.w.o.eggs"]="TAGGED.V.NOTCH.W.O.EGGS"
names(Fu)[names(Fu) == "Snow.Crab"]="SNOW.CRAB"
names(Fu)[names(Fu) == "Green.Crab"]="GREEN.CRAB"
names(Fu)[names(Fu) == "Rock.Crab"]= "ROCK.CRAB"      
names(Fu)[names(Fu) == "Jonah.Crab"]="JONAH.CRAB"
names(Fu)[names(Fu) == "Spider.Crab"]="SPIDER.CRAB"
names(Fu)[names(Fu) == "Comments"]="COMMENTS"

Fu$HAUL_YEAR=year(Fu$HAUL_DATE)
Fu$S_LABEL=Fu$HAUL_YEAR

#Need to get LFA's 33-34 into format 2019-2020
Fu$S_LABEL[Fu$LFA%in%c("33","34")]= paste(as.character(as.numeric(Fu$HAUL_YEAR[Fu$LFA%in%c("33","34")])-1),
                                    Fu$HAUL_YEAR[Fu$LFA%in%c("33","34")], sep="-" )
Fu$S_LABEL[Fu$LFA%in%c("33","34")& month(Fu$HAUL_DATE) %in% c("11", "12")]= 
                                  paste(Fu$HAUL_YEAR[Fu$LFA%in%c("33","34")& month(Fu$HAUL_DATE) %in% c("11", "12")],
                                  Fu$HAUL_YEAR[Fu$LFA%in%c("33","34")& month(Fu$HAUL_DATE) %in% c("11", "12")] +1, sep="-" )
#Fu$TEMP=NA
Fu$SHORT=(as.integer(Fu$SHORT)) #convert logical to integer to match fsrs data
Fu$V_NOTCHED=(as.integer(Fu$V_NOTCHED)) #convert logical to integer to match fsrs data


write.csv(Fu,(file.path( project.datadirectory("bio.lobster"), "data","inputs","non.db.fsrs.csv")))

