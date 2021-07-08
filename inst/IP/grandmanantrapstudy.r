#grand manan trap study

require(bio.lobster)
require(devtools)
require(bio.utilities)
require(lubridate)
a = lobster.db('cris')

g = subset(a$crports.csv, LFA_ID==12)

tr = subset(cris.trips, PORT %in% g$PORT & year(STARTDATE) %in% 2000:2002 & month(STARTDATE)==9)
tr$ID = paste(tr$TRIPNO, tr$PORT, sep= '-')
cris.samples$ID = paste(cris.samples$TRIPNO, cris.samples$PORT, sep= '-')

sa = subset(cris.samples, ID %in% tr$ID & SPECIESCODE==1)

hist(sa$CARLENGTH, breaks=seq(0,220), xlab='Carapace Length',main='September Trap Study 2000-2002')
abline(v=82.5,col='red', lwd=2)
savePlot('~/tmp/L38Trapping.png')
lobster.db("survey")

SETSSum = subset(surveyCatch, month(BOARD_DATE) %in% 6:8, select=SET_ID)[,1]
SETSFa = subset(surveyCatch, month(BOARD_DATE) %in% 9:10, select=SET_ID)[,1]


LongForm = aggregate(FISH_NO~floor(FISH_LENGTH)+SEX+SET_ID,data=subset(surveyMeasurements,SPECCD_ID==2550 & SET_ID %in% SETSSum),FUN=length)
names(LongForm) = c("FL","SEX","SET_ID","N")
x = aggregate(N~FL, data=LongForm,FUN=sum)
x = rep(x$FL, times=x$N)
x= x[x<220]
hist(x, breaks=seq(0,220), xlab='Carapace Length',main='ILTS Trawl Survey SUMMER')
abline(v=82.5, col='red',lwd=2)
savePlot('~/tmp/ILTSSummer.png')



LongForm = aggregate(FISH_NO~floor(FISH_LENGTH)+SEX+SET_ID,data=subset(surveyMeasurements,SPECCD_ID==2550 & SET_ID %in% SETSFa),FUN=length)
names(LongForm) = c("FL","SEX","SET_ID","N")
x = aggregate(N~FL, data=LongForm,FUN=sum)
x = rep(x$FL, times=x$N)
x= x[x<220]
hist(x, breaks=seq(0,220), xlab='Carapace Length',main='ILTS Trawl Survey SEPTEMBER')
abline(v=82.5, col='red',lwd=2)
savePlot('~/tmp/ILTSFall.png')
