lobster.db('fsrs')

fsrs$id=paste(fsrs$VESSEL_CD, fsrs$HAUL_DATE, sep=':')
f=subset(fsrs, !duplicated(subset(fsrs, select=c(id))))

#remove extraneous columns
f=f[c("HAUL_DATE", "WIND_SPEED", "WIND_DIRECTION", "LAT_DD", "LONG_DD")]

f=f[is.finite(f$WIND_DIRECTION)&is.finite(f$WIND_SPEED),]

#Remove "variable" and "unknown" for speed and direction
f=f[f$WIND_DIRECTION<9,]
f=f[f$WIND_SPEED<10,]



f$w.kph=NA
f$w.br=NA

#Recode Windspeed codes into km/h
f$w.kph[f$WIND_SPEED==0]=0
f$w.kph[f$WIND_SPEED==1]=3.7
f$w.kph[f$WIND_SPEED==2]=9.25
f$w.kph[f$WIND_SPEED==3]=15.725
f$w.kph[f$WIND_SPEED==4]=24.975
f$w.kph[f$WIND_SPEED==5]=35.15
f$w.kph[f$WIND_SPEED==6]=45.325
f$w.kph[f$WIND_SPEED==7]=56.425
f$w.kph[f$WIND_SPEED==8]=68.45
f$w.kph[f$WIND_SPEED==9]=83.25

#Recode wind direction codes into bearing
f$w.br[f$WIND_DIRECTION==0]=0
f$w.br[f$WIND_DIRECTION==1]=0
f$w.br[f$WIND_DIRECTION==2]=45
f$w.br[f$WIND_DIRECTION==3]=90
f$w.br[f$WIND_DIRECTION==4]=135
f$w.br[f$WIND_DIRECTION==5]=180
f$w.br[f$WIND_DIRECTION==6]=225
f$w.br[f$WIND_DIRECTION==7]=270
f$w.br[f$WIND_DIRECTION==8]=315

summary(as.factor(f$WIND_DIRECTION))
summary(as.factor(f$w.br))

summary(as.factor(f$WIND_SPEED))
summary(as.factor(f$w.kph))

