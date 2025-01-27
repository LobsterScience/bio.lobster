require(bio.lobster)
require(bio.utilities)
require(gamlss)
require(tidyr)
require(devtools)
require(geosphere)
require(sf)
require(ggplot2)
la()
options(stringsAsFactors=F)

fpa = file.path(project.datadirectory('bio.lobster'),'analysis','ILTSSurvey')
v = ILTS_ITQ_All_Data(species=2550,redo_base_data =F,return_base_data = T,biomass=F )
v = subset(v,SPECCD_ID==2511)
cs = read.csv(file.path(project.datadirectory('bio.lobster'),'data','survey','comparativeStations.csv'))
cs = subset(cs,YEAR %in% c(2016,2019))
cs$ID = paste(cs$YEAR,cs$STATION)

v$ID = paste(v$YEAR,v$STATION)

v$II = paste(v$TRIP_ID,v$SET_NO)
vv = aggregate(TRIP_ID~YEAR+VESSEL_NAME+II,data=v,FUN=length)
vvv = aggregate(II~YEAR+VESSEL_NAME,data=vv,FUN=function(x) length(unique(x)))
vvv[order(vvv$YEAR),]

v$rFL = floor(v$FISH_LENGTH/5)*5+2
v = subset(v,FISH_LENGTH>5 & FISH_LENGTH<150)
v = subset(v,ID %in% cs$ID)
v$SID = paste(v$TRIP_ID,v$SET_NO)


va = aggregate(SA_CORRECTED_PRORATED_N~STATION+GEAR+VESSEL_NAME+YEAR+SID+SET_DEPTH+SET_LONG+SET_LAT+sweptArea+spread+SET_DATE+SET_TIME,data=v,FUN=sum)
names(va)[13]='N'
##########NOT ENOUGH DATA
