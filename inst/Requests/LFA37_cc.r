#LFA 37

require(bio.lobster)
require(bio.utilities)
require(devtools)
require(SpatialHub)
la()
p = list()
 logsInSeason<-lobster.db('process.logs.unfiltered')

fi= read.csv(file.path(project.datadirectory('bio.lobster'),'data','LicenceHolder','L3638_cc_Jan2026.csv'))
 
 ##
 #what communities fished in 37 grids since Nov 2021
 #trends in numbers of boats in these grids
 #Landings by licence for cc and what proportion of landings came from 37 grids. same for commercial that fish there too
 
 
grids = c(38,39,40,41,42) 

#by cc community

#l = lobster.db('licence_characteristics')
#li = subset(l, LFA %in% c(36,38))
#lim = merge(li,fi,by='LICENCE_ID',all.x=T)
#lim$Community = ifelse(is.na(lim$Community),'Commercial',lim$Community)

s = subset(logsInSeason,LFA %in% c(36,38) & SYEAR>2021 & SYEAR<2026)
s1 = merge(s,fi[,c('Community','LICENCE_ID')],by=c('LICENCE_ID'),all.x=T)
s1$Community = ifelse(is.na(s1$Community),'Commercial',s1$Community)
st = aggregate(LICENCE_ID~SYEAR+Community+LFA, data=s1, FUN=function(x) length(unique(x))) #how many total licences reporting in logs
cX(st)

#all licences
s1$id = paste(s1$DATE_FISHED,s1$LICENCE_ID)
sr = subset(s1,GRID_NUM %in% grids) 
tr = aggregate(id~SYEAR+LICENCE_ID+Community+LFA,data=sr,FUN=function(x) length(unique(x)))
tru = aggregate(LICENCE_ID~SYEAR+Community+LFA,data=tr,FUN=function(x) length(unique(x)))#how many total licences reporting in logs from LFA 37 grids

tri = aggregate(id~SYEAR+LICENCE_ID+Community+LFA,data=subset(s1,LICENCE_ID %in% unique(tr$LICENCE_ID)),FUN=function(x) length(unique(x)))
names(tri)[5] = 'TotalTrips'
names(tr)[5] = 'L37Trips'

tt = merge(tri,tr,all.x=T)
tt = bio.utilities::na.zero(tt)
tt$LICENCE_ID = ifelse(tt$Community=='Commercial',9999999,tt$LICENCE_ID)

ts = aggregate(cbind(TotalTrips,L37Trips)~Community+LFA+SYEAR+LICENCE_ID,data=tt,FUN=sum)


#of those fishing in 37 what is the change in trips
fe = aggregate(cbind(TotalTrips,L37Trips)~SYEAR+Community+LFA,data=tt,FUN=sum)
fe$Prop = fe$L37Trips/fe$TotalTrips
cX(fe)

#total weight in 37
s1$L37 = ifelse(s1$GRID_NUM %in% grids,1,0)
s1$W37 = s1$WEIGHT_KG*s1$L37
gr = aggregate(cbind(WEIGHT_KG,W37)~SYEAR+LICENCE_ID+Community+LFA,data=subset(s1),FUN=sum)
grr = aggregate(cbind(WEIGHT_KG,W37)~SYEAR+Community+LFA,data=subset(gr),FUN=sum)

grr$prop37 = grr$W37/grr$WEIGHT_KG


####not just those in 37
s1$id = paste(s1$DATE_FISHED,s1$LICENCE_ID)
sr = subset(s1,GRID_NUM %in% grids) 
tr = aggregate(id~SYEAR+LICENCE_ID+Community+LFA,data=sr,FUN=function(x) length(unique(x)))
tri = aggregate(id~SYEAR+LICENCE_ID+Community+LFA,data=subset(s1),FUN=function(x) length(unique(x)))
names(tri)[5] = 'TotalTrips'
names(tr)[5] = 'L37Trips'

tt = merge(tri,tr,all.x=T)
tt = bio.utilities::na.zero(tt)
tt$LICENCE_ID = ifelse(tt$Community=='Commercial',9999999,tt$LICENCE_ID)

ts = aggregate(cbind(TotalTrips,L37Trips)~Community+LFA+SYEAR+LICENCE_ID+GRID_NUM,data=tt,FUN=sum)
cX(ts)



#total weight in 37
s1$L37 = ifelse(s1$GRID_NUM %in% grids,1,0)
s1$W37 = s1$WEIGHT_KG*s1$L37
s1$LICENCE_ID = ifelse(s1$Community=='Commercial',9999999,s1$LICENCE_ID)

gr = aggregate(cbind(WEIGHT_KG,W37)~SYEAR+LICENCE_ID+Community+LFA,data=subset(s1),FUN=sum)
gs = aggregate(cbind(WEIGHT_KG,W37)~Community+LFA+SYEAR+LICENCE_ID,data=s1,FUN=sum)

ws =merge(gs,ts)


######################
#by grid


grr = data.frame(GRID_NUM=grids)

s = subset(logsInSeason,LFA %in% c(36,38) & SYEAR>2021 & SYEAR<2026)
s1 = merge(s,fi[,c('Community','LICENCE_ID')],by=c('LICENCE_ID'),all.x=T)
s1$Community = ifelse(is.na(s1$Community),'Commercial',s1$Community)

s2 = subset(s1,Community != 'Commercial')

s2$id = paste(s2$LICENCE_ID,s2$DATE_FISHED)

s3 = aggregate(id~SYEAR+LFA+GRID_NUM+Community+LICENCE_ID,data=s2,FUN=function(x) length(unique(x)))
s4 = aggregate(WEIGHT_KG~SYEAR+LFA+GRID_NUM+Community+LICENCE_ID,data=s2,FUN=sum)

s34 = merge(s3,s4)

names(s34) = c('Season (year ending)','LFA','GRID_NUM','Community','Licence_id','N Days Fished','Total Weight Landed')
write.csv(s34,'~/tmp/WNNB_licences_LFA36_37_38.csv')
