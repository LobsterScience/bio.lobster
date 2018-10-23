# Lobster Fishery Footprint
require(bio.survey)
require(bio.lobster)
p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),'analysis','lfa41Assessment')

assessment.year = p$current.assessment.year ########### check the year ############### !!!!!!!!!!!

require(bio.lobster)
require(bio.utilities)
la()
require(SpatialHub) #hubley package
lobster.db('logs41') #make sure to do a database recapture through logs41.redo before moving on

logs41 = rename.df(logs41,c('FV_FISHED_DATETIME'),c('DATE_FISHED'))

logs41$yr = year(logs41$DATE_FISHED) #2002 to present
ziff41$yr = year(ziff41$DATE_FISHED) #1995 to 2001
ziff41$DDLON = ziff41$DDLON * -1
off41$yr  = year(off41$DATE_FISHED) #1981 to 1994
 
off41 = subset(off41,  select=c('MON_DOC_ID','VR_NUMBER','DATE_FISHED','DDLAT','DDLON','NUM_OF_TRAPS','LOB_EST_LBS','ADJ_LOB_LBS','yr'))
ziff41 = subset(ziff41,select=c('MON_DOC_ID','VR_NUMBER','DATE_FISHED','DDLAT','DDLON','NUM_OF_TRAPS','EST_WEIGHT_LOG_LBS','ADJCATCH','yr'))
logs41 = subset(logs41, select=c('MON_DOC_ID','VR_NUMBER','DATE_FISHED','DDLAT','DDLON','NUM_OF_TRAPS','EST_WEIGHT_LOG_LBS','ADJCATCH','yr'))

off41 = rename.df(off41,c('LOB_EST_LBS','ADJ_LOB_LBS'),c('EST_WEIGHT_LOG_LBS','ADJCATCH'))													

#oct16-oct15 fishing year until 2005 switch to Jan 1 to Dec 31

a41 = rbind(off41,ziff41,logs41)
a41$fishingYear = sapply(a41$DATE_FISHED,offFishingYear)
a41 = makePBS(a41,polygon=F)
a41$ADJCATCH = a41$ADJCATCH / 2.2 #convert to kgs then to t


#prune out 
	LFA41 = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA41Offareas.csv"))
	LFA41 = joinPolys(as.PolySet(LFA41),operation='UNION')
	LFA41 = subset(LFA41,SID==1)
	attr(LFA41,'projection') <- 'LL'

pra41 = completeFun(a41,c('X','Y'))

p1 = findPolys(pra41,LFA41)
pra41 = pra41[which(pra41$EID %in% p1$EID),]
a = fisheryFootprint(pra41,var='CPUE',res=0.05,propArea = 0.95)
b = fisheryFootprint(pra41,var='LANDINGS',propArea=0.95,res=0.05)
d = fisheryFootprint(pra41,var='EFFORT',res=0.05,propArea=.95)


#similar patterns for Landings with prop of landings and gini, but what about changing resolution, which remains robust
rr = c(.05,.1,.15,.2,.25,.5)
out = list()
for(i in 1:length(rr)) {
		out[[i]] = fisheryFootprint(pra41,var='LANDINGS',propArea=0.95,res=rr[i])
	}

correlationMatrix(cbind(out[[c(1,2)]][,1],out[[c(2,2)]][,1],out[[c(3,2)]][,1],out[[c(4,2)]][,1],out[[c(5,2)]][,1],out[[c(6,2)]][,1]))

#gini is a better metric than integer of an arbitrary number of squares and the resolution

#need to record the number of logbook entries not included in current boundaries for fishing footprint
p = list()
p$metric = 'gini'
p$running.median=T
p$file.name = 'giniFootprintCPUE.png'
p$measure = 'mean'
p$time.series.start.year=1981
p$time.series.end.year=assessment.year
p$error.polygon=F
p$error.bars=F
p$running.mean=F
p$running.length=3
p$add.reference.lines=F
p$legend=F
p$figure.title=''
gg = as.data.frame(a[[1]])
names(gg) = c('gini','yr')
gg = gg[,c('yr','gini')]
write.csv(gg,file=file.path(fp,'indicators','giniCPUE.csv'))
figure.stratified.analysis(gg,p=p)

gg = as.data.frame(b[[1]])
names(gg) = c('gini','yr')
gg = gg[,c('yr','gini')]
p$file.name = 'giniFootprintLandings.png'
figure.stratified.analysis(gg,p=p)
write.csv(gg,file=file.path(fp,'indicators','giniLandings.csv'))


gg = as.data.frame(d[[1]])
names(gg) = c('gini','yr')
gg = gg[,c('yr','gini')]
p$file.name = 'giniFootprintEffort.png'
figure.stratified.analysis(gg,p=p)
write.csv(gg,file=file.path(fp,'indicators','giniEffort.csv'))
