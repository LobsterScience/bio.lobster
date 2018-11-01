#seasonal landings


#this is not used

require(bio.survey)
require(bio.lobster)
p = bio.lobster::load.environment()
p$libs = NULL

require(bio.lobster)
require(bio.utilities)
la()

lobster.db('logs41') #make sure to do a database recapture through logs41.redo before moving on

logs41 = rename.df(logs41,c('FV_FISHED_DATETIME'),c('DATE_FISHED'))
off41 = rename.df(off41,c('LOB_EST_LBS','ADJ_LOB_LBS'),c('EST_WEIGHT_LOG_LBS','ADJCATCH'))													

logs41$yr = year(logs41$DATE_FISHED) #2002 to present
ziff41$yr = year(ziff41$DATE_FISHED) #1995 to 2001
ziff41$DDLON = ziff41$DDLON * -1
off41$yr  = year(off41$DATE_FISHED) #1981 to 1994
 
off41 = subset(off41,  select=c('MON_DOC_ID','VR_NUMBER','DATE_FISHED','DDLAT','DDLON','NUM_OF_TRAPS','EST_WEIGHT_LOG_LBS','ADJCATCH','yr'))
ziff41 = subset(ziff41,select=c('MON_DOC_ID','VR_NUMBER','DATE_FISHED','DDLAT','DDLON','NUM_OF_TRAPS','EST_WEIGHT_LOG_LBS','ADJCATCH','yr'))
logs41 = subset(logs41, select=c('MON_DOC_ID','VR_NUMBER','DATE_FISHED','DDLAT','DDLON','NUM_OF_TRAPS','EST_WEIGHT_LOG_LBS','ADJCATCH','yr'))


#oct16-oct15 fishing year until 2005 switch to Jan 1 to Dec 31

a41 = rbind(off41,ziff41,logs41)
a41$fishingYear = sapply(a41$DATE_FISHED,offFishingYear)
a41 = makePBS(a41,polygon=F)
a41$ADJCATCH = a41$ADJCATCH / 2.2 #convert to kgs

#prune out 
	LFA41 = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA41Offareas.csv"))
	attr(LFA41,'projection') <- 'LL'

pra41 = completeFun(a41,c('X','Y'))

p1 = findPolys(pra41,LFA41)
pp = merge(pra41,p1,by=c('EID'))

fy = unique(pp$fishingYear)

for(i in 1:length(fy)) {
	r = subset(pp,fishingYear==fy[i])
	up = unique(r$PID)
	for(j in 1:length(up)) {
		w = subset(r,PID==up[j])

		fishing.season(w[,c('DATE_FISHED','ADJCATCH')],smooth=0.01)
	}

}
