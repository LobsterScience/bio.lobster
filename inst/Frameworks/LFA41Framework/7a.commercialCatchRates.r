#Commercial Catch rates

require(bio.survey)
require(bio.lobster)
p = bio.lobster::load.environment()
p$libs = NULL

require(bio.lobster)
require(bio.utilities)
la()

lobster.db('logs41') #make sure to do a database recapture through logs41.redo before moving on

logs41 = rename.df(logs41,c('FV_FISHED_DATETIME'),c('DATE_FISHED'))

logs41$yr = year(logs41$DATE_FISHED) #2002 to present
ziff41$yr = year(ziff41$DATE_FISHED) #1995 to 2001
ziff41$DDLON = ziff41$DDLON * -1
off41$yr  = year(off41$DATE_FISHED) #1981 to 1994

logs41$OFFAREA = NULL	

#oct16-oct15 fishing year until 2005 switch to Jan 1 to Dec 31

a41 = rbind(off41,ziff41,logs41)
a41$fishingYear = sapply(a41$DATE_FISHED,offFishingYear)
a41 = makePBS(a41,polygon=F)
a41$ADJCATCH = a41$ADJCATCH / 2.2 #convert to kgs then to t

#prune out 
	LFA41 = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA41Offareas.csv"))
	attr(LFA41,'projection') <- 'LL'

pra41 = completeFun(a41,c('X','Y'))

p1 = findPolys(pra41,LFA41)

pp = merge(pra41,p1,by='EID')

mp = as.data.frame(unique(cbind(LFA41$PID,LFA41$OFFAREA)))
names(mp) = c('PID','Area')

pp = merge(pp,mp,by='PID')
	x = completeFun(pp,c('ADJCATCH','NUM_OF_TRAPS'))
				xy = aggregate(cbind(ADJCATCH,NUM_OF_TRAPS)~fishingYear,data=x,FUN=sum)
				xy$lL = xy$ADJCATCH / xy$NUM_OF_TRAPS
	
with(xy,plot(fishingYear,lL,type='l',xlab='Year',ylab='CPUE',yaxt='n'))
savePlot(file=file.path(project.figuredirectory('bio.lobster'),'CPUE.png'))
xy = xy[,c('fishingYear','lL')]
names(xy)[2] = 'CPUE'
write.csv(xy,file=file.path(project.datadirectory('bio.lobster'),'analysis','indicators','commercialCatchrates.csv'))