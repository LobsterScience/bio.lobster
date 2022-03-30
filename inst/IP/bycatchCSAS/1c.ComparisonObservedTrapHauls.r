#comparison of Observed trap hauls and 


require(bio.lobster)
require(bio.utilities)
load_all('C:/Users/Cooka/Documents/git/bio.utilities')

wd = ('C:\\Users\\Cooka\\OneDrive - DFO-MPO\\BycatchLobster')

setwd(wd)
la()


a =  bycatch.db('logbook.merge')
a = subset(a, LFA%in% 33:35)
aA = read.csv(file=file.path('results','CompliedDataForModelling.csv'))
aA = subset(aA, LFA %in% 33:35)

ag = aggregate(LegalWt~TRIP+Period+LFA+GridGroup,data=aA,FUN=mean)

aag = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SD_LOG_ID+Period+GridGroup+LFA,data=a,FUN=sum)


aag$ST = paste(aag$Period, aag$GridGroup, aag$LFA,sep='-')
ag$ST = paste(ag$Period, ag$GridGroup, ag$LFA,sep='-')

aag$CPUE = aag$WEIGHT_KG / aag$NUM_OF_TRAPS


aSS = split(aag,f=aag$ST)

for(i in 1:length(aSS)){
		g = aSS[[i]]
		g$CPUEi = round(g$CPUE,1)
		g = aggregate(SD_LOG_ID~CPUEi,data=g,FUN=length)
		g$CS = cumsum(g$SD_LOG_ID) / sum(g$SD_LOG_ID)

		k = subset(ag,ST==unique(aSS[[i]]$ST))
		k$CPUEi = round(k$LegalWt,1)
		k = aggregate(TRIP~CPUEi,data=k,FUN=length)
		k$CS = cumsum(k$TRIP) / sum(k$TRIP)

}

