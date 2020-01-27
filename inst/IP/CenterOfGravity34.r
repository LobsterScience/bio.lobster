#landings by grid

require(bio.lobster)
require(bio.survey)
require(bio.lobster)
p = bio.lobster::load.environment()
p$libs = NULL

require(bio.utilities)
la()

a = lobster.db('process.logs') 

#grid areas by LFA
		LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
		LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
		attr(LFAgrid,'projection') <- "LL"
		ar = calcArea(LFAgrid)
		names(ar)[1:2] <- c('LFA','GRID_NUM')
		ar7 = subset(ar,LFA==37)
		ar6 = subset(ar,LFA==36)
		ar8 = subset(ar,LFA==38)

		ar6b = ar7
		ar6b$LFA=36
		ar6 = as.data.frame(rbind(ar6,ar6b))
		ar6 = aggregate(area~LFA+GRID_NUM,data=ar6,FUN=sum)

		ar8b = ar7
		ar8b$LFA=38
		ar8 = as.data.frame(rbind(ar8,ar8b))
		ar8 = aggregate(area~LFA+GRID_NUM,data=ar8,FUN=sum)

		ar = subset(ar, !LFA %in% c(36,38))
		ar = as.data.frame(rbind(rbind(ar,ar6),ar8))


aA = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~GRID_NUM+LFA+SYEAR,data=a,FUN=sum)
aA = merge(aA, ar, by=c('LFA','GRID_NUM'),all.x=T)
aA$CPUE = aA$WEIGHT_KG / aA$NUM_OF_TRAPS
lfa = c(34,35,36,38)
out = list()
m= 0 
for(i in 1:length(lfa)){
		aB = subset(aA,LFA==lfa[i])
		y = unique(aB$SYEAR)

		for(j in 1:length(y)){
			print(paste(lfa[i],y[j]))
			aC = subset(aB,SYEAR==y[j])
			if(nrow(aC)>=5){
			m=m+1
			out[[m]] = c(lfa[i],y[j],giniFootprint(aC$CPUE,aC$area),giniFootprint(aC$WEIGHT_KG,aC$area),giniFootprint(aC$NUM_OF_TRAPS,aC$area))
		}
	}
}
	out = as.data.frame(do.call(rbind,out))
	names(out) = c('LFA','YEAR','CPUE','LANDINGS','EFFORT')

out = out[order(out$LFA, out$YEAR),]

png(file=file.path(project.figuredirectory('bio.lobster'),'LFA3438Framework2019','GiniLandings34.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
with(subset(out,LFA==34),plot(YEAR,LANDINGS, pch=16,xlab='Year',ylab='Gini Index'))
with(subset(out,LFA==34),lines(YEAR,runmed(LANDINGS,k=3), lwd=3,col='salmon'))
dev.off()

png(file=file.path(project.figuredirectory('bio.lobster'),'LFA3438Framework2019','GiniLandings35.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
with(subset(out,LFA==35),plot(YEAR,LANDINGS, pch=16,xlab='Year',ylab='Gini Index'))
with(subset(out,LFA==35),lines(YEAR,runmed(LANDINGS,k=3), lwd=3,col='salmon'))
dev.off()

png(file=file.path(project.figuredirectory('bio.lobster'),'LFA3438Framework2019','GiniLandings36.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
with(subset(out,LFA==36),plot(YEAR,LANDINGS, pch=16,xlab='Year',ylab='Gini Index'))
with(subset(out,LFA==36),lines(YEAR,runmed(LANDINGS,k=3), lwd=3,col='salmon'))
dev.off()

png(file=file.path(project.figuredirectory('bio.lobster'),'LFA3438Framework2019','GiniLandings38.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
with(subset(out,LFA==38),plot(YEAR,LANDINGS, pch=16,xlab='Year',ylab='Gini Index'))
with(subset(out,LFA==38),lines(YEAR,runmed(LANDINGS,k=3), lwd=3,col='salmon'))
dev.off()