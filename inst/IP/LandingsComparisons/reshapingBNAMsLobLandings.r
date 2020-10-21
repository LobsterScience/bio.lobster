require(bio.lobster)
la()
require(lubridate)
anc = bnamR(redo=F)


	TempData = read.csv(file.path( project.datadirectory("bio.lobster"),"Temperature Data","TempData.csv"))
 	TempData$Date = as.Date(TempData$DATE)
	TempData$mon = month(TempData$Date)
 	TempData$yr = year(TempData$Date)
 	TempData$EID = 1:nrow(TempData)
#shrinking LFA27-32 to 100m iso
require(PBSmapping)
load(file='/SpinDr/backup/bio_data/bio.lobster/data/maps/LFA27-33100mIsobath.rdata') #Isob100 the 100 m isobath for 27-33
load(file='/SpinDr/backup/bio_data/bio.lobster/data/maps/iso100.rdata') #Isob100 the 100 m isobath for 27-33
load(file='/SpinDr/backup/bio_data/bio.lobster/data/maps/iso100contin.rdata') #Isob100 the 100 m isobath for 27-33

#us 
a = importShapefile(file.path(project.datadirectory('bio.lobster'),'data','maps','MaineLobsterAreas/StatAreas_Lobster_SmoothCoast'),readDBF=T) 
          attr(a,'projection') <- "LL"
           l = attributes(a)$PolyData[,c('PID','Id')]
           l = subset(l,PID %in% c(3,4,5,6))
USareas = merge(a,l,by='PID')
USareas = USareas[order(USareas$PID,USareas$SID,USareas$POS),]
USareas = subset(USareas,SID==1)       
USareas$PID=USareas$Id
USareas$Id = NULL

	Isob100=o #full area
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
		LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
		sLFAs = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","sGSLLFAPolys.csv"))
		sLFAs$X.1 = NULL

		L = LFAs = rbind(rbind(LFAs,sLFAs),USareas)
		out=list()
		j=0
		io = c(23,24,25,261,262,27,29,30,311,312,32,33,34,35,36,38,511,512,513,514)
for(i in io){
			j = j+1
			ij = subset(LFAs,PID==i& SID==1)
			#if(i %in% c(33,34)) ij = subset(ij,SID==1)
			#if(i %in% c(27,29,30,311,312,32,33)){
			a = joinPolys(ij,Isob100,operation='INT')
			if(i != 27) a = subset(a,SID==1)
			if(i == 27) a = subset(a,SID==2)
			#} else {
			#a = ij	
			#}
			a$LFA = i
			a$PID = i
			out[[j]] = a
		}

	LFAs = do.call(rbind,out)
hg = findPolys(anc$locsP,LFAs)
ahr = merge(hg,anc$locsP,by='EID')

#Pruning Temp data to 100m iso
#pdf('~/tmp/bnamLFA100isobath2real.pdf')
#Data within LFAs
outBnam = list()
m=0
for(i in io){
		m=m+1
		h = subset(ahr,PID == i)
		k = anc$bTs[which(anc$bTs[,1] %in% h$EID),]
		outBnam[[m]]=k[,2:ncol(k)]
		}		



##

#seasonal cycles within each outBnam
l = length(outBnam)
ts = anc$timeS
out = list()

for(i in 1:l){
	o = outBnam[[i]]
	s = seq(1,337,by=12)
	m=0
	t=c()
		for(j in 1:12){	
			t=c(t,mean(o[,s]))
			s=s+1
		}
	out[[i]]=t
}
os  = as.data.frame(do.call(rbind,out))
matplot(x=1:12,t(os),type='l')
rownames(os)=io		
names(os) = month(1:12,label=T)
write.csv(os,file='~/tmp/BnamClimatology.csv')

###time series trends within lFA

l = length(outBnam)
ts = anc$timeS
out = list()
out1 = list()

for(i in 1:l){
	o = outBnam[[i]]
	ioi = apply(o,2,mean)
#	op = decompose(ts(ioi,frequency=12),'additive')
	op = stl(ts(ioi,frequency=12),s.window=7)

	out[[i]] = op$time.series[,2]
	out1[[i]] = op$time.series[,1]
	}

out = do.call(rbind,out)
rownames(out)=io		

out1 = do.call(rbind,out1)
rownames(out1)=io		

write.csv(out,file='~/tmp/Bnamtrends.csv')
out = read.csv('~/tmp/Bnamtrends.csv')



###
 a = as.data.frame(cbind(io,do.call(rbind,lapply(outBnam,mean))))
 names(a) = c('LFA','bt')

	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	#LobsterMap('all')
plotPolys(L)
at = seq(1,8,length=10)
cols = colorRampPalette(c("darkblue","cyan","green", "yellow", "orange","darkred"), space = "Lab")
col.regions = cols(length(at)+1)
a$col = NA
for(i in 1:nrow(a)){
a$col[i] = col.regions[which.min(abs(at-a$bt[i]))]
}
a$PID = a$LFA

addPolys(L,polyProps=a)
contLegend("topright",lvls=round(at,2),Cont.data=data.frame(at=at,col=cols(length(at))),title="#/square km",inset=0.02,cex=0.8,bg='white')
	
#Size at Maturity
ALFA = c(23,24,25,261,262,27,29,30,311,312,32,33,34,35,36,38,511,512,513,514)
SAM =  c(70,72,76,78,76,73,82,78,72,72,89,97,97,96,98,94,88,88,83,83)