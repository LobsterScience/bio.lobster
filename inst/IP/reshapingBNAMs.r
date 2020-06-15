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
Isob100=g #full area
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
		LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
		sLFAs = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","sGSLLFAPolys.csv"))
		sLFAs$X.1 = NULL

		L = LFAs = rbind(LFAs,sLFAs)
		out=list()
		j=0
		io = c(23,24,25,261,262,27,29,30,311,312,32,33,34,35,36,38)
for(i in io){
			j = j+1
			ij = subset(LFAs,PID==i)
			if(i %in% c(24,261,262,27,29,30,311,312,32)){
			a = joinPolys(ij,Isob100,operation='INT')
			if(i != 27) a = subset(a,SID==1)
			if(i == 27) a = subset(a,SID==5)
			} else {
			a = ij	
			}
			a$LFA = i
			out[[j]] = a
		}

	LFAs = do.call(rbind,out)
LFAs$PID = LFAs$PID+LFAs$LFA

Lu = as.data.frame(unique(cbind(LFAs$PID, LFAs$LFA)))
names(Lu) = c('PID','LFA')
hg = findPolys(anc$locsP,LFAs)
loc = merge(hg,anc$locsP,by='EID')
ahr = merge(loc,Lu,all.x=T,by='PID')

#Pruning Temp data to 100m iso
hgg = findPolys(TempData,LFAs,maxRows=1e6)
locc = merge(hgg,TempData,by='EID')
locc$LFA = NULL
ahra = merge(locc,Lu,all.x=T,by='PID')
#pdf('~/tmp/bnamLFA100isobath2real.pdf')
#Data within LFAs
outBnam = list()
m=0
for(i in io){
		m=m+1
		h = subset(ahr,LFA == i)
		k = anc$bTs[which(anc$bTs[,1] %in% h$EID),]
		outBnam[[m]]=k[,2:ncol(k)]
		g = subset(ahra,LFA == i)
		if(nrow(g)<1) next()
 		g$id = paste(g$mon,g$yr,sep='--')
 		sd = aggregate(TEMPERATURE~id,data=g,FUN=length)
 		sds = subset(sd,TEMPERATURE>30)
 		sd = aggregate(TEMPERATURE~yr+mon,data=subset(g,id %in% sds$id),FUN=mean)
 		sd$decyr = sd$yr+sd$mon/12
		sd = sd[order(sd$decyr),]
 		if(i==311) i='31A'
 		if(i==312 	) i='31B'
		LobsterMap(as.character(i))
		addPoints(h[,c('EID','X','Y')],col='black',pch=16)
		addPoints(subset(g, id %in% sds$id,select=c(X,Y,EID)),pch=16,col='red')
		fp = file.path('~','tmp')
		savePlot(file.path(fp,paste('bnamMap.',i,'.png',sep="")),type='png')

		matplot(x=anc$timeS,t(k[,2:ncol(k)]),type='l',xlim=c(1999,2019))
		points(sd$decyr,sd$TEMPERATURE,pch=16,col='red')
	
		title(paste('LFA',i,sep=" "))
		savePlot(file.path(fp,paste('bnamSeps.',i,'.png',sep="")),type='png')
 		
 		plot(anc$timeS,apply(k[,2:ncol(k)],2,mean),type='l',xlim=c(1999,2019))
 		
		points(sd$decyr,sd$TEMPERATURE,pch=16,col='red')
		title(paste('LFA',i,sep=" "))
		savePlot(file.path(fp,paste('bnamMeans.',i,'.png',sep="")),type='png')

		}

dev.off()


##
 a = as.data.frame(cbind(io,do.call(rbind,lapply(outBnam,mean))))
 names(a) = c('LFA','bt')

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

for(i in 1:l){
	o = outBnam[[i]]
	ioi = apply(o,2,mean)
	op = decompose(ts(ioi,frequency=12),'additive')
	out[[i]] = op$trend
	}

out = do.call(rbind,out)
rownames(os)=io		
write.csv(os,file='~/tmp/Bnamtrends.csv')




###

	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	LobsterMap('all')
at = seq(1,8,length=50)
cols = colorRampPalette(c("darkblue","cyan","green", "yellow", "orange","darkred", "black"), space = "Lab")
col.regions = cols(length(at)+1)
a$col = NA
for(i in 1:nrow(a)){
a$col[i] = col.regions[which.min(abs(at-a$bt[i]))]
}
a$PID = a$LFA

addPolys(LFAs,polyProps=a)
contLegend("topright",lvls=at,Cont.data=cont.lst$Cont.data,title="#/square km",inset=0.02,cex=0.8,bg='white')
	
