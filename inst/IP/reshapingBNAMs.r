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
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
		LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
		out=list()
		j=0
		io = c(27,29,30,311,312,32)
for(i in io){
			j = j+1
			ij = subset(LFAs,PID==i)
			a = joinPolys(ij,Isob100,operation='INT')
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
hgg = findPolys(TempData,LFAs)
locc = merge(hgg,TempData,by='EID')
locc$LFA = NULL
ahra = merge(locc,Lu,all.x=T,by='PID')
pdf('~/tmp/bnamLFA100isobath2real.pdf')
#Data within LFAs
for(i in io){
		h = subset(ahr,LFA == i)
		k = anc$bTs[which(anc$bTs[,1] %in% h$EID),]
		g = subset(ahra,LFA == i)
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

		matplot(x=anc$timeS,t(k[,2:ncol(k)]),type='l',xlim=c(1999,2019))
		points(sd$decyr,sd$TEMPERATURE,pch=16,col='red')
	
		title(paste('LFA',i,sep=" "))
 		
 		plot(anc$timeS,apply(k[,2:ncol(k)],2,mean),type='l',xlim=c(1999,2019))
 		
		points(sd$decyr,sd$TEMPERATURE,pch=16,col='red')
		title(paste('LFA',i,sep=" "))

		}

dev.off()


#trying sf
library(sf) 	
library(magrittr)
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

			df <- st_as_sf(x = anc$locsP,                         
			           coords = c("X", "Y"),
			           crs = projcrs)

			bts = anc$bTs

			p0 = st_polygon(list(as.matrix(l348[,c('X','Y')])))
			p1 = st_sfc(p0)
			st_crs(p1) <- projcrs

			ints = st_intersects(p0,df)

			df[which(df$EID %in% ints[[1]]),]

			plot(df[which(df$EID %in% ints[[1]]),'Depth'])

			a = bts[which(bts[,1] %in% ints[[1]]),]
			#plot the results
				matplot(x=anc$timeS,t(a[,2:ncol(a)]),type='l')

			#some highly variable 
				vA = a[unique(which(a[,2:ncol(a)]>15,arr.ind=T)[,1]),1]
			g = df %>% subset(.,EID %in% vA)


plot(df[which(df$EID %in% ints[[1]]),'Depth'], add=T,pch=16,col='red')