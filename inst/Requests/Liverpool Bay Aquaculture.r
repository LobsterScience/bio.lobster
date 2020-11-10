
p = bio.lobster::load.environment()

#require(PBSmapping)
#require(bio.utilities)


 a = importShapefile('~/tmp/Site_FarmersLedge',readDBF=T) 
 b = importShapefile('~/tmp/PEZ_FarmersLedge4748m',readDBF=T) 
 attr(b,'projection') <- "LL"                   
 LobsterMap('33', labels= c('grid', 'lfa')) 
 addPolys(b,border='red')


	log = lobster.db('process.logs') 
	log33 = subset(log,LFA==33)
	
	NLics = aggregate(LICENCE_ID~SYEAR+GRID_NUM,data=log33,FUN=function(x) length(unique(x)))
	NLicsT = aggregate(LICENCE_ID~SYEAR,data=log33,FUN=function(x) length(unique(x)))
	rownames(NLicsT) = NLicsT$SYEAR
	NLicsT$SYEAR = NULL
	NLicsT = c(t(NLicsT[,1]))

	NLics = reshape(NLics,idvar='SYEAR',timevar='GRID_NUM',direction='wide')
	NLics = NLics[order(NLics$SYEAR),]
	rownames(NLics) = NLics$SYEAR
	NLics$SYEAR = NULL
	NLics = as.matrix(NLics)
	
	PropLics = na.zero(round(NLics / NLicsT,2))

	PropLicsT = apply(PropLics,2,mean,na.rm=T)
	#Proportion of licence holders with valid logs reporting fishing by grid in LFA 33
	PropLicsT[order(PropLicsT,decreasing=T)]

#Total landings
	
	L = aggregate(WEIGHT_KG~SYEAR+GRID_NUM,data=log33,FUN=sum)
	LT = aggregate(WEIGHT_KG~SYEAR,data=log33,FUN=sum)
	rownames(LT) = LT$SYEAR
	LT$SYEAR = NULL
	LT = c(t(LT[,1]))

	L = reshape(L,idvar='SYEAR',timevar='GRID_NUM',direction='wide')
	L = L[order(L$SYEAR),]
	rownames(L) = L$SYEAR
	L$SYEAR = NULL
	L = as.matrix(L)
	
	PropL = na.zero(round(L / LT,2))

	PropLT = apply(PropL,2,mean,na.rm=T)
	#Proportion of landings from logs reporting fishing by grid in LFA 38
	PropLT[order(PropLT,decreasing=T)]

#at sea samples
	cc = lobster.db('cris')


gg = merge(cris.samples[,1:18],cris.traps[,1:14],by=c('TRIPNO','PORT','TRAPNO'))

gg = merge(gg,cris.trips[,1:10],by=c('TRIPNO','PORT'))
gg$I = 1
gg = merge(gg,cc$crports.csv[,1:6],by='PORT')
gg = merge(gg,cc$crlfas.csv[,1:3],by='LFA_ID')

g38 = subset(gg,LFA==33)
g38$yr = year(g38$TRAPDATE)
g38$mon = month(g38$TRAPDATE)

g38 = makePBS(g38,polygon=F)
g38 = subset(g38, !is.na(X) | !is.na(Y))                    
 LobsterMap('38',label=c('grid','lfa'))
 addPolys(b,border='red')
 addPoints(g38,pch='.',col='blue')
 savePlot('~/tmp/LFA38samples.png')

gl = findPolys(g38,b)$EID
##bys sex

#Inside
inC = subset(g38, EID %in% gl)

ss = aggregate(SAMPLE_ID~yr+mon+SEX,data=subset(inC,SPECIESCODE==1),FUN=length)
s = aggregate(SAMPLE_ID~yr+mon,data=subset(inC,SPECIESCODE==1),FUN=length)

s2 = merge(ss,s,all.x=T,by=c('yr','mon'))
s2$prop = s2$SAMPLE_ID.x / s2$SAMPLE_ID.y


ss = aggregate(SAMPLE_ID~mon+SEX,data=subset(inC,SPECIESCODE==1),FUN=length)
s = aggregate(SAMPLE_ID~mon,data=subset(inC,SPECIESCODE==1),FUN=length)

s2 = merge(ss,s,all.x=T,by=c('mon'))
s2$prop = s2$SAMPLE_ID.x / s2$SAMPLE_ID.y
s2 = subset(s2,SAMPLE_ID.y>50)
require(ggplot)
s2$SEX = as.character(s2$SEX)
ggplot(s2,aes(fill=SEX,y=prop,x=as.character(mon))) + geom_bar(position='stack',stat='identity')+ scale_fill_viridis(discrete = T)+xlab("Month")+ylab("Proportion of Total Sample")
savePlot('~/tmp/LFA38inside.png')


#outside

gl = findPolys(g38,b)$EID
inC = subset(g38, EID %ni% gl & SEX<4)
ss = aggregate(SAMPLE_ID~yr+mon+SEX,data=subset(inC,SPECIESCODE==1),FUN=length)
s = aggregate(SAMPLE_ID~yr+mon,data=subset(inC,SPECIESCODE==1),FUN=length)

s2 = merge(ss,s,all.x=T,by=c('yr','mon'))
s2$prop = s2$SAMPLE_ID.x / s2$SAMPLE_ID.y


ss = aggregate(SAMPLE_ID~mon+SEX,data=subset(inC,SPECIESCODE==1),FUN=length)
s = aggregate(SAMPLE_ID~mon,data=subset(inC,SPECIESCODE==1),FUN=length)

s2 = merge(ss,s,all.x=T,by=c('mon'))
s2$prop = s2$SAMPLE_ID.x / s2$SAMPLE_ID.y
s2 = subset(s2,SAMPLE_ID.y>50)
s2$SEX = as.character(s2$SEX)
ggplot(s2,aes(fill=SEX,y=prop,x=as.character(mon))) + geom_bar(position='stack',stat='identity')+ scale_fill_viridis(discrete = T)+xlab("Month")+ylab("Proportion of Total Sample")
savePlot('~/tmp/LFA38outside.png')


#by length
##bys sex

#Inside
inC = subset(g38, EID %in% gl)
data_summary <- function(x) {
   m <- mean(x)
   ymin <- m-sd(x)
   ymax <- m+sd(x)
   return(c(y=m,ymin=ymin,ymax=ymax))
}
ggplot(inC,aes(x=as.factor(mon),y=CARLENGTH))+geom_violin() +  stat_summary(fun.data=data_summary, color="red")+xlab("Month")+ylab("Median Carapace Length (mm)")
savePlot('~/tmp/LFA38insideCL.png')


#outside
inC = subset(g38, EID %ni% gl)
data_summary <- function(x) {
   m <- mean(x)
   ymin <- m-sd(x)
   ymax <- m+sd(x)
   return(c(y=m,ymin=ymin,ymax=ymax))
}
ggplot(inC,aes(x=as.factor(mon),y=CARLENGTH))+geom_violin() +  stat_summary(fun.data=data_summary, color="red")+xlab("Month")+ylab("Median Carapace Length (mm)")
savePlot('~/tmp/LFA38outsideCL.png')


#footprint maps
   
        p$lfas = c("38") # specify lfas for data summary
 #landings 
    logsInSeason<-lobster.db('process.logs')
catchLevels = c(0,5,10,15,20,25,30,35,40)
	
		catchgrids = lobGridPlot(subset(logsInSeason,LFA%in%p$lfas&SYEAR==2018,c("LFA","GRID_NUM","LICENCE_ID")),FUN='LU',lvls=catchLevels)
		LobsterMap('38',poly.lst=catchgrids)
	  	SpatialHub::contLegend('bottomright',lvls=catchgrids$lvls,Cont.data=catchgrids,title="Licenses",inset=0.02,cex=0.8,bg='white')
	
savePlot('~/tmp/LFA38Licences.png')

#landings
catchLevels = c(2100,15000,30000,60000,90000,150000,250000,500000)
	
		catchgrids = lobGridPlot(subset(logsInSeason,LFA%in%p$lfas&SYEAR==2018,c("LFA","GRID_NUM","WEIGHT_KG")),FUN='sum',lvls=catchLevels)
		LobsterMap('38',poly.lst=catchgrids)
	  	SpatialHub::contLegend('bottomright',lvls=catchgrids$lvls/1000,Cont.data=catchgrids,title="Landings (t)",inset=0.02,cex=0.8,bg='white')
	
savePlot('~/tmp/LFA38Landings.png')
