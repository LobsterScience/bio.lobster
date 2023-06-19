#SMB
require(bio.lobster)
require(devtools)

la()

##licence characteristics
a = lobster.db('licence_characteristics')
a = subset(a,LFA==34)


g = lobster.db('process.logs.unfiltered')
g = subset(g, LFA==34)
gG = unique(g$GRID_NUM)

gS = subset(g, GRID_NUM %in% c(69,81,92) & LFA ==34)
gF = subset(g, GRID_NUM %ni% c(69,81,92) & LFA == 34)

agS = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SYEAR,data=gS,FUN=sum)
agF = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SYEAR,data=gF,FUN=sum)

agS$CPUE = agS$WEIGHT_KG/agS$NUM_OF_TRAPS
plot(agS$SYEAR,agS$WEIGHT_KG)
plot(agS$SYEAR,agS$NUM_OF_TRAPS)
plot(agS$SYEAR,agS$CPUE)


a <- lm(WEIGHT_KG/1000~SYEAR,data=agS[22:24,])
qq=predict(a)
catSMB=(qq[3]-qq[1])/qq[1]


a <- lm(NUM_OF_TRAPS/1000~SYEAR,data=agS[22:24,])
qq=predict(a)
effSMB = (qq[3]-qq[1])/qq[1]

a <- lm(CPUE~SYEAR,data=agS[22:24,])
qq=predict(a)
cpuSMB = (qq[3]-qq[1])/qq[1]

#grids

LFAgrid <- read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
grL = subset(LFAgrid, PID==34)
grL$area=grL$PID
attr(grL,'projection') <- 'LL'

#grs is the grids
#making the list of connections to grids
require(sp)
require(spdep)
g1 = split(grL,f=grL$SID)
nm = c()
gp = list()
for(i in 1:length(g1)){
  gp[[i]] = Polygons(list(Polygon(g1[[i]][,c('X','Y')])),unique(g1[[i]]$SID))
}
gpp = SpatialPolygons(gp,proj4string=CRS("+proj=longlat +datum=WGS84"))
gpnb = poly2nb(gpp,row.names=names(gpp))
names(gpnb)=names(gpp)

out=list()

for(i in 1:length(gpnb)){
  w=c()
  gr = names(gpnb)[gpnb[[i]]]
  gr = sample(gr,3)
  tr = subset(g, GRID_NUM %in% gr )
    trr = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SYEAR,data=tr,FUN=sum)
    
    trr$CPUE = trr$WEIGHT_KG/trr$NUM_OF_TRAPS
    a <- lm(WEIGHT_KG/1000~SYEAR,data=subset(trr, SYEAR %in% c(2020, 2021, 2022)))
    qq=predict(a)
    w = c(w,(qq[3]-qq[1])/qq[1])
    a <- lm(NUM_OF_TRAPS/1000~SYEAR,data=subset(trr, SYEAR %in% c(2020, 2021, 2022)))
    qq=predict(a)
    w = c(w,(qq[3]-qq[1])/qq[1])
    a <- lm(CPUE~SYEAR,data=subset(trr, SYEAR %in% c(2020, 2021, 2022)))
    qq=predict(a)
    w = c(w,(qq[3]-qq[1])/qq[1])
    out[[i]]=w
    }

oo = do.call(rbind,out)

hist(oo[,1]*100,main='Percent Change in Landings (2020-2022)','fd',xlab="")
abline(v=catSMB*100, col='red')

hist(oo[,2]*100,main='Percent Change in Effort (2020-2022)','fd',xlab="")
abline(v=effSMB*100,col='red')

hist(oo[,3]*100,main='Percent Change in CPUE (2020-2022)','fd',xlab="")
abline(v=cpuSMB,col='red')

median(oo[,3])


#################
##matching vics for aquaculture sighting

a = lobster.db('process.logs.unfiltered')

gr = c(69,81,92)
#a$WEIGHT_KG = a$WEIGHT_KG*a$BUMPUP
gS = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~LFA+SYEAR+GRID_NUM,data=subset(a,GRID_NUM %in% gr & LFA ==34 & SYEAR %in% 2017:2022),FUN=sum)
names(gS)[4:5] = c('SMBWT','SMBNT')
gA = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~LFA+SYEAR,data=subset(a, LFA ==34& SYEAR %in% 2017:2022),FUN=sum)

gC = merge(gA,gS)
gC$preW = round(gC$SMBWT/gC$WEIGHT_KG*100,2)
gC$preT = round(gC$SMBNT/gC$NUM_OF_TRAPS*100,2)

gSp = aggregate(cbind(SMBWT,SMBNT)~GRID_NUM,data=subset(gS, SYEAR %in% 2020:2022),FUN=mean)

#nlicenses 
g5 = aggregate(SD_LOG_ID~LFA+SYEAR+GRID_NUM+LICENCE_ID,data=subset(a, LFA ==34 & SYEAR %in% 2017:2022),FUN=length)
g5a = aggregate(LICENCE_ID~SYEAR, data=subset(g5,GRID_NUM %in% gr), FUN=function(x) length(unique(x)))
aggregate(LICENCE_ID~GRID_NUM, data=subset(g5a,SYEAR %in% 2020:2022), FUN=mean)

####
sl = lobster.db('slips')
i = which(sl$PRICE<2)
j = which(sl$PRICE>30)
i = c(i,j)
sl$PRICE[i] = NA
sl = subset(sl, SPECIES_CODE==700 & NIL_REPORT_FLAG=='N') #nil reports still have to be submitted

sl$DYR = lubridate::decimal_date(as.Date(sl$DATE_LANDED)) - lubridate::year(as.Date(sl$DATE_LANDED))
sl$WYR = ceiling(sl$DYR*52)
sl$DWYR = lubridate::year(as.Date(sl$DATE_LANDED)) + sl$WYR/52
sl$MWYR = lubridate::year(as.Date(sl$DATE_LANDED)) + ceiling(sl$DYR*12)/12
sl$YR = lubridate::year(as.Date(sl$DATE_LANDED))

price.data = aggregate(PRICE~DWYR+LFA, data=sl, FUN=function(x) c(mean(x,na.rm=T)))
price.data2 = aggregate(PRICE~MWYR+LFA, data=sl, FUN=function(x) c(mean(x,na.rm=T)))
price.data3 = aggregate(PRICE~YR+LFA, data=sl, FUN=function(x) c(mean(x,na.rm=T)))

sll = bio.utilities::fillNaDf2(sl, price.data, mergeCols=c('DWYR','LFA'),fillCols=c('PRICE'))
slll = bio.utilities::fillNaDf2(sll, price.data2, mergeCols=c('MWYR','LFA'),fillCols=c('PRICE'))
sllll = bio.utilities::fillNaDf2(slll, price.data3, mergeCols=c('YR','LFA'),fillCols=c('PRICE'))
xx = aggregate(PRICE~DATE_LANDED+LFA,data=sllll,FUN=mean)
xx$DATE_LANDED = format(xx$DATE_LANDED,'%Y-%m-%d')

xx1 = merge(a,xx,by.x=c('DATE_FISHED','LFA'),by.y=c('DATE_LANDED','LFA'))

xx = subset(xx1,SYEAR>2010 & LFA==34)

xx$Land = xx$WEIGHT_KG * xx$BUMPUP
xx$value = xx$Land * xx$PRICE
xxa = aggregate(cbind(value,Land,NUM_OF_TRAPS)~SYEAR+LICENCE_ID,data=subset(xx,GRID_NUM %in% gr),FUN=sum)
names(xxa)[3:5]= c('SMBV','SBML','SMBT')
xxT = aggregate(cbind(value,Land,NUM_OF_TRAPS)~SYEAR+LICENCE_ID,data=subset(xx),FUN=sum)

xC = merge(xxa,xxT)

xC$Per = xC$SMBV/xC$value
ggplot(subset(xC,SYEAR<2023),aes(x=Per))+geom_histogram(binwidth=.10)+facet_wrap(~SYEAR)+xlab('Proportion of Lobster Income from SMB')+ylab('Frequency')

###
mp = read.csv(file.path(project.datadirectory('bio.lobster'),'data','maps','gridsWLand.csv'))
mp = makePBS(mp)
mp$X.1 = NULL
require(PBSmapping)
plotPolys(subset(mp,GridNo %in% gr))
mpi = convUL(mp)


###########
require(bio.lobster)
require(bio.utilities)
require(lubridate)
require(devtools)
require(ggplot2)
options(stringAsFactors=F)
la()
load_all('C:/Users/Cooka/Documents/git/bio.utilities')


ef = readRDS('results/LandingsPredictedActual.rds')
ef = bio.utilities::rename.df(ef,c('PID','SID'),c('LFA','GRID_NO'))
ef$Z = ef$Rediduals/1000
x =  subset(ef,LFA==33)
q = quantile(x$Z,probs=.99,na.rm=T)
x$Z[which(x$Z>q)] <- q
ux = c(min(x$Z),max(x$Z))
x$WOS = x$WOS-6

png('Figures/ModelOutput/LobsterPredictionsvLogs331-12.png',width=10,height=12,units='in',res=300)
ggLobsterMap('33',bathy=T,attrData = subset(x,LFA==33& WOS %in% 1:12),fw='WOS',legLab='Landings Difference (t)',addLFALabels = F,scaleTrans = 'identity' ,brks=ux)
dev.off()




####################################################################################################################################
##survey
#survey data.
lobster.db('survey')

SL<-LobsterSurveyProcess(lfa="L34", yrs=1996:2022, mths=c("Aug","Jul","Jun"), bin.size=5, Net='NEST',size.range=c(0,200),biomass=T)
LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
grL = subset(LFAgrid, PID==34)
grL$area=grL$PID
attr(grL,'projection') <- 'LL'
SMB = subset(grL, SID %in% c(69,81,92))
SMB$PID = SMB$SID
smb = joinPolys(SMB, operation = 'UNION')

SL$EID = 1:nrow(SL)
SL$X = SL$SET_LONG
SL$Y = SL$SET_LAT
Stsmb = findPolys(SL,smb)
Stsmb = subset(SL, EID %in% Stsmb$EID)

nS = aggregate(SET_ID~YEAR, data=Stsmb,FUN=length)
nA = aggregate(SET_ID~YEAR, data=SL,FUN=length)

plot2ax<-function(x1,y1,xlim1,ylim1,x2,y2,xlim2,ylim2,y2lab,type1="h",type2='o',pch1=1,pch2=2,col2='red',...) {
  if(!missing(y1)) {
    oldmar<-par("mar")
    par(mar=c(5,4,4,4))
    if(missing(x1)) x1<-1:length(y1)
    if(missing(xlim1)) xlim1<-range(x1)
    if(missing(ylim1)) ylim1<-range(y1)
    plot(x1,y1,xlim=xlim1,ylim=ylim1,pch=pch1,type=type1,...)
    if(!missing(y2)) {
      if(missing(x2)) x2<-1:length(y2)
      if(missing(xlim2)) xlim2<-range(x2)
      if(missing(ylim2)) ylim2<-range(pretty(y2))
      ax2val<-pretty(y2)
      xmult<-(xlim1[2] - xlim1[1])/(xlim2[2] - xlim2[1])
      ymult<-(ylim1[2] - ylim1[1])/(ylim2[2] - ylim2[1])
      axis(4,(ax2val-ylim2[1])*ymult+ylim1[1],labels=as.character(ax2val))
      points((x2-xlim2[1])*xmult+xlim1[1],(y2-ylim2[1])*ymult+ylim1[1],
             pch=pch2,type=type2,col=col2)
      if(!missing(y2lab)) mtext(y2lab,4,2)
    }
    par(oldmar)
  }
  else cat("Usage: plot2ax(x1,y1,xlim1,ylim1,x2,y2,xlim2,ylim2,y2lab,type=\"b\",pch1=1,pch2=2)\n")
}

plot2ax(x1=nA$YEAR,y1=nA$SET_ID,ylim1=c(0,163),x2=nS$YEAR,y2=nS$SET_ID, ylim2=c(0,8),type1='l',type2='l',ylab='NSets LFA34', y2lab = 'NSets SMB',col='black',col2='red')
legend('topleft',col=c('black','red'), lty=c(1,1),c('LFA 34','SMB'),bty='n')
savePlot(file.path(project.figuredirectory('bio.lobster'),'LFA34Update','NStations.png'))

mS = aggregate(NUM_STANDARDIZED~YEAR, data=Stsmb, FUN=mean)
mA = aggregate(NUM_STANDARDIZED~YEAR, data=SL, FUN=mean)

plot2ax(x1=mA$YEAR,y1=mA$NUM_STANDARDIZED,ylim1=c(0,100),x2=mS$YEAR,y2=mS$NUM_STANDARDIZED, ylim2=c(0,600),type1='l',type2='l',ylab='Mean Density Biomass LFA34', y2lab = 'Mean Density Biomass SMB',col='black',col2='red')
legend('topleft',col=c('black','red'), lty=c(1,1),c('LFA 34','SMB'),bty='n')
savePlot(file.path(project.figuredirectory('bio.lobster'),'LFA34Update','RawMeanPerTowLFA34SMB.png'))

aa = Stsmb[,c(1,2,grep('CL',names(Stsmb)))]
require(bio.utilities)
aa = na.zero(aa)
aa$NSizes = rowSums(aa>0)-2

sS = aggregate(NSizes~YEAR,data=subset(aa,YEAR>2012),FUN=mean)


bb = SL[,c(1,2,grep('CL',names(SL)))]
bb = na.zero(bb)
bb$NSizes = rowSums(bb>0)-2

sA = aggregate(NSizes~YEAR,data=subset(bb,YEAR>2012),FUN=mean)

plot2ax(x1=sA$YEAR,y1=sA$NSizes,ylim1=c(0,12),x2=sS$YEAR,y2=sS$NSizes, ylim2=c(0,23),type1='l',type2='l',ylab='NSizes LFA34', y2lab = 'NSizes SMB',col='black',col2='red')
legend('topleft',col=c('black','red'), lty=c(1,1),c('LFA 34','SMB'),bty='n')
savePlot(file.path(project.figuredirectory('bio.lobster'),'LFA34Update','NSizes.png'))


###
SL<-LobsterSurveyProcess(lfa="L34", yrs=1996:2022, mths=c("Aug","Jul","Jun"), bin.size=5, Net='NEST',size.range=c(0,200),sex=3,biomass=T)
LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
grL = subset(LFAgrid, PID==34)
grL$area=grL$PID
attr(grL,'projection') <- 'LL'
SMB = subset(grL, SID %in% c(69,81,92))
SMB$PID = SMB$SID
smb = joinPolys(SMB, operation = 'UNION')

SL$EID = 1:nrow(SL)
SL$X = SL$SET_LONG
SL$Y = SL$SET_LAT
Stsmb = findPolys(SL,smb)
Stsmb = subset(SL, EID %in% Stsmb$EID)

aggregate(SET_ID~YEAR, data=Stsmb,FUN=length)

plot(aggregate(NUM_STANDARDIZED~YEAR, data=Stsmb, FUN=mean))
lines(aggregate(NUM_STANDARDIZED~YEAR, data=SL, FUN=mean),col='red')

aa = Stsmb[,c(1,2,grep('CL',names(Stsmb)))]
require(bio.utilities)
aa = na.zero(aa)
aa$NBerried = rowSums(aa[,c(3:ncol(aa))])

bS = aggregate(NBerried~YEAR,data=aa,FUN=mean)


#total

ab = SL[,c(1,2,grep('CL',names(SL)))]
require(bio.utilities)
ab = na.zero(ab)
ab$NBerried = rowSums(ab[,c(3:ncol(ab))])
bA = aggregate(NBerried~YEAR,data=ab,FUN=mean)

bS = subset(bS, YEAR>2012)
bA = subset(bA, YEAR>2012)
plot2ax(x1=bA$YEAR,y1=bA$NBerried,ylim1=c(0,200),x2=bS$YEAR,y2=bS$NBerried, ylim2=c(0,1000),type1='l',type2='l',ylab='Mean Density of Berried LFA34', y2lab = 'Mean Density of Berried SMB',col='black',col2='red')
legend('topleft',col=c('black','red'), lty=c(1,1),c('LFA 34','SMB'),bty='n')
savePlot(file.path(project.figuredirectory('bio.lobster'),'LFA34Update','NBerried.png'))


