#SMB
require(bio.lobster)
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


a <- lm(WEIGHT_KG/1000~SYEAR,data=agS[20:22,])
qq=predict(a)
(qq[3]-qq[1])/qq[1]


a <- lm(NUM_OF_TRAPS/1000~SYEAR,data=agS[20:22,])
qq=predict(a)
(qq[3]-qq[1])/qq[1]

a <- lm(CPUE~SYEAR,data=agS[20:22,])
qq=predict(a)
(qq[3]-qq[1])/qq[1]

#grids

LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
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
for(i in 1:length(g)){
  gp[[i]] = Polygons(list(Polygon(g[[i]][,c('X','Y')])),unique(g1[[i]]$SID))
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
    a <- lm(WEIGHT_KG/1000~SYEAR,data=subset(trr, SYEAR %in% c(2018, 2019, 2020)))
    qq=predict(a)
    w = c(w,(qq[3]-qq[1])/qq[1])
    a <- lm(NUM_OF_TRAPS/1000~SYEAR,data=subset(trr, SYEAR %in% c(2020, 2018, 2019)))
    qq=predict(a)
    w = c(w,(qq[3]-qq[1])/qq[1])
    a <- lm(CPUE~SYEAR,data=subset(trr, SYEAR %in% c(2020, 2018, 2019)))
    qq=predict(a)
    w = c(w,(qq[3]-qq[1])/qq[1])
    out[[i]]=w
    }

oo = do.call(rbind,out)

hist(oo[,1]*100,main='Percent Change in Landings (2018-2020)','fd',xlab="")
abline(v=-54.6, col='red')

hist(oo[,2]*100,main='Percent Change in Effort (2018-2020)','fd',xlab="")
abline(v=-44.1,col='red')


hist(oo[,3]*100,main='Percent Change in CPUE (2018-2020)','fd',xlab="")
abline(v=-16.7,col='red')

median(oo[,3])


#survey data.
lobster.db('survey')

SL<-LobsterSurveyProcess(lfa="34", yrs=1996:2020, mths=c("Aug","Jul","Jun"), bin.size=5, Net='NEST',size.range=c(0,200),biomass=T)
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

plot2ax(x1=nA$YEAR,y1=nA$SET_ID,ylim1=c(0,155),x2=nS$YEAR,y2=nS$SET_ID, ylim2=c(0,8),type1='l',type2='l',ylab='NSets LFA34', y2lab = 'NSets SMB',col='black',col2='red')
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
SL<-LobsterSurveyProcess(lfa="34", yrs=1996:2020, mths=c("Aug","Jul","Jun"), bin.size=5, Net='NEST',size.range=c(0,200),sex=3,biomass=T)
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


gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/default -dNOPAUSE -dQUIET -dBATCH -dDetectDuplicateImages -dCompressFonts=true -r150 -sOutputFile=output.pdf SR_LFA\ 34_Post\ meetingEdits_FinalEdits.pdf ATIPResultsDec2020.pdf