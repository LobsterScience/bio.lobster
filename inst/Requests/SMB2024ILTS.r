#SMB
require(bio.lobster)
require(devtools)
require(ggplot2)
require(sf)
la()
#####raw survey

lobster.db('survey')
sc = st_as_sf(surveyCatch,coords=c('SET_LONG','SET_LAT'),crs=4326)
ggplot(subset(gr,GRID_NO %in% c(69,81,92)))+geom_sf()+geom_sf(data=subset(sc,STATION %in%c(3,4,5,103,113,116) & YEAR>2016))
ss = subset(sc,YEAR>2013 & SPECCD_ID==2550 & LFA=='L34' & GEAR=='NEST')
# Step 1: Rank values within each subset
ss_ranked <- ss %>%
  group_by(YEAR) %>%
  mutate(ranking = rank(-NUM_CAUGHT))  # Ranking in descending order (change if needed)

ssR <- ss_ranked %>%
  filter(STATION %in% c(3,4,5,103,113,116)) 

ssr = merge(ssR,xx,by='YEAR')
ssr$pro = 100-ssr$ranking/ssr$SET_ID.y*100
# Print the ranking of the specific group

ggplot(ssr,aes(x=YEAR,y=pro,group=STATION,colour=STATION))+geom_line()+theme_test(base_size = 14)+labs(x='Year',y='Standardized Rank')
ss$x = st_coordinates(ss)[,1]
ss$y = st_coordinates(ss)[,2]

ggplot(subset(gr,LFA==34))+geom_sf()+geom_sf(data=subset(sc,STATION %in%c(3,4,5,103,113,116) & YEAR==2022),colour='red')+
  geom_sf(data=subset(ss,STATION %ni%c(3,4,5,103,113,116) & YEAR==2022),colour='blue')+
  geom_text(data=subset(ss,STATION %in%c(3) & YEAR==2022),nudge_x = .02,nudge_y = .02, aes(x=x,y=y,label='3'))+
  geom_text(data=subset(ss,STATION %in%c(4) & YEAR==2022),nudge_x = .02,nudge_y = .02, aes(x=x,y=y,label='4'))+
  geom_text(data=subset(ss,STATION %in%c(5) & YEAR==2022),nudge_x = .02,nudge_y = .02, aes(x=x,y=y,label='5'))+
  geom_text(data=subset(ss,STATION %in%c(103) & YEAR==2022),nudge_x = .02,nudge_y = .02, aes(x=x,y=y,label='103'))+
  geom_text(data=subset(ss,STATION %in%c(113) & YEAR==2022),nudge_x = .02,nudge_y = .02, aes(x=x,y=y,label='113'))+
  geom_text(data=subset(ss,STATION %in%c(116) & YEAR==2022),nudge_x = .02,nudge_y = .02, aes(x=x,y=y,label='116'))+
  theme_test_adam()


####################################################################################################################################
##survey
#survey data.
y = ILTS_ITQ_All_Data(redo_base_data = F,size=c(0,300),aggregate=F,species=2550,biomass = F)
y = subset(y,YEAR>=2017)
ys = st_as_sf(y,coords=c('SET_LONG','SET_LAT'),crs=4326)
gr = readRDS(file.path( bio.directory,"bio.lobster.data", "mapping_data","GridPolys_DepthPruned_37Split.rds"))

allI = st_intersects(ys,gr)
ys$Grid <- unlist(sapply(allI, function(x) if(length(x) > 0) gr$GRID_NO[x[1]] else NA))

 ys = subset(ys,LFA=='L34')
 
 xx = aggregate(SET_ID~YEAR, data=subset(ys),FUN= function(x) length(unique(x)))
 
 nS = aggregate(SET_ID~YEAR, data=subset(ys,STATION %in% c(3,4,5,103,113,116 )),FUN= function(x) length(unique(x)))
 nNS = aggregate(SET_ID~YEAR, data=subset(ys,Grid %ni% c(3,4,5,103,113,116)),FUN= function(x) length(unique(x)))

#total lobsters
 gg = aggregate(SA_CORRECTED_PRORATED_N~Grid+YEAR+SET_ID+STATION,data=ys,FUN=sum) 
 
 # Step 1: Rank values within each subset
 gg_ranked <- gg %>%
   group_by(YEAR) %>%
   mutate(ranking = rank(-SA_CORRECTED_PRORATED_N))  # Ranking in descending order (change if needed)
 
 ggR <- gg_ranked %>%
   filter(STATION %in% c(3,4,5,103,113,116)) 
 
 ggr = merge(ggR,xx,by='YEAR')
 
 # Print the ranking of the specific group
 print(specific_group_ranking)
 
 
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


