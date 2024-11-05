# LFA 34 Script to create some figures for AC presentation. Others created by Adam through assessment update.

p = bio.lobster::load.environment()
require(devtools)
require(bio.lobster)
require(bio.utilities)
require(sf)
require(ggplot2)
la()


#la()
#If running script after the fishery year, run this line
#p$current.assessment.year=p$current.assessment.year-1


# define place for figures to go
figdir = file.path(project.datadirectory("bio.lobster","assessments","Updates","LFA34",p$current.assessment.year),"AC_Pres")
dir.create( figdir, recursive = TRUE, showWarnings = FALSE )
p$lfas = c("34") # specify lfas for data summary

setwd(figdir)

#If you only want to update logs for the last two years, run this:
#p$yr=p$current.assessment.year

# update data through ROracle
NewDataPull =F
#NewDataPull =T

if(NewDataPull){
  lobster.db('fsrs.redo')
  lobster.db('logs.redo')
  lobster.db('annual.landings.redo')
  lobster.db('seasonal.landings.redo')
  #lobster.db('vlog.redo')
  logs=lobster.db('process.logs.redo')
  per.rec= lobster.db("percent_reporting")
}

#Run a report of missing vs received logs and save a csv copy

fl.name=paste("percent_logs_reported", Sys.Date(),"csv", sep=".")
per.rec=per.rec[order(per.rec$YEARMTH),]
write.csv(per.rec, file=paste0(figdir,"/",fl.name),na="", row.names=F)

# Map ################

png(filename=file.path(figdir, "MapLFA33.png"),width=5, height=5, units = "in", res = 800)
LobsterMap('34')
dev.off()


# CPUE ###############

logs=lobster.db("process.logs")

#Choose one to redo or not Add TempSkip=T to not model CPUE with Temps
CPUE.data<-CPUEModelData2(p,redo=T)
#CPUE.data<-CPUEModelData2(p,redo=F)


cpueData= CPUEplot(CPUE.data,lfa= p$lfas,yrs=1981:max(p$current.assessment.year),graphic='R')$annual.data

crd = subset(cpueData,LFA==34,c("YEAR","CPUE"))
crd = crd[is.finite(crd$CPUE),]


png(filename=file.path(figdir, "CPUE_only.png"),width=8, height=5.5, units = "in", res = 800)
par(mar=c(2.0,5.5,2.0,3.0))
xlim=c(1990,max(crd$YEAR))
plot(crd[,1],crd[,2],xlab='Year',ylab='CPUE (kg/TH)',type='b',pch=16,xlim=xlim, ylim=c(0, 1.05*max(crd$CPUE)))
points(max(crd$YEAR), crd$CPUE[which(crd$YEAR==max(crd$YEAR))], pch=17, col='red', cex=1.2)
dev.off()


png(filename=file.path(figdir, "CPUE_LFA34.png"),width=8, height=5.5, units = "in", res = 800)
par(mar=c(4.0,5.5,2.0,3.0))
xlim=c(1990,max(crd$YEAR))
plot(crd[,1],crd[,2],xlab='Year',ylab='CPUE (kg/TH)',type='p',pch=16,xlim=xlim, ylim=c(0, 1.05*max(crd$CPUE)))
points(max(crd$YEAR), crd$CPUE[which(crd$YEAR==max(crd$YEAR))], pch=17, col='red', cex=1.2)
running.median = with(rmed(crd[,1],crd[,2]),data.frame(YEAR=yr,running.median=x))
crd=merge(crd,running.median,all=T)
lines(crd[,1],crd$running.median,col='blue',lty=1,lwd=2)
dev.off()

write.csv(crd,file.path(figdir,file='CatchRateRefs34.csv'))

#French Version of Figure:
png(filename=file.path(figdir, "CPUE_LFA33.French.png"),width=8, height=5.5, units = "in", res = 800)
par(mar=c(4.0,5.5,2.0,3.0))
xlim=c(1990,max(crd$YEAR))
plot(crd[,1],crd[,2],xlab='Annee',ylab='CPUE (kg/casier leve)',type='p',pch=16,xlim=xlim, ylim=c(0, 1.05*max(crd$CPUE)))
points(max(crd$YEAR), crd$CPUE[which(crd$YEAR==max(crd$YEAR))], pch=17, col='red', cex=1.2)
running.median = with(rmed(crd[,1],crd[,2]),data.frame(YEAR=yr,running.median=x))
crd=merge(crd,running.median,all=T)
lines(crd[,1],crd$running.median,col='blue',lty=1,lwd=2)
abline(h=usr,col='green',lwd=2,lty=2)
abline(h=lrp,col='red',lwd=2,lty=3)
dev.off()

# plot
#x11(width=8,height=5)
CatchRatePlot(data = crd ,usr = usr,lrp=lrp,lfa = 33,fd=figdir, save=F)

# Plots unbiased annual CPUE for all LFAs in Maritimes region
# Good for context in presentations at AC
{  
  a = lobster.db('process.logs')
  a = subset(a,SYEAR %in% 2004:p$current.assessment.year) 
  
  aa = split(a,f=list(a$LFA,a$SYEAR))
  cpue.lst<-list()
  m=0
  #by time
  for(i in 1:length(aa)){
    tmp<-aa[[i]]
    tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
    names(tmp)<-c('time','catch','effort')
    tmp$date<-as.Date(tmp$time)
    first.day<-min(tmp$date)
    tmp$time<-julian(tmp$date,origin=first.day-1)
    tmp$time = ceiling(tmp$time/7) #convert to week of season
    if(nrow(tmp)>5){
      m=m+1
      g<-as.data.frame(biasCorrCPUE(tmp,by.time=F))
      g$lfa=unique(aa[[i]]$LFA)
      g$yr = unique(aa[[i]]$SYEAR)
      g = t(g)[,2]
      cpue.lst[[m]] <- g
    }
  }
  cc =as.data.frame(do.call(rbind,cpue.lst))
  cc$CPUE = as.numeric(cc$`biasCorrCPUE(tmp, by.time = F)`)
  cc = cc[order(cc$lfa,cc$yr),]
  cc$yr = as.numeric(cc$yr)
  cc$fyr = as.factor(cc$yr)
  last_bar_color="black"
    point_colors <- ifelse(cc$yr <max(cc$yr), last_bar_color, "orange")
    cc1 = cc
    
    png(filename=file.path(figdir, "all_lfas_cpue.png"),width=8, height=5.5, units = "in", res = 800)
    ggplot(cc,aes(x=yr,y=CPUE))+geom_point()+
      geom_smooth(se=FALSE)+geom_point(data=cc1,aes(x=yr,y=CPUE,colour=fyr))+facet_wrap(~lfa,scales='free_y')+
      scale_colour_manual(values = point_colors)+theme(legend.position = 'none')+
      labs(y= "CPUE", x = "Year")
    dev.off()
   
}     

     

#Unbiased cpue patterns by week of season
#Will need to modify AC presentation to include these contextual CPUE figures
#-----------------------------------------
{   
  a = lobster.db('process.logs')
  a = subset(a,SYEAR %in% 2004:p$current.assessment.year & LFA %in% p$lfas)
  strt.yr=p$current.assessment.year-11
  a = subset(a,SYEAR %in% strt.yr:strt.yr:p$current.assessment.year & LFA %in% p$lfas) 
  
  aa = split(a,f=list(a$LFA,a$SYEAR))
  aa = rm.from.list(aa)
  cpue.lst<-list()
  
  
  aa = split(a,f=list(a$LFA,a$SYEAR))
  cpue.lst<-list()
  m=0
  #annual
  for(i in 1:length(aa)){
    tmp<-aa[[i]]
    tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
    names(tmp)<-c('time','catch','effort')
    tmp$date<-as.Date(tmp$time)
    first.day<-min(tmp$date)
    tmp$time<-julian(tmp$date,origin=first.day-1)
    tmp$time = ceiling(tmp$time/7) #convert to week of season
    if(nrow(tmp)>5){
      m=m+1
      g<-as.data.frame(biasCorrCPUE(tmp,by.time=F))
      g$lfa=unique(aa[[i]]$LFA)
      g$yr = unique(aa[[i]]$SYEAR)
      g = t(g)[,2]
      cpue.lst[[m]] <- g
    }
  }
  cc =as.data.frame(do.call(rbind,cpue.lst))
  cc$CPUE = as.numeric(cc$`biasCorrCPUE(tmp, by.time = F)`)
  cc = cc[order(cc$lfa,cc$yr),]
  cc$yr = as.numeric(cc$yr)
  cc$fyr = as.factor(cc$yr)
  
  cc1 = split(cc,f=cc$lfa)
  
  for(i in 1:length(cc1)){
    cc1[[i]]$mCPUE = as.numeric(with(cc1[[i]],rmed(yr,CPUE))$x)
  }
  
  cc2 = do.call(rbind,cc1)
  
  #ggplot(cc2,aes(x=yr,y=CPUE))+geom_point()+
  #  geom_line(aes(x=yr,y=mCPUE),colour='red',size=1.1)+facet_wrap(~lfa,scales='free_y')+geom_point(data=subset(cc2,yr==2023),aes(x=yr,y=CPUE),colour='orange',shape=16,size=2)
  
  ##by week
  aa = split(a,f=list(a$LFA,a$SYEAR))
  cpue.lst<-list()
  m=0
  
  aa = split(a,f=list(a$LFA,a$SYEAR))
  aa = rm.from.list(aa)
  cpue.lst<-list()
  
  #by time
  for(i in 1:length(aa)){
    tmp<-aa[[i]]
    tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
    names(tmp)<-c('time','catch','effort')
    tmp$date<-as.Date(tmp$time)
    first.day<-min(tmp$date)
    tmp$time<-julian(tmp$date,origin=first.day-1)
    tmp = tmp[order(tmp$time),]
    tmp$time = ceiling(tmp$time/7) #convert to week of season
    g<-as.data.frame(biasCorrCPUE(tmp,by.time=T,min.sample.size = 5))
    g$lfa=unique(aa[[i]]$LFA)
    g$yr = unique(aa[[i]]$SYEAR)
    # g = t(g)[,1]
    cpue.lst[[i]] <- g
  }
  
  cc =as.data.frame(do.call(rbind,cpue.lst))
  
  mean= aggregate(cc, CPUE~yr+lfa,mean )
  
  
  l=p$lfas
  png(filename=file.path(figdir, paste0("weekly_cpue_",l,".png")),width=8, height=5.5, units = "in", res = 800)
  print(
    ggplot(subset(cc,lfa==l),aes(x=t,y=CPUE))+geom_point()+
      geom_smooth(se=F)+facet_wrap(~yr)+
      labs(title =paste0("LFA ",l))+
      labs(y= "CPUE (kg/th)", x = "Week of Season") +
      theme(plot.title = element_text(hjust = 0.5))+
      # stat_summary(fun='mean', geom="line")
      geom_hline(data=mean[mean$lfa==l,], aes(yintercept= CPUE), color='red', linewidth=0.6)
    
  )
  dev.off()
}


# Landings and Effort ############
{
  land = lobster.db('seasonal.landings')
  
  
  #if running this section without having done the CPUE analysis during the same session, run 2 lines below 
  #CPUE.data<-CPUEModelData(p,redo=F)
  #cpueData=    CPUEplot(CPUE.data,lfa= p$lfas,yrs=1981:max(CPUE.data$SYEAR),graphic='R')$annual.data
  
  land$YEAR = as.numeric(substr(land$SYEAR,6,9))
  land$LANDINGS = land$LFA34
  fishData = merge(cpueData,land[,c("YEAR","LANDINGS")])
  fishData$EFFORT2 = fishData$LANDINGS * 1000 / fishData$CPUE
  fishData2=fishData
  fishData2$EFFORT2[fishData2$YEAR<2000]=NA
  
  
  
  # plot Landings
  
  png(filename=file.path(figdir, "Landings_LFA34.png"),width=8, height=5, units = "in", res = 800)
  par(mar=c(5.1, 4.1, 4.1, 5.1),las=1)
  plot(fishData$YEAR,fishData$LANDINGS,xlab='Year',ylab='Landings (t)',type='h',main="LFA 33",ylim=c(0,max(fishData$LANDINGS)*1.2),pch=15,col='grey',lwd=10,lend=3)
  lines(max(fishData$YEAR),fishData$LANDINGS[length(fishData$LANDINGS)],type='h',pch=21,col=rgb(1,0.6,0),lwd=10,lend=3)
  dev.off()
  
  # plot Landings / Effort Together
  
  png(filename=file.path(figdir, "Landings_Effort_LFA34.png"),width=8, height=5, units = "in", res = 800)
  #FisheryPlot <- function(data,lfa=NULL,fd=file.path(project.figuredirectory('bio.lobster','ReferencePoints')),title = paste('LFA',lfa),fn=paste0('FisheryPlot',lfa),preliminary=NULL,units='t',...) {
  par(mar=c(5.1, 5, 4.1, 5.1),las=1)
  plot(fishData$YEAR,fishData$LANDINGS,xlab='Year',ylab='Landings (t)',type='h',main="LFA 34",ylim=c(0,max(fishData$LANDINGS)*1.2),pch=15,col='grey',lwd=10,lend=3,mgp=c(4,1,0))
  lines(max(fishData$YEAR),fishData$LANDINGS[length(fishData$LANDINGS)],type='h',pch=21,col=rgb(1,0.6,0),lwd=10,lend=3)
  
  par(new=T)
  plot(fishData2$YEAR,fishData2$EFFORT2/1000,ylab='',xlab='', type='b', pch=16, axes=F,ylim=c(0,max(fishData2$EFFORT2/1000,na.rm=T)))
  points(max(fishData2$YEAR),fishData2$EFFORT2[length(fishData2$EFFORT)]/1000, type='b', pch=21,bg=rgb(1,0.6,0))
  axis(4)
  mtext("Effort ('000s Trap Hauls)", 4, 3.5, outer = F,las=0)

  write.csv(fishData,file.path(figdir,paste('FisheryPlot34.csv',sep='')))
  dev.off()
  

}

# to compare weekly fishing effort year to year (include in AC presentation if desired)

logs=lobster.db("process.logs")
logs34=logs[logs$LFA=="34",]
logs34$unique_days=paste(logs34$VR_NUMBER, logs34$DATE_FISHED, sep=':')

#-----------------------------------------------------------------------



# to plot weekly fishing effort year to year (include in AC presentation if desired)
png(filename=file.path(figdir, "Weekly_Comparison_effort.png"),width=8, height=5.5, units = "in", res = 1200)
days=aggregate(unique_days~WOS+SYEAR, data=logs34, length)
days.y0=days[days$SYEAR==max(days$SYEAR),]
days.y1=days[days$SYEAR==(max(days$SYEAR)-1),]
plot(x=days$WOS,y=days$unique_days, type='n', main= "Days Fished by Week", xlab="Week of Season", ylab="Days Fished", xlim=c(2,27), xaxt='n')
axis(side=1, at=seq(1,25,by=4.4), lab=c('Dec','Jan', 'Feb', "Mar", 'Apr', 'May'))
#plot past 10 years in light gray)
for (i in c(0:10)){
  day=days[days$SYEAR==(max(days$SYEAR)-i),]  
  lines(day$WOS, day$unique_days, col="gray83") 
}
lines(days.y0$WOS, days.y0$unique_days, col="red")
#text(paste(days.y0$SYEAR[1]), x=26, y=300, col="red", cex=1.5)
lines(days.y1$WOS, days.y1$unique_days, col="blue")
#text(paste(days.y1$SYEAR[1]), x=26, y=800, col="blue", cex=1.5)
legend(x=19, y=1200, lty=1,c(paste((max(days$SYEAR)-10), (max(days$SYEAR)-2), sep=" - "),days.y1$SYEAR[1],days.y0$SYEAR[1]), col=c("gray88", "blue", "red"), bty='n')
dev.off()

# to plot weekly fishing CPUE year to year (include in AC presentation if desired)
png(filename=file.path(figdir, "Weekly_Comparison_cpue.png"),width=8, height=5.5, units = "in", res = 1200)
days=aggregate(CPUE~WOS+SYEAR, data=logs34, median)
days.y0=days[days$SYEAR==max(days$SYEAR),]
days.y1=days[days$SYEAR==(max(days$SYEAR)-1),]
plot(x=days$WOS,y=days$CPUE, type='n', main= "Median CPUE by Week", xlab="Week of Season", ylab="CPUE (kg/trap)", xlim=c(2,27), xaxt='n')
for (i in c(0:10)){
  day=days[days$SYEAR==(max(days$SYEAR)-i),]  
  lines(day$WOS, day$CPUE, col="gray82")
}
axis(side=1, at=seq(1,25,by=4.4), lab=c('Dec','Jan', 'Feb', "Mar", 'Apr', 'May'))
lines(days.y0$WOS, days.y0$CPUE, col="red")
#text(paste(days.y0$SYEAR[1]), x=26, y=1, col="red", cex=1.5)
lines(days.y1$WOS, days.y1$CPUE, col="blue")
#text(paste(days.y1$SYEAR[1]), x=26, y=1.4, col="blue", cex=1.5)
legend(x=23, y=1.55,cex=0.8, lty=1,c(paste((max(days$SYEAR)-10), (max(days$SYEAR)-2), sep=" - "),days.y1$SYEAR[1],days.y0$SYEAR[1]), col=c("gray82", "blue", "red"), bty='n')
dev.off()




# Fishery footprint- Useful in comparing years, etc
#------------------------------------------------------------


layerDir=file.path(project.datadirectory("bio.lobster"), "data","maps")
r<-readRDS(file.path( layerDir,"GridPolysSF.rds"))
r = st_as_sf(r)

a =  lobster.db('process.logs')
a = subset(a,SYEAR>2004 & SYEAR<=p$current.assessment.year)
b = lobster.db('seasonal.landings')
b = subset(b,!is.na(SYEAR))
b$SYEAR = 1976:p$current.assessment.year
b$LFA38B <- NULL
b = subset(b,SYEAR>2004 & SYEAR<=p$current.assessment.year)
b = reshape(b,idvar='SYEAR', varying=list(2:6),direction='long')
num.yr=length(2005:p$current.assessment.year)
b$LFA=rep(c(33,34,35,36,38),each=num.yr)
b$time <- NULL
names(b)[1:2]=c('YR','SlipLand')


d = lobster.db('annual.landings')
d = subset(d,YR>2004 & YR<=p$current.assessment.year, select=c(YR,LFA27,LFA28,LFA29,LFA30,LFA31A,LFA31B,LFA32))
d = reshape(d,idvar='YR', varying=list(2:8),direction='long')
d$LFA=rep(c(27,28,29,30,'31A','31B',32),each=num.yr)
d$time <- NULL
names(d)[1:2]=c('YR','SlipLand')
bd = rbind(d,b)

bup = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SYEAR+LFA,data=a,FUN=sum)
bup$CPUE = bup$WEIGHT_KG/bup$NUM_OF_TRAPS
bAll = merge(bd,bup,by.x=c('YR','LFA'),by.y=c('SYEAR','LFA'))

sL= split(a,f=list(a$LFA, a$SYEAR))
sL = rm.from.list(sL)
cpue.lst<-list()
cpue.ann = list()

for(i in 1:length(sL)){
  tmp<-sL[[i]]
  tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
  names(tmp)<-c('time','catch','effort')
  tmp$date<-as.Date(tmp$time)
  first.day<-min(tmp$date)
  tmp$time<-julian(tmp$date,origin=first.day-1)
  g<-biasCorrCPUE(tmp,by.time = F)
  cpue.lst[[i]] <- c(lfa=unique(sL[[i]]$LFA),yr = unique(sL[[i]]$SYEAR),g)
}

cc =as.data.frame(do.call(rbind,cpue.lst))

cAll = merge(bAll,cc,by.x=c('LFA','YR'),by.y=c('lfa','yr'))

cAll$NTRAPs = cAll$SlipLand*1000/as.numeric(cAll$unBCPUE)
cAll$NTRAPSU = cAll$SlipLand*1000/as.numeric(cAll$l95)
cAll$NTRAPSL = cAll$SlipLand*1000/as.numeric(cAll$u95)


###########################################
#part the effort to grids

partEffort = list()

for(i in 1:length(sL)){
  tmp = sL[[i]]
  tTH = aggregate(NUM_OF_TRAPS~LFA,data=tmp,FUN=sum)
  tC = subset(cAll, LFA==unique(tmp$LFA) & YR == unique(tmp$SYEAR)) 
  pTH = aggregate(NUM_OF_TRAPS~GRID_NUM+LFA+SYEAR,data=tmp,FUN=sum)
  pTH$BTTH = pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPs
  pTH$BlTH = pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPSL
  pTH$BuTH = pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPSU
  
  partEffort[[i]] = pTH
}

partEffort = do.call(rbind, partEffort)

#pe = merge(partEffort,r,by.x=c('GRID_NUM','LFA'),by.y=c('GRID_NO','LFA'))

saveRDS(partEffort,'TrapHaulsWithinGrid.rds')


#############################################
# PartitionLandings to Grids

partLandings = list()

for(i in 1:length(sL)){
  tmp = sL[[i]]
  tTH = aggregate(WEIGHT_KG~LFA,data=tmp,FUN=sum)
  tC = subset(cAll, LFA==unique(tmp$LFA) & YR == unique(tmp$SYEAR)) 
  pTH = aggregate(WEIGHT_KG~GRID_NUM+LFA+SYEAR,data=tmp,FUN=sum)
  pTH$BL = pTH$WEIGHT_KG / (tTH$WEIGHT_KG )* (tC$SlipLand*1000)
  partLandings[[i]] = pTH
}

partLandings = do.call(rbind, partLandings)

saveRDS(partLandings,'LandingsWithinGrid.rds')

###################################################
##Licenses By Grid and Week

g = lobster.db('process.logs')
g = subset(g,SYEAR>2004 & SYEAR<=p$current.assessment.year)

gg = aggregate(SD_LOG_ID~LFA+GRID_NUM+SYEAR,data = g,FUN=function(x) length(unique(x)))

saveRDS(gg,'SDLOGSWithinGrid.rds')

#############merge
#Licenses By Grid and Week

g = lobster.db('process.logs')
g = subset(g,SYEAR>2004 & SYEAR<=p$current.assessment.year)

gKL = aggregate(LICENCE_ID~LFA+GRID_NUM+SYEAR,data = g,FUN=function(x) length(unique(x)))

saveRDS(gKL,'LicencesWithinCommunity.rds')

#############merge


Tot = merge(merge(merge(partEffort,partLandings),gg),gKL)

Tot = subset(Tot,select=c(SYEAR,LFA,GRID_NUM,BTTH,BL,SD_LOG_ID,LICENCE_ID))
names(Tot)= c('FishingYear','LFA','Grid','TrapHauls','Landings','Trips','NLics')
Tot$PrivacyScreen = ifelse(Tot$NLics>4,1,0)

# we lose 149
saveRDS(Tot,'PrivacyScreened_TrapHauls_Landings_Trips_Gridand.rds')

Tot = readRDS('PrivacyScreened_TrapHauls_Landings_Trips_Gridand.rds')
Tot$LFA = ifelse(Tot$LFA=='31B',312,Tot$LFA)
Tot$LFA = ifelse(Tot$LFA=='31A',311,Tot$LFA)


#making plots of Tot

GrMap = readRDS(file.path(project.datadirectory('bio.lobster'),'data','maps','LFA27-38GridsPrunedtoDepth-sf.rds'))
coa = st_as_sf(readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastlineSF_NY_NL.rds")))


GrMap1 = GrMap
GrMap1$area = st_area(GrMap1)/1000000
GrMap1$V2 = paste(GrMap1$LFA, GrMap1$GRID_NO,sep="-")
st_geometry(GrMap1)<- NULL
gg = aggregate(area~LFA+GRID_NO,data=GrMap1,FUN=function(x) abs(sum(x)))

GrMap2 =merge(GrMap,gg)

gTot = merge(GrMap2,Tot,by.x=c('LFA','GRID_NO'),by.y=c('LFA','Grid'),all.x=T)


r<-readRDS(file.path( layerDir,"GridPolysSF.rds"))
b=subset(r,LFA %in% c(27:33, 311, 312))

o=subset(GrMap,LFA %in% c(27:33, 311, 312))

ggplot(b)+
  geom_sf()+
  geom_sf(data=coa,fill='grey')+
  geom_sf(data=o,fill='red')+
  coord_sf(xlim = c(st_bbox(b)$xmin,st_bbox(b)$xmax),
           ylim = c(st_bbox(b)$ymin,st_bbox(b)$ymax),
           expand = FALSE)


gTot$CPUE = gTot$Landings/gTot$TrapHauls
g27p = subset(gTot, LFA%in% c(27:34, 311, 312) & FishingYear%in%2016:p$current.assessment.year)

ok1 = ggplot(g27p,aes(fill=CPUE))+
  geom_sf() +
  scale_fill_distiller(trans='identity',palette='Spectral') +
  facet_wrap(~FishingYear)+
  #  geom_sf(data=g27n,fill='white')+  
  geom_sf(data=coa,fill='grey')+
  geom_sf(data=GrMap,fill=NA)+
  coord_sf(xlim = c(st_bbox(g27p)$xmin,st_bbox(g27p)$xmax),
           ylim = c(st_bbox(g27p)$ymin,st_bbox(g27p)$ymax),
           expand = FALSE)+
  scale_x_continuous(breaks = c(round(seq(st_bbox(g27p)$xmin,st_bbox(g27p)$xmax,length.out=2),2)))+
  scale_y_continuous(breaks = c(round(seq(st_bbox(g27p)$ymin,st_bbox(g27p)$ymax,length.out=2),2)))

#Can run this line to only take certain LFAs
#g27p=g27p[g27p$LFA %in% c('34'),]

#Slice out individual years
gl = subset(g27p,FishingYear==p$current.assessment.year-1)

gp = subset(g27p,FishingYear==p$current.assessment.year)

gl$geometry<- NULL

gg = merge(gp,gl[,c('LFA','GRID_NO','CPUE')],by=c('LFA','GRID_NO'))


ls=unique(gg$LFA)
print(paste0('Looking at the following LFA(s):', ls,' for the following years: ', gl$FishingYear[1], ' & ',gp$FishingYear[1] ))

percent_diff <- function(row) {
  row$geometry<- NULL
  
  abs_diff <- (as.numeric(row[1]) - as.numeric(row[2]))
  mean_val <- mean(as.numeric(row))
  percent_diff <- (abs_diff / mean_val) * 100
  return(percent_diff)
}

gg$percentChange =  apply(gg[,c('CPUE.x','CPUE.y')],1,percent_diff)


require(colorspace)
lab=paste(gl$FishingYear[1], sprintf('\u2192'),gp$FishingYear[1], sep=" " )


cpue.diff={
  ggplot(subset(gg,PrivacyScreen==1),aes(fill=percentChange))+
    geom_sf() +
    scale_fill_continuous_diverging(palette='Purple-Green') +
    labs(fill = "    CPUE\n% Change")+
    #facet_wrap(~FishingYear)+
    #  geom_sf(data=g27n,fill='white')+  
    geom_sf(data=coa,fill='grey')+
    geom_sf(data=GrMap,fill=NA)+
    coord_sf(xlim = c(st_bbox(g27p)$xmin,st_bbox(g27p)$xmax),
             ylim = c(st_bbox(g27p)$ymin,st_bbox(g27p)$ymax),
             expand = FALSE)+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          plot.margin=grid::unit(c(8,2,8,2), "mm"))+
    scale_x_continuous(breaks = c(round(seq(st_bbox(g27p)$xmin,st_bbox(g27p)$xmax,length.out=2),2)))+
    scale_y_continuous(breaks = c(round(seq(st_bbox(g27p)$ymin,st_bbox(g27p)$ymax,length.out=2),2)))+
    annotate("text", x=(st_bbox(g27p)$xmax)-0.6, y=(st_bbox(g27p)$ymin)+0.2, label= lab, size=6 )
}

png(filename=file.path(figdir, "cpue.diff.png"), width=1600, height=900, res=175)
print(cpue.diff)
dev.off()	



