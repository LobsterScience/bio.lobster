p = bio.lobster::load.environment()
require(SpatialHub)
require(lubridate)

#la()

assessment.year = p$current.assessment.year 
p$current.assessment.year = p$current.assessment.year - 1 

figdir = file.path(project.datadirectory("bio.lobster","assessments","MRLAC",assessment.year))
dir.create( figdir, recursive = TRUE, showWarnings = FALSE )

p$lfas = c("27", "28", "29", "30", "31A", "31B", "32", "33") # specify lfas for data summary
p$subareas = c("27N","27S", "28", "29", "30", "31A", "31B", "32", "33E", "33W") #lfas for data summary


png(filename=file.path(figdir, "MapLFA27-33.png") ,width=6.5, height=6.5, units = "in", res = 800)
LobsterMap('27-33', labels=c('lfa','grid'), grid.labcex=0.6)
dev.off()

CPUE.data<-CPUEModelData(p,redo=F, TempSkip=T) #Given recent assessment updates, shouldn't need a redo

cpueData=CPUEplot(CPUE.data,lfa= p$lfas,yrs=1981:max(CPUE.data$SYEAR), graphic='R')$annual.data
cpueData=cpueData[cpueData$YEAR<=p$current.assessment.year,]

#add lrp and USR

cpueData$usr=NA
cpueData$lrp=NA

for (l in p$lfas){
mu=median(cpueData$CPUE[cpueData$YEAR %in% c(1985:2009) & cpueData$LFA==l])
cpueData$usr[cpueData$LFA==l]=0.8*mu
cpueData$lrp[cpueData$LFA==l]=0.4*mu
}

ls=c('27', '28', '29', '30')
ls2=c('31A', '31B', '32', '33')

xlim=c(1985,p$current.assessment.year)

crplot= function(x, French=F){
  crd = subset(cpueData,LFA==l,c("YEAR","CPUE"))	
  mu = median(crd$CPUE[crd$YEAR %in% c(1985:2009)])
  usr = mu * 0.8
  lrp = mu * 0.4
  crd  = merge(data.frame(YEAR=min(crd$YEAR):max(crd$YEAR)),crd,all.x=T)
  
  par(mar=c(3.0,5.0,2.0,2.0))	
  
  ylab='CPUE (kg/TH)'
  if (French){ylab='CPUE (kg/casier levé)'}
  plot(crd[,1],crd[,2],xlab=' ',ylab=ylab,type='p',pch=16, xlim=xlim, ylim=c(lrp-.1,1.05*(max(crd$CPUE, na.rm = TRUE)) ))
  running.median = with(rmed(crd[,1],crd[,2]),data.frame(YEAR=yr,running.median=x))
  crd=merge(crd,running.median,all=T)
  lines(crd[,1],crd$running.median,col='blue',lty=1,lwd=2)
  abline(h=usr,col='green',lwd=2,lty=2)
  abline(h=lrp,col='red',lwd=2,lty=3)
  text(x=1988, y= max(crd$CPUE, na.rm = TRUE), l, cex=2) 
}



# Begin first CPUE figure (27, 28, 29, 30)
png(filename=file.path(figdir, "CPUE_LFA27-30.png"),width=8, height=5.5, units = "in", res = 800)
par(mfrow=c(2,2))		
  for (l in ls) {
    crplot(French=F) #Change to crplot(French=T) to produce French axis labels
    }
dev.off()

# Begin second CPUE figure 31A, 31B, 32 then add 33
png(filename=file.path(figdir, "CPUE_LFA31A-33.png"),width=8, height=5.5, units = "in", res = 800)
par(mfrow=c(2,2))		
for (l in ls2) {
  crplot()
}
dev.off()


## EXploitation PLots

load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR2732.rdata'))

ex33=read.csv(file.path(project.datadirectory("bio.lobster","assessments","Updates","LFA33",p$current.assessment.year, "LFA33ccirout.csv")))
ex33=ex33[,-1]
ex33$LFA="33"
oo=rbind(oo, ex33)

RR75  = aggregate(ERf75~LFA,data=oo,FUN=max)
for(i in oo$LFA){
oo$RR75[oo$LFA==i]=RR75$ERf75[RR75$LFA==i]
}


png(filename=file.path(figdir, "exploitation.27-31A.png"),width=10, height=7, units = "in", res = 800)
  par(mfrow=c(2,2))	
  for(i in c("27", "29", "30", "31A")){
      o = subset(oo,LFA==i)
      RR7 = subset(RR75,LFA==i)$ERf75
      ExploitationRatePlots(data = o[,c("Yr","ERfm","ERfl","ERfu")],lrp=RR7,lfa = i,fd=figdir, save=F, title=i) 
      }
dev.off()
  
png(filename=file.path(figdir, "exploitation.31B-33.png"),width=10, height=7, units = "in", res = 800)
par(mfrow=c(2,2))	
for(i in c("31B", "32", "33")){
  o = subset(oo,LFA==i)
  RR7 = subset(RR75,LFA==i)$ERf75
  ExploitationRatePlots(data = o[,c("Yr","ERfm","ERfl","ERfu")],lrp=RR7,lfa = i,fd=figdir, save=F, title=i) 
}
dev.off()

