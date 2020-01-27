
p = bio.lobster::load.environment()

p$syr = 2005
p$yrs = p$syr:p$current.assessment.year

figdir = file.path(project.datadirectory("bio.lobster"),"figures","LFA3438Framework2019")

p$lfas = c("34","35","36","38") # specify lfas for data summary


#CPUE
 # specify lfas for data summary
logsInSeason<-lobster.db('process.logs')
cpueLFA.dat = CPUEplot(logsInSeason,lfa= p$lfas,yrs=2006:2018,graphic='pdf',path=figdir)


## Fishery Footprint - CPUE
cpueLevels = c(0,0.2,0.4,0.6,0.8,0.9,1,2,3)
yrs = 2014:2018
#logsInSeason$logCPUE = log(logsInSeason$CPUE+1)
for(i in 1:length(yrs)){
  cpuegrids = lobGridPlot(subset(logsInSeason,LFA%in%p$lfas&SYEAR == yrs[i],c("LFA","GRID_NUM","CPUE")),FUN=median,lvls=cpueLevels)	
   pdf(file.path(figdir,paste0("FFCPUE", yrs[i],".pdf")))
 # saveRDS(cpuegrids,file=file.path(fd,paste('Figure',yrs[i],'.rds')))
  #	
  LobsterMap(ylim=c(42.5,46), xlim=c(-67.8,-63), poly.lst=cpuegrids)
  title(yrs[i],line=-3,cex.main=2,adj=0.3)
  SpatialHub::contLegend('bottomright',lvls=cpuegrids$lvls,Cont.data=cpuegrids,title="CPUE (kg/TH)",inset=0.02,cex=0.8,bg='white')
  dev.off()
}


## Fishery Footprint - Mean Pots Hauled 
potLevels = c (0,1000,100000,200000,300000,400000,500000,600000)
yrs = 2014:2018
for(i in 1:length(yrs)){
  potgrids = lobGridPlot(subset(logsInSeason,LFA%in%p$lfas&SYEAR == yrs[i],c("LFA","GRID_NUM","NUM_OF_TRAPS")),FUN=sum,lvls=potLevels) 
  #saveRDS(potgrids,file=file.path(fd,paste('Figure4',yrs[i],'.rds')))
  
pdf(file.path(figdir,paste0("FFPotsHaul", yrs[i],".pdf")))
 
 LobsterMap(ylim=c(42.5,46), xlim=c(-67.8,-63),poly.lst=potgrids)
  title(yrs[i],line=-3,cex.main=2,adj=0.3)
  SpatialHub::contLegend('bottomright',lvls=potgrids$lvls/1000,Cont.data=potgrids,title="Pots Hauled (000s)",inset=0.02,cex=0.8,bg='white')
  dev.off()
}


## Fishery Footprint - Days Fished
daysLevels = c(0,500,1000,1500,2000,2500,3000)
daysFished<-aggregate(DATE_FISHED ~ SYEAR + LFA + GRID_NUM + LICENCE_ID, data=logsInSeason,FUN= function(x) length(unique(x)))	
yrs = 2014:2018
for (i in 1: length(yrs)){
  daysgrids = lobGridPlot(subset(daysFished, LFA%in%p$lfas&SYEAR == yrs[i],c("LFA", "GRID_NUM", "DATE_FISHED")),FUN=sum, lvls= daysLevels)
  #saveRDS(daysgrids,file=file.path(fd,paste('FFDaysFish',yrs[i],'.rds')))
  
  pdf(file.path(figdir,paste0('FFDaysFish', yrs[i],".pdf")))
  LobsterMap(ylim=c(42.5,46), xlim=c(-67.8,-63),poly.lst=daysgrids)
  title(yrs[i],line=-3,cex.main=2,adj=0.3)
  SpatialHub::contLegend('bottomright',lvls=daysgrids$lvls,Cont.data=daysgrids,title="Total Days Fished",inset=0.02,cex=0.8,bg='white')
  dev.off()
}


## Fishery Footprint - Licences Fished
licenceLevels = c(0,15,30,45,60,75,90,105,120)
yrs=2014:2018
daysFished$LICENCE<-1
for(i in 1: length(yrs)){
  licencegrids = lobGridPlot(subset(daysFished, LFA%in%p$lfas&SYEAR==yrs[i], c("LFA", "GRID_NUM", "LICENCE")), FUN=sum, lvls= licenceLevels)
  #saveRDS(licencegrids,file=file.path(fd,paste('Figure7',yrs[i],'.rds')))
  
  pdf(file.path(figdir,paste0("FFlicence", yrs[i],".pdf")))
  LobsterMap(ylim=c(42.5,46), xlim=c(-67.8,-63), poly.lst=licencegrids)
  title(yrs[i],line=-3,cex.main=2,adj=0.3)
  SpatialHub::contLegend('bottomright', lvls=licencegrids$lvls, Cont.data=licencegrids, title= "Number of Licence Fished", inset =0.02,cex=0.8,bg='white')
  dev.off()
}
