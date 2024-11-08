require(bio.lobster)
require(SpatialHub)
require(lubridate)
require(bio.utilities)
require(ggplot2)
require(sf)

p = bio.lobster::load.environment()


la()

#Choose one
#assessment.year = p$current.assessment.year 
assessment.year = p$current.assessment.year-1 

#If you only want to update logs and CCIR for the last two years, run this:
#p$yr=p$current.assessment.year

figdir = file.path(project.datadirectory("bio.lobster","assessments","Updates","LFA27-32",assessment.year))
dir.create( figdir, recursive = TRUE, showWarnings = FALSE )

cpue.dir=file.path(figdir, "cpue")
dir.create( cpue.dir, recursive = TRUE, showWarnings = TRUE )

p$lfas = c("27", "28", "29", "30", "31A", "31B", "32") # specify lfas for data summary
p$subareas = c("27N","27S", "28", "29", "30", "31A", "31B", "32") # specify lfas for data summary

#If you only want to update logs for the last two years, run this:
#p$yr=p$current.assessment.year

# update data through ROracle
NewDataPull =F
if(NewDataPull){
  lobster.db('fsrs.redo')
  lobster.db('logs.redo')
  lobster.db('annual.landings.redo')
  #lobster.db('vlog.redo') #These are static now, no need to update
  logs=lobster.db('season.dates.redo') #updates season dates as required
  logs=lobster.db('process.logs.redo')
  per.rec= lobster.db("percent_reporting")
}

#Run a report of missing vs received logs and save a csv copy
fl.name=paste("percent_logs_reported", Sys.Date(),"csv", sep=".")
per.rec=per.rec[order(per.rec$YEARMTH),]
write.csv(per.rec, file=paste0(figdir,"/",fl.name),na="", row.names=F)

#27-32 Map for Documents, presentations, etc.
png(filename=file.path(figdir, "MapLFA27-32.png") ,width=6.5, height=6.5, units = "in", res = 800)
LobsterMap('27-32', labels=c('lfa','grid'), grid.labcex=0.6)
dev.off()

#For Individual LFAs with grids labelled
#png(filename=file.path(figdir, "MapLFA32.png") ,width=6.5, height=6.5, units = "in", res = 800)
#LobsterMap('32', labels=c('lfa','grid'), grid.labcex=0.6)
#dev.off()

#######-----------------------------------------
# Primary Indicator- Commercial CPUE
#######-----------------------------------------


logs=lobster.db("process.logs")

#Choose One:

CPUE.data<-CPUEModelData2(p,redo=T) #Reruns cpue model. Only takes a couple minutes now.
#CPUE.data<-CPUEModelData2(p,redo=F) Doesn't rerun model. Just takes last run version.

cpueData=CPUEplot(CPUE.data,lfa= p$lfas,yrs=1981:p$current.assessment.year, graphic='R')$annual.data #index end year

#add lrp and USR

cpueData$usr=NA
cpueData$lrp=NA

for (l in p$lfas){
mu=median(cpueData$CPUE[cpueData$YEAR %in% c(1990:2016) & cpueData$LFA==l])
cpueData$usr[cpueData$LFA==l]=0.8*mu
cpueData$lrp[cpueData$LFA==l]=0.4*mu
}
ls=c('27', '28', '29', '30')
ls2=c('31A', '31B', '32')

xlim=c(1985,p$current.assessment.year)

crplot= function(x, French=F){
  crd = subset(cpueData,LFA==l,c("YEAR","CPUE"))
  mu = median(crd$CPUE[crd$YEAR %in% c(1990:2016)])
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

write.csv(cpueData, file=paste0(cpue.dir, "/fishery.stats.27-32.csv"), row.names=F )
save(cpueData, file=paste0(cpue.dir, "/cpueData.Rdata") )

# Begin first CPUE figure (27, 28, 29, 30)
png(filename=file.path(cpue.dir, "CPUE_LFA27-30.png"),width=8, height=5.5, units = "in", res = 800)
par(mfrow=c(2,2))
  for (l in ls) {
    crplot(French=F) #Change to crplot(French=T) to produce French axis labels
    }
dev.off()

#French 27-30
png(filename=file.path(cpue.dir, "CPUE_LFA27-30.French.png"),width=8, height=5.5, units = "in", res = 800)
par(mfrow=c(2,2))
for (l in ls) {
  crplot(French=T) #Change to crplot(French=T) to produce French axis labels
}
dev.off()


# Begin second CPUE figure 31A, 31B, 32
png(filename=file.path(cpue.dir, "CPUE_LFA31A-32.png"),width=8, height=5.5, units = "in", res = 800)
par(mfrow=c(2,2))
for (l in ls2) {
  crplot(French=F) #Change to crplot(French=T) to produce French axis labels
}
dev.off()


# French 31A, 31B, 32
png(filename=file.path(cpue.dir, "CPUE_LFA31A-32.French.png"),width=8, height=5.5, units = "in", res = 800)
par(mfrow=c(2,2))
for (l in ls2) {
  crplot(French=T) #Change to crplot(French=T) to produce French axis labels
}
dev.off()



#Create individual plots for AC Meetings
for (l in p$lfas){
  png(filename=file.path(cpue.dir, paste0("CPUE_LFA",l, ".png")),width=8, height=5.5, units = "in", res = 800)
  crplot()
  dev.off()
}

# # Plots unbiased annual CPUE for all LFAs in Maritimes region
# # Good for context in presentations at AC
# 
# a = lobster.db('process.logs')
# a = subset(a,SYEAR %in% 2004:p$current.assessment.year) 
# 
# aa = split(a,f=list(a$LFA,a$SYEAR))
# cpue.lst<-list()
# m=0
# #by time
# for(i in 1:length(aa)){
#   tmp<-aa[[i]]
#   tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
#   names(tmp)<-c('time','catch','effort')
#   tmp$date<-as.Date(tmp$time)
#   first.day<-min(tmp$date)
#   tmp$time<-julian(tmp$date,origin=first.day-1)
#   tmp$time = ceiling(tmp$time/7) #convert to week of season
#   if(nrow(tmp)>5){
#     m=m+1
#     g<-as.data.frame(biasCorrCPUE(tmp,by.time=F))
#     g$lfa=unique(aa[[i]]$LFA)
#     g$yr = unique(aa[[i]]$SYEAR)
#     g = t(g)[,2]
#     cpue.lst[[m]] <- g
#   }
# }
# cc =as.data.frame(do.call(rbind,cpue.lst))
# cc$CPUE = as.numeric(cc$`biasCorrCPUE(tmp, by.time = F)`)
# cc = cc[order(cc$lfa,cc$yr),]
# cc$yr = as.numeric(cc$yr)
# cc$fyr = as.factor(cc$yr)
# last_bar_color="black"
# point_colors <- ifelse(cc$yr <max(cc$yr), last_bar_color, "orange")
# cc1 = cc
# 
# png(filename=file.path(cpue.dir, "all_lfas_cpue.png"),width=8, height=5.5, units = "in", res = 800)
# ggplot(cc,aes(x=yr,y=CPUE))+geom_point()+
#   geom_smooth(se=FALSE)+geom_point(data=cc1,aes(x=yr,y=CPUE,colour=fyr))+facet_wrap(~lfa,scales='free_y')+
#   scale_colour_manual(values = point_colors)+theme(legend.position = 'none')+
#   labs(y= "CPUE", x = "Year")
# dev.off()


#Unbiased cpue patterns by week of season
#-----------------------------------------

a = lobster.db('process.logs')
a = subset(a,SYEAR %in% 2004:p$current.assessment.year & LFA %in% p$lfas) 

#quick cludge to remove a couple bad date entries for 2021 in LFA 27+
a=a[a$SD_LOG_ID %ni% c("2904147","2904341"),]

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


for (l in p$lfas){
png(filename=file.path(cpue.dir, paste0("weekly_cpue_",l,".png")),width=8, height=5.5, units = "in", res = 800)
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


#######-----------------------------------------
# Primary Indicator- Continuous Change In Ratio (CCIR)
#######-----------------------------------------
## Continuous Change In Ratio (CCIR)

#lobster.db('ccir')
lobster.db('ccir.redo') #Must 'redo' to bring in new data


 inp = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_inputs.csv'))
 load(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_groupings.rdata')) #object names Groupings
 load(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_seasons.rdata'))

logs = lobster.db('process.logs')

require(bio.ccir)
require(rstan)

load_all(paste(git.repo,'bio.ccir',sep="/")) # for debugging
#start.year=p$current.assessment.year-4 #to run on last four years
#start.year=2000 #to run on entire data set
start.year=assessment.year-3 #run last three years, past data shouldn't change

dat = ccir_compile_data(x = ccir_data,log.data = logs, area.defns = Groupings[1:6], size.defns = inp, season.defns = Seasons, sexs = 1.5, start.yr = start.year) #sexs 1.5 means no sex defn

out.binomial = list()
attr(out.binomial,'model') <- 'binomial'
for(i in 1:length(dat)) { #Change to restart a broken run based on iteration number (count files in summary folder...run from there)
  print(i)
  ds = dat[[i]]

#line underneath likely redundant. Should default to binomial
  #ds$method = 'binomial'

  x = ccir_stan_run_binomial(dat = ds,save=F)
  out.binomial[[i]] <- ccir_stan_summarize(x)
}

### If the folder C:\bio.data\bio.lobster\outputs\ccir\summary contains other model runs for different areas (i.e.27-32)
### move these to the appropriate folder within the summary folder (aka hide them)

#Need to move the new files into the proper folder to combine historic and current data
#Take all 27-32 files from "C:\bio.data\bio.lobster\outputs\ccir\summary" and move them to
#C:\bio.data\bio.lobster\outputs\ccir\summary\LFA27.32 
# then drop a copy of all summary files for all years back in "C:\bio.data\bio.lobster\outputs\ccir\summary"

#load statement below combines ccir summaries if broken runs
#ensure folder has only model run summaries
da = file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary') #modify as required

d = list.files(da,full.names=T)
d=d[!file.info(d)$isdir] #ensures only files are listed not directories
out.binomial = list()
#ensure folder has only model run summaries!!!!!
for( i in 1:length(d)){
  load(d[i])
  out.binomial[[i]] = out
}

#if(grepl(351,x$Grid[1])) Main = 'LFA 27 South'
#if(grepl(356,x$Grid[1])) Main = 'LFA 27 North'

#out.binomial[[1]]$LFA = "27N"
#out.binomial[[2]]$LFA = "27S"

ouBin = ccir_collapse_summary(out.binomial)
attr(ouBin,'model') <- 'binomial'

save(ouBin,file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBinomialModels2732.rdata'))
load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBinomialModels2732.rdata'))

#Combine  LFA 27 north and south
u = subset(ouBin, LFA == 27)
g = unique(u$Grid)
g = strsplit(g,"\\.")
o = aggregate(WEIGHT_KG~SYEAR,data=subset(logs,GRID_NUM %in% g[[1]]),FUN=sum)
names(o)[2] = g[[1]][1]
o2 = aggregate(WEIGHT_KG~SYEAR,data=subset(logs,GRID_NUM %in% g[[2]]),FUN=sum)
names(o2)[2] = g[[2]][1]
o = merge(o,o2)
names(o)[1] = 'Yr'

ccir.dir=file.path(figdir, "ccir")
dir.create( ccir.dir, recursive = TRUE, showWarnings = TRUE )

#png(filename=file.path(ccir.dir, "TS.exploitation.27.combined.png"),width=8, height=5.5, units = "in", res = 800)
oo <- ccir_timeseries_exploitation_plots(ouBin,combined.LFA=T,landings=o, fdir=ccir.dir)
#dev.off()

u = subset(ouBin, LFA != 27)
kl = unique(u$Grid)
outs=list()
for(i in 1:length(kl)) {
  u = subset(ouBin, Grid == kl[i])
 # png(filename=paste(ccir.dir,"/TS.exploitation.",u$LFA[1],".",u$Grid[1] ,".png", sep=""),width=8, height=5.5, units = "in", res = 800)
  outs[[i]] <- ccir_timeseries_exploitation_plots(u, fdir=ccir.dir)
 # dev.off()
}


o = do.call(rbind,outs)
ooo = subset(o,select=c(Yr,ERfl,ERfm,ERfu,ERf75,LFA))

oo = rbind(oo,ooo)
oo$LFA[oo$LFA == "LFA 27 Combined"] = 27
oo$LFA[oo$LFA == "LFA 29"] = 29
oo$LFA[oo$LFA == "LFA 30"] = 30
oo$LFA[oo$LFA == "LFA 31A"] = "31A"
oo$LFA[oo$LFA == "LFA 31B"] = "31B"
oo$LFA[oo$LFA == "LFA 32"] = 32

save(oo,file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR2732.rdata'))
load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR2732.rdata'))
RR75  = aggregate(ERf75~LFA,data=oo,FUN=max)

for(i in oo$LFA){
oo$RR75[oo$LFA==i]=RR75$ERf75[RR75$LFA==i]
}

ccir.sum=oo[,c(1, 6, 3, 7)]
write.csv(ccir.sum, file=paste0(ccir.dir, "/ccir.27-32.csv"), row.names=F )

# plot Individual
for(i in c("27", "29", "30", "31A", "31B", "32")){
  o = subset(oo,LFA==i)
  RR7 = subset(RR75,LFA==i)$ERf75
  #English
  png(filename=file.path(ccir.dir, paste0('ExploitationRefs',i, '.png')),width=8, height=4, units = "in", res = 800)
  ExploitationRatePlots(data = o[,c("Yr","ERfm","ERfl","ERfu")],lrp=RR7,lfa = i,fd=ccir.dir, save=F, title=i, French=F)
  dev.off()
  #French
  png(filename=file.path(ccir.dir, paste0('ExploitationRefs',i, '.French.png')),width=8, height=4, units = "in", res = 800)
  ExploitationRatePlots(data = o[,c("Yr","ERfm","ERfl","ERfu")],lrp=RR7,lfa = i,fd=ccir.dir, save=F, title=i, French=T)
  dev.off()

}

#Plot as one figure for document

#English
png(filename=file.path(ccir.dir, paste0('Fig 4.ExploitationRefs 27-32.png')),width=10, height=8, units = "in", res = 800)
par(mfrow=c(3,2))

for(i in c("27", "29", "30", "31A", "31B", "32")){
  o = subset(oo,LFA==i)
  RR7 = subset(RR75,LFA==i)$ERf75
  ExploitationRatePlots(data = o[,c("Yr","ERfm","ERfl","ERfu")],lrp=RR7,lfa = i,fd=ccir.dir, save=F, title=i, French=F)
}
dev.off()

#French
png(filename=file.path(ccir.dir, paste0('Fig 4. ExploitationRefs27-32.French.png')),width=10, height=8, units = "in", res = 800)
par(mfrow=c(3,2))

for(i in c("27", "29", "30", "31A", "31B", "32")){
  o = subset(oo,LFA==i)
  RR7 = subset(RR75,LFA==i)$ERf75
  #French
  ExploitationRatePlots(data = o[,c("Yr","ERfm","ERfl","ERfu")],lrp=RR7,lfa = i,fd=ccir.dir, save=F, title=i, French=T)
}
dev.off()

## Secondary Indicators


### Landings and Effort

land.dir=file.path(figdir, "landings")
dir.create( land.dir, recursive = TRUE, showWarnings = TRUE )

land = lobster.db('annual.landings')
logs=lobster.db("process.logs")
CPUE.data<-CPUEModelData(p,redo=F)
cpueData=CPUEplot(CPUE.data,lfa= p$lfas,yrs=1981:2023, graphic='R')$annual.data #index end year
land =land[order(land$YR),]

write.csv(land[c(1:9)], file=paste0(land.dir, "/landings.27-32.csv"), row.names=F )

ls=c('27', '28', '29', '30')
ls2=c('31A', '31B', '32')

xlim<-c(1982,p$current.assessment.year)

#1 Landings Figure- LFAs 27, 28, 29, 20)
#-------------------------------------------

French=F #change to T to create landings figures with French Labels
#French=T
if (French){

  ylab= 'Debarquements (t)'  
  efftext= "Effort (x 1000 casiers leves)"   

}else  {
ylab= 'Landings (t)'
efftext= "Effort ('000s Trap Hauls)"
}


png(filename=file.path(land.dir, "Landings_LFA27-30.png"),width=8, height=5.5, units = "in", res = 800)
par(mfrow=c(2,2))

lst=ls
for (i in 1:length(lst)) {
d1 = data.frame(YEAR = land$YR, LANDINGS = land[,paste("LFA", lst[i], sep="")])
#d1 = data.frame(YEAR = land$YR, LANDINGS = land[,"LFA27"])
d2 = subset(cpueData,LFA==lst[i])
d2 = merge(data.frame(LFA=d2$LFA[1],YEAR=min(d2$YEAR):max(d2$YEAR)),d2,all.x=T)
data=fishData = merge(d2,d1)
data$EFFORT2=fishData$EFFORT2 = fishData$LANDINGS * 1000 / fishData$CPUE

par(mar=c(3,5,2.0,4.5))
plot(data$YEAR,data$LANDINGS,ylab=ylab,type='h',xlim=xlim, xlab=" ", ylim=c(0,max(data$LANDINGS)*1.2),pch=15,col='gray73',lwd=4,lend=3)
lines(data$YEAR[nrow(data)],data$LANDINGS[nrow(data)],type='h',pch=21,col='steelblue4',lwd=4, lend=3)
text(x=(xlim[1]+2), y= 1.15*max(d1$LANDINGS, na.rm = TRUE), lst[i], cex=1.5)

par(new=T)

plot(data$YEAR,data$EFFORT2/1000,ylab='',xlab='', type='b', pch=16, axes=F,xlim=xlim,ylim=c(0,max(data$EFFORT2/1000,na.rm=T)))
points(data$YEAR[nrow(data)],data$EFFORT2[nrow(data)]/1000, type='b', pch=24,size = 2,bg='black')
axis(4)
if (i %in% c(2,4)) {mtext(efftext,cex = 0.75, side=4, line = 3, outer = F, las = 0)}
}

dev.off()

#2 Landings Figure- LFAs 31A, 31B, 32
#-------------------------------------------------------------------

png(filename=file.path(land.dir, "Landings_LFA31-32.png"),width=8, height=5.5, units = "in", res = 800)

par(mfrow=c(2,2))

lst=ls2
for (i in 1:length(lst)) {
  d1 = data.frame(YEAR = land$YR, LANDINGS = land[,paste("LFA", lst[i], sep="")])
  #d1 = data.frame(YEAR = land$YR, LANDINGS = land[,"LFA27"])
  d2 = subset(cpueData,LFA==lst[i])
  d2 = merge(data.frame(LFA=d2$LFA[1],YEAR=min(d2$YEAR):max(d2$YEAR)),d2,all.x=T)
  data=fishData = merge(d2,d1)
  data$EFFORT2=fishData$EFFORT2 = fishData$LANDINGS * 1000 / fishData$CPUE

  par(mar=c(3,5,2.0,4.5))
  plot(data$YEAR,data$LANDINGS,ylab=ylab,type='h',xlim=xlim, xlab=" ", ylim=c(0,max(data$LANDINGS)*1.2),pch=15,col='gray73',lwd=4,lend=3)
  lines(data$YEAR[nrow(data)],data$LANDINGS[nrow(data)],type='h',pch=21,col='steelblue4',lwd=4, lend=3)
  text(x=(xlim[1]+2), y= 1.15*max(d1$LANDINGS, na.rm = TRUE), lst[i], cex=1.7)
  par(new=T)

  plot(data$YEAR,data$EFFORT2/1000,ylab='',xlab='', type='b', pch=16, axes=F,xlim=xlim,ylim=c(0,max(data$EFFORT2/1000,na.rm=T)))
  points(data$YEAR[nrow(data)],data$EFFORT2[nrow(data)]/1000, type='b', pch=24,size = 2,bg='black')
  axis(4)
  if (i %in% c(2,3)) {mtext(efftext,cex = 0.75, side=4, line = 3, outer = F, las = 0)}

  }
dev.off()



#Individual LFAs for AC Meetings

lst=p$lfas
for (i in 1:length(lst)) {
png(filename=file.path(land.dir, paste0("Landings_LFA",lst[i],".png")),width=8, height=5.5, units = "in", res = 800)

  d1 = data.frame(YEAR = land$YR, LANDINGS = land[,paste("LFA", lst[i], sep="")])
  #d1 = data.frame(YEAR = land$YR, LANDINGS = land[,"LFA27"])
  d2 = subset(cpueData,LFA==lst[i])
  d2 = merge(data.frame(LFA=d2$LFA[1],YEAR=min(d2$YEAR):max(d2$YEAR)),d2,all.x=T)
  data=fishData = merge(d2,d1)
  data$EFFORT2=fishData$EFFORT2 = fishData$LANDINGS * 1000 / fishData$CPUE

  par(mar=c(3,5,2.0,4.5))
  plot(data$YEAR,data$LANDINGS,ylab=ylab,type='h',xlim=xlim, xlab=" ", ylim=c(0,max(data$LANDINGS)*1.2),pch=15,col='gray73',lwd=4,lend=3)
  lines(data$YEAR[nrow(data)],data$LANDINGS[nrow(data)],type='h',pch=21,col='steelblue4',lwd=4, lend=3)
  text(x=(xlim[1]+2), y= 1.15*max(d1$LANDINGS, na.rm = TRUE), lst[i], cex=1.7)

  par(new=T)

  plot(data$YEAR,data$EFFORT2/1000,ylab='',xlab='', type='b', pch=16, axes=F,xlim=xlim,ylim=c(0,max(data$EFFORT2/1000,na.rm=T)))
  points(data$YEAR[nrow(data)],data$EFFORT2[nrow(data)]/1000, type='b', pch=24,size = 2,bg='black')
  axis(4)
  mtext(efftext,cex = 0.75, side=4, line = 3, outer = F, las = 0)
dev.off()
}



### Recruitment Trap Catch Rates
fsrs.dir=file.path(figdir, "fsrs")
dir.create( fsrs.dir, recursive = TRUE, showWarnings = TRUE )

FSRSvesday<-FSRSModelData()

for(i in c("27", "29", "30", "31A", "31B", "32")){

  mdata = subset(FSRSvesday,LFA==i&SYEAR<=p$cu)

  FSRSModelResultsLegal=FSRSmodel(mdata,lfa=i, response="LEGALS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)
  FSRSModelShortsRecruit=FSRSmodel(mdata,lfa=i, response="SHORTS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)
  FSRSModelResultsRecruit=FSRSmodel(mdata,lfa=i, response="RECRUITS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)

  FSRSModelResultsLegal=FSRSmodel(mdata,lfa=i, response="LEGALS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
  legals = FSRSModelResultsLegal$pData
  legals$Area = i

  FSRSModelResultsRecruit=FSRSmodel(mdata,lfa=i, response="RECRUITS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
  recruit =  FSRSModelResultsRecruit$pData
  recruit$Area = i

  FSRSModelShortsRecruit=FSRSmodel(mdata,lfa=i, response="SHORTS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
  shorts =  FSRSModelShortsRecruit$pData
  shorts$Area = i

  save(list=c("shorts","legals","recruit"),file=file.path(project.datadirectory("bio.lobster"),"outputs",paste0("fsrsModelIndicators",i,".rdata")))

  i=recruit$Area[1]
  # plot
  #x11(width=8,height=7)
  png(filename=file.path(fsrs.dir, paste('FSRSRecruitCatchRate',i,'png', sep='.')),width=8, height=6.5, units = "in", res = 800)
  i=recruit$Area[1]
  FSRSCatchRatePlot(recruits = recruit[,c("YEAR","median","lb","ub")],legals=legals[,c("YEAR","median","lb","ub")],
                    lfa = i,fd=figdir,title=i, save=F, rm=F, French=F) #Change French=T for french labels in figure
  dev.off()
  }

#French Figures
for(i in c("27", "29", "30", "31A", "31B", "32")){
load(file=file.path(project.datadirectory("bio.lobster"),"outputs",paste0("fsrsModelIndicators",i,".rdata")))

# plot
  png(filename=file.path(fsrs.dir, paste('FSRSRecruitCatchRate',i,'French.png', sep='.')),width=8, height=6.5, units = "in", res = 800)
  FSRSCatchRatePlot(recruits = recruit[,c("YEAR","median","lb","ub")],legals=legals[,c("YEAR","median","lb","ub")],
                    lfa = i,fd=figdir,title=i, save=F, rm=F, French=T) #Change French=T for french labels in figure
  dev.off()
}

#All in one figure for document
#Combine online using https://products.aspose.app/pdf/merger/png-to-png


# Phase plots for conclusions and advice

hcr.dir=file.path(figdir, "hcr")
dir.create( hcr.dir, recursive = TRUE, showWarnings = TRUE )

load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR2732.rdata'))
RR75  = aggregate(ERf75~LFA,data=oo,FUN=max)
load(file=paste0(figdir, "/cpue/cpueData.Rdata") )


lfas2 = c("27", "29", "30", "31A", "31B", "32")

#Individual Phase plots
for(i in 1:length(lfas2)){
   x = subset(cpueData,LFA==lfas2[i])
  y = read.csv(file.path(figdir,"ccir",paste0("ExploitationRefs",lfas2[i],".csv")))
  y=y[y$Yr>2004,]

  usr=x$usr[1]
  lrp=x$lrp[1]
  RR=RR75$ERf75[RR75$LFA==lfas2[i]]

  running.median = with(rmed(x[,2],x[,6]),data.frame(YEAR=yr,running.median=x))
  x=merge(x,running.median,all=T)

   png(file=file.path(hcr.dir,paste0('PhasePlot',lfas2[i],'.png')))
     hcrPlot(B=x$running.median[x$YEAR>=min(y$Yr)],mF=y$running.median,USR=usr,LRP=lrp,RR=RR,big.final=T,
     yrs=min(y$Yr):p$current.assessment.year,ylims=c(0,1),xlims=NULL,labels=c('USR','LRP','RR') ,
     RRdec=F,  ylab = 'Exploitation', xlab = 'CPUE', yr.ends=T, main=paste0("LFA ",lfas2[i]), cex.main=1.6 )
  dev.off()

  png(file=file.path(hcr.dir,paste0('PhasePlot',lfas2[i],'.French.png')))
  hcrPlot(B=x$running.median[x$YEAR>=min(y$Yr)],mF=y$running.median,USR=usr,LRP=lrp,RR=RR,big.final=T,
          yrs=min(y$Yr):p$current.assessment.year,ylims=c(0,1),xlims=NULL,labels=c('USR','LRP','RR') ,
          RRdec=F,  ylab = 'Exploitation', xlab = 'CPUE', yr.ends=T, main=paste0("ZPH ",lfas2[i]) , cex.main=1.6, FrenchCPUE=T)
  dev.off()
}

#Panel Plot with all areas for HCR

#English
png(file=file.path(hcr.dir,paste0('LFAs27-32.PhasePlot.png')),,width=9, height=11, units = "in", res = 400)
  par(mfrow=c(3,2))

    for(i in 1:length(lfas2)){
      x = subset(cpueData,LFA==lfas2[i])
      y = read.csv(file.path(figdir,"ccir",paste0("ExploitationRefs",lfas2[i],".csv")))
      y=y[y$Yr>2004,]
      usr=x$usr[1]
      lrp=x$lrp[1]
      RR=RR75$ERf75[RR75$LFA==lfas2[i]]
      running.median = with(rmed(x[,2],x[,6]),data.frame(YEAR=yr,running.median=x))
      x=merge(x,running.median,all=T)

      hcrPlot(B=x$running.median[x$YEAR>=min(y$Yr)],mF=y$running.median,USR=usr,LRP=lrp,RR=RR,big.final=T,
              yrs=min(y$Yr):p$current.assessment.year,ylims=c(0,1),xlims=NULL,labels=c('USR','LRP','RR') ,
              RRdec=F,  ylab = 'Exploitation', xlab = 'CPUE', yr.ends=T, main=paste0("LFA ",lfas2[i]), cex.main=1.6 )
}
dev.off()


#French
png(file=file.path(hcr.dir,paste0('LFAs27-32.PhasePlot.French.png')),width=9, height=11, units = "in", res = 800)
  par(mfrow=c(3,2))

      for(i in 1:length(lfas2)){
        x = subset(cpueData,LFA==lfas2[i])
        y = read.csv(file.path(figdir,"ccir",paste0("ExploitationRefs",lfas2[i],".csv")))
        y=y[y$Yr>2004,]
        usr=x$usr[1]
        lrp=x$lrp[1]
        RR=RR75$ERf75[RR75$LFA==lfas2[i]]
        running.median = with(rmed(x[,2],x[,6]),data.frame(YEAR=yr,running.median=x))
        x=merge(x,running.median,all=T)

        hcrPlot(B=x$running.median[x$YEAR>=min(y$Yr)],mF=y$running.median,USR=usr,LRP=lrp,RR=RR,big.final=T,
                yrs=min(y$Yr):p$current.assessment.year,ylims=c(0,1),xlims=NULL,labels=c('USR','LRP','RR') ,
                RRdec=F,  ylab = 'Exploitation', xlab = 'CPUE', yr.ends=T, main=paste0("ZPH ",lfas2[i]) , cex.main=1.6, FrenchCPUE=T)
      }
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
#g27p=g27p[g27p$LFA %in% c('27','28','29','30','311','312','32'),]

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
tt=gg
cpue.diff= function(x, tsize=6, vj=20){
  ggplot(tt,aes(fill=percentChange))+
    geom_sf() +
    scale_fill_continuous_diverging(palette='Purple-Green') +
    labs(fill = "    CPUE\n% Change")+
    geom_sf(data=coa,fill='grey')+
    geom_sf(data=GrMap,fill=NA)+
    coord_sf(xlim = c(st_bbox(tt)$xmin,st_bbox(tt)$xmax),
             ylim = c(st_bbox(tt)$ymin,st_bbox(tt)$ymax),
             expand = FALSE)+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          plot.margin=grid::unit(c(8,2,8,2), "mm"))+
    scale_x_continuous(breaks = c(round(seq(st_bbox(tt)$xmin,st_bbox(tt)$xmax,length.out=2),2)))+
    scale_y_continuous(breaks = c(round(seq(st_bbox(tt)$ymin,st_bbox(tt)$ymax,length.out=2),2)))+
    #annotate("text", x=(st_bbox(tt)$xmax)-0.2, y=(st_bbox(tt)$ymin)+0.05, label= lab, size=tsize )
    annotate("text", x = Inf, y = Inf, label = lab, vjust = vj, hjust = 1.2, size=tsize)
 }

#One figure 27-32 for context
png(filename=file.path(cpue.dir, "cpue_diff.png"), width=1600, height=900, res=175)
print(cpue.diff())
dev.off()	

#Each Separate

for (xx in ls){
tt=subset(gg, LFA==xx)
if(xx=="27") {scale_y_continuous(breaks = c(round(seq(st_bbox(tt)$ymin,st_bbox(tt)$ymax,length.out=2),2)))}
if(xx=="27"){tsize=5}else{tsize=6}
if(xx=="27"){vj=30}else{vj=20}
if(xx=="27"){png(filename=file.path(cpue.dir, paste(xx,"cpue.diff.png", sep="_")), width=900, height=900, res=175)}else
  {png(filename=file.path(cpue.dir, paste(xx,"cpue.diff.png", sep="_")), width=1600, height=900, res=175)}
print(cpue.diff(tsize=tsize, vj=vj))
dev.off()	
}

### Bycatch### - NOT USED SINCE 2020

#Bycatch estimates are calculated using effort from logbook data for LFAs 31A and 31B
#To estimate LFA 27 bycatch, gulf landings need to be added to logs.

bc.dir=file.path(figdir, "bycatch")
dir.create( bc.dir, recursive = TRUE, showWarnings = TRUE )

Lobster.Bycatch(lfa=c("31A","31B"), save=T, save.dir=bc.dir)

