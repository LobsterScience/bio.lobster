p = bio.lobster::load.environment()
require(SpatialHub)
require(lubridate)

la()

#assessment.year = p$current.assessment.year 
p$current.assessment.year = p$current.assessment.year - 1 

figdir = file.path(project.datadirectory("bio.lobster","assessments","Updates","LFA27-32",p$current.assessment.year))
dir.create( figdir, recursive = TRUE, showWarnings = FALSE )

p$lfas = c("27", "28", "29", "30", "31A", "31B", "32") # specify lfas for data summary
p$subareas = c("27N","27S", "28", "29", "30", "31A", "31B", "32") # specify lfas for data summary

# update data through ROracle
NewDataPull =F
if(NewDataPull){
  lobster.db('fsrs.redo')
  lobster.db('logs.redo')
  lobster.db('annual.landings.redo')
  lobster.db('vlog.redo')
  logs=lobster.db('process.logs.redo')
}


png(filename=file.path(figdir, "MapLFA2732.png") ,width=6.5, height=6.5, units = "in", res = 800)
 LobsterMap('27-32')
dev.off()


logs=lobster.db("process.logs")

CPUE.data<-CPUEModelData(p,redo=F)
cpueData=CPUEplot(CPUE.data,lfa= p$lfas,yrs=1981:2020, graphic='R')$annual.data

ls=c('27', '28', '29', '30')
ls2=c('31A', '31B', '32')

# Begin first CPUE figure (27, 28, 29, 30)

png(filename=file.path(figdir, "CPUE_LFA27-30.png"),width=8, height=5.5, units = "in", res = 800)
xlim=c(1985,p$current.assessment.year)
par(mfrow=c(2,2))		

for (l in ls) {
####27####		
crd = subset(cpueData,LFA==l,c("YEAR","CPUE"))	
mu = median(crd$CPUE[crd$YEAR %in% c(1985:2009)])
usr = mu * 0.8
lrp = mu * 0.4
crd  = merge(data.frame(YEAR=min(crd$YEAR):max(crd$YEAR)),crd,all.x=T)


par(mar=c(2.0,2.0,2.0,2.0))	
plot(crd[,1],crd[,2],xlab=' ',ylab='CPUE (kg/TH)',type='p',pch=16, xlim=xlim, ylim=c(lrp-.1,1.05*(max(crd$CPUE, na.rm = TRUE)) ))
running.median = with(rmed(crd[,1],crd[,2]),data.frame(YEAR=yr,running.median=x))
crd=merge(crd,running.median,all=T)
lines(crd[,1],crd$running.median,col='blue',lty=1,lwd=2)
abline(h=usr,col='green',lwd=2,lty=2)
abline(h=lrp,col='red',lwd=2,lty=3)
text(x=1990, y= .9*max(crd$CPUE, na.rm = TRUE), l, cex=2)
}
dev.off()

# Begin second CPUE figure 31A, 31B, 32

png(filename=file.path(figdir, "CPUE_LFA31A-32.png"),width=8, height=5.5, units = "in", res = 800)
xlim=c(1985,2020)
par(mfrow=c(2,2)) 
for (l in ls2) {
  ####27####		
  crd = subset(cpueData,LFA==l,c("YEAR","CPUE"))	
  mu = median(crd$CPUE[crd$YEAR %in% c(1985:2009)])
  usr = mu * 0.8
  lrp = mu * 0.4
  crd  = merge(data.frame(YEAR=min(crd$YEAR):max(crd$YEAR)),crd,all.x=T)
  
  
  par(mar=c(2.0,2.0,2.0,2.0))
  plot(crd[,1],crd[,2],xlab=' ',ylab='CPUE (kg/TH)',type='p',pch=16, xlim=xlim, ylim=c(lrp-.1,1.05*(max(crd$CPUE, na.rm = TRUE)) ))
  running.median = with(rmed(crd[,1],crd[,2]),data.frame(YEAR=yr,running.median=x))
  crd=merge(crd,running.median,all=T)
  lines(crd[,1],crd$running.median,col='blue',lty=1,lwd=2)
  abline(h=usr,col='green',lwd=2,lty=2)
  abline(h=lrp,col='red',lwd=2,lty=3)
  text(x=1990, y= .9*max(crd$CPUE, na.rm = TRUE), l, cex=2)
}
dev.off()



## Continuous Change In Ratio (CCIR)

lobster.db('ccir.redo')
inp = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_inputs.csv'))
load(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_groupings.rdata')) #object names Groupings
load(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_seasons.rdata'))


lobster.db('ccir')

logs = lobster.db('process.logs')

require(bio.ccir)
require(rstan)
#load_all(paste(git.repo,'bio.ccir',sep="/")) # for debugging
ccir_data2 = subset(ccir_data,YEAR<=2021)
dat = ccir_compile_data(x = ccir_data,log.data = logs, area.defns = Groupings[7], size.defns = inp, season.defns = Seasons, sexs = 1.5) #sexs 1.5 means no sex defn

out.binomial = list()
attr(out.binomial,'model') <- 'binomial'
for(i in 1:length(dat)) {
  ds = dat[[i]]
  ds$method = 'binomial'
  x = ccir_stan_run(dat = ds,save=T)
  out.binomial[[i]] <- ccir_stan_summarize(x)
}

#load statement below combines ccir summaries if broken runs
#ensure folder has only model run summaries
da = file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary') #modify as required

d = list.files(da,full.names=T)
out.binomial = list()

for( i in 1:length(d)){
  load(d[i])
  out.binomial[[i]] <- out
}


out.binomial[[1]]$LFA = "33W"
out.binomial[[2]]$LFA = "33E"
ouBin = ccir_collapse_summary(out.binomial)
attr(ouBin,'model') <- 'binomial'
#ouBin$Yr = ouBin$Yr +1
save(ouBin,file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBinomialModels33.rdata'))
load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBinomialModels33.rdata'))

g = unique(ouBin$Grid)
g = strsplit(g,"\\.")
o = aggregate(WEIGHT_KG~SYEAR,data=subset(logs,GRID_NUM %in% g[[1]]),FUN=sum)
names(o)[2] = g[[1]][1]
o2 = aggregate(WEIGHT_KG~SYEAR,data=subset(logs,GRID_NUM %in% g[[2]]),FUN=sum)
names(o2)[2] = g[[2]][1]
o = merge(o,o2)
names(o)[1] = 'Yr'
oo <- ccir_timeseries_exploitation_plots(ouBin,combined.LFA=T,landings=o)

save(oo,file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR33.rdata'))
load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR33.rdata'))
RR75 = max(oo$ERf75[oo$Yr<2021])#0.8338764
#########Linux to here

oo=read.csv(file.path(figdir, "LFA33ccirout.csv"))
# plot

png(filename=file.path(figdir, "CCIR_LFA33.png"),width=8, height=5, units = "in", res = 800)
ExploitationRatePlots(data = oo[,c("Yr","ERfm","ERfl","ERfu")],lrp=RR75,lfa = 33,fd=figdir, save=F)
dev.off()

write.csv(data,file.path(fd,paste(fn,'.csv',sep='')))



## Secondary Indicators


### Landings and Effort

land = lobster.db('annual.landings')
logs=lobster.db("process.logs")
CPUE.data<-CPUEModelData(p,redo=F)
land =land[order(land$YR),]

ls=c('27', '28', '29', '30')
ls2=c('31A', '31B', '32')

#1 Landings Figure- LFAs 27, 28, 29, 20)
#-------------------------------------------

png(filename=file.path(figdir, "Landings_LFA27-30.png"),width=8, height=5.5, units = "in", res = 800)

xlim<-c(1982,p$current.assessment.year)
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
plot(data$YEAR,data$LANDINGS,ylab='Landings(t)',type='h',xlim=xlim, xlab=" ", ylim=c(0,max(data$LANDINGS)*1.2),pch=15,col='gray73',lwd=4,lend=3)
lines(data$YEAR[nrow(data)],data$LANDINGS[nrow(data)],type='h',pch=21,col='steelblue4',lwd=4, lend=3)
text(x=(xlim[1]+2), y= 1.15*max(d1$LANDINGS, na.rm = TRUE), lst[i], cex=3.3, col="darkred")

par(new=T)

plot(data$YEAR,data$EFFORT2/1000,ylab='',xlab='', type='b', pch=16, axes=F,xlim=xlim,ylim=c(0,max(data$EFFORT2/1000,na.rm=T)))
points(data$YEAR[nrow(data)],data$EFFORT2[nrow(data)]/1000, type='b', pch=24,size = 2,bg='black')
axis(4)
if (i %in% c(2,4)) {mtext("Effort ('000s Trap Hauls)",cex = 0.75, side=4, line = 3, outer = F, las = 0)}

}

dev.off()

#2 Landings Figure- LFAs 31A, 31B, 32
#-------------------------------------------------------------------

png(filename=file.path(figdir, "Landings_LFA31-32.png"),width=8, height=5.5, units = "in", res = 800)

xlim<-c(1982,p$current.assessment.year)
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
  plot(data$YEAR,data$LANDINGS,ylab='Landings(t)',type='h',xlim=xlim, xlab=" ", ylim=c(0,max(data$LANDINGS)*1.2),pch=15,col='gray73',lwd=4,lend=3)
  lines(data$YEAR[nrow(data)],data$LANDINGS[nrow(data)],type='h',pch=21,col='steelblue4',lwd=4, lend=3)
  text(x=(xlim[1]+2), y= 1.15*max(d1$LANDINGS, na.rm = TRUE), lst[i], cex=3.3, col="darkred")
  
  par(new=T)
  
  plot(data$YEAR,data$EFFORT2/1000,ylab='',xlab='', type='b', pch=16, axes=F,xlim=xlim,ylim=c(0,max(data$EFFORT2/1000,na.rm=T)))
  points(data$YEAR[nrow(data)],data$EFFORT2[nrow(data)]/1000, type='b', pch=24,size = 2,bg='black')
  axis(4)
  if (i %in% c(2,3)) {mtext("Effort ('000s Trap Hauls)",cex = 0.75, side=4, line = 3, outer = F, las = 0)}
}

dev.off()


### Recruitment Trap Catch Rates 

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
  
  
  # plot
  x11(width=8,height=7)
  FSRSCatchRatePlot(recruits = recruit[,c("YEAR","median","lb","ub")],legals=legals[,c("YEAR","median","lb","ub")],lfa = i,fd=figdir,title='')
  
}


### Bycatch

Bycatch estimates are calculated using effort from logbook data for all LFAs except LFA 27. To estimate bycatch in LFA 27, effort was estimated from the combined Gulf Logbooks and Maritimes logbooks using the median CPUE and landings from the Gulf logs and logbook effort from the Maritimes Region. 2019 Gulf Landings in LFA 27 are calculated by adding estimated slip landings based on previous years to the logbook landings. 2019 Bycatch estimates for LFAs 27-32 are preliminary due to log records being incomplete with no bycatch estimate for LFA 29, 30 and 32 due to low sampling numbers or no data available.

```{r, fig.cap="Bycatch Estimates for LFA 27"}
knitr::include_graphics("LFA27Bycatch.png")
```



```{r, fig.cap="Bycatch Estimates for LFA 31A. 1 Not Specified (contains Longhorn and Shorthorn Sculpin)"}
knitr::include_graphics("LFA31ABycatch.png")
```



```{r, fig.cap="Bycatch Estimates for LFA 31B. 1 Not Specified (contains Longhorn and Shorthorn Sculpin)"}
knitr::include_graphics("LFA31BBycatch.png")
```




