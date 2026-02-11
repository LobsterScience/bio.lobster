require(bio.lobster)
require(SpatialHub)
require(lubridate)
require(bio.utilities)
require(ggplot2)
require(dplyr)
require(sf)
require(PBSmapping)
require(grid)
require(patchwork)
require(tidyr)
library(gridExtra)
library(stringr)

p = bio.lobster::load.environment()


la()

#Choose one
#assessment.year = p$current.assessment.year 
assessment.year = p$current.assessment.year-1 

#create subfolders
# Define all directory paths in a *named list*+

figdir = file.path(project.datadirectory("bio.lobster","assessments","Updates","LFA27-32",assessment.year))

dir.paths <- list(
    figdir = file.path(project.datadirectory("bio.lobster","assessments","Updates","LFA27-32",assessment.year)),
    cpue.dir  = file.path(figdir, "cpue"),
    grid.dir  = file.path(figdir, "grid.data"),
    temp.dir  = file.path(figdir, "temp"),
    csas.dir  = file.path(figdir, "fsr.panel.plots"),
    land.dir  = file.path(figdir, "landings"),
    fsrs.dir  = file.path(figdir, "fsrs"),
    ccir.dir  = file.path(figdir, "ccir"),
    tag.dir   = file.path(project.datadirectory("bio.lobster","tagging","tagmaps", assessment.year))
)

# Create each directory AND assign each path as a variable
for (nm in names(dir.paths)) {
    # actually create the directory
    dir.create(dir.paths[[nm]], recursive = TRUE, showWarnings = TRUE)

    # assign variable to global env
    assign(nm, dir.paths[[nm]], envir = .GlobalEnv)
}

p$lfas = c("27", "28", "29", "30", "31A", "31B", "32") # specify lfas for data summary
p$subareas = c("27N","27S", "28", "29", "30", "31A", "31B", "32") # specify lfas for data summary

#If you only want to update logs and CCIR for the last two years, run this:
p$yr=assessment.year

# update data through ROracle
NewDataPull =F
if(NewDataPull){
  lobster.db('fsrs.redo')
  lobster.db('logs.redo')
  lobster.db('annual.landings.redo')
  #lobster.db('vlog.redo') #These are static now, no need to update
  logs=lobster.db('season.dates.redo') #updates season dates as required
  logs=lobster.db('process.logs.redo')
 
}

#Run a report of missing vs received logs and save a csv copy
per.rec= lobster.db("percent_reporting")
per.rec <- per.rec[order(per.rec$YEARMTH), ]
per.rec <- per.rec[grepl(assessment.year, per.rec$YEARMTH), ]
columns_to_keep <- c("YEARMTH", grep("L27|L28|L29|L30|L31A|L31B|L32", colnames(per.rec), value = TRUE))
per.rec <- per.rec[, columns_to_keep]
per.rec <- per.rec[!apply(per.rec[, -which(names(per.rec) == "YEARMTH")], 1, function(x) all(is.na(x))), ]
lfs= c("L27","L28", "L29", "L30" , "L31A", "L31B", "L32")

# Initialize an empty dataframe to store the results
    perc.log.rec <- data.frame(lfa = character(0), perc.rec = numeric(0))
    
    # Loop through variables in lfs
    for (var in lfs) {
        
        # Column for "MISS"
        miss_column <- grep(paste0("^", var, "MISS"), colnames(per.rec), value = TRUE)
        if (length(miss_column) > 0) {
            miss = sum(per.rec[, miss_column], na.rm = TRUE)
        }
        
        # Column for "RECD"
        recd_column <- grep(paste0("^", var, "RECD"), colnames(per.rec), value = TRUE)
        if (length(recd_column) > 0) {
            recd = sum(per.rec[, recd_column], na.rm = TRUE)
        }
        
        # Calculate the percentage
        percent.rec = recd / (miss + recd) * 100
        percent.rec = round(percent.rec, 1)
        
        # Append the result to the dataframe
        perc.log.rec <- rbind(perc.log.rec, data.frame(lfa = var, perc.rec = percent.rec))
    }
    
    # View the result
    print(perc.log.rec)
    

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

#Double check that % logs in Marfis Science match perc.rec above

double.check.logs=F
#double.check.logs=T
if (double.check.logs){
        for (ii in p$lfas){
            t=subset(logs, LFA==ii)
            print(ii)
            print(table(t$SYEAR))
            }
}



#Establishing a master reference table of all lrp, usr
ref = data.frame(LFA=c(27:30,'31A','31B',32),
                 lrp=c(.14,.12,.11,.28,.16,.16,.14),
                 usr=c(.27,.25,.22,.56,.31,.32,.29)
                 )

logs=lobster.db("process.logs")
g = logs
g = subset(g, SYEAR<=p$current.assessment.year)

#bring in voluntary log data to populate <2005
fn.root =  file.path( project.datadirectory('bio.lobster'), "data")
fnODBC  =  file.path(fn.root, "ODBCDump")
get.vlog=load(file.path( fnODBC, "processed.vlog.rdata"),.GlobalEnv)
v = subset(vlog,SYEAR<=2005, select=c("SYEAR","W_KG","N_TRP","LFA"))
names(v)=c("SYEAR","WEIGHT_KG","NUM_OF_TRAPS","LFA")
v$LFA[v$LFA%in%c("27N","27S")] = "27"
v$LFA[v$LFA%in%c("33W","33E")] = "33"

va = aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~SYEAR+LFA,data=v,FUN=sum)
gag = aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~SYEAR+LFA,data=g,FUN=sum) 

#merge vlog and logs here
ga=rbind(va, gag)
ga$cpue = ga$WEIGHT_KG/ga$NUM_OF_TRAPS

#Some LFAs have both vlogs and mandatory (mid 2010's); Keep whichever has more trap hauls
ga <- ga %>%
    group_by(SYEAR, LFA) %>%
    slice_max(order_by = NUM_OF_TRAPS, n = 1, with_ties = FALSE) %>%  # <- ensures only one row kept
    ungroup()


l = unique(ga$LFA)
o = list()
for(j in 1:length(l)){
    n = subset(ga,LFA==l[j])
    running.median = with(rmed(n$SYEAR,n$cpue),data.frame(SYEAR=yr,running.median=x))
    o[[j]]=merge(n,running.median,all=T)
}
o = dplyr::bind_rows(o)
names(o)=c("YEAR", "LFA", "NUM_OFTRAPS","WEIGHT_KG", "CPUE", "CPUErmed")

#need to add NA's for LFA 28 for missing years

o$YEAR <- as.numeric(as.character(o$YEAR))

o28 <- subset(o, LFA == "28")

yrs28 <- seq(min(o28$YEAR, na.rm = TRUE),
             max(o28$YEAR, na.rm = TRUE))

full28 <- data.frame(
    YEAR = yrs28,
    LFA  = "28"
)

o28 <- merge(full28, o28, by = c("YEAR", "LFA"), all.x = TRUE)

# Set ALL fields to NA for 1996–2007 (except YEAR and LFA)
o28[o28$YEAR %in% 1996:2007,
    setdiff(names(o28), c("YEAR", "LFA"))] <- NA

# Sort
o28 <- o28[order(o28$YEAR), ]

# Recombine with all other LFAs
o <- rbind(
    subset(o, LFA != "28"),
    o28
)



crd <- merge(o, ref, by = "LFA", all.x = TRUE) #add ref points
crd = crd[order(crd$LFA,crd$YEAR),]
#crd = crd[is.finite(crd$CPUE),]

write.csv(crd, file=paste0(cpue.dir, "/fishery.stats.27-32.csv"), row.names=F )
save(crd, file=paste0(cpue.dir, "/cpueData.Rdata") )

ls=c('27', '28', '29', '30')
ls2=c('31A', '31B', '32')

xlim=c(1985,p$current.assessment.year)

crplot= function(x, French=F){
        cr=subset(crd, LFA==l)
        usr=cr$usr[1]
        lrp=cr$lrp[1]
        
        par(mar=c(3.0,5.0,2.0,2.0))
        ylab='CPUE (kg/TH)'
        if (French){ylab='CPUE (kg/casier levé)'}
        plot(cr$YEAR,cr$CPUE,xlab=' ',ylab=ylab,type='p',pch=16, 
             xlim=xlim, ylim=c(lrp-.1,1.05*(max(cr$CPUE, na.rm = TRUE)) ))
        lines(cr$YEAR,cr$CPUErmed,col='blue',lty=1,lwd=2)
        abline(h=usr,col='green',lwd=2,lty=2)
        abline(h=lrp,col='red',lwd=2,lty=3)
        text(x=1988, y= max(cr$CPUE, na.rm = TRUE), l, cex=2)
        points(x=assessment.year, y=cr$CPUE[cr$YEAR==assessment.year], pch=17, col="orange", cex=1.4)
        }


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


pls=c('27', '28', '29', '30', '31A', '31B', '32')
#Create individual plots for AC Meetings
for (l in pls){
      png(filename=file.path(cpue.dir, paste0("CPUE_LFA",l, ".png")),width=8, height=5.5, units = "in", res = 800)
      crplot()
      dev.off()
    }

#-------------------------------------------------
#density of CPUE's
#a=  lobster.db('process.logs.unfiltered')
#a = subset(a,LFA==32)
#ad = density(a$CPUE,na.rm=T)
#ad = data.frame(x=ad$x,y=ad$y)
#ggplot(subset(a,CPUE<10),aes(x=CPUE))+geom_histogram(aes(y=..density..))+
#    facet_wrap(~SYEAR)+
#    geom_line(data=subset(ad,x<10),aes(x=x,y=y),colour='red')+
#    theme_test()

# # Plots unbiased annual CPUE for all LFAs in Maritimes region
# # Good for context in presentations at AC
# 
 a = lobster.db('process.logs')
 a = subset(a,SYEAR %in% 2004:assessment.year) 
 
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
 
 png(filename=file.path(cpue.dir, "all_lfas_cpue.png"),width=8, height=5.5, units = "in", res = 800)
 ggplot(cc,aes(x=yr,y=CPUE))+geom_point()+
   geom_smooth(se=FALSE)+geom_point(data=cc1,aes(x=yr,y=CPUE,colour=fyr))+facet_wrap(~lfa,scales='free_y')+
   scale_colour_manual(values = point_colors)+theme(legend.position = 'none')+
   labs(y= "CPUE", x = "Year")+
    theme(axis.text.x = element_text(size = 5))
 dev.off()

#Unbiased cpue patterns by week of season
#-----------------------------------------

a = lobster.db('process.logs')
a = subset(a,SYEAR %in% (assessment.year-11):assessment.year & LFA %in% p$lfas) 

#quick cludge to remove a couple bad date entries for 2021 in LFA 27+
a=a[a$SD_LOG_ID %ni% c("2904147","2904341"),]

aa = split(a,f=list(a$LFA,a$SYEAR))
aa = rm.from.list2(aa)
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

##by week
aa = split(a,f=list(a$LFA,a$SYEAR))
cpue.lst<-list()
m=0

aa = split(a,f=list(a$LFA,a$SYEAR))
source("C:/bio/bio.lobster/R/rm.from.list2.r") #had a problem with original rm.from.list. Modified under new name
aa = rm.from.list2(aa)
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
#start.year=assessment.year-4 #to run on last four years
#start.year=2000 #to run on entire data set
start.year=assessment.year-2 #run last three years, past data shouldn't change

#taken from LFA 33 assessment Oct 25. Might not be useful 

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

### If the folder C:\bio.data\bio.lobster\outputs\ccir\summary contains other model runs for different areas (i.e.LFA 33)
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

oo <- oo %>%
    group_by(LFA) %>%
    arrange(Yr) %>%   # sorting is important
    mutate(
        ERrmed = rmed(Yr, ERfm)$x
    ) %>%
    ungroup()

oo <- oo %>% arrange(LFA, Yr)

RR75  = aggregate(ERf75~LFA,data=oo,FUN=max)
oo$RR75=NA

for(i in oo$LFA){
oo$RR75[oo$LFA==i]=RR75$ERf75[RR75$LFA==i]
}

save(oo,file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR2732.rdata'))
load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR2732.rdata'))
RR75  = aggregate(ERf75~LFA,data=oo,FUN=max)
oo=as.data.frame(oo)
ccir.sum=oo[,c(1, 6, 3, 7, 8 )]

write.csv(ccir.sum, file=paste0(ccir.dir, "/ccir.27-32.csv"), row.names=F )

# plot Individual
for(i in c("27", "29", "30", "31A", "31B", "32")){
  o = subset(oo,LFA==i)
  RR7=o$RR75[1]
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
  RR7 = o$RR75[1]
  ExploitationRatePlots(data = o[,c("Yr","ERfm","ERfl","ERfu")],lrp=RR7,lfa = i,fd=ccir.dir, save=F, title=i, French=F)
}
dev.off()

#French
png(filename=file.path(ccir.dir, paste0('Fig 4. ExploitationRefs27-32.French.png')),width=10, height=8, units = "in", res = 800)
par(mfrow=c(3,2))

for(i in c("27", "29", "30", "31A", "31B", "32")){
  o = subset(oo,LFA==i)
  RR7 = o$RR75[1]
  #French
  ExploitationRatePlots(data = o[,c("Yr","ERfm","ERfl","ERfu")],lrp=RR7,lfa = i,fd=ccir.dir, save=F, title=i, French=T)
}
dev.off()

## Secondary Indicators


### Landings and Effort


h = lobster.db('annual.landings')
h <- h[order(h$YR), ]
write.csv(h, file=paste0(land.dir, "/landings.27-32.csv"), row.names=F )

h_long <- h %>%  #convert landings to merge with other fishery data in "crd"
    pivot_longer(
        cols = starts_with("LFA"),
        names_to = "LFA",
        values_to = "LANDINGS"
    ) %>%
    drop_na(LANDINGS) %>%
    mutate(LFA = str_remove(LFA, "LFA"))   # strip the "LFA" prefix for consistency




# Join with crd on YEAR + LFA
#might need to look at a RIGHT.join to maintain landings records when no CPUE
load (file=paste0(cpue.dir, "/cpueData.Rdata") ) #bring in crd dataset

fishData <- full_join(crd, h_long, by = c("YEAR" = "YR", "LFA" = "LFA"))
fishData$EFFORT2 = fishData$LANDINGS * 1000 / fishData$CPUE #add CPUE
fishData = fishData[order(fishData$LFA,fishData$YEAR),] 

#merge fishData and ER's (oo) to have a master data table
load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR2732.rdata')) #loads df "oo"

oo2 = oo
names(oo2)[names(oo2) == "Yr"] <- "YEAR"
oo_sub <- oo2[, c("LFA", "YEAR", "ERfm", "ERfl", "ERfu", "ERrmed")]
fishData <- merge(fishData, oo_sub, by = c("LFA", "YEAR"), all.x = TRUE)

RR_lookup <- RR75[, c("LFA", "ERf75")]
names(RR_lookup)[2] <- "RR"   # rename only in the copy

fishData <- merge(fishData, RR_lookup, by = "LFA", all.x = TRUE)
fishData = fishData[order(fishData$LFA, fishData$YEAR),]
fishData = subset(fishData, is.finite(CPUE))
rm(RR_lookup, oo2, oo_sub)

save(fishData, file=file.path(figdir, "fishData.RData"))
write.csv(fishData, file=file.path(figdir, "fishData.csv"), row.names = F)

#land = lobster.db('annual.landings')
#land=fishData
#logs=lobster.db("process.logs")
#CPUE.data<-CPUEModelData(p,redo=F)
##cpueData=CPUEplot(CPUE.data,lfa= p$lfas,yrs=1981:assessment.year, graphic='R')$annual.data #index end year
#land =land[order(land$YR),]


ls=c('27', '28', '29', '30')
ls2=c('31A', '31B', '32')

xlim<-c(1984,assessment.year)

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
        data=subset(fishData, YEAR<=assessment.year & LFA==lst[i]) 
        data$EFFORT2[data$YEAR<2005]=NA
        effort_max <- max(data$EFFORT2 / 1000, na.rm = TRUE) * 0.99
        
        data <- data %>%
            arrange(YEAR)
        
        par(mar=c(3,5,2.0,4.5))
        plot(data$YEAR,data$LANDINGS,ylab=ylab,type='h',xlim=xlim, xlab=" ", ylim=c(0,max(data$LANDINGS, na.rm=T)*1.2),pch=15,col='royalblue1',lwd=4,lend=3, col.lab='royalblue3', col.axis='royalblue3')
        lines(data$YEAR[nrow(data)],data$LANDINGS[nrow(data)],type='h',pch=21,col='green1',lwd=4, lend=3)
        text(x=(xlim[1]+2), y= 1.15*max(data$LANDINGS, na.rm = TRUE), lst[i], cex=1.2)
        
        par(new=T)
        
        plot(data$YEAR,data$EFFORT2/1000,ylab='',xlab='', type='b', pch=16, cex=0.8,  axes=F,xlim=xlim,ylim=c(0,effort_max))
        points(data$YEAR[nrow(data)],data$EFFORT2[nrow(data)]/1000, type='b', pch=24,cex = 1.0,bg='green1')
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
    data=subset(fishData, YEAR<=assessment.year & LFA==lst[i]) 
    data$EFFORT2[data$YEAR<2005]=NA
    effort_max <- max(data$EFFORT2 / 1000, na.rm = TRUE) * 0.99
    
    data <- data %>%
        arrange(YEAR)
    
    par(mar=c(3,5,2.0,4.5))
    plot(data$YEAR,data$LANDINGS,ylab=ylab,type='h',xlim=xlim, xlab=" ", ylim=c(0,max(data$LANDINGS, na.rm=T)*1.2),pch=15,col='royalblue1',lwd=4,lend=3, col.lab='royalblue3', col.axis='royalblue3')
   lines(data$YEAR[nrow(data)],data$LANDINGS[nrow(data)],type='h',pch=21,col='green1',lwd=4, lend=3)
    text(x=(xlim[1]+2), y= 1.15*max(data$LANDINGS, na.rm = TRUE), lst[i], cex=1.2)
    
    par(new=T)
    
    plot(data$YEAR,data$EFFORT2/1000,ylab='',xlab='', type='b', pch=16, cex=0.8,  axes=F,xlim=xlim,ylim=c(0,effort_max))
    points(data$YEAR[nrow(data)],data$EFFORT2[nrow(data)]/1000, type='b', pch=24,cex = 1.0,bg='green1')
    axis(4)
    if (i %in% c(2,4)) {mtext(efftext,cex = 0.75, side=4, line = 3, outer = F, las = 0)}

  }
dev.off()

par(mfrow =c(1,1)) 

#Individual LFAs for AC Meetings

lst=p$lfas
for (i in 1:length(lst)) {
png(filename=file.path(land.dir, paste0("Landings_LFA",lst[i],".png")),width=8, height=5.5, units = "in", res = 800)
    data=subset(fishData, YEAR<=assessment.year & LFA==lst[i]) 
    data$EFFORT2[data$YEAR<2005]=NA
    effort_max <- max(data$EFFORT2 / 1000, na.rm = TRUE) * 0.99
    
    data <- data %>%
        arrange(YEAR)
    
    par(mar=c(3,5,2.0,4.5))
    plot(data$YEAR,data$LANDINGS,ylab=ylab,type='h',xlim=xlim, xlab=" ", ylim=c(0,max(data$LANDINGS, na.rm=T)*1.2),pch=15,col='royalblue1',lwd=4,lend=3, col.lab='royalblue3', col.axis='royalblue3')
    lines(data$YEAR[nrow(data)],data$LANDINGS[nrow(data)],type='h',pch=21,col='green1',lwd=4, lend=3)
    text(x=(xlim[1]+2), y= 1.15*max(data$LANDINGS, na.rm = TRUE), lst[i], cex=1.2)
    
    par(new=T)
    
    plot(data$YEAR,data$EFFORT2/1000,ylab='',xlab='', type='b', pch=16, cex=0.8,  axes=F,xlim=xlim,ylim=c(0,effort_max))
    points(data$YEAR[nrow(data)],data$EFFORT2[nrow(data)]/1000, type='b', pch=24,cex = 1.0,bg='green1')
    axis(4)
    if (i %in% c(2,4)) {mtext(efftext,cex = 0.75, side=4, line = 3, outer = F, las = 0)}
 
dev.off()
}

### Recruitment Trap Catch Rates

FSRSvesday<-FSRSModelData()

for(i in c("27", "28", "29", "30", "31A", "31B", "32")){

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
    
    png(filename=file.path(fsrs.dir, paste('FSRSRecruitCatchRate',i,'png', sep='.')),width=8, height=6.5, units = "in", res = 800)
    FSRSCatchRatePlot(recruits = recruit[,c("YEAR","median","lb","ub")],legals=legals[,c("YEAR","median","lb","ub")],
                      lfa = i,fd=figdir,title=i, name.labels=T, save=F, rm=F, French=F) #Change French=T for french labels in figure
    dev.off()

      
  png(filename=file.path(fsrs.dir, paste('FSRSRecruitCatchRate',i,'French.png', sep='.')),width=8, height=6.5, units = "in", res = 800)
  FSRSCatchRatePlot(recruits = recruit[,c("YEAR","median","lb","ub")],legals=legals[,c("YEAR","median","lb","ub")],
                    lfa = i,fd=figdir,title=i, save=F, rm=F, French=T) #Change French=T for french labels in figure
  dev.off()
}


#All in one figure for document
#Combine online using https://products.aspose.app/pdf/merger/png-to-png




# Phase plots for conclusions and advice

load(file=paste0(figdir, "/fishData.RData"))

hcr.dir=file.path(figdir, "hcr")
dir.create( hcr.dir, recursive = TRUE, showWarnings = TRUE )

lfas2 = c("27", "29", "30", "31A", "31B", "32")

#Individual Phase plots
for(i in 1:length(lfas2)){
   x = subset(fishData,LFA==lfas2[i])
 # y = read.csv(file.path(figdir,"ccir",paste0("ExploitationRefs",lfas2[i],".csv")))
  x=x[x$YEAR>2004,]

  usr=x$usr[1]
  lrp=x$lrp[1]
  RR=x$RR[1]

   png(file=file.path(hcr.dir,paste0('PhasePlot',lfas2[i],'.png')), width=6, height=6, units = "in", res = 400)
     #hcrPlot(B=x$CPUErmed[x$YEAR>=min(y$Yr)],mF=x$ERrmed,USR=usr,LRP=lrp,RR=RR,big.final=T,
   hcrPlot(B=x$CPUErmed,mF=x$ERrmed,USR=usr,LRP=lrp,RR=RR,big.final=T,
     yrs=min(x$YEAR):p$current.assessment.year,ylims=c(0,1),xlims=NULL,labels=c('USR','LRP','RR') ,
     RRdec=F,  ylab = 'Exploitation', xlab = 'CPUE', yr.ends=T, main=paste0("LFA ",lfas2[i]), cex.main=1.6 )
  dev.off()

  png(file=file.path(hcr.dir,paste0('PhasePlot',lfas2[i],'.French.png')), width=6, height=6, units = "in", res = 400)
  hcrPlot(B=x$CPUErmed,mF=x$ERrmed,USR=usr,LRP=lrp,RR=RR,big.final=T,
                  yrs=min(x$YEAR):p$current.assessment.year,ylims=c(0,1),xlims=NULL,labels=c('USR','LRP','RR') ,
          RRdec=F,  ylab = 'Exploitation', xlab = 'CPUE', yr.ends=T, main=paste0("ZPH ",lfas2[i]) , cex.main=1.6, FrenchCPUE=T)
  dev.off()
}

#Panel Plot with all areas for HCR

#English
png(file=file.path(hcr.dir,paste0('LFAs27-32.PhasePlot.png')),width=9, height=11, units = "in", res = 400)
  par(mfrow=c(3,2))

    for(i in 1:length(lfas2)){
      x = subset(fishData,LFA==lfas2[i])
      x=x[x$YEAR>2004,]
      usr=x$usr[1]
      lrp=x$lrp[1]
      RR=x$RR[1]
    
      hcrPlot(B=x$CPUErmed,mF=x$ERrmed,USR=usr,LRP=lrp,RR=RR,big.final=T,
              yrs=min(x$YEAR):p$current.assessment.year,ylims=c(0,1),xlims=NULL,labels=c('USR','LRP','RR') ,
              RRdec=F,  ylab = 'Exploitation', xlab = 'CPUE', yr.ends=T, main=paste0("LFA ",lfas2[i]), cex.main=1.6 )
}
dev.off()


#French
png(file=file.path(hcr.dir,paste0('LFAs27-32.PhasePlot.French.png')),width=9, height=11, units = "in", res = 800)
  par(mfrow=c(3,2))

      for(i in 1:length(lfas2)){
          x = subset(fishData,LFA==lfas2[i])
          x=x[x$YEAR>2004,]
          usr=x$usr[1]
          lrp=x$lrp[1]
          RR=x$RR[1]
          
          hcrPlot(B=x$CPUErmed,mF=x$ERrmed,USR=usr,LRP=lrp,RR=RR,big.final=T,
                  yrs=min(x$YEAR):p$current.assessment.year,ylims=c(0,1),xlims=NULL,labels=c('USR','LRP','RR') ,
                  RRdec=F,  ylab = 'Exploitation', xlab = 'CPUE', yr.ends=T, main=paste0("ZPH ",lfas2[i]) , cex.main=1.6, FrenchCPUE=T)
      }
dev.off()

#----------------------------------------------------------------
#plotting as per csasdown 4 panel plot
#added directly from LFA 33 update. NEEDS TO BE LOOPED AND MODIFIED HERE

#add in the theme_csas

theme_csas <- function(base_size = 11, base_family = "", text_col = "grey20",
                       panel_border_col = "grey70") {
    half_line <- base_size / 2
    theme_light(base_size = base_size, base_family = "") +
        theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks.length = unit(half_line / 2.2, "pt"),
            strip.background = element_rect(fill = NA, colour = NA),
            strip.text.x = element_text(colour = text_col),
            strip.text.y = element_text(colour = text_col),
            axis.text = element_text(colour = text_col),
            axis.title = element_text(colour = text_col),
            legend.title = element_text(colour = text_col, size = rel(0.9)),
            panel.border = element_rect(fill = NA, colour = panel_border_col, linewidth = 1),
            legend.key.size = unit(0.9, "lines"),
            legend.text = element_text(size = rel(0.7), colour = text_col),
            legend.key = element_rect(colour = NA, fill = NA),
            legend.background = element_rect(colour = NA, fill = NA),
            plot.title = element_text(colour = text_col, size = rel(1)),
            plot.subtitle = element_text(colour = text_col, size = rel(.85))
        )
}


#format from FSAR branch of CSASdown

load(file=paste0(figdir, "/fishData.RData"))

# Catch and eff

for (i in 1:length(p$lfas)){
    
aaa=subset(fishData, LFA==p$lfas[i])
aaa$EFFORT2[aaa$YEAR<2005]=NA
aaa= aaa[order(aaa$YEAR), ]
aaa= subset(aaa, YEAR<=assessment.year)


#ymax=12000 
#scaleright = max(aaa$EFFORT2)/ymax

#Adaptive for looping:
# Compute maxima
land_max <- max(aaa$LANDINGS, na.rm = TRUE)
eff_max  <- max(aaa$EFFORT2,  na.rm = TRUE)

#Determine breaks for effort by LFA
effnum=NA
if(i==1) {effnum=2000}
if(i==2) {effnum=20}
if(i==3) {effnum=200}
if(i==4) {effnum=100}
if(i==5) {effnum=200}
if(i==6) {effnum=200}
if(i==7) {effnum=500}


# We want the highest effort point to be at 90% of the landings height
target_fraction <- 1.2

scaleright <- eff_max / (land_max * target_fraction)
ymax <- land_max * 1.20  # small buffer


g1 <- ggplot(data = aaa, aes(x = YEAR,y=LANDINGS)) +
    geom_bar(stat='identity',fill='black') +
    geom_bar(data=aaa,aes(x=YEAR,y=LANDINGS),stat='identity',fill='gray66') +
    geom_point(data=aaa,aes(x=YEAR,y=EFFORT2/scaleright),colour='black',shape=16)+
    geom_point(data=aaa,aes(x=YEAR,y=EFFORT2/scaleright),colour='grey66',shape=17,size=1.5)+
    geom_line(data=aaa,aes(x=YEAR,y=EFFORT2/scaleright),colour='black',linetype='dashed')+
    geom_bar(data=subset(aaa, YEAR==max(aaa$YEAR)),aes(x=YEAR,y=LANDINGS),stat='identity',fill='gold')+
    geom_point(data=subset(aaa, YEAR==max(aaa$YEAR)),aes(x=YEAR,y=EFFORT2/scaleright),colour='black', shape=24,bg="gold",size=2.2)+
    scale_y_continuous(name='Landings', sec.axis= sec_axis(~.*scaleright/1000, name= 'Effort',breaks = seq(0,eff_max,by=effnum)))+
    labs(x = "Year") +
    theme_csas()

#French Landings

g1.fr <- ggplot(data = aaa, aes(x = YEAR,y=LANDINGS)) +
    geom_bar(stat='identity',fill='black') +
    geom_bar(data=aaa,aes(x=YEAR,y=LANDINGS),stat='identity',fill='gray66') +
    geom_point(data=aaa,aes(x=YEAR,y=EFFORT2/scaleright),colour='black',shape=16)+
    geom_point(data=aaa,aes(x=YEAR,y=EFFORT2/scaleright),colour='grey66',shape=17,size=1.5)+
    geom_line(data=aaa,aes(x=YEAR,y=EFFORT2/scaleright),colour='black',linetype='dashed')+
    geom_bar(data=subset(aaa, YEAR==max(aaa$YEAR)),aes(x=YEAR,y=LANDINGS),stat='identity',fill='gold')+
    geom_point(data=subset(aaa, YEAR==max(aaa$YEAR)),aes(x=YEAR,y=EFFORT2/scaleright),colour='black', shape=24,bg="gold",size=2.2)+
    scale_y_continuous(name='Débarquements', sec.axis= sec_axis(~.*scaleright/1000, name= 'Effort',breaks = seq(0,eff_max,by=effnum)))+
    labs(x = "Année") +
    theme_csas()

# standardized cpue
if (p$lfa[i]==28){
    
    g2 <- ggplot(data = aaa, aes(x = YEAR)) +
        geom_point(aes(y = CPUE),size=1.5) +
        geom_line(data = subset(aaa, YEAR <= 1996),aes(y = CPUErmed),colour = "grey45" ) + #start to 1886
        geom_line(data = subset(aaa, YEAR >= 2007),aes(y = CPUErmed),colour = "grey45" )+ #from 2007 onward
        geom_point(data=subset(aaa, YEAR==max(aaa$YEAR)),aes(x=YEAR,y=CPUE),colour='black', shape=24,bg="gold",size=2.2)+
        labs(x = "Year", y = " CPUE") +
        geom_hline(yintercept=aaa$usr[1],colour='grey50',lwd=1.1,linetype='dashed')+
        geom_hline(yintercept=aaa$lrp[1],colour='grey50',lwd=1.1,linetype='dotted')+
        theme_csas() 
    
    g2.fr <- ggplot(data = aaa, aes(x = YEAR)) +
        geom_point(aes(y = CPUE),size=1.5) +
        geom_line(data = subset(aaa, YEAR <= 1996),aes(y = CPUErmed),colour = "grey45" ) + #start to 1886
        geom_line(data = subset(aaa, YEAR >= 2007),aes(y = CPUErmed),colour = "grey45" )+ #from 2007 onward
        geom_point(data=subset(aaa, YEAR==max(aaa$YEAR)),aes(x=YEAR,y=CPUE),colour='black', shape=24,bg="gold",size=2.2)+
        labs(x = "Année", y = " CPUE") +
        geom_hline(yintercept=aaa$usr[1],colour='grey50',lwd=1.1,linetype='dashed')+
        geom_hline(yintercept=aaa$lrp[1],colour='grey50',lwd=1.1,linetype='dotted')+
        theme_csas() 
}
else{
g2 <- ggplot(data = aaa, aes(x = YEAR)) +
    geom_point(aes(y = CPUE),size=1.5) +
    geom_line(aes(y= CPUErmed), colour='grey45')+
    geom_point(data=subset(aaa, YEAR==max(aaa$YEAR)),aes(x=YEAR,y=CPUE),colour='black', shape=24,bg="gold",size=2.2)+
    labs(x = "Year", y = " CPUE") +
    geom_hline(yintercept=aaa$usr[1],colour='grey50',lwd=1.1,linetype='dashed')+
    geom_hline(yintercept=aaa$lrp[1],colour='grey50',lwd=1.1,linetype='dotted')+
    theme_csas() 

g2.fr <- ggplot(data = aaa, aes(x = YEAR)) +
    geom_point(aes(y = CPUE),size=1.5) +
    geom_line(aes(y= CPUErmed), colour='grey45')+
    geom_point(data=subset(aaa, YEAR==max(aaa$YEAR)),aes(x=YEAR,y=CPUE),colour='black', shape=24,bg="gold",size=2.2)+
    labs(x = "Année", y = " CPUE") +
    geom_hline(yintercept=aaa$usr[1],colour='grey50',lwd=1.1,linetype='dashed')+
    geom_hline(yintercept=aaa$lrp[1],colour='grey50',lwd=1.1,linetype='dotted')+
    theme_csas() 
}

# Exploitation CCIR

if (aaa$LFA[1] == "28") {
    
    g3 <- ggplot() +
        annotate("text", x = 1, y = 1, 
                 label = "Data Not Available", 
                 size = 6) +
        labs(x = "Year", y = "Exploitation Index") +
        theme_csas() +
        theme(
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank()
        )
    
} else {
    
    g3 <- ggplot(data = subset(aaa, YEAR>2004), aes(x = YEAR)) +
        geom_ribbon(aes(ymin=ERfl, ymax=ERfu), fill="grey", alpha=0.22) +
        geom_point(aes(y = ERfm)) +
        geom_line(aes(y = ERfm), colour='grey', lwd=0.9, linetype='dotted') +
        geom_line(aes(y = ERrmed), colour='grey45') +
        geom_hline(yintercept=aaa$RR[1], colour='grey50', lwd=1.1, linetype='dashed') +
        scale_y_continuous(limits=c(0,1), n.breaks=6) +
        labs(x = "Year", y = "Exploitation Index") +
        theme_csas()
}


if (aaa$LFA[1] == "28") {
    
    g3.fr <- ggplot() +
        annotate("text", x = 1, y = 1, 
                 label = "Données non disponibles", 
                 size = 6) +
        labs(x = "Année", y = "Indice d'exploitation") +
        theme_csas() +
        theme(
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank()
        )
    
} else {
    
    g3.fr <- ggplot(data = subset(aaa, YEAR>2004), aes(x = YEAR)) +
        geom_ribbon(aes(ymin=ERfl,ymax=ERfu), fill="grey", alpha=0.22) +
        geom_point(aes(y = ERfm)) +
        geom_line(aes(y = ERfm), colour='grey', lwd=0.9, linetype='dotted') +
        geom_line(aes(y = ERrmed), colour='grey45') +
        geom_hline(yintercept=aaa$RR[1], colour='grey50', lwd=1.1, linetype='dashed') +
        scale_y_continuous(limits=c(0,1), n.breaks=6) +
        labs(x = "Année", y = "Indice d'exploitation") +
        theme_csas()
}


# Recruitment 
rec=read.csv(file.path(fsrs.dir, paste0("FSRSRecruitCatchRate", p$lfas[i], ".recruits.csv")))


if (aaa$LFA[1] == "28") {
    
    g4 <- ggplot() +
        annotate("text", x = 1, y = 1, 
                 label = "Data Not Available", 
                 size = 6) +
        labs(x = "Year", y = "Recruitment Index") +
        theme_csas() +
        theme(
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank()
        )
    
} else {
    
    g4 <- ggplot(data = rec, aes(x = YEAR)) +
        geom_ribbon(aes(ymin = lb, ymax = ub), fill = "grey", alpha = 0.2) +
        geom_point(aes(y = median)) +
        geom_line(aes(y = median), colour = "grey45") +
        coord_cartesian(ylim = c(0, 1.1 * max(rec$ub))) +
        scale_y_continuous(n.breaks = 5) +
        labs(x = "Year", y = "Recruitment Index") +
        theme_csas()
}


if (aaa$LFA[1] == "28") {
    
    g4.fr <- ggplot() +
        annotate("text", x = 1, y = 1, 
                 label = "Données non disponibles", 
                 size = 6) +
        labs(x = "Année", y = "Indice de recrutement") +
        theme_csas() +
        theme(
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank()
        )
    
} else {
    
    g4.fr <- ggplot(data = rec, aes(x = YEAR)) +
        geom_ribbon(aes(ymin = lb, ymax = ub), fill = "grey", alpha = 0.2) +
        geom_point(aes(y = median)) +
        geom_line(aes(y = median), colour = "grey45") +
        coord_cartesian(ylim = c(0, 1.1 * max(rec$ub))) +
        scale_y_continuous(n.breaks = 5) +
        labs(x = "Année", y = "Indice de recrutement") +
        theme_csas()
}



fsrplot=cowplot::plot_grid(g1, g2, g3, g4, ncol = 2, labels = "AUTO",label_x=0.15,label_y=0.98, label_size = 15, align = "hv")
fsrplot.fr=cowplot::plot_grid(g1.fr, g2.fr, g3.fr, g4.fr, ncol = 2, labels = "AUTO",label_x=0.15,label_y=0.98, label_size = 15, align = "hv")


figname=paste0("LFA", p$lfa[i], "fsr.panel.plot.png")
figname.fr=paste0("LFA", p$lfa[i], "fsr.panel.plot.fr.png")

png(filename=file.path(csas.dir,figname), width=1200, height=900, res=125)
print(fsrplot)
dev.off()	  

#French version
png(filename=file.path(csas.dir,figname.fr), width=1200, height=900, res=125)
print(fsrplot.fr)
dev.off()

}



# Fishery footprint- Useful in comparing years, etc
#------------------------------------------------------------
#setwd(grid.dir)

layerDir=file.path(code_root,"bio.lobster.data", "mapping_data")
r<-readRDS(file.path( layerDir,"GridPolys_DepthPruned_37Split.rds"))
r = st_as_sf(r)

a =  lobster.db('process.logs')
a = subset(a,SYEAR>2004 & SYEAR<=assessment.year)
b = lobster.db('seasonal.landings')
b = subset(b,!is.na(SYEAR))
b$SYEAR = 1976:assessment.year
b$LFA38B <- NULL
b = subset(b,SYEAR>2004 & SYEAR<=assessment.year)
b = reshape(b,idvar='SYEAR', varying=list(2:6),direction='long')
num.yr=length(2005:assessment.year)
b$LFA=rep(c(33,34,35,36,38),each=num.yr)
b$time <- NULL
names(b)[1:2]=c('YR','SlipLand')


d = lobster.db('annual.landings')
d = subset(d,YR>2004 & YR<=assessment.year, select=c(YR,LFA27,LFA28,LFA29,LFA30,LFA31A,LFA31B,LFA32))
d = reshape(d,idvar='YR', varying=list(2:8),direction='long')
d$LFA=rep(c(27,28,29,30,'31A','31B',32),each=num.yr)
d$time <- NULL
names(d)[1:2]=c('YR','SlipLand')
bd = rbind(d,b)

bup = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SYEAR+LFA,data=a,FUN=sum)
bup$CPUE = bup$WEIGHT_KG/bup$NUM_OF_TRAPS
bAll = merge(bd,bup,by.x=c('YR','LFA'),by.y=c('SYEAR','LFA'))

sL= split(a,f=list(a$LFA, a$SYEAR))
sL = rm.from.list2(sL)
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
g = subset(g,SYEAR>2004 & SYEAR<=assessment.year)

gg = aggregate(SD_LOG_ID~LFA+GRID_NUM+SYEAR,data = g,FUN=function(x) length(unique(x)))

saveRDS(gg,'SDLOGSWithinGrid.rds')

#############merge
#Licenses By Grid and Week

g = lobster.db('process.logs')
g = subset(g,SYEAR>2004 & SYEAR<=assessment.year)

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

GrMap = readRDS(file.path( layerDir,"GridPolys_DepthPruned_37Split.rds"))
#ex.grids=c("27-357.1", "27-357.3", "27-357.4", "27-357.6", "27-358.1", "27-356.1", "311-338.3", "311-338.4", "311-337.4")
#GrMap=GrMap[row.names(GrMap) %ni% ex.grids,] #Removes polygons offshore in LFA 27
coa = st_as_sf(readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastlineSF_NY_NL.rds")))


GrMap1 = GrMap
GrMap1$area = st_area(GrMap1)/1000000
GrMap1$V2 = paste(GrMap1$LFA, GrMap1$GRID_NO,sep="-")
st_geometry(GrMap1)<- NULL
gg = aggregate(area~LFA+GRID_NO,data=GrMap1,FUN=function(x) abs(sum(x)))

GrMap2 =merge(GrMap,gg)

gTot = merge(GrMap2,Tot,by.x=c('LFA','GRID_NO'),by.y=c('LFA','Grid'),all.x=T)


r<-readRDS(file.path( layerDir,"GridPolys_DepthPruned_37Split.rds"))
b=subset(r,LFA %in% c(27:32, 311, 312))

o=subset(GrMap,LFA %in% c(27:32, 311, 312))

ggplot(b)+
  geom_sf()+
  geom_sf(data=coa,fill='grey')+
  geom_sf(data=o,fill='red')+
  coord_sf(xlim = c(st_bbox(b)$xmin,st_bbox(b)$xmax),
           ylim = c(st_bbox(b)$ymin,st_bbox(b)$ymax),
           expand = FALSE)


gTot$CPUE = gTot$Landings/gTot$TrapHauls
g27p = subset(gTot, LFA%in% c(27:32, 311, 312) & FishingYear%in%2019:assessment.year)
g27p <- g27p %>%
    mutate(LFA = case_when(
        LFA == "311" ~ "31A",
        LFA == "312" ~ "31B",
        TRUE ~ LFA  # Keep other values unchanged
    ))


#Create a cpue chart by year for 27-32
#One figure 27-32 for context
#past year only

ok = function(x = g27p, yr=assessment.year) {
    # Filter the data to include only the rows for the specified FishingYear
    x_filtered <- x %>% filter(FishingYear == yr)
    
    # Plot the data
    ggplot(x_filtered, aes(fill = CPUE)) +
        geom_sf() +
        scale_fill_distiller(trans = 'identity', palette = 'Spectral') +
        geom_sf(data = coa, fill = 'grey') +
        geom_sf(data = GrMap, fill = NA) +
        coord_sf(xlim = c(st_bbox(x_filtered)$xmin, st_bbox(x_filtered)$xmax),
                 ylim = c(st_bbox(x_filtered)$ymin, st_bbox(x_filtered)$ymax),
                 expand = FALSE) +
        scale_x_continuous(breaks = c(round(seq(st_bbox(x_filtered)$xmin, st_bbox(x_filtered)$xmax, length.out = 2), 2))) +
        scale_y_continuous(breaks = c(round(seq(st_bbox(x_filtered)$ymin, st_bbox(x_filtered)$ymax, length.out = 2), 2)))+
        theme(
            axis.text.y = element_blank()  # Remove the labels on the y-axis
        )
}

png(filename=file.path(cpue.dir, "current_grid_cpue_all_lfas.png"), width=1200, height=900, res=175)
print(ok())
dev.off()


#past 6 years
ok1 = function(x=g27p){
  ggplot(x,aes(fill=CPUE))+
  geom_sf() +
  scale_fill_distiller(trans='identity',palette='Spectral') +
  facet_wrap(~FishingYear)+
  geom_sf(data=coa,fill='grey')+
  geom_sf(data=GrMap,fill=NA)+
  coord_sf(xlim = c(st_bbox(x)$xmin,st_bbox(x)$xmax),
           ylim = c(st_bbox(x)$ymin,st_bbox(x)$ymax),
           expand = FALSE)+
  scale_x_continuous(breaks = c(round(seq(st_bbox(x)$xmin,st_bbox(x)$xmax,length.out=2),2)))+
  scale_y_continuous(breaks = c(round(seq(st_bbox(x)$ymin,st_bbox(x)$ymax,length.out=2),2)))+
        theme(
            axis.text.y = element_blank()  # Remove the labels on the y-axis
        )
}

png(filename=file.path(cpue.dir, "multiyear_grid_cpue_all_lfas.png"), width=1200, height=900, res=175)
print(ok1())
dev.off()


ls.no28=c("27","29","30","31A","31B","32")

#One time series CPUE map for each LFA
for (xx in ls.no28){
    png(filename=file.path(cpue.dir, paste(xx,"grid_cpue.png", sep="_")), width=1600, height=900, res=175)
    gpxx=subset(g27p, LFA==xx)
    if (length(gpxx$LFA>0)) {print(ok1(x=gpxx))}
    dev.off()	
}

#Comparative CPUE Maps
#Can run this line to only take certain LFAs
#g27p=g27p[g27p$LFA %in% c('27','28','29','30','311','312','32'),]

#Slice out individual years for comparison
gl = subset(g27p,FishingYear==assessment.year-1)

gp = subset(g27p,FishingYear==assessment.year)

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
          axis.text.y = element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          plot.margin=grid::unit(c(8,2,8,2), "mm"))+
    scale_x_continuous(breaks = c(round(seq(st_bbox(tt)$xmin,st_bbox(tt)$xmax,length.out=2),2)))+
    scale_y_continuous(breaks = c(round(seq(st_bbox(tt)$ymin,st_bbox(tt)$ymax,length.out=2),2)))+
    #annotate("text", x=(st_bbox(tt)$xmax)-0.2, y=(st_bbox(tt)$ymin)+0.05, label= lab, size=tsize )
    #annotate("text", x = Inf, y = -Inf, label = lab, vjust = vj, hjust = 1.2, size=tsize)
    #annotate("text", x = Inf, y = -Inf, label = lab, vjust = vj, hjust = 1.2, size=tsize)
    annotate("text", x = Inf, y = -Inf, label = lab, hjust = 1.2, vjust = -1, size = tsize)
}

#Add the following to ensure that text appears in the final pngs
library(showtext)
showtext_auto()
showtext_opts(dpi = 200)


#One figure 27-32 for context
png(filename=file.path(cpue.dir, "cpue_diff_all_lfas.png"), width=1300, height=1000, res=175)
print(cpue.diff())
dev.off()	

#Each Separate

for (xx in ls.no28){
tt=subset(gg, LFA==xx)
if(xx=="27") {scale_y_continuous(breaks = c(round(seq(st_bbox(tt)$ymin,st_bbox(tt)$ymax,length.out=2),2)))}
if(xx=="27"){tsize=3}else{tsize=6}
if(xx=="27"){vj=30}else{vj=20}
if (length(tt$LFA>0)) {print(cpue.diff(tsize=tsize, vj=vj))}
if(xx=="27"){png(filename=file.path(cpue.dir, paste(xx,"cpue.diff.png", sep="_")), width=700, height=900, res=175)}else
  {png(filename=file.path(cpue.dir, paste(xx,"cpue.diff.png", sep="_")), width=1600, height=900, res=175)}
if (length(tt$LFA>0)) {print(cpue.diff(tsize=tsize, vj=vj))}
dev.off()	
}

#--------------------------------------------------
#Landings per vessel by LFA
pr= lobster.db("percent_reporting")

# Create the base u.logs with Year and Month
u.logs <- data.frame(
    Year = substr(pr$YEARMTH, 1, 4),  # Extract Year from YEARMTH
    Month = as.numeric(substr(pr$YEARMTH, 5, 6))  # Extract Month as numeric
)

# Loop through the LFA columns (L27MISS, L27RECD, L28MISS, L28RECD, ...)
lfa_numbers <- c("27", "28", "29", "30", "31A", "31B", "32", "33", "34", "35", "36", "38")
for (lfa in lfa_numbers) {
    miss_column <- paste0("L", lfa, "MISS")
    recd_column <- paste0("L", lfa, "RECD")
    
    # Check if the columns exist in the data frame (to prevent errors)
    if (miss_column %in% colnames(pr) && recd_column %in% colnames(pr)) {
        # Sum the MISS and RECD columns for each LFA and add to u.logs
        u.logs[[paste0("L", lfa, "_MISS_RECD")]] <- pr[[miss_column]] + pr[[recd_column]]
    } else {
        # If the column doesn't exist, add a column with NA values
        u.logs[[paste0("L", lfa, "_MISS_RECD")]] <- NA
    }
}

u.logs=subset(u.logs, as.numeric(Year)<=assessment.year)

# Pivot the data to long form, gathering the LFA columns into a single column
u.logs_long <- u.logs %>%
    pivot_longer(cols = starts_with("L"), 
                 names_to = "LFA", 
                 values_to = "MISS_RECD") %>%
    mutate(LFA = gsub("L([0-9]+)_.*", "\\1", LFA))  # Extract numerals between L and _

# Group by Year and LFA, then filter to keep only the rows with the max MISS_RECD for each group
# Create u.logs_max with the max value of MISS_RECD for each Year and LFA
u.logs_max <- u.logs_long %>%
    group_by(Year, LFA) %>%   # Group by Year and LFA
    summarise(MISS_RECD = max(MISS_RECD, na.rm = TRUE)) %>%  # Find the max MISS_RECD
    ungroup()  # Ungroup the data

library(zoo)

u.logs_max <- u.logs_max %>%
    mutate(MISS_RECD = ifelse(MISS_RECD == -Inf, NA, MISS_RECD)) %>%  # Replace -Inf with NA
    arrange(LFA, Year) %>%  # Sort by LFA and Year
    group_by(LFA) %>%  # Group by LFA
    mutate(MISS_RECD = zoo::na.locf(MISS_RECD, na.rm = FALSE, fromLast = TRUE)) %>%  # Forward fill
    ungroup()

# Rename the column MISS_RECD to tot.lic
u.logs_max <- u.logs_max %>%
    rename(tot.lic = MISS_RECD)



# Remove the Month column if it exists using base R
if ("Month" %in% colnames(u.logs_max)) {
    u.logs_max$Month <- NULL  # Remove the Month column
}

# Sort the data by Year and LFA
u.logs_max <- u.logs_max %>%
    arrange(Year, LFA)  # Sort by Year first, then LFA


u.logs_max$Year <- as.double(u.logs_max$Year)



#alternate approach. Using logs
logs=lobster.db("process.logs")

u.vrn <- logs %>%
    group_by(SYEAR, LFA, DATE_FISHED) %>%  # Group by SYEAR, LFA, and DATE_FISHED
    summarise(unique_vr_count = n_distinct(VR_NUMBER), .groups = "drop") %>%  # Count unique VR_NUMBER per DATE_FISHED
    group_by(SYEAR, LFA) %>%  # Group by SYEAR and LFA
    summarise(max_unique_vr = max(unique_vr_count), .groups = "drop")  # Find the max number of unique VR_NUMBER per group

u.vrn=subset(u.vrn, SYEAR %in% c((assessment.year-2):assessment.year))


#Bring in landings

land.s = lobster.db('seasonal.landings') #Seasonal for 33-28
land.s$year = as.numeric(substr(land.s$SYEAR,6,9))
land.s=subset(land.s, year %in% c((assessment.year-2):assessment.year) )

land = lobster.db('annual.landings')
land=subset(land, YR %in% c((assessment.year-2):assessment.year) )
land=subset(land, select= -c(LFA33, LFA34, LFA35, LFA36, LFA38, LFA38B, LFA41))

landings=merge(land, land.s, by.x = "YR", by.y="year")
landings=subset(landings, select= -c(SYEAR, LFA38B))

landings_long <- landings %>%
    pivot_longer(cols = starts_with("LFA"),   # Select all columns starting with "LFA"
                 names_to = "LFA",            # The new column name for LFA
                 values_to = "landings") %>%
    mutate(LFA = sub("LFA", "", LFA))  # Remove the "L" prefix from LFA values




# bring in missing logs
#bumps up current year by missing logs

per.rec= lobster.db("percent_reporting")
per.rec <- per.rec[order(per.rec$YEARMTH), ]
per.rec <- per.rec[grepl(assessment.year, per.rec$YEARMTH), ]
columns_to_keep <- c("YEARMTH", grep("L27|L28|L29|L30|L31A|L31B|L32", colnames(per.rec), value = TRUE))
per.rec <- per.rec[, columns_to_keep]
per.rec <- per.rec[!apply(per.rec[, -which(names(per.rec) == "YEARMTH")], 1, function(x) all(is.na(x))), ]
lfs= c("L27","L28", "L29", "L30" , "L31A", "L31B", "L32")

# Initialize an empty dataframe to store the results
perc.log.rec <- data.frame(lfa = character(0), perc.rec = numeric(0))

# Loop through variables in lfs
for (var in lfs) {
    
    # Column for "MISS"
    miss_column <- grep(paste0("^", var, "MISS"), colnames(per.rec), value = TRUE)
    if (length(miss_column) > 0) {
        miss = sum(per.rec[, miss_column], na.rm = TRUE)
    }
    
    # Column for "RECD"
    recd_column <- grep(paste0("^", var, "RECD"), colnames(per.rec), value = TRUE)
    if (length(recd_column) > 0) {
        recd = sum(per.rec[, recd_column], na.rm = TRUE)
    }
    
    # Calculate the percentage
    percent.rec = recd / (miss + recd) * 100
    percent.rec = round(percent.rec, 1)
    
    # Append the result to the dataframe
    perc.log.rec <- rbind(perc.log.rec, data.frame(lfa = var, perc.rec = percent.rec))
}

perc.log.rec$YR=assessment.year

# Remove the "L" prefix from perc.log.rec$lfa
perc.log.rec$lfa <- sub("L", "", perc.log.rec$lfa)

# Perform a left join on landings_long and perc.log.rec based on LFA and YR
adj.land <- landings_long %>%
    left_join(perc.log.rec, by = c("LFA" = "lfa", "YR" = "YR"))
adj.land$perc.rec[!is.finite(adj.land$perc.rec)]=100

adj.land$adj.land=adj.land$landings/(adj.land$perc.rec/100)

adj.land <- merged_data %>%
    rename(Year = YR)

# Now perform the left join
adj.land <- adj.land %>%
    left_join(u.logs_max, by = c("LFA", "Year"))

adj.land$mt.per.boat=adj.land$landings/adj.land$tot.lic
adj.land$lb.per.boat=adj.land$mt.per.boat*2204

# Load the ggplot2 package
library(ggplot2)

# Create the bar plot with stacked facets and no legend
ggplot(adj.land, aes(x = LFA, y = lb.per.boat, fill = LFA)) +
    geom_bar(stat = "identity") +  # Use 'identity' to plot actual values
    facet_wrap(~ Year, ncol = 1) +  # Facet by Year in stacked format
    labs(title = "Landings per Boat (lb) by LFA", 
         x = "LFA", 
         y = "Landings per Boat (lb)") +  # Add labels
    theme_minimal() +  # Use a minimal theme
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        legend.position = "none"  # Remove the legend
    )



#####----------------------------------------------------------------------------
# Environmental / Temperature

## Temperature (from FSRS)
p$lfas = c("27", "29", "30", "31A", "31B", "32")


lobster.db('fsrs') #brings in all fsrs data in df "fsrs"
    
    fsrs$LFA=as.character(fsrs$LFA)
    fsrs$LFA[fsrs$LFA == "31.1"] = "31A"
    fsrs$LFA[fsrs$LFA == "31.2"] = "31B"
    fsrs$TEMP[fsrs$TEMP < -1] = NA
    
    fsrs$HAUL_DATE=as.Date(fsrs$HAUL_DATE,"%Y-%m-%d", tz="UTC")
    
    
    

    Fish.Date = lobster.db('season.dates')
    Fish.Date$START_DATE[Fish.Date$LFA=="31B" & Fish.Date$SYEAR=="2024"]="2024-04-19"
    Fish.Date$START_DATE[Fish.Date$LFA=="29" & Fish.Date$SYEAR=="2024"]="2024-04-30"
    
# add day (DOS) and week (WOS) of season variable
fsrs$DOS = fsrs$WOS = NA


for(i in 1:length(p$lfas)) {
    h  =  Fish.Date[Fish.Date$LFA==p$lfas[i],]
    for(j in sort(unique(fsrs$SYEAR[fsrs$LFA==p$lfas[i]]))){
        print(c(p$lfas[i],j))
        fsrs$DOS[fsrs$SYEAR==j&fsrs$LFA==p$lfas[i]] = fsrs$HAUL_DATE[fsrs$SYEAR==j&fsrs$LFA==p$lfas[i]]-h$START_DATE[h$SYEAR==j]+1
        fsrs$WOS[fsrs$LFA==p$lfas[i]&fsrs$SYEAR==j] = floor(as.numeric(fsrs$HAUL_DATE[fsrs$LFA==p$lfas[i]&fsrs$SYEAR==j]-min(h$START_DATE[h$SYEAR==j]))/7)+1
    }
}

fsrs=subset(fsrs, WOS %in% c(1:9)) #removes data with erroneous WOS
fsrs$unq=paste(fsrs$VESSEL_CD, fsrs$HAUL_DATE, sep=":") #unique identifier for vessel and date

saveRDS(fsrs, file.path( temp.dir, "fsrs.temps.rds"))

temp.sum <- fsrs %>%
    group_by(unq) %>%
    summarise(
        TEMP = mean(TEMP, na.rm = TRUE),         # Average of TEMP
        WOS = first(WOS, order_by = unq),           # Average of WOS
        LFA = first(LFA, order_by = unq),  # First LFA
        SYEAR = first(SYEAR, order_by = unq)  # First SYEAR
    )

saveRDS(fsrs, file.path( temp.dir, "fsrs.weekly.temps.rds"))

#The following plots the temp anomalies for each LFA for the past 9 years

lfas.no28 = c("27", "29", "30", "31A", "31B", "32")
for (jj in lfas.no28){
    past_nine_years <- assessment.year - 8  # The starting year of the last 9 years
    
    temp.sum_filtered <- temp.sum %>%
        filter(LFA == jj, SYEAR >= past_nine_years) %>%  # Filter for LFA == 29 and the past 9 years
        group_by(WOS, SYEAR) %>%  # Group by WOS and SYEAR (year)
        summarise(mean_TEMP = mean(TEMP, na.rm = TRUE))  # Calculate mean of TEMP for each group
    
    # Step 2: Calculate the overall average TEMP by WOS across all years past 20 years
    temp_sum_20 <- temp.sum %>%
        filter(SYEAR >= assessment.year-19)  # Filter for past 20 years
    
    overall_avg_TEMP_20 <- temp_sum_20 %>%
        filter(LFA == jj) %>%  # Filter for LFA == 29
        group_by(WOS) %>%
        summarise(overall_avg = mean(TEMP, na.rm = TRUE))  # Calculate the average TEMP by WOS
    
    # Step 3: Plot the data using ggplot2 with a red line for the overall average TEMP
    
     p <- ggplot(temp.sum_filtered, aes(x = WOS, y = mean_TEMP)) +
        geom_line() +  # Plot the mean TEMP by WOS for each year
        geom_line(data = overall_avg_TEMP_20, aes(x = WOS, y = overall_avg), color = "red", linewidth = .7) +  # Add the red line for overall average TEMP since 2009
        facet_wrap(~ SYEAR, scales = "free_y") +  # Separate panels by year (SYEAR)
        labs(title = paste0("Bottom Temperature by Week LFA",jj), x = "Week", y = expression("Temperature (°C)")) +
         scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, 2)) +  # Set y-axis limits to 0 to 14 with breaks at 2
         theme_minimal() 
    
 
    # Step 4: Add annotation outside the plot using patchwork's plot_annotation()
 final_plot <- p + 
        plot_annotation(
            caption = "*Red line is 20 year climatology",
            theme = theme(plot.title = element_text(size = 16),
            plot.caption = element_text(size = 12, hjust = 0, color="red")  # Caption size and alignment (left)
            ))
        
  
    
# Save the final plot  
ggsave(final_plot,filename=file.path(temp.dir, paste0("LFA",jj,"_weekly_temps.png")), width=10, height=8, dpi=300)

temp.sum_fil <- temp.sum %>%
    filter(LFA == jj, SYEAR >= assessment.year-19) %>%  # Filter for LFA == 29 and the past 9 years
    group_by(WOS, SYEAR) %>%  # Group by WOS and SYEAR (year)
    summarise(mean_TEMP = mean(TEMP, na.rm = TRUE))  # Calculate mean of TEMP for each group

filtered_data <- temp.sum_fil %>%
    filter(WOS %in% c(2, 5, 8))

# Find the most recent year in the data
most_recent_year <- max(filtered_data$SYEAR)

# Filter the data for the last 7 years
last_7_years_data <- filtered_data %>%
    filter(SYEAR >= most_recent_year - 6)

# Create the plot with trend lines
pp= ggplot(filtered_data, aes(x = SYEAR, y = mean_TEMP)) +
    geom_line() +
    geom_point() +
    # Trend line for the entire period
    geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +  
    # Trend line for the last 7 years
    geom_smooth(data = last_7_years_data, method = "lm", se = FALSE, color = "red", linetype = "solid") + 
    facet_wrap(~ WOS, scales = "fixed", 
               labeller = labeller(WOS = c("2" = "Week 2", "5" = "Week 5", "8" = "Week 8"))) +  # Create separate plots for each WOS
    labs(title = paste0("Average Temperature by Week of Season LFA",jj ),
         x = "Year", y = "Mean Temperature") +
    theme_minimal()

fin_plot <- pp + 
    plot_annotation(
        caption = "*Blue is 20 year trend, red is 7 year trend",
        theme = theme(plot.title = element_text(size = 16),
                      plot.caption = element_text(size = 12, hjust = 0, color="black")  # Caption size and alignment (left)
        ))

ggsave(fin_plot,filename=file.path(temp.dir, paste0("LFA",jj,"temp_trends_WOS.png")), width=10, height=8, dpi=300)

}

# all temp anomalies for assessment.year by LFA
plot_list <- list()
temp_filtered_list <- list()

p$lfas = c("27", "29", "30", "31A", "31B", "32")

for (jj in p$lfas) {
    past_nine_years <- assessment.year - 8  # The starting year of the last 9 years
    
    # Filter for the last 9 years and calculate mean TEMP
    temp.sum_filtered <- temp.sum %>%
        filter(LFA == jj, SYEAR >= past_nine_years) %>%  # Filter for LFA == jj and the past 9 years
        group_by(WOS, SYEAR) %>%  # Group by WOS and SYEAR (year)
        summarise(mean_TEMP = mean(TEMP, na.rm = TRUE))  # Calculate mean of TEMP for each group
    
    # Add the LFA column to the filtered data and populate it with jj
    temp.sum_filtered <- temp.sum_filtered %>%
        mutate(LFA = jj)  # Add the LFA column with the current value of jj
    
    # Append the filtered data to the list
    temp_filtered_list[[jj]] <- temp.sum_filtered
    
    # Step 2: Calculate the overall average TEMP by WOS across all years past 20 years
    temp_sum_20 <- temp.sum %>%
        filter(SYEAR >= assessment.year - 19)  # Filter for past 20 years
    
    overall_avg_TEMP_20 <- temp_sum_20 %>%
        filter(LFA == jj) %>%  # Filter for LFA == jj
        group_by(WOS) %>%
        summarise(overall_avg = mean(TEMP, na.rm = TRUE))  # Calculate the average TEMP by WOS
    
    # Step 3: Filter for 2024 data and plot the data using ggplot2 with a red line for the overall average TEMP
    temp.sum_filtered_2024 <- temp.sum_filtered %>%
        filter(SYEAR == 2024)  # Filter to keep only the 2024 data
    
    overall_avg_TEMP_20_2024 <- overall_avg_TEMP_20 %>%
        filter(WOS %in% temp.sum_filtered_2024$WOS)  # Ensure matching WOS values for 2024
    
    p_2024 <- ggplot(temp.sum_filtered_2024, aes(x = WOS, y = mean_TEMP)) +
        geom_line() +  # Plot the mean TEMP by WOS for each year
        geom_line(data = overall_avg_TEMP_20_2024, aes(x = WOS, y = overall_avg), color = "red", linewidth = .7) +  # Add the red line for overall average TEMP
        labs(title = jj, x = "Week", y = expression("Temperature (°C)")) +
        scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, 2)) +  # Set y-axis limits to 0 to 14 with breaks at 2
        scale_x_continuous(limits = c(0, 9), breaks = seq(0, 9, 2)) +  # Set x-axis limits to 0 to 9 with breaks at 2
        theme_minimal() +
        theme(legend.position = "none")  # Hide the legend since it's not needed
    
    # Add the 2024 plot for the specific LFA to the plot list
    #plot_list=list()
    plot_list[[paste0("LFA_", jj)]] <- p_2024
}

combined_temp_filtered <- bind_rows(temp_filtered_list)
saveRDS(combined_temp_filtered, file.path(temp.dir, "fsrs.weekly.temps.rds"))

# Combine all the 2024 plots into a single figure
combined_plot <- wrap_plots(plot_list, ncol = 3) + 
    plot_annotation(
        title = "Temperature Anomalies by LFA",  # Title for the combined plot
        subtitle = NULL,  # No subtitle
        caption = "*Red line is 20 year LFA-specific climatology",  # Caption at the bottom right
        theme = theme(
            plot.margin = margin(0, 0, 0, 0),  # Optional: Adjust margin if needed
            plot.caption = element_text(size = 10, hjust = 0, color="red"),  # Caption size and alignment (left)
            plot.title = element_text(size = 14, hjust = 0)  # Title size and alignment
        )
    ) 

# Display the combined plot
combined_plot

# Save the combined plot
ggsave(combined_plot, filename = file.path(temp.dir, paste0(assessment.year, "_fsrs_temp_anomalies_by_lfa.png")), width = 8 , height = 5, dpi = 300)


#Glorys Temp Model Data

layerDir=file.path(code_root,"bio.lobster.data", "mapping_data")
r<-readRDS(file.path( layerDir,"GridPolys_DepthPruned_37Split.rds"))
r = st_as_sf(r)

glorys <- st_as_sf(readRDS("C:/bio.data/bio.lobster/assessments/Updates/LFA27-32/2024/temp/ClimatologyGlorys20232024.rds")) #Import data
coa = st_as_sf(readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastlineSF_NY_NL.rds")))

library(RColorBrewer)
cols <- rev(rainbow(7)[-7])

# Step 2: Plot the sf object with ggplot2 with panels for years for May-June
ano.by.year = ggplot(subset(glorys, mn %in% c(5:6))) +
    geom_sf(aes(fill = ano, color = ano)) +  # Map the 'ano' variable to fill color
    scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, guide = "none") +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, guide = guide_colorbar(barwidth = 1, barheight = 4)) +
    geom_sf(data = coa, fill = 'grey') +
    labs(title = "Temperature Anomalies", fill = expression("°C")) +  # Title and legend label
    facet_wrap(vars(yr)) +
    coord_sf(xlim = c((st_bbox(glorys)$xmin) + 4, st_bbox(glorys)$xmax),
             ylim = c((st_bbox(glorys)$ymin) + 3, (st_bbox(glorys)$ymax) - 0.5),
             expand = FALSE) +
    theme(axis.text = element_blank(),  # Remove axis tick labels
          axis.ticks = element_blank(),  # Remove axis tick marks,
          plot.title = element_text(size = 10),
          legend.text = element_text(size = 6),
          legend.title = element_text(size = 8),
          legend.position = "right",  # Put legend to the right
          legend.margin = margin(t = 10),
          strip.text = element_text(size = 8),  # Adjust strip text size (facet labels)
          strip.background = element_rect(fill = "lightgray", color = "gray"),  # Facet label background
          axis.title.x = element_blank(),  # Remove x-axis title
          axis.title.y = element_blank()   # Remove y-axis title
    )

ano.by.year

ggsave(ano.by.year,filename=file.path(temp.dir, paste0("gloys_temp_anomoly_by_year.png")), width=6, height=3, dpi=180)

#most recent year only
ano.last.yr=ggplot(subset(glorys, mn %in% c(5:6) & yr==assessment.year)) +
    geom_sf(aes(fill = ano, color= ano)) +  # Map the 'ano' variable to fill color
    scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, guide="none")+
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, guide = guide_colorbar(barwidth = 1, barheight = 4))+
    geom_sf(data=coa,fill='grey')+
    labs(title = "Temperature Anomolies May/June",
         fill = expression("°C")) + # Title and legend label
    coord_sf(xlim = c((st_bbox(glorys)$xmin)+4,st_bbox(glorys)$xmax),
             ylim = c((st_bbox(glorys)$ymin)+3,(st_bbox(glorys)$ymax)-0.5),
             expand = FALSE)+
    theme(axis.text = element_text(size = 5),
          plot.title = element_text(size = 10),
          legend.text = element_text(size = 6),
          legend.title = element_text(size = 8),
          legend.position = c(1.1, 0.25),
          legend.margin = margin(t = 10)) 

ano.last.yr
ggsave(ano.last.yr,filename=file.path(temp.dir, paste0(assessment.year, "_glorys_temp_anomalies.png")), width=6, height=3, dpi=180)

ano.by.month=ggplot(subset(glorys, yr=="2024")) +
    geom_sf(aes(fill = ano, color= ano)) +  # Map the 'ano' variable to fill color
    scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, guide="none")+
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)+
    geom_sf(data=coa,fill='grey')+
    labs(title = "Temperature Anomolies",
         fill = expression("°C")) + # Title and legend label
    facet_wrap(vars(mn)) +
    coord_sf(xlim = c((st_bbox(glorys)$xmin)+4,st_bbox(glorys)$xmax),
             ylim = c((st_bbox(glorys)$ymin)+3,(st_bbox(glorys)$ymax)-0.5),
             expand = FALSE)
ano.by.monthsummary(f.temps$doy)


#-------------------------------------------------------------------------------
#Investigate link between temps and CCIR

ex=read.csv(file.path(ccir.dir, "ccir.27-32.csv"))
tp=data.frame(readRDS(file.path(temp.dir, "fsrs.weekly.temps.rds")))


# Ensure dplyr is loaded
library(dplyr)

# Filter the tp data frame where WOS = 1 and select the necessary columns
tp_wos1 <- tp %>%
    filter(WOS == 1) %>%
    dplyr::select(SYEAR, LFA, mean_TEMP)  # Specify the package to avoid conflicts

# Perform the join between ex and the filtered tp data frame
ex_with_temp <- ex %>%
    left_join(tp_wos1, by = c("Yr" = "SYEAR", "LFA" = "LFA")) %>%
    filter(!is.na(mean_TEMP)) %>%  # Remove rows where there's no match for mean_TEMP
    rename(WOS1.temp = mean_TEMP)  # Rename mean_TEMP to WOS1.temp

# View the resulting ex data frame with the new column
head(ex_with_temp)

# Rename the columns before plotting
ex_with_temp_renamed <- ex_with_temp %>%
    rename(
        "Week 1 Temp" = WOS1.temp,
        "CCIR Exploitation" = ERfm
    )

# Plot the data
ggplot(ex_with_temp_renamed, aes(x = `Week 1 Temp`, y = `CCIR Exploitation`)) +
    geom_point() +  # Create scatter plot
    facet_wrap(~ LFA) +  # Facet by LFA
    labs(
        title = "Scatter plot of Week 1 Temp vs CCIR Exploitation",
        x = "Week 1 Temp (°C)",
        y = "CCIR Exploitation"
    ) +
    theme_minimal()  # Apply a minimal theme

#-------------------------------------------------------------------------------
#Lobsters R Us (Blaire Martell) provided data on incoming lobster health at the plant

x=readRDS(project.datadirectory( "bio.lobster","requests","season.dates", "lru", "lobsters.r.us.master.rds"))
x$yrdoy=paste(x$yr, x$doy, sep=":")

f.temps=readRDS(file.path(temp.dir, "fsrs.temps.rds")) #bring in fsrs temps to merge into the data set
f.temps$doy=yday(f.temps$HAUL_DATE)
f.temps=subset(f.temps, LFA %in% c("29", "30") & SYEAR > 2019)

f.sum<- f.temps %>%
    group_by(SYEAR, doy) %>%
    summarize(avg_temp = mean(TEMP, na.rm = TRUE), .groups = "drop")
f.sum$yrdoy=paste(f.sum$SYEAR, f.sum$doy, sep=":")

x = merge(x, f.sum[, c("yrdoy", "avg_temp")], by = "yrdoy", all.x = TRUE)


# Create the plot
 
 
# Prepare the data
x_long <- x %>% #no temperature
    filter(yr != 2020) %>%
    pivot_longer(cols = c(dead, soft), 
                 names_to = "variable", 
                 values_to = "value") %>%
    mutate(variable = recode(variable, 
                             "dead" = "Dead", 
                             "soft" = "Soft")) %>%  # Change name here
    mutate(variable = factor(variable, levels = c("Dead", "Soft")))  # Control facet order
    

x_long.temp <- x %>%
    filter(yr != 2020) %>% #With Temperature
    pivot_longer(cols = c(dead, soft, avg_temp), 
                 names_to = "variable", 
                 values_to = "value") %>%
    mutate(variable = recode(variable, 
                             "dead" = "Dead", 
                             "soft" = "Soft",
                             "avg_temp" = "FSRS Temp °C")) %>%  # Change name here
    mutate(variable = factor(variable, levels = c("Dead", "Soft", "FSRS Temp °C")))  # Control facet order
    
lruplot.temp <-
    ggplot(x_long.temp, aes(x = doy, y = value, color = factor(yr), group = yr)) +
    geom_smooth(method = "loess", se = FALSE) +  # LOESS smooth line, no confidence interval
    labs(
        y = "% of Lobsters",  # Shared y-axis label
        color = "Year",  # Legend label for the color
        title = "LRU Lobster"  # Overall title
    ) +
    facet_wrap(~ variable, scales = "free_y", ncol = 1) +  # Stack facets vertically
    scale_x_continuous(
        breaks = c(121, 153, 183),  # Custom breaks for May (121), June (153), July (183)
        labels = c("May", "June", "July")  # Labels for custom breaks
    ) +
    theme_minimal() +  # Clean theme
    theme(
        axis.title.x = element_blank(),  # Remove x-axis title
        plot.title = element_text(size = 16, hjust = 0.1),
        strip.text = element_text(
            face = "bold",  # Bold facet labels
            size = 11, 
            color = "black", 
            hjust = 0.5  # Center facet labels
        ),
        axis.text.x = element_text(angle = 0, hjust = 0.5),  # Align x-axis text
        legend.position = "right",  # Legend will appear on the right
        axis.title.y = element_text(size = 12)  # Y-axis title for all facets
    ) +
    scale_y_continuous(
        labels = scales::number_format()  # Ensure numeric y-axis labels are shown
    )

# Print the plot
print(lruplot.temp)

ggsave(lruplot.temp,filename=file.path(temp.dir, "lru_data_temp.pdf"), width=6, height=6, dpi=180)


lruplot <-
    ggplot(x_long, aes(x = doy, y = value, color = factor(yr), group = yr)) +
    geom_smooth(method = "loess", se = FALSE) +  # LOESS smooth line, no confidence interval
    labs(
        y = "% of Lobsters",  # Shared y-axis label
        color = "Year",  # Legend label for the color
        title = "LRU Lobster"  # Overall title
    ) +
    facet_wrap(~ variable, scales = "free_y", ncol = 1) +  # Stack facets vertically
    scale_x_continuous(
        breaks = c(121, 153, 183),  # Custom breaks for May (121), June (153), July (183)
        labels = c("May", "June", "July")  # Labels for custom breaks
    ) +
    theme_minimal() +  # Clean theme
    theme(
        axis.title.x = element_blank(),  # Remove x-axis title
        plot.title = element_text(size = 16, hjust = 0.1),
        strip.text = element_text(
            face = "bold",  # Bold facet labels
            size = 11, 
            color = "black", 
            hjust = 0.5  # Center facet labels
        ),
        axis.text.x = element_text(angle = 0, hjust = 0.5),  # Align x-axis text
        legend.position = "right",  # Legend will appear on the right
        axis.title.y = element_text(size = 12)  # Y-axis title for all facets
    ) +
    scale_y_continuous(
        labels = scales::number_format()  # Ensure numeric y-axis labels are shown
    )

# Print the plot
print(lruplot)

ggsave(lruplot,filename=file.path(temp.dir, "lru_data.pdf"), width=6, height=6, dpi=180)

#-----------------------------------------------------------------------------------------------
#Tagging
#Need to change to geraint's oracle account for 2025 (in transition to lobster space)

require(LobTag2)

#geraint.oracle=T
#if(geraint.oracle) {
#    oracle.personal.user <- 'ELEMENTG'
#    oracle.personal.password <- 'P2wj4yq4'
#     }
setwd(tag.dir) #by setting wd, it will default to this location for saving tagging maps

#map releases only. Globally then by LFA
map_by_factor(db="Oracle",filter.from="releases", factor.by="YEAR", all.release=T, show.recaptures = F, inset.map =F, tag.prefix = 'XY', point.size=1.2, file.type="png", zoom.out=1)

map_by_factor(db="Oracle",filter.from="releases", map.by="MANAGEMENT_AREA", filter.by="YEAR", all.release=T, show.recaptures = F, inset.map =F,
              point.size=1, file.type="png", zoom.out=10)

#map recaptures only
map_by_factor(db="Oracle",factor.from="recaptures", group.by="YEAR", all.release=T, show.releases= F, inset.map =F,
              point.size=1.2, file.type="png", zoom.out=10)

map_by_factor(db="Oracle",factor.from="recaptures", map.by="MANAGEMENT_AREA", group.by="YEAR", show.releases = F, inset.map =F,
              point.size=1, file.type="png", zoom.out=10)

#map paths only for tags with recaptures
path.dir=file.path(tag.dir, "paths")
dir.create(path.dir)
setwd(path.dir)

map_by_factor(db="Oracle",factor.from="releases", all.release=F, add.paths = T,  inset.map =F, point.size=1.2, file.type="png", zoom.out=10)
map_by_factor(db="Oracle",factor.from="recaptures", all.release=F, add.paths = T,  inset.map =F, point.size=1.2, file.type="png", zoom.out=10)


map_by_factor(db="Oracle",factor.from="releases", map.by="MANAGEMENT_AREA", group.by="YEAR", all.releases = T, add.paths = T, inset.map =F, 
              point.size=1, file.type="png", zoom.out=10)

map_by_factor(db="Oracle",factor.from="recaptures", map.by="MANAGEMENT_AREA", group.by="YEAR", all.releases = T, add.paths = T, inset.map =F, 
              point.size=1, file.type="png", zoom.out=10)



### Bycatch### - NOT USED SINCE 2020

#Bycatch estimates are calculated using effort from logbook data for LFAs 31A and 31B
#To estimate LFA 27 bycatch, gulf landings need to be added to logs.

bc.dir=file.path(figdir, "bycatch")
dir.create( bc.dir, recursive = TRUE, showWarnings = TRUE )

Lobster.Bycatch(lfa=c("31A","31B"), save=T, save.dir=bc.dir)

