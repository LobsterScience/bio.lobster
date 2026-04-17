p = bio.lobster::load.environment()
require(SpatialHub)
require(lubridate)
require(dplyr)
require(bio.ccir)
library(ggplot2)
library(tidyr)
library(stringr)

#la()

#Choose one
#assessment.year = p$current.assessment.year 
assessment.year = p$current.assessment.year-1

figdir = file.path(project.datadirectory("bio.lobster","assessments","MRLAC",assessment.year))
dir.create( figdir, recursive = TRUE, showWarnings = FALSE )

p$lfas = c("27", "28", "29", "30", "31A", "31B", "32", "33") # specify lfas for data summary
p$subareas = c("27N","27S", "28", "29", "30", "31A", "31B", "32", "33E", "33W") #lfas for data summary


pp <- ggLobsterMap(area='27-38', addLFALabels=T, LFA_label_size = 5, addGrids = F)
ggsave(file=file.path(figdir, "Map27-38.png"))

#Establishing a master reference table of all lrp, usr

ref = data.frame(LFA=c(27:30,'31A','31B',32,33),
                 lrp=c(.14,.12,.11,.28,.16,.16,.14,.14),
                 usr=c(.27,.25,.22,.56,.31,.32,.29,.28))

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

write.csv(crd, file=paste0(figdir, "/fishery.stats.27-33.csv"), row.names=F )
save(crd, file=paste0(cpue.dir, "/cpueData.Rdata") )

ls=c('27', '28', '29', '30')
ls2=c('31A', '31B', '32', '33')

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
png(filename=file.path(figdir, "CPUE_LFA27-30.png"),width=8, height=5.5, units = "in", res = 800)
par(mfrow=c(2,2))
for (l in ls) {
  crplot(French=F) #Change to crplot(French=T) to produce French axis labels
}
dev.off()



# Begin second CPUE figure 31A, 31B, 32, 33)
png(filename=file.path(figdir, "CPUE_LFA31A-33.png"),width=8, height=5.5, units = "in", res = 800)
par(mfrow=c(2,2))
for (l in ls2) {
  crplot(French=F) #Change to crplot(French=T) to produce French axis labels
}
dev.off()


#Exploitation
{   
## EXploitation PLots
load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR2732.rdata')) #file =oo
ex2732=oo
rm(oo)

load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary', 'lfa33', 'compiledExploitationCCIR33.rdata')) #file =oo
ex33=oo
rm(oo)
ex33$LFA='33'
ex33$RR75 = max(ex33$ERf75[ex33$Yr<p$current.assessment.year])

ex33 <- ex33 %>%
  arrange(Yr) %>%   # sorting is important
  mutate(
    ERrmed = rmed(Yr, ERfm)$x
  ) %>%
  ungroup()

oo=as.data.frame(rbind(ex2732, ex33))


png(filename=file.path(figdir, "exploitation.27-30.png"),width=10, height=7, units = "in", res = 800)
  par(mfrow=c(2,2))	
  for(i in c("27", "28", "29", "30")){
      if (i=="28"){
        #par(mar = c(0,0,0,0))
        plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
        text(x = 0.5, y = 0.5, paste("LFA 28- No Data Available"),  cex = 1.5, col = "black")
      }else{
      o = subset(oo,LFA==i)
      RR7 = o$RR75[1]
      ExploitationRatePlots(data = o[,c("Yr","ERfm","ERfl","ERfu")],lrp=RR7,lfa = i,fd=figdir, save=F, title=i) 
      }}
dev.off()
  
png(filename=file.path(figdir, "exploitation.31A-33.png"),width=10, height=7, units = "in", res = 800)
par(mfrow=c(2,2))	
for(i in c("31A", "31B", "32", "33")){
  o = subset(oo,LFA==i)
  RR7 = o$RR75[1]
  ExploitationRatePlots(data = o[,c("Yr","ERfm","ERfl","ERfu")],lrp=RR7,lfa = i,fd=figdir, save=F, title=i) 
}
dev.off()
}

#Landings


sl = lobster.db('seasonal.landings')
sl$YR = as.numeric(substr(sl$SYEAR,6,9))
sl=sl[,c("YR", "LFA33")]

al=lobster.db('annual.landings')
al=al[,c("YR","LFA27","LFA28","LFA29","LFA30","LFA31A","LFA31B","LFA32")]

h <- merge(al,sl, by = "YR", all = TRUE)
h=subset(h, YR %in% c(1985:assessment.year))

names(h)[names(h) == "YR"] <- "Year"

h_long <- h %>%
  pivot_longer(-Year, names_to = "LFA", values_to = "mt") %>%
  arrange(LFA, Year)

h_long <- h_long %>%
  mutate(LFA = str_remove(LFA, "^LFA"))

h_long=as.data.frame(h_long)



ls=c('27', '28', '29', '30')
ls2=c('31A', '31B', '32', '33')

xlim<-c(1984,assessment.year)

#1 Landings Figure- LFAs 27, 28, 29, 20)
#-------------------------------------------
  ylab= 'Landings (t)'
 

png(filename=file.path(figdir, "Landings_LFA27-30.png"),width=8, height=5.5, units = "in", res = 800)
par(mfrow=c(2,2))

lst=ls
for (i in 1:length(lst)) {
  
  data <- subset(h_long, Year <= assessment.year & LFA == lst[i]) 
  data <- data %>% arrange(Year)
  par(mar=c(3.0,5.0,2.0,2.0))
  ymax <- max(data$mt, na.rm = TRUE)
  plot(data$Year, data$mt, 
       ylab = ylab, type='h', xlim = xlim, xlab="Year",
       ylim = c(0, ymax * 1.2),
       pch=15, col='royalblue1', lwd=4, lend=3,
       col.lab='royalblue3', col.axis='royalblue3')
  
  if (unique(data$LFA) != "LFA33") {
    lines(data$Year[nrow(data)], data$mt[nrow(data)],
          type='h', pch=21, col='green1', lwd=4, lend=3)
  }
  text(x=(xlim[1]+2), y= 1.15*max(data$mt, na.rm = TRUE), lst[i], cex=1.6)
}
dev.off()

#2 Landings Figure- LFAs 31A, 31B, 32, 33
#-------------------------------------------------------------------

png(filename=file.path(figdir, "Landings_LFA31-33.png"),width=8, height=5.5, units = "in", res = 800)
par(mfrow=c(2,2))

lst=ls2
for (i in 1:length(lst)) {
  
  data <- subset(h_long, Year <= assessment.year & LFA == lst[i]) 
  data <- data %>% arrange(Year)
  par(mar=c(3.0,5.0,2.0,2.0))
  ymax <- max(data$mt, na.rm = TRUE)
  plot(data$Year, data$mt, 
       ylab = ylab, type='h', xlim = xlim, xlab="Year",
       ylim = c(0, ymax * 1.2),
       pch=15, col='royalblue1', lwd=4, lend=3,
       col.lab='royalblue3', col.axis='royalblue3')
  
  if (unique(data$LFA) != "33") {
    lines(data$Year[nrow(data)], data$mt[nrow(data)],
          type='h', pch=21, col='green1', lwd=4, lend=3)
  }
  text(x=(xlim[1]+2), y= 1.15*max(data$mt, na.rm = TRUE), lst[i], cex=1.6)
}
dev.off()

par(mfrow =c(1,1)) 
