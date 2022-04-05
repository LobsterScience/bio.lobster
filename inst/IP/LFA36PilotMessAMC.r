#LFA 36 Season Extension

require(rstan)
require(RODBC)
require(rgdal)
require(devtools)
require(roxygen2)
require(geosphere)
require(SpatialHub)
require(lubridate)
require(bio.utilities)
require(bio.lobster)
require(rstanarm)
require(rlang)
require(glue)
require(PBSmapping)
require(dplyr)
require(tidyverse)
require(ggplot2)
require(dplyr)
require(tidyr)
require(wesanderson)
require(viridis)
require(RODBC)
require(bio.ccir)

la()
p = bio.lobster::load.environment()


setwd("D:/Nova Scotia/Lobster Job/Projects/Season Extension - LFA 36 Pilot")

#lobster.db('fsrs.redo')
lobster.db("fsrs")
fsrs = subset(fsrs,LFA==36)
FSRS.dat<-fsrs
FSRS.dat$VES_DATE<-paste(FSRS.dat$VESSEL_CD,FSRS.dat$HAUL_DATE,sep='.')
FSRS.dat$SYEAR<-FSRS.dat$HAUL_YEAR
FSRS.dat$HAUL_DATE<-as.Date(FSRS.dat$HAUL_DATE)


fsrs.new=subset(FSRS.dat, SYEAR>=2019)
scd.new<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","inputs","FSRS_SIZE_CODES_NEW2020.csv"))
scd.new$LENGTH<-rowMeans(scd.new[c("MIN_S","MAX_S")])
fsrs.new<-merge(fsrs.new,scd.new[c("SIZE_CD","LENGTH")])
fsrs.new$CODES_VERSION="new"

FSRS.dat=fsrs.new


wa<-c(0.000608, 0.001413, 0.00482)
wb<-c(3.058, 2.875, 2.638)

FSRS.dat$WEIGHT<-NA
for(i in 1:3){
  FSRS.dat$WEIGHT[FSRS.dat$SEX==i]<-FSRS.dat$LENGTH[FSRS.dat$SEX==i]^wb[i]*wa[i]
}


dat=FSRS.dat

#only LFA 36 for extension
dat$mn = month(dat$HAUL_DATE)
dat$UID = paste(dat$RECORD_NUMBER,dat$TRAP_NO,dat$LOBSTER_NO,dat$VESSEL_CD,dat$HAUL_DATE, sep="-")
dat$TID = paste(dat$RECORD_NUMBER,dat$TRAP_NO,dat$VESSEL_CD,dat$HAUL_DATE, sep="-")

TH = aggregate(TID~SYEAR,data=dat,FUN=function(x) length(unique(x)))
THR = aggregate(TID~SYEAR,data=subset(dat, mn<7) ,FUN=function(x) length(unique(x)))
THE = aggregate(TID~SYEAR,data=subset(dat, mn>6) ,FUN=function(x) length(unique(x)))

CTH = aggregate(WEIGHT~SYEAR,data=subset(dat,SHORT==0),FUN=sum)
CTHR = aggregate(WEIGHT~SYEAR,data=subset(dat, mn<7 & SHORT==0) ,FUN=sum)
CTHE = aggregate(WEIGHT~SYEAR,data=subset(dat, mn>6 & SHORT==0) ,FUN=sum)

TH = merge(TH,CTH)
THR = merge(THR,CTHR)
THE = merge(THE,CTHE)
TH$CPUET = TH$WEIGHT/TH$TID/1000
THR$CPUER = THR$WEIGHT/THR$TID/1000
THE$CPUEE = THE$WEIGHT/THE$TID/1000

Te = merge(merge(TH[,c('SYEAR','CPUET')],THE[,c('SYEAR','CPUEE')]),THR[,c('SYEAR','CPUER')])

##commercial traps

dat$UID=paste(dat$Trap.Number,dat$Vessel.Code, dat$Date,sep="-")
scd.new<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","inputs","FSRS_SIZE_CODES_NEW2020.csv"))
scd.new$LENGTH<-rowMeans(scd.new[c("MIN_S","MAX_S")])
dd<-merge(dat,scd.new[c("SIZE_CD","LENGTH")],by.x='Size',by.y='SIZE_CD')


wa<-c(0.000608, 0.001413, 0.00482)
wb<-c(3.058, 2.875, 2.638)

dd$WEIGHT<-NA
for(i in 1:3){
  dd$WEIGHT[dd$Sex==i]<-dd$LENGTH[dd$Sex==i]^wb[i]*wa[i]
}
tt = aggregate(Record.Number~Date,data=subset(dd,Short=='Yes'& Size %in% 17:19),FUN=length)
te = aggregate(Record.Number~Date,data=subset(dd,Short=='No' & Size %in% 19:21),FUN=length)
names(te)[2] = 'leg'
ttt = merge(tt,te)
ttt$t = ttt[,2]+ttt[,3]

ttt$Date = as.Date(ttt$Date,format='%d-%b-%y')


with(gg,plot(yr,cp, xlab='Year',ylab='CPUE', ylim=c(0.3,2.2),type='b'))
with(Te,lines(SYEAR,CPUET,type='b',col='red'))
points(x=2021,y=2.12,pch=16)

##CPUE
a = lobster.db('process.logs')
a = subset(a,SYEAR %in% 2017:2021 & LFA %in% 34:38)
aa = split(a,f=list(a$LFA,a$SYEAR))
cpue.lst<-list()
m=0
for(i in 1:length(aa)){
  tmp<-aa[[i]]
  if(nrow(tmp)>5){
    m=m+1
    y = unique(tmp$SYEAR)-1
    org = as.Date(paste(y,10,13,sep="-"))
    tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
    names(tmp)<-c('time','catch','effort')
    tmp$date<-as.Date(tmp$time)
    first.day<-min(tmp$date)
    tmp$time<-julian(tmp$date,origin=org)
    tmp$time = floor(tmp$time/7)*7
    g<-as.data.frame(biasCorrCPUE(tmp,by.time=T))
    g$lfa=unique(aa[[i]]$LFA)
    g$yr = unique(aa[[i]]$SYEAR)
    cpue.lst[[m]] <- g
  }
}
out = do.call(rbind, cpue.lst)
yrs = 2017:2021
yl = max(out$u95)
xl = max(out$t)



par(mfrow = c(2, 3),mar=c(2,3,1,1),omi=c(.4,.6,0.2))
for(i in 1:length(yrs)){
  j = subset(out,yr==yrs[i])
  with(subset(j,lfa==34),plot(t,unBCPUE,type='l',lwd=2,col='red',ylim=c(0,yl),xlim=c(0,xl),xlab= "Day of Season",ylab='CPUE'))
  with(subset(j,lfa==35& t<70),lines(t,unBCPUE,type='l',lwd=2,col='orange',ylim=c(0,yl),xlim=c(0,xl),xlab= "Day of Season",ylab='CPUE'))
  with(subset(j,lfa==35& t>170),lines(t,unBCPUE,type='l',lwd=2,col='orange',ylim=c(0,yl),xlim=c(0,xl),xlab= "Day of Season",ylab='CPUE'))
  with(subset(j,lfa==36 & t<90),lines(t,unBCPUE,type='l',lwd=2,col='black',ylim=c(0,yl),xlim=c(0,xl),xlab= "Day of Season",ylab='CPUE'))
  with(subset(j,lfa==36 & t>150),lines(t,unBCPUE,type='l',lwd=2,col='black',ylim=c(0,yl),xlim=c(0,xl),xlab= "Day of Season",ylab='CPUE'))
  with(subset(j,lfa==38),lines(t,unBCPUE,type='l',lwd=2,col='blue',ylim=c(0,yl),xlim=c(0,xl),xlab= "Day of Season",ylab='CPUE'))
  legend('topright',paste(yr[i]-1,yr[i],sep="-"),bty='n')
       }
  plot(1, axes = FALSE, type = "n",ylab="")
  legend('center',lty=c(1,1,1,1),legend=c('34','35','36','38'),col=c('red','orange','black','blue'),bty="n")


  
  a = lobster.db('process.logs')
  a$mn = month(a$DATE_FISHED)
  a = subset(a,SYEAR %in% 2017:2021 & LFA %in% 36 &mn>3)
  aa = split(a,f=list(a$LFA,a$SYEAR))
  cpue.lst<-list()
  m=0
  for(i in 1:length(aa)){
    tmp<-aa[[i]]
    if(nrow(tmp)>5){
      m=m+1
      y = unique(tmp$SYEAR)-1
      org = as.Date(paste(y,10,13,sep="-"))
      tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
      names(tmp)<-c('time','catch','effort')
      tmp$date<-as.Date(tmp$time)
      first.day<-min(tmp$date)
      tmp$time<-julian(tmp$date,origin=org)
      tmp$time = floor(tmp$time/7)*7
      g<-as.data.frame(biasCorrCPUE(tmp,by.time=F))
      g$lfa=unique(aa[[i]]$LFA)
      g$yr = unique(aa[[i]]$SYEAR)
      cpue.lst[[m]] <- (g)
    }
  }
  out = do.call(rbind, cpue.lst)
  yrs = 2017:2021
  yl = max(out$u95)
  xl = max(out$t)
  
  
  
  
  ##This sums the number of lobster per trap keeping record number
datsum<-dat%>%
  group_by(HAUL_YEAR,RECORD_NUMBER,TRAP_NO)%>%
  summarise(LOBSTERS=sum(LOBSTER_NO))
datsum=as.data.frame(datsum)

#Average number of Lobster per trap per year

datsum2<-datsum%>%
  group_by(HAUL_YEAR)%>%
  summarise(LOBSTERS = sum(LOBSTERS))
datsum2=as.data.frame(datsum2)




##CHECK DATE !
dat<- subset(dat, LFA == 36 & HAUL_DATE >="2020-04-01" & HAUL_DATE <= "2020-07-31")  ##CHECK DATES


LobsterMap('36')
dat = makePBS(dat,polygon=F,coords=c('LONGITUDE','LATITUDE'))
dat$X =dat$LONG_DD
dat$Y =dat$LAT_DD
addPoints(dat,col='red',pch=16,cex=.5)
#LFAgrid<-read.csv("GridPolys.csv")
LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
attr(LFAgrid,'projection') <- "LL"
g = findPolys(dat,LFAgrid)
dat = merge(dat,g[,c('EID','SID')])
#dat$Date = as.Date(dat$HAUL_DATE,format='%d-%b-%Y')
#i = which(dat$Date=='21-06-01')
#dat$Date[i] = dat$Date[i]+365




##Summaries for whole data set for each year of extension

#subset the FSRS file for only LFA 36
#Subset the data for traps sampled  between April 1st and July 31
#loop for each year? 2017-2021



Fu <- dat

Fu$Date = as.Date(Fu$HAUL_DATE)

#This is for looking at Total Lobsters Sampled
Fu$LID = paste(Fu$LOBSTER_NO,Fu$TRAP_NO,Fu$RECORD_NUMBER,sep="-")

##This sums the number of lobster per trap keeping record number
FuSum<-Fu%>%
  group_by(RECORD_NUMBER,TRAP_NO)%>%
  summarise(LOBSTERS=sum(LOBSTER_NO))
FuSum=as.data.frame(FuSum)

#Average number of Lobster per trap
FuAvg<-mean(FuSum$LOBSTERS) ##50 per trap on average for 2021


#This is for looking at total traps sampled
Fu$TID = paste(Fu$TRAP_NO,Fu$RECORD_NUMBER,sep="-")
length(unique(Fu$TID))
##647 Traps sampled in 2021


#Pulls out Sub-legal lobster based on the Size bins from FSRS gauges
Fu$R1 = ifelse(Fu$SIZE_CD %in% c(15,16) | c(Fu$SIZE_CD == 17 & Fu$SHORT ==1),1,0)
#Pulls out Commercial sized lobster
Fu$R0 = ifelse(Fu$SIZE_CD %in% c(18) | c(Fu$SIZE_CD == 17 & Fu$SHORT ==0),1,0)

##Day of Season
Fu$DoSa = as.numeric(Fu$HAUL_DATE - min(Fu$HAUL_DATE))
#Week of Season
Fu$WoSa = round(Fu$DoSa/7)



#Total area
# a includes the legals and sublegals for each Day of Season and the proportion of legals for total lobster (Sum of legal and sublegal)
a = aggregate(cbind(R1,R0)~DoSa+Date, data=Fu, FUN=sum)
a$p = a$R0/(a$R1+a$R0)

## Logs from commercial data
logs = lobster.db('process.logs')
l = subset(logs,LFA==36 & SYEAR==2021 & DATE_FISHED > '2020-03-31')  ### CHECK THE YEAR /DATE ####

# Same thing as above -- Day of season and the date, and landings and number of traps for each day
l$DoSa = as.numeric(l$DATE_FISHED - min(l$DATE_FISHED))
b = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~DoSa+DATE_FISHED, data=l, FUN=sum)


#cumL is cumulative sum divided sum of landings
b$cumL = cumsum(b$WEIGHT_KG) / sum(b$WEIGHT_KG)

dd = merge(a,b,by.x='Date',by.y= 'DATE_FISHED')
dd = subset(dd,is.finite(p))



dd = merge(ttt,b, by.x='D',by.y='DATE_FISHED')

load_all('~/GitHub/bio.ccir')
#full season
da = list()
da$method = 'bino'
da$n = nrow(dd)
da$p = dd$p
da$Cuml = dd$cumL
da$N = dd$t
da$E = dd$leg
da$dates = dd$D
x = ccir_stan_run_binomial(dat = da)
ccir_stan_plots(x,type='predicted')
ccir_stan_plots(x,type='exploitation')
Full= ccir_stan_summarize(x)$ERf ##AMC median .496

##To check exploitation amount
Full= ccir_stan_summarize(x)
#o = data.frame(Full$ERp[3,], Full$dates)
# o


##proportion of exploited to total (ce = exploited, cr =recruits)
#With Extension			 	 
jpeg("cumulative_Ext2020.jpeg",units='in', width=5, height=5, res=300)####CHECK DATE
a = rstan::extract(x$fit,'phat')$phat
b = apply(a,2,quantile,probs=c(0.025,0.5,0.975))
Ylab <- expression(p == c[e] / (c[e] + c[r]))
CC = da$Cuml

cumulativeExt<- plot(CC, da$p,ylim=c(0,1),type='p',col='red',
                     xlab='Cumulative legal catch (proportion of total)',ylab=Ylab,pch=16)
text (xy.coords(0.8,0.95), "2020 Extension", cex = 1.25)  ##CHECK DATE ##
lines(CC,b[2,],col='blue',lwd=2)
lines(CC,b[1,],col='blue',lty=2)
lines(CC,b[3,],col='blue',lty=2)
dev.off()


jpeg("exploitation_Ext2020.jpeg",units='in', width=5, height=5, res=300) ####CHECK DATE
a = rstan::extract(x$fit,'ERp')$ERp
b = apply(a,2,quantile,probs=c(0.025,0.5,0.975))
exploitExt	 <-plot(as.Date(da$dates), b[2,],ylim=c(0,1),type='l',col='blue',
                   xlab='Date',ylab='Exploitation Index',lwd=2)
text(xy.coords(18087.66,0.95), pos=2, "2020 Extension", cex = 1.25)  ##CHECK DATE ##
lines(as.Date(da$dates),b[1,],col='blue',lty=2)
lines(as.Date(da$dates),b[3,],col='blue',lty=2)
dev.off()






#############without extension################


#Total area
# a includes the legals and sublegals for each Day of Season and the proportion of legals for total lobster (Sum of legal and sublegal)
a = aggregate(cbind(R1,R0)~DoSa+Date, data=Fu, FUN=sum)
a$p = a$R0/(a$R1+a$R0)

## Logs from commercial data
logs = lobster.db('process.logs')
l = subset(logs,LFA==36 & SYEAR==2020 & DATE_FISHED > '2020-03-31' & DATE_FISHED <'2020-07-01')  ### CHECK THE YEAR /DATE ####

# Same thing as above -- Day of season and the date, and landings and number of traps for each day
l$DoSa = as.numeric(l$DATE_FISHED - min(l$DATE_FISHED))
b = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~DoSa+DATE_FISHED, data=l, FUN=sum)


#cumL is cumulative sum divided sum of landings
b$cumL = cumsum(b$WEIGHT_KG) / sum(b$WEIGHT_KG)

dd = merge(a,b,by.x='Date',by.y= 'DATE_FISHED')
dd = subset(dd,is.finite(p))

dd = subset(dd,Date<'2020-07-01') ####CHECK DATE



da = list()
da$method = 'bino'
da$n = nrow(dd)
da$p = dd$p
da$Cuml = dd$cumL
da$N = dd$R1+dd$R0
da$E = dd$R0
da$dates = dd$Date
x2 = ccir_stan_run_binomial(dat = da)
ccir_stan_plots(x2,type='predicted')
ccir_stan_plots(x2,type='exploitation')
Old = ccir_stan_summarize(x2)$ERf #AMC median .474		 	  	 	  

#Without Extension			 	
jpeg("cumulative_Reg2020.jpeg",units='in', width=5, height=5, res=300)
a = rstan::extract(x2$fit,'phat')$phat
b = apply(a,2,quantile,probs=c(0.025,0.5,0.975))
Ylab <- expression(p == c[e] / (c[e] + c[r]))
CC = da$Cuml

cumulativeExt<- plot(CC, da$p,ylim=c(0,1),type='p',col='red',
                     xlab='Cumulative legal catch (proportion of total)',ylab=Ylab,pch=16)
text (xy.coords(0.57,0.95), "2020 Without Extension", cex = 1.25)  ##CHECK DATE ##
lines(CC,b[2,],col='blue',lwd=2)
lines(CC,b[1,],col='blue',lty=2)
lines(CC,b[3,],col='blue',lty=2)
dev.off()


jpeg("exploitation_Reg2020.jpeg",units='in', width=5, height=5, res=300) 
a = rstan::extract(x2$fit,'ERp')$ERp
b = apply(a,2,quantile,probs=c(0.025,0.5,0.975))
exploitExt	 <-plot(as.Date(da$dates), b[2,],ylim=c(0,1),type='l',col='blue',
                   xlab='Date',ylab='Exploitation Index',lwd=2)
text(xy.coords(18080.05,0.95), pos=2, "2020 Without Extension", cex = 1.25)  ##CHECK DATE ##
lines(as.Date(da$dates),b[1,],col='blue',lty=2)
lines(as.Date(da$dates),b[3,],col='blue',lty=2)
dev.off()


#Landings show amount from extension 	  

plogs = lobster.db('process.logs.unfiltered')
log36 =subset(plogs, LFA==36)

log36 = log36[,c('SYEAR',"DATE_FISHED",'WEIGHT_KG','BUMPUP')]
log36<-subset(log36, SYEAR<2022)

log36$LANDINGS<-(log36$WEIGHT_KG*log36$BUMPUP)

log36$DATE_FISHED<-as.Date(log36$DATE_FISHED)
log36$year<-as.numeric(format(log36$DATE_FISHED, format = "%Y"))

log36$month<-as.numeric(format(log36$DATE_FISHED, format = "%m"))
log36$day<-as.numeric(format(log36$DATE_FISHED, format = "%d"))



#convert to tonnes
log36$WEIGHT_T<-log36$LANDINGS/1000

#Sort by season full or extension
logs36ext<-log36 %>%
  group_by(SYEAR)%>%
  mutate("Timing"=ifelse(SYEAR < 2017, "Season", 
                         ifelse (SYEAR > 2018 & month >6 & month <8, "Extension", "Season" )))

logs36ext<-as.data.frame(logs36ext)



##sum by TOY
landingext<-logs36ext %>%
  group_by(SYEAR,Timing)%>%
  summarise(catch=sum(WEIGHT_T))
landingext<-as.data.frame(landingext)  


##plot all years with portion of bar for landings by the season and extension
pal <- c("#F24D29","#1C366B")



jpeg("extendedlandings_2021.jpeg",units='in', width=5, height=5, res=300) 
ggplot(landingext, aes(fill=Timing, y =catch, x=SYEAR))+
  geom_bar(position="stack", stat="identity")+
  scale_y_continuous(name="Landings (t)",limits = c(0,5000), expand = c(0, 0))+
  scale_x_continuous(name = "Year", limits = c(2001,2022), expand=c(0,0))+
  scale_fill_manual(values=pal)+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
dev.off()



figdir = getwd()
logs = lobster.db('process.logs')
logfoot = subset(logs,LFA==36)
p = bio.lobster::load.environment()
la()
require(devtools)

## Fishery Footprint - Landings
catchLevels = c(0,50000,100000,200000,300000,400000,500000,600000)
yrs = 2015:2021    ###CHECK YEARS
catchgrids.lst=list()
#par(mfrow = c(2, 4),mar=c(2,3,1,1),omi=c(.4,.6,0.2,0.1))

for(i in 1:length(yrs)){
  print(i)
  catchgrids.lst[[i]] = lobGridPlot(subset(logfoot,LFA%in%p$lfas&SYEAR==yrs[i],c("LFA","GRID_NUM","TOTAL_WEIGHT_KG")),FUN=sum,lvls=catchLevels)
  pdf(file.path(figdir,paste0("FisheryFootprint",yrs[i],".pdf")),5,5)
  LobsterMap('36',poly.lst=catchgrids.lst[[i]])
  text(x=c(-65.5,-65.5,-64.5),y=c(43.1,42.7,42.7),labels=c(34,40,41),col=rgb(0,0,0,0.8),cex=1.5)
  title(yrs[i],line=-3,cex.main=2,adj=0.3)
  SpatialHub::contLegend('bottomright',lvls=catchgrids.lst[[i]]$lvls/1000,Cont.data=catchgrids.lst[[i]],title="Catch (tons)",inset=0.02,cex=0.8,bg='white')
  dev.off()
  #pdf2png(file.path(figdir,paste0("FisheryFootprint",yrs[i])))
}

#Fishery Footprint - Total traps?



#CPUE by Date_fished + SYEAR for each year of the season EXT

h = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~DATE_FISHED+SYEAR,data=logs,FUN=sum)
h$CPUE = h$WEIGHT_KG/h$NUM_OF_TRAPS



###proportional Cumulative catch for date of season with extension 

## run 3 times for each year
#lobster.db('process.logs.redo')

u = lobster.db('process.logs')
lfa=36 
u = subset(u, SYEAR==2021 & LFA==36)  ##CHECK YEAR
u$DATE_FISHED<-as.POSIXct(u$DATE_FISHED)
u = u[order(u$DATE_FISHED),]
u<- subset(u, DATE_FISHED >="2020-11-01" & DATE_FISHED <= "2021-07-31") ##CHECK DATE###
u$DAYSD<-(u$DATE_FISHED - min(u$DATE_FISHED))/3600/24 
u<-as.data.frame(u) 

jpeg("CumulativeProp_18.jpeg",units='in', width=5, height=5, res=300) 
uu = aggregate(WEIGHT_KG~DAYSD+DATE_FISHED, data=u, FUN=sum)
uu$CSUM=cumsum(uu$WEIGHT_KG)/sum(uu$WEIGHT_KG)
with(uu, plot(DAYSD, CSUM, xlab='Day of Season',ylab='Cumulative Proportion of Landings',type='l', main='2020-2021'))
with(subset(uu,DATE_FISHED>'2021-06-29'),   ###CHECK YEAR
     lines(DAYSD, CSUM, xlab='Day of Season',ylab='Cumulative Proportion of Landings',type='l', 
           main='2020-2021',col='red',lwd=3))  ##CHECK YEAR#
dev.off()


load("C:/Users/Cooka/Documents/bio_data/bio.lobster/analysis/LFA34-38/LFA35-38LengthFrequenciespolygonSummerRV19992018.rdata")
ab = aa
load("C:/Users/Cooka/Documents/bio_data/bio.lobster/analysis/LFA34-38/LFA35-38LengthFrequenciespolygonSummerRV.rdata")
aa$df.yst <- NULL

aa = as.data.frame(rbind(aa,ab))
yrs = 2014:2021
aa$CL = round(aa$FLEN/7)*7
gb = aggregate(n.yst~CL,data=subset(aa,yr %in% 2014:2017),FUN=sum)
ga = aggregate(n.yst~CL,data=subset(aa,yr %in% 2018:2021),FUN=sum)
gb$SC = gb$n.yst/sum(gb$n.yst)
ga$SC = ga$n.yst/sum(ga$n.yst)

cc = c(rep('black',4),rep('red',4))
for(i in 1 :length(yrs)){
  j = subset(aa,yr==yrs[i])
  j$CL = round(j$FLEN/7)*7
  k = aggregate(n.yst~CL,data=j,FUN=sum)
  k$SC = k$n.yst/sum(k$n.yst)
  if(i==1) with(k,plot(CL,SC,type='l',ylab='Scaled Frequency',xlab='Carapace Length',col=cc[i],ylim=c(0,.2))) 
  with(k,lines(CL,SC,type='l',col=cc[i])) 
}

with(gb,lines(CL,SC,lwd=3,col='black'))
with(ga,lines(CL,SC,lwd=3,col='red'))
