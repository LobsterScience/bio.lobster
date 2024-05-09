setwd("C:/Users/HowseVJ/Documents/GitHub/bio.lobster/inst/Updates/LFA35-38/2023 Updates")

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

p = bio.lobster::load.environment()
la()
assessment.year = 2023 ##Check Year
p$syr = 1989
p$yrs = p$syr:assessment.year

figdir = file.path("2023 Updates")
p$lfas = c("35") # specify lfa


x11(width=5, height=5)
LobsterMap(ylim=c(43.3,46),	xlim=c(-67.8,-63.2))



p$lfas = c("35") # specify lfa


###################### LOAD DATA ########################

lobster.db('logs.redo') 
lobster.db('logs')

lobster.db('process.logs.redo')
logs=lobster.db("process.logs")

lobster.db('annual.landings.redo')
land =lobster.db("annual.landings")

lobster.db('seasonal.landings.redo')
Sland = lobster.db('seasonal.landings')

######################### MODELLED CPUE ############################
#TempModelling = TempModel( annual.by.area=F, redo.data=T)
CPUE.data<-CPUEModelData(p,redo=F,TempModelling2023)
## Commercial CPUE MOdels
mf1 = formula(logWEIGHT ~ fYEAR + DOS + TEMP + DOS * TEMP)

CPUE.data<- CPUEModelData(p,redo=F)
t=with(subset(CPUE.data,DOS==1),tapply(TEMP,LFA,mean))

pData=list()

CPUEModelResults = list()
p$lfas =c("35","36","38") 
for(i in 1:length( p$lfas)){
  
  mdata = subset(CPUE.data,LFA==p$lfas[i]&SYEAR%in%p$yrs)
  if(nrow(mdata)>10){
    CPUEModelResults[[i]] = CPUEmodel(mf1,mdata,t=t[i],d=1)
    pData[[i]]=CPUEModelResults[[i]]$pData
    pData[[i]]$LFA=p$lfas[i]
  }
}

names(CPUEModelResults) = p$lfas

CPUEindex=do.call("rbind",pData)




#tiff("C:/Users/Howsevj/Documents/3638_Update/36plotcpue.tiff", width=7,height=6, units="in",res=300)

l35 = subset(CPUEindex,LFA==35,c("YEAR","mu"))
k = median(l35$mu[which(l35$YEAR %in% 2011:2018)])
usr = .4*k
lrp = .2*k

######## ENGLISH PLOT ###########
plot(l35$YEAR,l35$mu,xlab='Year',ylab=' Standardized CPUE (kg/TH)',type='p',pch=16,ylim=c(0,8),xlim=c(2005,2023))
running.median = with(rmed(l35$YEAR,l35$mu),data.frame(YEAR=yr,running.median=x))
l35=merge(l35,running.median,all=T)
lines(l35$YEAR,l35$running.median,col='blue',lty=1,lwd=3)
points(l35$YEAR[nrow(l35)],l35$mu[nrow(l35)], type='b', pch=24,bg='blue')
abline(h=usr,col='green',lwd=2,lty=2)
abline(h=lrp,col='red',lwd=2,lty=3)

dev.off()

######## FRENCH PLOT ###########
plot(l35$YEAR,l35$mu,xlab='Ann?e',ylab='CPUE Standardis? (kg/casier lev?)',type='p',pch=16,ylim=c(0,6),xlim=c(2005,2023))
running.median = with(rmed(l35$YEAR,l35$mu),data.frame(YEAR=yr,running.median=x))
l35=merge(l35,running.median,all=T)
lines(l35$YEAR,l35$running.median,col='blue',lty=1,lwd=3)
points(l35$YEAR[nrow(l35)],l35$mu[nrow(l35)], type='b', pch=24,bg='blue')
abline(h=usr,col='green',lwd=2,lty=2)
abline(h=lrp,col='red',lwd=2,lty=3)

dev.off()

############################ RAW CPUE ###################################
a = lobster.db('process.logs')
a = subset(a,LFA==35)
b=aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~SYEAR,data=a,FUN=sum)
b$CPUE=b$WEIGHT_KG/b$NUM_OF_TRAPS

#tiff("R:/Shared/CSA_Office/2022 CSAS Processes/LFA 35 Update/Completed Edits and FR Fig/36plotcpueraw.tiff", width=7,height=6, units="in",res=300)
######### FRENCH PLOT ###############
plot(b$SYEAR, b$CPUE, xlab="Year", ylab="CPUE (kg/Trap Hauls)", type='p', pch=16, ylim=c(0,3))
points(b$SYEAR[nrow(b)],b$CPUE[nrow(b)], type='b', pch=24,cex = 1.5, bg='blue')
running.median = with(rmed(b$SYEAR,b$CPUE),data.frame(SYEAR=yr,running.median=x))
b=merge(b,running.median,all=T)
lines(b$SYEAR,b$running.median,col='blue',lty=1,lwd=3)
dev.off()

######### ENGLISH PLOT ###############
plot(b$SYEAR, b$CPUE, xlab="Ann?e", ylab="CPUE (kg/casier lev?)", type='p', pch=16, ylim=c(0,2.5))
points(b$SYEAR[nrow(b)],b$CPUE[nrow(b)], type='b', pch=24,cex = 1.5, bg='blue')
running.median = with(rmed(b$SYEAR,b$CPUE),data.frame(SYEAR=yr,running.median=x))
b=merge(b,running.median,all=T)
lines(b$SYEAR,b$running.median,col='blue',lty=1,lwd=3)
dev.off()

############################### LANDINGS ###################################

a=lobster.db('process.logs')
a = subset(a,LFA==35)
b=aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~SYEAR,data=a,FUN=sum)
b$CPUE=b$WEIGHT_KG/b$NUM_OF_TRAPS
#h=lobster.db('seasonal.landings.redo')
h=lobster.db('seasonal.landings')
h
h=h[,c('SYEAR','LFA35')]
h
h$SYEAR=1976:2023
merge(b,h)
g=merge(b,h)
g$EFF = g$LFA35/g$CPUE

write.csv(h, "C:/Users/HowseVJ/Documents/GitHub/bio.lobster/inst/Updates/LFA35-38/2023 Updates/landings.csv" )

tiff("R:/Shared/CSA_Office/2022 CSAS Processes/LFA 35 Update/Completed Edits and FR Fig/36plotlandeff.tiff", width=7,height=6, units="in",res=300)



################## ENGLISH PLOT #######################
par(mar=c(5.1, 4.1, 4.1, 5.1),las=1)
plot(g$SYEAR,g$LFA35,xlab='Year',ylab='Landings (t)',type='h',ylim=c(0,max(g$LFA35)*1.2),pch=15,col='grey',lwd=10,lend=3)
lines(g$SYEAR[nrow(g)],g$LFA35[nrow(g)],type='h',pch=21,col='steelblue4',lwd=10,lend=3)
par(new=T)

plot(g$SYEAR,g$EFF,ylab='',xlab='', type='b', pch=16, axes=F,ylim=c(0,max(g$EFF,na.rm=T)))
points(g$SYEAR[nrow(g)],g$EFF[nrow(g)], type='b', pch=24,bg='black')
axis(4)
mtext("Effort ('000s Trap Hauls)", 4, 3.5, outer = F,las=0)	

dev.off()		


subsamp<-g[g$SYEAR >2009 & g$SYEAR <2024,]
range(subsamp$LFA35)
median(subsamp$LFA35)

################## FRENCH PLOT #######################
par(mar=c(5.1, 4.1, 4.1, 5.1),las=1)

plot(g$SYEAR,g$LFA35,xlab='Ann?e',ylab='D?barquements (t)',type='h',ylim=c(0,max(g$LFA35)*1.2),pch=15,col='grey',lwd=10,lend=3)
lines(g$SYEAR[nrow(g)],g$LFA35[nrow(g)],type='h',pch=21,col='steelblue4',lwd=10,lend=3)
par(new=T)

plot(g$SYEAR,g$EFF,ylab='',xlab='', type='b', pch=16, axes=F,ylim=c(0,max(g$EFF,na.rm=T)))
points(g$SYEAR[nrow(g)],g$EFF[nrow(g)], type='b', pch=24,bg='black')
axis(4)
mtext("Effort (milliers de?casiers lev?s)", 4, 3.5, outer = F,las=0)	

dev.off()		


subsamp<-g[g$SYEAR >2009 & g$SYEAR <2024,]
range(subsamp$LFA35)
median(subsamp$LFA35)



############ SCALLOP RECRUITMENT ##############


p = bio.lobster::load.environment()
la()

assessment.year = 2023 ########### check the year #######

p$syr = 1989
p$yrs = p$syr:assessment.year



# define place for figures to go
#figdir = file.path("C:/Users/HowseVJ/Documents/GitHub/bio.lobster/inst/Updates/LFA35-38")

p$lfas = c("35") # specify lfa
p$subareas = c("35") # specify subareas for data summary

# update data through RODBC

lobster.db('scallop.redo')
lobster.db('scallop')

scalSurv<-ScallopSurveyProcess(size.range=c(70,82.5),bin.size=2.5)
scalSurv<-scalSurv[scalSurv$YEAR <2024,] # 2022 data not complete yet

R1.ab.35=with(subset(scalSurv,LFA==35&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
R1.ab.35_19=with(subset(scalSurv,LFA==35&YEAR>1998&YEAR<2024),tapply(LobDen,YEAR,mean,na.rm=T))



require(dplyr)
RA35<-as.data.frame(R1.ab.35)
RA35<-data.frame(cbind(rownames(RA35), data.frame(RA35, row.names = NULL)))
RA35<-RA35 %>%
  rename(Year = rownames.RA35., 
         Recruit_Abundance = R1.ab.35)

tiff("R:/Shared/CSA_Office/2022 CSAS Processes/LFA 35 Update/Completed Edits and FR Fig/35plotrecruit.tiff", width=7,height=6, units="in",res=300)
plot(RA35$Year, RA35$Recruit_Abundance, pch=16,xlab='Year',ylab = 'Recruit Abundance')
lines(1999:2023,runmed(R1.ab.35_19,3),lwd=2, col='salmon')
dev.off()
