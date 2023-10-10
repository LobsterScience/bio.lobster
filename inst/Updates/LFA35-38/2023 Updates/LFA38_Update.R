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
p$lfas = c("38") # specify lfa


x11(width=5, height=5)
LobsterMap(ylim=c(43.3,46),	xlim=c(-67.8,-63.2))



p$lfas = c("38") # specify lfa


###################### LOAD DATA ########################

#lobster.db('logs.redo') 
lobster.db('logs')

#lobster.db('process.logs.redo')
logs=lobster.db("process.logs")

#lobster.db('annual.landings.redo')
land =lobster.db("annual.landings")

#lobster.db('seasonal.landings.redo')
Sland = lobster.db('seasonal.landings')

######################### MODELLED CPUE ############################
#TempModelling = TempModel( annual.by.area=F, redo.data=T)
CPUE.data<-CPUEModelData(p,redo=F,TempModelling)
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

l38 = subset(CPUEindex,LFA==38,c("YEAR","mu"))
k = median(l38$mu[which(l38$YEAR %in% 2011:2018)])
usr = .4*k
lrp = .2*k

######## ENGLISH PLOT ###########
plot(l38$YEAR,l38$mu,xlab='Year',ylab=' Standardized CPUE (kg/TH)',type='p',pch=16,ylim=c(0,6),xlim=c(2005,2023))
running.median = with(rmed(l38$YEAR,l38$mu),data.frame(YEAR=yr,running.median=x))
l38=merge(l38,running.median,all=T)
lines(l38$YEAR,l38$running.median,col='blue',lty=1,lwd=3)
points(l38$YEAR[nrow(l38)],l38$mu[nrow(l38)], type='b', pch=24,bg='blue')
abline(h=usr,col='green',lwd=2,lty=2)
abline(h=lrp,col='red',lwd=2,lty=3)

dev.off()

######## FRENCH PLOT ###########
plot(l38$YEAR,l38$mu,xlab='Année',ylab='CPUE Standardisé (kg/casier levé)',type='p',pch=16,ylim=c(0,6),xlim=c(2005,2023))
running.median = with(rmed(l38$YEAR,l38$mu),data.frame(YEAR=yr,running.median=x))
l38=merge(l38,running.median,all=T)
lines(l38$YEAR,l38$running.median,col='blue',lty=1,lwd=3)
points(l38$YEAR[nrow(l38)],l38$mu[nrow(l38)], type='b', pch=24,bg='blue')
abline(h=usr,col='green',lwd=2,lty=2)
abline(h=lrp,col='red',lwd=2,lty=3)

dev.off()

############################ RAW CPUE ###################################
a = lobster.db('process.logs')
a = subset(a,LFA==38)
b=aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~SYEAR,data=a,FUN=sum)
b$CPUE=b$WEIGHT_KG/b$NUM_OF_TRAPS

#tiff("R:/Shared/CSA_Office/2022 CSAS Processes/LFA 35 Update/Completed Edits and FR Fig/36plotcpueraw.tiff", width=7,height=6, units="in",res=300)
######### FRENCH PLOT ###############
plot(b$SYEAR, b$CPUE, xlab="Year", ylab="CPUE (kg/THs)", type='p', pch=16, ylim=c(0,2.5))
points(b$SYEAR[nrow(b)],b$CPUE[nrow(b)], type='b', pch=24,cex = 1.5, bg='blue')
running.median = with(rmed(b$SYEAR,b$CPUE),data.frame(SYEAR=yr,running.median=x))
b=merge(b,running.median,all=T)
lines(b$SYEAR,b$running.median,col='blue',lty=1,lwd=3)
dev.off()


######### French PLOT ###############
plot(b$SYEAR, b$CPUE, xlab="Année", ylab="CPUE brut", type='p', pch=16, ylim=c(0,2.5))
points(b$SYEAR[nrow(b)],b$CPUE[nrow(b)], type='b', pch=24,cex = 1.5, bg='blue')
running.median = with(rmed(b$SYEAR,b$CPUE),data.frame(SYEAR=yr,running.median=x))
b=merge(b,running.median,all=T)
lines(b$SYEAR,b$running.median,col='blue',lty=1,lwd=3)
dev.off()

############################### LANDINGS ###################################

a=lobster.db('process.logs')
a = subset(a,LFA==38)
b=aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~SYEAR,data=a,FUN=sum)
b$CPUE=b$WEIGHT_KG/b$NUM_OF_TRAPS
#h=lobster.db('seasonal.landings.redo')
h=lobster.db('seasonal.landings')
h
h=h[,c('SYEAR','LFA38')]
h=na.omit(h)
h$SYEAR=1976:2023
merge(b,h)
g=merge(b,h)
g$EFF = g$LFA38/g$CPUE

#tiff("R:/Shared/CSA_Office/2022 CSAS Processes/LFA 35 Update/Completed Edits and FR Fig/36plotlandeff.tiff", width=7,height=6, units="in",res=300)



################## ENGLISH PLOT #######################
par(mar=c(5.1, 4.1, 4.1, 5.1),las=1)

plot(g$SYEAR,g$LFA38,xlab='Year',ylab='Landings (t)',type='h',ylim=c(0,max(g$LFA38)*1.2),pch=15,col='grey',lwd=10,lend=3)
lines(g$SYEAR[nrow(g)],g$LFA38[nrow(g)],type='h',pch=21,col='steelblue4',lwd=10,lend=3)
par(new=T)

plot(g$SYEAR,g$EFF,ylab='',xlab='', type='b', pch=16, axes=F,ylim=c(0,max(g$EFF,na.rm=T)))
points(g$SYEAR[nrow(g)],g$EFF[nrow(g)], type='b', pch=24,bg='black')
axis(4)
mtext("Effort ('000s Trap Hauls)", 4, 3.5, outer = F,las=0)	

dev.off()		


subsamp<-g[g$SYEAR >2009 & g$SYEAR <2023,]
range(subsamp$LFA38)
median(subsamp$LFA38)

################## FRENCH PLOT #######################
par(mar=c(5.1, 4.1, 4.1, 5.1),las=1)

plot(g$SYEAR,g$LFA38,xlab='Année',ylab='Débarquements (t)',type='h',ylim=c(0,max(g$LFA38)*1.2),pch=15,col='grey',lwd=10,lend=3)
lines(g$SYEAR[nrow(g)],g$LFA38[nrow(g)],type='h',pch=21,col='steelblue4',lwd=10,lend=3)
par(new=T)

plot(g$SYEAR,g$EFF,ylab='',xlab='', type='b', pch=16, axes=F,ylim=c(0,max(g$EFF,na.rm=T)))
points(g$SYEAR[nrow(g)],g$EFF[nrow(g)], type='b', pch=24,bg='black')
axis(4)
mtext("Effort (milliers de casiers levés)", 4, 3.5, outer = F,las=0)	

dev.off()		


subsamp<-g[g$SYEAR >2009 & g$SYEAR <2024,]
range(subsamp$LFA38)
median(subsamp$LFA38)